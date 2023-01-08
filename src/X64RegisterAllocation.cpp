struct BasicBlock
{
	s64 beginIdx;
	s64 endIdx;
	bool livenessAnalizedOnce;
	DynamicArray<BasicBlock *, ThreadAllocator> inputs;
	DynamicArray<BasicBlock *, ThreadAllocator> outputs;

	// @Todo: bitmaps
	DynamicArray<u32, ThreadAllocator> liveValuesAtInput;
	DynamicArray<u32, ThreadAllocator> liveValuesAtOutput;
};

void X64Patch(Context *context, X64Instruction *original, X64Instruction newInst)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	X64Instruction *patch1 = BucketArrayAdd(&jobData->bePatchedInstructions);
	*patch1 = newInst;
	X64Instruction *patch2 = BucketArrayAdd(&jobData->bePatchedInstructions);
	*patch2 = *original;

	X64Instruction patchInst = { {}, X64_Patch };
	patchInst.patch1 = patch1;
	patchInst.patch2 = patch2;
	*original = patchInst;
}

BasicBlock *PushBasicBlock(BasicBlock *currentBasicBlock,
		BucketArray<BasicBlock, ThreadAllocator, 512> *basicBlocks)
{
	s64 endOfLastBlock = -1;
	if (currentBasicBlock)
		endOfLastBlock = currentBasicBlock->endIdx;

	BasicBlock *result = BucketArrayAdd(basicBlocks);
	*result = {};

	result->beginIdx = endOfLastBlock + 1;
	DynamicArrayInit(&result->inputs, 4);
	DynamicArrayInit(&result->outputs, 4);
	DynamicArrayInit(&result->liveValuesAtInput,  8);
	DynamicArrayInit(&result->liveValuesAtOutput, 8);

	return result;
}

bool CanBeRegister(Context *context, u32 valueIdx)
{
	Value v = IRGetValue(context, valueIdx);
	if ((v.flags & (VALUEFLAGS_IS_MEMORY | VALUEFLAGS_IS_ALLOCATED)) == VALUEFLAGS_IS_ALLOCATED)
		// Allocated on register
		return true;
	if (valueIdx & VALUE_GLOBAL_BIT)
		return false; // Global values can't be registers (other than physical registers, checked above)
	if (v.flags & (VALUEFLAGS_FORCE_MEMORY | VALUEFLAGS_ON_STATIC_STORAGE |
				VALUEFLAGS_IS_EXTERNAL))
		return false;
	if (v.typeTableIdx == TYPETABLEIDX_128)
		return true;
	TypeInfo typeInfo = GetTypeInfo(context, v.typeTableIdx);
	if (typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		typeInfo.typeCategory == TYPECATEGORY_UNION)
		return false;
	if (!IsPowerOf264(typeInfo.size) || typeInfo.size > 8)
		// @Improve: we could actually fit up to like 32/64 bytes with SIMD registers, but right now
		// this helps find errors so it stays like this for now.
		return false;
	return true;
}

inline bool AddValue(Context *context, u32 valueIdx, DynamicArray<u32, ThreadAllocator> *array)
{
	IRSetValueFlags(context, valueIdx, VALUEFLAGS_IS_USED);

	// Nonsense to take these into account
	if (!CanBeRegister(context, valueIdx))
	{
		u32 valueFlags = IRGetValue(context, valueIdx).flags;
		if (!(valueFlags & VALUEFLAGS_ON_STATIC_STORAGE |
					VALUEFLAGS_IS_EXTERNAL))
		{
			IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
			DynamicArrayAddUnique(&jobData->spilledValues, valueIdx);
		}
		return false;
	}

	return DynamicArrayAddUnique(array, valueIdx);
}

// @Speed: delete? this will most likely get inlined anyways
inline bool AddIfValue(Context *context, IRValue irValue, DynamicArray<u32, ThreadAllocator> *array)
{
	if (irValue.valueType != IRVALUETYPE_VALUE &&
			irValue.valueType != IRVALUETYPE_VALUE_DEREFERENCE)
		return false;

	bool mainValueAdded = AddValue(context, irValue.value.valueIdx, array);
	bool indexValueAdded = false;

	if (irValue.value.elementSize > 0)
		indexValueAdded = AddValue(context, irValue.value.indexValueIdx, array);

	return mainValueAdded || indexValueAdded;
}

inline void RemoveIfValue(Context *context, IRValue irValue,
		DynamicArray<u32, ThreadAllocator> *array)
{
	if (irValue.valueType == IRVALUETYPE_VALUE)
	{
		for (int i = 0; i < array->size; ++i)
		{
			if ((*array)[i] == irValue.value.valueIdx)
			{
				(*array)[i] = (*array)[--array->size];
				break;
			}
		}
	}
	else if (irValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
	{
		// The value is actually _used_ here, and not written to. Add instead.
		AddValue(context, irValue.value.valueIdx, array);
		if (irValue.value.elementSize > 0)
			AddValue(context, irValue.value.indexValueIdx, array);
	}
}

inline bool IsXMMFast(IRJobData *jobData, u32 valueIdx)
{
	if (valueIdx >= RAX.value.valueIdx && valueIdx <= R15.value.valueIdx)
		return false;
	if (valueIdx >= XMM0.value.valueIdx && valueIdx <= XMM15.value.valueIdx)
		return true;
	return BitfieldGetBit(jobData->valueIsXmmBits, valueIdx);
}

void DoLivenessAnalisisOnInstruction(Context *context, BasicBlock *basicBlock, X64Instruction *inst,
		DynamicArray<u32, ThreadAllocator> *liveValues)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	Procedure *proc = &context->procedures.unsafe[jobData->procedureIdx];

	if (context->config.logAllocationInfo)
	{
		if (inst->type != X64_Patch && inst->type != X64_Patch_Many)
		{
			Print("\t");
			s64 s = Print("%S", X64InstructionToStr(context, *inst, &proc->localValues));
			if (s < 40)
			{
				char buffer[40];
				memset(buffer, ' ', sizeof(buffer));
				buffer[39] = 0;
				Print("%s", buffer + s);
			}
			for (int i = 0; i < liveValues->size; ++i)
				Print("%S, ", X64IRValueToStr(context, IRValueValue(context, (*liveValues)[i]),
							&proc->localValues));
			Print("\n");
		}
	}

	switch (inst->type)
	{
	// weird ones
	case X64_CALL:
	{
		ArrayInit(&inst->liveValues, liveValues->size);
		inst->liveValues.size = liveValues->size;
		for (int i = 0; i < liveValues->size; ++i)
			inst->liveValues[i] = (*liveValues)[i];

		u64 totalParameters = inst->parameterValues.size;
		for (int paramIdx = 0; paramIdx < totalParameters; ++paramIdx)
			AddValue(context, inst->parameterValues[paramIdx], liveValues);
	} break;
	case X64_CALL_Indirect:
	{
		ArrayInit(&inst->liveValues, liveValues->size);
		inst->liveValues.size = liveValues->size;
		for (int i = 0; i < liveValues->size; ++i)
			inst->liveValues[i] = (*liveValues)[i];

		AddIfValue(context, inst->dst, liveValues);

		u64 totalParameters = inst->parameterValues.size;
		for (int paramIdx = 0; paramIdx < totalParameters; ++paramIdx)
			AddValue(context, inst->parameterValues[paramIdx], liveValues);
	} break;
	case X64_DIV:
	case X64_IDIV:
	case X64_MUL:
	{
		AddValue(context, RAX.value.valueIdx, liveValues);
		AddValue(context, RDX.value.valueIdx, liveValues);
		AddIfValue(context, inst->dst, liveValues);
	} break;
	case X64_CQO:
	{
		// CQO writes to both RAX and RDX
		for (int i = 0; i < liveValues->size; ++i)
		{
			if ((*liveValues)[i] == RAX.value.valueIdx || (*liveValues)[i] == RDX.value.valueIdx)
				(*liveValues)[i--] = (*liveValues)[--liveValues->size];
		}
	} break;
	case X64_XOR:
	case X64_XORPS:
	case X64_XORPD:
	{
		// Detect xors of same thing (zero-ing)
		if (inst->src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER &&
			memcmp(&inst->dst.value, &inst->src.value, sizeof(inst->src.value)) == 0)
			RemoveIfValue(context, inst->dst, liveValues);
		else
		{
			AddIfValue(context, inst->dst, liveValues);
			AddIfValue(context, inst->src, liveValues);
		}
	} break;
	case X64_Push_Value:
	{
		// @Improve: This sucks a little bit.
		IRSetValueFlags(context, inst->valueIdx, VALUEFLAGS_HAS_PUSH_INSTRUCTION);
	} break;
	case X64_Patch:
	case X64_Patch_Many:
	{
		ASSERT(!"Patches not supported here");
	} break;
	default:
	{
		X64InstructionInfo instInfo = x64InstructionInfos[inst->type];
		if (instInfo.operandAccessLeft & OPERANDACCESS_READ)
			AddIfValue   (context, inst->dst, liveValues);
		else if (instInfo.operandAccessLeft & OPERANDACCESS_WRITE)
			RemoveIfValue(context, inst->dst, liveValues);

		if (instInfo.operandAccessRight & OPERANDACCESS_READ)
			AddIfValue   (context, inst->src, liveValues);
		else if (instInfo.operandAccessRight & OPERANDACCESS_WRITE)
			RemoveIfValue(context, inst->src, liveValues);
	}
	}

	// Add edges to graph
	u64 liveValuesCount = liveValues->size;
	for (int liveValueIdx = 0; liveValueIdx < liveValuesCount; ++liveValueIdx)
	{
		u32 valueIdx = (*liveValues)[liveValueIdx];

		u32 nodeIdx;

		// Find a node in the graph with this value, or create it if there isn't one
		u32 *found = HashMapGet(jobData->beInterferenceGraph.valueToNodeMap, valueIdx);
		if (found)
			nodeIdx = *found;
		else
		{
			// No node found, create one
			nodeIdx = jobData->beInterferenceGraph.count++;
			if (nodeIdx >= jobData->beInterferenceGraph.capacity)
			{
				u64 oldCapacity = jobData->beInterferenceGraph.capacity;
				jobData->beInterferenceGraph.capacity *= 2;
				jobData->beInterferenceGraph.valueIndices = (u32 *)
						ThreadAllocator::Realloc(jobData->beInterferenceGraph.valueIndices,
						sizeof(jobData->beInterferenceGraph.valueIndices[0]) * oldCapacity,
						sizeof(jobData->beInterferenceGraph.valueIndices[0]) *
						jobData->beInterferenceGraph.capacity, alignof(u32));
				jobData->beInterferenceGraph.removed = (u8 *)
						ThreadAllocator::Realloc(jobData->beInterferenceGraph.removed,
						sizeof(jobData->beInterferenceGraph.removed[0]) * oldCapacity,
						sizeof(jobData->beInterferenceGraph.removed[0]) *
						jobData->beInterferenceGraph.capacity, alignof(u8));
				jobData->beInterferenceGraph.edges = (HashSet<u32, ThreadAllocator> *)
						ThreadAllocator::Realloc(jobData->beInterferenceGraph.edges,
						sizeof(jobData->beInterferenceGraph.edges[0]) * oldCapacity,
						sizeof(jobData->beInterferenceGraph.edges[0]) *
						jobData->beInterferenceGraph.capacity, alignof(HashSet<u32, ThreadAllocator>));
			}
			jobData->beInterferenceGraph.valueIndices[nodeIdx] = valueIdx;
			jobData->beInterferenceGraph.removed[nodeIdx]      = false;
			HashSetInit(&jobData->beInterferenceGraph.edges[nodeIdx], 32);

			*HashMapGetOrAdd(&jobData->beInterferenceGraph.valueToNodeMap, valueIdx) = nodeIdx;
		}

		HashSet<u32, ThreadAllocator> *edges = &jobData->beInterferenceGraph.edges[nodeIdx];
		bool isXMM = IsXMMFast(jobData, valueIdx);
		for (int j = 0; j < liveValuesCount; ++j)
		{
			if (liveValueIdx == j) continue;
			u32 edgeValueIdx = (*liveValues)[j];
			bool edgeIsXMM = IsXMMFast(jobData, edgeValueIdx);
			// Add only other values that compete for the same pool of registers.
			// Floating point values use a different set of registers (xmmX).
			if (isXMM == edgeIsXMM)
				HashSetAdd(edges, edgeValueIdx);
		}

		// No live values that cross a procedure call can be stored in RAX/XMM0.
		// Note that this doesn't make RAX and XMM0 _live_ but just flag them as co-existing with
		// all the currently live values.
		if (inst->type == X64_CALL || inst->type == X64_CALL_Indirect)
		{
			HashSetAdd(edges, RAX.value.valueIdx);
			HashSetAdd(edges, XMM0.value.valueIdx);
		}
	}
}

void DoLivenessAnalisis(Context *context, BasicBlock *basicBlock,
		DynamicArray<u32, ThreadAllocator> *liveValues)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	if (context->config.logAllocationInfo)
		Print("Doing liveness analisis on block %S %d-%d\n",
				GetProcedureRead(context, jobData->procedureIdx).name,
				basicBlock->beginIdx, basicBlock->endIdx);

	for (int i = 0; i < basicBlock->liveValuesAtOutput.size; ++i)
		DynamicArrayAddUnique(liveValues, basicBlock->liveValuesAtOutput[i]);

	for (int i = 0; i < liveValues->size; ++i)
		DynamicArrayAddUnique(&basicBlock->liveValuesAtOutput, (*liveValues)[i]);

	for (int i = 0; i < jobData->returnValueIndices.size; ++i)
		AddValue(context, jobData->returnValueIndices[i], liveValues);

	// Check all basic block instructions
	for (s64 instructionIdx = basicBlock->endIdx; instructionIdx >= basicBlock->beginIdx;
			--instructionIdx)
	{
		X64Instruction *inst = &jobData->beInstructions[instructionIdx];
		DoLivenessAnalisisOnInstruction(context, basicBlock, inst, liveValues);
	}

	bool somethingChanged = false;
	for (int i = 0; i < liveValues->size; ++i)
	{
		if (DynamicArrayAddUnique(&basicBlock->liveValuesAtInput, (*liveValues)[i]))
			somethingChanged = true;
	}
	if (!somethingChanged && basicBlock->livenessAnalizedOnce)
		return;

	basicBlock->livenessAnalizedOnce = true;

	for (int i = 0; i < basicBlock->inputs.size; ++i)
	{
		BasicBlock *inputBlock = basicBlock->inputs[i];
		// Copy live registers array
		DynamicArray<u32, ThreadAllocator> liveValuesCopy;
		DynamicArrayInit(&liveValuesCopy, liveValues->capacity);
		DynamicArrayCopy(&liveValuesCopy, liveValues);

		DoLivenessAnalisis(context, inputBlock, &liveValuesCopy);
	}
}

void GenerateBasicBlocks(Context *context)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	Procedure *proc = &context->procedures.unsafe[jobData->procedureIdx];

	if (context->config.logAllocationInfo)
		Print("GENERATING BASIC BLOCKS FOR %S\n",
				GetProcedureRead(context, jobData->procedureIdx).name);

	BasicBlock *currentBasicBlock = PushBasicBlock(nullptr, &jobData->beBasicBlocks);

	u64 instructionCount = jobData->beInstructions.count;
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
	{
		X64Instruction inst = jobData->beInstructions[instructionIdx];

		if (context->config.logAllocationInfo)
			Print("\t%S\n", X64InstructionToStr(context, inst, &proc->localValues));

		if (inst.type >= X64_Jump_Begin && inst.type <= X64_Jump_End)
		{
			if (context->config.logAllocationInfo)
				Print("- Split\n");

			currentBasicBlock->endIdx = instructionIdx;
			BasicBlock *previousBlock = currentBasicBlock;
			currentBasicBlock = PushBasicBlock(currentBasicBlock, &jobData->beBasicBlocks);

			// Only on conditional jumps, add previous block as input too.
			if (inst.type != X64_JMP)
			{
				*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
				*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
			}
		}
		else switch (inst.type)
		{
		case X64_Label:
		{
			if (context->config.logAllocationInfo)
				Print("- Split\n");

			currentBasicBlock->endIdx = instructionIdx - 1;
			BasicBlock *previousBlock = currentBasicBlock;
			currentBasicBlock = PushBasicBlock(currentBasicBlock, &jobData->beBasicBlocks);
			*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
			*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
		} break;
		case X64_Patch:
		case X64_Patch_Many:
			ASSERT(!"Patches not supported here!");
		}
	}

	currentBasicBlock->endIdx = instructionCount - 1;
	jobData->beLeafBasicBlock = currentBasicBlock;

	if (context->config.logAllocationInfo)
		Print("- End\n\n");

	// Link basic blocks together
	const u64 basicBlockCount = jobData->beBasicBlocks.count;
	for (int i = 0; i < basicBlockCount; ++i)
	{
		BasicBlock *jumpBlock = &jobData->beBasicBlocks[i];

		IRLabel *label = nullptr;
		X64Instruction endInstruction = jobData->beInstructions[jumpBlock->endIdx];
		if (endInstruction.type >= X64_Jump_Begin && endInstruction.type <= X64_Jump_End)
			label = endInstruction.label;
		else
			continue;

		for (int j = 0; j < basicBlockCount; ++j)
		{
			BasicBlock *labelBlock = &jobData->beBasicBlocks[j];
			X64Instruction beginInstruction =
				jobData->beInstructions[labelBlock->beginIdx];

			if (beginInstruction.type == X64_Label && beginInstruction.label == label)
			{
				*DynamicArrayAdd(&jumpBlock->outputs) = labelBlock;
				*DynamicArrayAdd(&labelBlock->inputs) = jumpBlock;
				goto foundBlock;
			}
		}
		ASSERT(!"Couldn't find basic block beggining with label!");
foundBlock:
		continue;
	}
}

void ResolveStackOffsets(Context *context)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);

	DynamicArray<s64, ThreadAllocator> stack;
	DynamicArrayInit(&stack, 16);

	s64 stackCursor = 0;

	// Allocate space for the parameters we pass on the stack to procedures we call.
	s64 allocParameters = jobData->allocatedParameterCount;
	if (allocParameters & 1) ++allocParameters;
	stackCursor += allocParameters * 8;
	if (stackCursor & 15)
		stackCursor = (stackCursor + 16) & (~15);

	// Allocate spilled values
	for (int spillIdx = 0; spillIdx < jobData->spilledValues.size; ++spillIdx)
	{
		u32 valueIdx = jobData->spilledValues[spillIdx];
		Value *value = IRGetLocalValue(context, valueIdx);
		ASSERT(!(value->flags & VALUEFLAGS_IS_ALLOCATED));

		// If the value has properly scoped allocation don't dumbly spill into stack.
		if (value->flags & VALUEFLAGS_HAS_PUSH_INSTRUCTION)
			continue;

		u64 size = GetTypeInfo(context, value->typeTableIdx).size;
		int alignment = size > 8 ? 8 : NextPowerOf2((int)size);
		if (stackCursor & (alignment - 1))
			stackCursor = (stackCursor + alignment) & ~(alignment - 1);
		ASSERT(stackCursor < S32_MAX);
		value->stackOffset = (s32)stackCursor;
		value->flags |= VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY;
		stackCursor += size;
	}

	jobData->stackSize = stackCursor;

	X64InstructionStream stream = X64InstructionStreamBegin(&jobData->beInstructions);
	X64Instruction *inst = X64InstructionStreamAdvance(&stream);
	while (inst)
	{
		switch (inst->type)
		{
		case X64_Push_Value:
		{
			Value *value = IRGetLocalValue(context, inst->valueIdx);
			ASSERT(value->flags & VALUEFLAGS_HAS_PUSH_INSTRUCTION);
			if (value->flags & VALUEFLAGS_IS_ALLOCATED)
			{
				ASSERT(!(value->flags & VALUEFLAGS_IS_MEMORY));
				goto next;
			}
			// We don't allocate static values, the assembler/linker does.
			ASSERT(!(value->flags & VALUEFLAGS_ON_STATIC_STORAGE));
			ASSERT(!(value->flags & VALUEFLAGS_IS_EXTERNAL));

			u64 size = GetTypeInfo(context, value->typeTableIdx).size;
			int alignment = size > 8 ? 8 : NextPowerOf2((int)size);
			if (stackCursor & (alignment - 1))
				stackCursor = (stackCursor + alignment) & ~(alignment - 1);
			ASSERT(stackCursor < S32_MAX);
			value->stackOffset = (s32)stackCursor;
			value->flags |= VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY;
			stackCursor += size;
		} break;
		case X64_Push_Scope:
		{
			*DynamicArrayAdd(&stack) = stackCursor;
		} break;
		case X64_Pop_Scope:
		{
			if (stackCursor > (s64)jobData->stackSize)
				jobData->stackSize = stackCursor;
			stackCursor = stack[--stack.size];
		} break;
		}
next:
		inst = X64InstructionStreamAdvance(&stream);
	}
	if (stackCursor > (s64)jobData->stackSize)
		jobData->stackSize = stackCursor;

	// Align stack to 16 bytes.
	if (jobData->stackSize & 15)
		jobData->stackSize = (jobData->stackSize & ~15) + 16;
}

inline u64 BitIfRegister(Context *context, IRValue irValue)
{
	if (irValue.valueType == IRVALUETYPE_VALUE || irValue.valueType == IRVALUETYPE_VALUE_DEREFERENCE)
	{
		Value value = IRGetValue(context, irValue.value.valueIdx);
		if ((value.flags & (VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY)) ==
				(VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED))
		{
			ASSERT(value.allocatedRegister < 64);
			return 1ull << value.allocatedRegister;
		}
	}
	return 0;
}

inline u64 RegisterSavingInstruction(Context *context, X64Instruction *inst, u64 usedRegisters)
{
	switch(inst->type)
	{
	case X64_CALL:
	case X64_CALL_Indirect:
	{
		// Caller save registers
		u64 liveRegisterBits = 0;
		for (int i = 0; i < inst->liveValues.size; ++i)
		{
			Value v = IRGetValue(context, inst->liveValues[i]);
			if ((v.flags & (VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY)) ==
					(VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED))
				liveRegisterBits |= (1ull << v.allocatedRegister);
		}

		u64 usedCalleeSaveRegisters = callerSaveRegisters & liveRegisterBits;
		s64 callerSaveRegCount = CountOnes64(usedCalleeSaveRegisters);
		X64Instruction patchInst = { {}, X64_Patch_Many };
		ArrayInit(&patchInst.patchInstructions, 1 + 3 * callerSaveRegCount);
		patchInst.patchInstructions.size = 1 + 3 * callerSaveRegCount;
		int count = 0;
		for (int i = 0; i < 64; ++i)
		{
			if (usedCalleeSaveRegisters & (1ull << i))
			{
				u32 newValueIdx = IRNewValue(context, "_save_reg"_s, TYPETABLEIDX_S64,
						VALUEFLAGS_IS_USED | VALUEFLAGS_FORCE_MEMORY |
						VALUEFLAGS_HAS_PUSH_INSTRUCTION);

				IRValue reg = x64Registers[i];

				X64InstructionType movType = i >= XMM0_idx ? X64_MOVSD : X64_MOV;

				X64Instruction pushInst = { {}, X64_Push_Value };
				pushInst.valueIdx = newValueIdx;
				X64Instruction saveInst = { {}, movType, IRValueValue(newValueIdx, TYPETABLEIDX_S64), reg };
				X64Instruction restoreInst = { {}, movType, reg, IRValueValue(newValueIdx, TYPETABLEIDX_S64) };

				patchInst.patchInstructions[count * 2] = pushInst;
				patchInst.patchInstructions[count * 2 + 1] = saveInst;
				patchInst.patchInstructions[callerSaveRegCount * 2 + 1 + count] = restoreInst;
				++count;
			}
		}

		patchInst.patchInstructions[callerSaveRegCount * 2] = *inst;
		*inst = patchInst;
	} break;
	default:
	{
		X64InstructionInfo instInfo = x64InstructionInfos[inst->type];
		if (instInfo.operandAccessLeft  != OPERANDACCESS_NONE)
			usedRegisters |= BitIfRegister(context, inst->dst);
		if (instInfo.operandAccessRight != OPERANDACCESS_NONE)
			usedRegisters |= BitIfRegister(context, inst->src);
	}
	}
	return usedRegisters;
}

void X64AllocateRegisters(Context *context)
{
	IRJobData *jobData = (IRJobData *)SYSGetFiberData(context->flsIndex);
	Procedure *proc = &context->procedures.unsafe[jobData->procedureIdx];

	BucketArrayInit(&jobData->beBasicBlocks);

	jobData->beInterferenceGraph = {};
	jobData->beInterferenceGraph.capacity = 128;
	jobData->beInterferenceGraph.valueIndices = (u32 *)
		ThreadAllocator::Alloc(sizeof(u32) * 128, alignof(u32));
	jobData->beInterferenceGraph.removed = (u8 *)
		ThreadAllocator::Alloc(sizeof(u8) * 128, alignof(u8));
	jobData->beInterferenceGraph.edges = (HashSet<u32, ThreadAllocator> *)
		ThreadAllocator::Alloc(sizeof(HashSet<u32, ThreadAllocator>) * 128, alignof(HashSet<u32,
					ThreadAllocator>));

	HashMapInit(&jobData->beInterferenceGraph.valueToNodeMap, 256);

	// Cache what values are to be stored in XMM registers
	// The main reasoning behind this is to avoid so many queries into cold type table data just to
	// see if each value is an xmm register or not.
	{
		u64 valueCount = proc->localValues.count;

		u64 qwordCount = valueCount >> 6;
		if (valueCount & 63) ++qwordCount;
		ArrayInit(&jobData->valueIsXmmBits, qwordCount);
		memset(jobData->valueIsXmmBits.data, 0, qwordCount * 8);
		jobData->valueIsXmmBits.size = qwordCount;

		for (int valueIdx = 1; valueIdx < valueCount; ++valueIdx)
		{
			u32 typeTableIdx = IRGetValue(context, valueIdx).typeTableIdx;
			if (typeTableIdx >= 0)
			{
				TypeInfo typeInfo = GetTypeInfo(context, StripAllAliases(context, typeTableIdx));
				bool isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;
				if (isXMM)
					BitfieldSetBit(jobData->valueIsXmmBits, valueIdx);
			}
		}
	}

	GenerateBasicBlocks(context);

	int availableRegisters = sizeof(x64ScratchRegisters) / sizeof(x64ScratchRegisters[0]);
	int availableRegistersFP = 16;

	// Do liveness analisis, starting from all leaf blocks
	BasicBlock *currentLeafBlock = jobData->beLeafBasicBlock;

#if USE_PROFILER_API
	String procName = GetProcedureRead(context, jobData->procedureIdx).name;
	ProfilerBegin("Liveness analisis", StringToCStr(procName, ThreadAllocator::Alloc), PERFORMANCEAPI_DEFAULT_COLOR);
#endif

	jobData->beInterferenceGraph.count = 0;
	HashMapClear(jobData->beInterferenceGraph.valueToNodeMap);

	// @Todo: iterative instead of recursive?
	DynamicArray<u32, ThreadAllocator> liveValues;
	DynamicArrayInit(&liveValues, 32);
	DoLivenessAnalisis(context, currentLeafBlock, &liveValues);

	ProfilerEnd();

	InterferenceGraph interferenceGraph = jobData->beInterferenceGraph;

	if (context->config.logAllocationInfo)
	{
		for (u32 nodeIdx = 0; nodeIdx < interferenceGraph.count; ++nodeIdx)
		{
			u32 currentNodeValueIdx = interferenceGraph.valueIndices[nodeIdx];
			HashSet<u32, ThreadAllocator> currentNodeEdges = interferenceGraph.edges[nodeIdx];
			Print("Value %S coexists with: ", X64IRValueToStr(context,
						IRValueValue(context, currentNodeValueIdx), &proc->localValues));

			u32 *keys = HashSetKeys(currentNodeEdges);
			for (u32 slotIdx = 0; slotIdx < currentNodeEdges.capacity; ++slotIdx)
				if (HashSetSlotOccupied(currentNodeEdges, slotIdx))
					Print("%S, ", X64IRValueToStr(context,
								IRValueValue(context, keys[slotIdx]), &proc->localValues));
			Print("\n");
		}
	}

	Array<u32, ThreadAllocator> nodeStack;
	ArrayInit(&nodeStack, interferenceGraph.count);

	// Allocate values to registers when possible
	while (nodeStack.size < interferenceGraph.count)
	{
		u32 nodeToRemoveIdx = U32_MAX;
		s64 mostEdges;

		// Remove nodes that have a number of edges that fit in the available registers
		for (u32 nodeIdx = 0; nodeIdx < interferenceGraph.count; ++nodeIdx)
		{
			if (interferenceGraph.removed[nodeIdx])
				continue;
			if (HashSetCount(interferenceGraph.edges[nodeIdx]) < availableRegisters)
			{
				nodeToRemoveIdx = nodeIdx;
				goto gotNodeToRemove;
			}
		}

		// Here we pick one that we're probably going to spill. Choose the one with most edges.
		mostEdges = -1;
		for (u32 nodeIdx = 0; nodeIdx < interferenceGraph.count; ++nodeIdx)
		{
			if (interferenceGraph.removed[nodeIdx])
				continue;

			u32 valueIdx = interferenceGraph.valueIndices[nodeIdx];
			// Skip physical register values
			if (valueIdx >= RAX.value.valueIdx && valueIdx <= XMM15.value.valueIdx)
				continue;
			u32 vFlags = IRGetLocalValue(context, valueIdx)->flags;
			if (vFlags & VALUEFLAGS_FORCE_REGISTER)
				continue;

			s64 edgeCount = HashSetCount(interferenceGraph.edges[nodeIdx]);
			if (edgeCount > mostEdges)
			{
				nodeToRemoveIdx = nodeIdx;
				mostEdges = edgeCount;
			}
		}
		if (mostEdges >= 0)
			goto gotNodeToRemove;

		// Pick a fallback node that might be flagged as no-spill. When adding back nodes, we
		// might get lucky and not spill it.
		for (u32 nodeIdx = 0; nodeIdx < interferenceGraph.count; ++nodeIdx)
		{
			if (interferenceGraph.removed[nodeIdx])
				continue;

			nodeToRemoveIdx = nodeIdx;
		}
gotNodeToRemove:

		u32 nodeCount = interferenceGraph.count;
		u32 parameterValuesBegin = jobData->x64SpilledParametersRead[0];
		u32 parameterValuesEnd = jobData->x64SpilledParametersWrite[31];
		// We assume we allocated the read parameter values first...
		ASSERT(jobData->x64SpilledParametersRead[0] < jobData->x64SpilledParametersWrite[0]);
		for (u32 nodeIdx = 0; nodeIdx < nodeCount; ++nodeIdx)
		{
			if (interferenceGraph.removed[nodeIdx])
				continue;

			HashSet<u32, ThreadAllocator> *edges = &interferenceGraph.edges[nodeIdx];
			u32 valueIdx = interferenceGraph.valueIndices[nodeToRemoveIdx];
			if (!(valueIdx & VALUE_GLOBAL_BIT) &&
				(valueIdx < parameterValuesBegin || valueIdx > parameterValuesEnd) &&
				HashSetHas(*edges, valueIdx))
			{
				// The only allocated things thus far should be physical register values and stack
				// parameter values.
				ASSERT(!(IRGetLocalValue(context, valueIdx)->flags & VALUEFLAGS_IS_ALLOCATED));
				HashSetRemove(edges, valueIdx);
			}
		}
		interferenceGraph.removed[nodeToRemoveIdx] = true;
		*ArrayAdd(&nodeStack) = nodeToRemoveIdx;
	}

	for (int nodeIdx = (int)nodeStack.size - 1; nodeIdx >= 0; --nodeIdx)
	{
		u32 currentNodeIdx = nodeStack[nodeIdx];
		u32 valueIdx = interferenceGraph.valueIndices[currentNodeIdx];

		// Skip physical register values
		if (valueIdx >= RAX.value.valueIdx && valueIdx <= XMM15.value.valueIdx)
			continue;

		Value *v = IRGetLocalValue(context, valueIdx);
		const HashSet<u32, ThreadAllocator> edges = interferenceGraph.edges[currentNodeIdx];
		const u32 *edgesKeys = HashSetKeys(edges);

		// We don't allocate static values, the assembler/linker does.
		ASSERT(!(v->flags & VALUEFLAGS_ON_STATIC_STORAGE));
		ASSERT(!(v->flags & VALUEFLAGS_IS_EXTERNAL));

		if (v->flags & VALUEFLAGS_IS_ALLOCATED)
			continue;

		bool isXMM = BitfieldGetBit(jobData->valueIsXmmBits, valueIdx);

		if (v->flags & VALUEFLAGS_TRY_IMMITATE)
		{
			u32 immitateValueIdx = v->tryImmitateValueIdx;

			// Can't immitate a global value
			if (immitateValueIdx & VALUE_GLOBAL_BIT)
				goto skipImmitate;

			Value *immitateValue = IRGetLocalValue(context, immitateValueIdx);
#if 0
			while (immitateValue->flags & VALUEFLAGS_TRY_IMMITATE &&
				   immitateValueIdx != immitateValue->tryImmitateValueIdx)
			{
				immitateValueIdx = immitateValue->tryImmitateValueIdx;
				immitateValue = IRGetLocalValue(context, immitateValueIdx);
			}
#endif

			if ((immitateValue->flags & VALUEFLAGS_IS_ALLOCATED) &&
			  !(immitateValue->flags & VALUEFLAGS_IS_MEMORY))
			{
				bool isOtherXMM = BitfieldGetBit(jobData->valueIsXmmBits, immitateValueIdx);
				if (isXMM != isOtherXMM)
					goto skipImmitate;

				// Check the candidate is not used on any edge, and that the value we're trying
				// to copy doesn't coexist with this one.
				s32 candidate = immitateValue->allocatedRegister;
				if (HashSetHas(edges, immitateValueIdx))
					goto skipImmitate;

				for (u32 slotIdx = 0; slotIdx < edges.capacity; ++slotIdx)
				{
					if (!HashSetSlotOccupied(edges, slotIdx))
						continue;
					Value edgeValue = IRGetValue(context, edgesKeys[slotIdx]);
					if (!(edgeValue.flags & VALUEFLAGS_IS_ALLOCATED) ||
						  edgeValue.flags & VALUEFLAGS_IS_MEMORY)
						continue;
					if (edgeValue.allocatedRegister == candidate)
						goto skipImmitate;
				}

				v->allocatedRegister = candidate;
				v->flags &= ~VALUEFLAGS_IS_MEMORY;
				v->flags |= VALUEFLAGS_IS_ALLOCATED;
				continue;
			}
			else if (!(immitateValue->flags & VALUEFLAGS_IS_ALLOCATED) &&
					 !(immitateValue->flags & VALUEFLAGS_TRY_IMMITATE) &&
					 CanBeRegister(context, immitateValueIdx))
			{
				immitateValue->flags |= VALUEFLAGS_TRY_IMMITATE;
				immitateValue->tryImmitateValueIdx = valueIdx;
			}
		}
skipImmitate:

		int max = isXMM ? availableRegistersFP : availableRegisters;
		u64 usedRegisters = 0;
		for (u32 slotIdx = 0; slotIdx < edges.capacity; ++slotIdx)
		{
			if (!HashSetSlotOccupied(edges, slotIdx))
				continue;
			Value edgeValue = IRGetValue(context, edgesKeys[slotIdx]);
			if ((edgeValue.flags & VALUEFLAGS_IS_ALLOCATED) &&
			   !(edgeValue.flags & VALUEFLAGS_IS_MEMORY))
			{
				usedRegisters |= 1ull << edgeValue.allocatedRegister;
			}
		}
		for (int candidateIdx = 0; candidateIdx < max; ++candidateIdx)
		{
			X64Register candidate = (X64Register)(isXMM ? XMM0_idx + candidateIdx :
					x64ScratchRegisters[candidateIdx]);
			u64 registerBit = 1ull << candidate;
			if (!(usedRegisters & registerBit))
			{
#if DEBUG_BUILD
				TypeInfo t = GetTypeInfo(context, StripAllAliases(context, v->typeTableIdx));
				if (t.typeCategory == TYPECATEGORY_FLOATING || t.size > 8)
					ASSERT(candidate >= XMM0_idx);
				else
					ASSERT(candidate < XMM0_idx);
#endif
				v->allocatedRegister = candidate;
				v->flags &= ~VALUEFLAGS_IS_MEMORY;
				v->flags |= VALUEFLAGS_IS_ALLOCATED;
				break;
			}
		}
		if (!(v->flags & VALUEFLAGS_IS_ALLOCATED))
		{
			if (v->flags & VALUEFLAGS_FORCE_REGISTER)
			{
				ASSERT(!"Can't allocate value to register!");
				continue;
			}

			// Spill!
			*DynamicArrayAdd(&jobData->spilledValues) = valueIdx;
		}
	}

	// Do register saving
	u64 usedRegisters = 0;
	X64InstructionStream stream = X64InstructionStreamBegin(&jobData->beInstructions);
	X64Instruction *inst = X64InstructionStreamAdvance(&stream);
	while (inst)
	{
		usedRegisters = RegisterSavingInstruction(context, inst, usedRegisters);
		inst = X64InstructionStreamAdvance(&stream);
	}

	// Don't save registers used to return values
	u32 procTypeIdx = GetProcedureRead(context, jobData->procedureIdx).typeTableIdx;
	TypeInfoProcedure procTypeInfo = GetTypeInfo(context, procTypeIdx).procedureInfo;
	u64 returnValueCount = procTypeInfo.returnTypeIndices.size;
	for (int i = 1; i < returnValueCount; ++i)
	{
		static u64 returnRegisters[] = {
			0b00000000000000000000000000000001, // RDI
			0b00000000000000000000000000100000, // RDI
			0b00000000000000000000000000010000, // RSI
			0b00000000000000000000000000000100, // RDX
			0b00000000000000000000000000000010, // RCX
			0b00000000000000000000000100000000, // R8
			0b00000000000000000000001000000000, // R9
		};
		static u64 returnRegistersXMM[] = {
			0b00000000000000010000000000000000, // XMM0
			0b00000000000000100000000000000000, // XMM1
			0b00000000000001000000000000000000, // XMM2
			0b00000000000010000000000000000000, // XMM3
			0b00000000000100000000000000000000, // XMM4
			0b00000000001000000000000000000000, // XMM5
			0b00000000010000000000000000000000, // XMM6
			0b00000000100000000000000000000000, // XMM7
		};
		/* For reference
		IRValue x64Registers[X64REGISTER_Count] = {
			RAX,	RCX,	RDX,	RBX,
			RSI,	RDI,	RSP,	RBP,
			R8,		R9,		R10,	R11,
			R12,	R13,	R14,	R15,
			XMM0,	XMM1,	XMM2,	XMM3,
			XMM4,	XMM5,	XMM6,	XMM7
			XMM8,	XMM9,	XMM10,	XMM11,
			XMM12,	XMM13,	XMM14,	XMM15
		};*/
		u32 typeIdx = procTypeInfo.returnTypeIndices[i];
		if (GetTypeInfo(context, typeIdx).typeCategory == TYPECATEGORY_FLOATING)
		{
			if (i < ArrayCount(returnRegistersXMM))
				usedRegisters &= ~returnRegistersXMM[i];
		}
		else
		{
			if (i < ArrayCount(returnRegisters))
				usedRegisters &= ~returnRegisters[i];
		}
	}

	// Callee save registers
	u64 usedCallerSaveRegisters = calleeSaveRegisters & usedRegisters;
	s64 calleeSaveRegCount = CountOnes64(usedCallerSaveRegisters);
	X64Instruction patchTop =    { {}, X64_Patch_Many };
	X64Instruction patchBottom = { {}, X64_Patch_Many };
	ArrayInit(&patchTop.patchInstructions, 1 + calleeSaveRegCount);
	ArrayInit(&patchBottom.patchInstructions, 1 + calleeSaveRegCount);

	u64 instructionCount = jobData->beInstructions.count;
	*ArrayAdd(&patchBottom.patchInstructions) = jobData->beInstructions[instructionCount - 1];

	for (int i = 0; i < 64; ++i)
	{
		if (usedCallerSaveRegisters & (1ull << i))
		{
			u32 newValueIdx = IRNewValue(context, "_save_reg"_s, TYPETABLEIDX_S64,
					VALUEFLAGS_IS_USED | VALUEFLAGS_FORCE_MEMORY);
			*DynamicArrayAdd(&jobData->spilledValues) = newValueIdx;

			IRValue reg = x64Registers[i];

			X64InstructionType movType = i >= XMM0_idx ? X64_MOVSD : X64_MOV;

			X64Instruction *saveInst = ArrayAdd(&patchTop.patchInstructions);
			*saveInst = { {}, movType, IRValueValue(newValueIdx, TYPETABLEIDX_S64),
				reg };

			X64Instruction *restoreInst = ArrayAdd(&patchBottom.patchInstructions);
			*restoreInst = { {}, movType, reg, IRValueValue(newValueIdx, TYPETABLEIDX_S64) };
		}
	}

	*ArrayAdd(&patchTop.patchInstructions) = jobData->beInstructions[0];
	jobData->beInstructions[0] = patchTop;
	jobData->beInstructions[instructionCount - 1] = patchBottom;

	ResolveStackOffsets(context);
}
