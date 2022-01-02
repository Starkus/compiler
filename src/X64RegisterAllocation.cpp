const u64 calleeSaveRegisters = 0b001111110000111100000110;
const u64 callerSaveRegisters = 0b110000001111000011111000;
/* For reference
IRValue x64Registers[X64REGISTER_Count] = {
	RAX,	RCX,	RDX,	RBX,
	RSI,	RDI,	RSP,	RBP,
	R8,		R9,		R10,	R11,
	R12,	R13,	R14,	R15,
	XMM0,	XMM1,	XMM2,	XMM3,
	XMM4,	XMM5,	XMM6,	XMM7
};*/

struct BasicBlock
{
	X64Procedure *procedure;
	s64 beginIdx;
	s64 endIdx;
	bool livenessAnalizedOnce;
	DynamicArray<BasicBlock *, FrameAlloc, FrameRealloc> inputs;
	DynamicArray<BasicBlock *, FrameAlloc, FrameRealloc> outputs;

	// @Todo: bitmaps
	DynamicArray<u32, FrameAlloc, FrameRealloc> liveValuesAtInput;
	DynamicArray<u32, FrameAlloc, FrameRealloc> liveValuesAtOutput;
};

struct InterferenceGraphNode
{
	u32 valueIdx;
	DynamicArray<u32, FrameAlloc, FrameRealloc> edges; // @Improve: eugh
	bool removed;
};

void X64Patch(Context *context, X64Instruction *original, X64Instruction newInst)
{
	X64Instruction *patch1 = BucketArrayAdd(&context->bePatchedInstructions);
	*patch1 = newInst;
	X64Instruction *patch2 = BucketArrayAdd(&context->bePatchedInstructions);
	*patch2 = *original;

	X64Instruction patchInst = { X64_Patch };
	patchInst.patch1 = patch1;
	patchInst.patch2 = patch2;
	*original = patchInst;
}

BasicBlock *PushBasicBlock(BasicBlock *currentBasicBlock,
		BucketArray<BasicBlock, 512, malloc, realloc> *basicBlocks)
{

	X64Procedure *procedure = nullptr;
	s64 endOfLastBlock = -1;
	if (currentBasicBlock)
	{
		procedure = currentBasicBlock->procedure;
		endOfLastBlock = currentBasicBlock->endIdx;
	}

	BasicBlock *result = BucketArrayAdd(basicBlocks);
	*result = {};

	result->procedure = procedure;
	result->beginIdx = endOfLastBlock + 1;
	DynamicArrayInit(&result->inputs, 4);
	DynamicArrayInit(&result->outputs, 4);
	DynamicArrayInit(&result->liveValuesAtInput,  8);
	DynamicArrayInit(&result->liveValuesAtOutput, 8);

	return result;
}

bool CanBeRegister(Context *context, u32 valueIdx)
{
	Value v = context->values[valueIdx];
	if (v.flags & (VALUEFLAGS_FORCE_MEMORY | VALUEFLAGS_ON_STATIC_STORAGE |
				VALUEFLAGS_IS_EXTERNAL))
		return false;
	if (v.typeTableIdx == TYPETABLEIDX_128)
		return true;
	TypeInfo typeInfo = context->typeTable[v.typeTableIdx];
	if (typeInfo.typeCategory == TYPECATEGORY_STRUCT ||
		typeInfo.typeCategory == TYPECATEGORY_UNION)
		return false;
	if (!IsPowerOf2(typeInfo.size) || typeInfo.size > 8)
		return false;
	return true;
}

inline bool AddValue(Context *context, u32 valueIdx, X64Procedure *proc,
		DynamicArray<u32, FrameAlloc, FrameRealloc> *array)
{
	context->values[valueIdx].flags |= VALUEFLAGS_IS_USED;

	// Nonsense to take these into account
	if (!CanBeRegister(context, valueIdx))
	{
		if (!(context->values[valueIdx].flags & VALUEFLAGS_ON_STATIC_STORAGE |
					VALUEFLAGS_IS_EXTERNAL))
			DynamicArrayAddUnique(&proc->spilledValues, valueIdx);
		return false;
	}

	return DynamicArrayAddUnique(array, valueIdx);
}

// @Speed: delete? this will most likely get inlined anyways
inline bool AddIfValue(Context *context, IRValue value, X64Procedure *proc,
		DynamicArray<u32, FrameAlloc, FrameRealloc> *array)
{
	if (value.valueType != IRVALUETYPE_VALUE && value.valueType != IRVALUETYPE_MEMORY)
		return false;

	return AddValue(context, value.valueIdx, proc, array);
}

inline void RemoveIfValue(Context *context, IRValue value, X64Procedure *proc,
		DynamicArray<u32, FrameAlloc, FrameRealloc> *array)
{
	if (value.valueType == IRVALUETYPE_VALUE)
	{
		for (int i = 0; i < array->size; ++i)
		{
			if ((*array)[i] == value.valueIdx)
			{
				(*array)[i] = (*array)[--array->size];
				break;
			}
		}
	}
	else if (value.valueType == IRVALUETYPE_MEMORY)
	{
		// The value is actually _used_ here, and not written to. Add instead.
		AddValue(context, value.valueIdx, proc, array);
	}
}

void DoLivenessAnalisisOnInstruction(Context *context, BasicBlock *basicBlock, X64Instruction *inst,
		DynamicArray<u32, FrameAlloc, FrameRealloc> *liveValues)
{
	if (context->config.logAllocationInfo)
	{
		if (inst->type != X64_Patch && inst->type != X64_Patch_Many)
		{
			Print("\t");
			s64 s = Print("%S", X64InstructionToStr(context, *inst));
			if (s < 40)
			{
				char buffer[40];
				memset(buffer, ' ', sizeof(buffer));
				buffer[39] = 0;
				Print("%s", buffer + s);
			}
			for (int i = 0; i < liveValues->size; ++i)
				Print("%S, ", X64IRValueToStr(context, IRValueValue(context, (*liveValues)[i])));
			Print("\n");
		}
	}

	switch (inst->type)
	{
	// weird ones
	case X64_DIV:
	case X64_IDIV:
	case X64_MUL:
	{
		AddValue(context, RAX.valueIdx, basicBlock->procedure, liveValues);
		AddValue(context, RDX.valueIdx, basicBlock->procedure, liveValues);
		AddIfValue(context, inst->dst, basicBlock->procedure, liveValues);
	} break;
	case X64_CQO:
	{
		// CQO writes to both RAX and RDX
		for (int i = 0; i < liveValues->size; ++i)
		{
			if ((*liveValues)[i] == RAX.valueIdx || (*liveValues)[i] == RDX.valueIdx)
				(*liveValues)[i--] = (*liveValues)[--liveValues->size];
		}
	} break;
	case X64_XORPS:
	case X64_XORPD:
	{
		// Detect xors of same thing (zero-ing)
		if (inst->src.valueType != IRVALUETYPE_IMMEDIATE_INTEGER &&
			inst->dst.valueIdx == inst->src.valueIdx)
			RemoveIfValue(context, inst->dst, basicBlock->procedure, liveValues);
		else
		{
			AddIfValue(context, inst->dst, basicBlock->procedure, liveValues);
			AddIfValue(context, inst->src, basicBlock->procedure, liveValues);
		}
	} break;
	// dst write, src read
	case X64_MOV:
	case X64_MOVZX:
	case X64_MOVSX:
	case X64_MOVSXD:
	case X64_MOVSS:
	case X64_MOVSD:
	case X64_LEA:
	case X64_SQRTSS:
	case X64_SQRTSD:
	case X64_CVTSI2SS:
	case X64_CVTSI2SD:
	case X64_CVTTSS2SI:
	case X64_CVTTSD2SI:
	case X64_CVTSS2SD:
	case X64_CVTSD2SS:
	case X64_MOVUPS:
	case X64_MOVAPS:
	{
		RemoveIfValue(context, inst->dst, basicBlock->procedure, liveValues);
		AddIfValue   (context, inst->src, basicBlock->procedure, liveValues);
	} break;
	// dst read/write, src read
	case X64_ADD:
	case X64_SUB:
	case X64_IMUL:
	case X64_SAR:
	case X64_SAL:
	case X64_AND:
	case X64_OR:
	case X64_XOR:
	case X64_ADDSS:
	case X64_ADDSD:
	case X64_SUBSS:
	case X64_SUBSD:
	case X64_MULSS:
	case X64_MULSD:
	case X64_DIVSS:
	case X64_DIVSD:
	// dst read, src read
	case X64_CMP:
	case X64_COMISS:
	case X64_COMISD:
	{
		AddIfValue(context, inst->dst, basicBlock->procedure, liveValues);
		AddIfValue(context, inst->src, basicBlock->procedure, liveValues);
	} break;
	// dst read/write
	case X64_NOT:
	case X64_NEG:
	{
		AddIfValue(context, inst->dst, basicBlock->procedure, liveValues);
	} break;
	// nothing
	case X64_CALL:
	{
		ArrayInit(&inst->liveValues, liveValues->size, FrameAlloc);
		inst->liveValues.size = liveValues->size;
		for (int i = 0; i < liveValues->size; ++i)
			inst->liveValues[i] = (*liveValues)[i];

		Procedure *proc = GetProcedure(context, inst->procedureIdx);
		ASSERT(context->typeTable[proc->typeTableIdx].typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = context->typeTable[proc->typeTableIdx].procedureInfo;

		int paramIdx = 0;
		// Take into account return value pointer in RCX
		if (proc->returnValueIdx != U32_MAX && IRShouldPassByCopy(context, procTypeInfo.returnTypeTableIdx))
		{
			AddValue(context, x64ParameterValuesWrite[0], basicBlock->procedure, liveValues);
			++paramIdx;
		}

		for (int i = 0; i < procTypeInfo.parameters.size; ++i, ++paramIdx)
		{
			s64 paramTypeIdx = procTypeInfo.parameters[i].typeTableIdx;
			bool isXMM = context->typeTable[paramTypeIdx].typeCategory == TYPECATEGORY_FLOATING;
			if (!isXMM || paramIdx >= 4)
				AddValue(context, x64ParameterValuesWrite[paramIdx], basicBlock->procedure, liveValues);
			else switch (paramIdx)
			{
				case 0: AddValue(context, XMM0.valueIdx, basicBlock->procedure, liveValues); break;
				case 1: AddValue(context, XMM1.valueIdx, basicBlock->procedure, liveValues); break;
				case 2: AddValue(context, XMM2.valueIdx, basicBlock->procedure, liveValues); break;
				case 3: AddValue(context, XMM3.valueIdx, basicBlock->procedure, liveValues); break;
				default: ASSERT(false);
			}
		}

		// Add varargs array
		if (procTypeInfo.isVarargs)
			AddValue(context, x64ParameterValuesWrite[paramIdx], basicBlock->procedure, liveValues);
	} break;
	case X64_CALL_Indirect:
	{
		ArrayInit(&inst->liveValues, liveValues->size, FrameAlloc);
		inst->liveValues.size = liveValues->size;
		for (int i = 0; i < liveValues->size; ++i)
			inst->liveValues[i] = (*liveValues)[i];

		AddValue(context, inst->valueIdx, basicBlock->procedure, liveValues);

		s64 procTypeIdx = context->values[inst->valueIdx].typeTableIdx;
		ASSERT(context->typeTable[procTypeIdx].typeCategory == TYPECATEGORY_PROCEDURE);
		TypeInfoProcedure procTypeInfo = context->typeTable[procTypeIdx].procedureInfo;

		s64 returnTypeIdx = procTypeInfo.returnTypeTableIdx;

		int paramIdx = 0;
		// Take into account return value pointer in RCX
		if (returnTypeIdx > 0 && IRShouldPassByCopy(context, returnTypeIdx))
		{
			AddValue(context, x64ParameterValuesWrite[0], basicBlock->procedure, liveValues);
			++paramIdx;
		}

		for (int i = 0; i < procTypeInfo.parameters.size; ++i, ++paramIdx)
		{
			bool isXMM = context->typeTable[procTypeInfo.parameters[i].typeTableIdx].typeCategory ==
				TYPECATEGORY_FLOATING;
			if (!isXMM || paramIdx >= 4)
				AddValue(context, x64ParameterValuesWrite[paramIdx], basicBlock->procedure, liveValues);
			else switch (paramIdx)
			{
				case 0: AddValue(context, XMM0.valueIdx, basicBlock->procedure, liveValues); break;
				case 1: AddValue(context, XMM1.valueIdx, basicBlock->procedure, liveValues); break;
				case 2: AddValue(context, XMM2.valueIdx, basicBlock->procedure, liveValues); break;
				case 3: AddValue(context, XMM3.valueIdx, basicBlock->procedure, liveValues); break;
				default: ASSERT(false);
			}
		}

		if (procTypeInfo.isVarargs)
			AddValue(context, x64ParameterValuesWrite[paramIdx], basicBlock->procedure, liveValues);
	} break;
	case X64_PUSH:
	case X64_POP:
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
	case X64_JG:
	case X64_JL:
	case X64_JGE:
	case X64_JLE:
	case X64_LEAVE:
	case X64_RET:
	case X64_SETG:
	case X64_SETL:
	case X64_SETGE:
	case X64_SETLE:
	case X64_SETE:
	case X64_SETNE:
	case X64_Label:
	case X64_Comment:
	case X64_Push_Scope:
	case X64_Pop_Scope:
	{
	} break;
	case X64_Push_Value:
	{
		context->values[inst->valueIdx].flags |= VALUEFLAGS_HAS_PUSH_INSTRUCTION;
	} break;
	case X64_Patch:
	{
		DoLivenessAnalisisOnInstruction(context, basicBlock, inst->patch2, liveValues);
		DoLivenessAnalisisOnInstruction(context, basicBlock, inst->patch1, liveValues);
	} break;
	case X64_Patch_Many:
	{
		for (int i = (int)inst->patchInstructions.size - 1; i >= 0 ; --i)
			DoLivenessAnalisisOnInstruction(context, basicBlock, &inst->patchInstructions[i], liveValues);
	} break;
	default:
	{
		ASSERT(!"Unknown x64 instruction while register allocating.");
	}
	}

	// Add edges to graph
	for (int i = 0; i < liveValues->size; ++i)
	{
		u32 valueIdx = (*liveValues)[i];

		InterferenceGraphNode *node = nullptr;
		for (int nodeIdx = 0; nodeIdx < context->beInterferenceGraph.size; ++nodeIdx)
		{
			InterferenceGraphNode *currentNode = &context->beInterferenceGraph[nodeIdx];
			if (currentNode->valueIdx == valueIdx)
			{
				node = currentNode;
				goto nodeFound;
			}
		}
		node = DynamicArrayAdd(&context->beInterferenceGraph);
		*node = {};
		node->valueIdx = valueIdx;
		DynamicArrayInit(&node->edges, 8);
nodeFound:
		ASSERT(node);
		Value value = context->values[valueIdx];
		TypeInfo typeInfo = context->typeTable[value.typeTableIdx];
		bool isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;
		for (int j = 0; j < liveValues->size; ++j)
		{
			if (i == j) continue;
			u32 edgeValueIdx = (*liveValues)[j];
			Value edgeValue = context->values[edgeValueIdx];
			TypeInfo edgeTypeInfo = context->typeTable[edgeValue.typeTableIdx];
			bool edgeIsXMM = edgeTypeInfo.size > 8 ||
				edgeTypeInfo.typeCategory == TYPECATEGORY_FLOATING;
			// Add only other values that compete for the same pool of registers.
			// Floating point values use a different set of registers (xmmX).
			if (isXMM == edgeIsXMM)
				DynamicArrayAddUnique(&node->edges, edgeValueIdx);
		}

		// No live values that cross a procedure call can be stored in RAX.
		if (inst->type == X64_CALL || inst->type == X64_CALL_Indirect)
			DynamicArrayAddUnique(&node->edges, RAX.valueIdx);
	}
}

void DoLivenessAnalisis(Context *context, BasicBlock *basicBlock,
		DynamicArray<u32, FrameAlloc, FrameRealloc> *liveValues)
{
	if (context->config.logAllocationInfo)
		Print("Doing liveness analisis on block %S %d-%d\n", basicBlock->procedure->name,
				basicBlock->beginIdx, basicBlock->endIdx);

	for (int i = 0; i < basicBlock->liveValuesAtOutput.size; ++i)
	{
		DynamicArrayAddUnique(liveValues, basicBlock->liveValuesAtOutput[i]);
	}
	for (int i = 0; i < liveValues->size; ++i)
	{
		DynamicArrayAddUnique(&basicBlock->liveValuesAtOutput, (*liveValues)[i]);
	}

	if (basicBlock->procedure->returnValueIdx != U32_MAX)
		AddValue(context, basicBlock->procedure->returnValueIdx, basicBlock->procedure,
				liveValues);
	for (s64 instructionIdx = basicBlock->endIdx; instructionIdx >= basicBlock->beginIdx;
			--instructionIdx)
	{
		X64Instruction *inst = &basicBlock->procedure->instructions[instructionIdx];
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
		DynamicArray<u32, FrameAlloc, FrameRealloc> liveValuesCopy;
		DynamicArrayInit(&liveValuesCopy, liveValues->capacity);
		DynamicArrayCopy(&liveValuesCopy, liveValues);

		DoLivenessAnalisis(context, inputBlock, &liveValuesCopy);
	}
}

void GenerateBasicBlocks(Context *context, Array<X64Procedure> x64Procedures)
{
	for (int procedureIdx = 1; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64Procedure *proc = &x64Procedures[procedureIdx];

		if (context->config.logAllocationInfo)
		{
			Print("GENERATING BASIC BLOCKS FOR %S\n", proc->name);
		}

		BasicBlock *currentBasicBlock = PushBasicBlock(nullptr, &context->beBasicBlocks);
		currentBasicBlock->procedure = proc;

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			X64Instruction inst = proc->instructions[instructionIdx];

			if (context->config.logAllocationInfo)
				Print("\t%S\n", X64InstructionToStr(context, inst));

			switch (inst.type)
			{
			case X64_Label:
			{
				if (context->config.logAllocationInfo)
					Print("- Split\n");

				currentBasicBlock->endIdx = instructionIdx - 1;
				BasicBlock *previousBlock = currentBasicBlock;
				currentBasicBlock = PushBasicBlock(currentBasicBlock, &context->beBasicBlocks);
				*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
				*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
			} break;
			case X64_JE:
			case X64_JNE:
			case X64_JG:
			case X64_JL:
			case X64_JGE:
			case X64_JLE:
			case X64_JMP:
			{
				if (context->config.logAllocationInfo)
					Print("- Split\n");

				currentBasicBlock->endIdx = instructionIdx;
				BasicBlock *previousBlock = currentBasicBlock;
				currentBasicBlock = PushBasicBlock(currentBasicBlock, &context->beBasicBlocks);
				*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
				*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
			} break;
			case X64_Patch:
			case X64_Patch_Many:
				ASSERT(!"Patches not supported here!");
			}
		}

		currentBasicBlock->endIdx = instructionCount - 1;
		*DynamicArrayAdd(&context->beLeafBasicBlocks) = currentBasicBlock;

		if (context->config.logAllocationInfo)
			Print("- End\n\n");
	}

	const u64 basicBlockCount = BucketArrayCount(&context->beBasicBlocks);
	for (int i = 0; i < basicBlockCount; ++i)
	{
		BasicBlock *jumpBlock = &context->beBasicBlocks[i];

		IRLabel *label = nullptr;
		X64Instruction endInstruction = jumpBlock->procedure->instructions[jumpBlock->endIdx];
		if (endInstruction.type == X64_JMP ||
			endInstruction.type == X64_JE ||
			endInstruction.type == X64_JNE ||
			endInstruction.type == X64_JG ||
			endInstruction.type == X64_JL ||
			endInstruction.type == X64_JGE ||
			endInstruction.type == X64_JLE)
			label = endInstruction.label;
		else
			continue;

		for (int j = 0; j < basicBlockCount; ++j)
		{
			BasicBlock *labelBlock = &context->beBasicBlocks[j];
			X64Instruction beginInstruction =
				labelBlock->procedure->instructions[labelBlock->beginIdx];

			if (beginInstruction.type == X64_Label &&
					beginInstruction.label == label)
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

void ResolveStackOffsets(Context *context, Array<X64Procedure> x64Procedures)
{
	DynamicArray<s64, FrameAlloc, FrameRealloc> stack;
	DynamicArrayInit(&stack, 16);

	for (int procedureIdx = 1; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64Procedure *proc = &x64Procedures[procedureIdx];
		s64 stackCursor = 0;

		// @Incomplete: implement calling conventions other than MS ABI
		s64 allocParameters = proc->allocatedParameterCount;
		if (allocParameters < 4) allocParameters = 4;
		else if (allocParameters & 1) ++allocParameters;
		stackCursor += allocParameters * 8;
		if (stackCursor & 15)
			stackCursor = (stackCursor + 16) & (~15);

		// Allocate spilled values
		for (int spillIdx = 0; spillIdx < proc->spilledValues.size; ++spillIdx)
		{
			Value *value = &context->values[proc->spilledValues[spillIdx]];
			ASSERT(!(value->flags & VALUEFLAGS_IS_ALLOCATED));

			// If the value has properly scoped allocation don't dumbly spill into stack.
			if (value->flags & VALUEFLAGS_HAS_PUSH_INSTRUCTION)
				continue;

			u64 size = context->typeTable[value->typeTableIdx].size;
			int alignment = size > 8 ? 8 : NextPowerOf2((int)size);
			if (stackCursor & (alignment - 1))
				stackCursor = (stackCursor + alignment) & ~(alignment - 1);
			ASSERT(stackCursor < S32_MAX);
			value->stackOffset = (s32)stackCursor;
			value->flags |= VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY;
			stackCursor += size;
		}

		proc->stackSize = stackCursor;

		X64InstructionStream stream = X64InstructionStreamBegin(proc);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			switch (inst->type)
			{
			case X64_Push_Value:
			{
				Value *value = &context->values[inst->valueIdx];
				ASSERT(value->flags & VALUEFLAGS_HAS_PUSH_INSTRUCTION);
				if (value->flags & VALUEFLAGS_IS_ALLOCATED)
				{
					ASSERT(!(value->flags & VALUEFLAGS_IS_MEMORY));
					goto next;
				}
				// We don't allocate static values, the assembler/linker does.
				ASSERT(!(value->flags & VALUEFLAGS_ON_STATIC_STORAGE));
				ASSERT(!(value->flags & VALUEFLAGS_IS_EXTERNAL));

				u64 size = context->typeTable[value->typeTableIdx].size;
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
				if (stackCursor > (s64)proc->stackSize)
					proc->stackSize = stackCursor;
				stackCursor = stack[--stack.size];
			} break;
			}
next:
			inst = X64InstructionStreamAdvance(&stream);
		}
		if (stackCursor > (s64)proc->stackSize)
			proc->stackSize = stackCursor;

		// Align stack to 16 bytes.
		if (proc->stackSize & 15)
			proc->stackSize = (proc->stackSize & ~15) + 16;
	}
}

inline u64 BitIfRegister(Context *context, IRValue irValue)
{
	if (irValue.valueType == IRVALUETYPE_VALUE || irValue.valueType == IRVALUETYPE_MEMORY)
	{
		Value value = context->values[irValue.valueIdx];
		if (value.flags & VALUEFLAGS_IS_USED && VALUEFLAGS_IS_ALLOCATED &&
				!(value.flags & VALUEFLAGS_IS_MEMORY))
		{
			ASSERT(value.allocatedRegister < 64);
			return 1ll << value.allocatedRegister;
		}
	}
	return 0;
}

inline u64 RegisterSavingInstruction(Context *context, X64Instruction *inst, u64 usedRegisters)
{
	switch(inst->type)
	{
	// two operands
	case X64_MOV:
	case X64_MOVZX:
	case X64_MOVSX:
	case X64_MOVSXD:
	case X64_MOVSS:
	case X64_MOVSD:
	case X64_LEA:
	case X64_CVTSI2SS:
	case X64_CVTSI2SD:
	case X64_CVTTSS2SI:
	case X64_CVTTSD2SI:
	case X64_CVTSS2SD:
	case X64_ADD:
	case X64_SUB:
	case X64_IMUL:
	case X64_IDIV:
	case X64_SAR:
	case X64_SAL:
	case X64_AND:
	case X64_OR:
	case X64_XOR:
	case X64_ADDSS:
	case X64_ADDSD:
	case X64_SUBSS:
	case X64_SUBSD:
	case X64_MULSS:
	case X64_MULSD:
	case X64_DIVSS:
	case X64_DIVSD:
	case X64_XORPS:
	case X64_XORPD:
	case X64_SQRTSS:
	case X64_SQRTSD:
	case X64_CMP:
	case X64_COMISS:
	case X64_COMISD:
	case X64_MOVUPS:
	case X64_MOVAPS:
	{
		usedRegisters |= BitIfRegister(context, inst->dst);
		usedRegisters |= BitIfRegister(context, inst->src);
	} break;
	// one operand
	case X64_NOT:
	case X64_NEG:
	{
		usedRegisters |= BitIfRegister(context, inst->dst);
	} break;
	case X64_CALL:
	case X64_CALL_Indirect:
	{
		// Callee save registers
		u64 liveRegisterBits = 0;
		for (int i = 0; i < inst->liveValues.size; ++i)
		{
			Value v = context->values[inst->liveValues[i]];
			if ((v.flags & (VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED | VALUEFLAGS_IS_MEMORY)) ==
					(VALUEFLAGS_IS_USED | VALUEFLAGS_IS_ALLOCATED))
				liveRegisterBits |= (1ll << v.allocatedRegister);
		}

		u64 usedCalleeSaveRegisters = calleeSaveRegisters & liveRegisterBits;
		s64 calleeSaveRegCount = CountOnes(usedCalleeSaveRegisters);
		X64Instruction patchInst = { X64_Patch_Many };
		ArrayInit(&patchInst.patchInstructions, 1 + 3 * calleeSaveRegCount, FrameAlloc);
		patchInst.patchInstructions.size = 1 + 3 * calleeSaveRegCount;
		int count = 0;
		for (int i = 0; i < 64; ++i)
		{
			if (usedCalleeSaveRegisters & ((u64)1 << i))
			{
				u32 newValueIdx = NewValue(context, "_save_reg"_s, TYPETABLEIDX_S64,
						VALUEFLAGS_IS_USED | VALUEFLAGS_FORCE_MEMORY |
						VALUEFLAGS_HAS_PUSH_INSTRUCTION);

				IRValue reg = x64Registers[i];

				X64InstructionType movType = i >= XMM0_idx ? X64_MOVSD : X64_MOV;

				X64Instruction pushInst = { X64_Push_Value };
				pushInst.valueIdx = newValueIdx;
				X64Instruction saveInst = { movType, IRValueMemory(newValueIdx, 0, TYPETABLEIDX_S64), reg };
				X64Instruction restoreInst = { movType, reg, IRValueMemory(newValueIdx, 0, TYPETABLEIDX_S64) };

				patchInst.patchInstructions[count * 2] = pushInst;
				patchInst.patchInstructions[count * 2 + 1] = saveInst;
				patchInst.patchInstructions[calleeSaveRegCount * 2 + 1 + count] = restoreInst;
				++count;
			}
		}

		patchInst.patchInstructions[calleeSaveRegCount * 2] = *inst;
		*inst = patchInst;
	} break;
	}
	return usedRegisters;
}

void X64AllocateRegisters(Context *context, Array<X64Procedure> x64Procedures)
{
	BucketArrayInit(&context->beBasicBlocks);
	DynamicArrayInit(&context->beLeafBasicBlocks, 128);
	DynamicArrayInit(&context->beInterferenceGraph, 128);

	GenerateBasicBlocks(context, x64Procedures);

	int availableRegisters = sizeof(x64ScratchRegisters) / sizeof(x64ScratchRegisters[0]);
	int availableRegistersFP = 8;

	// Do liveness analisis, starting from all leaf blocks
	for (int leafIdx = 0; leafIdx < context->beLeafBasicBlocks.size; ++leafIdx)
	{
		BasicBlock *currentLeafBlock = context->beLeafBasicBlocks[leafIdx];

		String procName = currentLeafBlock->procedure->name;

		context->beInterferenceGraph.size = 0;

		// @Todo: iterative instead of recursive?
		DynamicArray<u32, FrameAlloc, FrameRealloc> liveValues;
		DynamicArrayInit(&liveValues, 32);
		DoLivenessAnalisis(context, currentLeafBlock, &liveValues);

		if (context->config.logAllocationInfo)
		{
			for (int nodeIdx = 0; nodeIdx < context->beInterferenceGraph.size; ++nodeIdx)
			{
				InterferenceGraphNode *currentNode = &context->beInterferenceGraph[nodeIdx];
				Print("Value %S coexists with: ", X64IRValueToStr(context, IRValueValue(context, currentNode->valueIdx)));
				for (int i = 0; i < currentNode->edges.size; ++i)
					Print("%S, ", X64IRValueToStr(context, IRValueValue(context, currentNode->edges[i])));
				Print("\n");
			}
		}

		Array<InterferenceGraphNode *> nodeStack;
		ArrayInit(&nodeStack, context->beInterferenceGraph.size, malloc);

		// Allocate values to registers when possible
		while (nodeStack.size < context->beInterferenceGraph.size)
		{
			InterferenceGraphNode *nodeToRemove = nullptr;
			int nodeToRemoveIdx = -1;
			for (int nodeIdx = 0; nodeIdx < context->beInterferenceGraph.size; ++nodeIdx)
			{
				InterferenceGraphNode *currentNode = &context->beInterferenceGraph[nodeIdx];
				if (currentNode->removed)
					continue;
				if (currentNode->edges.size < availableRegisters)
				{
					nodeToRemove = currentNode;
					nodeToRemoveIdx = nodeIdx;
					break;
				}
			}

			// Fallback node might be flagged as no-spill. When adding back nodes, we might get
			// lucky and not spill it.
			InterferenceGraphNode *fallbackNode = nullptr;
			if (!nodeToRemove)
			{
				// Choose a register to spill onto the stack.
				// This heuristic is very arbitrary and sub-optimal.
				s64 mostEdges = -1;
				for (int nodeIdx = 0; nodeIdx < context->beInterferenceGraph.size; ++nodeIdx)
				{
					InterferenceGraphNode *currentNode = &context->beInterferenceGraph[nodeIdx];

					if (currentNode->removed)
						continue;
					fallbackNode = currentNode;

					Value v = context->values[currentNode->valueIdx];
					if (v.flags & VALUEFLAGS_FORCE_REGISTER)
						continue;

					if ((s64)currentNode->edges.size > mostEdges)
					{
						nodeToRemove = currentNode;
						nodeToRemoveIdx = nodeIdx;
						mostEdges = currentNode->edges.size;
					}
					else if ((s64)currentNode->edges.size == mostEdges)
					{
						if (currentNode->valueIdx < nodeToRemove->valueIdx)
						{
							nodeToRemove = currentNode;
							nodeToRemoveIdx = nodeIdx;
						}
					}
				}
			}

			if (!nodeToRemove) nodeToRemove = fallbackNode;
			ASSERT(nodeToRemove);

			for (int nodeIdx = 0; nodeIdx < context->beInterferenceGraph.size; ++nodeIdx)
			{
				InterferenceGraphNode *currentNode = &context->beInterferenceGraph[nodeIdx];
				if (currentNode->removed)
					continue;
				for (int i = 0; i < currentNode->edges.size; ++i)
				{
					Value v = context->values[nodeToRemove->valueIdx];
					if (!(v.flags & VALUEFLAGS_IS_ALLOCATED) &&
						currentNode->edges[i] == nodeToRemove->valueIdx)
					{
						// Remove from node edges
						currentNode->edges[i] = currentNode->edges[--currentNode->edges.size];
					}
				}
			}
			nodeToRemove->removed = true;
			*ArrayAdd(&nodeStack) = nodeToRemove;
		}

		for (int nodeIdx = (int)nodeStack.size - 1; nodeIdx >= 0; --nodeIdx)
		{
			InterferenceGraphNode *currentNode = nodeStack[nodeIdx];
			Value *v = &context->values[currentNode->valueIdx];

			if (v->flags & VALUEFLAGS_IS_ALLOCATED)
				continue;

			TypeInfo typeInfo = context->typeTable[v->typeTableIdx];
			bool isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;

			int max = isXMM ? availableRegistersFP : availableRegisters;
			for (int candidateIdx = 0; candidateIdx < max; ++candidateIdx)
			{
				s32 candidate = isXMM ? XMM0_idx + candidateIdx : x64ScratchRegisters[candidateIdx];
				for (int edgeIdx = 0; edgeIdx < currentNode->edges.size; ++edgeIdx)
				{
					u32 edgeValueIdx = currentNode->edges[edgeIdx];
					Value edgeValue = context->values[edgeValueIdx];
					if (!(edgeValue.flags & VALUEFLAGS_IS_ALLOCATED) ||
						  edgeValue.flags & VALUEFLAGS_IS_MEMORY)
						continue;
					if (edgeValue.allocatedRegister == candidate)
						goto skipCandidate;
				}
				v->allocatedRegister = candidate;

				// We don't allocate static values, the assembler/linker does.
				ASSERT(!(v->flags & VALUEFLAGS_ON_STATIC_STORAGE));
				ASSERT(!(v->flags & VALUEFLAGS_IS_EXTERNAL));

				v->flags &= ~VALUEFLAGS_IS_MEMORY;
				v->flags |= VALUEFLAGS_IS_ALLOCATED;
				break;
	skipCandidate:;
			}
			if (!(v->flags & VALUEFLAGS_IS_ALLOCATED))
			{
				if (v->flags & VALUEFLAGS_FORCE_REGISTER)
					continue;

				// Spill!
				*DynamicArrayAdd(&currentLeafBlock->procedure->spilledValues) =
					currentNode->valueIdx;
			}
		}
	}

	// Do register saving
	for (int procedureIdx = 1; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64Procedure *proc = &x64Procedures[procedureIdx];

		u64 usedRegisters = 0;
		X64InstructionStream stream = X64InstructionStreamBegin(proc);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst)
		{
			usedRegisters = RegisterSavingInstruction(context, inst, usedRegisters);
			inst = X64InstructionStreamAdvance(&stream);
		}

		// Caller save registers
		u64 usedCallerSaveRegisters = callerSaveRegisters & usedRegisters;
		s64 callerSaveRegCount = CountOnes(usedCallerSaveRegisters);
		X64Instruction patchTop =    { X64_Patch_Many };
		X64Instruction patchBottom = { X64_Patch_Many };
		ArrayInit(&patchTop.patchInstructions, 1 + callerSaveRegCount, malloc);
		ArrayInit(&patchBottom.patchInstructions, 1 + callerSaveRegCount, malloc);

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		*ArrayAdd(&patchBottom.patchInstructions) = proc->instructions[instructionCount - 1];

		for (int i = 0; i < 64; ++i)
		{
			if (usedCallerSaveRegisters & ((u64)1 << i))
			{
				u32 newValueIdx = NewValue(context, "_save_reg"_s, TYPETABLEIDX_S64,
						VALUEFLAGS_IS_USED | VALUEFLAGS_FORCE_MEMORY);
				*DynamicArrayAdd(&proc->spilledValues) = newValueIdx;

				IRValue reg = x64Registers[i];

				X64InstructionType movType = i >= XMM0_idx ? X64_MOVSD : X64_MOV;

				X64Instruction *saveInst = ArrayAdd(&patchTop.patchInstructions);
				*saveInst = { movType, IRValueMemory(newValueIdx, 0, TYPETABLEIDX_S64),
					reg };

				X64Instruction *restoreInst = ArrayAdd(&patchBottom.patchInstructions);
				*restoreInst = { movType, reg, IRValueMemory(newValueIdx, 0, TYPETABLEIDX_S64) };
			}
		}

		*ArrayAdd(&patchTop.patchInstructions) = proc->instructions[0];
		proc->instructions[0] = patchTop;
		proc->instructions[instructionCount - 1] = patchBottom;
	}

	ResolveStackOffsets(context, x64Procedures);
}
