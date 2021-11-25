const u64 calleeSaveRegisters = 0b111111001;
const u64 callerSaveRegisters = 0b111111111 & ~calleeSaveRegisters;

struct BasicBlock
{
	X64Procedure *procedure;
	s64 beginIdx;
	s64 endIdx;
	bool livenessAnalizedOnce;
	DynamicArray<BasicBlock *, FrameAlloc, FrameRealloc> inputs;
	DynamicArray<BasicBlock *, FrameAlloc, FrameRealloc> outputs;

	// @Todo: bitmaps
	DynamicArray<s64, FrameAlloc, FrameRealloc> liveRegistersAtInput;
	DynamicArray<s64, FrameAlloc, FrameRealloc> liveRegistersAtOutput;
};

struct InterferenceGraphNode
{
	s64 registerIdx;
	DynamicArray<s64, FrameAlloc, FrameRealloc> edges; // @Improve: eugh
	bool removed;
};

void X64Patch(Context *context, X64Instruction *original, X64Instruction newInst)
{
	X64Instruction *patch1 = BucketArrayAdd(&context->patchedInstructions);
	*patch1 = newInst;
	X64Instruction *patch2 = BucketArrayAdd(&context->patchedInstructions);
	*patch2 = *original;

	X64Instruction patchInst = { X64_Patch };
	patchInst.patch1 = patch1;
	patchInst.patch2 = patch2;
	*original = patchInst;
}

BasicBlock *PushBasicBlock(BasicBlock *currentBasicBlock,
		BucketArray<BasicBlock, 512, malloc, realloc> *basicBlocks)
{
	// @Fix: why doesn't this work?
#if 0
	if (currentBasicBlock && currentBasicBlock->endIdx <= currentBasicBlock->beginIdx)
	{
		// If current block is empty don't create a new one
		return currentBasicBlock;
	}
#endif

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
	DynamicArrayInit(&result->liveRegistersAtInput,  8);
	DynamicArrayInit(&result->liveRegistersAtOutput, 8);

	return result;
}

inline bool AddIfRegister(BasicBlock *basicBlock, IRValue value,
		DynamicArray<s64, FrameAlloc, FrameRealloc> *array)
{
	if (value.valueType != IRVALUETYPE_REGISTER && value.valueType != IRVALUETYPE_MEMORY_REGISTER)
		return false;

	if (value.registerIdx >= IRSPECIALREGISTER_BEGIN)
		return false;

	for (int i = 0; i < array->size; ++i)
	{
		if ((*array)[i] == value.registerIdx)
			return false; // No duplicates
	}
	*DynamicArrayAdd(array) = value.registerIdx;
	return true;
}

inline void RemoveIfRegister(BasicBlock *basicBlock, IRValue value,
		DynamicArray<s64, FrameAlloc, FrameRealloc> *array)
{
	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		for (int i = 0; i < array->size; ++i)
		{
			if ((*array)[i] == value.registerIdx)
				(*array)[i] = (*array)[--array->size];
		}
	}
	else if (value.valueType == IRVALUETYPE_MEMORY_REGISTER)
	{
		// The register is actually _used_ here, and not written to. Add instead.
		IRValue reg = IRValueRegister(value.memory.baseRegister, value.typeTableIdx);
		AddIfRegister(basicBlock, reg, array);
	}
}

inline void ReplaceIfRegister(IRValue *value, Array<s64> registerMap)
{
	if (value->valueType == IRVALUETYPE_REGISTER || value->valueType == IRVALUETYPE_MEMORY_REGISTER)
	{
		if (value->registerIdx >= 0 && value->registerIdx < (s64)registerMap.size)
			value->registerIdx = registerMap[value->registerIdx];
	}
}

void DoLivenessAnalisisOnInstruction(Context *context, BasicBlock *basicBlock, X64Instruction *inst,
		DynamicArray<s64, FrameAlloc, FrameRealloc> *liveRegisters)
{
#if DEBUG_OPTIMIZER
	if (inst->type != X64_Patch && inst->type != X64_Patch_Many)
	{
		Print("\t");
		s64 s = X64PrintInstruction(context, *inst);
		if (s < 40)
		{
			char buffer[40];
			memset(buffer, ' ', sizeof(buffer));
			buffer[39] = 0;
			Print("%s", buffer + s);
		}
		for (int i = 0; i < liveRegisters->size; ++i)
			Print("%S, ", X64RegisterToStr((*liveRegisters)[i], 8, false));
		Print("\n");
	}
#endif

	switch (inst->type)
	{
	// dst write, src read
	case X64_MOV:
	case X64_MOVZX:
	case X64_MOVSX:
	case X64_MOVSXD:
	case X64_MOVSS:
	case X64_MOVSD:
	case X64_LEA:
	case X64_CVTSI2SS:
	case X64_CVTSI2SD:
	case X64_CVTSS2SI:
	case X64_CVTSD2SI:
	case X64_CVTSS2SD:
	case X64_CVTSD2SS:
	{
		RemoveIfRegister(basicBlock, inst->dst, liveRegisters);
		AddIfRegister   (basicBlock, inst->src, liveRegisters);
	} break;
	// dst read/write, src read
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
	{
		AddIfRegister(basicBlock, inst->dst, liveRegisters);
		AddIfRegister(basicBlock, inst->src, liveRegisters);
	} break;
	// dst read, src read
	case X64_CMP:
	case X64_COMISS:
	case X64_COMISD:
	{
		AddIfRegister(basicBlock, inst->dst, liveRegisters);
		AddIfRegister(basicBlock, inst->src, liveRegisters);
	} break;
	// dst read/write
	case X64_NOT:
	case X64_NEG:
	{
		AddIfRegister(basicBlock, inst->dst, liveRegisters);
	} break;
	// nothing
	case X64_CQO:
	case X64_PUSH:
	case X64_POP:
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
	case X64_CALL:
	case X64_LEAVE:
	case X64_RET:
	case X64_SETG:
	case X64_SETL:
	case X64_SETGE:
	case X64_SETLE:
	case X64_SETE:
	case X64_Label:
	{
	} break;
	case X64_Patch:
	{
		DoLivenessAnalisisOnInstruction(context, basicBlock, inst->patch1, liveRegisters);
		DoLivenessAnalisisOnInstruction(context, basicBlock, inst->patch2, liveRegisters);
	} break;
	case X64_Patch_Many:
	{
		for (int i = 0; i < inst->patchInstructions.size; ++i)
			DoLivenessAnalisisOnInstruction(context, basicBlock, &inst->patchInstructions[i], liveRegisters);
	} break;
	default:
	{
		ASSERT(!"Unknown x64 instruction while register allocating.");
	}
	}

	// Add edges to graph
	for (int i = 0; i < liveRegisters->size; ++i)
	{
		s64 reg = (*liveRegisters)[i];

		if (reg < 0 || reg >= RAX_idx) continue;

		InterferenceGraphNode *node = nullptr;
		for (int nodeIdx = 0; nodeIdx < context->interferenceGraph.size; ++nodeIdx)
		{
			InterferenceGraphNode *currentNode = &context->interferenceGraph[nodeIdx];
			if (currentNode->registerIdx == reg)
			{
				node = currentNode;
				goto nodeFound;
			}
		}
		node = DynamicArrayAdd(&context->interferenceGraph);
		*node = {};
		node->registerIdx = reg;
		DynamicArrayInit(&node->edges, 8);
nodeFound:
		ASSERT(node);
		for (int j = 0; j < liveRegisters->size; ++j)
		{
			if (i == j) continue;
			s64 edge = (*liveRegisters)[j];

			if (edge < 0 || edge >= RAX_idx) continue;

			for (int k = 0; k < node->edges.size; ++k)
			{
				if (node->edges[k] == edge)
					goto skipEdge;
			}
			*DynamicArrayAdd(&node->edges) = edge;
skipEdge:;
		}
	}
}

void DoLivenessAnalisis(Context *context, BasicBlock *basicBlock,
		DynamicArray<s64, FrameAlloc, FrameRealloc> *liveRegisters)
{
#if DEBUG_OPTIMIZER
	Print("Doing liveness analisis on block %S %d-%d\n", basicBlock->procedure->name,
			basicBlock->beginIdx, basicBlock->endIdx);
#endif

	for (int i = 0; i < basicBlock->liveRegistersAtOutput.size; ++i)
	{
		AddIfRegister(basicBlock, IRValueRegister(basicBlock->liveRegistersAtOutput[i]),
				liveRegisters);
	}
	for (int i = 0; i < liveRegisters->size; ++i)
	{
		AddIfRegister(basicBlock, IRValueRegister((*liveRegisters)[i]),
				&basicBlock->liveRegistersAtOutput);
	}

	for (s64 instructionIdx = basicBlock->endIdx; instructionIdx >= basicBlock->beginIdx;
			--instructionIdx)
	{
		X64Instruction *inst = &basicBlock->procedure->instructions[instructionIdx];
		DoLivenessAnalisisOnInstruction(context, basicBlock, inst, liveRegisters);
	}

	bool somethingChanged = false;
	for (int i = 0; i < liveRegisters->size; ++i)
	{
		if (AddIfRegister(basicBlock, IRValueRegister((*liveRegisters)[i]),
				&basicBlock->liveRegistersAtInput))
			somethingChanged = true;
	}
	if (!somethingChanged && basicBlock->livenessAnalizedOnce)
		return;

	basicBlock->livenessAnalizedOnce = true;

	for (int i = 0; i < basicBlock->inputs.size; ++i)
	{
		BasicBlock *inputBlock = basicBlock->inputs[i];
		// Copy live registers array
		DynamicArray<s64, FrameAlloc, FrameRealloc> liveRegistersCopy;
		DynamicArrayInit(&liveRegistersCopy, liveRegisters->capacity);
		DynamicArrayCopy(&liveRegistersCopy, liveRegisters);

		DoLivenessAnalisis(context, inputBlock, &liveRegistersCopy);
	}
}

void GenerateBasicBlocks(Context *context, Array<X64Procedure> x64Procedures)
{
	for (int procedureIdx = 0; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64Procedure *proc = &x64Procedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (context->procedures[procedureIdx].isExternal)
			continue;

		BasicBlock *currentBasicBlock = PushBasicBlock(nullptr, &context->basicBlocks);
		currentBasicBlock->procedure = proc;

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			X64Instruction inst = proc->instructions[instructionIdx];
			switch (inst.type)
			{
			case X64_Label:
			{
				currentBasicBlock->endIdx = instructionIdx - 1;
				BasicBlock *previousBlock = currentBasicBlock;
				currentBasicBlock = PushBasicBlock(currentBasicBlock, &context->basicBlocks);

				*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
				*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
			} break;
			case X64_JE:
			case X64_JNE:
			case X64_JMP:
			{
				currentBasicBlock->endIdx = instructionIdx;
				BasicBlock *previousBlock = currentBasicBlock;
				currentBasicBlock = PushBasicBlock(currentBasicBlock, &context->basicBlocks);

				*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
				*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
			} break;
			case X64_RET:
			{
				*DynamicArrayAdd(&context->leafBasicBlocks) = currentBasicBlock;
			} break;
			}
		}
	}

	const u64 basicBlockCount = BucketArrayCount(&context->basicBlocks);
	for (int i = 0; i < basicBlockCount; ++i)
	{
		BasicBlock *jumpBlock = &context->basicBlocks[i];

		IRLabel *label = nullptr;
		X64Instruction endInstruction = jumpBlock->procedure->instructions[jumpBlock->endIdx];
		if (endInstruction.type == X64_JMP ||
			endInstruction.type == X64_JE ||
			endInstruction.type == X64_JNE)
			label = endInstruction.label;
		else
			continue;

		for (int j = 0; j < basicBlockCount; ++j)
		{
			BasicBlock *labelBlock = &context->basicBlocks[j];
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

void ReplaceRegisterForVariable(Context *context, X64Procedure *procedure,
		X64Instruction *instruction, IRValue *value, s64 registerIdx, Variable *variable)
{
	if (value->valueType == IRVALUETYPE_MEMORY_REGISTER &&
		value->memory.baseRegister == registerIdx)
	{
		s64 newVirtualRegister = procedure->virtualRegisterCount++;

		procedure->noSpillRegisters[newVirtualRegister >> 6] |= (1ll << newVirtualRegister);

		IRValue tmp = IRValueRegister(newVirtualRegister, value->typeTableIdx);

		X64Instruction newInst = { X64_MOV, tmp, IRValueMemory(variable, 0, value->typeTableIdx) };

		*value = IRValueMemory(tmp.registerIdx, value->memory.offset, value->typeTableIdx);

		X64Patch(context, instruction, newInst);
	}
	else if (value->valueType == IRVALUETYPE_REGISTER && value->registerIdx == registerIdx)
		*value = IRValueMemory(variable, 0, value->typeTableIdx);
}

void SpillRegisterIntoMemoryInstruction(Context *context, Variable *newVar, X64Instruction *inst,
		X64Procedure *procedure, s64 registerIdx)
{
	switch (inst->type)
	{
	// dst write, src read
	case X64_MOV:
	case X64_MOVZX:
	case X64_MOVSX:
	case X64_MOVSXD:
	case X64_MOVSS:
	case X64_MOVSD:
	case X64_LEA:
	case X64_CVTSI2SS:
	case X64_CVTSI2SD:
	case X64_CVTSS2SI:
	case X64_CVTSD2SI:
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
	case X64_CMP:
	case X64_COMISS:
	case X64_COMISD:
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->dst, registerIdx, newVar);
		ReplaceRegisterForVariable(context, procedure, inst, &inst->src, registerIdx, newVar);
	} break;
	// dst read/write
	case X64_NOT:
	case X64_NEG:
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->dst, registerIdx, newVar);
	} break;
	// nothing
	case X64_CQO:
	case X64_PUSH:
	case X64_POP:
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
	case X64_CALL:
	case X64_LEAVE:
	case X64_RET:
	case X64_SETG:
	case X64_SETL:
	case X64_SETGE:
	case X64_SETLE:
	case X64_SETE:
	case X64_Label:
	{
	} break;
	case X64_Patch:
	{
		SpillRegisterIntoMemoryInstruction(context, newVar, inst->patch1, procedure, registerIdx);
		SpillRegisterIntoMemoryInstruction(context, newVar, inst->patch2, procedure, registerIdx);
	} break;
	case X64_Patch_Many:
	{
		for (int i = 0; i < inst->patchInstructions.size; ++i)
			SpillRegisterIntoMemoryInstruction(context, newVar, &inst->patchInstructions[i], procedure, registerIdx);
	} break;
	default:
	{
		ASSERT(!"Unknown x64 instruction while register allocating.");
	}
	}
}

void SpillRegisterIntoMemory(Context *context, X64Procedure *procedure, s64 registerIdx)
{
	Variable *newVar = NewVariable(context, TPrintF("_spill_r%lld", registerIdx), TYPETABLEIDX_S64);
	*DynamicArrayAdd(&procedure->spillVariables) = newVar;

	u64 instructionCount = BucketArrayCount(&procedure->instructions);
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
	{
		X64Instruction *inst = &procedure->instructions[instructionIdx];
		SpillRegisterIntoMemoryInstruction(context, newVar, inst, procedure, registerIdx);
	}
}

void ResolveInstructionStackOffsets(Context *context, X64Instruction inst,
	DynamicArray<s64, FrameAlloc, FrameRealloc> *stack, s64 *stackCursor, u64 *stackSize)
{
	switch (inst.type)
	{
	case X64_Push_Variable:
	{
		Variable *var = inst.variable;
		u64 size = context->typeTable[var->typeTableIdx].size;
		//s64 alignment = size > 8 ? 8 : NextPowerOf2(size);
		s64 alignment = 8; //@Fix: hardcoded alignment because we use 8 byte operands everywhere
		if (*stackCursor & (alignment - 1))
			*stackCursor = (*stackCursor + alignment) & ~(alignment - 1);
		var->stackOffset = *stackCursor;
		var->isAllocated = true;
		*stackCursor += size;
	} break;
	case X64_Push_Scope:
	{
		*DynamicArrayAdd(stack) = *stackCursor;
	} break;
	case X64_Pop_Scope:
	{
		if (*stackCursor > (s64)*stackSize)
			*stackSize = *stackCursor;
		*stackCursor = (*stack)[--stack->size];
	} break;
	case X64_Patch:
	{
		ResolveInstructionStackOffsets(context, *inst.patch1, stack, stackCursor, stackSize);
		ResolveInstructionStackOffsets(context, *inst.patch2, stack, stackCursor, stackSize);
	} break;
	case X64_Patch_Many:
	{
		for (int i = 0; i < inst.patchInstructions.size; ++i)
			ResolveInstructionStackOffsets(context, inst.patchInstructions[i], stack, stackCursor, stackSize);
	} break;
	}
}

void ResolveStackOffsets(Context *context, Array<X64Procedure> x64Procedures)
{
	DynamicArray<s64, FrameAlloc, FrameRealloc> stack;
	DynamicArrayInit(&stack, 16);

	for (int procedureIdx = 0; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		X64Procedure *proc = &x64Procedures[procedureIdx];
		s64 stackCursor = 0;

		// @Speed: separate array of external procedures to avoid branching
		if (context->procedures[procedureIdx].isExternal)
			continue;

		// @Incomplete: implement calling conventions other than MS ABI
		s64 allocParameters = proc->allocatedParameterCount;
		if (allocParameters < 4) allocParameters = 4;
		else if (allocParameters & 1) ++allocParameters;
		stackCursor += allocParameters * 8;
		proc->stackSize += allocParameters * 8;
		if (proc->stackSize & 15)
			proc->stackSize = (proc->stackSize + 16) & (~15);

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			X64Instruction inst = proc->instructions[instructionIdx];
			ResolveInstructionStackOffsets(context, inst, &stack, &stackCursor, &proc->stackSize);
		}
	}
}

void ReplaceRegistersInInstruction(X64Instruction *inst, Array<s64> registerMap)
{
	switch (inst->type)
	{
	// dst write, src read
	case X64_MOV:
	case X64_MOVZX:
	case X64_MOVSX:
	case X64_MOVSXD:
	case X64_MOVSS:
	case X64_MOVSD:
	case X64_LEA:
	case X64_CVTSI2SS:
	case X64_CVTSI2SD:
	case X64_CVTSS2SI:
	case X64_CVTSD2SI:
	case X64_CVTSS2SD:
	case X64_CVTSD2SS:
	{
		ReplaceIfRegister(&inst->dst, registerMap);
		ReplaceIfRegister(&inst->src, registerMap);
	} break;
	// dst read/write, src read
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
	{
		ReplaceIfRegister(&inst->dst, registerMap);
		ReplaceIfRegister(&inst->src, registerMap);
	} break;
	// dst read, src read
	case X64_CMP:
	case X64_COMISS:
	case X64_COMISD:
	{
		ReplaceIfRegister(&inst->dst, registerMap);
		ReplaceIfRegister(&inst->src, registerMap);
	} break;
	// dst read/write
	case X64_NOT:
	case X64_NEG:
	{
		ReplaceIfRegister(&inst->dst, registerMap);
	} break;
	// nothing
	case X64_CQO:
	case X64_PUSH:
	case X64_POP:
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
	case X64_CALL:
	case X64_LEAVE:
	case X64_RET:
	case X64_SETG:
	case X64_SETL:
	case X64_SETGE:
	case X64_SETLE:
	case X64_SETE:
	case X64_Label:
	{
	} break;
	case X64_Patch:
	{
		ReplaceRegistersInInstruction(inst->patch1, registerMap);
		ReplaceRegistersInInstruction(inst->patch2, registerMap);
	} break;
	case X64_Patch_Many:
	{
		for (int i = 0; i < inst->patchInstructions.size; ++i)
			ReplaceRegistersInInstruction(&inst->patchInstructions[i], registerMap);
	} break;
	default:
	{
		ASSERT(!"Unknown x64 instruction while register allocating.");
	}
	}
}

inline u64 BitIfRegister(IRValue value)
{
	if (value.valueType == IRVALUETYPE_REGISTER && value.valueType == IRVALUETYPE_MEMORY_REGISTER)
	{
		if (value.registerIdx >= RAX_idx) return 0;
		ASSERT(value.registerIdx < 64);
		return 1ll << value.registerIdx;
	}
	return 0;
}

u64 RegisterSavingInstruction(Context *context, X64Instruction *inst, u64 usedRegisters)
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
	case X64_CVTSS2SI:
	case X64_CVTSD2SI:
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
	case X64_CMP:
	case X64_COMISS:
	case X64_COMISD:
	{
		usedRegisters |= BitIfRegister(inst->dst);
		usedRegisters |= BitIfRegister(inst->src);
	} break;
	// one operand
	case X64_NOT:
	case X64_NEG:
	{
		usedRegisters |= BitIfRegister(inst->dst);
	} break;
	// nothing
	case X64_CQO:
	case X64_PUSH:
	case X64_POP:
	case X64_JMP:
	case X64_JE:
	case X64_JNE:
	case X64_CALL:
	case X64_LEAVE:
	case X64_RET:
	case X64_SETG:
	case X64_SETL:
	case X64_SETGE:
	case X64_SETLE:
	case X64_SETE:
	{
	} break;
	}
	return usedRegisters;
}

void X64AllocateRegisters(Context *context, Array<X64Procedure> x64Procedures)
{
	BucketArrayInit(&context->basicBlocks);
	DynamicArrayInit(&context->leafBasicBlocks, 128);
	DynamicArrayInit(&context->interferenceGraph, 128);

	GenerateBasicBlocks(context, x64Procedures);

	// Do liveness analisis, starting from all leaf blocks
	for (int leafIdx = 0; leafIdx < context->leafBasicBlocks.size; ++leafIdx)
	{
		BasicBlock *currentLeafBlock = context->leafBasicBlocks[leafIdx];

		String procName = currentLeafBlock->procedure->name;

		// Assign real registers to virtual registers
		int availableRegisters = 8;
		while (true)
		{
			bool spilledSomething = false;
			context->interferenceGraph.size = 0;

			// @Todo: iterative instead of recursive?
			DynamicArray<s64, FrameAlloc, FrameRealloc> liveRegisters;
			DynamicArrayInit(&liveRegisters, 16);
			DoLivenessAnalisis(context, currentLeafBlock, &liveRegisters);

			Array<InterferenceGraphNode *> nodeStack;
			ArrayInit(&nodeStack, context->interferenceGraph.size, malloc);

			s64 highestVirtualRegister = -1;
			for (int nodeIdx = 0; nodeIdx < context->interferenceGraph.size; ++nodeIdx)
			{
				s64 currentReg = context->interferenceGraph[nodeIdx].registerIdx;
				ASSERT(currentReg < RAX_idx);
				if (currentReg > highestVirtualRegister)
					highestVirtualRegister = currentReg;
			}
			Array<s64> registerMap;
			ArrayInit(&registerMap, highestVirtualRegister + 1, malloc);
			registerMap.size = highestVirtualRegister + 1;
			memset(registerMap.data, 0xFF, registerMap.size * sizeof(s64));

			// Remap registers
			while (nodeStack.size < context->interferenceGraph.size)
			{
				InterferenceGraphNode *nodeToRemove = nullptr;
				int nodeToRemoveIdx = -1;
				for (int nodeIdx = 0; nodeIdx < context->interferenceGraph.size; ++nodeIdx)
				{
					InterferenceGraphNode *currentNode = &context->interferenceGraph[nodeIdx];
					if (currentNode->removed)
						continue;
					if (currentNode->edges.size < availableRegisters)
					{
						nodeToRemove = currentNode;
						nodeToRemoveIdx = nodeIdx;
						break;
					}
				}

				InterferenceGraphNode *fallbackNode = nullptr;
				if (!nodeToRemove)
				{
					// Choose a register to spill onto the stack.
					// This heuristic is very arbitrary and sub-optimal.
					s64 mostEdges = -1;
					for (int nodeIdx = 0; nodeIdx < context->interferenceGraph.size; ++nodeIdx)
					{
						InterferenceGraphNode *currentNode = &context->interferenceGraph[nodeIdx];

						if (currentNode->removed)
							continue;
						fallbackNode = currentNode;
						if (currentLeafBlock->procedure->noSpillRegisters[currentNode->registerIdx >> 6] &
								(1ll << currentNode->registerIdx))
							continue;

						if ((s64)currentNode->edges.size > mostEdges)
						{
							nodeToRemove = currentNode;
							nodeToRemoveIdx = nodeIdx;
							mostEdges = currentNode->edges.size;
						}
						else if ((s64)currentNode->edges.size == mostEdges)
						{
							if (currentNode->registerIdx < nodeToRemove->registerIdx)
							{
								nodeToRemove = currentNode;
								nodeToRemoveIdx = nodeIdx;
							}
						}
					}
				}

				if (!nodeToRemove) nodeToRemove = fallbackNode;

				for (int nodeIdx = 0; nodeIdx < context->interferenceGraph.size; ++nodeIdx)
				{
					InterferenceGraphNode *currentNode = &context->interferenceGraph[nodeIdx];
					if (currentNode->removed)
						continue;
					for (int i = 0; i < currentNode->edges.size; ++i)
					{
						if (currentNode->edges[i] == nodeToRemove->registerIdx)
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

				ASSERT(currentNode->registerIdx < RAX_idx);

				for (int candidate = 0; candidate < availableRegisters; ++candidate)
				{
					for (int edgeIdx = 0; edgeIdx < currentNode->edges.size; ++edgeIdx)
					{
						s64 edgeRegister = currentNode->edges[edgeIdx];
						if (registerMap[edgeRegister] == candidate)
							goto skipCandidate;
					}
					registerMap[currentNode->registerIdx] = candidate;
					break;
		skipCandidate:;
				}
				if (registerMap[currentNode->registerIdx] < 0)
				{
					if (currentLeafBlock->procedure->noSpillRegisters[currentNode->registerIdx >> 6] &
							(1ll << currentNode->registerIdx))
						continue;

					// Spill!
					SpillRegisterIntoMemory(context, currentLeafBlock->procedure,
							currentNode->registerIdx);
					spilledSomething = true;
				}
			}

			// Replace registers in x64 code
			X64Procedure *proc = currentLeafBlock->procedure;
			u64 instructionCount = BucketArrayCount(&proc->instructions);
			for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
			{
				X64Instruction *inst = &proc->instructions[instructionIdx];
				ReplaceRegistersInInstruction(inst, registerMap);
			}

			u64 newNoSpillRegisters[8] = {};
			for (int i = 0; i < registerMap.size; ++i)
			{
				if (proc->noSpillRegisters[i >> 6] & (1ll << i))
				{
					s64 j = registerMap[i];
					newNoSpillRegisters[j >> 6] |= (1ll << j);
				}
			}
			for (int i = (int)registerMap.size; i < proc->virtualRegisterCount; ++i)
			{
				if (proc->noSpillRegisters[i >> 6] & (1ll << i))
				{
					newNoSpillRegisters[i >> 6] |= (1ll << i);
				}
			}
			memcpy(proc->noSpillRegisters, newNoSpillRegisters, sizeof(proc->noSpillRegisters));

			if (spilledSomething)
			{
				// Clear 'liveness analized once' flag from all blocks
				u64 basicBlockCount = BucketArrayCount(&context->basicBlocks);
				for (int i = 0; i < basicBlockCount; ++i)
				{
					BasicBlock *basicBlock = &context->basicBlocks[i];
					basicBlock->livenessAnalizedOnce = false;
					basicBlock->liveRegistersAtInput.size = 0;
					basicBlock->liveRegistersAtOutput.size = 0;
				}
			}
			else
				break;
		}
	}

	u64 usedRegisters = 0;
	// Do register saving
	for (int procedureIdx = 0; procedureIdx < x64Procedures.size; ++procedureIdx)
	{
		// @Speed: separate array of external procedures to avoid branching
		if (context->procedures[procedureIdx].isExternal)
			continue;

		X64Procedure *proc = &x64Procedures[procedureIdx];

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			X64Instruction *inst = &proc->instructions[instructionIdx];
			usedRegisters = RegisterSavingInstruction(context, inst, usedRegisters);
		}

		// Caller save registers
		// @Todo: save only used registers
		u64 usedCallerSaveRegisters = callerSaveRegisters & usedRegisters;
		s64 callerSaveRegCount = CountOnes(usedCallerSaveRegisters);
		X64Instruction patchTop =    { X64_Patch_Many };
		X64Instruction patchBottom = { X64_Patch_Many };
		ArrayInit(&patchTop.patchInstructions, 1 + 2 * callerSaveRegCount, malloc);
		ArrayInit(&patchBottom.patchInstructions, 1 + callerSaveRegCount, malloc);
		for (int i = 0; i < 64; ++i)
		{
			if (usedCallerSaveRegisters & ((u64)1 << i))
			{
				Variable *var = NewVariable(context, "_save_reg"_s);

				X64Instruction *pushInst = ArrayAdd(&patchTop.patchInstructions);
				*pushInst = { X64_Push_Variable };
				pushInst->variable = var;

				X64Instruction *saveInst = ArrayAdd(&patchTop.patchInstructions);
				*saveInst = { X64_MOV, IRValueMemory(var, 0, TYPETABLEIDX_S64),
					IRValueRegister(i, TYPETABLEIDX_S64) };

				X64Instruction *restoreInst = ArrayAdd(&patchBottom.patchInstructions);
				*restoreInst = { X64_MOV, IRValueRegister(i, TYPETABLEIDX_S64),
					IRValueMemory(var, 0, TYPETABLEIDX_S64) };
			}
		}
		*ArrayAdd(&patchTop.patchInstructions) = proc->instructions[0];
		proc->instructions[0] = patchTop;
		*ArrayAdd(&patchBottom.patchInstructions) = proc->instructions[instructionCount - 1];
		proc->instructions[instructionCount - 1] = patchBottom;
	}

	ResolveStackOffsets(context, x64Procedures);
}
