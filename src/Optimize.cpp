struct BasicBlock
{
	Procedure *procedure;
	s64 beginIdx;
	s64 endIdx;
	bool livenessAnalizedOnce;
	DynamicArray<BasicBlock *, malloc, realloc> inputs;
	DynamicArray<BasicBlock *, malloc, realloc> outputs;

	// @Todo: bitmaps
	DynamicArray<s64, malloc, realloc> liveRegistersAtInput;
	DynamicArray<s64, malloc, realloc> liveRegistersAtOutput;
};

struct InterferenceGraphNode
{
	s64 registerIdx;
	DynamicArray<s64, malloc, realloc> edges; // @Improve: eugh
	bool removed;
};

#if DEBUG_OPTIMIZER
void PrintBasicBlock(Context *context, BasicBlock *basicBlock)
{
	Print(">>> START BASIC BLOCK\n");
	for (s64 i = basicBlock->beginIdx; i <= basicBlock->endIdx; ++i)
	{
		IRInstruction inst = basicBlock->procedure->instructions[i];
		PrintIRInstruction(context, inst);
	}
	Print(">>> END BASIC BLOCK\n");
	Print("\n\n");
}
#endif

BasicBlock *PushBasicBlock(Context *context, BasicBlock *currentBasicBlock)
{
	if (currentBasicBlock && currentBasicBlock->endIdx <= currentBasicBlock->beginIdx)
	{
		// If current block is empty don't create a new one
		//return currentBasicBlock;
	}

	Procedure *procedure = nullptr;
	s64 endOfLastBlock = -1;
	if (currentBasicBlock)
	{
		procedure = currentBasicBlock->procedure;
		endOfLastBlock = currentBasicBlock->endIdx;
	}

	BasicBlock *result = BucketArrayAdd(&context->basicBlocks);
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
		DynamicArray<s64, malloc, realloc> *array)
{
	s64 registerIdx;
	if (value.valueType == IRVALUETYPE_REGISTER)
		registerIdx = value.registerIdx;
	else if (value.valueType == IRVALUETYPE_MEMORY && !value.memory.baseVariable)
		registerIdx = value.memory.baseRegister;
	else
		return false;

	if (registerIdx >= IRSPECIALREGISTER_BEGIN)
		return false;

	for (int i = 0; i < array->size; ++i)
	{
		if ((*array)[i] == registerIdx)
			return false; // No duplicates
	}
	*DynamicArrayAdd(array) = registerIdx;
	return true;
}

inline void RemoveIfRegister(BasicBlock *basicBlock, IRValue value,
		DynamicArray<s64, malloc, realloc> *array)
{
	if (value.valueType == IRVALUETYPE_REGISTER)
	{
		for (int i = 0; i < array->size; ++i)
		{
			if ((*array)[i] == value.registerIdx)
				(*array)[i] = (*array)[--array->size];
		}
	}
	else if (value.valueType == IRVALUETYPE_MEMORY && !value.memory.baseVariable)
	{
		// The register is actually _used_ here, and not written to. Add instead.
		IRValue reg = IRValueRegister(value.memory.baseRegister, value.typeTableIdx);
		AddIfRegister(basicBlock, reg, array);
	}
}

inline void ReplaceIfRegister(IRValue *value, Array<s64> registerMap)
{
	if (value->valueType == IRVALUETYPE_REGISTER && value->registerIdx < IRSPECIALREGISTER_BEGIN)
	{
		if (value->registerIdx < (s64)registerMap.size)
			value->registerIdx = registerMap[value->registerIdx];
	}
	else if (value->valueType == IRVALUETYPE_MEMORY && !value->memory.baseVariable)
	{
		if (value->memory.baseRegister < (s64)registerMap.size)
			value->memory.baseRegister = registerMap[value->memory.baseRegister];
	}
}

void DoLivenessAnalisisOnInstruction(Context *context, BasicBlock *basicBlock, IRInstruction inst,
		DynamicArray<s64, malloc, realloc> *liveRegisters)
{
	if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
	{
		RemoveIfRegister(basicBlock, inst.unaryOperation.out, liveRegisters);
		AddIfRegister(basicBlock, inst.unaryOperation.in, liveRegisters);
	}
	else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
	{
		RemoveIfRegister(basicBlock, inst.binaryOperation.out, liveRegisters);
		AddIfRegister(basicBlock, inst.binaryOperation.left, liveRegisters);
		AddIfRegister(basicBlock, inst.binaryOperation.right, liveRegisters);
	}
	else switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		RemoveIfRegister(basicBlock, inst.assignment.dst, liveRegisters);
		AddIfRegister(basicBlock, inst.assignment.src, liveRegisters);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		RemoveIfRegister(basicBlock, inst.procedureCall.out, liveRegisters);
		for (int paramIdx = 0; paramIdx < inst.procedureCall.parameters.size; ++paramIdx)
			AddIfRegister(basicBlock, inst.procedureCall.parameters[paramIdx], liveRegisters);
	} break;
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		AddIfRegister(basicBlock, inst.memcpy.src, liveRegisters);
		AddIfRegister(basicBlock, inst.memcpy.dst, liveRegisters);
		AddIfRegister(basicBlock, inst.memcpy.size, liveRegisters);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		AddIfRegister(basicBlock, inst.conditionalJump.condition, liveRegisters);
	} break;
	case IRINSTRUCTIONTYPE_RETURN:
	{
		AddIfRegister(basicBlock, IRValueRegister(IRSPECIALREGISTER_RETURN),
				liveRegisters);
	} break;
	case IRINSTRUCTIONTYPE_PATCH:
	{
		DoLivenessAnalisisOnInstruction(context, basicBlock, *inst.patch.second, liveRegisters);
		DoLivenessAnalisisOnInstruction(context, basicBlock, *inst.patch.first,  liveRegisters);
		return;
	} break;
	}

#if DEBUG_OPTIMIZER
	Print(">\t");
	PrintIRInstruction(context, inst);
	Print("Live registers: ");
	for (int i = 0; i < liveRegisters->size; ++i)
		Print("%d, ", (*liveRegisters)[i]);
	Print("\n");
#endif

	// Add edges to graph
	for (int i = 0; i < liveRegisters->size; ++i)
	{
		s64 reg = (*liveRegisters)[i];
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
		DynamicArray<s64, malloc, realloc> *liveRegisters)
{
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

#if DEBUG_OPTIMIZER
	static int stackLevels = 0;

	Print("------ Static block (%d) -------------\n", stackLevels++);
	Print("Input: ");
	for (int i = 0; i < liveRegisters->size; ++i)
		Print("%d, ", (*liveRegisters)[i]);
	Print("\n");
#endif

	for (s64 instructionIdx = basicBlock->endIdx; instructionIdx >= basicBlock->beginIdx;
			--instructionIdx)
	{
		IRInstruction inst = basicBlock->procedure->instructions[instructionIdx];
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
	{
#if DEBUG_OPTIMIZER
		Print("DONE\n");
#endif
		return;
	}

#if DEBUG_OPTIMIZER
	Print("Output: ");
	for (int i = 0; i < liveRegisters->size; ++i)
		Print("%d, ", (*liveRegisters)[i]);
	Print("\n");
#endif

	basicBlock->livenessAnalizedOnce = true;

	for (int i = 0; i < basicBlock->inputs.size; ++i)
	{
		BasicBlock *inputBlock = basicBlock->inputs[i];
		// Copy live registers array
		DynamicArray<s64, malloc, realloc> liveRegistersCopy;
		DynamicArrayInit(&liveRegistersCopy, liveRegisters->capacity);
		DynamicArrayCopy(&liveRegistersCopy, liveRegisters);

		DoLivenessAnalisis(context, inputBlock, &liveRegistersCopy);
	}

#if DEBUG_OPTIMIZER
	--stackLevels;
#endif
}

void GenerateBasicBlocks(Context *context)
{
	const u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];

		// @Speed: separate array of external procedures to avoid branching
		if (proc->isExternal)
			continue;

		BasicBlock *currentBasicBlock = PushBasicBlock(context, nullptr);
		currentBasicBlock->procedure = proc;

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];

			switch (inst.type)
			{
			case IRINSTRUCTIONTYPE_LABEL:
			{
				currentBasicBlock->endIdx = instructionIdx - 1;
				BasicBlock *previousBlock = currentBasicBlock;
				currentBasicBlock = PushBasicBlock(context, currentBasicBlock);

				*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
				*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
			} break;
			case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
			case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
			{
				currentBasicBlock->endIdx = instructionIdx;
				BasicBlock *previousBlock = currentBasicBlock;
				currentBasicBlock = PushBasicBlock(context, currentBasicBlock);

				*DynamicArrayAdd(&currentBasicBlock->inputs) = previousBlock;
				*DynamicArrayAdd(&previousBlock->outputs) = currentBasicBlock;
			} break;
			case IRINSTRUCTIONTYPE_JUMP:
			{
				currentBasicBlock->endIdx = instructionIdx;
				currentBasicBlock = PushBasicBlock(context, currentBasicBlock);
			} break;
			case IRINSTRUCTIONTYPE_RETURN:
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
		IRInstruction endInstruction = jumpBlock->procedure->instructions[jumpBlock->endIdx];
		if (endInstruction.type == IRINSTRUCTIONTYPE_JUMP)
			label = endInstruction.jump.label;
		else if (endInstruction.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO ||
				endInstruction.type == IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO)
			label = endInstruction.conditionalJump.label;
		else
			continue;

		for (int j = 0; j < basicBlockCount; ++j)
		{
			BasicBlock *labelBlock = &context->basicBlocks[j];
			IRInstruction beginInstruction =
				labelBlock->procedure->instructions[labelBlock->beginIdx];

			if (beginInstruction.type == IRINSTRUCTIONTYPE_LABEL &&
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

inline void ReplaceRegisterForVariable(Context *context, Procedure *procedure,
		IRInstruction *instruction, IRValue *value, s64 registerIdx, Variable *variable)
{
	if (value->valueType == IRVALUETYPE_MEMORY && !value->memory.baseVariable &&
			value->memory.baseRegister == registerIdx)
	{
		s64 newVirtualRegister = procedure->registerCount++;
		IRValue tmp = IRValueRegister(newVirtualRegister, value->typeTableIdx);

		IRInstruction *newInst1 = BucketArrayAdd(&context->patchedInstructions);
		newInst1->type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		newInst1->assignment.dst = tmp;
		newInst1->assignment.src = IRValueMemory(variable, 0, value->typeTableIdx);

		*value = IRValueMemory(tmp.registerIdx, value->memory.offset, value->typeTableIdx);

		IRInstruction *newInst2 = BucketArrayAdd(&context->patchedInstructions);
		*newInst2 = *instruction;

		IRInstruction patchInstruction;
		patchInstruction.type = IRINSTRUCTIONTYPE_PATCH;
		patchInstruction.patch.first = newInst1;
		patchInstruction.patch.second = newInst2;
		*instruction = patchInstruction;
	}
	else if (value->valueType == IRVALUETYPE_REGISTER && value->registerIdx == registerIdx)
		*value = IRValueMemory(variable, 0, value->typeTableIdx);
}

void SpillRegisterIntoMemoryInstruction(Context *context, Variable *newVar, IRInstruction *inst,
		Procedure *procedure, s64 registerIdx)
{
	if (inst->type >= IRINSTRUCTIONTYPE_UNARY_BEGIN &&
			inst->type < IRINSTRUCTIONTYPE_UNARY_END)
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->unaryOperation.in, registerIdx, newVar);
		ReplaceRegisterForVariable(context, procedure, inst, &inst->unaryOperation.out, registerIdx, newVar);
	}
	else if (inst->type >= IRINSTRUCTIONTYPE_BINARY_BEGIN &&
			inst->type < IRINSTRUCTIONTYPE_BINARY_END)
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->binaryOperation.left, registerIdx, newVar);
		ReplaceRegisterForVariable(context, procedure, inst, &inst->binaryOperation.right, registerIdx, newVar);
		ReplaceRegisterForVariable(context, procedure, inst, &inst->binaryOperation.out, registerIdx, newVar);
	}
	else switch (inst->type)
	{
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->assignment.src, registerIdx, newVar);
		ReplaceRegisterForVariable(context, procedure, inst, &inst->assignment.dst, registerIdx, newVar);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->procedureCall.out, registerIdx, newVar);

		for (int paramIdx = 0; paramIdx < inst->procedureCall.parameters.size; ++paramIdx)
			ReplaceRegisterForVariable(context, procedure, inst, &inst->procedureCall.parameters[paramIdx], registerIdx, newVar);
	} break;
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->memcpy.src, registerIdx, newVar);
		ReplaceRegisterForVariable(context, procedure, inst, &inst->memcpy.dst, registerIdx, newVar);
		ReplaceRegisterForVariable(context, procedure, inst, &inst->memcpy.size, registerIdx, newVar);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		ReplaceRegisterForVariable(context, procedure, inst, &inst->conditionalJump.condition, registerIdx, newVar);
	} break;
	case IRINSTRUCTIONTYPE_PATCH:
	{
		SpillRegisterIntoMemoryInstruction(context, newVar, inst->patch.first,  procedure, registerIdx);
		SpillRegisterIntoMemoryInstruction(context, newVar, inst->patch.second, procedure, registerIdx);
	} break;
	}
}

void SpillRegisterIntoMemory(Context *context, Procedure *procedure, s64 registerIdx)
{
#if DEBUG_OPTIMIZER
	Print("Spilling r%lld!\n", registerIdx);
#endif

	Variable *newVar = NewVariable(context, TPrintF("_spill_r%lld", registerIdx), TYPETABLEIDX_S64);

	IRInstruction *newInst1 = BucketArrayAdd(&context->patchedInstructions);
	newInst1->type = IRINSTRUCTIONTYPE_PUSH_VARIABLE;
	newInst1->pushVariable.variable = newVar;

	IRInstruction *newInst2 = BucketArrayAdd(&context->patchedInstructions);
	*newInst2 = procedure->instructions[0];

	IRInstruction patchInst;
	patchInst.type = IRINSTRUCTIONTYPE_PATCH;
	patchInst.patch.first = newInst1;
	patchInst.patch.second = newInst2;
	procedure->instructions[0] = patchInst;

	u64 instructionCount = BucketArrayCount(&procedure->instructions);
	for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
	{
		IRInstruction *inst = &procedure->instructions[instructionIdx];
		SpillRegisterIntoMemoryInstruction(context, newVar, inst, procedure, registerIdx);
	}
}

void ResolveInstructionStackOffsets(Context *context, IRInstruction inst,
	DynamicArray<s64, malloc, realloc> *stack, s64 *stackCursor, u64 *stackSize)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_PUSH_VARIABLE:
	{
		Variable *var = inst.pushVariable.variable;
		u64 size = context->typeTable[var->typeTableIdx].size;
		*stackCursor -= size;
		var->stackOffset = *stackCursor;
		var->isAllocated = true;
	} break;
	case IRINSTRUCTIONTYPE_PUSH_SCOPE:
	{
		*DynamicArrayAdd(stack) = *stackCursor;
	} break;
	case IRINSTRUCTIONTYPE_POP_SCOPE:
	{
		if (-*stackCursor > (s64)*stackSize)
			*stackSize = -*stackCursor;
		*stackCursor = (*stack)[--stack->size];
	} break;
	case IRINSTRUCTIONTYPE_PATCH:
	{
		ResolveInstructionStackOffsets(context, *inst.patch.first,  stack, stackCursor, stackSize);
		ResolveInstructionStackOffsets(context, *inst.patch.second, stack, stackCursor, stackSize);
	} break;
	}
}

void ResolveStackOffsets(Context *context)
{
	DynamicArray<s64, malloc, realloc> stack;
	DynamicArrayInit(&stack, 16);

	const u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];
		s64 stackCursor = 0;

		// @Speed: separate array of external procedures to avoid branching
		if (proc->isExternal)
			continue;

		BasicBlock *currentBasicBlock = PushBasicBlock(context, nullptr);
		currentBasicBlock->procedure = proc;

		u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];
			ResolveInstructionStackOffsets(context, inst, &stack, &stackCursor, &proc->stackSize);
		}
	}
}

void ReplaceRegistersInInstruction(IRInstruction *inst, Array<s64> registerMap)
{
	if (inst->type >= IRINSTRUCTIONTYPE_UNARY_BEGIN &&
			inst->type < IRINSTRUCTIONTYPE_UNARY_END)
	{
		ReplaceIfRegister(&inst->unaryOperation.in, registerMap);
		ReplaceIfRegister(&inst->unaryOperation.out, registerMap);
	}
	else if (inst->type >= IRINSTRUCTIONTYPE_BINARY_BEGIN &&
			inst->type < IRINSTRUCTIONTYPE_BINARY_END)
	{
		ReplaceIfRegister(&inst->binaryOperation.left, registerMap);
		ReplaceIfRegister(&inst->binaryOperation.right, registerMap);
		ReplaceIfRegister(&inst->binaryOperation.out, registerMap);
	}
	else switch (inst->type)
	{
	case IRINSTRUCTIONTYPE_ASSIGNMENT:
	{
		IRValue *src = &inst->assignment.src;
		IRValue *dst = &inst->assignment.dst;
		if (dst->valueType == IRVALUETYPE_REGISTER && dst->registerIdx < IRSPECIALREGISTER_BEGIN)
		{
			if ((u64)dst->registerIdx >= registerMap.size)
			{
				return;
			}
			else if (registerMap[dst->registerIdx] == -1) // @Cleanup: I hate this!
			{
				// Replace assignment to unused register for a NOP.
				// Wanted to be specific with the '-1' here so we are more likely to see when
				// garbage gets into the register map.
				inst->type = IRINSTRUCTIONTYPE_NOP;
				return;
			}
		}
		ReplaceIfRegister(src, registerMap);
		ReplaceIfRegister(dst, registerMap);
	} break;
	case IRINSTRUCTIONTYPE_PROCEDURE_CALL:
	{
		IRValue *out = &inst->procedureCall.out;
		if (out->valueType == IRVALUETYPE_REGISTER &&
				out->registerIdx < IRSPECIALREGISTER_BEGIN &&
				registerMap[out->registerIdx] == -1)
			// Remove unused return value.
			// Wanted to be specific with the '-1' here so we are more likely to see when
			// garbage gets into the register map.
			out->valueType = IRVALUETYPE_INVALID;
		else
			ReplaceIfRegister(out, registerMap);

		for (int paramIdx = 0; paramIdx < inst->procedureCall.parameters.size; ++paramIdx)
			ReplaceIfRegister(&inst->procedureCall.parameters[paramIdx], registerMap);
	} break;
	case IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY:
	{
		ReplaceIfRegister(&inst->memcpy.src, registerMap);
		ReplaceIfRegister(&inst->memcpy.dst, registerMap);
		ReplaceIfRegister(&inst->memcpy.size, registerMap);
	} break;
	case IRINSTRUCTIONTYPE_JUMP_IF_ZERO:
	case IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO:
	{
		ReplaceIfRegister(&inst->conditionalJump.condition, registerMap);
	} break;
	case IRINSTRUCTIONTYPE_PATCH:
	{
		ReplaceRegistersInInstruction(inst->patch.first,  registerMap);
		ReplaceRegistersInInstruction(inst->patch.second, registerMap);
	} break;
	}
}

void OptimizerMain(Context *context)
{
	BucketArrayInit(&context->basicBlocks);
	DynamicArrayInit(&context->leafBasicBlocks, 128);
	DynamicArrayInit(&context->interferenceGraph, 128);
	BucketArrayInit(&context->patchedInstructions);

	GenerateBasicBlocks(context);

	// Do liveness analisis, starting from all leaf blocks
	for (int leafIdx = 0; leafIdx < context->leafBasicBlocks.size; ++leafIdx)
	{
		BasicBlock *currentLeafBlock = context->leafBasicBlocks[leafIdx];

		String procName = ""_s;
		StaticDefinition *staticDef = FindStaticDefinitionByProcedure(context,
				currentLeafBlock->procedure);
		if (staticDef)
			procName = staticDef->name;
#if DEBUG_OPTIMIZER
		Print("--------------- New procedure: %S\n", procName);
#endif

		// Assign real registers to virtual registers
		int availableRegisters = 8;
		while (true)
		{
			bool spilledSomething = false;
			context->interferenceGraph.size = 0;

			// @Todo: iterative instead of recursive?
			DynamicArray<s64, malloc, realloc> liveRegisters;
			DynamicArrayInit(&liveRegisters, 16);
			DoLivenessAnalisis(context, currentLeafBlock, &liveRegisters);

#if DEBUG_OPTIMIZER
			for (int nodeIdx = 0; nodeIdx < context->interferenceGraph.size; ++nodeIdx)
			{
				InterferenceGraphNode *currentNode = &context->interferenceGraph[nodeIdx];
				Print("Register %lld coexists with: ", currentNode->registerIdx);
				for (int i = 0; i < currentNode->edges.size; ++i)
					Print("%lld, ", currentNode->edges[i]);
				Print("\n");
			}
#endif

			Array<InterferenceGraphNode *> nodeStack;
			ArrayInit(&nodeStack, context->interferenceGraph.size, malloc);

			s64 highestVirtualRegister = -1;
			for (int nodeIdx = 0; nodeIdx < context->interferenceGraph.size; ++nodeIdx)
			{
				s64 currentReg = context->interferenceGraph[nodeIdx].registerIdx;
				if (currentReg > highestVirtualRegister)
					highestVirtualRegister = currentReg;
			}
			Array<s64> registerMap;
			ArrayInit(&registerMap, highestVirtualRegister + 1, malloc);
			registerMap.size = highestVirtualRegister + 1;
			memset(registerMap.data, 0xFF, registerMap.size * sizeof(s64));

			// Map all registers to fewer non-colliding registers
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
					// Spill!
					SpillRegisterIntoMemory(context, currentLeafBlock->procedure,
							currentNode->registerIdx);
					spilledSomething = true;
				}
			}

#if DEBUG_OPTIMIZER
			Print("\nMappings:\n");
			for (u64 reg = 0; reg < registerMap.size; ++reg)
			{
				Print("Register %lld -> %lld\n", reg, registerMap[reg]);
			}
#endif

			// Replace registers in IR code
			Procedure *proc = currentLeafBlock->procedure;
			u64 instructionCount = BucketArrayCount(&proc->instructions);
			for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
			{
				IRInstruction *inst = &proc->instructions[instructionIdx];
				ReplaceRegistersInInstruction(inst, registerMap);
			}

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

	ResolveStackOffsets(context);
}
