struct XEDContext
{
	BucketArrayView<Value> localValues;
	HashMap<u32, ValueAllocation, LinearAllocator> valueAllocations;
};

ValueAllocation XEDGetValueAlloc(XEDContext *xedContext, u32 valueIdx)
{
	if (valueIdx >= RAX.valueIdx && valueIdx < RAX.valueIdx + X64REGISTER_Count)
		return x64RegisterAllocations[valueIdx - RAX.valueIdx];
	else if ((valueIdx & VALUE_GLOBAL_BIT) != 0)
		return x64GlobalValueAllocation;
	else
		return *HashMapGet(xedContext->valueAllocations, valueIdx);
}

Value XEDGetValue(XEDContext *xedContext, u32 valueIdx)
{
	if (valueIdx & VALUE_GLOBAL_BIT)
		return GetGlobalValue(valueIdx);
	else
		return xedContext->localValues[valueIdx];
}

xed_encoder_operand_t X64IRValueToXEDOperand(XEDContext *xedContext, SourceLocation loc, IRValue value,
		Relocation *displacementRelocation)
{
	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_FLOAT);
	ASSERT(value.valueType != IRVALUETYPE_IMMEDIATE_STRING);

	xed_encoder_operand_t result = {};

	u64 size = 0;
	TypeInfo typeInfo = GetTypeInfo(StripAllAliases(value.typeTableIdx));
	bool isXMM;
	size = typeInfo.size;
	s64 offset = 0;

	int bitWidth = (int)size * 8;

	if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		return xed_imm0(value.immediate, bitWidth);

	if (value.valueType == IRVALUETYPE_PROCEDURE) {
		// @Improve: relocation here, don't do special handling for CALL

		// There can only be one displacement per instruction
		ASSERT(displacementRelocation->type == RELOCATIONTYPE_INVALID);
		displacementRelocation->type = RELOCATIONTYPE_PROCEDURE;
		displacementRelocation->procedureIdx = value.procedureIdx;

		return xed_mem_bd(XED_REG_RIP, xed_disp(0, 32), 64);
	}

	if (value.valueType == IRVALUETYPE_MEMORY)
		offset = value.mem.offset;

	Value v = XEDGetValue(xedContext, value.valueIdx);
	ValueAllocation alloc = XEDGetValueAlloc(xedContext, value.valueIdx);

	bool isExternal = v.flags & VALUEFLAGS_IS_EXTERNAL;
	bool isStatic = alloc.flags & VALUEALLOCATIONFLAGS_ON_STATIC_STORAGE;
	if (isStatic || isExternal) {
		if (!isExternal) {
			// There can only be one displacement per instruction
			ASSERT(displacementRelocation->type == RELOCATIONTYPE_INVALID);
			displacementRelocation->type = RELOCATIONTYPE_STATIC_DATA;

			u8 *ptrToStaticData = *(u8 **)HashMapGet(g_context->globalValueContents,
					value.valueIdx & VALUE_GLOBAL_MASK);
			ASSERT(ptrToStaticData >= STATIC_DATA_VIRTUAL_ADDRESS &&
					ptrToStaticData < STATIC_DATA_VIRTUAL_ADDRESS_END);
			offset += (u64)(ptrToStaticData - STATIC_DATA_VIRTUAL_ADDRESS);
		}

		result = xed_mem_bd(XED_REG_RIP, xed_disp(offset, 32), bitWidth);
		goto doIndexScale;
	}

	isXMM = typeInfo.size > 8 || typeInfo.typeCategory == TYPECATEGORY_FLOATING;

	ASSERT(alloc.flags & VALUEALLOCATIONFLAGS_IS_ALLOCATED);
	if (alloc.flags & VALUEALLOCATIONFLAGS_IS_MEMORY) {
		ASSERT(!(alloc.flags & VALUEALLOCATIONFLAGS_FORCE_REGISTER));
		offset += alloc.stackOffset;
		xed_reg_enum_t base;
		if (alloc.flags & VALUEALLOCATIONFLAGS_BASE_RELATIVE)
			base = XED_REG_RBP;
		else
			base = XED_REG_RSP;

		int offsetWidth = 32;
		if ((s8)offset == offset)
			offsetWidth = 8;

		result = xed_mem_bd(base, xed_disp(offset, offsetWidth), bitWidth);
	}
	else if (value.valueType == IRVALUETYPE_MEMORY) {
		ASSERTF(alloc.allocatedRegister <= R15_idx, "Value \"%S\" not allocated to GP register!", v.name);
		xed_reg_enum_t base = x64RegisterToXED[alloc.allocatedRegister];

		int offsetWidth = 32;
		if ((s8)offset == offset)
			offsetWidth = 8;

		result = xed_mem_bd(base, xed_disp(offset, offsetWidth), bitWidth);
	}
	else if (!isXMM) {
		if (alloc.allocatedRegister >= XMM0_idx)
#if DEBUG_BUILD
			LogCompilerError(loc, TPrintF("Value \"%S\" not allocated to GP register!",
						v.name));
#else
			LogCompilerError(loc, "Value not allocated to GP register!"_s);
#endif
		xed_reg_enum_t reg;
		switch (size) {
		case 8:
			reg = x64RegisterToXED  [alloc.allocatedRegister]; break;
		case 4:
			reg = x64RegisterToXED32[alloc.allocatedRegister]; break;
		case 2:
			reg = x64RegisterToXED16[alloc.allocatedRegister]; break;
		case 1:
			reg = x64RegisterToXED8 [alloc.allocatedRegister]; break;
		default:
			ASSERTF(false, "Invalid size for a register!");
		}
		ASSERT(offset == 0);
		result = xed_reg(reg);
	}
	else {
#if DEBUG_BUILD
		ASSERTF(alloc.allocatedRegister >= XMM0_idx && alloc.allocatedRegister <= XMM15_idx,
				"Value \"%S\" not allocated to XMM register!", v.name);
#else
		ASSERTF(alloc.allocatedRegister >= XMM0_idx && alloc.allocatedRegister <= XMM15_idx,
				"Value not allocated to XMM register!");
#endif
		result = xed_reg(x64RegisterToXED[alloc.allocatedRegister]);
	}

doIndexScale:
	if (result.u.mem.disp.displacement == 0)
		result.u.mem.disp.displacement_bits = 0;
	if (value.valueType == IRVALUETYPE_MEMORY && value.mem.elementSize > 0) {
		ASSERT(!(value.valueIdx & VALUE_GLOBAL_BIT));
		ValueAllocation indexValueAlloc = XEDGetValueAlloc(xedContext, value.mem.indexValueIdx);
		ASSERT(indexValueAlloc.flags & VALUEALLOCATIONFLAGS_IS_ALLOCATED);
		ASSERT(!(indexValueAlloc.flags & VALUEALLOCATIONFLAGS_IS_MEMORY));
		xed_reg_enum_t xedIndex = x64RegisterToXED[indexValueAlloc.allocatedRegister];

		ASSERT(result.type == XED_ENCODER_OPERAND_TYPE_MEM);
		result.u.mem.index = xedIndex;
		result.u.mem.scale = (u32)value.mem.elementSize;
	}

	return result;
}

int X64EncodeInstruction(XEDContext *xedContext, X64Instruction x64Inst, u8 *buffer)
{
	Relocation displacementRelocation = {};

	xed_iclass_enum_t xedIClass = x64InstructionInfos[x64Inst.type].xedIClass;

	xed_state_t dstate64 = {};
	dstate64.stack_addr_width = XED_ADDRESS_WIDTH_64b;
	dstate64.mmode = XED_MACHINE_MODE_LONG_64;

	xed_encoder_instruction_t inst;

	switch (x64Inst.type) {
	case X64_CALL:
	{
		if (!(x64Inst.procedureIdx & PROCEDURE_EXTERNAL_BIT)) {
			// I believe all near call instructions only have 1 byte before the displacement
			*DynamicArrayAdd(&g_relocations) = {
				.type = RELOCATIONTYPE_PROCEDURE,
				.procedureIdx = x64Inst.procedureIdx,
				.destOffset = g_context->outputBufferOffset + 1
			};
		}
		else {
			*DynamicArrayAdd(&g_relocations) = {
				.type = RELOCATIONTYPE_EXTERNAL_PROCEDURE,
				.procedureIdx = x64Inst.procedureIdx & PROCEDURE_EXTERNAL_MASK,
				.destOffset = g_context->outputBufferOffset + 1
			};
		}

		xed_encoder_operand_t dst = xed_relbr(0, 32);
		xed_inst1(&inst, dstate64, xedIClass, 0, dst);
		goto encode;
	}
	case X64_CALL_Indirect:
	{
		xed_encoder_operand_t dst = X64IRValueToXEDOperand(xedContext, x64Inst.loc,
				x64Inst.procedureIRValue, &displacementRelocation);
		xed_inst1(&inst, dstate64, xedIClass, 64, dst);
		goto encode;
	}
	case X64_JMP:
	{
		// Jmp has 1 byte of opcode before the offset. Same as CALL, only 64 bit jump far has a
		// REX.W prefix.
		*DynamicArrayAdd(&g_relocations) = {
			.type = RELOCATIONTYPE_LABEL,
			.label = x64Inst.label,
			.destOffset = g_context->outputBufferOffset + 1
		};
		// @Todo: smaller displacement bitwidth when possible
		xed_encoder_operand_t disp = xed_relbr(0xCCCCCCCC, 32);
		xed_inst1(&inst, dstate64, xedIClass, 0, disp);
		goto encode;
	}
	case X64_JE:
	case X64_JNE:
	case X64_JG:
	case X64_JL:
	case X64_JGE:
	case X64_JLE:
	case X64_JA:
	case X64_JB:
	case X64_JAE:
	case X64_JBE:
	{
		// All jumps with 32 bit offsets seem to have a 2-byte opcode.
		// @Important: if we implement short jumps (8bit displacement) the opcode is 1 byte! Change
		// it on the relocation record!
		*DynamicArrayAdd(&g_relocations) = {
			.type = RELOCATIONTYPE_LABEL,
			.label = x64Inst.label,
			.destOffset = g_context->outputBufferOffset + 2
		};
		// @Todo: smaller displacement bitwidth when possible
		xed_encoder_operand_t disp = xed_relbr(0xCCCCCCCC, 32);
		xed_inst1(&inst, dstate64, xedIClass, 0, disp);
		goto encode;
	}
	case X64_Label:
		x64Inst.label->address = g_context->outputBufferOffset;
		return 0;
	case X64_Comment:
	case X64_Ignore:
	case X64_Push_Scope:
	case X64_Pop_Scope:
	case X64_Push_Value:
		return 0;
	case X64_LEAVE:
	case X64_RET:
		// inst0 with 64 bit width
		xed_inst0(&inst, dstate64, xedIClass, 64);
		goto encode;
	default:
	{
		X64InstructionInfo instInfo = x64InstructionInfos[x64Inst.type];
		if (instInfo.operandTypesLeft != OPERANDTYPE_NONE) {
			if (instInfo.operandTypesRight != OPERANDTYPE_NONE) {
				if (instInfo.operandTypesDest != OPERANDTYPE_NONE)
					goto inst3;
				else
					goto inst2;
			}
			else
				goto inst1;
		}
		else
			goto inst0;
	}
	}

inst0:
	{
		int bitWidth = 0;
		xed_inst0(&inst, dstate64, xedIClass, bitWidth);
		goto encode;
	}
inst1:
	{
		int bitWidth = (int)GetTypeInfo(x64Inst.dst.typeTableIdx).size * 8;
		xed_encoder_operand_t dst = X64IRValueToXEDOperand(xedContext, x64Inst.loc, x64Inst.dst,
				&displacementRelocation);
		xed_inst1(&inst, dstate64, xedIClass, bitWidth, dst);
		goto encode;
	}
inst2:
	{
		int bitWidth = (int)GetTypeInfo(x64Inst.dst.typeTableIdx).size * 8;
		xed_encoder_operand_t dst = X64IRValueToXEDOperand(xedContext, x64Inst.loc, x64Inst.dst,
				&displacementRelocation);
		xed_encoder_operand_t src = X64IRValueToXEDOperand(xedContext, x64Inst.loc, x64Inst.src,
				&displacementRelocation);
		xed_inst2(&inst, dstate64, xedIClass, bitWidth, dst, src);
		goto encode;
	}
inst3:
	{
		int bitWidth = (int)GetTypeInfo(x64Inst.dst.typeTableIdx).size * 8;
		xed_encoder_operand_t dst  = X64IRValueToXEDOperand(xedContext, x64Inst.loc, x64Inst.dst,
				&displacementRelocation);
		xed_encoder_operand_t src  = X64IRValueToXEDOperand(xedContext, x64Inst.loc, x64Inst.src,
				&displacementRelocation);
		xed_encoder_operand_t src2 = X64IRValueToXEDOperand(xedContext, x64Inst.loc, x64Inst.src2,
				&displacementRelocation);
		xed_inst3(&inst, dstate64, xedIClass, bitWidth, dst, src, src2);
		goto encode;
	}
encode:
	{
		xed_encoder_request_t req;
		xed_convert_to_encoder_request(&req, &inst);
		unsigned int len;
		xed_error_enum_t error = xed_encode(&req, buffer, 16, &len);
		if (error != XED_ERROR_NONE)
			LogCompilerError(x64Inst.loc, "Could not encode instruction"_s);
		ASSERT(len > 0 && len < 16);

		if (displacementRelocation.type != RELOCATIONTYPE_INVALID) {
			s32 sizeOfImmediate = xed_decoded_inst_get_immediate_width(&req);
			displacementRelocation.destOffset = g_context->outputBufferOffset + len;
			displacementRelocation.destOffset -= sizeOfImmediate;
			displacementRelocation.destOffset -= 4; // Size of displacement
			displacementRelocation.offsetShift = sizeOfImmediate;
			*DynamicArrayAdd(&g_relocations) = displacementRelocation;
		}

		return len;
	}
}

void X64EncodeAllInstructions()
{
	XEDContext ctx = {};
	XEDContext *xedContext = &ctx;

	auto beFinalProcedureData = g_context->beFinalProcedureData.GetForRead();
	int procCount = (int)beFinalProcedureData->size;
	for (int finalProcIdx = 0; finalProcIdx < procCount; ++finalProcIdx) {
		X64FinalProcedure finalProc = beFinalProcedureData[finalProcIdx];

		xedContext->localValues = finalProc.localValues;
		xedContext->valueAllocations = finalProc.valueAllocations;

		g_procedureAddresses[finalProc.procedureIdx] = g_context->outputBufferOffset;

		u8 buffer[16];
		int bytes;

		// push rbp
		bytes = X64EncodeInstruction(xedContext, { {}, X64_PUSH, RBP }, buffer);
		OutputBufferPut(bytes, buffer);
		// mov rbp, rsp
		bytes = X64EncodeInstruction(xedContext, { {}, X64_MOV, RBP, RSP }, buffer);
		OutputBufferPut(bytes, buffer);
		if (finalProc.stackSize > 0) {
			// sub rsp, $stack_size
			bytes = X64EncodeInstruction(xedContext, { {}, X64_SUB, RSP,
					IRValueImmediate(finalProc.stackSize, TYPETABLEIDX_U32) }, buffer);
			OutputBufferPut(bytes, buffer);
		}

		X64InstructionStream stream = X64InstructionStreamBegin(&finalProc.instructions);
		X64Instruction *inst = X64InstructionStreamAdvance(&stream);
		while (inst) {
			bytes = X64EncodeInstruction(xedContext, *inst, buffer);
			OutputBufferPut(bytes, buffer);
			inst = X64InstructionStreamAdvance(&stream);
		}

		// leave
		bytes = X64EncodeInstruction(xedContext, { {}, X64_LEAVE }, buffer);
		OutputBufferPut(bytes, buffer);
		// ret
		bytes = X64EncodeInstruction(xedContext, { {}, X64_RET }, buffer);
		OutputBufferPut(bytes, buffer);
	}
}
