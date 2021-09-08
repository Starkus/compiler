#if PRINT_IR
void PrintIRInstructions(Context *context)
{
	const int padding = 20;
	const u64 procedureCount = BucketArrayCount(&context->procedures);
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		Procedure *proc = &context->procedures[procedureIdx];
		String returnTypeStr = TypeInfoToString(context, proc->returnTypeTableIdx);

		Log("proc %S(", proc->name);
		for (int paramIdx = 0; paramIdx < proc->parameters.size; ++paramIdx)
		{
			if (paramIdx) Log(", ");
			String typeStr = TypeInfoToString(context, proc->parameters[paramIdx].variable->typeTableIdx);
			Log("param%d : %S", paramIdx, typeStr);
		}
		Log(")");
		if (proc->returnTypeTableIdx != TYPETABLEIDX_VOID)
			Log(" -> %S", returnTypeStr);
		Log("\n");

		const u64 instructionCount = BucketArrayCount(&proc->instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc->instructions[instructionIdx];
			if (inst.type == IRINSTRUCTIONTYPE_LABEL)
			{
				Log("%S: ", inst.label);
				for (s64 i = inst.label.size + 2; i < padding; ++i)
					Log(" ");

				IRInstruction nextInst = proc->instructions[instructionIdx + 1];
				if (nextInst.type != IRINSTRUCTIONTYPE_LABEL)
				{
					++instructionIdx;
					if (instructionIdx >= instructionCount)
						break;
					inst = proc->instructions[instructionIdx];
				}
			}
			else
			{
				for (s64 i = 0; i < padding; ++i)
					Log(" ");
			}

			if (inst.type == IRINSTRUCTIONTYPE_JUMP)
			{
				Log("jump \"%S\"", inst.jump.label);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO)
			{
				Log("if !");
				PrintIRValue(context, inst.conditionalJump.condition);
				Log(" jump %S", inst.conditionalJump.label);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
			{
				Log("call %S", inst.procedureCall.procedure->name);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
			{
				Log("return");
			}
			else if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				Variable *var = inst.variableDeclaration.variable;
				String typeStr = TypeInfoToString(context, var->typeTableIdx);
				Log("variable %S : %S", var->name, typeStr);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			{
				PrintIRValue(context, inst.assignment.dst);
				Log(" = ");
				PrintIRValue(context, inst.assignment.src);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_MEMBER_ACCESS)
			{
				PrintIRValue(context, inst.memberAccess.out);
				Log(" = ");
				PrintIRValue(context, inst.memberAccess.in);

				TypeInfo *structTypeInfo = &context->typeTable[inst.memberAccess.in.typeTableIdx];
				if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
				{
					s64 structTypeInfoIdx = structTypeInfo->pointerInfo.pointedTypeTableIdx;
					structTypeInfo = &context->typeTable[structTypeInfoIdx];
				}
				ASSERT(structTypeInfo->typeCategory == TYPECATEGORY_STRUCT);

				String structName = structTypeInfo->structInfo.name;
				String memberName = inst.memberAccess.structMember->name;
				Log(" + offset(%S::%S)", structName, memberName);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ARRAY_ACCESS)
			{
				PrintIRValue(context, inst.arrayAccess.out);
				Log(" = ");
				PrintIRValue(context, inst.arrayAccess.array);
				Log(" + ");
				PrintIRValue(context, inst.arrayAccess.index);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
			{
				PrintIRValue(context, inst.unaryOperation.out);
				Log(" := ");
				PrintIRInstructionOperator(inst);
				PrintIRValue(context, inst.unaryOperation.in);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
			{
				PrintIRValue(context, inst.binaryOperation.out);
				Log(" := ");
				PrintIRValue(context, inst.binaryOperation.left);
				Log(" ");
				PrintIRInstructionOperator(inst);
				Log(" ");
				PrintIRValue(context, inst.binaryOperation.right);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY)
			{
				Log("memcpy(");
				PrintIRValue(context, inst.memcpy.dst);
				Log(", ");
				PrintIRValue(context, inst.memcpy.src);
				Log(", ");
				PrintIRValue(context, inst.memcpy.size);
				Log(")");
			}
			else
			{
				Log("???INST");
			}
			Log("\n");
		}
	}
	Log("\n");
}
#endif
