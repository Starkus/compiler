#if PRINT_IR
void PrintIRInstructions(Context *context)
{
	const int padding = 20;
	const u64 procedureCount = context->irProcedures.size;
	for (int procedureIdx = 0; procedureIdx < procedureCount; ++procedureIdx)
	{
		IRProcedure proc = context->irProcedures[procedureIdx];
		String returnTypeStr = IRTypeInfoToStr(proc.returnTypeInfo);

		Log("proc %.*s(", proc.name.size, proc.name.data);
		Log(")");
		if (proc.returnTypeInfo.type != IRTYPE_VOID)
			Log(" -> %.*s", returnTypeStr.size, returnTypeStr.data);
		Log("\n");

		const u64 instructionCount = BucketArrayCount(&proc.instructions);
		for (int instructionIdx = 0; instructionIdx < instructionCount; ++instructionIdx)
		{
			IRInstruction inst = proc.instructions[instructionIdx];
			if (inst.type == IRINSTRUCTIONTYPE_LABEL)
			{
				Log("%.*s: ", inst.label.size, inst.label.data);
				for (s64 i = inst.label.size + 2; i < padding; ++i)
					Log(" ");

				IRInstruction nextInst = proc.instructions[instructionIdx + 1];
				if (nextInst.type != IRINSTRUCTIONTYPE_LABEL)
				{
					++instructionIdx;
					if (instructionIdx >= instructionCount)
						break;
					inst = proc.instructions[instructionIdx];
				}
			}
			else
			{
				for (s64 i = 0; i < padding; ++i)
					Log(" ");
			}

			if (inst.type == IRINSTRUCTIONTYPE_JUMP)
			{
				Log("jump \"%.*s\"", inst.jump.label.size, inst.jump.label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_JUMP_IF_ZERO)
			{
				Log("if !");
				PrintIRValue(inst.conditionalJump.condition);
				Log(" jump %.*s", inst.conditionalJump.label.size, inst.conditionalJump.label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_PROCEDURE_CALL)
			{
				Log("call %.*s", inst.procedureCall.label.size, inst.procedureCall.label.data);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_RETURN)
			{
				Log("return ");
				PrintIRValue(inst.returnValue);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_VARIABLE_DECLARATION)
			{
				Log("%.*s : %d bytes", inst.variableDeclaration.name.size, inst.variableDeclaration.name.data,
						inst.variableDeclaration.size);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_ASSIGNMENT)
			{
				PrintIRValue(inst.assignment.dst);
				Log(" = ");
				PrintIRValue(inst.assignment.src);
			}
			else if (inst.type == IRINSTRUCTIONTYPE_MEMBER_ACCESS)
			{
				PrintIRValue(inst.memberAccess.out);
				Log(" = ");
				PrintIRValue(inst.memberAccess.in);
				Log(" -> offset(%.*s::%.*s)",
						inst.memberAccess.structName.size, inst.memberAccess.structName.data,
						inst.memberAccess.memberName.size, inst.memberAccess.memberName.data);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_UNARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_UNARY_END)
			{
				PrintIRValue(inst.unaryOperation.out);
				Log(" := ");
				PrintIRInstructionOperator(inst);
				PrintIRValue(inst.unaryOperation.in);
			}
			else if (inst.type >= IRINSTRUCTIONTYPE_BINARY_BEGIN && inst.type < IRINSTRUCTIONTYPE_BINARY_END)
			{
				PrintIRValue(inst.binaryOperation.out);
				Log(" := ");
				PrintIRValue(inst.binaryOperation.left);
				Log(" ");
				PrintIRInstructionOperator(inst);
				Log(" ");
				PrintIRValue(inst.binaryOperation.right);
			}
			Log("\n");
		}
	}
	Log("\n");
}
#endif
