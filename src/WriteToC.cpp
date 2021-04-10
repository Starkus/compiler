void WriteExpression(ASTExpression *expression);
void WriteStatement(ASTExpression *statement);

void WriteOperator(s32 op)
{
	switch (op)
	{
		case TOKEN_OP_ASSIGNMENT:
			Log("="); break;
		case TOKEN_OP_EQUALS:
			Log("=="); break;
		case TOKEN_OP_PLUS:
			Log("+"); break;
		case TOKEN_OP_MINUS:
			Log("-"); break;
		case TOKEN_OP_MULTIPLY:
			Log("*"); break;
		case TOKEN_OP_DIVIDE:
			Log("/"); break;
		case TOKEN_OP_ARROW:
			Log("->"); break;
		case TOKEN_OP_VARIABLE_DECLARATION:
			Log("!Variable Dec Operator!"); break;
		case TOKEN_OP_STATIC_DEF:
			Log("!Static Def Operator!"); break;
		case TOKEN_OP_AND:
			Log("&&"); break;
		case TOKEN_OP_OR:
			Log("||"); break;
		case TOKEN_OP_NOT:
			Log("!"); break;
		case TOKEN_OP_BITWISE_AND:
			Log("&"); break;
		case TOKEN_OP_BITWISE_OR:
			Log("|"); break;
		case TOKEN_OP_BITWISE_NOT:
			Log("~"); break;
		case TOKEN_OP_MEMBER_ACCESS:
			Log("."); break;
	}
}

void WriteVariableDeclaration(ASTVariableDeclaration *var)
{
		Log("%.*s %.*s", var->type->name.size, var->type->name.data,
				var->name.size, var->name.data);
		if (var->value)
		{
			Log(" = ");
			WriteExpression(var->value);
		}
}

void WriteProcedureDeclaration(ASTProcedureDeclaration *proc)
{
	Log("%.*s %.*s(", proc->returnType->name.size, proc->returnType->name.data,
			proc->name.size, proc->name.data);

	for (int paramIdx = 0; paramIdx < proc->parameters.size; ++paramIdx)
	{
		if (paramIdx > 0)
			Log(", ");

		ASTVariableDeclaration *param = &proc->parameters[paramIdx];
		WriteVariableDeclaration(param);
	}
	Log(")\n");
	WriteStatement(proc->body);
}

void WriteStruct(ASTStruct *structNode)
{
	Log("struct %.*s\n{\n", structNode->name.size, structNode->name.data);
	for (int memberIdx = 0; memberIdx < structNode->members.size; ++memberIdx)
	{
		ASTVariableDeclaration *member = &structNode->members[memberIdx];
		WriteVariableDeclaration(member);
		Log(";\n");
	}
	Log("};\n");
}

void WriteBinaryOperation(ASTBinaryOperation *op)
{
	WriteExpression(op->leftHand);
	Log(" ");
	WriteOperator(op->op);
	Log(" ");
	WriteExpression(op->rightHand);
}

void WriteBlock(ASTBlock *block)
{
	Log("{\n");
	for (int i = 0; i < block->statements.size; ++i)
	{
		WriteStatement(&block->statements[i]);
	}
	Log("}\n");
}

void WriteExpression(ASTExpression *expression)
{
	switch (expression->type)
	{
	case ASTNODETYPE_VARIABLE:
	{
		ASTVariable *var = &expression->variable;
		Log("%.*s", var->name.size, var->name.data);
	} break;
	case ASTNODETYPE_CONSTANT:
	{
		ASTConstant *constant = &expression->constant;
		switch (constant->type)
		{
		case LITERALTYPE_INTEGER:
			Log("%d", constant->integer);
			break;
		case LITERALTYPE_STRING:
			Log("%.*s", constant->string.size, constant->string.data);
			break;
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		WriteBinaryOperation(&expression->binaryOperation);
	} break;
	}
}

void WriteStatement(ASTExpression *statement)
{
	switch (statement->type)
	{
	case ASTNODETYPE_BLOCK:
	{
		WriteBlock(&statement->block);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		WriteVariableDeclaration(&statement->variableDeclaration);
		Log(";\n");
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		WriteProcedureDeclaration(&statement->procedureDeclaration);
	} break;
	case ASTNODETYPE_STRUCT_DECLARATION:
	{
		WriteStruct(&statement->structNode);
	} break;
	default:
	{
		WriteExpression(statement);
		Log(";\n");
	} break;
	}
}

void WriteToC(ASTRoot *root)
{
	for (int i = 0; i < root->statements.size; ++i)
	{
		ASTExpression *statement = &root->statements[i];
		WriteStatement(statement);
	}
}
