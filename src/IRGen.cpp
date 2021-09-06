enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_REGISTER,
	IRVALUETYPE_VARIABLE,
	IRVALUETYPE_PARAMETER,
	IRVALUETYPE_IMMEDIATE_INTEGER,
	IRVALUETYPE_IMMEDIATE_FLOAT,
	IRVALUETYPE_IMMEDIATE_STRING,
	IRVALUETYPE_SIZEOF
};
struct IRValue
{
	IRValueType valueType;
	union
	{
		s64 registerIdx;
		Variable *variable;
		s64 immediate;
		f64 immediateFloat;
		String immediateString;
		u64 sizeOfTypeTableIdx;
	};
	bool dereference;
	s64 typeTableIdx;
};

struct IRJump
{
	String label;
};

struct IRConditionalJump
{
	String label;
	IRValue condition;
};

struct IRProcedureCall
{
	String label;
	bool isExternal;
	Array<IRValue> parameters;
	IRValue out;
};

struct IRAssignment
{
	IRValue src;
	IRValue dst;
};

struct IRMemberAccess
{
	IRValue in;
	IRValue out;

	StructMember *structMember;
};

struct IRArrayAccess
{
	IRValue array;
	IRValue index;
	IRValue out;
	s64 elementTypeTableIdx;
};

struct IRUnaryOperation
{
	IRValue in;
	IRValue out;
};

struct IRBinaryOperation
{
	IRValue left;
	IRValue right;
	IRValue out;
};

struct IRGetParameter
{
	IRValue out;
	s64 parameterIdx;
};

struct IRSetParameter
{
	IRValue in;
	s64 parameterIdx;
};

struct IRVariableDeclaration
{
	Variable *variable;
};

struct IRIntrinsicMemcpy
{
	IRValue src;
	IRValue dst;
	IRValue size;
};

enum IRInstructionType
{
	IRINSTRUCTIONTYPE_INVALID = -1,

	IRINSTRUCTIONTYPE_LABEL,
	IRINSTRUCTIONTYPE_JUMP,
	IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL,

	IRINSTRUCTIONTYPE_VARIABLE_DECLARATION,
	IRINSTRUCTIONTYPE_ASSIGNMENT,
	IRINSTRUCTIONTYPE_MEMBER_ACCESS,
	IRINSTRUCTIONTYPE_ARRAY_ACCESS,

	IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_NOT = IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_UNARY_END,

	IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_ADD = IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_SUBTRACT,
	IRINSTRUCTIONTYPE_MULTIPLY,
	IRINSTRUCTIONTYPE_DIVIDE,
	IRINSTRUCTIONTYPE_EQUALS,
	IRINSTRUCTIONTYPE_GREATER_THAN,
	IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_LESS_THAN,
	IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_BINARY_END,

	IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY
};
struct IRInstruction
{
	IRInstructionType type;
	union
	{
		String label;
		IRJump jump;
		IRConditionalJump conditionalJump;
		IRProcedureCall procedureCall;
		IRValue returnValue;
		IRGetParameter getParameter;
		IRSetParameter setParameter;
		IRVariableDeclaration variableDeclaration;
		IRAssignment assignment;
		IRMemberAccess memberAccess;
		IRArrayAccess arrayAccess;
		IRUnaryOperation unaryOperation;
		IRBinaryOperation binaryOperation;

		IRIntrinsicMemcpy memcpy;
	};
};

struct IRScope
{
	DynamicArray<ASTExpression *, malloc, realloc> deferredStatements;
};

struct IRProcedure
{
	String name;
	bool isExternal;
	bool isVarargs;
	Array<Variable *> parameters;
	BucketArray<IRInstruction, 256, malloc, realloc> instructions;
	s64 returnTypeTableIdx;
	u64 registerCount;
};

struct IRStaticVariable
{
	Variable *variable;
	s64 typeTableIdx;
	IRValue initialValue;
};

s64 NewVirtualRegister(Context *context)
{
	return context->currentRegisterId++;
}

String NewLabel(Context *context, String prefix)
{
	return TPrintF("%S%d", prefix, context->currentLabelId++);
}

void PushIRScope(Context *context)
{
	IRScope *newScope = DynamicArrayAdd(&context->irStack);
	DynamicArrayInit(&newScope->deferredStatements, 4);
}

void PopIRScope(Context *context)
{
	--context->irStack.size;
}

inline IRInstruction *AddInstruction(Context *context)
{
	return BucketArrayAdd(&context->irProcedures[context->currentProcedureIdx].instructions);
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression);

IRValue IRDereferenceValue(Context *context, IRValue in)
{
	IRValue result = in;

	TypeInfo *pointerTypeInfo = &context->typeTable[in.typeTableIdx];
	ASSERT(pointerTypeInfo->typeCategory == TYPECATEGORY_POINTER);

	if (!result.dereference)
	{
		result.dereference = true;
		result.typeTableIdx = pointerTypeInfo->pointerInfo.pointedTypeTableIdx;
	}
	else
	{
		IRValue reg = {};
		reg.valueType = IRVALUETYPE_REGISTER;
		reg.registerIdx = NewVirtualRegister(context);
		reg.typeTableIdx = in.typeTableIdx;

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.dst = reg;
		inst.assignment.src = result;
		*AddInstruction(context) = inst;

		result = reg;
		result.dereference = true;
		result.typeTableIdx = pointerTypeInfo->pointerInfo.pointedTypeTableIdx;
	}

	return result;
}

IRValue IRPointerToValue(Context *context, IRValue in)
{
	ASSERT(in.dereference);
	IRValue result = in;
	result.dereference = false;
	result.typeTableIdx = GetTypeInfoPointerOf(context, in.typeTableIdx);
	return result;
}

IRInstruction IRInstructionFromBinaryOperation(Context *context, ASTExpression *expression, IRValue
		*outResultValue)
{
	IRInstruction inst = {};

	ASTExpression *rightHand = expression->binaryOperation.rightHand;
	ASTExpression *leftHand  = expression->binaryOperation.leftHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
	{
		IRValue left = IRGenFromExpression(context, leftHand);

		if (left.dereference)
		{
			TypeInfo *structTypeInfo = &context->typeTable[left.typeTableIdx];
			if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
			{
				s64 structTypeInfoIdx = structTypeInfo->pointerInfo.pointedTypeTableIdx;
				structTypeInfo = &context->typeTable[structTypeInfoIdx];

				// Only if it was pointer to a pointer, dereference
				if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
					left = IRDereferenceValue(context, left);
			}
			else
			{
				left.dereference = false;
				left.typeTableIdx = GetTypeInfoPointerOf(context, left.typeTableIdx);
			}
		}

		ASSERT(rightHand->nodeType == ASTNODETYPE_STRUCT_MEMBER);
		String memberName = rightHand->structMember.name;

		s64 typeTableIdx = leftHand->typeTableIdx;
		ASSERT(typeTableIdx >= 0);
		TypeInfo *typeInfo = &context->typeTable[typeTableIdx];

		if (typeInfo->typeCategory == TYPECATEGORY_POINTER)
		{
			s64 pointedTypeIdx = typeInfo->pointerInfo.pointedTypeTableIdx;
			typeInfo = &context->typeTable[pointedTypeIdx];
		}

		ASSERT(typeInfo->typeCategory == TYPECATEGORY_STRUCT);

		StructMember *structMember = rightHand->structMember.structMember;
		inst.type = IRINSTRUCTIONTYPE_MEMBER_ACCESS;
		inst.memberAccess.in = left;
		inst.memberAccess.structMember = structMember;
		inst.memberAccess.out.typeTableIdx = GetTypeInfoPointerOf(context,
				structMember->typeTableIdx);

		outResultValue->typeTableIdx = structMember->typeTableIdx;
		outResultValue->dereference = true;
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
	{
		inst.type = IRINSTRUCTIONTYPE_ARRAY_ACCESS;
		inst.arrayAccess.array = IRGenFromExpression(context, leftHand);
		inst.arrayAccess.index = IRGenFromExpression(context, rightHand);

		if (inst.arrayAccess.array.dereference)
			inst.arrayAccess.array = IRPointerToValue(context, inst.arrayAccess.array);

		TypeInfo *arrayTypeInfo = &context->typeTable[leftHand->typeTableIdx];
		if (arrayTypeInfo->typeCategory == TYPECATEGORY_POINTER)
		{
			s64 pointedTypeIdx = arrayTypeInfo->pointerInfo.pointedTypeTableIdx;
			arrayTypeInfo = &context->typeTable[pointedTypeIdx];
		}

		ASSERT(arrayTypeInfo->typeCategory == TYPECATEGORY_ARRAY);
		s64 elementType = arrayTypeInfo->arrayInfo.elementTypeTableIdx;
		inst.arrayAccess.elementTypeTableIdx = elementType;

		s64 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementType);
		inst.arrayAccess.out.typeTableIdx = pointerToElementTypeIdx;

		outResultValue->typeTableIdx = elementType;
		outResultValue->dereference = true;
	}
	else
	{
		inst.binaryOperation.left  = IRGenFromExpression(context, leftHand);
		inst.binaryOperation.right = IRGenFromExpression(context, rightHand);

		switch (expression->binaryOperation.op)
		{
		case TOKEN_OP_PLUS:
		{
			inst.type = IRINSTRUCTIONTYPE_ADD;
		} break;
		case TOKEN_OP_MINUS:
		{
			inst.type = IRINSTRUCTIONTYPE_SUBTRACT;
		} break;
		case TOKEN_OP_MULTIPLY:
		{
			inst.type = IRINSTRUCTIONTYPE_MULTIPLY;
		} break;
		case TOKEN_OP_DIVIDE:
		{
			inst.type = IRINSTRUCTIONTYPE_DIVIDE;
		} break;
		case TOKEN_OP_EQUALS:
		{
			inst.type = IRINSTRUCTIONTYPE_EQUALS;
		} break;
		case TOKEN_OP_GREATER_THAN:
		{
			inst.type = IRINSTRUCTIONTYPE_GREATER_THAN;
		} break;
		case TOKEN_OP_GREATER_THAN_OR_EQUAL:
		{
			inst.type = IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS;
		} break;
		case TOKEN_OP_LESS_THAN:
		{
			inst.type = IRINSTRUCTIONTYPE_LESS_THAN;
		} break;
		case TOKEN_OP_LESS_THAN_OR_EQUAL:
		{
			inst.type = IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS;
		} break;
		default:
		{
			inst.type = IRINSTRUCTIONTYPE_INVALID;
		} break;
		}

		inst.binaryOperation.out.typeTableIdx = leftHand->typeTableIdx;
		outResultValue->typeTableIdx = leftHand->typeTableIdx;
	}

	return inst;
}

IRInstruction IRInstructionFromAssignment(Context *context, IRValue leftValue, ASTExpression *rightHand)
{
	s64 rightType = rightHand->typeTableIdx;
	ASSERT(rightType >= 0);
	TypeInfo *rightTypeInfo = &context->typeTable[rightType];
	if (rightTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
		rightTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
	{
		IRValue sizeValue = {};
		sizeValue.valueType = IRVALUETYPE_SIZEOF;
		sizeValue.sizeOfTypeTableIdx = rightType;

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY;
		inst.memcpy.src = IRGenFromExpression(context, rightHand);
		inst.memcpy.dst = leftValue;
		inst.memcpy.size = sizeValue;

		// @Cleanup: is this the best way?
		inst.memcpy.src.dereference = false;
		inst.memcpy.dst.dereference = false;

		return inst;
	}
	else
	{
		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.src = IRGenFromExpression(context, rightHand);
		inst.assignment.dst = leftValue;

		return inst;
	}
}

IRValue IRValueFromVariable(Context *context, Variable *variable)
{
	IRValue result = {};
	result.typeTableIdx = variable->typeTableIdx;

	if (variable->parameterIndex >= 0)
	{
		result.valueType = IRVALUETYPE_PARAMETER;
		result.variable = variable;
		result.dereference = false;

		TypeInfo *typeInfo = &context->typeTable[variable->typeTableIdx];
		if (typeInfo->typeCategory == TYPECATEGORY_STRUCT ||
			typeInfo->typeCategory == TYPECATEGORY_ARRAY)
			result.typeTableIdx = GetTypeInfoPointerOf(context, variable->typeTableIdx);
	}
	else
	{
		result.valueType = IRVALUETYPE_VARIABLE;
		result.variable = variable;
		result.dereference = true; // Dereference variables by default, since they are really pointers
	}
	return result;
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		u64 prevProcedureIdx = context->currentProcedureIdx;
		u64 prevProcedureStackBase = context->currentProcedureStackBase;

		context->currentProcedureIdx = context->irProcedures.size;
		IRProcedure *procedure = DynamicArrayAdd(&context->irProcedures);
		*procedure = {};
		procedure->name = expression->procedureDeclaration.name;
		procedure->isExternal = expression->procedureDeclaration.isExternal;
		procedure->isVarargs = expression->procedureDeclaration.isVarargs;

		procedure->returnTypeTableIdx = expression->procedureDeclaration.returnTypeTableIdx;

		u64 paramCount = expression->procedureDeclaration.parameters.size;
		ArrayInit(&procedure->parameters, paramCount, malloc);
		procedure->parameters.size = paramCount;

		BucketArrayInit(&procedure->instructions);

		context->currentProcedureStackBase = context->irStack.size;
		PushIRScope(context);

		for (int paramIdx = 0; paramIdx < paramCount; ++paramIdx)
		{
			ASTVariableDeclaration param = expression->procedureDeclaration.parameters[paramIdx];
			procedure->parameters[paramIdx] = param.variable;
		}

		if (expression->procedureDeclaration.body)
		{
			context->currentRegisterId = 0;
			IRGenFromExpression(context, expression->procedureDeclaration.body);
			procedure->registerCount = context->currentRegisterId;
		}

		PopIRScope(context);
		context->currentProcedureIdx = prevProcedureIdx;
		context->currentProcedureStackBase = prevProcedureStackBase;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		PushIRScope(context);
		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			IRGenFromExpression(context, &expression->block.statements[i]);
		}

		IRScope *currentScope = &context->irStack[context->irStack.size - 1];
		for (s64 j = currentScope->deferredStatements.size - 1; j >= 0; --j)
		{
			IRGenFromExpression(context, currentScope->deferredStatements[j]);
		}

		PopIRScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		bool isGlobalScope = context->currentProcedureIdx == U64_MAX;
		if (isGlobalScope && !varDecl.variable->isStatic)
			PrintError(context, expression->any.loc, "Global variables have to be static"_s);

		if (varDecl.variable->isStatic)
		{
			IRStaticVariable newStaticVar = {};
			newStaticVar.variable = varDecl.variable;
			newStaticVar.typeTableIdx = varDecl.variable->typeTableIdx;
			newStaticVar.initialValue.valueType = IRVALUETYPE_INVALID;

			// Initial value
			if (varDecl.value)
			{
				if (varDecl.value->nodeType != ASTNODETYPE_LITERAL)
				{
					// @Todo: Somehow execute constant expressions and bake them?
					PrintError(context, expression->any.loc, "Non literal initial values for global variables not yet supported"_s);
				}
				newStaticVar.initialValue = IRGenFromExpression(context, varDecl.value);
			}

			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;
		}
		else
		{
			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
			inst.variableDeclaration.variable = varDecl.variable;
			*AddInstruction(context) = inst;

			// Initial value
			if (varDecl.value)
			{
				IRValue leftValue = IRValueFromVariable(context, varDecl.variable);
				*AddInstruction(context) = IRInstructionFromAssignment(context, leftValue, varDecl.value);
			}
		}
	} break;
	case ASTNODETYPE_VARIABLE:
	{
		result = IRValueFromVariable(context, expression->variable.variable);
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		String label = expression->procedureCall.name; // @Improve: oh my god...

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
		inst.procedureCall.label = label;
		inst.procedureCall.isExternal = expression->procedureCall.isExternal;
		ArrayInit(&inst.procedureCall.parameters, expression->procedureCall.arguments.size, malloc);

		// Set up parameters
		for (int argIdx = 0; argIdx < expression->procedureCall.arguments.size; ++argIdx)
		{
			ASTExpression *arg = &expression->procedureCall.arguments[argIdx];
			TypeInfo *argTypeInfo = &context->typeTable[arg->typeTableIdx];
			if (argTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
					argTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
			{
				// Struct/array by copy:
				// Declare a variable in the stack, copy the struct/array to it, then pass the new
				// variable as parameter to the procedure.
				static u64 structByCopyDeclarationUniqueID = 0;

				IRValue param = IRGenFromExpression(context, arg);

				// Declare temporal value in the stack
				String tempVarName = TPrintF("_valueByCopy%llu", structByCopyDeclarationUniqueID++);
				Variable *tempVar = BucketArrayAdd(&context->variables);
				*tempVar = {};
				tempVar->name = tempVarName;
				tempVar->parameterIndex = -1;
				tempVar->typeTableIdx = arg->typeTableIdx;

				IRInstruction varDeclInst = {};
				varDeclInst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
				varDeclInst.variableDeclaration.variable = tempVar;
				*AddInstruction(context) = varDeclInst;

				// Memcpy original to temporal
				IRValue tempVarIRValue = {};
				tempVarIRValue.valueType = IRVALUETYPE_VARIABLE;
				tempVarIRValue.variable = tempVar;

				IRValue sizeValue = {};
				sizeValue.valueType = IRVALUETYPE_SIZEOF;
				sizeValue.sizeOfTypeTableIdx = arg->typeTableIdx;

				IRInstruction memCpyInst = {};
				memCpyInst.type = IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY;
				memCpyInst.memcpy.src = param;
				memCpyInst.memcpy.dst = tempVarIRValue;
				memCpyInst.memcpy.size = sizeValue;

				if (memCpyInst.memcpy.src.dereference)
					memCpyInst.memcpy.src = IRPointerToValue(context, memCpyInst.memcpy.src);
				if (memCpyInst.memcpy.dst.dereference)
					memCpyInst.memcpy.dst = IRPointerToValue(context, memCpyInst.memcpy.dst);
				*AddInstruction(context) = memCpyInst;

				// Turn into a register
				IRValue paramReg = {};
				paramReg.valueType = IRVALUETYPE_REGISTER;
				paramReg.registerIdx = NewVirtualRegister(context);
				paramReg.typeTableIdx = GetTypeInfoPointerOf(context, arg->typeTableIdx);

				IRInstruction paramIntermediateInst = {};
				paramIntermediateInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				paramIntermediateInst.assignment.src = tempVarIRValue;
				paramIntermediateInst.assignment.dst = paramReg;
				*AddInstruction(context) = paramIntermediateInst;

				// Add register as parameter
				*ArrayAdd(&inst.procedureCall.parameters) = paramReg;
			}
			else
			{
				IRValue param = IRGenFromExpression(context, arg);

				IRValue paramReg = {};
				paramReg.valueType = IRVALUETYPE_REGISTER;
				paramReg.registerIdx = NewVirtualRegister(context);
				paramReg.typeTableIdx = arg->typeTableIdx;

				IRInstruction paramIntermediateInst = {};
				paramIntermediateInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				paramIntermediateInst.assignment.src = param;
				paramIntermediateInst.assignment.dst = paramReg;
				*AddInstruction(context) = paramIntermediateInst;

				*ArrayAdd(&inst.procedureCall.parameters) = paramReg;
			}
		}

		inst.procedureCall.out.valueType = IRVALUETYPE_INVALID;
		if (expression->typeTableIdx != TYPETABLEIDX_VOID)
		{
			inst.procedureCall.out = {};
			inst.procedureCall.out.valueType = IRVALUETYPE_REGISTER;
			inst.procedureCall.out.registerIdx = NewVirtualRegister(context);
			inst.procedureCall.out.typeTableIdx = expression->typeTableIdx;
			result = inst.procedureCall.out;
		}

		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_UNARY_OPERATION:
	{
		if (expression->unaryOperation.op == TOKEN_OP_POINTER_TO)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);
			result = IRPointerToValue(context, result);
		}
		else if (expression->unaryOperation.op == TOKEN_OP_DEREFERENCE)
		{
			result = IRGenFromExpression(context, expression->unaryOperation.expression);
			result = IRDereferenceValue(context, result);
		}
		else
		{
			IRInstruction inst = {};
			inst.unaryOperation.in  = IRGenFromExpression(context, expression->unaryOperation.expression);

			inst.unaryOperation.out = {};
			inst.unaryOperation.out.valueType = IRVALUETYPE_REGISTER;
			inst.unaryOperation.out.registerIdx = NewVirtualRegister(context);

			switch (expression->unaryOperation.op)
			{
			case TOKEN_OP_NOT:
			{
				inst.type = IRINSTRUCTIONTYPE_NOT;
			} break;
			default:
			{
				inst.type = IRINSTRUCTIONTYPE_INVALID;
			} break;
			}

			*AddInstruction(context) = inst;
			result = inst.unaryOperation.out;
		}
	} break;
	case ASTNODETYPE_BINARY_OPERATION:
	{
		ASTExpression *rightHand = expression->binaryOperation.rightHand;
		ASTExpression *leftHand  = expression->binaryOperation.leftHand;

		if (expression->binaryOperation.op == TOKEN_OP_ASSIGNMENT)
		{
			IRValue leftValue = IRGenFromExpression(context, leftHand);
			*AddInstruction(context) = IRInstructionFromAssignment(context, leftValue, rightHand);
			result = leftValue;
		}
		else
		{
			IRInstruction inst = IRInstructionFromBinaryOperation(context, expression, &result);
			s64 newRegisterIdx = NewVirtualRegister(context);
			result.valueType = IRVALUETYPE_REGISTER;
			result.registerIdx = newRegisterIdx;

			if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
			{
				inst.memberAccess.out.valueType = IRVALUETYPE_REGISTER;
				inst.memberAccess.out.registerIdx = newRegisterIdx;
			}
			else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
			{
				inst.arrayAccess.out.valueType = IRVALUETYPE_REGISTER;
				inst.arrayAccess.out.registerIdx = newRegisterIdx;
			}
			else
			{
				inst.binaryOperation.out.valueType = IRVALUETYPE_REGISTER;
				inst.binaryOperation.out.registerIdx = newRegisterIdx;
			}

			*AddInstruction(context) = inst;
		}
	} break;
	case ASTNODETYPE_LITERAL:
	{
		switch (expression->literal.type)
		{
		case LITERALTYPE_FLOATING:
			result.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
			result.immediateFloat = expression->literal.floating;
			break;
		case LITERALTYPE_INTEGER:
			result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
			result.immediate = expression->literal.integer;
			break;
		case LITERALTYPE_STRING:
		{
			static u64 stringStaticVarUniqueID = 0;

			IRStaticVariable newStaticVar = {};
			newStaticVar.variable = BucketArrayAdd(&context->variables);
			newStaticVar.variable->name = TPrintF("staticString%d", stringStaticVarUniqueID++);
			newStaticVar.variable->typeTableIdx = TYPETABLEIDX_STRUCT_STRING;
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRING;
			newStaticVar.initialValue.immediateString = expression->literal.string;
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;

			result.valueType = IRVALUETYPE_VARIABLE;
			result.variable = newStaticVar.variable;
		} break;
		}
	} break;
	case ASTNODETYPE_IF:
	{
		IRInstruction *jump = AddInstruction(context);

		// Body!
		IRGenFromExpression(context, expression->ifNode.body);

		IRInstruction *jumpAfterElse = nullptr;
		if (expression->ifNode.elseBody)
			// If we have an else, add a jump instruction here.
			jumpAfterElse = AddInstruction(context);

		IRInstruction *skipLabelInst = AddInstruction(context);

		String skipLabel = NewLabel(context, "skipIf"_s);
		String afterElseLabel = NewLabel(context, "afterElse"_s);

		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = skipLabel;
		jump->conditionalJump.condition = IRGenFromExpression(context, expression->ifNode.condition);

		skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		skipLabelInst->label = skipLabel;

		if (expression->ifNode.elseBody)
		{
			jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
			jumpAfterElse->jump.label = afterElseLabel;

			IRGenFromExpression(context, expression->ifNode.elseBody);
		}
		IRInstruction *afterElseLabelInst = AddInstruction(context);
		afterElseLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		afterElseLabelInst->label = afterElseLabel;

	} break;
	case ASTNODETYPE_WHILE:
	{
		IRInstruction *loopLabelInst = AddInstruction(context);

		IRValue condition = IRGenFromExpression(context, expression->whileNode.condition);

		String breakLabel = NewLabel(context, "break"_s);

		String oldBreakLabel = context->currentBreakLabel;
		context->currentBreakLabel = breakLabel;

		IRInstruction *jump = AddInstruction(context);
		IRGenFromExpression(context, expression->whileNode.body);
		IRInstruction *loopJump = AddInstruction(context);
		IRInstruction *breakLabelInst = AddInstruction(context);

		context->currentBreakLabel = oldBreakLabel;

		String loopLabel = NewLabel(context, "loop"_s);
		loopLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		loopLabelInst->label = loopLabel;

		jump->type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
		jump->conditionalJump.label = breakLabel;
		jump->conditionalJump.condition = condition;

		loopJump->type = IRINSTRUCTIONTYPE_JUMP;
		loopJump->jump.label = loopLabel;

		breakLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		breakLabelInst->label = breakLabel;
	} break;
	case ASTNODETYPE_BREAK:
	{
		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_JUMP;
		inst.jump.label = context->currentBreakLabel;
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_RETURN:
	{
		IRValue returnValue = IRGenFromExpression(context, expression->returnNode.expression);

		for (s64 i = context->irStack.size - 1; i >= context->currentProcedureStackBase; --i)
		{
			IRScope *currentScope = &context->irStack[i];
			for (s64 j = currentScope->deferredStatements.size - 1; j >= 0; --j)
				IRGenFromExpression(context, currentScope->deferredStatements[j]);
		}

		// Clear deferred statements so they don't get added after the return
		IRScope *stackTop = &context->irStack[context->irStack.size - 1];
		stackTop->deferredStatements.size = 0;

		IRInstruction inst;
		inst.type = IRINSTRUCTIONTYPE_RETURN;
		inst.returnValue = returnValue;
		*AddInstruction(context) = inst;
	} break;
	case ASTNODETYPE_DEFER:
	{
		IRScope *stackTop = &context->irStack[context->irStack.size - 1];
		*DynamicArrayAdd(&stackTop->deferredStatements) = expression->deferNode.expression;
	} break;
	}
	return result;
}

void PrintIRValue(Context *context, IRValue value)
{
	if (value.dereference)
		Log("[");

	if (value.valueType == IRVALUETYPE_REGISTER)
		Log("$r%d", value.registerIdx);
	else if (value.valueType == IRVALUETYPE_VARIABLE)
		Log("%S", value.variable->name);
	else if (value.valueType == IRVALUETYPE_PARAMETER)
		Log("param%hhd", value.variable->parameterIndex);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_INTEGER)
		Log("0x%x", value.immediate);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_FLOAT)
		Log("%f", value.immediateFloat);
	else if (value.valueType == IRVALUETYPE_IMMEDIATE_STRING)
		Log("%S", value.immediateString);
	else if (value.valueType == IRVALUETYPE_SIZEOF)
		Log("sizeof(%lld)", value.sizeOfTypeTableIdx);
	else
		Log("???");

	if (value.dereference)
		Log("]");

	Log(" : %S", TypeInfoToString(context, value.typeTableIdx));
}

void PrintIRInstructionOperator(IRInstruction inst)
{
	switch (inst.type)
	{
	case IRINSTRUCTIONTYPE_ADD:
		Log("+");
		break;
	case IRINSTRUCTIONTYPE_SUBTRACT:
		Log("-");
		break;
	case IRINSTRUCTIONTYPE_MULTIPLY:
		Log("*");
		break;
	case IRINSTRUCTIONTYPE_DIVIDE:
		Log("/");
		break;
	case IRINSTRUCTIONTYPE_EQUALS:
		Log("==");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN:
		Log(">");
		break;
	case IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS:
		Log(">=");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN:
		Log("<");
		break;
	case IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS:
		Log("<=");
		break;
	case IRINSTRUCTIONTYPE_NOT:
		Log("!");
		break;
	default:
		Log("<?>");
	}
}

void IRGenMain(Context *context)
{
	DynamicArrayInit(&context->irProcedures, 64);
	DynamicArrayInit(&context->irStaticVariables, 64);
	DynamicArrayInit(&context->irStack, 64);
	context->currentProcedureIdx = U64_MAX;
	context->currentRegisterId = 1;
	context->currentLabelId = 1;

	PushIRScope(context);

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		IRGenFromExpression(context, statement);
	}
}
