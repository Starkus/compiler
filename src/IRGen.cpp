enum IRSpecialRegisters : s64
{
	IRSPECIALREGISTER_BEGIN = S64_MAX - 1,
	IRSPECIALREGISTER_RETURN = IRSPECIALREGISTER_BEGIN,
	IRSPECIALREGISTER_SHOULD_RETURN
};

enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_REGISTER,
	IRVALUETYPE_VARIABLE,
	IRVALUETYPE_PARAMETER,
	IRVALUETYPE_IMMEDIATE_INTEGER,
	IRVALUETYPE_IMMEDIATE_FLOAT,
	IRVALUETYPE_IMMEDIATE_STRING,
	IRVALUETYPE_SIZEOF,
	IRVALUETYPE_TYPEOF
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
		s64 sizeOfTypeTableIdx;
		s64 typeOfTypeTableIdx;
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
	Procedure *procedure;
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

	IRINSTRUCTIONTYPE_COMMENT,

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
	IRINSTRUCTIONTYPE_SUBTRACT_UNARY,
	IRINSTRUCTIONTYPE_UNARY_END,

	IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_ADD = IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_SUBTRACT,
	IRINSTRUCTIONTYPE_MULTIPLY,
	IRINSTRUCTIONTYPE_DIVIDE,
	IRINSTRUCTIONTYPE_MODULO,
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
		String comment;
		String label;
		IRJump jump;
		IRConditionalJump conditionalJump;
		IRProcedureCall procedureCall;
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
	String closeLabel;
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
	newScope->closeLabel = {};
	DynamicArrayInit(&newScope->deferredStatements, 4);
}

void PopIRScope(Context *context)
{
	--context->irStack.size;
}

inline IRInstruction *AddInstruction(Context *context)
{
	return BucketArrayAdd(&context->currentProcedure->instructions);
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

IRValue IRDoMemberAccess(Context *context, IRValue value, StructMember *structMember, IRValue
		outValue)
{
	if (value.dereference)
	{
		TypeInfo *structTypeInfo = &context->typeTable[value.typeTableIdx];
		if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
		{
			s64 structTypeInfoIdx = structTypeInfo->pointerInfo.pointedTypeTableIdx;
			structTypeInfo = &context->typeTable[structTypeInfoIdx];

			// Only if it was pointer to a pointer, dereference
			if (structTypeInfo->typeCategory == TYPECATEGORY_POINTER)
				value = IRDereferenceValue(context, value);
		}
		else
		{
			value.dereference = false;
			value.typeTableIdx = GetTypeInfoPointerOf(context, value.typeTableIdx);
		}
	}

	IRInstruction inst = {};
	inst.type = IRINSTRUCTIONTYPE_MEMBER_ACCESS;
	inst.memberAccess.in = value;
	inst.memberAccess.structMember = structMember;
	inst.memberAccess.out = outValue;
	inst.memberAccess.out.typeTableIdx = GetTypeInfoPointerOf(context,
			structMember->typeTableIdx);
	*AddInstruction(context) = inst;

	IRValue result = outValue;
	result.typeTableIdx = structMember->typeTableIdx;
	result.dereference = true;
	return result;
}

IRValue IRDoArrayAccess(Context *context, IRValue arrayValue, IRValue indexValue, IRValue outValue,
		s64 elementTypeIdx)
{
	IRInstruction inst = {};
	inst.type = IRINSTRUCTIONTYPE_ARRAY_ACCESS;
	inst.arrayAccess.array = arrayValue;
	inst.arrayAccess.index = indexValue;

	if (inst.arrayAccess.array.dereference)
		inst.arrayAccess.array = IRPointerToValue(context, inst.arrayAccess.array);

	TypeInfo *arrayTypeInfo = &context->typeTable[arrayValue.typeTableIdx];
	if (arrayTypeInfo->typeCategory == TYPECATEGORY_POINTER)
	{
		s64 pointedTypeIdx = arrayTypeInfo->pointerInfo.pointedTypeTableIdx;
		arrayTypeInfo = &context->typeTable[pointedTypeIdx];
	}

	s64 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementTypeIdx);

	if (arrayTypeInfo->arrayInfo.count == 0)
	{
		// Access the 'data' pointer
		s64 arrayTypeTableIdx = FindTypeInStackByName(context, {}, "Array"_s);
		TypeInfo *arrayStructTypeInfo = &context->typeTable[arrayTypeTableIdx];
		StructMember *structMember = &arrayStructTypeInfo->structInfo.members[1];

		IRInstruction memberAccessInst;
		memberAccessInst.type = IRINSTRUCTIONTYPE_MEMBER_ACCESS;
		memberAccessInst.memberAccess.in = inst.arrayAccess.array;
		memberAccessInst.memberAccess.structMember = structMember;
		memberAccessInst.memberAccess.out.valueType = IRVALUETYPE_REGISTER;
		memberAccessInst.memberAccess.out.registerIdx = NewVirtualRegister(context);
		memberAccessInst.memberAccess.out.typeTableIdx = pointerToElementTypeIdx;
		memberAccessInst.memberAccess.out.dereference = false;
		*AddInstruction(context) = memberAccessInst;

		inst.arrayAccess.array = memberAccessInst.memberAccess.out;
		inst.arrayAccess.array.dereference = true;
	}

	inst.arrayAccess.elementTypeTableIdx = elementTypeIdx;
	inst.arrayAccess.out = outValue;
	inst.arrayAccess.out.typeTableIdx = pointerToElementTypeIdx;
	*AddInstruction(context) = inst;

	IRValue result = outValue;
	result.typeTableIdx = elementTypeIdx;
	result.dereference = true;
	return result;
}

IRValue IRInstructionFromBinaryOperation(Context *context, ASTExpression *expression,
		IRValue outValue)
{
	IRValue result = {};

	ASTExpression *rightHand = expression->binaryOperation.rightHand;
	ASTExpression *leftHand  = expression->binaryOperation.leftHand;

	if (expression->binaryOperation.op == TOKEN_OP_MEMBER_ACCESS)
	{
		IRValue value = IRGenFromExpression(context, leftHand);

		ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
		ASSERT(rightHand->identifier.type == NAMETYPE_STRUCT_MEMBER);
		StructMember *structMember = rightHand->identifier.structMemberInfo.offsets[0];

		result = IRDoMemberAccess(context, value, structMember, outValue);
	}
	else if (expression->binaryOperation.op == TOKEN_OP_ARRAY_ACCESS)
	{
		IRValue arrayValue = IRGenFromExpression(context, leftHand);
		IRValue indexValue = IRGenFromExpression(context, rightHand);
		result = IRDoArrayAccess(context, arrayValue, indexValue, result, expression->typeTableIdx);
	}
	else
	{
		IRInstruction inst = {};
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
		case TOKEN_OP_MODULO:
		{
			inst.type = IRINSTRUCTIONTYPE_MODULO;
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

		inst.binaryOperation.out = outValue;
		inst.binaryOperation.out.typeTableIdx = leftHand->typeTableIdx;
		*AddInstruction(context) = inst;

		result = outValue;
		result.typeTableIdx = leftHand->typeTableIdx;
	}

	return result;
}

void IRInstructionFromAssignment(Context *context, IRValue leftValue, IRValue rightValue)
{
	s64 rightType = rightValue.typeTableIdx;
	ASSERT(rightType >= 0);

	s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);
	s64 anyPointerTableIdx = GetTypeInfoPointerOf(context, anyTableIdx);
	if ((leftValue.typeTableIdx == anyTableIdx || leftValue.typeTableIdx == anyPointerTableIdx) &&
		(rightType != anyTableIdx && rightType != anyPointerTableIdx))
	{
		TypeInfo *anyTypeInfo = &context->typeTable[anyTableIdx];

		// Access typeInfo member
		IRInstruction typeMemAccessInst;
		typeMemAccessInst.type = IRINSTRUCTIONTYPE_MEMBER_ACCESS;
		typeMemAccessInst.memberAccess.in = leftValue.dereference ?
			IRPointerToValue(context, leftValue) : leftValue;
		typeMemAccessInst.memberAccess.structMember = &anyTypeInfo->structInfo.members[0];
		typeMemAccessInst.memberAccess.out.valueType = IRVALUETYPE_REGISTER;
		typeMemAccessInst.memberAccess.out.dereference = false;
		typeMemAccessInst.memberAccess.out.registerIdx = NewVirtualRegister(context);
		typeMemAccessInst.memberAccess.out.typeTableIdx = GetTypeInfoPointerOf(context,
				anyTypeInfo->structInfo.members[0].typeTableIdx);
		*AddInstruction(context) = typeMemAccessInst;

		// Write pointer to typeInfo to it
		IRInstruction typeAssignInst = {};
		typeAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		typeAssignInst.assignment.src.valueType = IRVALUETYPE_TYPEOF;
		typeAssignInst.assignment.src.typeOfTypeTableIdx = rightValue.typeTableIdx;
		typeAssignInst.assignment.src.typeTableIdx = GetTypeInfoPointerOf(context,
				FindTypeInStackByName(context, {}, "TypeInfo"_s));
		typeAssignInst.assignment.dst = typeMemAccessInst.memberAccess.out;
		typeAssignInst.assignment.dst.dereference = true;
		*AddInstruction(context) = typeAssignInst;

		// Access data member
		IRInstruction dataMemAccessInst;
		dataMemAccessInst.type = IRINSTRUCTIONTYPE_MEMBER_ACCESS;
		dataMemAccessInst.memberAccess.in = leftValue.dereference ?
			IRPointerToValue(context, leftValue) : leftValue;
		dataMemAccessInst.memberAccess.structMember = &anyTypeInfo->structInfo.members[1];
		dataMemAccessInst.memberAccess.out.valueType = IRVALUETYPE_REGISTER;
		dataMemAccessInst.memberAccess.out.dereference = false;
		dataMemAccessInst.memberAccess.out.registerIdx = NewVirtualRegister(context);
		dataMemAccessInst.memberAccess.out.typeTableIdx = GetTypeInfoPointerOf(context,
				rightValue.typeTableIdx);
		*AddInstruction(context) = dataMemAccessInst;

		IRValue data = rightValue;

		// If data isn't a pointer to something, copy to a variable
		if (!data.dereference)
		{
			static u64 tempVarForAnyUniqueID = 0;
			String tempVarName = TPrintF("_tempVarForAny%llu", tempVarForAnyUniqueID++);
			Variable *tempVar = BucketArrayAdd(&context->variables);
			*tempVar = {};
			tempVar->name = tempVarName;
			tempVar->parameterIndex = -1;
			tempVar->typeTableIdx = data.typeTableIdx;

			IRInstruction varDeclInst = {};
			varDeclInst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
			varDeclInst.variableDeclaration.variable = tempVar;
			*AddInstruction(context) = varDeclInst;

			IRInstruction dataCopyInst = {};
			dataCopyInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			dataCopyInst.assignment.src = data;
			dataCopyInst.assignment.dst.valueType = IRVALUETYPE_VARIABLE;
			dataCopyInst.assignment.dst.variable = tempVar;
			dataCopyInst.assignment.dst.dereference = true;
			dataCopyInst.assignment.dst.typeTableIdx = data.typeTableIdx;
			*AddInstruction(context) = dataCopyInst;

			data = {};
			data.valueType = IRVALUETYPE_VARIABLE;
			data.variable = tempVar;
			data.dereference = true;
		}

		// Write pointer to data to it
		IRInstruction dataAssignInst = {};
		dataAssignInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		dataAssignInst.assignment.src = IRPointerToValue(context, data);
		dataAssignInst.assignment.dst = dataMemAccessInst.memberAccess.out;
		dataAssignInst.assignment.dst.dereference = true;
		*AddInstruction(context) = dataAssignInst;

		return;
	}

	TypeInfo *rightTypeInfo = &context->typeTable[rightType];
	if (rightTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
		rightTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
	{
		IRValue sizeValue = {};
		sizeValue.valueType = IRVALUETYPE_SIZEOF;
		sizeValue.sizeOfTypeTableIdx = rightType;

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY;
		inst.memcpy.src = rightValue;
		inst.memcpy.dst = leftValue;
		inst.memcpy.size = sizeValue;

		if (inst.memcpy.src.dereference)
			inst.memcpy.src = IRPointerToValue(context, inst.memcpy.src);
		if (inst.memcpy.dst.dereference)
			inst.memcpy.dst = IRPointerToValue(context, inst.memcpy.dst);

		*AddInstruction(context) = inst;
	}
	else
	{
		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
		inst.assignment.src = rightValue;
		inst.assignment.dst = leftValue;

		*AddInstruction(context) = inst;
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

bool IRShouldReturnByCopy(Context *context, s64 returnTypeTableIdx)
{
	TypeInfo *returnTypeInfo = &context->typeTable[returnTypeTableIdx];
	// @Speed
	return  returnTypeInfo->typeCategory == TYPECATEGORY_ARRAY ||
		   (returnTypeInfo->typeCategory == TYPECATEGORY_STRUCT &&
			returnTypeInfo->structInfo.size != 1 &&
			returnTypeInfo->structInfo.size != 2 &&
			returnTypeInfo->structInfo.size != 4 &&
			returnTypeInfo->structInfo.size != 8);
}

IRValue IRValueImmediate(s64 immediate, s64 typeTableIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
	result.immediate = immediate;
	result.typeTableIdx = typeTableIdx;
	result.dereference = false;
	return result;
}

IRValue IRValueRegister(s64 registerIdx, s64 typeTableIdx)
{
	IRValue result;
	result.valueType = IRVALUETYPE_REGISTER;
	result.registerIdx = registerIdx;
	result.typeTableIdx = typeTableIdx;
	result.dereference = false;
	return result;
}

void IRAddComment(Context  *context, String comment)
{
	IRInstruction result;
	result.type = IRINSTRUCTIONTYPE_COMMENT;
	result.comment = comment;
	*AddInstruction(context) = result;
}

IRValue IRGenFromExpression(Context *context, ASTExpression *expression)
{
	IRValue result = {};
	result.valueType = IRVALUETYPE_INVALID;

	switch (expression->nodeType)
	{
	case ASTNODETYPE_STATIC_DEFINITION:
	{
		IRGenFromExpression(context, expression->staticDefinition.expression);
	} break;
	case ASTNODETYPE_PROCEDURE_DECLARATION:
	{
		Procedure *prevProcedure = context->currentProcedure;
		u64 prevProcedureStackBase = context->currentProcedureStackBase;
		String prevProcedureReturnLabel = context->currentProcedureReturnLabel;

		Procedure *procedure = expression->procedureDeclaration.procedure;
		context->currentProcedure = procedure;

		BucketArrayInit(&procedure->instructions);

		String returnLabel = NewLabel(context, "return"_s);
		context->currentProcedureReturnLabel = returnLabel;
		context->currentProcedureStackBase = context->irStack.size;
		PushIRScope(context);

		if (procedure->astBody)
		{
			context->currentRegisterId = 0;
			IRGenFromExpression(context, procedure->astBody);
			procedure->registerCount = context->currentRegisterId;

			IRInstruction *returnLabelInst = AddInstruction(context);
			returnLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
			returnLabelInst->label = returnLabel;

			// Return
			IRInstruction returnInst;
			returnInst.type = IRINSTRUCTIONTYPE_RETURN;
			*AddInstruction(context) = returnInst;
		}

		PopIRScope(context);

		context->currentProcedure = prevProcedure;
		context->currentProcedureStackBase = prevProcedureStackBase;
		context->currentProcedureReturnLabel = prevProcedureReturnLabel;
	} break;
	case ASTNODETYPE_BLOCK:
	{
		PushIRScope(context);
		IRScope *currentScope = &context->irStack[context->irStack.size - 1];
		currentScope->closeLabel = NewLabel(context, "closeScope"_s);

		for (int i = 0; i < expression->block.statements.size; ++i)
		{
			IRGenFromExpression(context, &expression->block.statements[i]);
		}

		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = context->irStack.size - 1;
				stackIdx >= context->currentProcedureStackBase; --stackIdx)
		{
			if (context->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			// Set should-return register to 0
			IRValue shouldReturnRegister = IRValueRegister(IRSPECIALREGISTER_SHOULD_RETURN,
					TYPETABLEIDX_U8);
			IRValue zero = IRValueImmediate(0, TYPETABLEIDX_U8);
			IRInstructionFromAssignment(context, shouldReturnRegister, zero);

			// Add close label
			IRInstruction *closeScopeLabelInst = AddInstruction(context);
			closeScopeLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
			closeScopeLabelInst->label = currentScope->closeLabel;

			// Run deferred statements
			for (s64 j = currentScope->deferredStatements.size - 1; j >= 0; --j)
			{
				IRGenFromExpression(context, currentScope->deferredStatements[j]);
			}

			// If should-return register is set, return
			if ((s64)(context->irStack.size - 2) != context->currentProcedureStackBase)
			{
				String skipLabel = NewLabel(context, "skipReturn"_s);

				IRInstruction jumpIfShouldntReturnInst;
				jumpIfShouldntReturnInst.type = IRINSTRUCTIONTYPE_JUMP_IF_ZERO;
				jumpIfShouldntReturnInst.conditionalJump.label = skipLabel;
				jumpIfShouldntReturnInst.conditionalJump.condition = shouldReturnRegister;
				*AddInstruction(context) = jumpIfShouldntReturnInst;

				// Jump to closing of next scope
				IRInstruction jumpInst;
				jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
				jumpInst.jump.label = context->irStack[context->irStack.size - 2].closeLabel;
				*AddInstruction(context) = jumpInst;

				IRInstruction *skipLabelInst = AddInstruction(context);
				skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
				skipLabelInst->label = skipLabel;
			}
		}

		PopIRScope(context);
	} break;
	case ASTNODETYPE_VARIABLE_DECLARATION:
	{
		ASTVariableDeclaration varDecl = expression->variableDeclaration;

		bool isGlobalScope = context->currentProcedure == nullptr;
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
				else if (varDecl.value->literal.type == LITERALTYPE_STRING)
				{
					newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRING;
					newStaticVar.initialValue.immediateString = varDecl.value->literal.string;
				}
				else
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
				IRValue rightValue = IRGenFromExpression(context, varDecl.value);
				IRInstructionFromAssignment(context, leftValue, rightValue);
			}
		}
	} break;
	case ASTNODETYPE_IDENTIFIER:
	{
		switch (expression->identifier.type)
		{
		case NAMETYPE_STATIC_DEFINITION:
		{
			switch (expression->identifier.staticDefinition->definitionType)
			{
			case STATICDEFINITIONTYPE_CONSTANT_INTEGER:
			{
				result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
				result.immediate = expression->identifier.staticDefinition->constantInteger.value;
				result.typeTableIdx = TYPETABLEIDX_S64;
			} break;
			case STATICDEFINITIONTYPE_CONSTANT_FLOATING:
			{
				result.valueType = IRVALUETYPE_IMMEDIATE_FLOAT;
				result.immediateFloat = expression->identifier.staticDefinition->constantFloating.value;
				result.typeTableIdx = TYPETABLEIDX_F64;
			} break;
			}
		} break;
		case NAMETYPE_STRUCT_MEMBER:
		{
			IRValue left = IRValueFromVariable(context, expression->identifier.structMemberInfo.base);

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

			for (int i = 0; i < expression->identifier.structMemberInfo.offsets.size; ++i)
			{
				StructMember *structMember = expression->identifier.structMemberInfo.offsets[i];

				IRInstruction inst;
				inst.type = IRINSTRUCTIONTYPE_MEMBER_ACCESS;
				inst.memberAccess.in = left;
				inst.memberAccess.structMember = structMember;
				inst.memberAccess.out = IRValueRegister(NewVirtualRegister(context),
						GetTypeInfoPointerOf(context, structMember->typeTableIdx));
				*AddInstruction(context) = inst;

				// Set left for next one
				left = inst.memberAccess.out;
				left.typeTableIdx = GetTypeInfoPointerOf(context, structMember->typeTableIdx);

				result = inst.memberAccess.out;
				result.typeTableIdx = structMember->typeTableIdx;
			}
			result.dereference = true;
		} break;
		case NAMETYPE_VARIABLE:
		{
			result = IRValueFromVariable(context, expression->identifier.variable);
		} break;
		default:
		{
			CRASH;
		}
		}
	} break;
	case ASTNODETYPE_PROCEDURE_CALL:
	{
		ASTProcedureCall *astProcCall = &expression->procedureCall;
		Procedure *procedure = astProcCall->procedure;

		IRInstruction procCallInst = {};
		procCallInst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
		procCallInst.procedureCall.procedure = procedure;

		bool isReturnByCopy = IRShouldReturnByCopy(context, expression->typeTableIdx);

		// Support both varargs and default parameters here
		s32 totalParamCount = (s32)procedure->parameters.size;
		s32 callParamCount = (s32)astProcCall->arguments.size;
		s32 paramCount = Max(totalParamCount, callParamCount) + isReturnByCopy;
		ArrayInit(&procCallInst.procedureCall.parameters, paramCount, malloc);

		// Return value
		procCallInst.procedureCall.out.valueType = IRVALUETYPE_INVALID;
		if (expression->typeTableIdx != TYPETABLEIDX_VOID)
		{
			if (isReturnByCopy)
			{
				static u64 returnByCopyDeclarationUniqueID = 0;

				// Allocate stack for return value
				String tempVarName = TPrintF("_returnByCopy%llu", returnByCopyDeclarationUniqueID++);
				Variable *tempVar = BucketArrayAdd(&context->variables);
				*tempVar = {};
				tempVar->name = tempVarName;
				tempVar->parameterIndex = -1;
				tempVar->typeTableIdx = expression->typeTableIdx;

				IRInstruction varDeclInst = {};
				varDeclInst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
				varDeclInst.variableDeclaration.variable = tempVar;
				*AddInstruction(context) = varDeclInst;

				// Turn into a register
				IRValue reg = IRValueRegister(NewVirtualRegister(context),
						GetTypeInfoPointerOf(context, expression->typeTableIdx));

				IRValue tempVarIRValue = {};
				tempVarIRValue.valueType = IRVALUETYPE_VARIABLE;
				tempVarIRValue.variable = tempVar;

				IRInstruction paramIntermediateInst = {};
				paramIntermediateInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				paramIntermediateInst.assignment.src = tempVarIRValue;
				paramIntermediateInst.assignment.dst = reg;

				// Add register as parameter
				*ArrayAdd(&procCallInst.procedureCall.parameters) = reg;

				result = reg;
			}
			else
			{
				procCallInst.procedureCall.out = IRValueRegister(NewVirtualRegister(context),
						expression->typeTableIdx);
				result = procCallInst.procedureCall.out;
			}
		}

		// Set up parameters
		s64 normalArgumentsCount = Min(astProcCall->arguments.size,
				procedure->parameters.size - procedure->isVarargs);
		for (int argIdx = 0; argIdx < normalArgumentsCount; ++argIdx)
		{
			ASTExpression *arg = &astProcCall->arguments[argIdx];
			s64 argTypeTableIdx = arg->typeTableIdx;

			ProcedureParameter originalParam = procedure->parameters[argIdx];
			if (originalParam.variable->typeTableIdx != argTypeTableIdx)
			{
				argTypeTableIdx = originalParam.variable->typeTableIdx;
			}

			TypeInfo *argTypeInfo = &context->typeTable[argTypeTableIdx];
			if (argTypeInfo->typeCategory == TYPECATEGORY_STRUCT ||
					argTypeInfo->typeCategory == TYPECATEGORY_ARRAY)
			{
				// Struct/array by copy:
				// Declare a variable in the stack, copy the struct/array to it, then pass the new
				// variable as parameter to the procedure.
				static u64 structByCopyDeclarationUniqueID = 0;

				IRAddComment(context, "Copy argument to the stack to pass as pointer"_s);
				// Declare temporal value in the stack
				String tempVarName = TPrintF("_valueByCopy%llu", structByCopyDeclarationUniqueID++);
				Variable *tempVar = BucketArrayAdd(&context->variables);
				*tempVar = {};
				tempVar->name = tempVarName;
				tempVar->parameterIndex = -1;
				tempVar->typeTableIdx = argTypeTableIdx;

				// Allocate stack space for temp struct
				IRInstruction varDeclInst = {};
				varDeclInst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
				varDeclInst.variableDeclaration.variable = tempVar;
				*AddInstruction(context) = varDeclInst;

				IRValue tempVarIRValue = {};
				tempVarIRValue.valueType = IRVALUETYPE_VARIABLE;
				tempVarIRValue.variable = tempVar;

				// Turn into a register, mainly to pass it to the procedure
				IRValue paramReg = IRValueRegister(NewVirtualRegister(context),
						GetTypeInfoPointerOf(context, argTypeTableIdx));
				IRInstructionFromAssignment(context, paramReg, tempVarIRValue);

				paramReg.dereference = true;
				IRValue rightValue = IRGenFromExpression(context, arg);
				IRInstructionFromAssignment(context, paramReg, rightValue);

				// Add register as parameter
				*ArrayAdd(&procCallInst.procedureCall.parameters) = paramReg;
			}
			else
			{
				IRValue param = IRGenFromExpression(context, arg);

				// Make it a register if it's not already one
				if (param.valueType != IRVALUETYPE_REGISTER)
				{
					IRValue paramReg = IRValueRegister(NewVirtualRegister(context),
							argTypeTableIdx);
					IRInstructionFromAssignment(context, paramReg, param);
					param = paramReg;
				}

				*ArrayAdd(&procCallInst.procedureCall.parameters) = param;
			}
		}

		// Default parameters
		for (u64 argIdx = astProcCall->arguments.size; argIdx < astProcCall->procedure->parameters.size;
				++argIdx)
		{
			ASTExpression *arg = astProcCall->procedure->parameters[argIdx].defaultValue;
			IRValue param = IRGenFromExpression(context, arg);

			// Make it a register if it's not already one
			if (param.valueType != IRVALUETYPE_REGISTER)
			{
				IRValue paramReg = IRValueRegister(NewVirtualRegister(context), arg->typeTableIdx);
				IRInstructionFromAssignment(context, paramReg, param);
				param = paramReg;
			}

			*ArrayAdd(&procCallInst.procedureCall.parameters) = param;
		}

		// Varargs
		s64 varargsCount = astProcCall->arguments.size - (procedure->parameters.size -
				procedure->isVarargs);
		if (varargsCount > 0)
		{
			static u64 varargsUniqueID = 0;

			IRAddComment(context, "Build varargs array"_s);

			s64 anyTableIdx = FindTypeInStackByName(context, {}, "Any"_s);

			// Declare temporal value in the stack
			String tempVarName = TPrintF("_varargsBuffer%llu", varargsUniqueID++);
			Variable *bufferVar = BucketArrayAdd(&context->variables);
			*bufferVar = {};
			bufferVar->name = tempVarName;
			bufferVar->parameterIndex = -1;
			bufferVar->typeTableIdx = GetTypeInfoArrayOf(context, anyTableIdx, varargsCount);

			// Allocate stack space for buffer
			IRInstruction bufferDeclInst = {};
			bufferDeclInst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
			bufferDeclInst.variableDeclaration.variable = bufferVar;
			*AddInstruction(context) = bufferDeclInst;

			IRValue bufferIRValue = {};
			bufferIRValue.valueType = IRVALUETYPE_VARIABLE;
			bufferIRValue.variable = bufferVar;
			bufferIRValue.typeTableIdx = bufferVar->typeTableIdx;
			bufferIRValue.dereference = true;

			// Fill the buffer
			int nonVarargs = (int)procedure->parameters.size - 1;
			for (int argIdx = 0; argIdx < varargsCount; ++argIdx)
			{
				ASTExpression *arg = &astProcCall->arguments[argIdx + nonVarargs];

				IRValue bufferIndexValue = {};
				bufferIndexValue.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
				bufferIndexValue.immediate = argIdx;

				IRValue bufferSlotValue = {};
				bufferSlotValue.valueType = IRVALUETYPE_REGISTER;
				bufferSlotValue.registerIdx = NewVirtualRegister(context);
				bufferSlotValue.typeTableIdx = anyTableIdx;
				bufferSlotValue = IRDoArrayAccess(context, bufferIRValue, bufferIndexValue,
						bufferSlotValue, anyTableIdx);

				IRValue rightValue = IRGenFromExpression(context, arg);
				IRInstructionFromAssignment(context, bufferSlotValue, rightValue);
			}

			// By now we should have the buffer with all the varargs as Any structs.
			// Now we put it into a dynamic array struct.
			s64 dynamicArrayTableIdx = FindTypeInStackByName(context, {}, "Array"_s);

			// Declare temporal value in the stack
			tempVarName = TPrintF("_varargsArray%llu", varargsUniqueID++);
			Variable *arrayVar = BucketArrayAdd(&context->variables);
			*arrayVar = {};
			arrayVar->name = tempVarName;
			arrayVar->parameterIndex = -1;
			arrayVar->typeTableIdx = dynamicArrayTableIdx;

			// Allocate stack space for array
			IRInstruction arrayDeclInst = {};
			arrayDeclInst.type = IRINSTRUCTIONTYPE_VARIABLE_DECLARATION;
			arrayDeclInst.variableDeclaration.variable = arrayVar;
			*AddInstruction(context) = arrayDeclInst;

			IRValue arrayIRValue = {};
			arrayIRValue.valueType = IRVALUETYPE_VARIABLE;
			arrayIRValue.variable = arrayVar;
			arrayIRValue.typeTableIdx = arrayVar->typeTableIdx;
			arrayIRValue.dereference = true;

			TypeInfo *dynamicArrayTypeInfo = &context->typeTable[dynamicArrayTableIdx];
			// Size
			StructMember *sizeStructMember = &dynamicArrayTypeInfo->structInfo.members[0];
			IRValue sizeMember = {};
			sizeMember.valueType = IRVALUETYPE_REGISTER;
			sizeMember.registerIdx = NewVirtualRegister(context);
			sizeMember = IRDoMemberAccess(context, arrayIRValue, sizeStructMember, sizeMember);

			IRValue sizeValue = {};
			sizeValue.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
			sizeValue.immediate = varargsCount;
			sizeValue.typeTableIdx = TYPETABLEIDX_U64;

			IRInstructionFromAssignment(context, sizeMember, sizeValue);

			// Data
			StructMember *dataStructMember = &dynamicArrayTypeInfo->structInfo.members[1];
			IRValue dataMember = {};
			dataMember.valueType = IRVALUETYPE_REGISTER;
			dataMember.registerIdx = NewVirtualRegister(context);
			dataMember = IRDoMemberAccess(context, arrayIRValue, dataStructMember, dataMember);

			IRValue dataValue = IRPointerToValue(context, bufferIRValue);

			IRInstructionFromAssignment(context, dataMember, dataValue);

			// Pass array as parameter!
			IRValue paramReg = {};
			paramReg.valueType = IRVALUETYPE_REGISTER;
			paramReg.registerIdx = NewVirtualRegister(context);
			paramReg.typeTableIdx = GetTypeInfoPointerOf(context, dynamicArrayTableIdx);
			IRInstructionFromAssignment(context, paramReg, IRPointerToValue(context, arrayIRValue));
			*ArrayAdd(&procCallInst.procedureCall.parameters) = paramReg;
		}

		*AddInstruction(context) = procCallInst;
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
			inst.unaryOperation.out.typeTableIdx = expression->typeTableIdx;

			switch (expression->unaryOperation.op)
			{
			case TOKEN_OP_NOT:
			{
				inst.type = IRINSTRUCTIONTYPE_NOT;
			} break;
			case TOKEN_OP_MINUS:
			{
				inst.type = IRINSTRUCTIONTYPE_SUBTRACT_UNARY;
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
			IRValue rightValue = IRGenFromExpression(context, rightHand);
			IRInstructionFromAssignment(context, leftValue, rightValue);
			result = leftValue;
		}
		else
		{
			s64 newRegisterIdx = NewVirtualRegister(context);
			result.valueType = IRVALUETYPE_REGISTER;
			result.registerIdx = newRegisterIdx;
			result = IRInstructionFromBinaryOperation(context, expression, result);
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
		case LITERALTYPE_CHARACTER:
			result.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
			result.immediate = expression->literal.character;
			break;
		case LITERALTYPE_STRING:
		{
			static u64 stringStaticVarUniqueID = 0;

			IRStaticVariable newStaticVar = {};
			newStaticVar.variable = BucketArrayAdd(&context->variables);
			newStaticVar.variable->name = TPrintF("staticString%d", stringStaticVarUniqueID++);
			newStaticVar.variable->typeTableIdx = FindTypeInStackByName(context, {}, "String"_s);
			newStaticVar.initialValue.valueType = IRVALUETYPE_IMMEDIATE_STRING;
			newStaticVar.initialValue.immediateString = expression->literal.string;
			*DynamicArrayAdd(&context->irStaticVariables) = newStaticVar;

			result.valueType = IRVALUETYPE_VARIABLE;
			result.variable = newStaticVar.variable;
		} break;
		}
		result.typeTableIdx = expression->typeTableIdx;
	} break;
	case ASTNODETYPE_IF:
	{
		IRValue conditionResult = IRGenFromExpression(context, expression->ifNode.condition);
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
		jump->conditionalJump.condition = conditionResult;

		skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
		skipLabelInst->label = skipLabel;

		if (expression->ifNode.elseBody)
		{
			jumpAfterElse->type = IRINSTRUCTIONTYPE_JUMP;
			jumpAfterElse->jump.label = afterElseLabel;

			IRGenFromExpression(context, expression->ifNode.elseBody);

			IRInstruction *afterElseLabelInst = AddInstruction(context);
			afterElseLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
			afterElseLabelInst->label = afterElseLabel;
		}

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

		bool isThereCleanUpToDo = false;
		for (s64 stackIdx = context->irStack.size - 1;
				stackIdx >= context->currentProcedureStackBase; --stackIdx)
		{
			if (context->irStack[stackIdx].deferredStatements.size)
			{
				isThereCleanUpToDo = true;
				break;
			}
		}

		if (isThereCleanUpToDo)
		{
			// Set should return to one
			IRValue one = IRValueImmediate(1, TYPETABLEIDX_U8);
			IRValue shouldReturnRegister = IRValueRegister(IRSPECIALREGISTER_SHOULD_RETURN, TYPETABLEIDX_U8);
			IRInstructionFromAssignment(context, shouldReturnRegister, one);
		}
		else
		{
		}

		s64 returnTypeTableIdx = expression->returnNode.expression->typeTableIdx;
		if (IRShouldReturnByCopy(context, returnTypeTableIdx))
		{
			s64 pointerToType = GetTypeInfoPointerOf(context, returnTypeTableIdx);

			IRValue src = returnValue;
			src.typeTableIdx = pointerToType;
			src.dereference = false;

			IRValue dst;
			dst.valueType = IRVALUETYPE_REGISTER;
			dst.registerIdx = IRSPECIALREGISTER_RETURN;
			dst.typeTableIdx = pointerToType;
			dst.dereference = false;

			IRValue sizeValue = {};
			sizeValue.valueType = IRVALUETYPE_SIZEOF;
			sizeValue.sizeOfTypeTableIdx = returnTypeTableIdx;

			IRInstruction inst = {};
			inst.type = IRINSTRUCTIONTYPE_INTRINSIC_MEMCPY;
			inst.memcpy.src = src;
			inst.memcpy.dst = dst;
			inst.memcpy.size = sizeValue;
			*AddInstruction(context) = inst;

			returnValue = dst;
		}
		else
		{
			IRValue returnRegister;
			returnRegister.valueType = IRVALUETYPE_REGISTER;
			returnRegister.registerIdx = IRSPECIALREGISTER_RETURN;
			returnRegister.typeTableIdx = returnValue.typeTableIdx;
			returnRegister.dereference = false;

			IRInstruction returnValueSaveInst;
			returnValueSaveInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			returnValueSaveInst.assignment.src = returnValue;
			returnValueSaveInst.assignment.dst = returnRegister;
			*AddInstruction(context) = returnValueSaveInst;
		}

		if (isThereCleanUpToDo)
		{
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.jump.label = context->irStack[context->irStack.size - 1].closeLabel;
			*AddInstruction(context) = jumpInst;
		}
		else
		{
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.jump.label = context->currentProcedureReturnLabel;
			*AddInstruction(context) = jumpInst;
		}
	} break;
	case ASTNODETYPE_DEFER:
	{
		IRScope *stackTop = &context->irStack[context->irStack.size - 1];
		*DynamicArrayAdd(&stackTop->deferredStatements) = expression->deferNode.expression;
	} break;
	case ASTNODETYPE_TYPEOF:
	{
		result.valueType = IRVALUETYPE_TYPEOF;
		result.typeOfTypeTableIdx = expression->typeOfNode.expression->typeTableIdx;
		result.typeTableIdx = expression->typeTableIdx;
	} break;
	case ASTNODETYPE_CAST:
	{
		result = IRGenFromExpression(context, expression->castNode.expression);
		result.typeTableIdx = expression->typeTableIdx;
	} break;
	case ASTNODETYPE_ENUM_DECLARATION:
	{
	} break;
	}
	return result;
}

void PrintIRValue(Context *context, IRValue value)
{
	if (value.dereference)
		Log("[");

	if (value.valueType == IRVALUETYPE_REGISTER)
		if (value.registerIdx == IRSPECIALREGISTER_RETURN)
			Log("$rRet");
		else if (value.registerIdx == IRSPECIALREGISTER_SHOULD_RETURN)
			Log("$rDoRet");
		else
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
	case IRINSTRUCTIONTYPE_MODULO:
		Log("%");
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
	DynamicArrayInit(&context->irStaticVariables, 64);
	DynamicArrayInit(&context->irStack, 64);
	context->currentProcedure = nullptr;
	context->currentRegisterId = 0;
	context->currentLabelId = 1;

	PushIRScope(context);

	for (int statementIdx = 0; statementIdx < context->astRoot->block.statements.size; ++statementIdx)
	{
		ASTExpression *statement = &context->astRoot->block.statements[statementIdx];
		IRGenFromExpression(context, statement);
	}
}
