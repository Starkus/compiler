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

		ASSERT(rightHand->nodeType == ASTNODETYPE_IDENTIFIER);
		ASSERT(rightHand->identifier.type == NAMETYPE_STRUCT_MEMBER);
		String memberName = rightHand->identifier.string;

		s64 typeTableIdx = leftHand->typeTableIdx;
		ASSERT(typeTableIdx >= 0);
		TypeInfo *typeInfo = &context->typeTable[typeTableIdx];

		if (typeInfo->typeCategory == TYPECATEGORY_POINTER)
		{
			s64 pointedTypeIdx = typeInfo->pointerInfo.pointedTypeTableIdx;
			typeInfo = &context->typeTable[pointedTypeIdx];
		}

		if (typeInfo->typeCategory == TYPECATEGORY_ARRAY)
		{
			s64 arrayTypeTableIdx = FindTypeInStackByName(context, {}, "Array"_s);
			typeInfo = &context->typeTable[arrayTypeTableIdx];
		}

		ASSERT(typeInfo->typeCategory == TYPECATEGORY_STRUCT);
		ASSERT (rightHand->identifier.structMemberInfo.offsets.size == 1);

		StructMember *structMember = rightHand->identifier.structMemberInfo.offsets[0];
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
		s64 pointerToElementTypeIdx = GetTypeInfoPointerOf(context, elementType);

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
			*AddInstruction(context) = memberAccessInst;

			inst.arrayAccess.array = memberAccessInst.memberAccess.out;
			inst.arrayAccess.array.dereference = true;
		}

		inst.arrayAccess.elementTypeTableIdx = elementType;
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

		if (inst.memcpy.src.dereference)
			inst.memcpy.src = IRPointerToValue(context, inst.memcpy.src);
		if (inst.memcpy.dst.dereference)
			inst.memcpy.dst = IRPointerToValue(context, inst.memcpy.dst);

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

		Procedure *procedure = expression->procedureDeclaration.procedure;
		context->currentProcedure = procedure;

		BucketArrayInit(&procedure->instructions);

		context->currentProcedureStackBase = context->irStack.size;
		PushIRScope(context);

		if (procedure->astBody)
		{
			context->currentRegisterId = 0;
			IRGenFromExpression(context, procedure->astBody);
			procedure->registerCount = context->currentRegisterId;

			// Return
			IRInstruction returnInst;
			returnInst.type = IRINSTRUCTIONTYPE_RETURN;
			*AddInstruction(context) = returnInst;
		}

		PopIRScope(context);
		context->currentProcedure = prevProcedure;
		context->currentProcedureStackBase = prevProcedureStackBase;
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

		IRValue shouldReturnRegister;
		shouldReturnRegister.valueType = IRVALUETYPE_REGISTER;
		shouldReturnRegister.registerIdx = IRSPECIALREGISTER_SHOULD_RETURN;
		shouldReturnRegister.typeTableIdx = TYPETABLEIDX_U8;
		shouldReturnRegister.dereference = false;

		{
			IRValue zero;
			zero.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
			zero.immediate = 0;
			zero.typeTableIdx = TYPETABLEIDX_U8;
			zero.dereference = false;

			IRInstruction setShouldReturnInst;
			setShouldReturnInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			setShouldReturnInst.assignment.src = zero;
			setShouldReturnInst.assignment.dst = shouldReturnRegister;
			*AddInstruction(context) = setShouldReturnInst;
		}

		{
			IRInstruction *closeScopeLabelInst = AddInstruction(context);
			closeScopeLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
			closeScopeLabelInst->label = currentScope->closeLabel;
		}

		// Run deferred statements
		for (s64 j = currentScope->deferredStatements.size - 1; j >= 0; --j)
		{
			IRGenFromExpression(context, currentScope->deferredStatements[j]);
		}

		PopIRScope(context);

		// If should-return register is set, return
		{
			if ((s64)(context->irStack.size - 1) != context->currentProcedureStackBase)
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
				jumpInst.jump.label = context->irStack[context->irStack.size - 1].closeLabel;
				*AddInstruction(context) = jumpInst;

				IRInstruction *skipLabelInst = AddInstruction(context);
				skipLabelInst->type = IRINSTRUCTIONTYPE_LABEL;
				skipLabelInst->label = skipLabel;
			}
		}
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
				*AddInstruction(context) = IRInstructionFromAssignment(context, leftValue, varDecl.value);
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
				inst.memberAccess.out.valueType = IRVALUETYPE_REGISTER;
				inst.memberAccess.out.registerIdx = NewVirtualRegister(context);
				inst.memberAccess.out.typeTableIdx = GetTypeInfoPointerOf(context, structMember->typeTableIdx);
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

		IRInstruction inst = {};
		inst.type = IRINSTRUCTIONTYPE_PROCEDURE_CALL;
		inst.procedureCall.procedure = astProcCall->procedure;

		bool isReturnByCopy = IRShouldReturnByCopy(context, expression->typeTableIdx);

		// Support both varargs and default parameters here
		s32 totalParamCount = (s32)astProcCall->procedure->parameters.size;
		s32 callParamCount = (s32)astProcCall->arguments.size;
		s32 paramCount = Max(totalParamCount, callParamCount) + isReturnByCopy;
		ArrayInit(&inst.procedureCall.parameters, paramCount, malloc);

		// Return value
		inst.procedureCall.out.valueType = IRVALUETYPE_INVALID;
		if (expression->typeTableIdx != TYPETABLEIDX_VOID)
		{
			if (isReturnByCopy)
			{
				static u64 returnByCopyDeclarationUniqueID = 0;

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
				IRValue reg = {};
				reg.valueType = IRVALUETYPE_REGISTER;
				reg.registerIdx = NewVirtualRegister(context);
				reg.typeTableIdx = GetTypeInfoPointerOf(context, expression->typeTableIdx);

				IRValue tempVarIRValue = {};
				tempVarIRValue.valueType = IRVALUETYPE_VARIABLE;
				tempVarIRValue.variable = tempVar;

				IRInstruction paramIntermediateInst = {};
				paramIntermediateInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
				paramIntermediateInst.assignment.src = tempVarIRValue;
				paramIntermediateInst.assignment.dst = reg;
				*AddInstruction(context) = paramIntermediateInst;

				// Add register as parameter
				*ArrayAdd(&inst.procedureCall.parameters) = reg;

				result = reg;
			}
			else
			{
				inst.procedureCall.out = {};
				inst.procedureCall.out.valueType = IRVALUETYPE_REGISTER;
				inst.procedureCall.out.registerIdx = NewVirtualRegister(context);
				inst.procedureCall.out.typeTableIdx = expression->typeTableIdx;
				result = inst.procedureCall.out;
			}
		}

		// Set up parameters
		for (int argIdx = 0; argIdx < astProcCall->arguments.size; ++argIdx)
		{
			ASTExpression *arg = &astProcCall->arguments[argIdx];
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

		// Default parameters
		for (u64 argIdx = astProcCall->arguments.size; argIdx < astProcCall->procedure->parameters.size;
				++argIdx)
		{
			ASTExpression *arg = astProcCall->procedure->parameters[argIdx].defaultValue;
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

		{
			IRValue one;
			one.valueType = IRVALUETYPE_IMMEDIATE_INTEGER;
			one.immediate = 1;
			one.typeTableIdx = TYPETABLEIDX_U8;
			one.dereference = false;

			IRValue shouldReturnRegister;
			shouldReturnRegister.valueType = IRVALUETYPE_REGISTER;
			shouldReturnRegister.registerIdx = IRSPECIALREGISTER_SHOULD_RETURN;
			shouldReturnRegister.typeTableIdx = TYPETABLEIDX_U8;
			shouldReturnRegister.dereference = false;

			IRInstruction setShouldReturnInst;
			setShouldReturnInst.type = IRINSTRUCTIONTYPE_ASSIGNMENT;
			setShouldReturnInst.assignment.src = one;
			setShouldReturnInst.assignment.dst = shouldReturnRegister;
			*AddInstruction(context) = setShouldReturnInst;
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

		{
			IRInstruction jumpInst;
			jumpInst.type = IRINSTRUCTIONTYPE_JUMP;
			jumpInst.jump.label = context->irStack[context->irStack.size - 1].closeLabel;
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
