struct IRJobArgs
{
	Context *context;
	u32 jobIdx;
	u32 procedureIdx;
	BucketArray<Value, HeapAllocator, 1024> localValues;
	const ASTExpression *expression;
};

enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_VALUE,
	IRVALUETYPE_VALUE_DEREFERENCE,
	IRVALUETYPE_PROCEDURE,
	IRVALUETYPE_TUPLE,
	IRVALUETYPE_IMMEDIATE_INTEGER,
	IRVALUETYPE_IMMEDIATE_FLOAT,
	IRVALUETYPE_IMMEDIATE_STRING
};
struct IRValue
{
	IRValueType valueType;
	union
	{
		s64 immediate;
		f64 immediateFloat;
		u32 immediateStringIdx;
		Array<IRValue, LinearAllocator> tuple;
		u32 procedureIdx;
		struct
		{
			u32 valueIdx;
			u32 indexValueIdx;
			u64 elementSize;
			s64 offset;
		} value;
	};
	u32 typeTableIdx;
};

struct IRLabel
{
	String name;
	s64 instructionIdx;
};

struct IRJump
{
	IRLabel *label;
};

struct IRConditionalJump
{
	IRLabel *label;
	IRValue condition;
};

struct IRConditionalJump2
{
	IRLabel *label;
	IRValue left;
	IRValue right;
};

struct IRProcedureCall
{
	union
	{
		u32 procedureIdx;
		IRValue procIRValue;
	};
	Array<IRValue, LinearAllocator> parameters;
	Array<IRValue, LinearAllocator> returnValues;

	// Filled during register allocation
	u64 liveRegisters;
};

struct IRIntrinsic
{
	IntrinsicType type;
	Array<IRValue, LinearAllocator> parameters;
};

struct IRPushValue
{
	u32 valueIdx;
};

struct IRAssignment
{
	IRValue src;
	IRValue dst;
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

struct IRCopyMemory
{
	IRValue src;
	IRValue dst;
	IRValue size;
};

struct IRZeroMemory
{
	IRValue dst;
	IRValue size;
};

enum IRInstructionType
{
	IRINSTRUCTIONTYPE_INVALID = -1,

	IRINSTRUCTIONTYPE_NOP,

	IRINSTRUCTIONTYPE_COMMENT,

	IRINSTRUCTIONTYPE_LABEL,
	IRINSTRUCTIONTYPE_JUMP,
	IRINSTRUCTIONTYPE_JUMP_IF_ZERO,
	IRINSTRUCTIONTYPE_JUMP_IF_NOT_ZERO,

	IRINSTRUCTIONTYPE_COMPARE_JUMP_BEGIN,
	IRINSTRUCTIONTYPE_JUMP_IF_EQUALS = IRINSTRUCTIONTYPE_COMPARE_JUMP_BEGIN,
	IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS,
	IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN,
	IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN,
	IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_COMPARE_JUMP_END,

	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT,
	IRINSTRUCTIONTYPE_INTRINSIC,
	IRINSTRUCTIONTYPE_PUSH_VALUE,
	IRINSTRUCTIONTYPE_PUSH_SCOPE,
	IRINSTRUCTIONTYPE_POP_SCOPE,

	IRINSTRUCTIONTYPE_ASSIGNMENT,

	IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_NOT = IRINSTRUCTIONTYPE_UNARY_BEGIN,
	IRINSTRUCTIONTYPE_BITWISE_NOT,
	IRINSTRUCTIONTYPE_SUBTRACT_UNARY,
	IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
	IRINSTRUCTIONTYPE_UNARY_END,

	IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_ADD = IRINSTRUCTIONTYPE_BINARY_BEGIN,
	IRINSTRUCTIONTYPE_SUBTRACT,
	IRINSTRUCTIONTYPE_MULTIPLY,
	IRINSTRUCTIONTYPE_DIVIDE,
	IRINSTRUCTIONTYPE_MODULO,
	IRINSTRUCTIONTYPE_SHIFT_LEFT,
	IRINSTRUCTIONTYPE_SHIFT_RIGHT,
	IRINSTRUCTIONTYPE_OR,
	IRINSTRUCTIONTYPE_AND,
	IRINSTRUCTIONTYPE_BITWISE_OR,
	IRINSTRUCTIONTYPE_BITWISE_XOR,
	IRINSTRUCTIONTYPE_BITWISE_AND,

	IRINSTRUCTIONTYPE_COMPARE_BEGIN,
	IRINSTRUCTIONTYPE_EQUALS = IRINSTRUCTIONTYPE_COMPARE_BEGIN,
	IRINSTRUCTIONTYPE_NOT_EQUALS,
	IRINSTRUCTIONTYPE_GREATER_THAN,
	IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_LESS_THAN,
	IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_COMPARE_END,
	IRINSTRUCTIONTYPE_BINARY_END,

	IRINSTRUCTIONTYPE_COPY_MEMORY,
	IRINSTRUCTIONTYPE_ZERO_MEMORY,
};
struct IRInstruction
{
	IRInstructionType type;
	union
	{
		String comment;
		IRLabel *label;
		IRJump jump;
		IRConditionalJump conditionalJump;
		IRConditionalJump2 conditionalJump2;
		IRProcedureCall procedureCall;
		IRIntrinsic intrinsic;
		IRPushValue pushValue;
		IRAssignment assignment;
		IRUnaryOperation unaryOperation;
		IRBinaryOperation binaryOperation;

		IRCopyMemory copyMemory;
		IRZeroMemory zeroMemory;
	};
};

struct IRScope
{
	IRLabel *closeLabel;
	DynamicArray<ASTExpression *, ThreadAllocator> deferredStatements;
};

struct IRStaticVariable
{
	u32 valueIdx;
	IRValue initialValue;
};

void IRJobProcedure(void *args);
void IRJobExpression(void *args);
