enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_VALUE,
	IRVALUETYPE_VALUE_DEREFERENCE,
	IRVALUETYPE_PROCEDURE,
	IRVALUETYPE_TUPLE,
	IRVALUETYPE_IMMEDIATE_INTEGER,
	IRVALUETYPE_IMMEDIATE_FLOAT,
	IRVALUETYPE_IMMEDIATE_STRING,
	IRVALUETYPE_IMMEDIATE_CSTR
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
	DynamicArray<IRValue, LinearAllocator> parameters;
	DynamicArray<IRValue, LinearAllocator> returnValues;

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

struct IRReturn
{
	Array<u32, LinearAllocator> returnValueIndices;
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

	IRINSTRUCTIONTYPE_CompareJumpBegin,
	IRINSTRUCTIONTYPE_JUMP_IF_EQUALS,
	IRINSTRUCTIONTYPE_JUMP_IF_NOT_EQUALS,
	IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN,
	IRINSTRUCTIONTYPE_JUMP_IF_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN,
	IRINSTRUCTIONTYPE_JUMP_IF_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_CompareJumpEnd,

	IRINSTRUCTIONTYPE_RETURN,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL,
	IRINSTRUCTIONTYPE_PROCEDURE_CALL_INDIRECT,
	IRINSTRUCTIONTYPE_INTRINSIC,
	IRINSTRUCTIONTYPE_PUSH_VALUE,
	IRINSTRUCTIONTYPE_PUSH_SCOPE,
	IRINSTRUCTIONTYPE_POP_SCOPE,

	IRINSTRUCTIONTYPE_ASSIGNMENT,

	IRINSTRUCTIONTYPE_UnaryBegin,
	IRINSTRUCTIONTYPE_NOT,
	IRINSTRUCTIONTYPE_BITWISE_NOT,
	IRINSTRUCTIONTYPE_SUBTRACT_UNARY,
	IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS,
	IRINSTRUCTIONTYPE_UnaryEnd,

	IRINSTRUCTIONTYPE_BinaryBegin,
	IRINSTRUCTIONTYPE_ADD,
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

	IRINSTRUCTIONTYPE_CompareBegin,
	IRINSTRUCTIONTYPE_EQUALS,
	IRINSTRUCTIONTYPE_NOT_EQUALS,
	IRINSTRUCTIONTYPE_GREATER_THAN,
	IRINSTRUCTIONTYPE_GREATER_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_LESS_THAN,
	IRINSTRUCTIONTYPE_LESS_THAN_OR_EQUALS,
	IRINSTRUCTIONTYPE_CompareEnd,
	IRINSTRUCTIONTYPE_BinaryEnd = IRINSTRUCTIONTYPE_CompareEnd,

	IRINSTRUCTIONTYPE_COPY_MEMORY,
	IRINSTRUCTIONTYPE_ZERO_MEMORY,

	IRINSTRUCTIONTYPE_COMPILER_BREAKPOINT,
};
struct IRInstruction
{
	IRInstructionType type;
	SourceLocation loc;
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
		IRReturn returnInst;

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

struct IRJobArgs
{
	Context *context;
	u32 procedureIdx;
	BucketArray<Value, LinearAllocator, 1024> localValues;
	const ASTExpression *expression;
};

void IRJobProcedure(void *args);
void IRJobExpression(void *args);
