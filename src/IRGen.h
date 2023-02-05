enum IRValueType
{
	IRVALUETYPE_INVALID = -1,
	IRVALUETYPE_VALUE,
	IRVALUETYPE_MEMORY,
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
		u32 valueIdx;
		s64 immediate;
		f64 immediateFloat;
		u32 immediateStringIdx;
		Array<IRValue, LinearAllocator> tuple;
		u32 procedureIdx;
		struct
		{
			// When value type is DEREFERENCE, valueIdx contains the value to which a pointer has
			// been assigned (i.e. with IRINSTRUCTIONTYPE_LOAD_EFFECTIVE_ADDRESS), offset is an
			// immediate added to the pointer, and if elementSize is not 0, the value of
			// indexValueIdx * elementSize is also added. The resulting pointer is dereferenced.
			u32 baseValueIdx;
			u32 indexValueIdx;
			u64 elementSize;
			s64 offset;
		} mem;
	};
	u32 typeTableIdx;
};
static_assert(offsetof(IRValue, valueIdx) == offsetof(IRValue, mem.baseValueIdx));

struct IRLabel
{
	String name;
	s64 instructionIdx;

	// @Improve: move this out of here?
	// Offset into executable image, for backend
	u64 address;
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

	IRINSTRUCTIONTYPE_AssignmentBegin,
	IRINSTRUCTIONTYPE_ASSIGNMENT,
	IRINSTRUCTIONTYPE_CONVERT_INT_TO_FLOAT,
	IRINSTRUCTIONTYPE_CONVERT_FLOAT_TO_INT,
	IRINSTRUCTIONTYPE_CONVERT_PRECISION,
	IRINSTRUCTIONTYPE_SIGN_EXTEND,
	IRINSTRUCTIONTYPE_ZERO_EXTEND,
	IRINSTRUCTIONTYPE_TRUNCATE,
	IRINSTRUCTIONTYPE_AssignmentEnd,

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

struct IRJobArgs
{
	u32 procedureIdx;
	BucketArray<Value, LinearAllocator, 256> localValues;
	const ASTExpression *expression;
};

struct StringLiteral
{
	// This global value is a handle to the static data where the string is.
	// @Improve: We should probably do this in a better way...
	u32 globalValueIdx;
	String string;
};

struct FloatLiteral
{
	u32 globalValueIdx;
	union {
		f32 *asF32;
		f64 *asF64;
	};
};

struct IRContext : JobContext
{
	u32 procedureIdx;
	BucketArray<Value, LinearAllocator, 256> *localValues;
	BucketArray<IRInstruction, LinearAllocator, 256> *irInstructions;
	BucketArray<IRLabel, LinearAllocator, 256> irLabels;
	DynamicArray<IRScope, ThreadAllocator> irStack;
	IRLabel *returnLabel;
	IRLabel *currentBreakLabel;
	IRLabel *currentContinueLabel;
	IRLabel *currentContinueSkipIncrementLabel;
	struct {
		IRValue arrayValue;
		IRValue indexValue;
	} irCurrentForLoopInfo;
	ArrayView<u32> returnValueIndices;
	u32 shouldReturnValueIdx;
};

IRValue IRGenFromExpression(IRContext *context, const ASTExpression *expression);
void IRJobProcedure(u32 jobIdx, void *args);
void IRJobExpression(u32 jobIdx, void *args);
