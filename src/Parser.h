struct Tokenizer
{
	const char *cursor;
	const char *end;
	u32 line;
	u32 fileIdx;
	const char *beginningOfLine;
};

struct Context;
struct ParseJobArgs
{
	u32 fileIdx;
};

struct JobRequest
{
	u32 jobIdx;
	JobType type;
	void (*proc)(void *);
	void *args;
};

struct FatSourceLocation
{
	const char *beginingOfLine;
	u32 lineSize;
	u32 size;
	u32 line;
	u32 column;
};

struct SourceFile
{
	String name;
	SourceLocation includeLoc;
	const char *buffer;
	u64 size;
};

struct Token
{
	enum TokenType type;
	u16 size;
	SourceLocation loc;
};

void ParseJobProc(u32, void *);

struct PContext
{
	u32 fileIdx;
	u64 currentTokenIdx;
	Token *token;
	BucketArray<Token, HeapAllocator, 1024> tokens;
	ASTRoot astRoot;
	BucketArray<ASTExpression, HeapAllocator, 1024> astTreeNodes;
	BucketArray<ASTType, HeapAllocator, 1024> astTypes;
};
