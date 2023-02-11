struct BasicBlock;
struct BEInstruction;
struct BEFinalProcedure;

struct InterferenceGraph
{
	u32 count;
	u32 capacity;
	u32 *valueIndices;
	u64 *removed;
	HashSet<u32, ThreadAllocator> *edges; // @Improve?

	HashMap<u32, u32, ThreadAllocator> valueToNodeMap;
};

void BackendJobProc(IRContext *context, u32 procedureIdx);
