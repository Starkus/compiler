struct BasicBlock;
struct BEInstruction;
struct BEFinalProcedure;

struct InterferenceGraph
{
	u32 count;
	u32 capacity;
	u32 *valueIndices;
	u8 *removed;
	HashSet<u32, ThreadAllocator> *edges; // @Improve?

	HashMap<u32, u32, ThreadAllocator> valueToNodeMap;
};

void BackendJobProc(Context *context, s32 procedureIdx);
