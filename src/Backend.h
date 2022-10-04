struct BasicBlock;
struct BEInstruction;
struct BEFinalProcedure;

struct InterferenceGraph
{
	u32 count;
	u32 capacity;
	u32 *valueIndices;
	u8 *removed;
	HashSet<u32, JobAllocator> *edges; // @Improve?

	HashMap<u32, u32, JobAllocator> valueToNodeMap;
};

void BackendJobProc(Context *context, u32 procedureIdx);
