#define PRINT_TOKEN_SOURCE_LOCATION 0
#define PRINT_DEBUG_NOTES_UPON_YIELDING 0
#define USE_OWN_ASSEMBLER 1

// To properly turn this off, we'd need to make sure we don't leak locks anywhere we call LogError.
#define EXIT_ON_FIRST_ERROR 1

u64 g_pointerSize = 8;
