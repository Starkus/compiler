#include <stdint.h>
#if _MSC_VER
#define IMPORT __declspec(dllimport)
#define EXPORT __declspec(dllexport)
#define WINDOWSCC
#else
#define IMPORT
#define EXPORT
#define WINDOWSCC __attribute__((ms_abi))
#endif

#if _MSC_VER
IMPORT void *GetStdHandle(unsigned int num);
IMPORT int WriteFile(void *file, void *buffer, unsigned int count,
		unsigned int *written, void *overlapped);
#else
//int64_t write(int fd, const void *buf, uint64_t count);
#include <unistd.h>
#include <sys/syscall.h>
#endif
//IMPORT void Print(uint64_t size, char *buffer);

const char prefix[] = "Arguments: ";

EXPORT int TestProc(int a, int b, int c, int d, int e, int f, int g, int h)
{
	int args[] = { a, b, c, d, e, f, g, h };
	int sum = 0;
	char buffer[512];
	// Copy start of string "Arguments:" avoiding a memcpy
    *(uint64_t *)(buffer+0) = *(uint64_t *)(prefix+0);
    *(uint32_t *)(buffer+8) = *(uint32_t *)(prefix+8);
	char *cursor = buffer + 11;
	for (int i = 0; i < 8; ++i) {
		if (i > 0) {
			*cursor++ = ',';
			*cursor++ = ' ';
		}
		int n = args[i];
		if (n < 0) {
			*cursor++ = '-';
			n = -n;
		}
		while (n > 0) {
			int digit = n % 10;
			*cursor++ = '0' + digit;
			n /= 10;
		}
	}
	*cursor++ = '\n';

#if _MSC_VER
	void *stdOut = GetStdHandle((unsigned int)-11);
	WriteFile(stdOut, buffer, cursor - buffer, 0, 0);
#else
	register int syscallCode asm ("eax") = SYS_write;
	register int fd asm ("edi") = 1;
	register const void *buf asm ("rsi") = buffer;
	register uint64_t count asm ("rdx") = cursor - buffer;
	asm volatile (
		"syscall"
		:: "r" (syscallCode), "r" (fd), "r" (buf), "r" (count)
		);
#endif

	return a + b + c + d + e + f + g + h;
}

EXPORT WINDOWSCC int TestProcWinCC(int a, int b, int c, int d, int e, int f, int g, int h)
{
	return TestProc(a, b, c, d, e, f, g, h);
}
