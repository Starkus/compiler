#include <stdint.h>

#define IMPORT __declspec(dllimport)
#define EXPORT __declspec(dllexport)
#define WINDOWSCC __attribute__((ms_abi))
#define LINUXCC   __attribute__((sysv_abi))

#if _WIN32
IMPORT void *GetStdHandle(unsigned int num);
IMPORT int WriteFile(void *file, void *buffer, unsigned int count,
		unsigned int *written, void *overlapped);
#else
#include <unistd.h>
#include <sys/syscall.h>
#endif

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

		char numBuffer[32];
		char *numCursor = &numBuffer[31];
		// Fill numBuffer backwards
		while (n > 0) {
			int digit = n % 10;
			*numCursor-- = '0' + digit;
			n /= 10;
		}
		// Copy only what we wrote, forwards
		while (numCursor != numBuffer+32)
			*cursor++ = *++numCursor;
	}
	*cursor++ = '\n';

#if _WIN32
	void *stdOut = GetStdHandle((unsigned int)-11);
	WriteFile(stdOut, buffer, cursor - buffer, 0, 0);
#else
	// Just calling 'write' forces whoever uses this lib to link against libc.so. I don't wanna link
	// libc!
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

EXPORT LINUXCC int TestProcLinuxCC(int a, int b, int c, int d, int e, int f, int g, int h)
{
	return TestProc(a, b, c, d, e, f, g, h);
}
