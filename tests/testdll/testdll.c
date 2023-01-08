__declspec(dllimport) void *GetStdHandle(unsigned int num);
__declspec(dllimport) int WriteFile(void *file, void *buffer, unsigned int count,
		unsigned int *written, void *overlapped);

__declspec(dllexport) int TestProc(int a, int b, int c, int d, int e, int f, int g, int h)
{
	int args[] = { a, b, c, d, e, f, g, h };
	int sum = 0;
	char buffer[512] = "Arguments: ";
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
	void *stdOut = GetStdHandle((unsigned int)-11);
	WriteFile(stdOut, buffer, cursor - buffer, 0, 0);
	return a + b + c + d + e + f + g + h;
}
