#include <assert.h>

int main()
{
	assert("a"[0] == 97);
	assert("z"[0] == 122);
	assert("A"[0] == 65);
	assert("Z"[0] == 90);
	assert("\n"[0] == 10);
	assert("\r"[0] == 13);
	assert("\0"[0] == 0);
	assert("\""[0] == 34);
	assert("\01"[0] == 1);
	assert("\013"[0] == 013);
	assert("\x12"[0] == 0x12);

	return 0;
}
