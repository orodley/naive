#include <assert.h>

int main()
{
	assert('a' == 97);
	assert('z' == 122);
	assert('A' == 65);
	assert('Z' == 90);
	assert('\n' == 10);
	assert('\r' == 13);
	assert('\0' == 0);
	assert('\01' == 1);
	assert('\013' == 013);
	assert('\x12' == 0x12);

	return 0;
}
