#include <string.h>
#include <stdio.h>
#include <assert.h>

#define STR_LEN 10

int main(int argc, char **argv)
{
	/* Simple example of buffer overflow.
	 * strcpy does not check the length of the data it copied.
	 * */
	char str[STR_LEN];
	/* strncpy adds the null terminate for the dest string since C99. */
	strncpy(str, "akjsdhkhqiueryo34928739r27yeiwuyfiusdciuti7twe79ye",
		STR_LEN - 1);
	assert(str[9] == 0);
	printf("%s\n", str);
	strcpy(str, "akjsdhkhqiueryo34928739r27yeiwuyfiusdciuti7twe79ye");
	return 0;
}
