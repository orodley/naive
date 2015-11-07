#include <stdio.h>
#include "misc.h"
#include "tokenise.h"

int main(int argc, char *argv[])
{
	if (argc != 2) {
		fprintf(stderr, "Usage: %s <input file>\n", argv[0]);
		return 1;
	}

	char *input_filename = argv[1];
	tokenise(input_filename);

	return 0;
}
