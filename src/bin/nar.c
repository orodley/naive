#include <stdio.h>
#include <string.h>

#include "array.h"
#include "file.h"
#include "misc.h"
#include "util.h"

static u8 read_buf[8192];

int main(int argc, char *argv[])
{
	if (argc < 3) {
		fprintf(stderr, "Usage: %s -cr <archive name> <input files>...\n", argv[0]);
		return 1;
	}
	if (!streq(argv[1], "-cr")) {
		fprintf(stderr, "Sorry, we only support '-cr' at the moment, not '%s'\n",
				argv[1]);
		return 2;
	}

	char *ar_filename = argv[2];
	int input_files_start = 3;

	FILE *ar_file = fopen(ar_filename, "wb");
	if (ar_file == NULL) {
		perror("Failed to create archive file");
		return 3;
	}

	checked_fwrite(AR_GLOBAL_HEADER, sizeof AR_GLOBAL_HEADER - 1, 1, ar_file);

	Array(char) too_long_filenames = EMPTY_ARRAY;
	for (int i = input_files_start; i < argc; i++) {
		// File headers are aligned to even byte boundaries.
		if (checked_ftell(ar_file) % 2 == 1)
			checked_fseek(ar_file, 1, SEEK_CUR);

		ArFileHeader file_header;
		memset(&file_header, ' ', sizeof file_header);

		char *input_filename = argv[i];

		FILE *input_file = fopen(input_filename, "rb");
		if (input_file == NULL) {
			perror("Failed to open input file");
			return 6;
		}

		checked_fseek(input_file, 0, SEEK_END);
		long file_size = checked_ftell(input_file);
		checked_fseek(input_file, 0, SEEK_SET);


		// Bleh - there's no way to tell *printf not to append a NULL, so we
		// manually overwrite the NULL with a space. Since we write from
		// left-to-right we accept that the NULL might run over into the next
		// field - it won't overwrite anything we've already written, and we
		// can't overrun the buffer since we don't write into the last member.
		size_t filename_len = strlen(input_filename);
		if (filename_len > sizeof file_header.name) {
			u32 offset = too_long_filenames.size;

			ARRAY_APPEND_ELEMS(&too_long_filenames, char, filename_len, input_filename);
			*ARRAY_APPEND(&too_long_filenames, char) = '\n';

			u32 n = snprintf(file_header.name,
					sizeof file_header.name + 1,
					"/%u", offset);
			file_header.name[n] = ' ';
		} else {
			u32 n = snprintf(file_header.name,
					sizeof file_header.name + 1,
					"%s/", input_filename);
			file_header.name[n] = ' ';
		}
		file_header.modification_timestamp_decimal[0] = '0';
		file_header.owner_id_decimal[0] = '0';
		file_header.group_id_decimal[0] = '0';
		file_header.mode_octal[0] = '6';
		file_header.mode_octal[1] = '4';
		file_header.mode_octal[2] = '4';
		u32 n = snprintf(file_header.size_bytes_decimal,
				sizeof file_header.size_bytes_decimal + 1,
				"%ld", file_size);
		file_header.size_bytes_decimal[n] = ' ';
		file_header.magic[0] = '\x60';
		file_header.magic[1] = '\x0A';

		checked_fwrite(&file_header, sizeof file_header, 1, ar_file);

		long bytes_left = file_size;
		while (bytes_left != 0) {
			long bytes_read = fread(read_buf, 1, sizeof read_buf, input_file);
			if (bytes_read != sizeof read_buf && !feof(input_file)) {
				perror("Error reading from input file");
				return 7;
			}
			checked_fwrite(read_buf, 1, bytes_read, ar_file);

			bytes_left -= bytes_read;
		}

		fclose(input_file);
	}

	if (too_long_filenames.size != 0) {
		ArFileHeader file_header;
		memset(&file_header, ' ', sizeof file_header);

		file_header.name[0] = '/';
		file_header.name[1] = '/';
		u32 n = snprintf(file_header.size_bytes_decimal,
				sizeof file_header.size_bytes_decimal + 1,
				"%u", too_long_filenames.size);
		file_header.size_bytes_decimal[n] = ' ';
		file_header.magic[0] = '\x60';
		file_header.magic[1] = '\x0A';

		// File headers are aligned to even byte boundaries.
		if (checked_ftell(ar_file) % 2 == 1)
			checked_fseek(ar_file, 1, SEEK_CUR);

		checked_fwrite(&file_header, sizeof file_header, 1, ar_file);

		checked_fwrite(too_long_filenames.elements,
				too_long_filenames.size, 1, ar_file);
	}

	fclose(ar_file);

	return 0;
}
