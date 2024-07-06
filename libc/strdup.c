#define _POSIX_C_SOURCE 200809L
#include <string.h>

char *strdup(const char *str) { return strndup(str, strlen(str)); }
