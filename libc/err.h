#ifndef _ERR_H
#define _ERR_H

#include <stdint.h>

#define MAX_ERRNO 4095

#define PTR_IS_ERR(x) ((uint64_t)(void *)(x) >= (uint64_t)-MAX_ERRNO)

#endif
