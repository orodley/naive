#ifndef _SIGNAL_H
#define _SIGNAL_H

// @TODO: This isn't correct! signal.h is not permitted to define all the types
// defined by sys/types.h, only some of them. The way to work around this seems
// to be nasty macros indicating which types need to be defined, and including
// a master "types" header file which defines the types that are needed (unless
// already defined). I can't be bothered doing this right now.
#include <sys/types.h>

int kill(pid_t pid, int sig);
int raise(int sig);

#define SIGABRT 6

#endif
