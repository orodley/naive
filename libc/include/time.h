#ifndef _TIME_H
#define _TIME_H

typedef long time_t;

struct timespec
{
	time_t tv_sec;
	long tv_nsec;
};

time_t time(time_t *t);

#endif
