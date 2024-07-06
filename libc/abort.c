#include <signal.h>
#include <stdlib.h>

void abort(void)
{
  // @TODO: sigprocmask to unblock SIGABRT
  raise(SIGABRT);
}
