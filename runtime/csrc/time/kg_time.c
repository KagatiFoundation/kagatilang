#include <time.h>
#include <unistd.h>

#include "kg_time.h"

void __KG_POSIX__sleep(int s) {
	sleep(s);
}