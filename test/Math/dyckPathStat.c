#include <string.h>

/* Number of initial rises for a dyck path */

long
init (const long *w, long len)
{
	long acc = 0;

	for(; len > 1; len--; w++){
		if (*w == 'u')
			acc++
		else
			return acc;
	}

	return acc;
}
