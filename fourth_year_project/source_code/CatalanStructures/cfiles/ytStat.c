#include<stdio.h>

long
des(const long *w, long len)
{
	long acc = 0;

	for (; len > 1; len--, w++) {
		if (*w > *(w+1))
			acc++;
	}
	return acc;
}
