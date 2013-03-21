#include <string.h>
#include "bijections.h"

long
richards (const long *b, long len)
{
	char str[len];

	long a[len];
	long n = len;
	long r = n+1;
	long s = n+1;
	int i, j, k = 1;

	for(i = 0; i < len; i++)
	{
		if (&b[j] == 1) {
			do {
				s = s-1;
				j = j+1;
			} while (&b[j] == 0);
		
			a[s] = i;
		}
		else {
			do {
				r = r-1;
			} while (&a[r] != 0);

			a[r] = i;
		}
		
		j = j+1;

	}

	for(k = 0; k < sizeof(a); k++)
	{
		str[k] = a[k];
	}	

	return strtol (*str, NULL, 10); 
}
