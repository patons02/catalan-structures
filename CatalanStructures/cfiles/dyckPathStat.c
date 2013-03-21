#include <stdbool.h>
#include <string.h>

/* Number of initial rises for a dyck path */

long
initR (const long *w, long len)
{
	long acc = 0;
	bool flag = false;
	int i;	

	for(i = 0; len > 1, len--; w++, i++) {
		if(w[0] == 1)
			flag = true;		
		
		if (flag == true && w[i] == 1)
			acc++;
	}

	return acc;
}

/* Number of double rises for a dyck path */

long
doubR (const long *w, long len)
{
	long acc = 0;
	
	for(; len > 1, len--; w++) {
		if (*w == 1 && *(w-1) == 1)
			acc++;
	}

	return acc;
}

/* Major index of dyck path */
long
majD (const long *w, long len){
	
	long i, acc = 0;

	for(i = 1; i < len; i++, w++) {
		if(*w > *(w+1))
			acc += i;
	}

	return acc;
}
