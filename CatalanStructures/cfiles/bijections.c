#include <string.h>
#include "bijections.h"

int
richards(const int *w, int len){

	int l = (int) len;
	int n = l/2;	

	int a[len];	
	int r = n + 1;
	int s = n + 1;
	int j = 1;
	int i, k, perm;

	for(i = 0; i < len; i++){
		if (w[j] == 2) {
			do {
				s = s - 1;
				j = j + 1;
			} while (w[j] == 0);
		
			a[s] = i;		
		}
		else if (w[j] == 1)  {
			do {
				r = r - 1;
			} while (a[r] != 0);

		a[r] = i;

		}
		else {
			continue;
		}

		j = j + 1;

	}

	for(k = 0, perm = 0; k < len; k++) {
		perm = 10 * perm + a[k];
	}

	return perm;
}
