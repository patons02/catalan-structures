#include <stdio.h>
#include <string.h>
#include "bijections.h"

long richards (const long *b, long len){

/* B is a dyck path b1b2...b2n
   pi is a permutation a1a2...an in Sn(123)
*/

	int r, s, i, j, k;
	long n, p;

	n = len / 2;

	long pi[n];
	
	
	memset(pi, NULL, n);

	r = n+1; s = n+1; j = 0;

	for (i = 0; i < n; i++){
		if(b[j] == 2){
			do{
				s = s-1; j = j+1;
			} while(b[j] != 1);
			
			pi[s] = i;
		}
		else if(b[j] == 1){
			do{		
				r = r-1;
			} while(pi[r] != NULL);
			
			pi[r] = i;
		}
		else {
			continue;
		}
		
		j = j+1;
	}
	
	for(k = 0, p = 0; k < n; k++) 
	{	
		p=10*p+pi[k];
	}	
	
	return p;
}





