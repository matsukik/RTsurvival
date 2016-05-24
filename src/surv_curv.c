#include <R.h>

void surv_curv(double *x, int *nx, double *bin, int *nbin, double *result)
{
	int i, j;
	double sum;
	
	for(i=0; i<*nbin; i++){
		sum = 0;
		for(j=0;j<*nx; j++){
			if(x[j] > bin[i])
				sum++;
		}
		result[i] = sum / *nx;
	}
}