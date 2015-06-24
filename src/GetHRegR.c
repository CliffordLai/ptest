// Fits 4 parameter harmonic regression using OLS
// y = mu + A cos(2 pi f t) + B sin(2 pi f t) + e
// returns sqrt(R-square) and  f

   
#include "stdafx.h"           

// Input Variables:
//  *y is pointer to a vector containing the time series (missing values excluded)
//  *t is pointer to a vector containing the time points (in full data case: 1, ..., n)
//  *an is the number of time points corresponding to the observed data
//
// Output Variables:
//   sqrt(R-square) = sqrt(SSR/SST)
   
// __declspec( dllexport ) void GetHRegR(double *y, double *t, double *theta){                             
void GetHRegR(double *y, double *t, double *theta){ 
	int i,j, nF=50; //nF number of frequencies//
	
	VECTOR lam;
	MATRIX X;
	int n;
	VECTOR Reg;
	double MaxRegSS, lamOpt, aF, an;
	double SSTot, SSR;
	double ave, sumY;
	
	SSTot = 0.0;
	sumY = 0.0;
	n= (int) theta[0];
	an= theta[0];
	X=Matrix(n,3);
        Reg = Vector(nF);
	lam = Vector(nF);
	aF = 2*nF+1;
	for (i=0;i<nF;i++)
		lam[i]=(i+1.0)/aF;

  
	for(i=0;i<nF;i++)
		Reg[i]=RegSS(lam[i],t,n,y,X);  
	
	MaxRegSS = Reg[0];
	lamOpt = lam[0];
	for (j=1;j<nF;j++)
		if(Reg[j]>MaxRegSS) {
			MaxRegSS = Reg[j];
			lamOpt = lam[j];
			}
	for (i = 0;i<n;i++)
		sumY +=y[i];
	ave=sumY/n;
	for(i = 0;i<n;i++)
		SSTot +=((y[i]-ave)*(y[i]-ave));
	
	SSR = MaxRegSS - an*ave*ave;	

        theta[0] = sqrt(SSR/SSTot);  //     sqrt(R-square) = sqrt(SSR/SST)
        theta[1] = lamOpt;

	free_matrix(X);
	free_vector(lam);
	free_vector(Reg);
	
	return;
	
}

