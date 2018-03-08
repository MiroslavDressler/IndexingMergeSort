
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#define N 99999999

float A[N];
int   B[N],C[N];


void mergesort (long n, float a[], int ib[], int ic[])    //   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15  ...
{int ll, lr, le;                                          // |--|--|--|--|--|--|--|--|--|--|--|--|--|--|--| ...
 int i,j,k,m;                                             //  ll          lr          le                    ... example for k=4
                                                          //        SA1         SA2

 for (i=0; i<n; i++) ib[i]=i;                             // set initial index array
 for (k=1; k < n; k=k+k )                                 // outer cycle for increasing subarray (SA) size k
  {for (ll=0; ll+k < n; ll += k*2 )                       // ll = left index of the first SA
   {lr = ll + k;                                          // lr = right index of SA; inner cycle
    le = lr + k;                                          // le = end of second SA
    if (le > n) le = n;
    m = ll; i = ll; j = lr;
    while (i < lr && j < le)                              // merge the first and the second SA into ic
     {if   (a[ib[i]] <= a[ib[j]]) {ic[m] = ib[i]; i++;}   // move from the first SA
      else                        {ic[m] = ib[j]; j++;}   // move from the second SA
      m++;}
    while (i < lr) {ic[m]=ib[i]; i++; m++;}               // move the rest of the first SA
    while (j < le) {ic[m]=ib[j]; j++; m++;}               // move the rest of the second SA
    for (m=ll; m < le; m++) ib[m] = ic[m];                // move merged part into ib, merging finished
   }
  }
}


int isSorted(float A[], int B[])
{float prev = A[B[0]];                                    // check if the array is sorted
 for (int i = 1; i < N; i++)
  {if (prev > A[B[i]]) {printf("Iterative MergeSort Fails!!"); return 0;}
   prev = A[B[i]];}
 return 1;
}


int main()
{clock_t start, end;
 double cpu_time_used;

 // generate input array
 for (int i = 0; i < N; i++)
  if (i % 3 ==0) A[i] = (N-i)/3.0; else A[i]=(2*i-N)/7.0;

 start = clock();

 // sort array A[0..N-1] using temporary arrays B and C
 mergesort(N, A, B, C);

 end = clock();
 cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

 // check if array A is sorted via index array B
 if (isSorted(A,B))
  {printf("%f %f\n", A[B[0]],A[B[N-1]]);
   printf("Elapsed time: %f sec\n",cpu_time_used);}

    return 0;
}



