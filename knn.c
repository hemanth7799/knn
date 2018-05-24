#include<stdio.h>
#include <string.h>
#include <math.h>
#include<stdlib.h>
#include<time.h>
#include<limits.h>
typedef int validation;
typedef float validation1;

void merge(validation1 *arr,validation *ind,int str, int l, int m, int r,int c)
{
    int i, j, k;
    int n1 = m - l + 1;
    int n2 =  r - m;
    float L[n1], R[n2];
    int L1[n1], R1[n2];
 

    for (i = 0; i < n1; i++){
        L[i] = *(arr+str*c+(l + i));
        L1[i] = *(ind+str*c+(l + i));
    }
    for (j = 0; j < n2; j++){
        R[j] = *(arr+str*c+(m + 1+ j));
        R1[j] = *(ind+str*c+(m + 1+ j));
    }
    
    i = 0; 
    j = 0;
    k = l; 
    while (i < n1 && j < n2)
    {
        if (L[i] <= R[j])
        {
            *(arr+str*c+k) = L[i];
            *(ind+str*c+k) = L1[i];
            i++;
        }
        else
        {
            *(arr+str*c+k) = R[j];
            *(ind+str*c+k) = R1[j];
            j++;
        }
        k++;
    }
    /* Copy the remaining elements of L[], if there
       are any */
    while (i < n1)
    {
        *(arr+str*c+k) = L[i];
        *(ind+str*c+k) = L1[i];
        i++;
        k++;
    }
    while (j < n2)
    {
        *(arr+str*c+k) = R[j];
        *(ind+str*c+k) = R1[j];
        j++;
        k++;
    }
}
 

void mergeSort(validation1 *arr,validation *ind,int str, int l, int r,int c)
{
    if (l < r)
    {
        
        int m = l+(r-l)/2;
 
     
        mergeSort(arr,ind,str,l,m,c);
        mergeSort(arr,ind,str,m+1,r,c);
 
        merge(arr,ind,str,l,m,r,c);
    }
}
int findmax(validation *mat, int str, int col,int lab[]){
    int i,ind;
    int max=INT_MIN;
    for(i=0;i<10;i++){
        if(*(mat+str*10+i)>max){
            max=*(mat+str*10+i);
            ind=i;
        }
    }
    return ind;
}
void max_index(validation *mat, int arr[],int n, int lab[]){
    int i,j;
    for(i=0;i<n;i++){
       j=findmax(mat,i,10,lab);
       arr[i]=j;
    }

}
void error_calculate(validation *test, validation *train, float err[][25],int lab_val[], int lab_tra[], int r, int n){
	int i,j,l,k,m,error,lab_max[2223]={0};
    float sum;
    validation1 *dist=(validation1 *)malloc(2223*4446*sizeof(validation1));
    validation *index=(validation *)malloc(2223*4446*sizeof(validation));
	for(i=0;i<2223;i++){
		for(j=0;j<4446;j++){
            sum = 0.0;
            for(l=0;l<192;l++){
            	sum = sum + pow((*(test+i*192+l) - *(train+j*192+l)),2);
            }
           *(dist+i*4446+j)=sqrt(sum);
           *(index+i*4446+j)=j;
		}	
	}
    for(i=0;i<2223;i++){
        mergeSort(dist,index,i,0,4446,4446);
    }

    
	for(k=0;k<n;k++){
		error=0;
     
        for(i=0;i<2223;i++){
            for(j=0;j<k+1;j++){
                if(lab_val[i]!=lab_tra[*(index+i*2223+j)]){
                    error++;
                }
            }
        }

        err[r][k]=( error/((k+1)*2223*1.0) );
	}
    printf("error %d fold:\n",r);
    for(k=0;k<n;k++){
        printf("%f ",err[r][k]);
    }
    printf("\n");
}
void accurate(int test[][192], int train[][192],int labelTest[],int label[],int k){
    int i,j,l,m,error,lab_max[3333]={0};
    float sum;
    validation1 *dist=(validation1 *)malloc(3333*6669*sizeof(validation1));
    validation *index=(validation *)malloc(3333*6669*sizeof(validation));
    for(i=0;i<3333;i++){
        for(j=0;j<6669;j++){
            sum = 0.0;
            for(l=0;l<192;l++){
                sum = sum + pow(test[i][l] - train[j][l],2);
            }
           *(dist+i*6669+j)=sqrt(sum);
           *(index+i*6669+j)=j;
        }   
    }
    for(i=0;i<3333;i++){
        mergeSort(dist,index,i,0,6669,6669);
    } 
    validation *mode=(validation *)malloc(3333*10*sizeof(validation));
    error=0;
    for(i=0;i<3333;i++){
        for(j=0;j<10;j++){
           *(mode+i*10+j)=0;
        }
    }
    for(m=0;m<k;m++){
       for(i=0;i<3333;i++){
            *(mode+i*10+label[*(index+i*6669+m)])=*(mode+i*10+label[*(index+i*6669+m)])+1;
        }
    }
    max_index(mode,lab_max,3333,labelTest);
    printf("\n");
    for(i=0;i<3333;i++){
        if(labelTest[i]==lab_max[i]){
            error++;
        }
    }
        
    printf("%d ",error);

    printf("\n");
    printf("%f\n",(error/3333.0)*100);
}
int main(){
    
	FILE *f1 = fopen("pp_tra.dat", "r"); 
    //printf("hi\n");
	int train[6669][192];
	int label[6669];
	int i,j,c;
	for(i=0;i<6669;i++){
		for(j=0;j<192;j++){
			fscanf(f1,"%d",&train[i][j]);
		}
        fscanf(f1,"%d",&label[i]);
	}
	//---------------------------------------------------validation process------------------------------------//
	validation *validation_1=(validation *)malloc(2223*192*sizeof(validation));
	validation *validation_2=(validation *)malloc(2223*192*sizeof(validation));
    validation *validation_3=(validation *)malloc(2223*192*sizeof(validation));
    validation *train_1=(validation *)malloc(4446*192*sizeof(validation));
    validation *train_2=(validation *)malloc(4446*192*sizeof(validation));
    validation *train_3=(validation *)malloc(4446*192*sizeof(validation));
    int labVal_1[2223],labVal_2[2223],labVal_3[2223];
    int labTra_1[4446],labTra_2[4446],labTra_3[4446];

    for(i=0;i<2223;i++){
    	for(j=0;j<192;j++){
    		*(validation_1+i*192+j)=train[i][j];
    		
    		*(train_2+i*192+j)=train[i][j];
    		*(train_3+i*192+j)=train[i][j];
    	}
    	labVal_1[i]=label[i];
    	labTra_2[i]=label[i];
    	labTra_3[i]=label[i];
    }
    for(i=2223;i<4446;i++){
    	for(j=0;j<192;j++){
    		*(validation_2+(i-2223)*192+j)=train[i][j];
    		*(train_1+(i-2223)*192+j)=train[i][j];
    		*(train_3+i*192+j)=train[i][j];
    	}
    	labVal_2[i-2223]=label[i];
    	labTra_1[i-2223]=label[i];
    	labTra_3[i]=label[i];
    }
    for(i=4446;i<6669;i++){
    	for(j=0;j<192;j++){
            *(validation_3+(i-4446)*192+j)=train[i][j];
    		*(train_1+(i-2223)*192+j)=train[i][j];
    		*(train_2+(i-2223)*192+j)=train[i][j];
    	}
    	labVal_3[i-4446]=label[i];
        labTra_1[i-2223]=label[i];
    	labTra_2[i-2223]=label[i];
    }
    int r,k;
    float error[3][25];
    for(r=0;r<3;r++){
    	for(k=0;k<25;k++){
    		error[r][k]=0.0;
    	}
    }
    for(r=0;r<3;r++){
    		if(r==0){
    			error_calculate(validation_1,train_1,error,labVal_1,labTra_1,r,25);
    		}
    		if(r==1){
    			error_calculate(validation_2,train_2,error,labVal_2,labTra_2,r,25);
    		}
    		if(r==2){
    			error_calculate(validation_3,train_3,error,labVal_3,labTra_3,r,25);
    		}
    }
    float err_mean[25],sum;
    for(k=0;k<25;k++){
    	sum=0.0;
    	for(r=0;r<3;r++){
    		sum = sum+error[r][k];
    	}
    	err_mean[k]=sum/3.0;
    }

    for(k=0;k<25;k++){
    	printf("%f ",err_mean[k]);
    }
    printf("\n\n");
    float min = INT_MAX*1.0;
    int ind;
    for(k=0;k<25;k++){
        if(err_mean[k]<min){
            min = err_mean[k];
            ind =k;
        }
    }
    printf("final K-Value:%d",ind+1);

    FILE *f2 = fopen("pp_tes.dat", "r"); 
  
    int test[3333][192];
    int label_tes[3333];

    for(i=0;i<3333;i++){
        for(j=0;j<192;j++){
            fscanf(f2,"%d",&test[i][j]);
        }
        fscanf(f2,"%d",&label_tes[i]);
    }
   accurate(test,train,label_tes,label,ind+1);
	return 0;	
}
