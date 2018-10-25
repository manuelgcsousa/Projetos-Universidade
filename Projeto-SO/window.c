#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

int comp(int nLinhas, int linhasAnteriores[nLinhas]){
	int i = 0;
	while(linhasAnteriores[i]!=0)
		i++;
	return i;
}

void da2e4Col(int* inicio, char buffer[256], int col1){
	int a = 0;
	int Sc = 0;
	int pontos = 0;
	char Scol[10];

	while(buffer[a]!='\n'){
		if(buffer[a]==':') pontos++;
		if(pontos == (col1-1) && buffer[a]!=':'){
			Scol[Sc] = buffer[a];
			Sc++;
		}
		a++;
	}
	Scol[Sc]='\0';
	*inicio = atoi(Scol);
}

int isFull(int nLinhas, int linhasAnteriores[nLinhas]){
	int i = 0;
	while(linhasAnteriores[i]!=0){
		i++;
	}
	return i;
}

int average(int nLinhas, int linhasAnteriores[nLinhas]){
	int i;
	int res = 0;
	int n = 0;

	for(i=0;i<nLinhas && linhasAnteriores[i]!=0;i++){
		res += linhasAnteriores[i];
		n++;
	}

	return (res/n);
}

int maximo(int nLinhas, int linhasAnteriores[nLinhas]){
	int i;
	int max = 0;
	
	for(i=0; linhasAnteriores[i]!=0 && i<nLinhas; i++){
		if(linhasAnteriores[i] > max){
			max = linhasAnteriores[i];
		}
	}
	return max;
}

int minimo(int nLinhas, int linhasAnteriores[nLinhas]){
	int i;
	int min = 10000;

	for(i=0; linhasAnteriores[i]!=0 && i<nLinhas; i++){
		if(linhasAnteriores[i] < min){
			min = linhasAnteriores[i];
		}
	}
	return min;
}

int soma(int nLinhas, int linhasAnteriores[nLinhas]){
	int i;
	int res = 0;

	for(i = 0; linhasAnteriores[i]!=0 && i<nLinhas;i++){
		res+=linhasAnteriores[i];
	}
	return res;
}


int operacao(char* func, int nLinhas, int linhasAnteriores[nLinhas]){
	int res;
	if(strcmp(func,"avg")==0){
		res = average(nLinhas, linhasAnteriores);
	}

	if(strcmp(func,"max")==0){
		res = maximo(nLinhas, linhasAnteriores);
	}

	if(strcmp(func,"min")==0){
		res = minimo(nLinhas, linhasAnteriores);
	}

	if(strcmp(func,"sum")==0){
		res = soma(nLinhas, linhasAnteriores);
	}

	return res;
}

void window(char* coluna, char* func, int nLinhas){
	int i = 0;
	int indice = 0;
	int primeiraLinha = 0;
	int linhasAnteriores[nLinhas];
	int r;
	char buffer[256];
	char Scol[10];
	int inicio;

	for(;indice < nLinhas ; indice++){
		linhasAnteriores[indice] = 0;
	}

	indice = 0;

	while((r=read(0,buffer,256)) >= 0){

		da2e4Col(&inicio, buffer, atoi(coluna));

		if(primeiraLinha == 0){
			buffer[r-1] = ':';
			buffer[r] = '0';
			buffer[r+1] = '\n';
			write(1,buffer,r+2);
			primeiraLinha = 1;
			linhasAnteriores[indice] = inicio;
			indice++;
		} else {
			
			int op = operacao(func, nLinhas, linhasAnteriores);
			printf("%d\n",op );
			char aux[10];
			sprintf(aux,"%d",op);

			linhasAnteriores[indice] = inicio;
			indice++;
			if(indice == nLinhas) indice = 0;
			
			buffer[r-1] = ':';
			for(i = 0;aux[i]!='\0';r++){
				buffer[r] = aux[i];
				i++;
			}
			buffer[r]='\n';
			write(1,buffer,r+1);
			
		}	
	}
}

int main(int argc, char** argv){
	window(argv[1],argv[2],atoi(argv[3]));
}