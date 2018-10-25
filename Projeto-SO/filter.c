#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/wait.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>


void da2e4Col(int* inicio, int* fim, char buffer[256], int col1, int col2){
	//printf("Buffer = %s\n",buffer );
	int a = 0;
	int Sc = 0;
	int Qc = 0;
	int pontos = 0;
	char Scol[10];
	char Qcol[10];

	while(buffer[a]!='\n'){
		if (buffer[a]==':') pontos++;
		if(pontos == (col1-1) && buffer[a]!=':'){
			Scol[Sc]=buffer[a];
			Sc++;
		}
		if(pontos == (col2-1) && buffer[a]!=':'){
			Qcol[Qc]=buffer[a];
			Qc++;
		}
		a++;
	}
	Scol[Sc]='\0';
	Qcol[Qc]='\0';

	*inicio = atoi(Scol);
	*fim = atoi(Qcol);
}

int isOperator(int inicio, int fim, char* operator){

	if(strcmp(operator,"=")==0){
		if(inicio == fim) return 1;
	}

	if(strcmp(operator,">=")==0){
		if(inicio >= fim) return 1;
	}

	if(strcmp(operator,"<=")==0){
		if(inicio <= fim) return 1;
	}

	if(strcmp(operator,">")==0){
		if(inicio > fim) return 1;
	}

	if(strcmp(operator,"<")==0){
		if(inicio < fim) return 1;
	}

	if(strcmp(operator,"!=")==0){
		if(inicio != fim) return 1;
	}

	return 0;
}


void filter(char* col1, char* col2, char* operator){
	//printf("col1 -> %s, col2 -> %s\n",col1, col2 );
	int inicio;
	int fim;
	int r;
	char buffer[256];
	memset(buffer,0,256);
	char c;
	int i = 0;

	while((r=read(0,&c,1)) >=  0){

		buffer[i] = c;
		i++;

		if(c == '\n'){
			printf("BUFFER_FILTER = %s",buffer);
			da2e4Col(&inicio, &fim, buffer, atoi(col1), atoi(col2));
			//printf("Inicio = %d\n",inicio);
			//printf("FIm = %d\n",fim );
		
			if(isOperator(inicio, fim, operator)){
				write(1,buffer,i);
			}
			memset(buffer,0,256);
			i = 0;
		}
	}

}

int main(int argc, char** argv){
	filter(argv[1],argv[3],argv[2]);
	return 0;
}
