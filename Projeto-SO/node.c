#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <fcntl.h> /* O_RDONLY, O_WRONLY, O_CREAT, O_* */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "struct.h"


int filedescriptor;

int strl(char buffer[256]){
	int i;
	for(i = 0; buffer[i] != '\n'; i++);
	return i + 1;
}

int qualPosicao(int connections[10]){
	int i;
	for(i = 0; connections[i] != 0; i++);

	return i;

}

int qualNode(char * buffer){
	char aux[10];
	int i;

	for(i = 0; buffer[i] >= 48 && buffer[i] <= 57; i++){
		aux[i] = buffer[i];
	}
	aux[i]='\0';

	int ret = atoi(aux);
	return ret;

}

int nodeToDisconnect(char buffer[256]){
	int i;
	char node[5];
	for(i = 0; buffer[i] >= 48 && buffer[i] <= 57; i++){
		node[i] = buffer[i];
	}

	int ret = atoi(node);
	return ret;
}

int posToDisconnect(int qNode, int connections[10]){
	int i;
	
	for(i = 0; connections[i] != qNode && i < 10; i++);

	return i;
}

void readcmd(char* argv2, char cmd[25]){
	int i;
	int a = 2;
	for(i = 0; argv2[i] != ' '; i++){
		cmd[a] = argv2[i];
		a++;
	}
	cmd[a] = '\0';
}

void readArgs(char* argv2, char* args[10]){
	int i;
	int a = 0;
	int k = 1;
	for(i = 0; argv2[i] != '\n'; i++){

		if(argv2[i] != ' ' && argv2[i] != '\n'){
			args[k][a] = argv2[i];
			a++;
		}
		else{
			args[k][a] = '\0';
			//printf("bufs = %s\n",args[k] );
			k++;
			a = 0;
		}
		//printf("args[%d] = %s\n",k,args[k] );
	}
	k++;
	args[k] = NULL;
}

void metezeros(int nodeCons[10]) {
	int i = 0;
	for(;i < 10; i++) {
		nodeCons[i] = 0;
	}
}

void nodeCode(char* node, char* argv2){
	int r;
	char buffer[256];
	memset(buffer,0,256);
	char flag;
	int i = 0;

	char cmd[25];
	cmd[0] = '.';
	cmd[1] = '/';
	readcmd(argv2, cmd);

	char* args[10];
	int m;
	for(m = 0; m < 10; m++){
		args[m] = (char*)malloc(sizeof(char) * 25);
	}
	strcpy(args[0],cmd);

	readArgs(argv2 + strlen(cmd) - 1, args);

	//int a;
	//for(a = 0; args[a] != NULL; a++) printf("args %d = %s\n",a + 1, args[a] );

	//Pipe para receber os args do stdin do node e executar a componente	
	int fdin[2];
	int pin = pipe(fdin);
	if (pin < 0) perror("Nao abriu o pipe de entrada de dados");

	//Pipe para escrever para os outros nodes
	int fdout[2];
	int pout = pipe(fdout);
	if(pout < 0) perror ("Nao abriu pipe de saida de dados");

	//Processo que recebe no stdin os dados passados pelo programa principal e executa a componente
	int fkin = fork();
	if(fkin == 0){
		dup2(fdin[0],0);
		dup2(fdout[1],1);
		close(fdin[0]);
		close(fdin[1]);
		close(fdout[0]);
		close(fdout[1]);
		execvp(cmd,args);
	}

	int fkout = fork();
	if (fkout == 0){
		dup2(fdout[0],0);
		close(fdin[0]);
		close(fdin[1]);
		close(fdout[0]);
		close(fdout[1]);

		int connections[10];
		char buffer[256];
		memset(buffer,0,256);
		char c;
		int r;
		int i = 0;
		metezeros(connections);

		while((r = read(0,&c,1)) >= 0){
			buffer[i] = c;
			i++;
			
			if(c == '\n'){
				printf("node = %s -> Buffer pipe = %s\n",node, buffer );
				if(buffer[0] == 'c'){

					int qNode = qualNode(buffer + 1);
					int qPosicao = qualPosicao(connections);
					connections[qPosicao] = qNode;
			
					
					//printf("Connect = %s\n",buffer );
					//printf("qNode = %d\n",qNode );
					//printf("qPosicao = %d\n", qPosicao );
			
				}
	
				else if(buffer[0] == 'd'){
					
					int qNode = nodeToDisconnect(buffer + 1);
					int qPosicao = posToDisconnect(qNode, connections);
					connections[qPosicao] = 0;

					//printf("Disconnect = %s\n",buffer );
					//printf("node to disconnect = %d\n",qNode);
					//printf("pos to disconnect = %d\n", qPosicao);
				}
				else{
					//printf("Node = %s -> Buffer Recebida no stdin = %s\n",node, buffer);
					int k;
					for(k = 0; k < 10; k++){
						if(connections[k] != 0){
							char con[5];
							sprintf(con,"%d",connections[k]);
							int fdopen = open(con,O_WRONLY, 0666);
							if(fdopen < 0) perror("Nao abriu bem o pipe");
							int w = write(fdopen, buffer, strl(buffer));
							if(w < 0) perror("Não escreveu para o pipe do outro nodo");
							close(fdopen);
						}
					}
				}
				
				//int k;
				//for(k = 0; k < 10; k++) printf("%d ",connections[k] );
				//printf("\n");
				
				memset(buffer, 0, 256);
				i = 0;
			}
		}

		exit(0);
	}

	close(fdin[0]);
	close(fdout[0]);

	while(1){
		while((r = read(filedescriptor, &flag, 1)) > 0){

			buffer[i] = flag;
			i++;

			if (flag == '\n'){
				//printf("Node = %s -> Buffer recebida = %s",node, buffer );
				if(buffer[0] == 'c'){
					int w = write(fdout[1],buffer,strl(buffer));
					if(w < 0) perror("Nao ligou o node de connect");

				}
	
				else if(buffer[0] == 'd'){
					int w = write(fdout[1],buffer,strl(buffer));
					if(w < 0) perror("Nao ligou o node de disconnect");
				}

				else{
					int w = write(fdin[1],buffer,strl(buffer));
					if (w < 0) perror("Nao Escrever para o fork");
				}

			memset(buffer,0,256);
			i = 0;
			}
		}
	}
}

int main(int argc, char* argv[]) {
	filedescriptor = open(argv[1], O_RDONLY, 0666);
	//printf("%s -> filedescriptor = %d\n",argv[1], filedescriptor );
	
	//int connections[10];
	//metezeros(connections);

	nodeCode(argv[1],argv[2]);
	
	//int i;
	//for(i = 0; i < 10; i++) printf("%s -> i[%d] = %d\n",argv[1], i, connections[i] );

	return 0;
}