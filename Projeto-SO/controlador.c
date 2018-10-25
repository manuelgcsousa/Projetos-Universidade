#include <unistd.h> /* chamadas ao sistema: defs e decls essenciais */
#include <fcntl.h> /* O_RDONLY, O_WRONLY, O_CREAT, O_* */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct connects {
	int node;
	struct connects *next;
} *Connects;

typedef struct infoctrl {
	char* pipeNames[25];
	Connects connections[25];
} *InfoCtrl;

void readNode(char* buffer, int N, char nodeName[N]) {
	int i = 0;
	while (buffer[i] != ' ' && buffer[i] != '\0' && buffer[i] != '\n') {
		nodeName[i] = buffer[i];
		i++;
	}
	nodeName[i] = '\0';
}

void createNode(char* nodeName, InfoCtrl ctrl) {
	mkfifo(nodeName,0666);
	int a = atoi(nodeName);
	char* aux = ctrl->pipeNames[a];
	strcpy(aux,nodeName);
}

void readConnections(char* buffer, int N, int nodeCons[N]) {
	int i = 0, j = 0, k = 0;
	char aux[10];
	int len = strlen(buffer) - 1;
	
	while (i < len) {
		while (buffer[i] != ' ' && i < len) {
			aux[j] = buffer[i];
			j++;
			i++;
		}
		aux[j] = '\0';
		int a = atoi(aux);
		nodeCons[k] = a;
		k++;
		j = 0;
		i++;
	}
}

void makeConnections(int nodeCons[25], InfoCtrl ctrl) {
	int i = 1;

	while (nodeCons[i] != 0) {
		Connects new = (Connects)malloc(sizeof(struct connects));
		new->node = nodeCons[i];
		new->next = NULL;

		Connects aux = ctrl->connections[nodeCons[0]];
		Connects ant = aux;

		if (aux == NULL) aux = new;
		else {
			while (aux != NULL) {
				ant = aux;
				aux = aux->next;
			}

			ant->next = new;
		}
		i++;
	}
}

void breakConnection(char* buffer, InfoCtrl ctrl) {
	int breakNodes[2];
	readConnections(buffer, 2, breakNodes);
	int node1 = breakNodes[0];
	int node2 = breakNodes[1];


	Connects aux = ctrl->connections[node1];
	Connects ant = aux;

	if (aux->node == node2) {
		ctrl->connections[node1] = ctrl->connections[node1]->next;
	}
	else {
		aux = aux->next;
		while (aux->node != node2) {
			ant = aux;
			aux = aux->next;
		}
		ant->next = aux->next;
		free(aux); 
	}
}

void readArgs(char* buffer, int N, char* argsNames[N]) {
	char aux[15];
	int i = 0;
	int a = 0;
	int k = 0;

    while (buffer[i] != '\n' && buffer[i] != '\0') {
     	while (buffer[i] != ' ' && buffer[i] != '\n' && buffer[i] != '\0') {
     		aux[a] = buffer[i];
     		a++;
     		i++;

     	}
     	if (buffer[i] == ' ') i++;
     	aux[a] = '\0';
     	a = 0;
     	strcpy(argsNames[k],aux);
     	k++;
     	memset(aux, 0, 15) ;
    }

    argsNames[k] = NULL;

    //for( i = 0; argsNames[i] != NULL; i++) {
    //	 printf("argsNames : %s\n",argsNames[i] );
    //}
}

void injectNode(char* buffer, InfoCtrl ctrl) {
	int i = 0;
	char nodeName[5];
	char cmdName[10];
	cmdName[0] = '.';
	cmdName[1] = '/';
	char* argsNames[20];

		for(i = 0; i < 20; i++) {
			argsNames[i] = (char*) malloc((sizeof(char))*10);
		}


	readNode(buffer, 2, nodeName);
	readNode(buffer + strlen(nodeName) + 1 , 10, cmdName+2);
	readArgs(buffer + strlen(nodeName) + 1 + strlen(cmdName) + 1 , 10, argsNames);

	int idPipe = atoi(nodeName);
	int fp = open(ctrl->pipeNames[idPipe], O_WRONLY, 0666);

	int x = fork();
	if(x == 0) {
		dup2(fp,1);
		close(fp);
		execvp(cmdName,argsNames);		
        perror("NAO EXECUTOU EXECVP");

	}
	wait(0L);

	close(fp);
}

void metezeros(int nodeCons[25]) {
	int i = 0;
	for(;i < 25; i++) {
		nodeCons[i] = 0;
	}
}

/*
void buildStr(char* str, int x, InfoCtrl ctrl) {
 	char strAux[5];
 	Connects aux = ctrl->connections[x];
	
	while (aux != NULL) { 
		sprintf(strAux, "%d", aux->node);
		
		int len = strlen(strAux);
		strAux[len] = ' ';
		strAux[len + 1] = '\0';
		strcat(str, strAux);
		
		aux = aux->next;
	}

	printf("%s\n", str);
}
*/

void buildNode(int node, int nodeCons[25], InfoCtrl ctrl) {
	int i;

	int pipe = open( ctrl->pipeNames[node], O_WRONLY, 0666);

	for(i = 1; nodeCons[i] != 0; i++){
		char nodes[5] = "c\0";
		char aux[5];

		sprintf(aux, "%d", nodeCons[i]);
		strcat(nodes, aux);
		int len = strlen(nodes);
		nodes[len] = '\n';

		int w = write(pipe, nodes, len + 1);

		if(w < 0) perror("ERRO!");
		
		memset(nodes, 0, 5);
		memset(aux, 0, 5);
	}

	close(pipe);
}

void destroyNode(int node, int nodeToDisconnect, InfoCtrl ctrl){

	char breakBond[5] = "d\0";
	char aux[5];

	int pipe = open(ctrl->pipeNames[node], O_WRONLY, 0666);
	if(pipe < 0) perror("NAO ABRIU PIPE PARA LER");

	//printf("abriu o pipe para disconnect %d\n", node);

	sprintf(aux, "%d", nodeToDisconnect);
	strcat(breakBond, aux);

	int len = strlen(breakBond);
	breakBond[len] = '\n';

	int w = write(pipe, breakBond, len + 1);
	if(w < 0) perror("OLA -> ERRO!");

	close(pipe);

}


void controlador(char *argv) {
	int i,x;
	InfoCtrl ctrl = (InfoCtrl)malloc(sizeof(struct infoctrl));

	for (i = 0; i < 25 ; i++){
		ctrl->connections[i] = (Connects)malloc(sizeof(struct connects)*25);
		ctrl->pipeNames[i] = (char*)malloc(sizeof(char)*25);
	}

	char buffer[256];

	int f = open(argv, O_RDWR, 0666);

	int r;
	
	char a;

	i = 0;

	while ((r = read(f, &a, 1)) > 0) {

		if (a == '\n') {
			buffer[i] = a;
			i = 0;
			
			if (strncmp(buffer, "node", 4) == 0) {
				char nodeName[25];
				readNode(buffer + 5, 25, nodeName);
				createNode(nodeName, ctrl);
				
				x = fork();
				if(!x) {
					execlp("./node", "./node", nodeName, buffer + 5 + strlen(nodeName) + 1, NULL);	
				}
				memset(buffer,0,256);	
			}
			
			if (strncmp(buffer, "connect", 7) == 0) {
				//makeConnections(nodeCons, ctrl);

				int nodeCons[25];
				metezeros(nodeCons);
				readConnections(buffer + 8, 25, nodeCons);

				buildNode(nodeCons[0], nodeCons, ctrl);

				memset(buffer,0,256);
			} 
			
			if (strncmp("disconnect", buffer, 10) == 0) {
				//breakConnection(buffer + 11, ctrl);

				int breakNodes[2];
				readConnections(buffer + 11, 2, breakNodes);
				int node = breakNodes[0];
				int nodeToDisconnect = breakNodes[1];
				
				destroyNode(node, nodeToDisconnect, ctrl);
				
				memset(buffer,0,256);
			}
			
			if (strncmp("inject", buffer, 6) == 0) {
				injectNode(buffer + 7, ctrl);
				memset(buffer,0,256);
			}	
		} 
		else {
			buffer[i] = a;
			i++;
		}

		//wait(0L);

	}

	close(f);

	//for (i = 0; i < 25 ; i++){
	//	printf("%s\n",ctrl->pipeNames[i]);
	//}

	//for(i = 0; i < 25; i++){
	//	Connects aux = ctrl->connections[i];
	//	while(aux!=NULL){
	//		printf(" pipe %d connection %d\n", i, aux->node );
	//		aux=aux->next;
	//	}
	//}

}

int main(int argc, char *argv[]) {
	controlador(argv[1]);
	return 0;
}
