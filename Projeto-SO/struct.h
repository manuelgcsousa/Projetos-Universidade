typedef struct connects {
	int node;
	struct connects *next;
} *Connects;

typedef struct infoctrl {
	char* pipeNames[25];
	Connects connections[25];
} *InfoCtrl;