#include "structs.h"

void initLineReader() {
    indReader = 0;

    lineReader[0] = (char*)malloc(sizeof(char) * 50);
    lineReader[1] = (char*)malloc(sizeof(char) * 50);
    lineReader[2] = (char*)malloc(sizeof(char) * 10);
    lineReader[3] = (char*)malloc(sizeof(char) * 3);
    lineReader[4] = (char*)malloc(sizeof(char) * 20);
    lineReader[5] = (char*)malloc(sizeof(char) * 10);
    lineReader[6] = (char*)malloc(sizeof(char) * 50);
    lineReader[7] = (char*)malloc(sizeof(char) * 4);
}

LPersonalData* createLPersonalData() {
    LPersonalData* pd = (LPersonalData*)malloc(sizeof(struct data));
    pd->destination = (char*)malloc(sizeof(char) * 50);
    pd->name = (char*)malloc(sizeof(char) * 50);
    pd->occupation = (char*)malloc(sizeof(char) * 20);
    pd->gender = (char*)malloc(sizeof(char) * 10);
    pd->source = (char*)malloc(sizeof(char) * 50);

    strcpy(pd->destination, lineReader[0]);
    strcpy(pd->name, lineReader[1]);
    pd->id = atoi(lineReader[2]);
    pd->age = atoi(lineReader[3]);
    strcpy(pd->occupation, lineReader[4]);
    strcpy(pd->gender, lineReader[5]);
    strcpy(pd->source, lineReader[6]);
    pd->departureYear = atoi(lineReader[7]);

    pd->next = NULL;

    indReader = 0;

    return pd;
}

void insertHTCountries(LPersonalData* pd) {
    LPersonalData* old = (LPersonalData*)g_hash_table_lookup(countries, pd->destination);
    pd->next = old;

    g_hash_table_insert(countries, pd->destination, pd);
}

Events* createEvent(int id, char* name) {
    Events* ev = (Events*)malloc(sizeof(struct events));
    ev->name = (char*)malloc(sizeof(char) * 50);
    ev->ops = NULL;

    ev->id = id;
    strcpy(ev->name, name);

    indReader = 0;

    return ev;
}

void insertHTEvents(Events* ev) {
    gint* key = g_new(gint, ev->id);
    *key = ev->id;

    g_hash_table_insert(ids, key, ev);
}

LOperations* createLOperations() {
    LOperations* op = (LOperations*)malloc(sizeof(struct operations));
    op->oper = (char*)malloc(sizeof(char) * 10);
    op->description = (char*)malloc(sizeof(char) * 50);

    strcpy(op->oper, lineReader[1]);
    strcpy(op->description, lineReader[2]);
    op->next = NULL;

    indReader = 0;

    return op;
} 

void insertHTOperations(int id, LOperations* op) {
    gint* key = g_new(gint, id);
    *key = id;

    Events* ev = (Events*)g_hash_table_lookup(ids, key);
    if (ev != NULL) {
        if (ev->ops != NULL) {
            op->next = ev->ops;
            ev->ops = op;
        } else {
            ev->ops = op;
        }
    }
    
    g_hash_table_insert(ids, key, ev);
}

void printHTCountries(gpointer key, gpointer value, gpointer user_data) {
    LPersonalData* pd = (LPersonalData*)value;
    FILE* f = NULL;
    FILE* f2 = NULL;
    char* filenameDOT = (char*)malloc(sizeof(char) * 100);
    char* filenameSVG = (char*)malloc(sizeof(char) * 100);
    char s[20];
    char* fname = (char*)malloc(sizeof(char) * 20);

    if (pd != NULL) {
        //printf("----------------%s----------------\n", pd->destination);
        f = fopen("dot_files/countries.dot", "a");
        fprintf(f, "\t\"%s\" [URL=\"%s.svg\"];\n", pd->destination, pd->destination);
        fclose(f);

        strcpy(filenameDOT, "dot_files/");
        strcpy(filenameSVG, "svg_files/");
        strcat(filenameDOT, pd->destination);
        strcat(filenameSVG, pd->destination);
        strcat(filenameDOT, ".dot");
        strcat(filenameSVG, ".svg");

        f2 = fopen(filenameDOT, "a");
        fprintf(f2, "digraph G {\n\tlayout=\"circo\"\n");
    }

    while (pd != NULL) {
        //printf(":: %s\n-> %d\n-> %d\n-> %s\n-> %s\n-> %s\n-> %d\n", pd->name, pd->id, pd->age, pd->occupation, pd->gender, pd->source, pd->departureYear);
        //printf("\n");
        fprintf(f2, "\t\"%s\" -> \"%s (%d)\"\n", pd->destination, pd->name, pd->departureYear);
        fprintf(f2, "\t\"%s (%d)\" [URL=\"%d.svg\"];\n", pd->name, pd->departureYear, pd->id);
    
        sprintf(s, "%d", pd->id);
        
        strcpy(fname, "dot_files/");
        strcat(fname, s);
        strcat(fname, ".dot");

        FILE* f3 = fopen(fname, "a");

        fprintf(f3, "digraph G {\n\tlayout=\"circo\"\n");
        fprintf(f3, "\t\"Info\" -> \"%d\"\n", pd->id);
        fprintf(f3, "\t\"Info\" -> \"%d\"\n", pd->age);
        fprintf(f3, "\t\"Info\" -> \"%s\"\n", pd->occupation);
        fprintf(f3, "\t\"Info\" -> \"%s\"\n", pd->gender);
        fprintf(f3, "\t\"Info\" -> \"%s\"\n", pd->source);
        fprintf(f3, "\t\"Info\" -> \"%d\"\n", pd->departureYear);
        fclose(f3);

        pd = pd->next;

        memset(fname, 0, 20);
    }


    fprintf(f2, "}");
    fclose(f2);

    char* cmd = (char*)malloc(sizeof(char) * 150);
    strcpy(cmd, "dot -Tsvg -o ");
    strcat(cmd, filenameSVG);
    strcat(cmd, " ");
    strcat(cmd, filenameDOT);

    int x;
    if ( ( x = fork() ) == 0 ) {
        execlp("dot", "dot", "-Tsvg", "-o", filenameSVG, filenameDOT, NULL);
    } 

    free(filenameDOT);
    free(filenameSVG);
    free(fname);
    free(cmd);
}

void printHTOperations(gpointer key, gpointer value, gpointer user_data) {
    Events* ev = (Events*)value;
    LOperations* ops = ev->ops;
    FILE* f = NULL;
    char* a = (char*)malloc(sizeof(char) * 20);
    char* b = (char*)malloc(sizeof(char) * 20);
    char s[20];

    if (ev != NULL) {
        //printf("----------------%d -> %s----------------\n", ev->id, ev->name);
        

        sprintf(s, "%d", ev->id);
		
        strcpy(a, "dot_files/");
        strcpy(b, "svg_files/");
        strcat(a, s);
        strcat(b, s);
        strcat(a, ".dot");
        strcat(b, ".svg");

        f = fopen(a,"a");
    }

    while (ops != NULL) {
        //printf(":: op->%s\n description->%s\n", ops->oper, ops->description);
        fprintf(f, "\t\"%s\" -> \"%s\" [label=\"%s\"];\n", ev->name, ops->description, ops->oper);
        ops = ops->next;
    }

    fprintf(f,"}\n");
    fclose(f);
    
    int x;
    if ( ( x = fork() ) == 0 ) {
        execlp("dot", "dot", "-Tsvg", "-o", b, a, NULL);
    }

    free(a);
    free(b);
}

void initFile() {
    FILE* f = fopen("dot_files/countries.dot","a");
    fprintf(f,"digraph G {\n\tlayout=\"circo\"\n");
    fclose(f);
}

void endFile() {
    FILE* f = fopen("dot_files/countries.dot","a");
    fprintf(f,"}\n");
    fclose(f);
}
