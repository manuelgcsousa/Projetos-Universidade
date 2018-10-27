#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/wait.h>
#include <glib.h>

/* Structs */
typedef struct operations {
    char* oper;
    char* description;
    struct operations* next;
} LOperations;

typedef struct events {
    int id;
    char* name;
    LOperations* ops;
} Events;

typedef struct data {
    char* destination;
    char* name;
    int id;
    int age;
    char* occupation;
    char* gender;
    char* source;
    int departureYear;
    struct data* next;
} LPersonalData;

int indReader;
char* lineReader[8];

GHashTable* countries;
GHashTable* ids;


/* Functions */
void initLineReader();

void initFile();

void endFile();

LPersonalData* createLPersonalData();

void insertHTCountries(LPersonalData* pd);

Events* createEvent(int id, char* name);

void insertHTEvents(Events* ev);

LOperations* createLOperations();

void insertHTOperations(int id, LOperations* op);

void printHTCountries(gpointer key, gpointer value, gpointer user_data);

void printHTOperations(gpointer key, gpointer value, gpointer user_data);