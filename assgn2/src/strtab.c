#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "strtab.h"

struct strEntry strTable[MAXIDS] = {0};

/* Provided is a hash function that you may call to get an integer back. */
unsigned long hash(unsigned char *str)
{
    unsigned long hash = 5381;
    int c;

    while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

int ST_insert(char *id, char *scope, enum dataType data_type, enum symbolType symbol_type){
    // TODO: Concatenate the scope and id and use that to create the hash key
    char hashKey[strlen(id) + strlen(scope) + 2];
    strcpy(hashKey, id);
    strcat(hashKey, scope);
    unsigned long index = hash(hashKey) % MAXIDS;
    // TODO: Use ST_lookup to check if the id is already in the symbol table. If yes, ST_lookup will return an index that is not -1. if index != -1, that means the variable is already in the hashtable. Hence, no need to insert that variable again. However, if index == -1, then use linear probing to find an empty spot and insert there. Then return that index.
    if(ST_lookup(id, scope) != -1){
        return index;
    }
    while (strTable[index].id != NULL) {
        index = (index + 1) % MAXIDS; // Move to the next index
    }

    strTable[index].id = strdup(id);
    strTable[index].scope = strdup(scope);
    strTable[index].data_type = data_type;
    strTable[index].symbol_type = symbol_type;

    return index;
}

int ST_lookup(char *id, char *scope) {
    
    char hashKey[strlen(id) + strlen(scope) + 2];

    strcpy(hashKey, id);
    strcat(hashKey, scope);
    unsigned long index = hash(hashKey) % MAXIDS;
    // TODO: Use the hash value to check if the index position has the "id". If not, keep looking for id until you find an empty spot. If you find "id", return that index. If you arrive at an empty spot, that means "id" is not there. Then return -1. 
    while(strTable[index].id != NULL){
        if(strcmp(strTable[index].id, id) == 0){
            return index;
        }
        index = (index + 1) % MAXIDS;
    }
    return -1;
}

void output_entry(int i){
    printf("%d: %s ", i, strTable[i].data_type);
    printf("%s:%s%s\n", strTable[i].scope, strTable[i].id, strTable[i].symbol_type);
}

