#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stddef.h>
#include<../src/strtab.h>
#include<../src/queue.h>
#include<../src/tree.h>

/* You should use a linear linklist to keep track of all parameters passed to a function. 
The working_list_head should point to the beginning of the linklist and working_list_end should point to the end.
 Whenever a parameter is passed to a function, that node should also be added in this list. */
Param *workingListHead = NULL;
Param *workingListEnd = NULL;

TableNode* globalScope = NULL; // Pointer to the root scope for BFS of the symbolTable
TableNode* currentScope = NULL; // A global variable that should point to the symbol table node in the scope tree as discussed in lecture 13 and 14.

char* dataTypes[3] = {"int", "char", "void"};
char* symbolTypes[3] = {"Scalar", "Array", "Function"};

extern int yylineno;

unsigned long hash(unsigned char *str)
{
    unsigned long hash = 5381;
    int c;

    while (c = *str++)
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

    return hash;
}

/* Inserts a symbol into the current symbol table tree.
 Please note that this function is used to insert into the tree of symbol tables and NOT the AST. 
 Start at the returned hash and probe until we find an empty slot or the id.  */
int ST_insert(char* id, enum data_type dataType, enum symbol_type symbolType, int scope, int size, int value) {
    
   unsigned int index = hash(id) % MAXIDS;
   SymEntry** currentTable = currentScope->strTable;

   while(currentTable[index] != NULL) {
        if(strcmp(currentTable[index]->id, id) == 0) {
            return index;
        }

        index = (index + 1) % MAXIDS;

        if(index == hash(id) % MAXIDS) {
            fprintf(stderr, "Symbol Table Full.");
            return -1;
        }
   }

   currentTable[index] = malloc(sizeof(SymEntry));
   if(currentTable[index] == NULL) {
        free(currentTable[index]);
        fprintf(stderr, "Malloc Fail.");
        return -1;
   }

   currentTable[index]->id = malloc(strlen(id) + 1);

    strncpy(currentTable[index]->id, id, strlen(id) + 1);
    currentTable[index]->data_type = dataType;
    currentTable[index]->symbol_type = symbolType;
    currentTable[index]->scope = scope;
    currentTable[index]->size = size;
    currentTable[index]->value = value;
    currentTable[index]->params = NULL;

    return index;
}

/* The function for looking up if a symbol exists in the current_scope. 
Always start looking for the symbol from the node that is being pointed to by the current_scope variable*/
int ST_lookup(char* id) {

    TableNode* nodeIter = currentScope;
    unsigned int index = hash(id) % MAXIDS;
    int isGlobalScope = 0;

    // Check the current local scope and global scope.
    while(nodeIter != NULL) {

        SymEntry** currentTable = nodeIter->strTable;

        unsigned int startIndex = index;
        while(currentTable[index] != NULL) {
            if(strcmp(currentTable[index]->id, id) == 0) {
                //Need a way to indicate that the index was found outside of local scope. Will unpack upon call in parser.y
                return isGlobalScope ? index + MAXIDS : index;
            }

            index = (index + 1 ) % MAXIDS;

            if(index == startIndex) {
                break;
            }
        }
        nodeIter = nodeIter->parent;
        isGlobalScope = 1;
    }

    return -1;
}

/* Creates a param* whenever formalDecl in the parser.y file declares a formal parameter.
Please note that we are maining a separate linklist to keep track of all the formal declarations because until the function body is processed,
we will not know the number of parameters in advance. Link list provides a way for the formalDecl to declare as many parameters as needed.*/
void add_param(int dataType, int symbolType, char* id) {

    Param* newParam = malloc(sizeof(Param));

    if(newParam == NULL) {
        fprintf(stderr, "Memory allocation failed for newParam\n");
        exit(EXIT_FAILURE);
    }
    
    newParam->data_type = dataType;
    newParam->symbol_type = symbolType;
    newParam->id = id;
    newParam->next = NULL;

    if(workingListHead == NULL && workingListEnd == NULL) {

        workingListHead = newParam;
        workingListEnd = newParam;

    } else {

        workingListEnd->next = newParam;
        workingListEnd = newParam;

    }
}

/*connect_params is called after the funBody is processed in parser.y. 
At this point, the parser has already seen all the formal parameter declaration and has built the entire list of parameters to the function. 
This list is pointed to by the working_list_head pointer. current_scope->parent->strTable[index]->params should point to the header of that parameter list. 
The first argument `index' is the index into the current_scope->parent's symbol table*/
void connect_params(int index) {

    int numParams = 0;
    Param* paramIter = workingListHead;
    Param* newHead = NULL;
    Param* newTail = NULL;

    // Copy the linked list to avoid freeing the data after assignment
    while (paramIter != NULL) {
        // Allocate a new Param node
        Param* newParam = malloc(sizeof(Param));
        if (newParam == NULL) {
            fprintf(stderr, "Memory allocation failed\n");
            exit(EXIT_FAILURE);
        }
        // Copy data from the original node
        newParam->data_type = paramIter->data_type;
        newParam->symbol_type = paramIter->symbol_type;
        newParam->id = paramIter->id;
        newParam->next = NULL;

        // Append to the new list
        if (newHead == NULL) {
            newHead = newParam;
            newTail = newParam;
        } else {
            newTail->next = newParam;
            newTail = newParam;
        }

        numParams++;
        paramIter = paramIter->next;
    }

    // Set params in the symbol table to the new copied list
    currentScope->parent->strTable[index]->params = newHead;
    currentScope->parent->strTable[index]->size = numParams;

    // Free the original working list
    while (workingListHead != NULL) {
        Param* temp = workingListHead;
        workingListHead = workingListHead->next;
        free(temp);
    }

    workingListHead = NULL;
    workingListEnd = NULL;
}

int* getParamTypes(Param* params, int size) {

    Param* paramIter = params;
    
    int* types = malloc(size * sizeof(int));
   
    if (types == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    for(int i = 0; i < size; i++) {

        types[i] = paramIter->data_type;
        paramIter = paramIter->next;

    }

    return types;

}

int* getParamSymbolTypes(Param* params, int size) {
    Param* paramIter = params;
    
    int* sTypes = malloc(size * sizeof(int));
   
    if (sTypes == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }

    for(int i = 0; i < size; i++) {

        sTypes[i] = paramIter->symbol_type;
        paramIter = paramIter->next;

    }

    return sTypes;
}

// Creates a new scope within the current scope and sets that as the current scope.
void new_scope() {
    TableNode* newScope = malloc(sizeof(TableNode));
    if (!newScope) {
        fprintf(stderr, "Memory allocation failed in new_scope\n");
        exit(EXIT_FAILURE); // Handle memory allocation failure
    }

    // Initialize all fields in newScope
    newScope->numChildren = 0;
    newScope->parent = NULL;
    newScope->first_child = NULL;
    newScope->last_child = NULL;
    newScope->next = NULL;

    // Check if we're creating the global scope
    if (currentScope == NULL) {
        globalScope = newScope;
        currentScope = newScope;
    } else {
        currentScope->numChildren++;
        newScope->parent = currentScope;

        // Link the new scope as the first or subsequent child
        if (currentScope->numChildren == 1) {
            currentScope->first_child = newScope;
            currentScope->last_child = newScope;
        } else {
            currentScope->last_child->next = newScope;
            currentScope->last_child = newScope;
        }

        // Move currentScope to the new scope
        currentScope = newScope;
    }

}

// Moves towards the root of the sym table tree.
TableNode* up_scope() {
  currentScope = currentScope->parent;
  return currentScope;
}

void print_sym_tab() {
    
    int funcIndex = 0;

    printf("Symbol Table:: |--\n\n");

    printf("globalScope::globalScope |--\n");

    SymEntry** currentTable = globalScope->strTable;

    for(int i = 0; i < MAXIDS; i++) {
        
        if(currentTable[i] == NULL) {
            continue;
        }

        if(currentTable[i]->symbol_type == FUNCTION) {
            funcIndex++;
            printf("    Index: %d, ID: %s, Identifier Type: Function, Return Type: %s\n", i, currentTable[i]->id, dataTypes[currentTable[i]->data_type]);
            printf("                     %s %d\n", "argCount:", currentTable[i]->size);
                
            Param* paramRoot = currentTable[i]->params;

            while(paramRoot != NULL) {
                printf("                     argType: %s, argSymbolType: %s\n", symbolTypes[paramRoot->symbol_type], dataTypes[paramRoot->data_type]);
                paramRoot = paramRoot->next;
            }

            printf("                     localScope::%s |--\n\n", currentTable[i]->id);
            
            TableNode* subNode = globalScope->first_child;

            for(int k = 1; k < currentTable[i]->scope + 1; k++) {
                subNode = subNode->next;
            }

            SymEntry** subNodeTable = subNode->strTable;
            for(int j = 0; j < MAXIDS; j++) {

                if(j == MAXIDS-1) {
                    printf("\n");
                }

                if(subNodeTable[j] == NULL) {
                    continue;
                }

                if (subNodeTable[j]->symbol_type == ARRAY) {
                
                    printf("                         Index: %d, ID: %s, Identifier Type: Array, Data Type: %s, Size: %d\n", j, subNodeTable[j]->id, dataTypes[subNodeTable[j]->data_type], subNodeTable[j]->size);

                } else {

                     printf("                         Index: %d, ID: %s, Identifier Type: Scalar, Data Type: %s\n", j, subNodeTable[j]->id, dataTypes[subNodeTable[j]->data_type]);
                }

            }
        }
        else if (currentTable[i]->symbol_type == ARRAY) {
            printf("    Index: %d, ID: %s, Identifier Type: Array, Data Type: %s, Size: %d\n", i, currentTable[i]->id, dataTypes[currentTable[i]->data_type], currentTable[i]->size);
        }
        else {
            printf("    Index: %d, ID: m%s, Identifier Type:Scalar, Data Type:%s\n", i, currentTable[i]->id, dataTypes[currentTable[i]->data_type]);
        }
    }
}
    

