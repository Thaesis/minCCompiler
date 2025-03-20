#ifndef STRTAB_H
#define STRTAB_H
#define MAXIDS 1000

enum data_type {INT_TYPE, CHAR_TYPE, VOID_TYPE};
enum symbol_type {SCALAR, ARRAY, FUNCTION};

typedef struct TableNode TableNode;
struct Tree;

typedef struct Param{
    int data_type;
    int symbol_type;
    char* id;
    struct Param* next;
} Param;

typedef struct StrEntry{
    char* id;
    int value;
    int scope;
    int   data_type;
    int   symbol_type;
    int   size; //Num elements if array, num params if function
    Param*  params;
} SymEntry;

typedef struct TableNode{
    SymEntry* strTable[MAXIDS];
    int numChildren;
    struct TableNode* parent;
    struct TableNode* first_child; // First subscope
    struct TableNode* last_child;  // Most recently added subscope
    struct TableNode* next; // Next subscope that shares the same parent
} TableNode; // Describes each node in the symbol table tree and is used to implement a tree for the nested scope as discussed in lecture 13 and 14.

extern TableNode* currentScope;
extern TableNode* globalScope;


/* Inserts a symbol into the current symbol table tree. Please note that this function is used to instead into the tree of symbol tables and NOT the AST. Start at the returned hash and probe until we find an empty slot or the id.  */
int ST_insert(char *id, enum data_type dataType, enum symbol_type symbolType, int scope, int size, int val);

/* The function for looking up if a symbol exists in the current_scope. Always start looking for the symbol from the node that is being pointed to by the current_scope variable*/
int ST_lookup(char *id);

/* Creates a param* whenever formalDecl in the parser.y file declares a formal parameter. Please note that we are maining a separate linklist to keep track of all the formal declarations because until the function body is processed, we will not know the number of parameters in advance. Link list provides a way for the formalDecl to declare as many parameters as needed.*/
void add_param(int dataType, int symbolType, char* id);

/*connect_params is called after the funBody is processed in parser.y. At this point, the parser has already seen all the formal parameter declaration and has built the entire list of parameters to the function. This list is pointed to by the working_list_head pointer. current_scope->parent->strTable[index]->params should point to the header of that parameter list. The first argument `index' is the index into the current_scope->parent's symbol table*/
void connect_params(int index);

int* getParamTypes(Param* params, int size);

// Creates a new scope within the current scope and sets that as the current scope.
void new_scope();

// Moves towards the root of the sym table tree.
TableNode* up_scope();

// Retrieves the scope in which the string can be found.
TableNode* getScope();

void print_sym_tab();

#define getScopeIndex() (globalScope->numChildren - 1)

#endif
