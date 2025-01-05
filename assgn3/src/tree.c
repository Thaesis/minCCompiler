#include<tree.h>
#include<strtab.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

/* extern-defined in tree.h */
Tree *ast;

/* string values for ast node types, makes tree output more readable */
char *nodeTypes[33] = {"program", "declList", "decl", "arrayDecl", "varDecl",
                      "funcTypeName", "funcDecl", "formalDeclList", "formalDecl",
                      "funcBody", "localDeclList", "stmtList", "stmt", "compStmt", "assignStmt",
                      "condStmt", "elseStmt", "loopStmt", "returnStmt", "var", "expr",
                      "relOp", "addExpr", "addOp", "term", "mulOp", "factor",
                      "funcCall", "argList", "identifier", "integer", "char", "typeSpecifier"};

char *nodeDataType[3] = {"int", "char", "void"};
char *opTypes[10] = {"+", "-", "*", "/", "<", "<=", "==", ">", ">=", "!="};

Tree* makeTree(int kind) {
    Tree* this = (Tree *) malloc(sizeof(struct TreeNode));
    this->nodeKind = kind;
    this->numChildren = 0;
    this->scope = currentScope;
    return this;
}

Tree* makeTreeWithVal(int kind, int val) {
    Tree* this = (Tree*)malloc(sizeof(struct TreeNode));
    this->nodeKind = kind;
    this->numChildren = 0;
    this->val = val;
    this->scope = currentScope;
    return this;
}

Tree* makeTreeWithType(int kind, int type) {
    Tree* this = (Tree*)malloc(sizeof(struct TreeNode));
    this->nodeKind = kind;
    this->numChildren = 0;
    this->type = type;
    this->scope = currentScope;
    return this;
}

Tree* makeTreeWithTypeVal(int kind, int type, int val) {
    Tree* this = (Tree*)malloc(sizeof(struct TreeNode));
    this->nodeKind = kind;
    this->numChildren = 0;
    this->type = type;
    this->val = val;
    this->scope = currentScope;
    return this;
}

void addChild(Tree* parent, Tree* child) {

    if(parent->numChildren == MAXCHILDREN) {
        fprintf(stderr, "Error: Max Children Exceeded in Tree\n");
        exit(1);
    }

    nextAvailChild(parent) = child;
    parent->numChildren++;

    child->parent = parent;
}

int countChildren(Tree* root, int nodeType) {

    if(root == NULL) {
        return 0;
    }

    int count = 0;

    if(root->nodeKind == nodeType) {
        count = 1;
    }

    for(int i = 0; i < root->numChildren; i++) {
        count += countChildren(root->children[i], nodeType);
    }
    
    return count;
}

void getArgTypesHelper(Tree* node, int nodeType, int* argTypes, int* index) {
    if (node == NULL) return;

    // Add the node's type to the array if it matches nodeType
    if (node->nodeKind == nodeType) {
        argTypes[*index] = node->type;
        (*index)++;
    }

    // Recurse for each child
    for (int i = 0; i < node->numChildren; i++) {
        getArgTypesHelper(node->children[i], nodeType, argTypes, index);
    }
}

int* getArgTypes(Tree* root, int nodeType, int numArgs) {

    if (numArgs == 0) return NULL;

    // Allocate array for the argument types
    int* argTypes = malloc(numArgs * sizeof(int));
    if (argTypes == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(EXIT_FAILURE);
    }
 
    // Populate the types array
    int index = 0;
    getArgTypesHelper(root, nodeType, argTypes, &index);
    
    return argTypes;
}

void printAst(Tree* node, int nestLevel) {

    char* nodeType = nodeTypes[node->nodeKind];

    if(node == NULL) return;
  
    if(strncmp(nodeType, "typeSpecifier", 13) == 0) {
        printf("%s, %s\n", nodeType, nodeDataType[node->val]);
    }
    else if (strncmp(nodeType, "identifier", 10) == 0) {
        if(node->scope->strTable[node->val] == NULL) {
            printf("%s, %s\n", nodeType, node->scope->parent->strTable[node->val]->id);
        } else {
            printf("%s, %s\n", nodeType, node->scope->strTable[node->val]->id);
        }
    }
    else if (strncmp(nodeType, "integer", 7) == 0) {
        printf("%s, %d\n", nodeType, node->val);
    }
    else if (strncmp(nodeType, "addOp", 5) == 0) {
        printf("%s, %s\n", nodeType, opTypes[node->val]);
    }
    else if (strncmp(nodeType, "mulOp", 5) == 0) {
        printf("%s, %s\n", nodeType, opTypes[node->val]);
    }
    else if (strncmp(nodeType, "relOp", 5) == 0) {
        printf("%s, %s\n", nodeType, opTypes[node->val]);
    }
    else {
        printf("%s\n", nodeType);
    }

    int i, j;

    for (i = 0; i < node->numChildren; i++)  {
        for (j = 0; j < nestLevel; j++) 
        printf("  ");
        printAst(getChild(node, i), nestLevel + 1);
    }

}


