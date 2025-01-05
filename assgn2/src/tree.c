#include<tree.h>
#include<strtab.h>
#include<stdio.h>
#include<stdlib.h>
#include<string.h>

char *nodeTypes[33] = {"program", "declList", "decl", "arrayDecl", "varDecl",
                      "funcTypeName", "funcDecl", "formalDeclList", "formalDecl",
                      "funcBody", "localDeclList", "stmtList", "stmt", "compStmt", "assignStmt",
                      "condStmt", "elseStmt", "loopStmt", "returnStmt", "var", "expr",
                      "relOp", "addExpr", "addOp", "term", "mulOp", "factor",
                      "funcCall", "argList", "identifier", "integer", "char", "typeSpecifier"};

char* opTypes[10] = {"+", "-", "*", "/", "<", "<=", "==", ">", ">=", "!="};

char* nodeDataType[3] = {"int", "char", "void"};

Tree* ast = NULL;

/* Builds the sub-tree with zero children*/
Tree* makeTree(int kind) {
    Tree* this = (Tree*)malloc(sizeof(struct TreeNode));

    this->kind = kind;
    this->numChildren = 0;
    return this;
}

/*Builds the sub-tree with a single leaf node. Leaf nodes typically hold a value*/
Tree* makeTreeWithVal(int kind, int val) {
    Tree* this = (Tree*)malloc(sizeof(struct TreeNode));
    this->kind = kind;
    this->numChildren = 0;
    this->data = val;
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


void printAst(Tree* node, int nestLevel) {

    char* nodeType = nodeTypes[node->kind];

    if(node == NULL) return;
  
    if(strncmp(nodeType, "typeSpecifier", 13) == 0) {
        printf("%s, %s\n", nodeType, nodeDataType[node->data]);
    }
    else if (strncmp(nodeType, "identifier", 10) == 0) {
        printf("%s, %s\n", nodeType, strTable[node->data].id);
    }
    else if (strncmp(nodeType, "integer", 7) == 0) {
        printf("%s, %d\n", nodeType, node->data);
    }
    else if (strncmp(nodeType, "addOp", 5) == 0) {
        printf("%s, %s\n", nodeType, opTypes[node->data]);
    }
    else if (strncmp(nodeType, "mulOp", 5) == 0) {
        printf("%s, %s\n", nodeType, opTypes[node->data]);
    }
    else if (strncmp(nodeType, "relOp", 5) == 0) {
        printf("%s, %s\n", nodeType, opTypes[node->data]);
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