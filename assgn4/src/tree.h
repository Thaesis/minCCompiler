#ifndef TREE_H
#define TREE_H

#include <../src/strtab.h>

#define MAXCHILDREN 100

struct TableNode;

typedef struct TreeNode Tree;

/* tree node - you may want to add more fields */
struct TreeNode {
      int nodeKind;
      int numChildren;
      int val;
      struct TableNode* scope; // Used for var/id. Index of the scope. This works b/c only global and local.
      int type;
      int sym_type; // Only used by var to distinguish SCALAR vs ARRAY
      Tree *parent;
      Tree *children[MAXCHILDREN];
};

extern Tree *ast; /* pointer to AST root */


/* builds sub tree with zeor children  */
Tree* makeTree(int kind);

/* builds sub tree with leaf node */
Tree* makeTreeWithVal(int kind, int val);

/*Builds sub-tree with the type param. Useful for expr nodes*/
Tree* makeTreeWithType(int kind, int type);

Tree* makeTreeWithTypeVal(int kind, int type, int val);

void addChild(Tree *parent, Tree *child);

void printAst(Tree *root, int nestLevel);

/*
@brief Function to count number of nodes with a given type in a tree/subtree.

This function uses a DFS preorder-traversal algorithm to count the number of elements in the tree
with a given type. This is specifically helpful in counting the elements passed by argList in 
the parser, as we need to check the count of arguments passed by a function call. This function makes
the assumption that the nodeType you are looking for is deeper into the tree, or rather, exists in the given
tree.

@param root The root of a given Tree* object
@param nodeType The enumerated type to be counted in the tree, derived from enum node_type in parser.y

@return The count of matching nodes in the tree.
*/
int countChildren(Tree* root, int nodeType);

void getArgTypesHelper(Tree* root, int nodeType, int* argTypes, int* index);

int* getArgTypes(Tree* root, int nodeType, int numArgs);
/* tree manipulation macros */
/* if you are writing your compiler in C, you would want to have a large collection of these */

#define nextAvailChild(node) node->children[node->numChildren]
#define getChild(node, index) node->children[index]
#define getNodeKind(node) node->nodeKind

#endif
