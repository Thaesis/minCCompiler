#ifndef TREE_H
#define TREE_H

#define MAXCHILDREN 12

typedef struct TreeNode Tree;

struct TreeNode {
      int kind;
      int data;
      int numChildren;
      Tree* children[MAXCHILDREN];
      Tree* parent;
};

extern Tree* ast; /* pointer to AST root */

/* builds sub tree with zero children  */
Tree* makeTree(int kind);

/* builds sub tree with leaf node. Leaf nodes typically hold a value. */
Tree* makeTreeWithVal(int kind, int val);

/* assigns the subtree rooted at 'child' as a child of the subtree rooted at 'parent'. Also assigns the 'parent' node as the 'child->parent'. */
void addChild(Tree* parent, Tree* child);
      
/* prints the ast recursively starting from the root of the ast.
   This function prints the warning "undeclared variable" or <nodeKind, value> for identifiers and integers, or <nodeKind, type name> for typeSpecifiers, and <nodeName, 
   the operator symbol> for relational and arithmetic operators. For more information, take a look at the example in the "Sample Output" section of the assignment instructions.*/
void printAst(Tree* root, int nestLevel);

/* Helper Function to Print AST Indentation*/
void printIndentation(int nestLevel);

//int existsIn(char* lhs, char* rhs[]);

#define getChild(node, i) ((i < node->numChildren) ? node->children[i] : NULL)
#define nextAvailChild(node) node->children[node->numChildren]

#endif