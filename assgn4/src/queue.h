#ifndef QUEUE_H
#define QUEUE_H

#include <../src/strtab.h>

#define MAX_ELEMENTS 10

typedef struct Node{
    TableNode* symbolTable;
    struct Node* next;
} Node;

typedef struct {
    Node* front;
    Node* back;
} Queue;

/*
@brief Initalizes a Queue* object to NULL

This function initalizes a declared struct Queue* object to NULL on Queue->front and Queue->back

@param q the Queue* object to initalize.
*/
void init(Queue* q);

/*
@brief Checks if the Queue is empty.

This function checks to see if the underlying linked-list structure is NULL and holds no further elements.

@param q The Queue* object to evaluate for emptiness.

@return A boolean type integer, -1 if not empty. 
*/
int isEmpty(Queue* q);

/*
@brief Adds a TableNode* object to the back of the Queue* object.

This function adds a TableNode* object defined by strTab.h, to the back of the Queue* object.
The use of TableNode* is required for BFS navigation through the Tree of Symbol Tables to effectively
print the tree of symbol tables.

@param q The Queue* object to add the TableNode* object to the back of.
@param table The TableNode* object, defined in StrTab.h
*/
void enqueue(Queue* q, TableNode* table);

/*
@brief Removes the TableNode* object at the front of the Queue* object.

@param q The Queue* object to remove the front of.

@return Node* object from the front of the Queue* object.
*/
Node* dequeue(Queue* q);

#endif