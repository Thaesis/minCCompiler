#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<stddef.h>
#include <../src/queue.h>

void init(Queue* q) {
    q->front = NULL;
    q->back = NULL;
}

int isEmpty(Queue* q) {
    return q->front == NULL;
}

void enqueue(Queue* q, TableNode* table) {
    Node* newNode = (Node*)malloc(sizeof(Node));

    if(newNode == NULL) {
        printf("Memory Allocation Failed.\n");
        return;
    }

    newNode->symbolTable = table;
    newNode->next = NULL;

    if(isEmpty(q) == 1) {

        q->front = q->back = newNode;

    } else {

        q->back->next = newNode;
        q->back = newNode;

    }
}

Node* dequeue(Queue* q) {

    if(isEmpty(q)) { return NULL;}

    Node* temp = q->front;
    q->front = q->front->next;

    if(q->front == NULL) {
        q->back = NULL;
    }

    return temp;
    
}

 