#ifndef CODEGEN_H
#define CODEGEN_H

#include <../src/tree.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* MIPS Definitions */

#define lw "lw"         // Load
#define sw "sw"         // Store
#define li "li"         // Load-Immediate
#define la "la"         // Load-Address
#define move "move"     // Move
#define add "add"       // Add
#define mul "mul"       // Multiply
#define sub "sub"       // Subtract
#define addi "addi"     // Add-Immediate
#define bgt "bgt"       // Branch; Great-Than
#define bge "bge"       // Branch; Greater-Than-Equal
#define blt "blt"       // Branch; Less-Than
#define ble "ble"       // Branch; Less-Than-Equal
#define beq "beq"       // Branch; Equal
#define bne "bne"       // Branch; Not-Equal
#define j "j"           // Unconditional Branch
#define jal "jal"       // Jump-And-Link
#define and "and"       // AND
#define andi "andi"     // AND-Immediate
#define or "or"         // OR
#define ori "ori"       // OR-Immediate
#define xor "xor"       // XOR
#define xori "xori"     // XOR-Immediate
#define nor "nor"       // NOR
#define not "not"       // NOT
#define slt "slt"
#define sll "sll"       // Logical-Shift Left
#define sllv "sllv"     // Logical-Shift Left (Least Significant 5-bit determine amount to shift)
#define srl "srl"       // Logical-Shift Right
#define srlv "srlv"     // Same as sllv, but right.
#define rol "rol"       // Rotate left
#define ror "ror"       // Rotate Right
#define mflo "mflo"

/*  MIPS Calling Convention Definitions */

#define zero "$zero"    // Constant Value Zero
#define gp "$gp"        // Pointer to Global
#define sp "$sp"        // Stack Pointer
#define fp "$fp"        // Frame Pointer
#define ra "$ra"        // Return Address

#define MAX_VAR 100

typedef struct {
    char* data;
    size_t size;
    size_t used;
} dBuf;

typedef struct {
    char *opcode;
    int result;
    int t1;
    int t2;
    char *label;
    int imm;
} Instruction;

enum nodeTypes {PROGRAM, DECLLIST, DECL, ARRAYDECL, VARDECL,
                FUNCTYPENAME, FUNCDECL, FORMALDECLLIST, FORMALDECL,
                FUNCBODY, LOCALDECLLIST, STMTLIST, STMT, COMPSTMT, ASSIGNSTMT, 
                CONDSTMT, ELSESTMT, LOOPSTMT, RETURNSTMT, VAR, EXPR,
                RELOP, ADDEXPR, ADDOP, TERM, MULOP, FACTOR,
                FUNCCALL, ARGLIST, IDENTIFIER, INTEGER, CHARACTER,
                TYPESPEC
                };

enum opType {ADD, SUB, MUL, DIV, LT, LTE, EQ, GTE, GT, NEQ};

/*
@brief Function that inititalizes the writing buffers for the MIPS file, and begins the recursive process() call.

@param root The root of the AST.
*/
void generateMIPS(Tree *root);

/*
@brief Recursive-Helper function to traverse the AST and generate MIPS code

@param node Tree node to begin the recursive call and traverse.
@param dataBuf Dynamic Buffer object for the data portion of the MIPS code.
@param textBuf Dynamic Buffer object for the text portion of the MIPS code.
*/
void process(Tree* node);

/*
@brief Function that handles emits required for encountered expression.

@param node Tree node for expression subtree.
*/
int emitExpr(Tree* node);

/*
@brief Function that handles emits required for encountered conditional statements.

@param node Tree node for conditional statement.
*/
void emitCondStat(Tree* node);

/*
@brief Function that handles emits required for enocuntered loop statements.

@param node Tree node for loop statement.
*/
int emitLoop(Tree* node);

void emitAssignStat(Tree* node);

int emitArray(Tree* node);

/*
@brief Function that handles baseline emit requests.

@param inst Instruction object to be emited.
*/
void emit(Instruction *inst);

/*
@brief Function that handles the generation of new labels required for cond/loop stmts.
*/
char* newLabel();

/*
@brief Function that returns a validly available register for use in the program.
*/
int nextReg();

int offset(Tree* node);
//////////////////////Instruction Functions/////////////////
////////////////////////////////////////////////////////////

void initInstruction(Instruction *inst);

Instruction* newInstruction(const char *opcode, int result, int t1, int t2, const char *label, int imm);


//////////////////////Buffer Functions//////////////////////
////////////////////////////////////////////////////////////


/*
@brief Function that initalizes a new buffer object.

@param buffer Buffer object to be initialized.
@param inititalSize The amount of memory to be given to the buffer being initialized.
*/
void initBuf(dBuf *buffer, size_t initialSize);

/*
@brief Function that resizes a given buffer to a new amount of allocated memory.

@param buffer Buffer object to be resized.
@param newSize The amount of memory to resize the buffer to.
*/
void resizeBuf(dBuf *buffer, size_t newSize);

/*
@brief Function that simply writes to a given buffer with a given string and length.

@param buffer Buffer object to be written to.
@param data Char* stream to be written to the given buffer.
@param length size_t of char* stream to be written, or some variable length.
*/
void writeBuf(dBuf *buffer, const char *data, size_t length);

/*
@brief Function that appends a string to a given buffer.

@param buffer Buffer object to be appended to.
@param data Char* stream to be appeneded to the buffer object.
*/
void appendBuf(dBuf *buffer, const char *data);

/*
@brief Function to pre-append a string to a given buffer.

@param buffer Buffer object to be pre-appended to.
@param data Char* stream to be preappended to the buffer object.
*/
void preappendBuf(dBuf *buffer, const char *data);

/*
@brief Function that clears the buffer, but does not free the memory for reuse.

@param buffer Buffer object to be cleared.
*/
void clearBuf(dBuf *buffer);

/*
@brief Function that generates a buffer object containing information regarding the current function block being processed.

*This function relies on variables stored globally in codegen.c.

@param bytes The number of bytes derived from navigated the AST to assign stack space.

@return Buffer object containing the function header information.
*/
dBuf* generateFuncHeaderBuffer();

/*
@brief Function that generates a buffer object containing information regarding the current function block footer.

@return Buffer object containing the function footer information.
*/

dBuf* genreateFuncFooterBuffer();


/*
@brief Function that frees the dynamically allocated memory from a buffer object.

@param buffer Buffer object to be freed.
*/
void freeBuf(dBuf *buffer);

#endif