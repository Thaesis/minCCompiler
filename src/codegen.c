#include <codegen.h>
#include <../src/tree.h>
#include <stdarg.h>

dBuf *dataBuffer;
dBuf *textBuffer;
dBuf *globlMainBuffer;
dBuf *currentBlock;
dBuf *outputFunc;

char *globalVars[MAX_VAR];
int numLocals = 0;
int numLocalsUsed = 0;

// Global Register Counter
int currentReg = 0;

char currentFuncLabel[64];

void generateMIPS(Tree *root) {

    // Initalize Writing Buffers
    initBuf(dataBuffer, 256);
    initBuf(textBuffer, 256);
    initBuf(globlMainBuffer, 256);
    initBuf(currentBlock, 256);
    initBuf(outputFunc, 256);

    writeBuf(dataBuffer, "\t\t.data\n", 9);

    // .globl main technically occurs inside of the .text portion, but it always comes first.
    // Thus, we can simply consider it to be a portion of the globlMainBuffer, and write all remaining
    // function declarations to the .text portion.
    writeBuf(globlMainBuffer, "\t\t.text\n\t\t.globl main", 22);

    process(root);

    //Build Data Section
    printf("%s\n", dataBuffer->data);
    // Build global main, required since this occurs at the top of the program.
    printf("%s\n", globlMainBuffer->data);
    // Build all remaining function calls and data.
    printf("%s\n", textBuffer->data);

    // Always append the library function required by the assignment. Ideally this would only
    // trigger if it was encountered.

    appendBuf(outputFunc, "output:\n");
    appendBuf(outputFunc, "\t\tlw $a0, 4($sp)\n");
    appendBuf(outputFunc, "\t\tli $v0, 1\n");
    appendBuf(outputFunc, "\t\tsyscall");
    appendBuf(outputFunc, "\t\tjr $ra");
    
    printf("%s\n", outputFunc->data);

    freeBuf(dataBuffer);
    freeBuf(textBuffer);
    freeBuf(globlMainBuffer);
    freeBuf(currentBlock);
    freeBuf(outputFunc);
}

void process(Tree *node) {
    if (!node) return ;
    
    // Pre-Recursion Cases
    switch(getNodeKind(node)) {
        
        // Each of these cases follow their own recursive traversal starting from their passed node.
        // Thus, we only need to pick-out the top level encounters through the AST, everything else can be ignored.

        case FUNCTYPENAME:
            // Assign the function label upon entry to be preappended after function data has been collected.
            // This allows easier access of StrTable with scoping.
            snprintf(currentFuncLabel, sizeof(currentFuncLabel), "%s:\n", globalScope->strTable[getChild(node, 1)->val]->id);            
            break;
        case EXPR:
            emitExpr(node);
            break;
        case ASSIGNSTMT:
            emitAssignStat(node);
            break;
        case VARDECL:
            emitVar(node);
            break;
        case ARRAYDECL:
            emitArray(node);
            break;
        case CONDSTMT:
            emitCondStat(node);
            break;
        case LOOPSTMT:
            emitLoop(node);
            break;
        default:
            break;


    }

    // Recursively Handle the AST
    for (int i = 0; i < node->numChildren; i++) {
        process(getChild(node, i));
    }

    // Post-Recursion Cleanup of currentBlock
    switch(getNodeKind(node)) {
        
        case FUNCTYPENAME:
            char temp[64];

            // Generate function Header with the correct byte allocation.
            dBuf *funcHeader = generateFuncHeaderBuffer();
            dBuf *funcFooter = genreateFuncFooterBuffer();

            //handle header / footer appends
            preappendBuf(currentBlock, funcHeader->data);
            appendBuf(currentBlock, funcFooter->data);

            snprintf(temp, sizeof(temp), "end_%s:\n", currentFuncLabel);
            appendBuf(currentBlock, temp);

            // Append the currentBlock buffer to the .text buffer
            if(strncmp(currentFuncLabel, "main", sizeof(currentFuncLabel)) == 0) {
                appendBuf(globlMainBuffer, currentBlock->data);
            } else {
                appendBuf(textBuffer, currentBlock->data);
            }

            clearBuf(currentBlock);
            memset(currentFuncLabel, '\0', sizeof(currentFuncLabel));

            break;
        default:
            break;
    }
}

int emitExpr(Tree* node) {

    if (!node) return -1;

    int result = -1;

    switch (getNodeKind(node)) {
        
        case FUNCCALL: {
            if(strcmp(currentScope->strTable[getChild(node, 0)->val]->id, "output") == 0){
                int outArg = emitExpr(getChild(node, 0));
                char temp[64];
                snprintf(temp, sizeof(temp), "addu $a0, $zero, $t%d\naddiu $v0, $zero, 1\nsyscall", outArg);
                appendBuf(currentBlock, temp);
                break;
            }else{
                //handle other function calls
                break;
            }
        }
        case RELOP: {
            int op = node->val;

            int t1 = emitExpr(getChild(node, 0));
            int t2 = emitExpr(getChild(node, 1)); 
            result = nextReg();

            if(op == LT)
                emit(newInstruction(slt, result, t1, t2, NULL, NULL));
            else
                emit(newInstruction(slt, result, t2, t1, NULL, NULL));
            break;

        }
        case MULOP:
        case ADDOP: {
            int t1 = emitExpr(getChild(node, 0));
            int t2 = emitExpr(getChild(node, 1));

            char* operator = "";

            if(node->val == DIV) {
                emit(newInstruction(div, NULL, t1, t2, NULL, NULL));
                result = nextReg();
                emit(newInstuction(mflo, result, NULL, NULL, NULL));
                break;
            }

            result = nextReg();

            switch(node->val) {
                case MUL:
                    operator = mul;
                    break;
                case ADD:
                    operator = add;
                    break;
                case SUB:
                    operator = sub;
                    break;
            }
            emit(newInstruction(operator, result, t1, t2, NULL, NULL));
            break;
        }
        case IDENTIFIER: {

            int t1 = currentFuncLabel[0] == '\0' ? 0 : 1;
            int t2 = offset(node);

            result = nextReg();
            emit(newInstruction(lw, result, t1, NULL, NULL, t2));
            break;
        }
        case INTEGER: {
            result = nextReg();
            emit(newInstruction(li, result, NULL, NULL, NULL, value(node)));
        }
        default:
            break;
    }

    return result;
}

void emitAssignStat(Tree *node) {

    int t1 = emitExpr(getChild(node, 1));
    char *var = currentScope->strTable[getChild(getChild(node, 0), 0)->val]->id ;

    if(var != NULL && existsIn(globalVars, MAX_VAR, var)) {

        printf("\t\tsw $s%d, %s\n", t1, var);
        return;
    }   

    int t2 = currentFuncLabel[0] == '\0' ? 0 : 1;
    int t3 = offset(getChild(node, 0));

    emit(newInstruction(sw, t3, t1, t2, NULL, NULL));
}

int emitArray(Tree *node) {

    char temp[64];
    int dataType = getChild(node, 0)->val;
    int arraySize = getChild(node, 3)->val;
    int space;

    switch (dataType) {
        case INT_TYPE:
            space = arraySize * 4;
            break;
        case CHAR_TYPE:
            space = arraySize;
            break;
        default:
            perror("Invalid data type in AST.\n");
            exit(EXIT_FAILURE);
    }

    char *id = currentScope->strTable[getChild(node, 1)->val]->id;

    snprintf(temp, sizeof(temp), "%s:\t.space %d\n", id, space);
    appendBuf(dataBuffer, temp);

}

void emitCondStat(Tree *node) {

    int t1 = emitExpr(getChild(node, 0));
    int t2 = nextReg();
    emit(newInstruction(li, t2, NULL, NULL, NULL, 1));
    char *l1 = newLabel();

    //Check if the condition is true
    emit(newInstruction(beq, NULL, t2, t1, l1, NULL));

    int t3 = emitExpr(getChild(node, 2));
    char *l2 = newLabel();
    emit(newInstruction(j, NULL, NULL, NULL, l2, NULL));
    int t4 = emitExpr(getChild(node, 1));
    emit(newInstruction(NULL, NULL, NULL, NULL, l2, NULL));
}

int emitLoop(Tree *node) {
    
    int t1 = emitExpr(getChild(node, 0));
    int t2 = nextReg();
    emit(newInstruction(li, t2, NULL, NULL, NULL, 1));
    char *l1 = newLabel();
    char *l2 = newLabel();
    emit(newInstruction(beq, NULL, t1, t2, l2, NULL));

    emitExpr(getChild(node, 1));

    emit(newInstruction(j, NULL, NULL, NULL, l1, NULL));

    emit(newInstruction(NULL, NULL, NULL, NULL, l2, NULL));
}

void emitVar(Tree* node) {

    char temp[64];
    char* id = currentScope->strTable[getChild(node, 0)->val]->id;

    if(currentFuncLabel[0] = '\0') {
        // Global variable
        snprintf(temp, sizeof(temp), "%s:\t.word 0\n ", id);
        appendBuf(dataBuffer, temp);

    } else {
        // Local Variable
        numLocals++;
        
    }

}

void emit(Instruction *inst) {

    char temp[64];

    // Special Label Case
    if(inst->label && inst->opcode == NULL) {
        snprintf(temp, sizeof(temp), "%s:\n", inst->label);
        appendBuf(currentBlock, temp);
        return;
    }

    if ((strcmp(inst->opcode, j) == 0 || strcmp(inst->opcode, jal) == 0) && inst->label != NULL) {  
        const char *instructionType = (strncmp(inst->opcode, j, sizeof(inst->opcode)) == 0) ? j : jal;
        snprintf(temp, sizeof(temp), "\t\t%s %s", instructionType, inst->label);
        appendBuf(currentBlock, temp);
        return;
    }

    if(strcmp(inst->opcode, sw) == 0 || strcmp(inst->opcode, lw) == 0) {
        const char *instructionType = (strcmp(inst->opcode, sw) == 0) ? sw : lw;
        snprintf(temp,sizeof(temp), "\t\t%s $%d, 4($sp)", instructionType, inst->result); // THIS IS NOT CORRECT
        appendBuf(currentBlock, temp);
        return;
    }

    dBuf *instruction;
    initBuf(instruction, 256);
    writeBuf(instruction, "\t\t", 3);

    dBuf *instBuilder[5];

    // First element is always the opcode, edge cases filtered before.
    snprintf(temp, sizeof(temp), "%s ", inst->opcode);
    initBuf(instBuilder[0], 6);
    appendBuf(instBuilder[0], temp);
    memset(temp, '\0', sizeof(temp));

    int index = 1;

    if(inst->result) {
        snprintf(temp, sizeof(temp), "$s%d, ", inst->result);
        initBuf(instBuilder[index], 6);
        appendBuf(instBuilder[index], temp);
        memset(temp, '\0', sizeof(temp));
        index++;
    }

    if(inst->imm) {
        snprintf(temp, sizeof(temp), "%d ", inst->imm);
        initBuf(instBuilder[index], 6);
        appendBuf(instBuilder[index], temp);
        memset(temp, '\0', sizeof(temp));
        index++;
    }

    if(inst->t1) {
        snprintf(temp, sizeof(temp), "$s%d, ", inst->t1);
        initBuf(instBuilder[index], 6);
        appendBuf(instBuilder[index], temp);
        memset(temp, '\0', sizeof(temp));
        index++;
    }

    if(inst->t2) {
        snprintf(temp, sizeof(temp), "$s%d, ", inst->t2);
        initBuf(instBuilder[index], 6);
        appendBuf(instBuilder[index], temp);
        memset(temp, '\0', sizeof(temp));
        index++;
    }

}

char* newLabel() {
    return "temp";
}

int nextReg() {
    
    if(currentReg == 7) {
        currentReg = 0;
        return currentReg;
    }
    else
        return currentReg++;

}

int offset(Tree* node) {

    if(!node) return -1;

    return currentScope->strTable[getChild(node, 0)->val]->size;

}

void initInstruction(Instruction *inst) {

    if(inst == NULL) {
        return;
    }

    inst->opcode = NULL;
    inst->result = NULL;
    inst->t1 = NULL;
    inst->t2 = NULL;
    inst->label = NULL;
    inst->imm = NULL;

}

Instruction* newInstruction(const char *opcode, int result, int t1, int t2, const char *label, int imm) {

    Instruction *temp;
    
    temp->opcode = opcode ? strdup(opcode) : NULL; 
    temp->result = result ? strdup(result) : NULL;
    temp->t1 = t1 ? strdup(t1) : NULL;
    temp->t2 = t2 ? strdup(t2) : NULL;
    temp->label = label ? strdup(label) : NULL;
    temp->imm = imm ? strdup(imm) : NULL;

    return temp;
}

// Dirty solution, Non-effective way to find arrLen. However , it will aLways be MAX_VAR for this use.
int existsIn(const char* arr[], int arrLen, const char *str) {

    for(int i = 0; i < arrLen; i++) {

        if(strcmp(arr[i], str) == 0) {
            return 1;
        }

    }

    return 0;

}
/* Dynamic Buffer Functions */

void initBuf(dBuf *buffer, size_t initalSize) {
    buffer->data = malloc(initalSize);

    if(!buffer->data) {
        perror("Failed to allocate memory.\n");
        exit(EXIT_FAILURE);
    }

    buffer->size = initalSize;
    buffer->used = 0;
}

void resizeBuf(dBuf *buffer, size_t newSize) {
    char *newData = realloc(buffer->data, newSize);

    if(!newData) {
        perror("Failed to reallocate memory.\n");
        exit(EXIT_FAILURE);
    }

    buffer->data = newData;
    buffer->size = newSize;
}

void writeBuf(dBuf *buffer, const char *data, size_t length) {
    if(buffer->used + length > buffer->size) {
        size_t newSize = buffer->size * 2;

        while(buffer->used + length > newSize) {
            newSize *= 2;
        }

        resizeBuf(buffer, newSize);

    }
    memcpy(buffer->data + buffer->used, data, length);
    buffer->used += length;
}

void appendBuf(dBuf *buffer, const char *data) {
     
    if(!buffer || !data) return;

    size_t strLen = strlen(data);

    if(buffer->used + strLen + 1 >= buffer->size) {
        size_t newSize = buffer->size == 0 ? 64 : buffer->size * 2;

        while(buffer->used + strLen + 1 > newSize) {
            newSize *= 2;
        }

        resizeBuf(buffer, newSize);
    }
    memcpy(buffer->data + buffer->used, data, strLen);
    buffer->used += strLen;
    buffer->data[buffer->used] = '\0';

}

void preappendBuf(dBuf *buffer, const char *data) {
    size_t dataLen = strlen(data);
    size_t currentLen = strlen(buffer);

    if(currentLen + dataLen + 1 > buffer->size) {
        buffer->size = currentLen + dataLen + 1;
        buffer = realloc(buffer, buffer->size);

        if(buffer == NULL) {
            perror("Failed to reallocate memory.\n");
            exit(EXIT_FAILURE);
        }
    }

    memmove(buffer + dataLen, buffer, currentLen + 1);
    memcpy(buffer, data, dataLen);
}

void clearBuf(dBuf *buffer) {

    if(buffer == NULL || buffer->size == 0) {
        return;
    }

    memset(buffer, '\0', buffer->size);

}

dBuf* generateFuncHeaderBuffer() {
    dBuf *temp;
    char str[64];

    snprintf(str, sizeof(str), "%s:\n", currentFuncLabel);
    appendBuf(temp, str);
    memset(str, '\0', sizeof(str));

    appendBuf(temp, "\t\tsw $fp, ($sp)\n");
    appendBuf(temp, "\t\tmove $fp, $sp\n");
    appendBuf(temp, "\t\taddi $sp, $sp, -4\n");

    for(int i = 0; i < 8; i++) {
        snprintf(str, sizeof(str), "\t\tsw $s%d, ($sp)\n", i);
        appendBuf(temp, str);
        memset(str, '\0', sizeof(str));
        appendBuf(temp, "\t\taddi $sp, $sp, -4\n");
    }

    if(numLocals > 0) {
        snprintf(str, sizeof(str), "\t\taddi, $sp, $sp, -%d", numLocals * 4);
        appendBuf(temp, str);
        memset(str, '\0', sizeof(str));
    }

    return temp;

}

dBuf* genreateFuncFooterBuffer() {
    dBuf* temp;
    char str[64];

    snprintf(str, sizeof(str), "end_%s:\n", currentFuncLabel);
    appendBuf(temp, str);
    memset(str, '\0', sizeof(str));

    // Deallocation of local variable happens here

    if(numLocals > 0) {
        snprintf(str, sizeof(str), "\t\taddi, $sp, $sp, %d", numLocals * 4);
        appendBuf(temp, str);
        memset(str, '\0', sizeof(str));
    }

    for(int i = 7; i > 0; i--) {
        appendBuf(temp, "\t\taddi $sp, $sp, 4");
        snprintf(str, sizeof(str), "\t\tlw $s%d, ($sp)", i);
        appendBuf(temp, str);
        memset(str, '\0', sizeof(str));
    }

    appendBuf(temp, "\t\taddi $sp, $sp, 4");
    appendBuf(temp, "\t\tlw $fp, ($sp)");
    appendBuf(temp, "\t\tjr $ra\n");
    

    return temp;

}

void freeBuf(dBuf *buffer) {
    free(buffer->data);
    buffer->data = NULL;
    buffer->size = 0;
    buffer->used = 0;
}