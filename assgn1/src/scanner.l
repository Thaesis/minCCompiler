
/* definitions */

%{
#include <stdio.h>
#include "tokendef.h"

int scancol = 1;
int yycol = 1;
int scanlineno = 1;
int yyline = 1;

char* yyerror;

/* Function to track current column number */
void updateCol();

/* Function to count lines */
int countLines();

/* Function to check for illegal escape sequences in string literals and print out escape sequences correctly. */
int processString();

/* Check for likely unterminated vs. multilined comment */
int determineStringType();

/* Function to build errors to be passed to yyerror */
char* errorStringBuilder(char* str, int row, int col);

%}

newline         \n
whitespace      [ \t\r]+
digit			[0-9]
integer         0|([1-9]{digit}*)
integerlead0    0[0-9]+
character       [A-z]
asciicharacter	[,/\\t\\n"\\0A-z]
alphaNumeric	[A-z0-9]

/* String can contain any characters between the double quotes other than a newline or unescaped doublequotes. */

/* We need to call only the beginning of the strign declaration. This is documented in the Flex documentation, otherwise the */
/* scanne will attempt to consume to many characters and we must create our own parsing method defined as processString() */
string     		\"([^\"\\\n]|\\.)*\"
char      		\'{asciicharacter}\'

/* multiline string has \n somewhere in the middle; The multiline string and untermstring are unable to correctly detect test1.mC. The RegEx for the multilined string always takes precedence */
multlnstring 	\"([^\"\\]*(\\.[^\"\\]*)*[\n]?)*\"

/* If the end quote is found, one of the two regexes above will be matched. Otherwise, the string is unterminated. */
untermstring 	\"([^\"\\]*(\\.[^\"\\]*)*)$

comment         \/\/[^\n]*\n
multlncomment   \/\*([^*]|\*+[^*\/])*\*+\/
untermcomment   \/\*[^\*]*

identifier      {character}+{alphaNumeric}*
illidentifier   {digit}+{identifier}+



%%

 /* rules */

 /* Keywords */;
 
if				{updateCol(); return KWD_IF;}
else 			{updateCol(); return KWD_ELSE;}
while 			{updateCol(); return KWD_WHILE;}
int				{updateCol(); return KWD_INT;}
string 			{updateCol(); return KWD_STRING;}
char 			{updateCol(); return KWD_CHAR;}
return			{updateCol(); return KWD_RETURN;}
void			{updateCol(); return KWD_VOID;}

 /* Operators */;
 
\+ 				{updateCol(); return OPER_ADD;}
\- 				{updateCol(); return OPER_SUB;}
\* 				{updateCol(); return OPER_MUL;}
\/ 				{updateCol(); return OPER_DIV;}
\< 				{updateCol(); return OPER_LT;}
\> 				{updateCol(); return OPER_GT;}
\>\= 			{updateCol(); return OPER_GTE;}
\<\= 			{updateCol(); return OPER_LTE;}
\=\= 			{updateCol(); return OPER_EQ;}
\!\= 			{updateCol(); return OPER_NEQ;}
\= 				{updateCol(); return OPER_ASGN;}
\[ 				{updateCol(); return LSQ_BRKT; }
\] 				{updateCol(); return RSQ_BRKT; }
\{ 				{updateCol(); return LCRLY_BRKT; }
\} 				{updateCol(); return RCRLY_BRKT; }
\( 				{updateCol(); return LPAREN; }
\) 				{updateCol(); return RPAREN; }
\, 				{updateCol(); return COMMA; }
\; 				{updateCol(); return SEMICLN; }
\+\+			{updateCol(); return OPER_INC;}
\-\-			{updateCol(); return OPER_DEC;}
\&\&			{updateCol(); return OPER_AND;}
\|\|			{updateCol(); return OPER_OR;}
\!				{updateCol(); return OPER_NOT;}
\@				{updateCol(); return OPER_AT;}

 /* Identifiers */;
{identifier}    {updateCol(); return ID;}
{illidentifier} {updateCol(); yyerror = "Illidentifier expresssion"; return ERROR;}

 /* Constants */;
{integer}       {updateCol(); return INTCONST;}

{integerlead0}  {
				yyerror = "Integer with leading zero. ";
				return ERROR;
				}

{char}     		{
				updateCol();

				/* Convert the yytext return to ASCII */
				char character = yytext[1];
				int ascii = character;
				sprintf(yytext, "%d", ascii);
				return CHARCONST;
				}

{string}        {updateCol(); return processString();}
{untermstring}  {updateCol(); yyerror = "Unterminated String"; return ERROR;}
{multlnstring}  {updateCol(); return determineStringType();}

 /* Comments */;
{comment}       {updateCol();}

{untermcomment} {
				updateCol();
				yyerror = errorStringBuilder("Unterminated Comment", yylineno, yycol);
				return ERROR;
				}
{multlncomment} {updateCol(); yylineno = countLines();}

 /* Other */;
{newline}       {yylineno++; yycol = 1; scancol = 1;}
{whitespace}    {updateCol();}
.               {return ILLEGAL_TOK;}

%%

/* user routines */

void updateCol(){
	yycol = scancol;
	scancol += yyleng;
}

int countLines(){
	const char* p = yytext;
	int newlines = 0;

	while(*p != '\0') {
		
		if(*p == '\n') {
			newlines++;
			yycol = 1;
			scancol = 1;
		}
		
		p++;
	}
	return newlines; 
}

int processString(){

	size_t length = strlen(yytext);
	char* output = (char*)malloc((length + 1) * sizeof(char));

	if(!output) {
		printf("Malloc Fail!\n");
		exit(1);
	}

	char* outPtr = output;
	for(const char* inPtr = yytext; *inPtr != '\0'; inPtr++) {
		
		if(*inPtr == '\\') {
			inPtr++;
			switch(*inPtr) {
				case 'n':
					*outPtr = '\n';
					break;
				
				case 't':
					*outPtr = '\t';
					break;
				
				case '\\':
					*outPtr = '\\';
					break;

				case '"':
					*outPtr = '"';
					break;
				
				default:
					yyerror = "Unrecognized escape sequence in String";
					return ERROR;
			}
		} 
		
		else {
			*outPtr = *inPtr;
		}
		outPtr++;
	}

	*outPtr = '\0';
	yytext = output;

	return STRCONST;
}

int determineStringType() {
	/* In order to provide reasonable error messages: if a string is unterminated and a " immediately follows it on the subsequent line, we will consider this line to be only an unterminated. */
	/* Otherwise, it will be considered a multilined comment. */
	/* Lastly, if the text retrieved from the RegEx is > 100 characters, consider it to be an unterminated" */

	int numQuotes = 1; 
	int curStrPos = 1;
	int numNewLines = 0;

	const char* str = yytext;
	const MAX_CONSIDERABLE_LENGTH = 100; 

	/* Preprocessing to check for long unterminated string; This could still possibly be a multilined comment. */
	if(strlen(yytext) > MAX_CONSIDERABLE_LENGTH) {

		while(*str != '\n') {
			str++;
			curStrPos++;
		}

		yyless(curStrPos);
		yylineno += 1;
		yycol = 1;
		scancol = 1;
		yyerror = errorStringBuilder("Unterminated String Likely", yylineno, yycol);
		return ERROR;

	}

	/*First Check: Find and count the first " (Should always be the first element) Assuming starting with 1 */
	
	str++;

	while(*str != '\0') {
		
		switch(*str) {
			case '"':
				numQuotes++;

				/*Second Check: Check after the first encountered \n if there is a " present consider it to be unterminated. */
				if(numNewLines == 1 && numQuotes == 2) {
					yyless(curStrPos - 1 ); //Only consumes up to the index of yytext and pushes everything else back for processing.
					yylineno += 1;
					yycol = 1;
					scancol = 1;
					yyerror = errorStringBuilder("Unterminated String Likely", yylineno, yycol);
					return ERROR;
				}

				
				break;
			case '\n':
				numNewLines++;
				break;
			default:
				curStrPos++;
				str++;
				continue; /* Validity checks not needed */
		}

		str++;
		curStrPos++;
	}

	yylineno += countLines();
	yyerror = errorStringBuilder("Multi-Lined String Likely", yylineno, yycol);
	return ERROR;		

}

char* errorStringBuilder(char* str, int row, int col) {

	char* buf = (char*)malloc(50 * sizeof(char));
	char* buf2 = (char*)malloc(50 * sizeof(char));

	if(!buf) {
		printf("Malloc Fail!\n");
		exit(1);
	}

	sprintf(buf, "%s", str);
	sprintf(buf2, " begins at {%d, %d}", row - countLines(), col);
	strcat(buf, buf2);
	
	return buf;
}