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