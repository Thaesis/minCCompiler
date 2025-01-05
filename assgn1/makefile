CFLAGS = -Il. -Isrc
LEX = lex

# Target for the final executable
obj/scanner: obj/lex.yy.o obj/driver.o
	$(CC) $(CFLAGS) -o scanner obj/driver.o obj/lex.yy.o -lfl

# Rule for compiling the lex output
obj/lex.yy.o: obj/lex.yy.c
	$(CC) $(CFLAGS) -c obj/lex.yy.c -o obj/lex.yy.o 

# Rule for compiling the driver source file
obj/driver.o: src/driver.c
	@mkdir -p obj
	$(CC) $(CFLAGS) -c src/driver.c -o obj/driver.o 

# Rule for generating the lex.yy.c file from the scanner.l file
obj/lex.yy.c: src/scanner.l
	@mkdir -p obj
	$(LEX) -o obj/lex.yy.c src/scanner.l  

# Clean up the generated files
clean:
	rm -rf obj
	rm -f lex.yy.* *.o *~ scanner   

# Target for running tests
test: obj/scanner
	@python ./test/testScanner.py
