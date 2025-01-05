int main1(){
  output("Hello from main1\n");
  return 0;
}

char main2(int arg1){
  output("Hello from main2\n");
  output("arg1=");
  output(arg1);
  output("\n");
  return 'c';
}

void main3(void arg1, char arg2[]){
  output("Hello from main3\n");
}

char main(){
    int i;
    char c;
    char a[12];
    void v;
    a[0] = 'c';
    a[1] = 'h';
    a[2] = 'a';
    a[3] = 'r';
    main1();
    main2(i);
    main3(v,a);
}

