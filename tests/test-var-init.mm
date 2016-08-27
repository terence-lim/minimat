/* test-var-init: variables are initialized to default values when declared */
int i;         /* global variable declaration: all types */
float f;
bool b;
matrix m;
sequence v;
string s;

void printlocals() {
  int i;
  float f;
  bool b;
  matrix m;
  sequence v;
  string s;
  printmat(m);  /* print locally-declared variables: no seg faults */
  printseq(v);
  printf("\"%s\" ",s);
  printint(i);
  printfloat(f);
  printbool(b);
  println();
}

int main() {
  printmat(m);   /* print globally-declared variables: no seg faults */
  printseq(v);
  printf("\"%s\" ",s);
  printint(i);
  printfloat(f);
  printbool(b);
  println();
  printlocals();
}
