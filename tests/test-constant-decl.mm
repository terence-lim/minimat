/* test-constant-decl: definitions and scope */
constant int j      = 2 - 3;          /* definitions: all types */
constant float k    = -2.3;
constant bool l     = !true;
constant string s   = "hello, world";
constant matrix a   = [1.0; 2.0;]';
constant sequence x = new sequence(2);

void printconstants() {               /* global scope */
  printint(j);
  printfloat(k);
  printbool(l);
  printstring(s);
  println();
  printmat(a);
  printseq(x);
}

void printlocals() {    /* scope of constants suspended by local decls */
  int j;
  float k;
  bool l;
  string s;
  matrix a;
  int m;
  j = 99;
  k = 999.0;
  l = true;
  s = "goodbye";
  a = [99.9, 999.99, 9999.0;];
  m = 9999;
  printint(j);
  printfloat(k);
  printbool(l);
  printstring(s);
  println();
  printmat(a);
  printseq(x);
}

int main() {
  printconstants();
  printlocals();
}
