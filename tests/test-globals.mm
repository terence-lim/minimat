/* test-globals: global variables identifier names, definitions and scope */
int      the_int;       /* declare global variables: all types */
float    the_float;
bool     the_bool;
string   the_string;
matrix   the_matrix;
sequence the_sequence;

constant int      the_int      = 99;       /* constants are suspended  */
constant float    the_float    = -999.99;
constant bool     the_bool     = !true;
constant string   the_string   = " constant ";
constant matrix   the_matrix   = [999.99; 999.99;];
constant sequence the_sequence = [99, 99];

int      the_int()      { return  55; }    /* function names not interfere */
float    the_float()    { return  55.55; }
bool     the_bool()     { return  false; }
string   the_string()   { return  " function "; }
matrix   the_matrix()   { return  [555.55, 555.55;]; }
sequence the_sequence() { return  [55, 55]; }

void printglobals() {           /* scope of global variables */
  printint(the_int);
  printfloat(the_float);
  printbool(the_bool);
  printf(the_string);
  println();
  printmat(the_matrix);
  printseq(the_sequence);
}

void printfunctions() {         /* function names do not interfere */
  printint(the_int());
  printfloat(the_float());
  printbool(the_bool());
  printf(the_string());
  println();
  printmat(the_matrix());
  printseq(the_sequence());
}

void printlocals() {            /* scope suspended by local declarations */
  int      the_int;
  float    the_float;
  bool     the_bool;
  string   the_string;
  matrix   the_matrix;
  sequence the_sequence;
  the_int      = 88;
  the_float    = 88.88;
  the_bool     = true;
  the_string   = " local ";
  the_matrix   = [888.88, 888.88;];
  the_sequence = [88, 88];
  printint(the_int);
  printfloat(the_float);
  printbool(the_bool);
  printf(the_string);
  println();
  printmat(the_matrix);
  printseq(the_sequence);
}

int changeglobals() {
  the_int      = 44;            /* change values of globals, will persist */
  the_float    = 44.44;
  the_bool     = false;
  the_string   = " changed ";
  the_matrix   = [444.44, 444.44;];
  the_sequence = [44, 44];
}

int main() {
  the_int      = 77;
  the_float    = 77.77;
  the_bool     = true;
  the_string   = " global ";
  the_matrix   = [777.77, 777.77;];
  the_sequence = [77, 77];
  printglobals();                 /* print globals */
  printlocals();                  /* print locals, suspends globals */
  printfunctions();               /* print functions, do not interfere */
  changeglobals();                /* change values of globals */
  printglobals();                 /* reprint globals with changed values */
}
