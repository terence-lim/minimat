/*************************************************
  io.mm -- basic i/o and type conversion functions
  Minimat by Terence Lim tl2735@columbia.edu for COMS4115
*************************************************/
/*----------------------------------------------
  Define basic print to stdout functions
-----------------------------------------------*/
void println()             { printf("\n"); }
void printint(int i)       { printf("%d ",i); }
void printbool(bool b)     { if (b) printint(1); else printint(0); }
void printfloat(float f)   { printf("%6.2f ",f); }
void printstring(string s) { printf("%s ",s); }
void printhandle(handle i) { printf("%p ", i); }
void printdims(matrix x)   { printf("%d %d\n",rows(x),cols(x)); }

void printseq(sequence v) {
  int n;
  int i;
  n = length(v);
  printf("[%d int]\n",n);
  for(i = 0; i < n; i = i + 1) printint(int_of_seq(v[i]));
  if (i > 0) println();
}

void printmat(matrix v) {
  int c;
  int r;
  int i;
  int j;
  c = cols(v);
  r = rows(v);
  printf("[%d x %d float]\n",r,c);
  for(i = 0; i < r; i = i + 1) {
    for(j = 0; j < c; j = j + 1) {
      printfloat(float_of_mat(v[i, j]));
    }
    println();
  }
}

/*----------------------------------------------
  Define basic input from stdin functions
-----------------------------------------------*/
external int scanf(string s, string h);
string next() {
  string h;
  h = new string();
  scanf("%255s",h);
  return h;
}

float nextfloat() { return float_of_string(next()); }
int   nextint()   { return int_of_string(next()); }
