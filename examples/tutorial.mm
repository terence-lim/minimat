matrix twos(int r, int c) {   /* return r row by c column matrix of 2's */
  return new matrix(r, c) + [2.0;];
}

matrix changeit(matrix replaceme, matrix modifyme) {
  replaceme = [;];         /* attempt to replace with empty matrix */
  modifyme[0,1] = [42.0;]; /* change value in row 0, column 1 of matrix */
  return modifyme';        /* matrix transpose postfix operator */
}

int main() {
  matrix a;
  matrix b;
  matrix c;
  matrix d;
  sequence x;
  sequence y;
  sequence z;
  sequence fib;
  int i;
  float f;
  string s;
  bool B;

  printstring("Define a matrix literal:\n");
  a = [ 1.0, 2.0, 3.0, 4.0;
        5.0, 6.0, 7.0, 8.0; ];
  printmat(a);

  printstring("Matrix row and column attributes:\n");
  printint(rows(a));
  printint(cols(a));
  println();          /* print newline */

  printstring("Define a matrix function to return a matrix of 2's\n");
  b = twos(4, 2);
  printmat(b);

  printstring("Matrix function arguments\n");
  c = changeit(a, b);  /* matrix args are pointers passed by value */
  printmat(a);         /* original matrix will not be replaced */
  printmat(b);         /* but contents may be modified */
  printmat(c);

  printstring("Matrix operators -- addition, dot-multiply:\n");
  b = a + a .* a;
  printmat(b);

  printstring("Horizontally and vertically augment matrix:\n");
  c = [-1.0, -2.0, -3.0, -4.0,-5.0, -6.0, -7.0;
       a,                    [11.0, 12.0, 13.0;
                              14.0, 15.0, 16.0; ];
       new matrix(2, 3),  b;];
  printmat(c);

  printstring("Construct sequence literals:\n");
  x = 1:2:5;     /* colon stride operator, constructss [1, 3, 5] */
  y = -5::5;     /* double-colon operator: implicit stride length 1 */
  z = [1, 2, x]; /* comma-separated ints or sequences, in brackets */
  printseq(x);
  printseq(y);
  printseq(z);

  printstring("Subselect alternate columns of matrix with sequence index:\n");
  d = c[0::rows(c) - 1, x];    /* select multiple positions from c */
  printmat(d);

  printstring("Assign values into subset of a matrix with sequence index:\n");
  d = d * [10.0;];
  c[0::rows(c) - 1, x] = d;    /* assign to multiple positions in c */
  printmat(c);

  printstring("Loop to construct a sequence of Fibonacci numbers:\n");
  fib = new sequence(10);
  fib[0] = [0];
  fib[1] = [1];
  for(i = 2; i < length(fib); i = i + 1) {
    fib[i] = fib[i - 1] + fib[i - 2];
  }
  printseq(fib[end(fib)]);

  printstring("Type in a float: ");
  f = nextfloat();
  printstring("Float value read in = ");
  printfloat(f);
  
  printstring("\nType in an int: ");
  i = nextint();
  printstring("Int value read in = ");
  printint(i);

  printstring("\nType in the string (without quotes) \"hello\" : ");
  s = next();
  B = (s == "hello");
  printbool(B);
}
