/******************************************************************
  expressions.mm -- library helper functions to implement expressions
  Minimat by Terence Lim tl2735@columbia.edu for COMS4115
******************************************************************/
/*----------------------------------------------
  rows(), end(), size()
    -- return dimensional attributes of matrix or sequence object
-----------------------------------------------*/
int end(sequence m) { return length(m) - 1; }
int size(matrix m)  { return length(m); }
int rows(matrix m) { 
  if (cols(m) == 0) return 0; 
  else return length(m) / cols(m);
}

/*----------------------------------------------
  type-conversion functions
-----------------------------------------------*/

external int atoi(string s);
external float atof(string s);

float float_of_string(string s) { return atof(s); }
int   int_of_string(string s)   { return atoi(s); }
string string_of_int(int d)     { return new string("%d", d); }
string string_of_float(float f) { return new string("%f", f); }

matrix mat_of_seq(sequence v) {
  matrix a;
  int i;
  a = new matrix(1, length(v));
  for (i = 0; i < length(v); i = i + 1) 
    a[0, i] = [float_of_int(int_of_seq(v[i]));];
  return a;
}

sequence seq_of_mat(matrix v) {
  sequence a;
  int i;
  a = new sequence(size(v));
  for (i = 0; i < size(v); i = i + 1) 
    a[i] = [int_of_float(float_of_mat(v[i / cols(v), i % cols(v)]))];
  return a;
}

/*----------------------------------------------
  VERTCAT -- helper function to construct matrix expression [...;...;]. 
    Concatenate columns make taller matrix.
-----------------------------------------------*/
matrix vertcat(matrix left, matrix right) {
  matrix out;
  int i;
  int j;

  /* one matrix can be empty, else both must have same number of cols */
  if (!ismatempty(left) && !ismatempty(right)) checkmatcols(left, right);
  out = new matrix(rows(left) + rows(right), maxint2(cols(left),cols(right)));
  for(i = 0; i < cols(left); i = i + 1) {
    for(j = 0; j < rows(left); j = j + 1) {
      out[j, i] = left[j, i];
    }
  }
  for(i = 0; i < cols(right); i = i + 1) {
    for(j = 0; j < rows(right); j = j + 1) {
      out[j + rows(left), i] = right[j, i];
    }
  }
  return out;
}

/*----------------------------------------------
  HORZCAT -- helper function to construct a matrix row expression [1, 2, ...;]
    Concatenate rows make wider matrix
-----------------------------------------------*/
matrix horzcat(matrix left, matrix right) {
  matrix out;
  int i;
  int j;

  /* one matrix can be empty, else both must have same number of rows */
  if (!ismatempty(left) && !ismatempty(right)) checkmatrows(left, right);
  out = new matrix(maxint2(rows(left),rows(right)), cols(left) + cols(right));
  for(i = 0; i < rows(left); i = i + 1) {
    for(j = 0; j < cols(left); j = j + 1) {
      out[i, j] = left[i, j];
    }
  }
  for(i = 0; i < rows(right); i = i + 1) {
    for(j = 0; j < cols(right); j = j + 1) {
      out[i, j + cols(left)] = right[i, j];
    }
  }
  return out;
}

/*----------------------------------------------
  MSELECT -- helper function for matrix subselect expression A[2,d]
-----------------------------------------------*/
matrix mselect(matrix right, sequence row, sequence col) {
  matrix left;
  int i;
  int j;
  left = new matrix(length(row), length(col));
  for(i = 0; i < length(row); i = i + 1) {
    for(j = 0; j < length(col); j = j + 1) {
      checkmatrc(right, int_of_seq(row[i]), int_of_seq(col[j]));
      left[i, j] = right[int_of_seq(row[i]), int_of_seq(col[j])];
    }
  }
  return left;
}

/*----------------------------------------------
   MASSIGN -- helper function for matrix subassignment expression A[1,d] = 
-----------------------------------------------*/
matrix massign(matrix left, sequence row, sequence col, matrix right) {
  int i;
  int j;
  for(i = 0; i < length(row); i = i + 1) {
    for(j = 0; j < length(col); j = j + 1) {
      checkmatrc(left, int_of_seq(row[i]), int_of_seq(col[j]));
      if (cols(right) == 1 && rows(right) == 1) {
        left[int_of_seq(row[i]), int_of_seq(col[j])] = right[0, 0];
      }
      else {
        checkmatrc(right, i, j);
        left[int_of_seq(row[i]), int_of_seq(col[j])] = right[i, j];
      }
    }
  }
  return right;
}

/*----------------------------------------------
  select and assign to matrix with sequence of linear-method indexes
-----------------------------------------------*/
/* select right[s] */
matrix mat_select_seq(matrix right, sequence s) {
  matrix x;
  int i;
  x = new matrix(1, length(s));
  for (i = 0; i < length(s); i = i + 1) {
    checkmatindex(right, int_of_seq(s[i]));
    x[0, i] = right[int_of_seq(s[i]) / cols(right), 
                    int_of_seq(s[i]) % cols(right)];
  }
  return x;
}

/* assign left[s] = right */
matrix mat_assign_seq(matrix left, sequence s, matrix right) {
  int i;
  matrix x;
  if (!ismatscalar(right)) checkmatsize(left,right);
  for (i = 0; i < length(s); i = i + 1) {
    checkmatindex(left, int_of_seq(s[i]));
    if (ismatscalar(right)) x = right; 
    else x = right[i / cols(right), i % cols(right)];
    left[int_of_seq(s[i]) / cols(left), int_of_seq(s[i]) % cols(left)] = x;
  }
  return right;
}

void checkmatindex(matrix v, int i) {
  if (i >= size(v) || i < 0) errorexit("matrix linear index out of bounds");
}

/*----------------------------------------------
  APPEND -- define helper function to construct sequence literal [1, v, 5]
-----------------------------------------------*/
sequence append(sequence left, sequence right) {
  sequence out;
  int i;
  out = new sequence(length(left) + length(right));
  for(i = 0; i < length(left); i = i + 1) {
    out[i] = left[i];
  }
  for(i = 0; i < length(right); i = i + 1) {
    out[i + length(left)] = right[i];
  }
  return out;
}

/*----------------------------------------------
   VSELECT -- define helper for sequence subselect expression V[d]
-----------------------------------------------*/
sequence vselect(sequence right, sequence select) {
  sequence left;
  int i;
  int j;
  left = new sequence(length(select));
  for(i = 0; i < length(select); i = i + 1) {
    j = int_of_seq(select[i]);
    checkseqlength(right, j);
    left[i] = right[j];
  }
  return left;
}

/*----------------------------------------------
   VASSIGN -- define helper for sequence subassignment expression A(v) =
-----------------------------------------------*/
sequence vassign(sequence left, sequence select, sequence right) {
  int i;
  int j;
  for(i = 0; i < length(select); i = i + 1) {
    checkseqlength(left, int_of_seq(select[i]));
    if (length(select) == 1) j = 0; else j = i;
    checkseqlength(right,j);
    left[int_of_seq(select[i])] =  right[j];
  }
  return right;
}

/*----------------------------------------------
  STRIDE -- define helper for colon expression 1::3 10:5:30
----------------------------------------------*/
sequence stride(int beg, int by, int end) {
  int n;
  int i;
  sequence v;
  if ((beg <= end && by > 0) || (beg > end && by < 0)) {
    end = beg+(by*((end - beg) / by));
    n = ((end - beg)/by) + 1;
  }
  else {
    n = 0;
  }
  v = new sequence(n);
  for(i = 0; i < n; i = i + 1) {
    v[i] = [beg + (i * by)];
  }
  return v;
}

/*----------------------------------------------
  Error exit function
-----------------------------------------------*/
external void exit(int i);
void errorexit(string s) { printf("%s.  Exiting...\n",s); exit(0);}

/*----------------------------------------------
   utility functions to check dimensions
-----------------------------------------------*/

/*  Errorexit if row and column index out of matrix bounds */

void checkmatrc(matrix v, int i, int j) {
  if (i >= rows(v) || i < 0 || j >= cols(v) || j < 0)
    errorexit("matrix row-column index out of bounds");
}

/* Errorexit if index position out of sequence bounds */
void checkseqlength(sequence v, int i) {
  if (i >= length(v) || i < 0) errorexit("sequence index out of bounds");
}

/* Errorexit if matrix is empty */
void checkmatempty(matrix u) {
  if (cols(u) == 0 || rows(u) == 0) 
    errorexit("Matrix cannot be zero length");
}

/* Returns true if matrix is empty */
bool ismatempty(matrix u) { return (cols(u) == 0 || rows(u) == 0); }

/* Errorexit if two matrices have different number of columns */
void checkmatcols(matrix u, matrix v) {
  if (cols(u) != cols(v)) 
    errorexit("Matrices cannot have different col size");
}

/* Errorexit if two matrices have different number of rows */
void checkmatrows(matrix u, matrix v) {
  if (rows(u) != rows(v)) 
    errorexit("Matrices cannot have different row size");
}

/* Errorexit if two matrices have different dimensions */
void checkmatdims(matrix u, matrix v) {
  checkmatcols(u, v);
  checkmatrows(u, v);
}

/* Errorexit if two matrices have different capacity */
void checkmatsize(matrix u, matrix v) {
  if (size(u) != size(v)) 
    errorexit("Matrices cannot have different capacity");
}

/* Returns true if matrix is singleton */
bool ismatscalar(matrix u) { return rows(u) == 1 && cols(u) == 1; }

/* Errorexit if matrix is singleton */
void checkmatscalar(matrix u) {
  if (!ismatscalar(u)) errorexit("matrix not a scalar");
}

void checkseqscalar(sequence v) {
  if (length(v) != 1) errorexit("sequence not a scalar");
}

/* Errorexit if matrix is not square */
void checkmatsquare(matrix u) {
  if (cols(u) != rows(u)) errorexit("matrix is not square");
}

/* Errorexit if sequence is empty */
void checkseqempty(sequence u) {
  if (length(u) == 0) errorexit("Sequence cannot be zero length");
}

/* Errorexit if two sequences have different length */
void checkseqsize(sequence u, sequence v) {
  if (length(u) != length(v)) errorexit("Sequences not of same length");
}

/***************************************************************************
  operators.mm: library of helper functions to implement operators
  Minimat by Terence Lim tl2735@columbia.edu for COMS4115
***************************************************************************/
/*----------------------------------------------
  MATBINOP -- helper function for matrix arithmetic operator functions
-----------------------------------------------*/
constant int MATADD = 0;
constant int MATSUB = 1;
constant int MATDOTMUL = 2;
constant int MATDOTDIV = 3;
constant int MATDOTREM = 4;
constant int MATDOTPOW = 5;

matrix matbinop(matrix u, matrix v, int t) {
  matrix w;
  float i;
  float j;
  int k;
  int m;
  float x;
  checkmatempty(u);
  checkmatempty(v);
  if (!ismatscalar(u) && !ismatscalar(v)) checkmatsize(u,v);
  w = new matrix(maxint2(rows(u),rows(v)), maxint2(cols(v),cols(u)));
  for (k = 0; k < rows(w); k = k + 1) {
    for (m = 0; m < cols(w); m = m + 1) {
      if (ismatscalar(u)) i = float_of_mat(u[0, 0]); 
      else i = float_of_mat(u[k, m]);
      if (ismatscalar(v)) j = float_of_mat(v[0, 0]); 
      else j = float_of_mat(v[k, m]);
      if (t == MATADD) x = i + j;
      else if (t == MATSUB) x = i - j;
      else if (t == MATDOTMUL) x = i * j;
      else if (t == MATDOTDIV) x = i / j;
      else if (t == MATDOTREM) x = i % j;
      else if (t == MATDOTPOW) x = i ^ j;
      else errorexit("illegal matrix binop");
      w[k, m] = [x;];
    }
  }
  return w;
}

/*----------------------------------------------
  Define helper functions for matrix binary operators: + -
-----------------------------------------------*/
matrix madd(matrix u, matrix v)    { return matbinop(u, v, MATADD); }
matrix msub(matrix u, matrix v)    { return matbinop(u, v, MATSUB); }
matrix mdotmul(matrix u, matrix v) { return matbinop(u, v, MATDOTMUL); }
matrix mdotdiv(matrix u, matrix v) { return matbinop(u, v, MATDOTDIV); }
matrix mdotrem(matrix u, matrix v) { return matbinop(u, v, MATDOTREM); }
matrix mdotpow(matrix u, matrix v) { return matbinop(u, v, MATDOTPOW); }

/*----------------------------------------------
  MATBINCOMP -- helper function for matrix comparison operators
-----------------------------------------------*/
constant int MATLT = 10;
constant int MATLE = 11;
constant int MATGT = 12;
constant int MATGE = 14;
constant int MATEQ = 15;
constant int MATNE = 16;

sequence matbincomp(matrix u, matrix v, int t) {
  sequence w;
  sequence x;
  int n;
  float i;
  float j;
  int k;
  int m;
  int h;
  bool b;
  int r;
  int c;
  checkmatempty(u);
  checkmatempty(v);
  if (!ismatscalar(u) && !ismatscalar(v)) checkmatsize(u,v);
  r = maxint2(rows(u), rows(v));
  c = maxint2(cols(v), cols(u));
  w = new sequence(r * c);
  n = 0;
  for (k = 0; k < r; k = k + 1) {
    for (m = 0; m < c; m = m + 1) {
      if (ismatscalar(u)) i = float_of_mat(u[0, 0]); 
      else i = float_of_mat(u[k, m]);
      if (ismatscalar(v)) j = float_of_mat(v[0, 0]); 
      else j = float_of_mat(v[k, m]);
      if (t == MATLT) b = i < j;
      else if (t == MATLE) b = i <= j;
      else if (t == MATGT) b = i > j;
      else if (t == MATGE) b = i >= j;
      else if (t == MATEQ) b = i == j;
      else if (t == MATNE) b = i != j;
      else errorexit("illegal matrix comparison op");
      if (b) h = 1; else h = 0;
      w[(k * c) + m] = [h];
      n = n + h;
    }
  }
  x = new sequence(n);
  h = 0;
  for (k = 0; k < length(w); k = k + 1) {
    if (int_of_seq(w[k]) == 1) {
      x[h] = [k];
      h = h + 1;
    }
  }
  return x;
}

/*----------------------------------------------
  Define helper functions for matrix comparison operators: < <= > >= == !=
-----------------------------------------------*/
sequence mlt(matrix u, matrix v) { return matbincomp(u, v, MATLT); }
sequence mle(matrix u, matrix v) { return matbincomp(u, v, MATLE); }
sequence mgt(matrix u, matrix v) { return matbincomp(u, v, MATGT); }
sequence mge(matrix u, matrix v) { return matbincomp(u, v, MATGE); }
sequence meq(matrix u, matrix v) { return matbincomp(u, v, MATEQ); }
sequence mne(matrix u, matrix v) { return matbincomp(u, v, MATNE); }

/*----------------------------------------------
  MATTRANSP -- define function for "'" matrix postfix transpose operator
-----------------------------------------------*/
matrix mtransp(matrix right) {
  int i;
  int j;
  matrix left;
  left = new matrix(cols(right),rows(right));
  for(i = 0; i < rows(right); i = i + 1) {
    for(j = 0; j < cols(right); j = j + 1) {
      left[j, i] = right[i, j];
    }
  }
  return left;
}

/*----------------------------------------------
  MNEG -- define function function for "-" matrix unary prefix operator
-----------------------------------------------*/
matrix mneg(matrix right) {
  matrix left;
  int i;
  int j;
  left = new matrix(rows(right),cols(right));
  for(i = 0; i < rows(right); i = i + 1) {
    for(j = 0; j < cols(right); j = j + 1) {
      left[i, j] = [-float_of_mat(right[i, j]);];
    }
  }
  return left;
}

/*----------------------------------------------
  MMUL -- define function for the "*" matrix multiply infix operator
-----------------------------------------------*/
matrix mmul(matrix left, matrix right) {
  int i;
  int j;
  int k;
  matrix prod;
  float x;

  if (cols(left) == 1 && rows(left) == 1) {
    prod = new matrix(rows(right), cols(right));
    for(i = 0; i < rows(right); i = i + 1) {
      for(j = 0; j < cols(right); j = j + 1) {
        prod[i, j] = [float_of_mat(left[0, 0]) * float_of_mat(right[i, j]);];
      }
    }
  }
  else if (cols(right) == 1 && rows(right) == 1) {
    prod = new matrix(rows(left), cols(left));
    for(i = 0; i < rows(left); i = i + 1) {
      for(j = 0; j < cols(left); j = j + 1) {
        prod[i, j] = [float_of_mat(left[i, j]) * float_of_mat(right[0, 0]);];
      }
    }
  }
  else if (cols(left) != rows(right)) {
    errorexit("illegal matrix dimensions for multiplication");
  }
  else {
    prod = new matrix(rows(left), cols(right));
    for(i = 0; i < rows(prod); i = i + 1) {
      for(j = 0; j < cols(prod); j = j + 1) {
        x = 0.0;
        for(k = 0; k < cols(left); k = k + 1) {
          x = x + float_of_mat(left[i, k]) * float_of_mat(right[k, j]);
        }
        prod[i, j] = [x;];
      }
    }
  }
  return prod;
}

/*----------------------------------------------
  MPOW -- define function for matrix power infix operator "^"
-----------------------------------------------*/
matrix mpow(matrix u, int k) {
  matrix w;
  int i;
  checkmatsquare(u);
  w = u;
  for(i = 1; i < k; i = i + 1) w = mmul(w,u);
  return w;
}
/*----------------------------------------------
  MREM -- define function for matrix remainder infix operator "/" 
-----------------------------------------------*/
matrix mrem(matrix y, matrix x) {
  matrix b;
  b = mdiv(y,x);
  return y - (x * b);
}

/*----------------------------------------------
  VECBINOP -- helper function for sequence arithmetic operator functions
-----------------------------------------------*/
constant int VECADD = 20;
constant int VECSUB = 21;
constant int VECMUL = 22;
constant int VECDIV = 23;
constant int VECREM = 24;

sequence vecbinop(sequence u, sequence v, int t) {
  sequence w;
  int u1;
  int v1;
  int i;
  int ans;
  checkseqempty(u);
  checkseqempty(v);
  if (length(u) > 1 && length(v) > 1) checkseqsize(u,v);
  w = new sequence(maxint2(length(u), length(v)));
  for (i = 0; i < length(w); i = i+1) {
    if (length(u) == 1) u1 = int_of_seq(u[0]); else u1 = int_of_seq(u[i]);
    if (length(v) == 1) v1 = int_of_seq(v[0]); else v1 = int_of_seq(v[i]);
    if (t == VECADD) ans = u1 + v1;
    else if (t == VECSUB) ans = u1 - v1;
    else if (t == VECMUL) ans = u1 * v1;
    else if (t == VECDIV) ans = u1 / v1;
    else if (t == VECREM) ans = u1 % v1;
    else errorexit("illegal sequence binop");
    w[i] = [ans];
  }
  return w;
}

/*----------------------------------------------
  Define helper functions for sequence binary arithmetic operators: + - * / %
-----------------------------------------------*/
sequence vadd(sequence u, sequence v) { return vecbinop(u, v, VECADD); }
sequence vsub(sequence u, sequence v) { return vecbinop(u, v, VECSUB); }
sequence vmul(sequence u, sequence v) { return vecbinop(u, v, VECMUL); }
sequence vdiv(sequence u, sequence v) { return vecbinop(u, v, VECDIV); }
sequence vrem(sequence u, sequence v) { return vecbinop(u, v, VECREM); }

/*----------------------------------------------
  Define helper function for the "-" sequence unary prefix operator
-----------------------------------------------*/
sequence vneg(sequence right) {
  int i;
  sequence left;
  left = new sequence(length(right));
  for(i = 0; i < length(right); i = i + 1) {
    left[i] = [-int_of_seq(right[i])];
  }
  return left;
}


/*----------------------------------------------
  STRINGEQ STRINGNE -- define functions string comparison operators: == !=
-----------------------------------------------*/
external int strcmp(string s, string t);
bool stringeq(string a, string b) { return (strcmp(a,b) == 0); }
bool stringne(string a, string b) { return (strcmp(a,b) != 0); }
bool stringge(string a, string b) { return (strcmp(a,b) >= 0); }
bool stringgt(string a, string b) { return (strcmp(a,b) >  0); }
bool stringle(string a, string b) { return (strcmp(a,b) <= 0); }
bool stringlt(string a, string b) { return (strcmp(a,b) <  0); }

/* returns max or min of two ints */
int maxint2(int i, int j) { if (i > j) return i; else return j; }
int minint2(int i, int j) { if (i < j) return i; else return j; }
/***************************************************************************
  functions.mm: library of matrix functions
  Minimat by Terence Lim tl2735@columbia.edu for COMS4115
***************************************************************************/
/*----------------------------------------------
  declare useful external floating functions  
-----------------------------------------------*/
external float fabs(float x);
external float exp(float x);
external float log(float x);
external float pow(float x, float y);
float sqrt(float x) { return pow(x,0.5); }

/*----------------------------------------------
  MEXP MLOG MABS -- applies unary math function on each matrix element
-----------------------------------------------*/
constant int MATEXP = 1;
constant int MATLOG = 2;
constant int MATABS = 3;

/* helper for unary matrix math functions */
matrix matuop(matrix x, int op) {
  matrix y;
  int i;
  int j;
  float z;
  y = new matrix(rows(x), cols(x));
  for (i = 0; i < rows(y); i = i + 1) {
    for (j = 0; j < cols(y); j = j + 1) {
      z = float_of_mat(x[i, j]);
      if (op == MATEXP) z = exp(z);
      else if (op == MATLOG) z = log(z);
      else if (op == MATABS) z = fabs(z);
      else errorexit("illegal matrix uop");
      y[i, j] = [z;];
    }
  }
  return y;
}
matrix mexp(matrix x) { return matuop(x, MATEXP); }
matrix mlog(matrix x) { return matuop(x, MATLOG); }
matrix mabs(matrix x) { return matuop(x, MATABS); }

/*----------------------------------------------
  EYE -- constructs an identity matrix
-----------------------------------------------*/
matrix eye(int n) {
  matrix x;
  x = new matrix(n, n);
  mat_assign_seq(x,0 : cols(x)+1 : size(x)-1, [1.0;]);
  return x;
}

/*----------------------------------------------
  DIAG -- extracts diagonal elements from a matrix
-----------------------------------------------*/
matrix diag(matrix x) { return mat_select_seq(x, 0 : cols(x)+1 : size(x)-1); }

/*----------------------------------------------
  ONES -- constructs a matrix of 1's
-----------------------------------------------*/
matrix ones(int m, int n) { return new matrix(m, n) + [1.0;]; }

/*----------------------------------------------
  RESHAPE -- reshapes a matrix to new dimensions
-----------------------------------------------*/
matrix reshape(matrix a, int r, int c) {
  matrix b;
  b = new matrix(r, c);
  checkmatsize(a,b);
  mat_assign_seq(b, 0::size(b) - 1, a);
  return b;
}

/*----------------------------------------------
  SUM -- sums matrix elements and returns as a float
-----------------------------------------------*/
float sum(matrix x) {
  float sum;
  int i;
  int j;
  sum = 0.0;
  for (i = 0; i < rows(x); i = i + 1) {
    for (j = 0; j < cols(x); j = j + 1) {
      sum = sum + float_of_mat(x[i, j]);
    }
  }
  return sum;
}

/*----------------------------------------------
  MEAN -- returns average value of matrix elements
-----------------------------------------------*/
float mean(matrix x) { return sum(x) / float_of_int(size(x)); }

/*----------------------------------------------
  NORM -- returns euclidean L2-norm of matrix values
-----------------------------------------------*/
float norm(matrix x) { return sqrt(sum(x .^ [2.0;])); }

/*----------------------------------------------
  MIN -- returns minimum value in matrix
-----------------------------------------------*/
float min(matrix x) {
  int i;
  int j;
  float min;
  float tmp;
  min = float_of_mat(x[0,0]);
  for (i = 0; i < rows(x); i = i + 1) {
    for (j = 0; j < cols(x); j = j + 1) {
      tmp = float_of_mat(x[i, j]);
      if (tmp < min) min = tmp;
    }
  }
  return min;
}

/*----------------------------------------------
  MAX -- returns maximum value in matrix
-----------------------------------------------*/
float max(matrix x) {
  int i;
  int j;
  float max;
  float tmp;
  max = float_of_mat(x[0,0]);
  for (i = 0; i < rows(x); i = i + 1) {
    for (j = 0; j < cols(x); j = j + 1) {
      tmp = float_of_mat(x[i, j]);
      if (tmp > max) max = tmp;
    }
  }
  return max;
}

/*----------------------------------------------
  TRIL -- returns lower triangular submatrix
-----------------------------------------------*/
matrix tril(matrix a, int k) {
  matrix b;
  int r;
  int c;
  b = a;
  for (r = 0; r < rows(a); r = r + 1) {
    for (c = r + 1 + k; c < cols(a); c = c + 1) {
      b[r, c] = [0.0;];
    }
  }
  return b;
}

/*----------------------------------------------
  TRIU -- returns upper triangular submatrix
-----------------------------------------------*/
matrix triu(matrix a, int k) {
  matrix b;
  int r;
  int c;
  b = new matrix(rows(a), cols(a));
  for (r = 0; r < rows(a); r = r + 1) {
    for (c = r + k; c < cols(a); c = c + 1) {
      b[r, c] = a[r, c];
    }
  }
  return b;
}
/*----------------------------------------------
  ADJOINT -- returns adjoint of matrix
----------------------------------------------*/
matrix adjoint(matrix a) { return cofactor(a)'; }

/*----------------------------------------------
  REGRESS -- displays regression fit, returns predicted values
-----------------------------------------------*/
matrix regress(matrix y, matrix x) {
  matrix b;
  matrix se;
  matrix yhat;
  x = [ones(rows(x),1), x;];
  b = y / x;
  yhat = x * b;
  se = ([norm(y - yhat);] * (diag(inv(x' * x)) .^ [0.5;]))
         ./ [sqrt(float_of_int(size(yhat)));];
  printmat(b');
  printmat(se);
  printmat(b' ./ se);
  return yhat;
}
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
