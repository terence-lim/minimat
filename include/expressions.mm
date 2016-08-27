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

