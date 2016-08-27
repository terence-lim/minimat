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
/**/
/*----------------------------------------------
  DET -- computes determinant by recursively expanding minors
-----------------------------------------------*/
float det(matrix a) {
  matrix det;
  int i;
  int j;
  int j1;
  int j2;
  matrix m;
  float tmp;
  checkmatsquare(a);
  if (rows(a) == 1) det = a[0, 0];
  else if (rows(a) == 2) det = a[0, 0] * a[1, 1] - a[0, 1] * a[1, 0];
  else {
    det = [0.0;];
    for (j1 = 0; j1 < cols(a); j1 = j1 + 1) {
      m = new matrix(rows(a) - 1, cols(a) - 1);
      for (i = 1; i < rows(a); i = i + 1) {
        j2 = 0;
        for (j = 0; j < cols(a); j = j + 1) {
           if (j != j1) {
             m[i-1, j2] = a[i, j];
             j2 = j2 + 1;
           }
        }
      }
      det = det + [(-1.0 ^ (float_of_int(j1) + 2.0));] * a[0, j1] * [det(m);];
    }
  }
  return float_of_mat(det);
}

/*----------------------------------------------
  COFACTOR -- returns cofactor of a matrix
-----------------------------------------------*/
matrix cofactor(matrix a) {
  int i;
  int j;
  int ii;
  int jj;
  int i1;
  int j1;
  float det;
  matrix c;
  int n;
  matrix b;
  checkmatsquare(a);
  n = rows(a);
  b = new matrix(n, n);
  c = new matrix(n-1, n-1);
  for (j = 0; j < n; j = j + 1) {
    for (i = 0; i < n; i = i + 1) {
      i1 = 0;
      for (ii = 0; ii < n; ii = ii + 1) {
        if (ii != i) {
          j1 = 0;
          for (jj = 0; jj < n; jj = jj + 1) {
            if (jj != j) {
              c[i1, j1] = a[ii, jj];
              j1 = j1 + 1;
            }
          }
          i1 = i1 + 1;
        }
      }
      b[i, j] = [(-1.0 ^ (float_of_int(i+j)+2.0)) * det(c);];
    }
  }
  return b;
}

/*----------------------------------------------
  INV -- returns inverse of matrix
-----------------------------------------------*/
matrix inv(matrix a) { return cofactor(a)' ./ [det(a);]; }
/**/
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
