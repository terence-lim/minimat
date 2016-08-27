/**/
/*----------------------------------------------
  MDIV -- define function for matrix divide infix operator "/" 
-----------------------------------------------*/
matrix mdiv(matrix y, matrix x) {
  checkmatrows(y,x);
  return inv(x' * x) * (x' * y);
}
/**/
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
