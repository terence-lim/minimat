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
/**/
/*----------------------------------------------
  MDIV -- define function for matrix divide infix operator "/" 
-----------------------------------------------*/
matrix mdiv(matrix y, matrix x) {
  checkmatrows(y,x);
  return inv(x' * x) * (x' * y);
}
/**/
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
