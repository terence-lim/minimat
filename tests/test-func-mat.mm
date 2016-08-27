/* test-func-mat: library of matrix functions */
int main() {
  matrix u;
  u = [1.0, 2.0, -3.0; 4.0, -5.0, 6.0; 7.0, 8.0, -9.0;];
  printmat(mabs(u));    /* absolute values */
  printmat(mexp(u));    /* exponent values */
  printmat(mlog(u));    /* log values */
  printmat(eye(2));     /* identity matrix */
  printmat(diag(u));    /* diagonal of matrix */
  printmat(ones(2,4));  /* matrix of ones */
  printfloat(sum(u));   /* sum of matrix values */
  printfloat(mean(u));  /* mean of matrix values */
  printfloat(norm(u));  /* euclidean norm of matrix values */
  printfloat(min(u));   /* minimum of matrix values */
  printfloat(max(u));   /* maximum of matrix values */
  printfloat(det(u));   /* determinant of matrix */
  printmat(triu(u,0));  /* upper triangular matrix */
  printmat(tril(u,0));  /* lower triangular matrix */
  printmat(adjoint(u)); /* adjoint of matrix */
  printmat(inv(u));     /* inverse of matrix */
  printmat(inv(u) * u); /* check inverse */
}
