/* test-expr-sizeof: matrix and vector sizeof functions */
int main() {
  matrix u;
  sequence v;
  u = [1.0, 2.0, 3.0, 4.0; 11.0, 12.0, 13.0, 14.0;];
  v = [1, 2, 3, 4, 5, 6];
  printint(length(v));   /* length operator */
  printint(end(v));      /* sequence end function */
  printint(size(u));     /* matrix size function */
  printint(cols(u));     /* matrix cols operator */
  printint(rows(u));     /* matrix rows function */
  println();
}
