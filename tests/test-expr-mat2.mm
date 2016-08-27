/* test-expr-mat2: matrix assignment, subselect, definition */
int main() {
  matrix a;
  a = [1.0; 2.0;];   /* 2 x 1 matrix */
  a = [a, a';];      /* cannot horzcat matrices with different rows */
}
