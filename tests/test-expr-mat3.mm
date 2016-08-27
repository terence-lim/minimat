/* test-expr-mat3: matrix assignment, subselect, definition */
int main() {
  matrix a;
  a = [1.0; 2.0;];   /* 2 x 1 matrix */
  a = [a; a';];      /* cannot vertcat matrices with different columns */
}
