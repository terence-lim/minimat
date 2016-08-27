/* fail-expr-mat1: matrix assignment, subselect, definition */
int main() {
  matrix a;
  a = [1000;];     /* matrix literal cannot comprise ints */
}
