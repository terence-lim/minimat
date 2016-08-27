/* check-mat-assign -- index out of bounds */
int main() {
  matrix a;
  a = new matrix(2, 2);
  a[2, 2] = [1.0;];     /* cannot assign to matrix out of bounds */
}

