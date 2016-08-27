/* check-mat-add -- not same size */
int main() {
  matrix a;
  a = new matrix(4,2);
  printmat(a + a');     /* cannot add matrices of different sizes */
}

