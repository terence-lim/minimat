/* check-mat-augment -- adjacent matrix rows do  not have same #columns */
int main() {
  matrix a;
  a = [1.0, 2.0, 3.0;];
  [a; a';];             /* cannot augment matrices with different columns */
}

