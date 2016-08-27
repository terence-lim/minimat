/* test-op-matmul: matrix mult checks operands are conformable */
int main() {
  matrix u;
  u = [1.0, 2.0, 3.0, 4.0; 
      11.0, 12.0, 13.0, 14.0;];
  printmat(u * u);    /* matrix sizes do not conform for multiplication */
}
