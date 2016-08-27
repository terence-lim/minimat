/* fail-constant-decl2: constants can only be defined with literal values */
float b;
constant matrix a = [1.0, b;];  /* cannot define constant with identifiers */
int main() {
  b = [2.0, 3.0, 4.0;];
  printmat(b);
  printmat(a);
}
