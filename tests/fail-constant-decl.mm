/* fail-constant-decl: definitions and scope */
constant int m = 3;
int main() {
  m = 2;         /* cannot assign to or modify constant */
}
