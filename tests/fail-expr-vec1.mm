/* fail-expr-vec1: sequence assignment, subselect, definition */
int main() {
  sequence a;
  a = [1000;];   /* sequence cannot be terminated by semi colon */
}
