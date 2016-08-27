/* test-type-cast: type conversion operators */
int main() {
  printint(int_of_float(3.6));                 /* float to int */
  printfloat(float_of_int(3));                 /* int to float */
  printint(int_of_string("3.7"));              /* string to int */
  printstring(string_of_int(3));               /* int to string */
  printstring(string_of_float(5.5));           /* float to string */
  printfloat(float_of_string("5.5"));          /* string to float */
  printint(int_of_seq([7]));                   /* scalar sequence to int */
  printfloat(float_of_mat([7.7;]));            /* scalar matrix to float */
  println();
  printmat(mat_of_seq([1, 2, 3, 4]));          /* sequence to matrix */
  printseq(seq_of_mat([1.0, 2.0, 3.0, 4.0;])); /* matrix to sequence */
}
