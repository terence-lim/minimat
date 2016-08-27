/* check-seq-assign -- sequence index out of bounds*/
int main() {
  sequence a;
  a = new sequence(2);
  a[2] = [1];        /* cannot assign to sequence out of bounds */
}

