/* check-seq-select -- sequence index out of bounds*/
int main() {
  sequence a;
  a = new sequence(2);
  printseq(a[2]);        /* cannot reference sequence out of bounds */
}

