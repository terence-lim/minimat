/* test-op-vec: vector operators */
int main() {
  sequence v;
  sequence u;
  sequence w;
  v = 1::5;              /* colon expression */
  u = 100:2:108;
  printseq(u + v);       /* arithmetic */
  printseq(v + u);
  printseq(u - v);
  printseq(v - u);
  printseq(u * v);
  printseq(v * u);
  printseq(u / v);
  printseq(v / u);
  printseq(u % v);
  printseq(-v);
  printseq(v + [1000]);  /* arithmetic with scalar sequence */
  printseq([2000] + v);
  printseq([2000] / v);
}
