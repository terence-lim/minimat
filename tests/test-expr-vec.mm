/* test-expr-vec: vector assignment, subselect, definition, colon expression */
constant sequence x = [-99];     /* sequence constant definition */
int main() {
  sequence u;
  sequence v;
  sequence w;
  v = [1, 2, 3, 4];              /* construct sequence literal */
  printseq(v);
  printseq([]);                  /* empty sequence */
  v = [10, v, v, 11];            /* augment sequence */
  printseq(v);
  v = [101::105, 110:-1:106];    /* colon expression */
  printseq(v);
  printseq(v[2]);                /* subselect vector */
  u = [2, 5, 6];
  printseq(v[u]);
  v[0] = [200];                  /* subassign vector */
  printseq(v);
  v = u;                         /* assignment (copy values) */
  v[0] = [x];
  printseq(u);
  printseq(v);
  printseq(u[1] = v[1] = [99]);  /* chain assignment */
  printseq(u = v = [88, 888]);
}
