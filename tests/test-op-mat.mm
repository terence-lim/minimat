/* test-op-mat: matrix operators */
int main() {
  matrix u;
  matrix v;
  matrix w;
  matrix y;
  matrix x;
  u = [1.0, 2.0, 3.0, 4.0; 11.0, 12.0, 13.0, 14.0;];
  v = u;
  v[0,0] = [1000.0;];
  printmat(u);
  printmat(v);
  printmat(u + v);             /* arithmetic */
  printmat(v + u);
  printmat(u - v);
  printmat(v - u);
  printmat([1.0, 2.0; 3.0, 4.0;] ^ 3);
  printmat(-v);                /* unary */
  printmat(v');
  printmat(v + [1000.0;]);     /* arithmetic with scalar matrix*/
  printmat([2000.0;] - v);
  printmat(u .* u);            /* elemental */
  printmat(v ./ u);
  printmat(v .% u);
  printmat(v .^ [3.0;]);
  printseq(u < v);             /* relational */
  printseq(u <= v);
  printseq(u > v);
  printseq(u >= v);
  printseq(u == v);
  printseq(u != v);

  printmat(u * u');                      /* matrix multiplication */
  y = [2.0; 0.5; 1.5; 5.0; 7.0; 7.0;];   /* matrix regression */
  x = [1.0,2.0; 2.0,2.0; 3.0,3.0; 4.0,3.0; 5.0,5.0; 6.0,6.0;];
  x = [new matrix(rows(x), 1) + [1.0;], x;];
  printmat(y/x);
  printmat(y%x);
}
