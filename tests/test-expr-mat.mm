/* test-expr-mat: matrix assignment, subselect, definition */
constant matrix x = [-99.0;-9.9;];      /* matrix constant definition */
int main() {
  matrix a;
  matrix b;
  matrix c;
  sequence u;
  sequence v;
  a = [  1.1,  2.1,  3.1,  4.1; 
         5.2,  6.2,  7.2,  8.2; ];      /* construct matrix literal */
  b = [ 11.1, 12.1, 13.1, 14.1; 
        15.2, 16.2, 17.2, 18.2; ];
  printmat(a);
  printmat([;]);                        /* empty matrix */
  printmat([a; b;]);                    /* horzcat matrix */
  printmat([a', b';]);                  /* vertcat matrix */
  printmat([a, [;];]);                  /* one matrix can be empty */
  printmat([[;], b;]);
  printmat([a; [;];]);
  printmat([[;]; b;]);
  printmat(a[1, 2]);                    /* subselect matrix */
  u = 1::1;
  v = 1:2:3;
  printmat(a[u, v]);
  b[1, 3:-2:1] = a[0, v];               /* subassign matrix */
  printmat(b);
  b = a;                                /* assignment (copies values) */
  b[0,2::3] = x';
  printmat(a);
  printmat(b);
  printmat(a[1,2::3] = b[0,2::3] = [-99.9, -88.8;]);  /* chained assignment */
  printmat(a);
  printmat(b);
  printmat(a[1,0] = b[0,1] = [-77.7;]);
  printmat(a);
  printmat(b);
}
