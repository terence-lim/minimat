int main() {
  matrix y;
  matrix x;
  matrix xx;
  sequence outliers;

  /* create demonstration data set */
  y = [2.0; 0.5; 1.5; 5.0; 7.0; 7.0;];
  x = [1.0,2.0; 2.0,2.0; 3.0,3.0; 4.0,3.0; 5.0,5.0; 6.0,6.0;];

  /* insert column of ones */
  xx = new matrix(rows(x), 1) + [1.0;];
  x = [xx, x;];  

  /* check accuracy of inv() */
  xx = x' * x;
  printmat(inv(xx));
  printmat(inv(xx) * xx);

  /* compute outliers */
  outliers = y - x*(y/x) < [-1.0;];
  printmat([y, x;]);  /* print matrix of y and x side-by-side */
  printstring("Number of outliers: ");
  printint(length(outliers));
  println();
}
