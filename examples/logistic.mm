/* 
  logistic regression by iterated least squares/Newton-Raphson method.
  example adapted from http://strijov.com/sources/demo_logistic_regression.php 
*/

constant float MAXNORM = 0.000001;   /* threshold for convergence */
constant int   MAXITER = 8;          /* max number of iterations */

/* reweight each row of data matrix by column of weights */
matrix mweighted(matrix x, matrix w) {
  matrix y;
  int r;
  checkmatrows(x, w);
  y = new matrix(rows(x),cols(x));
  for(r = 0; r < rows(x); r = r + 1) {
    y[r, 0::cols(y)-1] = x[r, 0::cols(y)-1] * w[r, 0];
  }
  return y;
}

/* computes logistic regression from labels and vars (exclude intercept) */
matrix logistic(matrix labels, matrix vars, string s) {
  int iter;
  float delta;
  matrix beta;
  matrix prev;
  matrix X;
  matrix z;
  matrix p;
  matrix w;
  matrix u;
  matrix x;
  handle g;

  /* open a gnuplot session to plot fit iterations */
  g = gnuplot_init();      /* pointer to gnuplot session object */

  if (s	== "") gnuplot_cmd(g, "set terminal xterm");  
  else gnuplot_set_png(g, s);       /* select terminal or png file output */

  gnuplot_cmd(g, "set multiplot");  /* set axes, labels and plot styles  */
  gnuplot_set_yrange(g, labels);
  gnuplot_set_xrange(g, vars);
  gnuplot_set_ylabel(g, "estimated logistic probability");
  gnuplot_set_xlabel(g, "x");
  gnuplot_cmd(g, "set key top left Left reverse");
  gnuplot_setstyle(g, "linespoints");

  /* initialize parameters */
  X = [ones(rows(vars),1), vars;];
  beta = new matrix(cols(X),1);
  beta[0, 0] = [log(mean(labels) / (1.0 - mean(labels)));];
  delta = MAXNORM;

  /* iterate till  max iter, or little change in estimates */
  for(iter = 1; iter <= MAXITER && delta >= MAXNORM; iter = iter + 1) {
    prev = beta;
    z = X * beta;                  /* update probability estimates */
    p = [1.0;] ./ ([1.0;] + mexp(-z));
    w = p .* ([1.0;] - p);         /* update weights */
    u = z + ((labels - p) ./ w);   /* reweight data */
    x = mweighted(X, w .^ [0.5;]);
    u = mweighted(u, w .^ [0.5;]);
    beta = inv(x' * x) * (x' * u); /* update coeffs with reweighted data */
    delta = norm(beta - prev);     /* check magnitude of parameter changes */

    printstring("iter");           /* display iteration */
    printint(iter);
    printstring(":");
    printfloat(delta);
    println();
    gnuplot_plot_xy(g, vars, p, rows(p), string_of_int(iter));
  }

  /* gnuplot initial and final data points */
  p = [1.0;] ./ ([1.0;] + mexp(-X * beta));   /* final estimates */
  gnuplot_plot_xy(g, vars, p, rows(vars), "recovered data");
  gnuplot_setstyle(g, "points");
  gnuplot_plot_xy(g, vars, labels, rows(vars), "initial data");
  gnuplot_close(g);
  return beta;
}

int main() {
  matrix x;
  matrix y;

  /* create demonstration data set */
  x = [mat_of_seq(-8::1), mat_of_seq(2::11);]';
  y = [new matrix(9, 1); 1.0; 0.0; ones(9, 1);];

  /* display logistic regression results and save graph as PNG */
  printmat(logistic(y, x, "logistic.png")); 
}
