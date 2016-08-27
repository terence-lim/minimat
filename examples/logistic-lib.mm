/**/
/* Declare external GNUPLOT C API -- for visualizing plots */
external handle gnuplot_init();
external void gnuplot_cmd(handle g, string c);
external void gnuplot_plot_equation(handle g, string c,string s);
external void gnuplot_close(handle g);
external void gnuplot_plot_xy(handle g, matrix x, matrix y, int n, string s);
external void gnuplot_setstyle(handle g, string s);
  /* lines points linespoints impulses dots steps errorbars boxes */
external void gnuplot_resetplot(handle g);
external void gnuplot_set_xlabel(handle g, string s);
external void gnuplot_set_ylabel(handle g, string s);

/* sets output to a PNG picture file */
void gnuplot_set_png(handle g, string f) {
  gnuplot_cmd(g, "set terminal png");
  gnuplot_cmd(g, new string("set output \"%s\"", f));
}

/* sets yrange of plot from min and max values of data set */
void gnuplot_set_yrange(handle g, matrix y) {
  gnuplot_cmd(g, new string("set yrange [%g:%g]", min(y), max(y)));
}

/* sets xrange of plot from min and max values of data set */
void gnuplot_set_xrange(handle g, matrix x) {
  gnuplot_cmd(g, new string("set xrange [%g:%g]", min(x), max(x)));
}
/**/
