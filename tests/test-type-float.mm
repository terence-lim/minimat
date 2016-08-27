/* test-type-float: float literals, operators and relationals */
float addme(float x) {   /* float function definition and argument */
  return x + 39.5;
}
int main() {
  float a;
  float b;
  float c;
  a = -2.3;              /* literals */
  b = 4.6 + 3.0;
  printfloat(-a);        /* arithmetic operators */
  printfloat(a);
  printfloat(a + b);
  printfloat(a - 10.9); 
  printfloat(5.0 * b);
  printfloat(a / b);
  printfloat(a % 2.0);
  printfloat(a ^ 3.0);
  printb(a == -2.3);      /* relationals */
  printb(a == b);
  printb(a != b);
  printb(a >= b);
  printb(a > b);
  printb(a <= b);
  printb(a < b);
  printfloat(addme(2.5)); /* function call */
  println();
}
