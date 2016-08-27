/* test-type-string: string literals, relationals and functions */
string gets(string prompt) {     /* string function definition and argument */
  printstring(prompt);
  return("goodbye");
}
int main() {
  string s;
  string t;
  s = "hello, world\n";          /* literals */
  printstring(s);
  t = s;                         /* assignment */
  s = gets("Enter the word \"goodbye\" without quotes: ");
  printstring(s);
  printstring(t);
  printbool(s == "goodbye");     /* literals */
  printbool(s != "goodbye");
  printint(int_of_string("0500"));      /* conversions */
  printfloat(float_of_string("1.0"));
  println();
}

