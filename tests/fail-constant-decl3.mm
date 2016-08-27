/* fail-constant-decl3: duplicate constant definition */
constant int j = 2 - 3;          
constant int j = 1;         /* duplicate constant identifier */
int main() {}
