/*
This is a test source file
*/

// Test comment!

extern fun print(str: string): void;
extern fun print_num(str: i64): void;

fun my_return(): i64 {
  return 100;
}

fun main() {
  let my_str: string = "This is my string!";

  print(my_str);
  print("Hello, World!");

  let num: i64 = my_return();

  // print_num(num);

  return num;
}
