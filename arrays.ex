extern fun print(format: string, ...): void;

fun add(a: i64, b: i64): i64 {
  return a + b;
}

fun main() {
  let my_arr: i64[3] = [5, 10, 15];

  // let x: i64 = my_arr[0];

  // print("%i", add(2, 12));
 
  let my_val: i64 = my_arr[0];

  print("%i", my_val);

  return 0;
}
