extern fun get_argc(): i64;
extern fun get_argv(index: i64): string;
extern fun strcmp(a: string, b: string): i32;
extern fun my_strtol(str: string): i32;

fun add(a: i32, b: i32): i32 {
  return a + b;
}

fun main() {
  let my_arr: i64[3] = [5, 10, 15];

  my_arr[0] = 6;

  let i: i64 = 1;
  my_arr[i] = 69;

  let my_val: i64 = my_arr[i];

  print("%i", my_val);

  let argc: i64 = get_argc();
  let arg: string = get_argv(1);
  
  print("Args: %i", argc);
  print("Arg: %s", arg);

  if (argc != 4) {
    print("Usage: calc <action> <a> <b>");
    return 0;
  }

  if (get_argv(1) == "add") {
    let a: string = get_argv(2);
    let b: string = get_argv(3);

    print("A: %s B: %s", a, b);

    let sum: i32 = add(my_strtol(a), my_strtol(b));
    print("Sum: %i", sum);
  }

  return 0;
}
