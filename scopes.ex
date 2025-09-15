extern fun print(str: string): void;

fun main() {
  let x: i64 = 1;
  {
    let y: i64 = 0;
  }

  let cond: bool = false;

  if (cond) {
    print("If condition is true");
  } else {
    print("Hello, World!");
  }

  return x;
}
