extern fun print(format: string, ...): void;

fun add(a: i64, b: i64): i64 {
  return a + b;
}

fun test(a: i64, b: i64, c: i64): i64 {
  return a + (b * c);
}

fun test_f64(a: f64, b: f64): f64 {
  return a % b;
}

fun main() {
  let x: i64 = 1;
  let y: i64 = 1;
  {
    let y: i64 = 0;
  }

  let cond1: bool = true;
  let cond2: bool = true;
  let cond3: bool = !cond2;

  let x1: f64 = 1.0;
  let y1: f64 = (x1 + 2.0);

  print("%f", x1);

  if (x1 == y1 || !cond3) {
    print("If condition is true");
  } else {
    print("If condition is false");
  }

  print("%i", test(2, 4, 6));

  print("Sum: %i", add(1, 1));

  return x;
}
