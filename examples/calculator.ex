extern fun get_argc(): i64;
extern fun get_argv(index: i64): string;
extern fun my_strtol(str: string): i32;
extern fun strcmp(a: string, b: string): i32;
extern fun print(fmt: string, ...): void;

fun add(a: i32, b: i32): i32 {
    return a + b;
}

fun sub(a: i32, b: i32): i32 {
    return a - b;
}

fun mul(a: i32, b: i32): i32 {
    return a * b;
}

fun main() {
    let num_args: i64 = get_argc();

    if (num_args != 4) {
        print("Usage: calculator <action> <a> <b>");
        return 0;
    }

    let action: string = get_argv(1);

    let num_a: i32 = my_strtol(get_argv(2));
    let num_b: i32 = my_strtol(get_argv(3));

    if (action == "add") {
        let sum: i32 = add(num_a, num_b);
        print("Sum: %i", sum);
    } else if (action == "sub") {
        let diff: i32 = sub(num_a, num_b);
        print("Difference: %i", diff);
    } else if (action == "mul") {
        let product: i32 = mul(num_a, num_b);
        print("Product: %i", product);
    }

    return 0;
}
