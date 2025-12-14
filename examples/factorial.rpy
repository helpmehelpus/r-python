def factorial(n: Int) -> Int:
    if n <= 1:
        return 1;
    else:
        return n * factorial(n - 1);
    end;
end;

val result = factorial(11);
asserttrue(result == 39916800, "factorial(11) should be 39916800");
var _ = print(result);