
<!-- README.md is generated from README.Rmd. Please edit that file -->
# sometype

<!-- badges: start -->
<!-- badges: end -->
This is a collection of experiments implementing Option & Result types in (mostly) base R. The API draws most of its inspiration from the [Rust](https://www.rust-lang.org/) standard library ([Option<T>](https://doc.rust-lang.org/std/option/), [Result<T, E>](https://doc.rust-lang.org/std/result/)).

Other packages have done something for `Option` ([maybe](https://cran.r-project.org/package=maybe), [optional](https://cran.r-project.org/package=optional), among others).

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("snystrom/sometype")
```

## R Option<T>

`sometype` implements an `Option` S3 type:

Options are useful for wrapping values that can be missing (`None`) or real (`Some`), forcing the user to handle them explicitly.

``` r
library(sometype)

div0 <- function(x, y) {
  if (y == 0) {
    return(none)
  } 

  some(x/y)
}

answer <- div0(10,2)

# This is not allowed
answer > 2
#> Error: Cannot use > on Option<Some>
```

``` r
# Explicitly handling the option allows comparison
unwrap(answer) > 2
#> [1] TRUE
```

``` r
# Syntactic sugar allows auto-unwrapping 
# (this is probably a bad idea and I'll kill this later)
answer <- !div0(10,2)
answer == 5
#> [1] TRUE
```

``` r
# Unwrap(none) will crash
answer <- !div0(10,0)
#> Error: Cannot unwrap, got None
```

### Option<T> constructors

Users can manually build `Option<T>` objects using `some()` and `none`, or by using the `option()` constructor.

``` r
# Values become Some()
option(5)
#> some(numeric)
#> [1] 5
```

``` r
# Missing values become None
option(NULL)
#> None
```

``` r
# Options can be manually constructed
some(5) 
#> some(numeric)
#> [1] 5
```

``` r
# none is a special keyword export!
none
#> None
```

Users can provide custom predicates to produce `Option<None>` from values.

``` r
gt_five <- function(x) {
  x > 5
}

option(10, list(gt_five))
#> None
```

### Handling Options

``` r
# Return a default value on None
unwrap_or(div0(1,0), 0)
#> [1] 0
```

``` r
# Do a custom behavior on None
oh_no <- function() {
  print("Oh No!")
}

unwrap_or_else(div0(1,0), oh_no)
#> [1] "Oh No!"
```

``` r
# Throw a specific error on None
expect(div0(1,0), "I divided by zero!")
#> Error: I divided by zero!
```

## Result<T,E>

### Error

``` r
e <- error("some_error", "a custom message")
e
#> Result<Error>
#>   Error<some_error>
#>     'a custom message'
```

``` r
# NOTE: I had to hack RMD to display this suggesting I haven't quite got the error() implementation down yet.
stop(e)
#> Error: a custom message
```

### Ok(T)

``` r
ok(5)
#> Result<ok(numeric)>
#> [1] 5
```

``` r
# results cannot nest (this differs from Rust!)
ok(ok(5)) == ok(5)
#> [1] TRUE
```

Results can be unwrapped just like Options.

``` r
unwrap(ok(5))
#> [1] 5
```

### Results are incompatible with base methods

``` r
ok(5) + 1
#> Error: Cannot use + on Result<Ok>
```

``` r
as.integer(ok(5))
#> Error: Cannot convert Result<Ok> to integer.
```

But can be compared with other Results

``` r
ok(1) == ok(1)
#> [1] TRUE
```

``` r
ok(1) == ok(2)
#> [1] FALSE
```

``` r
ok(1) == 1
#> Error in `==.result`(ok(1), 1): Cannot compare Result<Ok> to non-Result.
```

``` r
error() == error()
#> [1] TRUE
```

``` r
error() == ok(1)
#> [1] FALSE
```

## Use Cases

### Alternative to tryCatch

Use with methods that do not natively support `sometype`.

``` r
may_fail <- function(x) {
  if (x > 10) {
    stop("failure!")
  } else {
    return("success!")
  }
}
```

``` r
may_fail(5)
#> [1] "success!"
```

``` r
may_fail(11)
#> Error in may_fail(11): failure!
```

``` r
try_result(may_fail(5))
#> Result<ok(character)>
#> [1] "success!"
```

``` r
try_result(may_fail(11))
#> Result<Error>
#>   Error<generic_result_error>
#>     'failure!'
```

Catch into custom error types

``` r
try_result(may_fail(11), .err_type = "a_custom_error")
#> Result<Error>
#>   Error<a_custom_error>
#>     'failure!'
```

### Refactors

Consider a situation where two methods are owned by an external source (`method_one`, `method_two`), that we wrap into our own handler, `nested_may_fail`.

``` r
# Pretend this is from another package, we don't control whether it `stop`s
method_one <- function() {
  stop("method one failed")
}

# Pretend this is from another package, we don't control whether it `stop`s
method_two <- function() {
  stop("method two failed")
}

# This is our function, we only control how to handle the outputs
nested_may_fail <- function(x = TRUE) {
  if (x) {
    method_one()
  } else {
    method_two()
  }
}
```

``` r
nested_may_fail(TRUE)
#> Error in method_one(): method one failed
```

``` r
nested_may_fail(FALSE)
#> Error in method_two(): method two failed
```

If we control `nested_may_fail` but do not control the implementations of `method_one` or `method_two`, it is difficult to handle each method failure without writing `tryCatch` logic in-place for each method.

``` r
handle_method_one_fail <- function(e) {
  message('caught failure one')
}

handle_method_two_fail <- function(e) {
  message('caught failure two')
}

nested_may_fail <- function(x = TRUE) {
  if (x) {
    tryCatch(method_one(),
             error = handle_method_one_fail
             )
  } else {
    tryCatch(method_two(),
             error = handle_method_two_fail
             )
  }
}

nested_may_fail()
#> caught failure one
```

Using `result`s and `error`s allows us to take ownership of errors and centralize how we handle the output.

``` r

nested_may_fail <- function(x = TRUE) {
  if (x) {
    try_result(method_one(), .err_type = "method_one")
  } else {
    try_result(method_two(), .err_type = "method_two")
  }
}
```

``` r
result <- nested_may_fail()

if (is_err(result)) {
  switch(result$error_type,
         method_one = message("caught failure one"),
         method_two = message("caught failure two"),
         generic_result_error = stop(result),
         stop("Unknown error type")
         )
} else {
  result <- unwrap(result)
}
#> caught failure one
# Continue with success logic
```

## Conversion Methods

### Option to Result

``` r
ok_or(some(1))
#> Result<ok(numeric)>
#> [1] 1
```

``` r
ok_or(none)
#> Result<Error>
#>   Error<generic_result_error>
#>     'generic_result_error'
```

### Result to Option

``` r
as_option(ok(1))
#> some(numeric)
#> [1] 1
```

``` r
as_option(error())
#> None
```

``` r
as_option(
  try_result({
    stop("oh no!")
  })
)
#> None
```

### Differences from prior implementations

For better or for worse, R's type system allows amazing flexibility often allowing things to "just work". This however doesn't work well for a data structure (like an `option`) that we want to **force** users to handle.

`sometype`'s `option`s are designed for minimal compatability with the rest of the R ecosystem. The goal is that users **must** handle `options` before actual work can be done on them. Other packages do not implement this behavior.

To demonstrate:

``` r
optional_five <- optional::option(5)
just_five <- maybe::just(5)
some_five <- sometype::some(5)
```

`optional` propagates the `option` type, but allows computation.

``` r
optional_five + 1
#> [1] 6
```

``` r
optional::none + 1
#> [1] "None"
```

`maybe` errors on some operations.

``` r
# This errors! Good!
just_five + 1
#> Error in just_five + 1: non-numeric argument to binary operator
```

But supports others:

``` r
# Oh no!
just_five[1]
#> $type
#> [1] "just"
```

``` r
# Oh no!
as.character(just_five)
#> [1] "just" "5"
```

`sometype` should fail on all base R operations

``` r
some_five + 1
#> Error: Cannot use + on Option<Some>
```

``` r
some_five[1]
#> Error: Cannot use [ on Option<Some>
```

``` r
as.character(some_five)
#> Error: Cannot convert Option<Some> to character.
```

If an `option` or `result` can be provided as a valid argument to a function that does not handle them and produce no errors: that's probably a [bug](https://github.com/snystrom/sometype/issues).

## TODO's

-   Boolean operators for Result & Option (atm they are forbidden)
-   Comparison operators for Result & Option (atm they are forbidden)
-   Still some weirdness with the `error` impl, it could be better. (should we store & throw the original condition somehow?)

-   I'm not sold whether S3 is the right impl. May test an R6 version so it is more clear what methods are allowed on Result vs Option, etc.
