
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sometype

<!-- badges: start -->

<!-- badges: end -->

This is a collection of experiments implementing Option & Result types
in (mostly) base R. The API draws most of its inspiration from the
[Rust](https://www.rust-lang.org/) standard library
([Option<T>](https://doc.rust-lang.org/std/option/), [Result\<T,
E\>](https://doc.rust-lang.org/std/result/)).

Other packages have done something for `Option`
([maybe](https://cran.r-project.org/package=maybe),
[optional](https://cran.r-project.org/package=optional), among others).

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("snystrom/sometype")
```

## R Option<T>

`sometype` implements an `Option` S3 type:

Options are useful for wrapping values that can be missing (`None`) or
real (`Some`), forcing the user to handle them explicitly.

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

Users can manually build `Option<T>` objects using `some()` and `none`,
or by using the `option()` constructor.

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

Users can provide custom predicates to produce `Option<None>` from
values.

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

### Differences from prior implementations

For better or for worse, R’s type system allows amazing flexibility
often allowing things to “just work”. This however doesn’t work well for
a data structure (like an `option`) that we want to **force** users to
handle.

`sometype`’s `option`s are designed for minimal compatability with the
rest of the R ecosystem. The goal is that users **must** handle
`options` before actual work can be done on them. Other packages do not
implement this behavior.

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

If an `option` can be provided as a valid argument to a function that
does not handle them and produce no errors: that’s probably a
[bug](https://github.com/snystrom/sometype/issues).
