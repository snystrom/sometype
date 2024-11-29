test_that("some() constructors work", {
  expect_error(some(none), "Cannot create Some from None")
  expect_true(is_some(some(5)))
  expect_false(is_none(some(5)))
  expect_s3_class(some(5), "option")
  # Invariant holds
  expect_equal(some(some(5)), some(5))
})

test_that("some equality works", {
  expect_error(some(5) == 5, "Cannot unwrap")
  expect_equal(some(5), some(5))
  expect_true(some(5) == some(5))
})

test_that("unwrap", {
  expect_error(unwrap(5), "Cannot unwrap")
  expect_error(unwrap(none), "got None")
  expect_error(!none, "got None")

  x <- some(5)
  expect_equal(unwrap(x), 5)
  expect_equal(!x, 5)
})

test_that("unwrap_or", {
  expect_equal(unwrap_or(1, 5), 5)
  expect_equal(unwrap_or(some(1), 5), 1)
  expect_equal(unwrap_or(none, 5), 5)
})

test_that("unwrap_or_else", {

  fun <- function() {
    return(5)
  }

  expect_equal(unwrap_or_else(1, fun), 5)
  expect_equal(unwrap_or_else(some(1), fun), 1)
  expect_equal(unwrap_or_else(none, fun), 5)
})

test_that("expect", {
  expect_error(expect(5, "expected some"), "expected some")
  expect_equal(expect(some(1), "expected some"), 1)
  expect_error(expect(none, "expected some"), "expected some")
})


test_that("option converts", {

  expect_equal(option(5), some(5))
  expect_equal(option(NULL), none)
  expect_equal(option(NA), none)
  expect_equal(option(), none)
  expect_equal(option(5, .none_predicates = list(function(x){x == 5})), none)
})

test_that("option operator methods forbidden", {
  # TODO: these are incomplete
  expect_error(some(5) + 1, "Cannot use")
  expect_error(some(5) - 1, "Cannot use")
  expect_error(some(5) / 1, "Cannot use")
  expect_error(some(5) * 1, "Cannot use")

  expect_error(none + 1, "Cannot use")
  expect_error(none - 1, "Cannot use")
  expect_error(none / 1, "Cannot use")
  expect_error(none * 1, "Cannot use")
})

test_that("option as methods are forbidden", {
  test_as_method <- function(method, x) {
    expect_error(method(x), "Cannot convert")
  }

  # TODO: incomplete
  ops <- list(
    as.character,
    as.integer,
    as.numeric,
    as.null,
    as.double,
    as.logical
  )

  lapply(ops, test_as_method, x = some(5))
  lapply(ops, test_as_method, x = none)

})
