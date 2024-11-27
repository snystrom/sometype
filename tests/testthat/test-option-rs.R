test_that("some() constructors work", {
  expect_error(some(none), "Cannot create Some from None")
  expect_true(is_some(some(5)))
  expect_false(is_none(some(5)))
  expect_s3_class(some(5), "option")
})

test_that("some equality works", {
  expect_error(some(5) == 5, "Cannot unwrap")
  expect_equal(some(5), some(5))
  expect_true(some(5) == some(5))
})

test_that("unwrap", {
  expect_error(unwrap(5), "Cannot unwrap")
  expect_error(unwrap(none), "got None")
})


test_that("option converts", {

  expect_equal(option(5), some(5))
  expect_equal(option(NULL), none)
  expect_equal(option(NA), none)
  expect_equal(option(), none)
  expect_equal(option(5, .none_predicates = list(function(x){x == 5})), none)
})

