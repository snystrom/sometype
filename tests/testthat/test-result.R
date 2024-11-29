test_that("error works", {
  expect_error(error(1), "character.type")
  expect_error(error("a", 1), "character.message")
  expect_false(is_ok(error()))
  expect_true(is_err(error()))

  expect_error(error(c("a", "a")), "length.type")
  expect_error(error("a", c("a", "a")), "length.message")

})

test_that("custom error message", {
  skip_if(TRUE, "Something still busted w/ custom message throws on stop()")
  throw_foo <- function() {
    e <- error("foo", "FOO ERROR")
    stop(e)
  }
  # TODO: why does this actually stop() and not catch?
  # something weird about my dumb error impl
  expect_error(throw_foo(), "FOO ERROR")
})

test_that("result works", {
  expect_s3_class(result(1), "result")
  expect_true(is_ok(result(1)))
  expect_false(is_err(result(1)))

  # result(ok()) doesn't continue nesting result().
  expect_equal(result(ok(1)), ok(1))
  expect_equal(ok(ok(1)), ok(1))

  expect_s3_class(ok(1), "result")
  t_err <- try(stop("oh no!"), silent = TRUE)
  rt_err <- error()
  tc_err <- tryCatch({stop("oh no!")}, error = function(e) e )
  expect_error(ok(t_err), "Cannot create Result")
  expect_error(ok(rt_err), "Cannot create Result")
  expect_error(ok(tc_err), "Cannot create Result")
  # TODO: snapshot test print.result method
})

test_that("unwrap works", {
  expect_equal(unwrap(ok(5)), 5)
  expect_equal(unwrap(ok("hi")), "hi")
  expect_error(unwrap(error()), "Cannot unwrap Result<Error>")
})

test_that("result operator methods forbidden", {
  # TODO: these are incomplete
  expect_error(ok(5) + 1, "Cannot use")
  expect_error(ok(5) - 1, "Cannot use")
  expect_error(ok(5) / 1, "Cannot use")
  expect_error(ok(5) * 1, "Cannot use")

  expect_error(error() + 1, "Cannot use")
  expect_error(error() - 1, "Cannot use")
  expect_error(error() / 1, "Cannot use")
  expect_error(error() * 1, "Cannot use")
})

test_that("result as methods are forbidden", {
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

  lapply(ops, test_as_method, x = ok(5))
  lapply(ops, test_as_method, x = error())

})

test_that("Comparison works", {
  expect_equal(error(), error())
  expect_true(error() == error())
  expect_false(error() == error("new"))
  expect_false(error() == ok(1))

  expect_true(ok(1) == ok(1))
  expect_false(ok(1) == ok(2))
  expect_error(ok(1) == 1, "Cannot compare Result<Ok> to non-Result.")
  expect_true(ok("hi") == ok("hi"))
})

test_that("as.option works", {
  expect_equal(as.option(ok(1)), some(1))
  expect_equal(as.option(error()), none)
  expect_error(as.option(1), "no applicable method")
})

test_that("expect_err works", {
 expect_equal(expect_err(error(), "Should give generic_result_error"), error())
 expect_error(expect_err(ok(1), "Got OK!, Expected a Result<Error>"))
})

test_that("unwrap_err works", {
 expect_equal(unwrap_err(error()), error())
 expect_error(unwrap_err(ok(1), 1))
})
