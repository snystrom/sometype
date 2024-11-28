test_that("error works", {
  expect_error(error(1), "character.type")
  expect_error(error("a", 1), "character.message")

  expect_error(error(c("a", "a")), "length.type")
  expect_error(error("a", c("a", "a")), "length.message")

  expect_error(stop(error("foo", "FOO ERROR")), "FOO ERROR")
})
