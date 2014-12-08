library(Ramble)

test_that("succeed", {
  expect_equal(succeed("1") ("abc")$result, "1")
  expect_equal(succeed("1") ("abc")$leftover, "abc")
})

test_that("item", {
  expect_equal(item() ("abc")$result, "a")
  expect_equal(item() ("abc")$leftover, "bc")
})

test_that("then", {
  expect_equal((item() %then% succeed("123") ) ("abc")$result[[1]], "a")
  expect_equal((item() %then% succeed("123") ) ("abc")$result[[2]], "123")
  expect_equal((item() %then% succeed("123") ) ("abc")$leftover, "bc")
})

test_that("alternation", {
  expect_equal((item() %alt% succeed("2")) ("abcdef")$result, "a")
  expect_equal((Digit() %alt% succeed("2")) ("abcdef")$result, "2")
})

test_that("many", {
  expect_equal(length(unlist(many(Digit()) ("123abc")$result)), 3)
  expect_equal(many(Digit()) ("123abc")$leftover, "abc")
})

test_that("identifier", {
  expect_equal(identifier()("  variable1  ")$result, "variable1")
  expect_equal(identifier()("  variable1  ")$leftover, "")
})





