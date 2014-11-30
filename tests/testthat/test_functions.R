library(Ramble)

test_that("succeed", {
  expect_equal(succeed("1") ("abc")$result, "1")
  expect_equal(succeed("1") ("abc")$leftover, "abc")
})

test_that("items", {
  expect_equal(item() ("abc")$result, "a")
  expect_equal(item() ("abc")$leftover, "bc")
})

test_that("then", {
  expect_equal((item() %then% succeed("123") ) ("abc")$result[1], "a")
  expect_equal((item() %then% succeed("123") ) ("abc")$result[2], "123")
  expect_equal((item() %then% succeed("123") ) ("abc")$leftover, "bc")
})

test_that("do", {
  expect_equal(do(x=item(), item(), y=item(), f = function(x,y) {c(x,y)}) ("abcdef")$result[1], "a")
  expect_equal(do(x=item(), item(), y=item(), f = function(x,y) {c(x,y)}) ("abcdef")$result[2], "c")
  expect_equal(do(x=item(), item(), y=item(), f = function(x,y) {c(x,y)}) ("abcdef")$leftover, "def")
})

test_that("alt", {
  expect_equal((item() %+++% succeed("2")) ("abcdef")$result, "a")
  expect_equal((Digit() %+++% succeed("2")) ("abcdef")$result, "2")
})

test_that("many", {
  expect_equal(length(many(Digit()) ("123abc")$result), 3)
  expect_equal(many(Digit()) ("123abc")$leftover, "abc")
})





