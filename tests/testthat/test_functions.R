library(Ramble)

test_that("returns", {
  expect_equal(returns("1") ("abc")$result, "1")
  expect_equal(returns("1") ("abc")$leftover, "abc")
})

test_that("items", {
  expect_equal(item() ("abc")$result, "a")
  expect_equal(item() ("abc")$leftover, "bc")
})

test_that("then", {
  expect_equal((item() %>>=% returns("123") ) ("abc")$result[1], "a")
  expect_equal((item() %>>=% returns("123") ) ("abc")$result[2], "123")
  expect_equal((item() %>>=% returns("123") ) ("abc")$leftover, "bc")
})

test_that("do", {
  expect_equal(do(x=item(), item(), y=item(), f = function(x,y) {c(x,y)}) ("abcdef")$result[1], "a")
  expect_equal(do(x=item(), item(), y=item(), f = function(x,y) {c(x,y)}) ("abcdef")$result[2], "c")
  expect_equal(do(x=item(), item(), y=item(), f = function(x,y) {c(x,y)}) ("abcdef")$leftover, "def")
})

test_that("choice", {
  expect_equal((item() %+++% returns("2")) ("abcdef")$result, "a")
  expect_equal((Digit() %+++% returns("2")) ("abcdef")$result, "2")
})

test_that("many", {
  expect_equal(length(many(Digit()) ("123abc")$result), 3)
  expect_equal(many(Digit()) ("123abc")$leftover, "abc")
})





