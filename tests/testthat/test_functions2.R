library(Ramble)

test_that("empty strings (ie. character(0)) are treated correctly", {
  expect_equal(item()(character(0)), NULL)
  expect_equal(satisfy()(character(0)), list())
})

test_that("literal correctly parses character", {
  expect_equal(literal("a")("a")$result, "a")
  expect_equal(literal("a")("a")$leftover, "")
}) 

test_that("then correctly works when we have empty 2nd predicate", {
  expect_equal((literal("a") %then% item()) ("a"), list())
}) 

test_that("using works as expected", {
  expect_equal((literal("a") %using%  toupper)("abc")$result, "A")
})

test_that("using correctly returns list when there is no result",{
  expect_equal(((literal("a") %then% item()) %using% toupper)("a"), list())
})

test_that("some has the correct leftover when it matches multiple things", {
  expect_equal((some(Digit()) ("123abc"))$leftover, "abc")
})

test_that("derived primatives work", {
  expect_equal(Lower()("abc")$result, "a")
  expect_equal(Upper()("Abc")$result, "A")
  expect_equal(Alpha()("abc")$result, "a")
  expect_equal(AlphaNum()("abc123")$result, "a")
  expect_equal(AlphaNum()("123")$result, "1")
  expect_equal(String("123")("123 abc")$result, "123")
  expect_equal(String("")("abc")$result, NULL)
  expect_equal(nat() ("123 + 456")$result, "123")
  expect_equal(natural()("123")$result, "123")
  expect_equal(symbol("[") ("  [123]")$result, "[")
})

