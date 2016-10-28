library(Ramble)

test_that("empty strings (ie. character(0)) are treated correctly", {
  expect_equal(item()(character(0)), NULL)
  expect_equal(satisfy()(character(0)), list())
})

test_that("literal correctly parses character", {
  expect_equal(literal("a")("a"),
               list(result="a", leftover=""))
}) 

test_that("then correctly works when we have empty 2nd predicate", {
  expect_equal((literal("a") %then% item())("a"), list())
}) 

test_that("using works as expected", {
  expect_equal((literal("a") %using% toupper)("abc"),
               list(result="A", leftover="bc"))
})

test_that("using correctly returns list when there is no result",{
  expect_equal(((literal("a") %then% item()) %using% toupper)("a"),
               list())
})

test_that("some has the correct leftover when it matches multiple things", {
  expect_equal((some(Digit())("123abc")),
               list(result=list("1", "2", "3", NULL), leftover="abc"))
})

test_that("derived primatives work", {
  expect_equal(Lower()("abc"),
               list(result="a", leftover="bc"))
  expect_equal(Upper()("Abc"),
               list(result="A", leftover="bc"))
  expect_equal(Alpha()("abc"),
               list(result="a", leftover="bc"))
  expect_equal(AlphaNum()("abc123"),
               list(result="a", leftover="bc123"))
  expect_equal(AlphaNum()("123"),
               list(result="1", leftover="23"))
  expect_equal(String("123")("123 abc"),
               list(result="123", leftover=" abc"))
  expect_equal(String("")("abc"),
               list(result=NULL, leftover="abc"))
  expect_equal(nat()("123 + 456"),
               list(result="123", leftover=" + 456"))
  expect_equal(natural()("123"),
               list(result=list("123"), leftover=""))
  expect_equal(symbol("[")("  [123]"),
               list(result=list("["), leftover="123]"))
})

