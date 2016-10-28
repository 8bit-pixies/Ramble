library(Ramble)

test_that("succeed", {
  expect_equal(succeed("1")("abc"),
               list(result="1", leftover="abc"))
})

test_that("item", {
  expect_equal(item()("abc"),
               list(result="a", leftover="bc"))
})

test_that("then", {
  expect_equal((item() %then% succeed("123"))("abc"),
               list(result=list("a", "123"),
                    leftover="bc"))
  ## Differentiate "then" from "thentree"
  newparam <- function(x) {
    function(string) {
      ret <- succeed(x)(string)
      ret$result <- list(value=ret$result,
                         more=TRUE)
      class(ret$result) <- "newparam"
      ret
    }
  }
  expect_equal((item() %then% newparam("123"))("abc"),
               list(result=list("a",
                                structure(list(value="123", more=TRUE),
                                          .Names=c("value", "more"),
                                          class="newparam")),
                    leftover="bc"))
})

test_that("thentree", {
  expect_equal((item() %thentree% succeed("123"))("abc"),
               list(result=list("a", "123"),
                    leftover="bc"))
  newparam <- function(x) {
    function(string) {
      ret <- succeed(x)(string)
      ret$result <- list(value=ret$result,
                         more=TRUE)
      ret
    }
  }
  expect_equal((item() %thentree% newparam("123"))("abc"),
               list(result=list("a", list(value="123", more=TRUE)),
                    leftover="bc"))
})

test_that("alternation", {
  expect_equal((item() %alt% succeed("2"))("abcdef"),
               list(result="a", leftover="bcdef"))
  expect_equal((Digit() %alt% succeed("2"))("abcdef"),
               list(result="2", leftover="abcdef"))
})

test_that("many", {
  expect_equal(length(unlist(many(Digit())("123abc")$result)), 3)
  expect_equal(many(Digit())("123abc"),
               list(result=list("1", "2", "3", NULL),
                    leftover="abc"))
})

test_that("identifier", {
  expect_equal(identifier()("  variable1  "),
               list(result=list("variable1"),
                    leftover=""))
})

test_that("token", {
  expect_equal(token(Digit())("123"),
               list(result=list("1"),
                    leftover="23"))
  expect_equal(token(Digit())(" 123"),
               list(result=list("1"),
                    leftover="23"))
  expect_equal(token(Digit())(" 1 23"),
               list(result=list("1"),
                    leftover="23"))
  expect_equal(token(Digit())(" 1   23"),
               list(result=list("1"),
                    leftover="23"))
  expect_equal(token(Digit())(" a 23"),
               list())
  ## Keep attributes now
  newparam <- function(x) {
    function(string) {
      ret <- succeed(x)(string)
      ret$result <- list(value=ret$result,
                         more=TRUE)
      ret
    }
  }
  expect_equal(token(String("abc") %thentree% newparam("123"))(" abc "),
               list(result=list("abc", list(value="123", more=TRUE)),
                    leftover=""))
})
