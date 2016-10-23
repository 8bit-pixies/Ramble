context("Test repetition functions")

test_that("some", {
  expect_equal(many(Digit())("a123"),
               list(result=NULL, leftover="a123"))
  expect_equal(many(Digit())("123"),
               list(result=list("1", "2", "3", NULL),
                    leftover=""))

  expect_equal(some(Digit())("a123"),
               list())
  expect_equal(some(Digit())("1a23"),
               list(result=list("1", NULL),
                    leftover="a23"))
  expect_equal(some(Digit())("123a"),
               list(result=list("1", "2", "3", NULL),
                    leftover="a"))
})