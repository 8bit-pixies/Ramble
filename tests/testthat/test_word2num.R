library(Ramble)

# we might have hyphens or spaces, 
# e.g. ninety-one or ninety one
remove_space_hyphen <- maybe(token(String("-"))) %using% function(...) return(0)

token_string <- function(x) token(String(x))

unit_definition <- (remove_space_hyphen %alt% succeed(NULL)) %then% (
  (token_string("ten") %using% function(...) return(10)) %alt% 
    (token_string("eleven") %using% function(...) return(11)) %alt% 
    (token_string("twelve") %using% function(...) return(12)) %alt% 
    (token_string("thirteen") %using% function(...) return(13)) %alt% 
    (token_string("fourteen") %using% function(...) return(14)) %alt% 
    (token_string("fifteen") %using% function(...) return(15)) %alt% 
    (token_string("sixteen") %using% function(...) return(16)) %alt% 
    (token_string("seventeen") %using% function(...) return(17)) %alt% 
    (token_string("eighteen") %using% function(...) return(18)) %alt% 
    (token_string("nineteen") %using% function(...) return(19)) %alt%
  (token_string("zero") %using% function(...) return(0)) %alt% 
    (token_string("oh") %using% function(...) return(0)) %alt% 
    (token_string("zip") %using% function(...) return(0)) %alt% 
    (token_string("zilch") %using% function(...) return(0)) %alt% 
    (token_string("nada") %using% function(...) return(0)) %alt% 
    (token_string("one") %using% function(...) return(1)) %alt% 
    (token_string("two") %using% function(...) return(2)) %alt% 
    (token_string("three") %using% function(...) return(3)) %alt% 
    (token_string("four") %using% function(...) return(4)) %alt% 
    (token_string("five") %using% function(...) return(5)) %alt% 
    (token_string("six") %using% function(...) return(6)) %alt% 
    (token_string("seven") %using% function(...) return(7)) %alt% 
    (token_string("eight") %using% function(...) return(8)) %alt% 
    (token_string("nine") %using% function(...) return(9)) 
)

tens_definition <- (
  (token_string("ten")     %using% function(...) return(10)) %alt%
    (token_string("twenty")  %using% function(...) return(20)) %alt%
    (token_string("thirty")  %using% function(...) return(30)) %alt%
    (token_string("forty")   %using% function(...) return(40)) %alt%
    (token_string("fourty")  %using% function(...) return(40)) %alt% 
    (token_string("fifty")   %using% function(...) return(50)) %alt%
    (token_string("sixty")   %using% function(...) return(60)) %alt%
    (token_string("seventy") %using% function(...) return(70)) %alt%
    (token_string("eighty")  %using% function(...) return(80)) %alt%
    (token_string("ninety")  %using% function(...) return(90))
)

# optional tens_defintion + units
word2num <- ((tens_definition %alt% succeed(NULL)) %then% unit_definition) %using% function(x) return(sum(unlist(x)))


test_that("word2num", {
  expect_equal(word2num("ninety one")$result, 91)
  expect_equal(word2num("ninety-two")$result, 92)
  expect_equal(word2num("ninety tone"), list())
  expect_equal(word2num("forty six")$result, 46)
  expect_equal(word2num("nineteen")$result, 19)
  expect_equal(word2num("thirty seven")$result, 37)
})
