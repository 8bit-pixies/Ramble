# utils

#' Unlist is the same as unlist, but doesn't recurses all the way to
#' preserve the type. This function is not well optimised.
#' 
#' @param a.list is a list to flatten
Unlist <- function(a.list) {
  hasLowerLevel = TRUE
  while(hasLowerLevel) {
    a.list1 <- unlist(a.list, recursive=FALSE, use.names=FALSE)
    if (is(a.list1, 'list')) {
      a.list <- a.list1
    }
    else {
      hasLowerLevel = FALSE
      return(a.list)
    }
  }
  warning("loop should have returned a list!")
  return(a.list)
}
