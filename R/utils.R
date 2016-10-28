#' Unlist is the same as unlist, but doesn't recurse all the way to
#' preserve the type. This function is not well optimised.
#' 
#' @param a.list is a list to be flatten
#' @importFrom methods is
Unlist <- function(obj) {
  ret <- list()
  for (i in seq_along(obj)) {
    if (is(obj[[i]], "list") &&
        is.null(names(obj[[i]]))) {
      ret <- append(ret, Unlist(obj[[i]]))
    } else {
      ret <- append(ret, obj[i])
    }
  }
  ret
}

