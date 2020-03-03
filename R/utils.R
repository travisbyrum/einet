#' @importFrom stats setNames
#'
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

### default pipe ------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a) || is.na(a)) b else a

### is truthy ---------------------------------------------------------------------------
is_truthy <- function(x) {
  if (is.null(x))
    return(FALSE)

  !identical(is.na(x), TRUE) &&
    !identical(length(x), 0L) &&
    !identical(nchar(x), 0L) &&
    !identical(x, FALSE)
}

