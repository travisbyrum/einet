#' @importFrom stats setNames
#'
#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

### default pipe ------------------------------------------------------------------------
`%||%` <- function(a, b) if (is.null(a)) b else a

### is truthy ---------------------------------------------------------------------------
is_truthy <- function(x) {
  if (is.null(x))
    return(FALSE)

  !identical(is.na(x), TRUE) &&
    !identical(length(x), 0L) &&
    !identical(nchar(x), 0L) &&
    !identical(x, FALSE)
}

entrospy <- function(x, base = 2) {
  #"""
   # pk = asarray(pk)
   # pk = 1.0*pk / np.sum(pk, axis=axis, keepdims=True)
  #  if qk is None:
  #      vec = entr(pk)
 #   else:
   #     qk = asarray(qk)
    #    if qk.shape != pk.shape:
  #          raise ValueError("qk and pk must have same shape.")
   #     qk = 1.0*qk / np.sum(qk, axis=axis, keepdims=True)
   #     vec = rel_entr(pk, qk)
    #S = np.sum(vec, axis=axis)
    #if base is not None:
     #   S /= log(base)
   # return S
   # """

  #entropy([9/10, 1/10], base=2)

  assertthat::assert_that(is.array(x))

  x = x / sum(x)

}
