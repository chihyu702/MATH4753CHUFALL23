#' create z from mpg
#'
#' @param x  X quantitative vector
#'
#' @importFrom stats sd
#' @return a list containing z and x
#' @export
#'
#' @examples (z(1:10))
z <- function(x){
  mpg <- NULL
  z <-  (mpg-mean(mpg))/sd(mpg)
  list (z = z, x = mpg)
}
