#' logbin
#'
#' natural logarithm of binomial probabilities
#'
#' @param x vector
#' @param param the probability of success in a single trial.
#'
#' @importFrom stats dbinom
#'
#' @return
#' @export
#'
#' @examples logbin(x=c(9,9,1,9,9,9),param=seq(0,1,length=1000))
logbin <- function(x=c(3,3,4,3,4,5,5,4),param=seq(0,1,length=1000)){
  log(dbinom(x,prob=param,size=10))
}
