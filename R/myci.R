#' myci
#'
#'Calculate 95% Confidence Interval for the Mean
#'
#' @param x vector
#'
#' @importFrom stats qt
#'
#' @return
#' @export
#'
#' @examples
myci<-function(x){
  mu <- mean(x)
  s <- sd(x)
  n <- length(x)
  t<- qt(1-0.05/2, n-1)
  mp <- c(-1, 1)
  ci95 <- mu+mp*t*s/sqrt(n)
  return (ci95)
}
