#' logbin
#'
#' @param x
#' @param param
#'
#' @return
#' @export
#'
#' @examples logbin(x=c(9,9,1,9,9,9),param=seq(0,1,length=1000))
logbin <- function(x,param){
  log(dbinom(x,prob=param,size=10))
}
