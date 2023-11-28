#' binCLT
#'
#' @param n sample size
#' @param iter number of iterations
#' @param p the probability of success in each binomial trial
#' @param ... additional parameters
#'
#' @importFrom stats rbinom
#' @importFrom graphics hist
#' @importFrom stats dnorm
#'
#' @return A histogram of sample means and a CLT-based normal curve.
#' @export
#'
#' @examples binCLT(n = 30, iter = 100, p = 0.4)
binCLT=function(n,iter,p=0.5,...){

  y=rbinom(n*iter,size=n,prob=p)

  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)

  w=apply(data,2,mean)

  param=hist(w,plot=FALSE)

  ymax=max(param$density)

  ymax=1.1*ymax

  hist(w,freq=FALSE,  ylim=c(0,ymax),
       main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
       xlab="Sample mean",...)

  curve <- NULL
  x <- NULL

  curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3)

}
