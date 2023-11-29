#' myboot2
#'
#' bootstrap analysis on a given dataset
#'
#' @param x the data
#' @param fun function using/the value we want to calculate
#' @param iter number of iterations
#' @param alpha bootstrap interval
#' @param cx size of the text in the graph
#' @param xlab label for the x-axis on the histogram
#' @param ... additional parameters
#'
#' @importFrom stats quantile
#' @importFrom graphics segments
#'
#' @return A list containing the calculated bootstrap interval, the function used, the input data, and the vector of bootstrap statistics.
#' @export
#' @examples myboot2(iter = 10000, x=rnorm(50), fun = "mean", alpha = 0.05, xlab = "mean")
myboot2<-function(x,fun="mean",iter=10000,alpha=0.05,cx=1.5,xlab=fun,...){  #Notice where the ... is repeated in the code
  n=length(x)   #sample size

  y=sample(x,n*iter,replace=TRUE)
  rs.mat=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  xstat=apply(rs.mat,2,fun) # xstat is a vector and will have iter values in it
  ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  # A histogram follows
  # The object para will contain the parameters used to make the histogram
  para=hist(xstat,freq=FALSE,las=1,
            main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
            xlab = fun,
            ...)

  #mat will be a matrix that contains the data, this is done so that I can use apply()
  mat=matrix(x,nrow=length(x),ncol=1,byrow=TRUE)

  #pte is the point estimate
  #This uses whatever fun is
  pte=apply(mat,2,fun)
  abline(v=pte,lwd=3,col="Black")# Vertical line
  segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
  text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=cx)
  text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=cx)

  # plot the point estimate 1/2 way up the density
  text(pte,max(para$density)/2,round(pte,2),cex=cx)

  invisible(list(ci=ci,fun=fun,x=x, xstat = xstat))# Some output to use if necessary
}
