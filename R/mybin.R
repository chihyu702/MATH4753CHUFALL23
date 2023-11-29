#' mybin
#'
#' perform a binomial simulation and create a barplot of the proportions of successes
#'
#' @param iter number of iterations for the binomial simulation.
#' @param n sample size for each trial
#' @param p probability of success in each binomial trial
#'
#' @importFrom graphics barplot
#'
#' @return A barplot of the proportions of successes and a table of success counts.
#' @export
#'
#' @examples mybin(iter = 100, n = 20, p = 0.6)
mybin=function(iter=100,n=10, p=0.7){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)

  #Make a vector to hold the number of successes in each trial
  succ=c()
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))

    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }

  # Create a table of successes
  succ.tab = table(factor(succ, levels=0:n))

  # Plot the barplot of the proportions
  graphics::barplot
  barplot(succ.tab/iter, main="Binomial simulation", xlab="Number of successes", ylab="Proportion")

  return(succ.tab/iter)
}
