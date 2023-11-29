#' ntickets
#'
#' Trying to find the optimization number of ticket to sell to avoid overbooking problem
#'
#' @param N number of seats on the flight
#' @param gamma   probability of overbooking
#' @param p probability of a passenger will show
#'
#' @importFrom stats pbinom
#' @importFrom pnorm
#' @importFrom stats uniroot
#' @importFrom graphics abline
#' @return  number of seats sold
#'          (nd is calculated by the discrete distribution and nc is calculated by the discrete distribution )
#' @export
#'
#' @examples ntickets(200, 0.02, 0.95)
ntickets <- function(N, gamma, p) {

  # Define the discrete objective function
  Discrete <- function(n, N, gamma, p) {
    1 - gamma - pbinom(N, n, p)
  }

  # Generate a sequence of possible nd
  # nd must greater than N, so set sequence from N to 1.1N, increasing by 1
  nValue <- seq(N , N *1.1, by = 1)
  # plug nValue to discrete objective function and get a sequence objectiveD
  objectiveD <- sapply(nValue, function(n) Discrete(n, N, gamma, p))
  # find the absolute minimum value, which is nd
  nd <- nValue[which.min(abs(objectiveD))]

  # Create the plot
  # x=possible nd = nValue, y=objectiveD
  titleD <- paste0("Objective Vs n (Discrete Case) to find optimal tickets sold\n
  (", nd, ")gamma=", gamma, " N=", N, "p=",p)
  plot(nValue, objectiveD, type="b", pch=20,
       xlab="n", ylab="Objective", main=titleD)
  # horizontal line
  abline(h=0, col="Red")
  # Vertical line
  abline(v=nd, col="Red", lty=2)


  # Define the normal approximation (continuous)
  Continuous <- function(n, N, gamma, p) {
    (1-gamma)-pnorm(N+0.5, mean = n*p, sd = sqrt(n*p*(1-p)))
  }
  # Generate a sequence of possible nc
  nValue <- seq(N , N *1.1)
  # plug nValue to discrete objective function and get a sequence objectiveC
  objectiveC <- sapply(nValue, function(n) Continuous(n,N, gamma, p))
  # Find root for continuous objective function, which is nc
  nc <- uniroot(function(n) Continuous(n, N, gamma, p), interval = c(N, 1.1*N))$root

  # Create the plot
  # x=possible nc = nValue, y=objectiveC
  titleC <- paste0("Objective Vs n (Continuous Case) to find optimal tickets sold\n
                   (nc=", nc, ") gamma=", gamma, " N=", N, "p=", p)
  plot(nValue,objectiveC,
       type ="l",
       xlab="n", ylab="Objective", main=titleC)
  # horizontal line
  abline(h=0, col="Blue")
  # Vertical line
  abline(v=nc, col="Blue", lty=2)


  # Creating the named list
  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)

  # Return the result
  return(result)
}
