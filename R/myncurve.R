#' myncurve
#'
#' @param mu mean (mu) of the normal distribution.
#' @param sigma standard deviation (sigma) of the normal distribution
#' @param a a point up to which you want to calculate the area under the curve.
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#' @return the area (probability, P(X<=a)) of the normal distribution
#' @export
#'
#' @examples myncurve(4,4,5)
myncurve = function(mu, sigma, a){
  # make the title of the plot
  mustr <- as.character(mu)
  sigmastr <- as.character(sigma)
  astr <- as.character(a)
  title <- paste("N(mu=", mustr, ",sigma=", sigmastr, ");P(Y<=", astr, ")")
  # plot the curve, with 3 standard deviation
  curve(dnorm(x, mean=mu, sd=sigma),
  xlim = c(mu-3*sigma, mu + 3*sigma),
  ylab = "Normal Density", xlab = "Y",
  main = title
  )
  x <- NULL
  # plot the region
  xcurve <- seq(mu-3*sigma, a, length = 1000)
  ycurve <- dnorm(xcurve, mu, sigma)
  polygon( x = c(mu-3*sigma, xcurve, a),
           y = c(0, ycurve, 0), col = "Pink")
  # calculate the region (probability)
  probability <- pnorm(a, mu, sigma)
  area <- round (probability, 4)
  # add the text to the graph
  text(x = mu, y = 0.5 * dnorm(a/2, mu, sigma), paste0("Area = ", area))
}
