#' Linear Model Calculation
#'
#' Calculate a linear regression model using the 'lm' function.
#'
#' @param x
#'
#' @return A linear regression model.
#' @export
#'
#' @examples (lab3(1:10))
lab3=function(x){
  ht.lm=with(fin.df, lm(HEAT~RATIO))
}
