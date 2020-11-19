#' @title A function for plotting a quadratic
#'
#' @param x The diameter
#' @param qlm A quadratic linear model
#'
#' @return Response i.e. estimated Height
#' @export
#'
#' @examples
#' \dontrun{quad.lm<-lm(y~x,data=df); quadPlot(x=15, qlm=quad.lm)}
quadPlot = function(x, qlm) {
  qlm$coef[1] + qlm$coef[2] * x  + qlm$coef[3] * x^2
}
