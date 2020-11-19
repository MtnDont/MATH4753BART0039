#' @title Function for printing probability on curves
#'
#' @param mu Mean
#' @param sigma Standard Deviation
#' @param a
#'
#' @return Displays a curve with a shaded area between the curve and the x axis
#' from -inf to x = a, and prints the probability P(X<=a)
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=10,sigma=5,a=6)}
myncurve = function(mu, sigma, a) {
  curve(dnorm(x, mean=mu, sd=sigma),
        xlim = c(mu - 3 * sigma, mu + 3 * sigma))
  xcurve = seq(mu - 3 * sigma,a,length=1000)
  ycurve = dnorm(xcurve,mu,sigma)
  polygon(c(mu - 3 * sigma,xcurve,a), c(0,ycurve,0), col = "Red")
  prob = pnorm(a, mu, sigma) - pnorm(mu - 3 * sigma, mu, sigma)
  prob = round(prob,4)
  prob
}
