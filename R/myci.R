#' @title 95 Percent Confidence Interval
#'
#' @param x Distribution
#'
#' @return Confidence interval for the mean of the distribution
#' @export
#'
#' @examples
#' \dontrun{x <- rnorm(20, 10, 10); myci(x=x)}
myci = function(x) {
  ci = c()
  ci[1] = mean(x) - qt(0.975, length(x) - 1)*sd(x)/sqrt(length(x))
  ci[2] = mean(x) + qt(0.975, length(x) - 1)*sd(x)/sqrt(length(x))
  ci
}
