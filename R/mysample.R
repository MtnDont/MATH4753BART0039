#' @title A function for showing a barplot of samples
#'
#' @param n Sample Size
#' @param iter Number of iterations
#' @param time Delay between iterations
#'
#' @return Set of iter barplots
#' @export
#'
#' @example
#' \dontrun{mysample(n=20, iter=3, time=1)}
mysample = function(n, iter=10, time=0.5) {
  for (i in 1:iter) {
    #make a sample
    s = sample(1:10, n, replace=TRUE)

    # turn the sample into a factor
    sf = factor(s, levels=1:10)

    #make a barplot
    barplot(table(sf)/n, beside=TRUE, col=rainbow(10),
            main = paste("Example sample()", " iteration ", i, " n= ", n, sep=""),
            ylim = c(0, 0.2))

    #release the table
    Sys.sleep(time)
  }
}
