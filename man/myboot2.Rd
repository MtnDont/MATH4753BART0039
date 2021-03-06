% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myboot2.R
\name{myboot2}
\alias{myboot2}
\title{Bootstrap Re-sampling Procedure}
\usage{
myboot2(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...)
}
\arguments{
\item{iter}{Iteration Size}

\item{x}{Vector of samples}

\item{fun}{Function to use}

\item{alpha}{Alpha of confidence interval}

\item{cx}{Numeric Character Expansion Factor for text function}

\item{...}{Further graphical parameters}
}
\value{
Returns a histogram of bootstrap sample statistics and possible a list of the quantile, function, and samples.
}
\description{
Bootstrap Re-sampling Procedure
}
