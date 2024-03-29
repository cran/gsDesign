% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/toBinomialExact.R
\name{toBinomialExact}
\alias{toBinomialExact}
\title{Translate survival design bounds to exact binomial bounds}
\usage{
toBinomialExact(x, observedEvents = NULL)
}
\arguments{
\item{x}{An object of class \code{gsSurv}; i.e., an object generated by
the \code{gsSurv()} function.}

\item{observedEvents}{If NULL (default), targeted timing of analyses will come from \code{x$n.I}. 
Otherwise, this should be vector of increasing positive integers with at most 1 value \code{>= x$n.IPlan} and of length at least 2.
Only one value can be greater than or equal to \code{x$maxn.IPlan}. 
This determines the case count at each analysis performed. 
Primarily, this is used for updating a design at the time of analysis.}
}
\value{
An object of class \code{gsBinomialExact}.
}
\description{
Translate survival design bounds to exact binomial bounds
}
\details{
The exact binomial routine \code{gsBinomialExact} has requirements that may not be satisfied
by the initial asymptotic approximation. 
Thus, the approximations are updated to satisfy the following requirements of \code{gsBinomialExact}:
\code{a} (the efficacy bound) must be positive, non-decreasing, and strictly less than n.I
\code{b} (the futility bound) must be positive, non-decreasing, strictly greater than a
\code{n.I - b} must be non-decreasing and >= 0
}
\examples{
# The following code derives the group sequential design using the method
# of Lachin and Foulkes

x <- gsSurv(
  k = 3,                 # 3 analyses
  test.type = 4,         # Non-binding futility bound 1 (no futility bound) and 4 are allowable
  alpha = .025,          # 1-sided Type I error
  beta = .1,             # Type II error (1 - power)
  timing = c(0.45, 0.7), # Proportion of final planned events at interims
  sfu = sfHSD,           # Efficacy spending function
  sfupar = -4,           # Parameter for efficacy spending function
  sfl = sfLDOF,          # Futility spending function; not needed for test.type = 1
  sflpar = 0,            # Parameter for futility spending function
  lambdaC = .001,        # Exponential failure rate
  hr = 0.3,              # Assumed proportional hazard ratio (1 - vaccine efficacy = 1 - VE)
  hr0 = 0.7,             # Null hypothesis VE
  eta = 5e-04,           # Exponential dropout rate
  gamma = 10,            # Piecewise exponential enrollment rates
  R = 16,                # Time period durations for enrollment rates in gamma
  T = 24,                # Planned trial duration
  minfup = 8,            # Planned minimum follow-up
  ratio = 3              # Randomization ratio (experimental:control)
)
# Convert bounds to exact binomial bounds
toBinomialExact(x)
# Update bounds at time of analysis
toBinomialExact(x, observedEvents = c(20,55,80))
}
\seealso{
\code{\link{gsBinomialExact}}
}
