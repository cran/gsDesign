## ---- include=FALSE-------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  dev = "ragg_png",
  dpi = 96,
  fig.retina = 1,
  fig.width = 7.2916667,
  fig.asp = 0.618,
  fig.align = "center",
  out.width = "80%"
)

options(width = 58)

## ---- message=FALSE, warning=FALSE, class.source="fold-hide"----
library(gsDesign)
library(gt)
library(dplyr)

## -------------------------------------------------------
pi1 <- .7
ratio <- 3
p1 <- ratio / (ratio + 1 / (1 - pi1))
p1

## -------------------------------------------------------
1 - 1 / (ratio * (1 / p1 - 1))

## -------------------------------------------------------
pi0 <- .3
p0 <- ratio / (ratio + 1 / (1 - pi0))
p0

## -------------------------------------------------------
alpha <- 0.023 # Type I error; this was adjusted from .025 to ensure Type I error control
beta <- 0.09 # Type II error (1 - power); this was reduced from .1 to .09 to ensure power
k <- 3 # number of analyses in group sequential design
timing <- c(.5, .8) # Relative timing of interim analyses compared to final
sfu <- sfLDOF # Efficacy bound spending function (O'Brien-Fleming-like here)
sfupar <- 0 # Parameter for efficacy spending function
sfl <- sfHSD # Futility bound spending function (Hwang-Shih-DeCani here)
sflpar <- -12 # Futility bound spending function parameter (conservative)
timename <- "Month" # Time unit
failRate <- .002 # Exponential failure rate
dropoutRate <- .0001 # Exponential dropout rate
enrollDuration <- 4 # Enrollment duration
trialDuration <- 8 # Planned trial duration
VE1 <- .7 # Alternate hypothesis vaccine efficacy
VE0 <- .3 # Null hypothesis vaccine efficacy
ratio <- 3 # Experimental/Control enrollment ratio

## -------------------------------------------------------
# Derive Group Sequential Design
# This determines final sample size
x <- gsSurv(
  k = k, test.type = 4, alpha = alpha, beta = beta, timing = timing,
  sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar,
  lambdaC = failRate, eta = dropoutRate,
  # Translate vaccine efficacy to HR
  hr = 1 - VE1, hr0 = 1 - VE0,
  R = enrollDuration, T = trialDuration,
  minfup = trialDuration - enrollDuration, ratio = ratio
)

gsBoundSummary(x, tdigits = 1) %>%
  gt() %>%
  tab_header(title = "Initial group sequential approximation")

## ---- class.source = 'fold-hide'------------------------
# Round up event counts and update spending based on slightly updated timing and then re-derive bounds.
# This will then be used to set event counts and bounds for the exact binomial bounds

counts <- round(x$n.I) # Round for interim counts
counts[k] <- ceiling(x$n.I[k]) # Round up for final count
timing <- counts / max(counts)
xx <- gsDesign(
  k = k, test.type = 4, n.I = counts, maxn.IPlan = x$n.I[k],
  alpha = alpha, beta = beta,
  delta = x$delta, delta1 = x$delta1, delta0 = x$delta0,
  sfu = sfu, sfupar = sfupar, sfl = sfl, sflpar = sflpar,
  usTime = timing,
  lsTime = timing
)
zupper <- xx$upper$bound # Updated upper bounds
zlower <- xx$lower$bound # Updated lower bounds
# For non-inferiority and super-superiority trials
xx$hr0 <- x$hr0

## -------------------------------------------------------
xxsum <- gsBoundSummary(xx, deltaname = "HR", logdelta = TRUE, Nname = "Events")
xxsum %>%
  gt() %>%
  tab_header(title = "Updated design with spending based on integer event counts")

## -------------------------------------------------------
# Translate vaccine efficacy to exact binomial probabilities

p0 <- (1 - VE0) * ratio / (1 + (1 - VE0) * ratio)
p1 <- (1 - VE1) * ratio / (1 + (1 - VE1) * ratio)

# Lower bound probabilities are for efficacy and Type I error should be controlled under p0
a <- qbinom(p = pnorm(-zupper), size = counts, prob = p0)
a[k] <- a[k] - 1
# Upper bound probabilities are for futility and spending should be under p1
b <- qbinom(p = pnorm(zlower), size = counts, prob = p0, lower.tail = FALSE)
# a < b required for each analysis; subtracting 1 makes one bound or the other
# cross at the end
xxx <- gsBinomialExact(k = k, theta = c(p0, p1), n.I = counts, a = a, b = b)
xxx

## -------------------------------------------------------
# Nominal p-values at efficacy bounds

pnorm(-xx$upper$bound) # Asymptotic
pbinom(xxx$lower$bound, prob = p0, size = xxx$n.I) # Exact binomial

# Nominal p-values for futility bounds
pnorm(-xx$lower$bound) # Asymptotic
pbinom(xxx$upper$bound, prob = p0, size = xxx$n.I) # Exact binomial

## -------------------------------------------------------
# Cumulative non-binding alpha-spending at lower bounds
cumsum(xx$upper$spend) # Asymptotic design
# Exact binomial design
# Compute by removing futility bounds
balt <- xxx$n.I + 1
xxxalt <- gsBinomialExact(k = k, theta = c(p0, p1), n.I = counts, a = a, b = balt)
cumsum(xxxalt$lower$prob[, 1])

## -------------------------------------------------------
# Cumulative non-binding beta-spending at lower bounds
cumsum(xx$lower$spend) # Asymptotic design
cumsum(xxx$upper$prob[, 2]) # Exact binomial design

## -------------------------------------------------------
p_lower <- xxx$lower$bound / xxx$n.I
p_upper <- xxx$upper$bound / xxx$n.I
p_lower
p_upper

## -------------------------------------------------------
1 - 1 / (ratio * (1 / p_lower - 1)) # Efficacy bound
1 - 1 / (ratio * (1 / p_upper - 1)) # Futility bound

## -------------------------------------------------------
# Translate ~HR at efficacy bound to VE for asymptotic design
1 - xxsum[c(3, 8, 13), 3] # Efficacy bound
1 - xxsum[c(3, 8, 13), 4] # Futility bound

