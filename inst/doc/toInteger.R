## -------------------------------------------------------
library(gsDesign)

x <- gsSurv(
  k = 3, # Number of analyses
  test.type = 4, # Asymmetric 2-sided design with non-binding futility bound
  alpha = 0.025, # 1-sided Type I error
  beta = 0.1, # Type II error (1 - power; 90% power)
  timing = c(.25, .7), # Fraction of final planned events at interim analyses
  sfu = sfLDOF, # O'Brien-Fleming-like spending for efficacy
  sfl = sfHSD, # Hwang-Shih-DeCani spending for futility
  sflpar = -2.2, # Futility spending parameter to customize bound
  lambdaC = log(2) / 12, # 12 month median control survival
  hr = 0.75, # Alternate hypothesis hazard ratio
  eta = -log(.98) / 12, # 2% dropout rate per year
  # Enrollment accelerates over 6 months to steady state
  gamma = c(2.5, 5, 7.5, 10), # Relative enrollment rates
  # Duration of relative enrollment rate
  R = c(2, 2, 2, 100),
  # Enrollment duration targeted to T - minfup = 12 months total
  T = 36, # Trial duration
  minfup = 24, # Minimum follow-up duration
  ratio = 1 # Randomization ratio is 1:1
)

## ----results='asis'-------------------------------------
cat(summary(x))

## -------------------------------------------------------
# Adjust design to integer-based event counts at analyses
# and even integer-based final event count
xi <- toInteger(x)
gsBoundSummary(xi) # Summarize design bounds

## -------------------------------------------------------
# Integer event counts at analyses are integer
xi$n.I
# Control planned sample size at analyses
# Final analysis is integer; interim analyses before enrollment completion
# are continuous
xi$eNC
# Experimental analysis planned sample size at analyses
xi$eNE

## -------------------------------------------------------
n.fix <- nBinomial(p1 = .2, p2 = .1, alpha = .025, beta = .2, ratio = 2)
n.fix

## -------------------------------------------------------
nBinomial(p1 = .2, p2 = .1, alpha = .025, n = 432, ratio = 2)

## -------------------------------------------------------
# 1-sided design (efficacy bound only; test.type = 1)
x <- gsDesign(alpha = .025, beta = .2, n.fix = n.fix, test.type = 1, sfu = sfLDOF, timing = c(.5, .75))
# Continuous sample size (non-integer) at planned analyses
x$n.I

## -------------------------------------------------------
# Convert to integer sample size with even multiple of ratio + 1
# i.e., multiple of 3 in this case at final analysis
x_integer <- toInteger(x, ratio = 2)
x_integer$n.I

## -------------------------------------------------------
# Bound for continuous sample size design
x$upper$bound
# Bound for integer sample size design
x_integer$upper$bound

## -------------------------------------------------------
# Continuous design sample size fractions at analyses
x$timing
# Integer design sample size fractions at analyses
x_integer$timing

## -------------------------------------------------------
# Continuous sample size design
cumsum(x$upper$prob[, 1])
# Specified spending based on the spending function
x$upper$sf(alpha = x$alpha, t = x$timing, x$upper$param)$spend

## -------------------------------------------------------
# Integer sample size design
cumsum(x_integer$upper$prob[, 1])
# Specified spending based on the spending function
# Slightly different from continuous design due to slightly different information fraction
x$upper$sf(alpha = x_integer$alpha, t = x_integer$timing, x_integer$upper$param)$spend

## -------------------------------------------------------
# Cumulative upper boundary crossing probability under alternate by analysis
# under alternate hypothesis for continuous sample size
cumsum(x$upper$prob[, 2])
# Same for integer sample sizes at each analysis
cumsum(x_integer$upper$prob[, 2])

## -------------------------------------------------------
# 2-sided asymmetric design with non-binding futility bound (test.type = 4)
xnb <- gsDesign(
  alpha = .025, beta = .2, n.fix = n.fix, test.type = 4,
  sfu = sfLDOF, sfl = sfHSD, sflpar = -2,
  timing = c(.5, .75), delta1 = .1
)
# Continuous sample size for non-binding design
xnb$n.I

## -------------------------------------------------------
xnbi <- toInteger(xnb, ratio = 2)
# Integer design sample size at each analysis
xnbi$n.I
# Information fraction based on integer sample sizes
xnbi$timing

## -------------------------------------------------------
# Type I error, continuous design
cumsum(xnb$upper$prob[, 1])
# Type I error, integer design
cumsum(xnbi$upper$prob[, 1])

## -------------------------------------------------------
# Type I error for integer design ignoring futility bound
cumsum(xnbi$falseposnb)

## -------------------------------------------------------
# Actual cumulative beta spent at each analysis
cumsum(xnbi$lower$prob[, 2])
# Spending function target is the same at interims, but larger at final
xnbi$lower$sf(alpha = xnbi$beta, t = xnbi$n.I / max(xnbi$n.I), param = xnbi$lower$param)$spend

## -------------------------------------------------------
# beta-spending
sum(xnbi$upper$prob[, 2])

