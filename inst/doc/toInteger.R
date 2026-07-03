## ----message=FALSE, warning=FALSE-----------------------
library(gsDesign)

## -------------------------------------------------------
n.fix <- nBinomial(p1 = .2, p2 = .1, alpha = .025, beta = .2, ratio = 2)
n.fix

## -------------------------------------------------------
nBinomial(p1 = .2, p2 = .1, alpha = .025, n = 432, ratio = 2)

## -------------------------------------------------------
# 1-sided design (efficacy bound only; test.type = 1)
xb <- gsDesign(alpha = .025, beta = .2, n.fix = n.fix, test.type = 1, sfu = sfLDOF, timing = c(.5, .75))
# Continuous sample size (non-integer) at planned analyses
xb$n.I

## -------------------------------------------------------
# Convert to integer sample size with even multiple of ratio + 1
# i.e., multiple of 3 in this case at final analysis
x_integer <- toInteger(xb, ratio = 2)
x_integer$n.I

## -------------------------------------------------------
# Bound for continuous sample size design
xb$upper$bound
# Bound for integer sample size design
x_integer$upper$bound

## -------------------------------------------------------
# Continuous design sample size fractions at analyses
xb$timing
# Integer design sample size fractions at analyses
x_integer$timing

## -------------------------------------------------------
# Continuous sample size design
cumsum(xb$upper$prob[, 1])
# Specified spending based on the spending function
xb$upper$sf(alpha = xb$alpha, t = xb$timing, xb$upper$param)$spend

## -------------------------------------------------------
# Integer sample size design
cumsum(x_integer$upper$prob[, 1])
# Specified spending based on the spending function
# Slightly different from continuous design due to slightly different information fraction
x_integer$upper$sf(alpha = x_integer$alpha, t = x_integer$timing, x_integer$upper$param)$spend

## -------------------------------------------------------
# Cumulative upper boundary crossing probability under alternate by analysis
# under alternate hypothesis for continuous sample size
cumsum(xb$upper$prob[, 2])
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

## -------------------------------------------------------
x <- gsSurv(ratio = 1, hr = .74)
y <- x |> toInteger()
# Continuous event counts
x$n.I
# Event counts converted to integers
y$n.I
# Continuous sample size at interim and final analyses
as.numeric(x$eNE + x$eNC)
# Rounded up to even final sample size given that x$ratio = 1
# and rounding to multiple of x$ratio + 1
as.numeric(y$eNE + y$eNC)
# With roundUpFinal = FALSE, final sample size rounded to nearest integer
z <- x |> toInteger(roundUpFinal = FALSE)
as.numeric(z$eNE + z$eNC)

## -------------------------------------------------------
seasonal_event_rate_control <- 0.003
season_length <- 0.5
dropout_6mo <- 0.10

season_rate <- -log(1 - seasonal_event_rate_control) / season_length
dropout_rate <- -log(1 - dropout_6mo) / season_length

seasonal_design <- gsSurv(
  k = 3,
  test.type = 4,
  alpha = 0.025,
  beta = 0.1,
  timing = c(1 / 3, 2 / 3),
  sfu = sfHSD,
  sfupar = 1,
  sfl = sfHSD,
  sflpar = -2,
  lambdaC = c(season_rate, 0, season_rate, 0, season_rate, 0),
  S = c(6, 6, 6, 6, 6),
  hr = 0.2,
  hr0 = 0.7,
  eta = dropout_rate,
  gamma = c(1, 0, 1, 0, 1, 0),
  R = c(2, 10, 2, 10, 2, 10),
  T = 42,
  minfup = 6,
  ratio = 3,
  testLower = c(TRUE, FALSE, FALSE)
)

# Continuous event targets and expected enrollment before integer conversion
seasonal_design$n.I
rowSums(seasonal_design$eNC + seasonal_design$eNE)
seasonal_design$T

## -------------------------------------------------------
seasonal_integer <- toInteger(seasonal_design)

data.frame(
  analysis = seq_len(seasonal_design$k),
  continuous_events = round(seasonal_design$n.I, 3),
  integer_events = seasonal_integer$n.I,
  continuous_time = round(seasonal_design$T, 3),
  integer_time = round(seasonal_integer$T, 3),
  integer_enrollment = round(rowSums(seasonal_integer$eNC + seasonal_integer$eNE), 3)
)

# Final integer sample size remains a multiple of ratio + 1.
rowSums(seasonal_integer$eNC + seasonal_integer$eNE)[seasonal_integer$k]

## -------------------------------------------------------
rare_design <- gsSurv(
  k = 3,
  test.type = 4,
  alpha = 0.025,
  beta = 0.1,
  timing = c(1 / 3, 2 / 3),
  sfu = sfHSD,
  sfupar = 1,
  sfl = sfHSD,
  sflpar = -2,
  lambdaC = -log(1 - 0.0015) / 0.5,
  hr = 0.2,
  hr0 = 0.7,
  eta = dropout_rate,
  gamma = c(1, 0, 1, 0, 1, 0),
  R = c(2, 10, 2, 10, 2, 10),
  T = 42,
  minfup = 6,
  ratio = 1
)

rare_integer <- toInteger(rare_design)

data.frame(
  continuous_final_events = rare_design$n.I[rare_design$k],
  integer_final_events = rare_integer$n.I[rare_integer$k],
  continuous_final_enrollment = rowSums(rare_design$eNC + rare_design$eNE)[rare_design$k],
  integer_final_enrollment = rowSums(rare_integer$eNC + rare_integer$eNE)[rare_integer$k]
)

