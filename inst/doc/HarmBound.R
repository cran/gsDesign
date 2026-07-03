## ----include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "svg",
  fig.ext = "svg",
  fig.width = 7.2916667,
  fig.asp = 0.618,
  fig.align = "center",
  out.width = "80%"
)

options(width = 68)

## ----echo=FALSE, message=FALSE, warning=FALSE---------------------
library(gsDesign)
library(knitr)

## -----------------------------------------------------------------
x8 <- gsSurvCalendar(
  test.type = 8,
  alpha = 0.0125,
  beta = 0.1,
  astar = 0.1,
  calendarTime = c(12, 24, 36, 48, 60),
  sfu = sfLDOF,
  sfl = sfHSD, sflpar = -2,
  sfharm = sfLDPocock,
  lambdaC = log(2) / 36,
  hr = 0.75,
  R = 18,
  minfup = 42
)

## -----------------------------------------------------------------
cat(strwrap(summary(x8), width = 65), sep = "\n")

## -----------------------------------------------------------------
gsBoundSummary(x8)

## -----------------------------------------------------------------
gsBoundSummary(x8, exclude = c())

## -----------------------------------------------------------------
bounds <- data.frame(
  Analysis = 1:x8$k,
  Month = x8$T,
  Events = ceiling(x8$n.I),
  Harm = round(x8$harm$bound, 2),
  Futility = round(x8$lower$bound, 2),
  Efficacy = round(x8$upper$bound, 2)
)
kable(bounds, caption = "Z-value boundaries at each analysis")

## -----------------------------------------------------------------
probs <- data.frame(
  Scenario = c(rep("Under H0 (HR=1)", x8$k), rep("Under H1 (HR=0.75)", x8$k)),
  Analysis = rep(1:x8$k, 2),
  Month = rep(x8$T, 2),
  `P(Efficacy)` = c(cumsum(x8$upper$prob[, 1]), cumsum(x8$upper$prob[, 2])),
  `P(Futility)` = c(cumsum(x8$lower$prob[, 1]), cumsum(x8$lower$prob[, 2])),
  `P(Harm)` = c(cumsum(x8$harm$prob[, 1]), cumsum(x8$harm$prob[, 2])),
  check.names = FALSE
)
kable(probs, digits = 4, caption = "Cumulative boundary crossing probabilities")

## ----fig.cap = "Z-value boundaries for non-binding harm bound design"----
plot(x8)

## ----fig.cap = "Boundary crossing probabilities for non-binding harm bound design"----
plot(x8, plottype = 2)

## ----fig.cap = "Approximate treatment effect at boundaries"-------
plot(x8, plottype = 3)

## ----fig.cap = "Conditional power at boundaries"------------------
plot(x8, plottype = 4)

## ----fig.cap = "Spending functions for non-binding harm bound design"----
plot(x8, plottype = 5)

## ----fig.cap = "B-values at boundaries"---------------------------
plot(x8, plottype = 7)

## -----------------------------------------------------------------
x7 <- gsSurvCalendar(
  test.type = 7,
  alpha = 0.0125,
  beta = 0.1,
  astar = 0.1,
  calendarTime = c(12, 24, 36, 48, 60),
  sfu = sfLDOF,
  sfl = sfHSD, sflpar = -2,
  sfharm = sfLDPocock,
  lambdaC = log(2) / 36,
  hr = 0.75,
  R = 18,
  minfup = 42
)

## -----------------------------------------------------------------
comparison <- data.frame(
  Bound = c("Efficacy", "Futility", "Harm"),
  `Binding (type 7)` = c(
    paste(round(x7$upper$bound, 3), collapse = ", "),
    paste(round(x7$lower$bound, 3), collapse = ", "),
    paste(round(x7$harm$bound, 3), collapse = ", ")
  ),
  `Non-binding (type 8)` = c(
    paste(round(x8$upper$bound, 3), collapse = ", "),
    paste(round(x8$lower$bound, 3), collapse = ", "),
    paste(round(x8$harm$bound, 3), collapse = ", ")
  ),
  check.names = FALSE
)
kable(comparison, caption = "Comparison of binding vs. non-binding Z-value boundaries")

## -----------------------------------------------------------------
gsBoundSummary(x7)

## -----------------------------------------------------------------
gsBoundSummary(x8, alpha = 0.025)

