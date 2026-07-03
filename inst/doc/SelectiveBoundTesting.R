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
# 3-analysis design with non-binding futility (test.type = 4)
# Futility testing only at IA1
x1 <- gsDesign(
  k = 3,
  test.type = 4,
  alpha = 0.025,
  beta = 0.1,
  sfu = sfHSD,
  sfupar = -4,
  sfl = sfHSD,
  sflpar = -2,
  testLower = c(TRUE, FALSE, FALSE)
)

## -----------------------------------------------------------------
gsBoundSummary(x1)

## -----------------------------------------------------------------
x1

## ----fig.cap="Power plot with futility only at IA1"---------------
plot(x1, plottype = 1)

## -----------------------------------------------------------------
# 3-analysis design with binding futility (test.type = 3)
# No efficacy testing at IA1
x2 <- gsDesign(
  k = 3,
  test.type = 3,
  alpha = 0.025,
  beta = 0.1,
  sfu = sfHSD,
  sfupar = -4,
  sfl = sfHSD,
  sflpar = -2,
  testUpper = c(FALSE, TRUE, TRUE)
)

## -----------------------------------------------------------------
gsBoundSummary(x2)

## -----------------------------------------------------------------
# Survival design with futility only at IA1
xs <- gsSurv(
  k = 3,
  test.type = 4,
  alpha = 0.025,
  beta = 0.1,
  hr = 0.7,
  timing = c(0.5, 0.75),
  sfu = sfHSD,
  sfupar = -4,
  sfl = sfHSD,
  sflpar = -2,
  lambdaC = log(2) / 12,
  eta = 0.01,
  gamma = 10,
  R = 12,
  T = 36,
  minfup = 24,
  testLower = c(TRUE, FALSE, FALSE)
)
gsBoundSummary(xs)

## -----------------------------------------------------------------
# Harm bound design with harm monitoring only at IA1 and IA2
xh <- gsDesign(
  k = 3,
  test.type = 8,
  alpha = 0.025,
  beta = 0.1,
  astar = 0.05,
  sfu = sfHSD,
  sfupar = -4,
  sfl = sfHSD,
  sflpar = -2,
  sfharm = sfHSD,
  sfharmparam = 1,
  testHarm = c(TRUE, TRUE, FALSE)
)
gsBoundSummary(xh)

## -----------------------------------------------------------------
# Futility only at IA1, efficacy only at IA2, both at Final
x5 <- gsDesign(
  k = 3,
  test.type = 4,
  alpha = 0.025,
  beta = 0.1,
  sfu = sfHSD,
  sfupar = -4,
  sfl = sfHSD,
  sflpar = -2,
  testUpper = c(FALSE, TRUE, TRUE),
  testLower = c(TRUE, FALSE, FALSE)
)
gsBoundSummary(x5)

## ----error=TRUE---------------------------------------------------
try({
# This fails: testUpper must be TRUE at the final analysis
try(gsDesign(k = 3, test.type = 3, testUpper = c(TRUE, TRUE, FALSE)))
})

## ----error=TRUE---------------------------------------------------
try({
# This fails: no bound active at analysis 1
try(gsDesign(k = 3, test.type = 4,
  testUpper = c(FALSE, TRUE, TRUE),
  testLower = c(FALSE, TRUE, TRUE)
))
})

## -----------------------------------------------------------------
x1$testUpper
x1$testLower
x1$testHarm

## -----------------------------------------------------------------
# Baseline non-binding design
x_nb <- gsDesign(k = 3, test.type = 4, alpha = 0.025, beta = 0.1)

# Remove futility at IA2 and final
x_nb_sel <- gsDesign(k = 3, test.type = 4, alpha = 0.025, beta = 0.1,
  testLower = c(TRUE, FALSE, FALSE))

# Non-binding alpha (computed ignoring lower bounds)
nb_alpha_base <- sum(gsDesign:::gsprob(0, x_nb$n.I, rep(-20, 3), x_nb$upper$bound, r = x_nb$r)$probhi)
nb_alpha_sel  <- sum(gsDesign:::gsprob(0, x_nb_sel$n.I, rep(-20, 3), x_nb_sel$upper$bound, r = x_nb_sel$r)$probhi)
cat("Baseline non-binding alpha: ", nb_alpha_base, "\n")
cat("Selective non-binding alpha:", nb_alpha_sel , "\n")
cat("Upper bounds identical:     ", all.equal(x_nb$upper$bound, x_nb_sel$upper$bound), "\n")

## -----------------------------------------------------------------
# Remove efficacy at IA1
x_nb_eff <- gsDesign(k = 3, test.type = 4, alpha = 0.025, beta = 0.1,
  testUpper = c(FALSE, TRUE, TRUE))
nb_alpha_eff <- sum(gsDesign:::gsprob(0, x_nb_eff$n.I, rep(-20, 3), x_nb_eff$upper$bound, r = x_nb_eff$r)$probhi)
cat("Non-binding alpha (skip IA1 efficacy):", nb_alpha_eff, "\n")

## -----------------------------------------------------------------
# Baseline binding design
x_b <- gsDesign(k = 3, test.type = 3, alpha = 0.025, beta = 0.1)
cat("Baseline alpha:", sum(x_b$upper$prob[, 1]), "\n")

# Remove futility at IA2 and final
x_b_sel <- gsDesign(k = 3, test.type = 3, alpha = 0.025, beta = 0.1,
  testLower = c(TRUE, FALSE, FALSE))
cat("Selective alpha:", sum(x_b_sel$upper$prob[, 1]), "\n")

# Remove efficacy at IA1
x_b_eff <- gsDesign(k = 3, test.type = 3, alpha = 0.025, beta = 0.1,
  testUpper = c(FALSE, TRUE, TRUE))
cat("Skip IA1 efficacy alpha:", sum(x_b_eff$upper$prob[, 1]), "\n")

