## ----include=FALSE------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo=FALSE, results='asis'-----------------------------------
if (knitr::is_html_output()) {
  knitr::asis_output(
    "<style>table > caption { caption-side: top; caption-location: top; }</style>"
  )
}

## -----------------------------------------------------------------
library(gsDesign)

## ----sas-reported-values------------------------------------------
sas_fractional <- data.frame(
  Analysis = 1:4,
  Events = c(22.26962, 44.53924, 66.80886, 89.07847),
  Calendar_time = c(11.2631, 16.2875, 20.4926, 25.13323),
  N = c(168.95, 244.31, 270.00, 270.00),
  Upper_Z = c(4.33263, 2.96333, 2.35902, 2.01409)
)

## ----translation-table, echo=FALSE--------------------------------
knitr::kable(
  data.frame(
    Quantity = c(
      "Two-sided Type I error",
      "Symmetric two-sided design",
      "Analysis timing input",
      "Event formula",
      "Accrual duration",
      "Total study duration",
      "Follow-up after accrual"
    ),
    SAS = c(
      "alpha = 0.05 total",
      "Early stop to reject either side",
      "Information fractions 0.25, 0.50, 0.75, 1.00",
      "Schoenfeld log-rank information",
      "ACCTIME = 18",
      "Total Time = 25.13323",
      "Derived as 7.133226"
    ),
    gsDesign = c(
      "alpha = 0.025 per tail",
      "test.type = 2",
      "gsSurv(timing = c(.25, .50, .75, 1))",
      "method = \"Schoenfeld\"",
      "R = 18",
      "T = NULL",
      "minfup = NULL"
    ),
    Reason = c(
      "gsDesign stores and spends one-sided alpha",
      "Mirrors the upper and lower efficacy boundaries",
      "Uses the SAS fractional information schedule",
      "Avoids Lachin-Foulkes default event calculation",
      "Keeps the same fixed accrual duration",
      "Lets gsSurv() solve total time from fixed accrual",
      "Lets gsSurv() solve the follow-up duration"
    ),
    check.names = FALSE
  ),
  caption = "Translation from the SAS PROC SEQDESIGN example to gsDesign inputs."
)

## ----commonparm, message=FALSE------------------------------------
k <- 4
alpha_sas <- 0.05 # Two-sided total alpha (SAS convention)
alpha_gsdesign <- alpha_sas / 2 # gsDesign uses one-sided alpha
beta <- 0.10 # 1 - power = 0.10 -> 90% power
lambdaC <- 0.03466 # Control hazard rate
lambdaE <- 0.01733 # Experimental hazard rate
HR <- lambdaE / lambdaC # = 0.5
timing <- c(0.25, 0.50, 0.75, 1.00) # Equally spaced information fractions
accrate <- 15 # Uniform accrual rate (subjects per time unit)
accrual_duration <- 18 # Accrual duration (time units)
sas_total_time <- 25.13323
sas_followup_time <- sas_total_time - accrual_duration
N <- accrate * accrual_duration

## ----approach2----------------------------------------------------
des_2 <- gsSurv(
  k = 4,
  test.type = 2, # Symmetric two-sided design
  alpha = alpha_gsdesign, # One-sided alpha; SAS total alpha is 2 * this
  beta = 0.10,
  sfu = sfLDOF,
  timing = c(.25, .50, .75, 1),
  lambdaC = 0.03466,
  hr = 0.5,
  eta = 0, # Assume no dropout
  gamma = accrate,
  R = accrual_duration,
  T = NULL,
  minfup = NULL,
  ratio = 1,
  method = "Schoenfeld"
)
des_2

## ----fractional-time-match----------------------------------------
gs_fractional <- data.frame(
  Analysis = 1:k,
  Events_SAS = sas_fractional$Events,
  Events_gsDesign = des_2$n.I,
  Time_SAS = sas_fractional$Calendar_time,
  Time_gsDesign = des_2$T,
  N_SAS = sas_fractional$N,
  N_gsDesign = rowSums(des_2$eNC) + rowSums(des_2$eNE),
  Z_SAS = sas_fractional$Upper_Z,
  Z_gsDesign = des_2$upper$bound
)

knitr::kable(
  round(gs_fractional, 5),
  caption = "Fractional-time SAS output compared with gsSurv()."
)

## ----fixed-followup-check-----------------------------------------
des_fixed_followup <- gsSurv(
  k = 4,
  test.type = 2,
  alpha = alpha_gsdesign,
  beta = 0.10,
  sfu = sfLDOF,
  timing = c(.25, .50, .75, 1),
  lambdaC = 0.03466,
  hr = 0.5,
  eta = 0,
  gamma = accrate,
  R = accrual_duration,
  T = NULL,
  minfup = sas_followup_time,
  ratio = 1,
  method = "Schoenfeld"
)

fixed_followup_check <- data.frame(
  Quantity = c(
    "Final study duration",
    "Accrual duration",
    "Final events",
    "Final N"
  ),
  Solved_followup = c(
    des_2$T[k],
    sum(des_2$R),
    des_2$n.I[k],
    sum(des_2$eNC[k, ] + des_2$eNE[k, ])
  ),
  Specified_followup = c(
    des_fixed_followup$T[k],
    sum(des_fixed_followup$R),
    des_fixed_followup$n.I[k],
    sum(des_fixed_followup$eNC[k, ] + des_fixed_followup$eNE[k, ])
  )
)
fixed_followup_check[-1] <- lapply(fixed_followup_check[-1], round, digits = 5)

knitr::kable(
  fixed_followup_check,
  caption = "Both fixed-accrual translations produce the same final design."
)

