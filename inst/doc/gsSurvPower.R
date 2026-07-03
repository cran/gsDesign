## ----include=FALSE--------------------------------------
library(gsDesign)

## ----gssurvpower_quick_start----------------------------
design <- gsSurv(
  k = 3, test.type = 4, alpha = 0.025, sided = 1, beta = 0.1,
  sfu = sfHSD, sfupar = -4, sfl = sfHSD, sflpar = -2,
  lambdaC = log(2) / 12, hr = 0.7, hr0 = 1,
  eta = 0.01, gamma = 10, R = 16, minfup = 12, T = 28
)

pwr_design <- gsSurvPower(x = design, plannedCalendarTime = design$T)
pwr_design$power

## ----gssurvpower_at_design------------------------------
cat("Design power:", round((1 - design$beta) * 100, 1), "%\n")
cat("gsSurvPower:  ", round(pwr_design$power * 100, 1), "%\n")

## ----gssurvpower_worse_hr-------------------------------
pwr_worse <- gsSurvPower(x = design, hr = 0.8, plannedCalendarTime = design$T)
cat("Power at HR = 0.8:", round(pwr_worse$power * 100, 1), "%\n")

## ----gssurvpower_sensitivity----------------------------
hr_grid <- seq(0.55, 0.90, by = 0.05)
power_vals <- sapply(hr_grid, function(h) {
  p <- gsSurvPower(x = design, hr = h, plannedCalendarTime = design$T)
  p$power
})
results <- data.frame(HR = hr_grid, Power = round(power_vals * 100, 1))
results

## ----timing_setup---------------------------------------
total_N <- floor(sum(design$gamma * design$R))

## ----timing_baseline------------------------------------
pwr_multi <- gsSurvPower(
  x = design,
  targetEvents = design$n.I,
  plannedCalendarTime = design$T,
  minN = c(NA, total_N, total_N),
  minFollowUp = c(NA, 2, 12),
  maxExtension = c(3, 12, 20)
)

data.frame(
  Analysis = 1:design$k,
  Planned_Time = round(design$T, 1),
  Actual_Time = round(pwr_multi$T, 1),
  Target_Events = round(design$n.I, 1),
  Actual_Events = round(pwr_multi$n.I, 1)
)
cat("Power:", round(pwr_multi$power * 100, 1), "%\n")

## ----timing_slow_enrollment_simple----------------------
pwr_slow_simple <- gsSurvPower(
  x = design,
  gamma = design$gamma / 2,
  targetN = total_N,
  targetEvents = design$n.I
)


pwr_slow_simple |> gsBoundSummary()

## ----timing_slow_enrollment-----------------------------
pwr_slow <- gsSurvPower(
  x = design,
  gamma = design$gamma / 2,
  targetEvents = design$n.I,
  plannedCalendarTime = design$T,
  minN = c(NA, total_N, total_N),
  minFollowUp = c(NA, 2, 12),
  maxExtension = c(3, 12, 20)
)

data.frame(
  Analysis = 1:design$k,
  Planned_Time = round(design$T, 1),
  Actual_Time = round(pwr_slow$T, 1),
  Target_Events = round(design$n.I, 1),
  Actual_Events = round(pwr_slow$n.I, 1)
)
cat("Power:", round(pwr_slow$power * 100, 1), "%\n")

## ----timing_fast_failure--------------------------------
pwr_fast <- gsSurvPower(
  x = design,
  lambdaC = log(2) / 8,
  targetEvents = design$n.I,
  plannedCalendarTime = design$T,
  minN = c(NA, total_N, total_N),
  minFollowUp = c(NA, 2, 12),
  maxExtension = c(3, 12, 20)
)

data.frame(
  Analysis = 1:design$k,
  Planned_Time = round(design$T, 1),
  Actual_Time = round(pwr_fast$T, 1),
  Target_Events = round(design$n.I, 1),
  Actual_Events = round(pwr_fast$n.I, 1)
)
cat("Power:", round(pwr_fast$power * 100, 1), "%\n")

## ----informationRates_demo------------------------------
# Scenario 1 with informationRates and fullSpendingAtFinal
planned_info_rates <- c(design$timing[-design$k], 0.95)

pwr_slow_ir <- gsSurvPower(
  x = design,
  gamma = design$gamma / 2,
  targetEvents = design$n.I,
  plannedCalendarTime = design$T,
  minN = c(NA, total_N, total_N),
  minFollowUp = c(NA, 2, 12),
  maxExtension = c(3, 12, 20),
  informationRates = planned_info_rates,
  fullSpendingAtFinal = TRUE
)

spending_frac_used <- pmin(planned_info_rates, pwr_slow_ir$timing)
spending_frac_used[design$k] <- 1

data.frame(
  Analysis = 1:design$k,
  Actual_Events = round(pwr_slow_ir$n.I, 1),
  Actual_InfoFrac = round(pwr_slow_ir$timing, 3),
  Planned_InfoFrac = round(planned_info_rates, 3),
  Spending_Frac = round(spending_frac_used, 3)
)
cat("Power (default spending):       ", round(pwr_slow$power * 100, 1), "%\n")
cat("Power (capped + full final):    ", round(pwr_slow_ir$power * 100, 1), "%\n")

## ----gssurvpower_vs_gsdesign----------------------------
design_events <- design$n.I
hr_grid <- seq(0.55, 0.95, by = 0.05)

comparison <- data.frame(
  HR = hr_grid,
  gsDesign_plot = sapply(hr_grid, function(h) {
    delta_ratio <- abs(log(h)) / abs(log(design$hr))
    theta_h <- design$delta * delta_ratio
    gsp <- gsDesign::gsProbability(
      k = design$k, theta = theta_h,
      n.I = design$n.I,
      a = design$lower$bound, b = design$upper$bound, r = 18)
    sum(gsp$upper$prob)
  }),
  fixed_events = sapply(hr_grid, function(h) {
    gsSurvPower(x = design, hr = h, targetEvents = design_events)$power
  }),
  fixed_calendar = sapply(hr_grid, function(h) {
    gsSurvPower(x = design, hr = h, plannedCalendarTime = design$T)$power
  })
)
comparison[, -1] <- round(comparison[, -1] * 100, 2)
comparison

## ----gssurvpower_bounds---------------------------------
design_events <- design$n.I
cat("Design bounds (Z-scale):\n")
cat("  Efficacy:", round(design$upper$bound, 4), "\n")
cat("  Futility:", round(design$lower$bound, 4), "\n\n")

for (h in c(0.5, 0.7, 0.8, 1.0)) {
  pwr <- gsSurvPower(x = design, hr = h, targetEvents = design_events)
  cat(sprintf("HR=%.1f  Efficacy: %s  Futility: %s  (identical: %s)\n",
    h,
    paste(round(pwr$upper$bound, 4), collapse = ", "),
    paste(round(pwr$lower$bound, 4), collapse = ", "),
    identical(pwr$upper$bound, design$upper$bound) &&
      identical(pwr$lower$bound, design$lower$bound)))
}

## ----alpha_change---------------------------------------
# Design at one-sided alpha = 0.0125
design_a0125 <- gsSurv(
  k = 3, test.type = 4, alpha = 0.0125, sided = 1, beta = 0.1,
  sfu = sfHSD, sfupar = -4, sfl = sfHSD, sflpar = -2,
  lambdaC = log(2) / 12, hr = 0.7, hr0 = 1,
  eta = 0.01, gamma = 10, R = 16, minfup = 12, T = 28
)

cat("=== Original design (alpha = 0.0125) ===\n")
cat("Efficacy bounds:", round(design_a0125$upper$bound, 4), "\n")
cat("Futility bounds:", round(design_a0125$lower$bound, 4), "\n\n")

# Power at alpha = 0.025 with same event counts (timing preserved)
events_a0125 <- design_a0125$n.I
pwr_a025 <- gsSurvPower(x = design_a0125, alpha = 0.025, targetEvents = events_a0125)

cat("=== gsSurvPower at alpha = 0.025 ===\n")
cat("Efficacy bounds:", round(pwr_a025$upper$bound, 4), "\n")
cat("Futility bounds:", round(pwr_a025$lower$bound, 4), "\n")
cat("Power:          ", round(pwr_a025$power * 100, 1), "%\n\n")

# Cross-check: gsBoundSummary at the same alternate alpha
# (only test.type 1, 4, 6, 7, 8 are supported)
cat("=== gsBoundSummary (alpha = 0.025) ===\n")
print(gsBoundSummary(design_a0125, alpha = 0.025))

## ----alpha_binding--------------------------------------
design3 <- gsSurv(
  k = 3, test.type = 3, alpha = 0.0125, sided = 1, beta = 0.1,
  sfu = sfHSD, sfupar = -4, sfl = sfHSD, sflpar = -2,
  lambdaC = log(2) / 12, hr = 0.7, eta = 0.01,
  gamma = 10, R = 16, minfup = 12, T = 28
)
events3 <- design3$n.I

pwr3_a025 <- gsSurvPower(x = design3, alpha = 0.025, targetEvents = events3)

cat("=== Binding futility (test.type=3) at alpha = 0.025 ===\n")
cat("Original efficacy:", round(design3$upper$bound, 4), "\n")
cat("New efficacy:     ", round(pwr3_a025$upper$bound, 4), "\n")
cat("Original futility:", round(design3$lower$bound, 4), "\n")
cat("New futility:     ", round(pwr3_a025$lower$bound, 4), "\n")
cat("Power:            ", round(pwr3_a025$power * 100, 1), "%\n")

## ----alpha_output_fields--------------------------------
cat("test.type:", pwr3_a025$test.type, "(same as input design)\n")
cat("alpha:    ", pwr3_a025$alpha, "(updated to new value)\n")

## ----gssurvpower_events---------------------------------
pwr_events <- gsSurvPower(
  x = design,
  targetEvents = c(75, 150, 225)
)
cat("Analysis times:", round(pwr_events$T, 1), "\n")
cat("Events at each analysis:",
    round(pwr_events$n.I, 1), "\n")
cat("Power:", round(pwr_events$power * 100, 1), "%\n")

## ----gssurvpower_slower_enrollment----------------------
pwr_slow_enroll <- gsSurvPower(
  x = design,
  gamma = design$gamma / 2,
  plannedCalendarTime = design$T
)

cat("Original final expected events:", round(pwr_design$n.I[design$k], 1), "\n")
cat("Slower-enrollment final events:", round(pwr_slow_enroll$n.I[design$k], 1), "\n")
cat("Original power:", round(pwr_design$power * 100, 1), "%\n")
cat("Slower-enrollment power:", round(pwr_slow_enroll$power * 100, 1), "%\n")

## ----calendar_spending----------------------------------
# Information-based spending (default)
pwr_info <- gsSurvPower(
  x = design,
  plannedCalendarTime = design$T,
  spending = "information"
)

# Calendar-based spending
pwr_cal <- gsSurvPower(
  x = design,
  plannedCalendarTime = design$T,
  spending = "calendar"
)

# Compare spending fractions and bounds
data.frame(
  Analysis = 1:design$k,
  Calendar_Time = round(pwr_info$T, 1),
  InfoFraction = round(pwr_info$timing, 3),
  CalendarFraction = round(pwr_cal$T / max(pwr_cal$T), 3),
  Bound_Info = round(pwr_info$upper$bound, 4),
  Bound_Calendar = round(pwr_cal$upper$bound, 4)
)

## ----calendar_ignores_usTime----------------------------
pwr_cal_override <- gsSurvPower(
  x = design,
  plannedCalendarTime = design$T,
  spending = "calendar",
  usTime = c(0.2, 0.6, 1),
  lsTime = c(0.3, 0.8, 1)
)

# Bounds are identical regardless of usTime/lsTime
identical(pwr_cal$upper$bound, pwr_cal_override$upper$bound)

## ----stratified_events----------------------------------
# Per-stratum event targets: rows = analyses, columns = strata
event_matrix <- matrix(
  c(20, 10,   # interim: 20 from stratum 1, 10 from stratum 2
    40, 20),  # final:   40 from stratum 1, 20 from stratum 2
  nrow = 2, byrow = TRUE
)

pwr_strat <- gsSurvPower(
  k = 2, test.type = 1, alpha = 0.025, sided = 1,
  lambdaC = matrix(log(2) / c(6, 12), ncol = 2),
  hr = 0.7, eta = 0.01,
  gamma = matrix(c(5, 5), ncol = 2), R = 12, ratio = 1,
  targetEvents = event_matrix
)

# The analysis times are solved so that total expected events
# match the row sums of the target matrix
data.frame(
  Analysis = 1:2,
  Target_Stratum1 = event_matrix[, 1],
  Target_Stratum2 = event_matrix[, 2],
  Target_Total = rowSums(event_matrix),
  Expected_Events = round(pwr_strat$n.I, 1),
  Calendar_Time = round(pwr_strat$T, 1)
)

cat("Power:", round(pwr_strat$power * 100, 1), "%\n")

## ----biomarker_design-----------------------------------
prevalence <- 0.6
median_bm_pos <- 12    # control median in biomarker+ (months)
median_bm_neg <- 10    # control median in biomarker- (shorter prognosis)

bm_design <- gsSurvCalendar(
  test.type = 4, alpha = 0.0125, beta = 0.1,
  sfu = sfHSD, sfupar = -4, sfl = sfHSD, sflpar = -2,
  calendarTime = c(12, 24, 36),
  lambdaC = log(2) / median_bm_pos, hr = 0.65, eta = 0.01,
  gamma = 10, R = 18, minfup = 18, ratio = 1
)

summary(bm_design)
gsBoundSummary(bm_design)

## ----stratified_power-----------------------------------
# Control hazard rates by stratum
lambdaC_pos <- log(2) / median_bm_pos
lambdaC_neg <- log(2) / median_bm_neg

# Enrollment rates by stratum (proportionate to prevalence)
gamma_pos <- bm_design$gamma
gamma_neg <- bm_design$gamma * (1 - prevalence) / prevalence

# Stratified inputs: matrix with columns = strata
lambdaC_strat <- matrix(c(lambdaC_pos, lambdaC_neg), ncol = 2)
gamma_strat <- matrix(c(gamma_pos, gamma_neg), ncol = 2)

pwr_overall <- gsSurvPower(
  k = 3, test.type = 4, alpha = 0.0125, sided = 1,
  sfu = sfHSD, sfupar = -4, sfl = sfHSD, sflpar = -2,
  lambdaC = lambdaC_strat, hr = 0.75, eta = 0.01,
  gamma = gamma_strat, R = 18, ratio = 1,
  plannedCalendarTime = c(12, 24, 36)
)

summary(pwr_overall)
gsBoundSummary(pwr_overall)

