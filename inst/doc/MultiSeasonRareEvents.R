## ----include=FALSE--------------------------------------
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

options(width = 58)

## ----message=FALSE, warning=FALSE-----------------------
library(gsDesign)
library(gt)
library(tibble)

## -------------------------------------------------------
alpha <- 0.025
beta <- 0.1
ratio <- 3

ve0 <- 0.30
ve1 <- 0.80
hr0 <- 1 - ve0
hr1 <- 1 - ve1

seasonal_event_rate_control <- 0.003
season_length_months <- 6
season_length_years <- season_length_months / 12
dropout_6mo <- 0.10

n_seasons <- 3
stopifnot(n_seasons >= 2)
timing <- seq_len(n_seasons) / n_seasons

enrollment_months <- 2
off_enrollment_months <- 10
annual_cycle_months <- enrollment_months + off_enrollment_months
enroll_pattern <- c(rep(c(1, 0), n_seasons - 1), 1)
enroll_periods <- c(rep(c(enrollment_months, off_enrollment_months), n_seasons - 1), enrollment_months)
calendar_time <- enrollment_months + season_length_months +
  annual_cycle_months * (seq_len(n_seasons) - 1)

test_lower <- rep(FALSE, n_seasons)
test_lower[1] <- TRUE

## -------------------------------------------------------
p_event_experimental <- function(ve, randomization_ratio) {
  randomization_ratio / (randomization_ratio + 1 / (1 - ve))
}

p0 <- p_event_experimental(ve0, ratio)
p1 <- p_event_experimental(ve1, ratio)
c(p0 = p0, p1 = p1)

## -------------------------------------------------------
lambdaC <- c(-log(1 - seasonal_event_rate_control) / season_length_months, 0)
S <- season_length_months
eta <- -log(1 - dropout_6mo) / season_length_months

# Integer conversion can slightly adjust total enrollment so the final integer
# event target is achievable with the seasonal piecewise event-rate model.
design_calendar <- gsSurvCalendar(
  test.type = 4,                  # Non-binding lower bound framework
  alpha = alpha,                  # One-sided Type I error
  beta = beta,                    # Type II error
  calendarTime = calendar_time,   # Analysis times in months
  spending = "information",       # Spending by information fraction
  sfu = sfHSD,                    # Efficacy spending function
  sfupar = 1,                     # Pocock-like efficacy spending
  sfl = sfHSD,                    # Futility spending function
  sflpar = -2,                    # Futility spending parameter
  lambdaC = lambdaC,              # Control hazard rate per month by period
  S = S,                          # Event-rate period duration in months
  hr = hr1,                       # Alternative hypothesis HR
  hr0 = hr0,                      # Null hypothesis HR
  eta = eta,                      # Dropout hazard rate per month
  gamma = enroll_pattern,         # Relative enrollment rates by period
  R = enroll_periods,             # Enrollment period durations in months
  minfup = season_length_months,  # Minimum follow-up for final enrollees
  ratio = ratio,                  # Experimental:control randomization ratio
  testLower = test_lower          # Futility only at selected analyses
) |>
  toInteger() |>
  suppressWarnings()

gsBoundSummary(design_calendar)

planned_final_events <- design_calendar$n.I[design_calendar$k]
planned_counts <- as.integer(round(planned_final_events * timing))
planned_counts[n_seasons] <- planned_final_events
for (j in seq_along(planned_counts)[-1]) {
  planned_counts[j] <- max(planned_counts[j], planned_counts[j - 1] + 1L)
}

design_exact <- toBinomialExact(design_calendar, observedEvents = planned_counts)

planned_enrollment_period <- as.numeric(rowSums(as.matrix(design_calendar$gamma))) *
  as.numeric(design_calendar$R)
season_id <- rep(seq_len(n_seasons), each = 2, length.out = length(planned_enrollment_period))
planned_enrollment_by_season <- as.integer(round(tapply(planned_enrollment_period, season_id, sum)))
planned_enrollment_control <- as.integer(round(planned_enrollment_by_season / (1 + ratio)))
planned_enrollment_experimental <- planned_enrollment_by_season - planned_enrollment_control
planned_cum_enrollment <- cumsum(planned_enrollment_by_season)

## -------------------------------------------------------
target_alpha_spend <- design_calendar$upper$sf(
  alpha = alpha,
  t = timing,
  param = design_calendar$upper$param
)$spend

achieved_alpha_spend <- cumsum(
  gsBinomialExact(
    k = design_exact$k,            # Number of analyses
    theta = design_exact$theta[1], # Null event probability in experimental arm
    n.I = design_exact$n.I,        # Cumulative events by analysis
    a = design_exact$lower$bound,  # Efficacy bounds (x <= a crosses)
    b = design_exact$n.I + 1       # Non-binding upper bound for alpha-spend check
  )$lower$prob[, 1]
)

achieved_power_h1 <- cumsum(
  gsBinomialExact(
    k = design_exact$k,            # Number of analyses
    theta = design_exact$theta[2], # Alternative event probability in experimental arm
    n.I = design_exact$n.I,        # Cumulative events by analysis
    a = design_exact$lower$bound,  # Efficacy bounds
    b = design_exact$n.I + 1       # Non-binding upper bound for cumulative efficacy probability
  )$lower$prob[, 1]
)

ve_from_bound <- function(x, n, ratio) {
  out <- rep(NA_real_, length(x))
  ok <- x >= 0 & x < n
  if (any(ok)) {
    p <- x[ok] / n[ok]
    hr <- p / (ratio * (1 - p))
    out[ok] <- 1 - hr
  }
  out
}

futility_active <- if (!is.null(design_calendar$testLower)) {
  tl <- design_calendar$testLower
  if (length(tl) == 1) tl <- rep(tl, design_exact$k)
  as.logical(tl)
} else {
  design_exact$upper$bound <= design_exact$n.I
}
nominal_p_futility <- rep(NA_real_, design_exact$k)
nominal_p_futility[futility_active] <- stats::pbinom(
  q = design_exact$upper$bound[futility_active],
  size = design_exact$n.I[futility_active],
  prob = p0
)

tibble(
  Season = seq_len(n_seasons),
  `Spending time` = timing,
  `Planned total events` = design_exact$n.I,
  `Approx cumulative enrollment` = planned_cum_enrollment,
  `Exact efficacy bound (x <= a)` = design_exact$lower$bound,
  `VE at bound (efficacy)` = ve_from_bound(design_exact$lower$bound, design_exact$n.I, ratio),
  `Nominal 1-sided p at bound (efficacy)` = stats::pbinom(design_exact$lower$bound, design_exact$n.I, p0),
  `Exact futility bound (x >= b)` = ifelse(futility_active, design_exact$upper$bound, NA_integer_),
  `VE at bound (futility)` = ve_from_bound(
    ifelse(futility_active, design_exact$upper$bound, -1L),
    design_exact$n.I,
    ratio
  ),
  `Nominal 1-sided p at bound (futility)` = nominal_p_futility,
  `Target cumulative alpha spend` = target_alpha_spend,
  `Achieved cumulative alpha spend` = achieved_alpha_spend,
  `Cumulative power under H1` = achieved_power_h1
) |>
  gt() |>
  fmt_number(columns = 2, decimals = 3) |>
  fmt_percent(columns = c(`VE at bound (efficacy)`, `VE at bound (futility)`), decimals = 1) |>
  fmt_number(
    columns = c(
      `Nominal 1-sided p at bound (efficacy)`,
      `Nominal 1-sided p at bound (futility)`,
      `Target cumulative alpha spend`,
      `Achieved cumulative alpha spend`,
      `Cumulative power under H1`
    ),
    decimals = 4
  ) |>
  tab_spanner(
    label = "Efficacy",
    columns = c(
      `Exact efficacy bound (x <= a)`,
      `VE at bound (efficacy)`,
      `Nominal 1-sided p at bound (efficacy)`
    )
  ) |>
  tab_spanner(
    label = "Futility",
    columns = c(
      `Exact futility bound (x >= b)`,
      `VE at bound (futility)`,
      `Nominal 1-sided p at bound (futility)`
    )
  ) |>
  tab_header(
    title = "Planned exact binomial seasonal monitoring",
    subtitle = "Super-superiority example with Pocock-like efficacy spending"
  ) |>
  tab_footnote(
    footnote = "x denotes cumulative observed events in the experimental arm; efficacy is established when x is at or below the listed efficacy bound.",
    locations = cells_column_labels(columns = `Exact efficacy bound (x <= a)`)
  ) |>
  tab_footnote(
    footnote = "Blank futility entries indicate no futility stopping boundary at that analysis.",
    locations = cells_column_labels(columns = `Exact futility bound (x >= b)`)
  )

## -------------------------------------------------------
enrollment_table <- tibble(
  Season = as.character(seq_len(n_seasons)),
  `Control planned enrollment` = planned_enrollment_control,
  `Experimental planned enrollment` = planned_enrollment_experimental
) |>
  dplyr::mutate(
    `Total planned enrollment` = `Control planned enrollment` + `Experimental planned enrollment`,
    `Cumulative planned enrollment` = cumsum(`Total planned enrollment`)
  )

dplyr::bind_rows(
  enrollment_table,
  tibble(
    Season = "Overall",
    `Control planned enrollment` = sum(enrollment_table$`Control planned enrollment`),
    `Experimental planned enrollment` = sum(enrollment_table$`Experimental planned enrollment`),
    `Total planned enrollment` = sum(enrollment_table$`Total planned enrollment`),
    `Cumulative planned enrollment` = sum(enrollment_table$`Total planned enrollment`)
  )
) |>
  gt() |>
  tab_header(title = "Planned enrollment by season and overall")

## -------------------------------------------------------
x_offset_from_efficacy <- rep(0L, n_seasons)
x_offset_from_efficacy[min(2L, n_seasons)] <- 1L
example_x <- pmax(0L, design_exact$lower$bound + x_offset_from_efficacy)
example_p <- repeatedPValueBinomialExact(
  gsD = design_calendar,
  n.I = design_exact$n.I,
  x = example_x
)
example_p

## -------------------------------------------------------
sequentialPValueBinomialExact(
  gsD = design_calendar,
  n.I = design_exact$n.I,
  x = example_x
)

## -------------------------------------------------------
observed_counts_update <- c(
  planned_counts[-n_seasons],
  max(planned_counts[n_seasons - 1] + 1L, planned_counts[n_seasons] - 5L)
)
update_exact <- toBinomialExact(design_calendar, observedEvents = observed_counts_update)
update_exact_full <- toBinomialExact(
  design_calendar,
  observedEvents = observed_counts_update,
  maxSpend = TRUE
)

tibble(
  Analysis = seq_along(update_exact$n.I),
  `Observed total events` = update_exact$n.I,
  `Updated efficacy bound (x <= a), default spending` = update_exact$lower$bound,
  `Updated efficacy bound (x <= a), maxSpend=TRUE` = update_exact_full$lower$bound,
  `Updated futility bound, default spending` = update_exact$upper$bound,
  `Updated futility bound, maxSpend=TRUE` = update_exact_full$upper$bound
) |>
  gt() |>
  tab_header(title = "Updated exact bounds using observedEvents")

## -------------------------------------------------------
ve_scenarios <- c(`H0 (VE=30%)` = ve0, `H1 (VE=80%)` = ve1)
planned_control_event_rates <- rep(seasonal_event_rate_control, length(ve_scenarios))

sim_light <- simBinomialSeasonalExact(
  gsD = design_calendar,
  ve = ve_scenarios,
  nsim = rep(150, length(ve_scenarios)),
  control_event_rate = planned_control_event_rates,
  season_length = season_length_years,
  dropout_rate = dropout_6mo,
  planned_counts = planned_counts,
  enroll_control_per_look = planned_enrollment_control,
  enroll_experimental_per_look = planned_enrollment_experimental,
  adaptive = c(FALSE, TRUE),
  max_multiplier = 2,
  final_full_spending = TRUE,
  seed = 101
)

## -------------------------------------------------------
oc <- sim_light$summary |>
  dplyr::mutate(
    Scenario = ifelse(adaptive, paste0("Adaptive: ", scenario), paste0("Fixed: ", scenario))
  ) |>
  dplyr::select(
    Scenario,
    `Efficacy crossing probability` = rejection_rate,
    `Futility stopping probability` = futility_stop_rate,
    `MC SE (efficacy)` = mc_se,
    `MC SE (futility)` = futility_mc_se,
    `Mean total events` = mean_total_events,
    `Mean total enrolled` = mean_total_enrolled,
    `Mean looks used` = mean_looks
  )

oc |>
  gt() |>
  fmt_number(columns = 2:5, decimals = 4) |>
  fmt_number(columns = 6:8, decimals = 2) |>
  tab_header(
    title = "Lightweight simulation results",
    subtitle = "Exact-binomial monitoring with seasonal analyses"
  ) |>
  tab_footnote(
    footnote = "For VE=30% scenarios, efficacy crossing probability is Type I error under the non-binding futility convention (futility crossings do not block later efficacy crossings).",
    locations = cells_column_labels(columns = `Efficacy crossing probability`)
  )

## -------------------------------------------------------
low_control_event_rates <- planned_control_event_rates / 2

sim_low <- simBinomialSeasonalExact(
  gsD = design_calendar,
  ve = ve_scenarios,
  nsim = rep(300, length(ve_scenarios)),
  control_event_rate = low_control_event_rates,
  season_length = season_length_years,
  dropout_rate = dropout_6mo,
  planned_counts = planned_counts,
  enroll_control_per_look = planned_enrollment_control,
  enroll_experimental_per_look = planned_enrollment_experimental,
  adaptive = c(FALSE, TRUE),
  max_multiplier = 2,
  final_full_spending = TRUE,
  seed = 505
)

low <- sim_low$summary
tibble(
  Scenario = c(
    "Without adaptation: Type I error (VE=30%)",
    "With adaptation: Type I error (VE=30%)",
    "Without adaptation: Power (VE=80%)",
    "With adaptation: Power (VE=80%)"
  ),
  `Efficacy crossing probability` = c(
    low$rejection_rate[!low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$rejection_rate[low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$rejection_rate[!low$adaptive & low$scenario == "H1 (VE=80%)"],
    low$rejection_rate[low$adaptive & low$scenario == "H1 (VE=80%)"]
  ),
  `Futility stopping probability` = c(
    low$futility_stop_rate[!low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$futility_stop_rate[low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$futility_stop_rate[!low$adaptive & low$scenario == "H1 (VE=80%)"],
    low$futility_stop_rate[low$adaptive & low$scenario == "H1 (VE=80%)"]
  ),
  `Mean total events` = c(
    low$mean_total_events[!low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$mean_total_events[low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$mean_total_events[!low$adaptive & low$scenario == "H1 (VE=80%)"],
    low$mean_total_events[low$adaptive & low$scenario == "H1 (VE=80%)"]
  ),
  `Mean total enrolled` = c(
    low$mean_total_enrolled[!low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$mean_total_enrolled[low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$mean_total_enrolled[!low$adaptive & low$scenario == "H1 (VE=80%)"],
    low$mean_total_enrolled[low$adaptive & low$scenario == "H1 (VE=80%)"]
  ),
  `Mean looks used` = c(
    low$mean_looks[!low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$mean_looks[low$adaptive & low$scenario == "H0 (VE=30%)"],
    low$mean_looks[!low$adaptive & low$scenario == "H1 (VE=80%)"],
    low$mean_looks[low$adaptive & low$scenario == "H1 (VE=80%)"]
  )
) |>
  gt() |>
  fmt_number(columns = 2:3, decimals = 4) |>
  fmt_number(columns = 4:6, decimals = 2) |>
  tab_header(
    title = "Lower-than-planned event rate illustration",
    subtitle = "Adaptive approach increases enrollment to recover information"
  ) |>
  tab_footnote(
    footnote = "Type I error rows use non-binding futility for efficacy crossing probability; futility stopping probability is shown separately.",
    locations = cells_column_labels(columns = `Efficacy crossing probability`)
  )

## ----eval=FALSE-----------------------------------------
# # Suggested offline settings
# type1_nsim <- 20000
# power_nsim <- 3500
# 
# sim_type1_big <- simBinomialSeasonalExact(
#   gsD = design_calendar,
#   ve = c(`H0 (VE=30%)` = ve0),
#   nsim = type1_nsim,
#   control_event_rate = seasonal_event_rate_control,
#   season_length = season_length_years,
#   dropout_rate = dropout_6mo,
#   planned_counts = planned_counts,
#   enroll_control_per_look = planned_enrollment_control,
#   enroll_experimental_per_look = planned_enrollment_experimental,
#   adaptive = c(FALSE, TRUE),
#   final_full_spending = TRUE,
#   seed = 5001
# )
# 
# sim_power_big <- simBinomialSeasonalExact(
#   gsD = design_calendar,
#   ve = c(`H1 (VE=80%)` = ve1),
#   nsim = power_nsim,
#   control_event_rate = seasonal_event_rate_control,
#   season_length = season_length_years,
#   dropout_rate = dropout_6mo,
#   planned_counts = planned_counts,
#   enroll_control_per_look = planned_enrollment_control,
#   enroll_experimental_per_look = planned_enrollment_experimental,
#   adaptive = c(FALSE, TRUE),
#   final_full_spending = TRUE,
#   seed = 6001
# )

