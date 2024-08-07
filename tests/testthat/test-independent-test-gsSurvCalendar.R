####################################################################################
###
###   Author: Myeongjong Kang (myeongjong.kang@merck.com)
###
###   Overview: Testing gsSurvCalendar function
###
###   Contents: Testing functions and unit testings
###
####################################################################################

#-----------------------------------------------------------------------------------
### Testing functions
#-----------------------------------------------------------------------------------

#' @title Comparing names of outputs of \code{gsSurv()} and \code{gsSurvCalendar()}
#'
#' @param dsgn1 Output of \code{gsSurv()}
#' @param dsgn2 Output of \code{gsSurvCalendar()}
comparison_names <- function(dsgn1, dsgn2)
{
  # Motivation: The list of gsSurvCalendar()'s outputs is supposed to include every element of the list of the gsSurv()'s outputs.
  testthat::expect_contains(names(dsgn2), names(dsgn1))
}

#' @title Comparing inputs of \code{gsSurv()} and \code{gsSurvCalendar()}
#'
#' @param dsgn1 Output of \code{gsSurv()}
#' @param dsgn2 Output of \code{gsSurvCalendar()}
#' @param tolerance A numerical tolerance used when comparing numerical values at \code{1e-6} by default
comparison_inputs <- function(dsgn1, dsgn2, tolerance = 1e-6)
{
  # Motivation: For fair comparison, inputs of gsSurvCalendar() are assumed to be equivalent to corresponding inputs of gsSurv().
  testthat::expect_identical(dsgn1$k, dsgn2$k)
  
  # Motivation: The following comparisons can be useful for future unit testing development, 
  #             since default values of the inputs are different for gsSurv() and gsSurvCalendar().
  testthat::expect_equal(dsgn1$minfup, dsgn2$minfup, tolerance = tolerance)
  testthat::expect_equal(dsgn1$tol, dsgn2$tol, tolerance = tolerance)
}

#' @title Comparing names of outputs of \code{gsSurv()} and \code{gsSurvCalendar()}
#'
#' @param dsgn1 Output of \code{gsSurv()}
#' @param dsgn2 Output of \code{gsSurvCalendar()}
#' @param tolerance A numerical tolerance used when comparing numerical values at \code{1e-6} by default
comparison_outputs <- function(dsgn1, dsgn2, tolerance = 1e-6)
{
  # Motivation: For direct comparison, outputs of gsSurvCalendar() are compared to corresponding outputs of gsSurv(). 
  #             The list is inspired by gsBoundSummary().
  testthat::expect_equal(dsgn1$delta, dsgn2$delta, tolerance = tolerance)
  testthat::expect_equal(dsgn1$delta0, dsgn2$delta0, tolerance = tolerance)
  testthat::expect_equal(dsgn1$delta1, dsgn2$delta1, tolerance = tolerance)
  testthat::expect_equal(dsgn1$theta, dsgn2$theta, tolerance = tolerance)
  testthat::expect_equal(dsgn1$ratio, dsgn2$ratio, tolerance = tolerance)
  
  testthat::expect_equal(dsgn1$lower$bound, dsgn2$lower$bound, tolerance = tolerance)
  testthat::expect_equal(dsgn1$upper$bound, dsgn2$upper$bound, tolerance = tolerance)
  
  testthat::expect_equal(dsgn1$n.fix, dsgn2$n.fix, tolerance = tolerance)
  testthat::expect_equal(dsgn1$n.I, dsgn2$n.I, tolerance = tolerance)
  testthat::expect_equal(dsgn1$nFixSurv, dsgn2$nFixSurv, tolerance = tolerance)
  
  testthat::expect_equal(dsgn1$eNE, dsgn2$eNE, tolerance = tolerance)
  testthat::expect_equal(dsgn1$eNC, dsgn2$eNC, tolerance = tolerance)
  
  testthat::expect_equal(dsgn1$T, dsgn2$T, tolerance = tolerance)
}

#-----------------------------------------------------------------------------------
### Unit testings
#-----------------------------------------------------------------------------------

### Check if gsSurvCalendar can mimic gsSurv with different test types

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 1 and default values for the other arguments)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 1, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 1, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 2 and default values for the other arguments)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 2, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 2, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 3 and default values for the other arguments)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 3, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 3, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 4 and default values for the other arguments)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 5 and default values for the other arguments)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 5, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 5, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 6 and default values for the other arguments)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 6, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 6, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

### Check if gsSurv can mimic gsSurvCalendar with different test types

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 1 and default values for the other arguments)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 1, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 1, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 2 and default values for the other arguments)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 2, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 2, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 3 and default values for the other arguments)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 3, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 3, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 4 and default values for the other arguments)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 5 and default values for the other arguments)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 5, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 5, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 6 and default values for the other arguments)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 6, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 6, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

### Check if gsSurv and gsSurvcalendar can mimic each other with extreme argument values

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 4, alpha = 0.0001, and beta = 0.01)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 4, alpha = 0.0001, sided = 1, beta = 0.01, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 4, alpha = 0.0001, sided = 1, beta = 0.01, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 4, alpha = 0.0001, and beta = 0.01)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 4, alpha = 0.0001, sided = 1, beta = 0.01, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 4, alpha = 0.0001, sided = 1, beta = 0.01, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 4 and 9 planned analyses)",
  code = {
    dsgn01 <- gsSurv(k = 9, 
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02, tolerance = 1e-5) # Please note that we are using tol = 1e-5 instead of 1e-6 for comparison. The number of planned analyses may impact the numerical accuracy or precision of the functions.
    comparison_outputs(dsgn01, dsgn02, tolerance = 1e-5)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 4 and 9 planned analyses)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             calendarTime = seq(from = 4, to = 36, by = 4), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 9, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03, tolerance = 1e-5)
    comparison_outputs(dsgn04, dsgn03, tolerance = 1e-5)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 4 and sfu = sfl = sfLDOF)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 4 and sfu = sfl = sfLDOF)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfLDOF, sfupar = NULL, sfl = gsDesign::sfLDOF, sflpar = NULL,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfLDOF, sfupar = NULL, sfl = gsDesign::sfLDOF, sflpar = NULL, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

testthat::test_that(
  desc = "From information timing to calendar timing (test.type = 4 and sfu = sfl = sfLDPocock)",
  code = {
    dsgn01 <- gsSurv(k = 3, 
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0, timing = 1, 
                     sfu = gsDesign::sfLDPocock, sfupar = -4, sfl = gsDesign::sfLDPocock, sflpar = -2,
                     r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, T = 18, minfup = 6, ratio = 1,
                     tol = .Machine$double.eps^0.25, usTime = NULL, lsTime = NULL)
    
    dsgn02 <- gsSurvCalendar(spending = "information", calendarTime = dsgn01$T, minfup = 6, tol = .Machine$double.eps^0.25, # These arguments' values are different from their default values.
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfLDPocock, sfupar = -4, sfl = gsDesign::sfLDPocock, sflpar = -2,
                             r = 18, lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL, 
                             gamma = 1, R = 12, S = NULL, ratio = 1)
    
    # dsgn02 is supposed to be the same as dsgn01.
    comparison_names(dsgn01, dsgn02)
    comparison_inputs(dsgn01, dsgn02)
    comparison_outputs(dsgn01, dsgn02)
  }
)

testthat::test_that(
  desc = "From calendar timing to information timing (test.type = 4 and sfu = sfl = sfLDPocock)",
  code = {
    dsgn03 <- gsSurvCalendar(spending = "information",
                             test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                             sfu = gsDesign::sfLDPocock, sfupar = NULL, sfl = gsDesign::sfLDPocock, sflpar = NULL,
                             calendarTime = c(12, 24, 36), 
                             lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                             gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                             tol = 1e-06)
    
    # Calculate the expected numbers of events for time points
    nevents <- sapply(dsgn03$T, 
                      function(x) nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[2]] + 
                        nEventsIA(x = dsgn03, tIA = x, simple = FALSE)[[3]])
    
    dsgn04 <- gsSurv(k = 3, timing = nevents / tail(nevents, 1), T = tail(dsgn03$T, 1), minfup = 18, tol = 1e-06, # These arguments' values are different from their default values.
                     test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                     sfu = gsDesign::sfLDPocock, sfupar = NULL, sfl = gsDesign::sfLDPocock, sflpar = NULL, 
                     lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                     gamma = 1, R = 12, S = NULL, ratio = 1, r = 18, 
                     usTime = NULL, lsTime = NULL)
    
    # dsgn04 is supposed to be the same as dsgn03.
    comparison_names(dsgn04, dsgn03)
    comparison_inputs(dsgn04, dsgn03)
    comparison_outputs(dsgn04, dsgn03)
  }
)

### Check if gsSurvcalendar (with different spending types) can provide an error when the arguments are not desirable

testthat::test_that(
  desc = "Performing an interim analysis for each timepoint is not an easy task (with spending = information)",
  code = {
    testthat::expect_error(
      object = gsSurvCalendar(spending = "information",
                              test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                              sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                              calendarTime = seq(from = 4, to = 36, by = 1), 
                              lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                              gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                              tol = 1e-06),
      regexp = NULL)
  }
)

testthat::test_that(
  desc = "A new user may be confused with the meaning of test.type (with spending = information)",
  code = {
    testthat::expect_error(
      object = gsSurvCalendar(spending = "information",
                              test.type = "one-sided", alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                              sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                              calendarTime = c(12, 24, 36), 
                              lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                              gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                              tol = 1e-06),
      regexp = NULL)
  }
)

testthat::test_that(
  desc = "Minimum follow-up must be smaller than study duration (with spending = information)",
  code = {
    testthat::expect_error(
      object = gsSurvCalendar(spending = "information",
                              test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                              sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                              calendarTime = c(4, 8, 12, 16), 
                              lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                              gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                              tol = 1e-06),
      regexp = NULL)
  }
)

testthat::test_that(
  desc = "Performing an interim analysis for each timepoint is not an easy task (with spending = calendar)",
  code = {
    testthat::expect_error(
      object = gsSurvCalendar(spending = "calendar",
                              test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                              sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                              calendarTime = seq(from = 4, to = 36, by = 1), 
                              lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                              gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                              tol = 1e-06),
      regexp = NULL)
  }
)

testthat::test_that(
  desc = "A new user may be confused with the meaning of test.type (with spending = calendar)",
  code = {
    testthat::expect_error(
      object = gsSurvCalendar(spending = "calendar",
                              test.type = "one-sided", alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                              sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                              calendarTime = c(12, 24, 36), 
                              lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                              gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                              tol = 1e-06),
      regexp = NULL)
  }
)

testthat::test_that(
  desc = "Minimum follow-up must be smaller than study duration (with spending = calendar)",
  code = {
    testthat::expect_error(
      object = gsSurvCalendar(spending = "calendar",
                              test.type = 4, alpha = 0.025, sided = 1, beta = 0.1, astar = 0,
                              sfu = gsDesign::sfHSD, sfupar = -4, sfl = gsDesign::sfHSD, sflpar = -2,
                              calendarTime = c(4, 8, 12, 16), 
                              lambdaC = log(2)/6, hr = 0.6, hr0 = 1, eta = 0, etaE = NULL,
                              gamma = 1, R = 12, S = NULL, minfup = 18, ratio = 1, r = 18,
                              tol = 1e-06),
      regexp = NULL)
  }
)

