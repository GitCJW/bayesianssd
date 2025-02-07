#' Calculates the 90% credible interval for a vector of success and failure counts.
#' @noRd
powerEstimator <- function(successes){
  nSuc <- sum(successes)
  nFail <- length(successes) - nSuc
  a <- nSuc +2
  b <- nFail +2
  ret <- hdi(qbeta, shape1 = a, shape2 = b, credMass = 0.90)
  pwrWidth <- ret[2] - ret[1]
  pwrLow <- ret[1]
  pwrHigh <- ret[2]
  pwrMean <- a/(a+b)

  return(list(pwrWidth=pwrWidth, pwrLow=pwrLow, pwrHigh=pwrHigh, pwrMean=pwrMean))
}
