#' @export
startSSDBayas <- function(model, initData, powerDesired, possN, goals,
                          con, iParallel){
  goals <- singleGoalAsList(goals)

  maxN <- max(possN)

  prob <- powerDesired
  alpha <- prob*(con-2)+1
  beta <- (1-prob)*(con-2)+1
  acceptHDI <- hdi(qbeta, shape1=alpha, shape2=beta, credMass=0.9)
  acceptHDIwidth <- acceptHDI[2] - acceptHDI[1]


  furtherArgs <- list(
    iParallel=iParallel,
    modelSeed = sample(1e5,iParallel),
    acceptHDIwidth = acceptHDIwidth,
    acceptHDIConcentration = con
  )

  ssd <- initSSD(model = model, data = initData,
                 powerDesired = powerDesired, possN = possN,
                 goals = goals,
                 furtherArgs = furtherArgs)
  return(ssd)
}

#' @export
updateSSDBayas <- function(ssd, newData){
  ssd <- continueSSD(ssd, newData)
  return(ssd)
}

#' @export
getValidNextNBayas <- function(nLow, nHigh, ssd){
  resultsSSD <- ssd$intern$resultsSSD
  nextN <- ssd$intern$N
  getValidNextN(nLow, nHigh, resultsSSD, nextN)
}
