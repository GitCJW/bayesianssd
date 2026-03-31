#' Creates an exemplary ssd
#' @noRd
example1_bayesianssd <- function() {
  structure(
    list(
      intern = list(
        powerDesired = 0.8,
        N = 48,
        maxN = 99,
        minN = 3,
        resultsSSD = data.frame(
          N = c(99,39,48,51),
          i = c(20,60,100,80),
          power = c(1, 0.7, 0.81, 0.8125),
          certainty = c(F,F,F,F),
          tendency = c(1, -1, 0, 1)
        ),
        possN = seq(1:33)*3,
        NHigh = 51,
        resultsPowerBinomial = data.frame(
          N = c(99, 39, 48, 51),
          powerMean = c(0.9166667, 0.6875000, 0.7934783, 0.8048780),
          powerLow = c(0.8389251, 0.5937902, 0.7450150, 0.7547323),
          powerHigh = c(0.9955778, 0.7824342, 0.8425751, 0.8557615)
        )
      )
    ),
    class = "bayesianssd"
  )
}

#' Creates an exemplary ssd
#' @noRd
example2_bayesianssd <- function() {
  structure(
    list(
      intern = list(
        powerDesired = 0.8,
        N = 150,
        maxN = 99,
        minN = 3,
        resultsSSD = data.frame(
          N = c(99,39,48,51),
          i = c(20,60,100,80),
          power = c(0.79, 0.6, 0.7, 0.71),
          certainty = c(F,F,F,F),
          tendency = c(-1, -1, -1, -1)
        ),
        possN = seq(1:33)*3,
        NHigh = 150,
        resultsPowerBinomial = data.frame(
          N = c(99, 39, 48, 51),
          powerMean = c(0.9166667, 0.6875000, 0.7934783, 0.8048780),
          powerLow = c(0.8389251, 0.5937902, 0.7450150, 0.7547323),
          powerHigh = c(0.9955778, 0.7824342, 0.8425751, 0.8557615)
        )
      )
    ),
    class = "bayesianssd"
  )
}
