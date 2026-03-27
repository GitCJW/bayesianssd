
### Test 'format'

test_that("test format.bayesianssd", {
  #Create an exemplary ssd
  powerDesired <- 0.8
  N <- 48
  NMax <- 99
  NMin <- 3
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(1, 0.7, 0.81, 0.8125),
    certainty = c(F,F,F,F),
    tendency = c(1, -1, 0, 1)
  )
  resultsSSD2 <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(0.79, 0.6, 0.7, 0.71),
    certainty = c(F,F,F,F),
    tendency = c(-1, -1, -1, -1)
  )
  resultsPowerBinomial <- data.frame(
    N = c(99, 39, 48, 51),
    powerMean = c(0.9166667, 0.6875000, 0.7934783, 0.8048780),
    powerLow = c(0.8389251, 0.5937902, 0.7450150, 0.7547323),
    powerHigh = c(0.9955778, 0.7824342, 0.8425751, 0.8557615)
  )
  possN <- seq(1:33)*3

  ssd <- list(
    intern = list(
      powerDesired = powerDesired,
      N = N,
      maxN = NMax,
      minN = NMin,
      resultsSSD = resultsSSD,
      possN = possN,
      NHigh = 51,
      resultsPowerBinomial = resultsPowerBinomial
    )
  )
  ssd2 <- list(
    intern = list(
      powerDesired = powerDesired,
      N = 150,
      maxN = NMax,
      minN = NMin,
      resultsSSD = resultsSSD2,
      possN = possN,
      NHigh = 150,
      resultsPowerBinomial = resultsPowerBinomial
    )
  )
  ssdNotClass <- ssd
  class(ssd) <- c("bayesianssd",class(ssd))
  class(ssd2) <- c("bayesianssd",class(ssd2))


  expect_error(format(ssdNotClass))
  expect_no_warning(format(ssd))
  expect_no_warning(format(ssd2))
})



### Test 'print'

test_that("test print.bayesianssd", {
  #Create an exemplary ssd
  powerDesired <- 0.8
  N <- 48
  NMax <- 99
  NMin <- 3
  resultsSSD <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(1, 0.7, 0.81, 0.8125),
    certainty = c(F,F,F,F),
    tendency = c(1, -1, 0, 1)
  )
  resultsSSD2 <- data.frame(
    N = c(99,39,48,51),
    i = c(20,60,100,80),
    power = c(0.79, 0.6, 0.7, 0.71),
    certainty = c(F,F,F,F),
    tendency = c(-1, -1, -1, -1)
  )
  resultsPowerBinomial <- data.frame(
    N = c(99, 39, 48, 51),
    powerMean = c(0.9166667, 0.6875000, 0.7934783, 0.8048780),
    powerLow = c(0.8389251, 0.5937902, 0.7450150, 0.7547323),
    powerHigh = c(0.9955778, 0.7824342, 0.8425751, 0.8557615)
  )
  possN <- seq(1:33)*3

  ssd <- list(
    intern = list(
      powerDesired = powerDesired,
      N = N,
      maxN = NMax,
      minN = NMin,
      resultsSSD = resultsSSD,
      possN = possN,
      NHigh = 51,
      resultsPowerBinomial = resultsPowerBinomial
    )
  )
  ssd2 <- list(
    intern = list(
      powerDesired = powerDesired,
      N = 150,
      maxN = NMax,
      minN = NMin,
      resultsSSD = resultsSSD2,
      possN = possN,
      NHigh = 150,
      resultsPowerBinomial = resultsPowerBinomial
    )
  )
  ssdNotClass <- ssd
  class(ssd) <- c("bayesianssd",class(ssd))
  class(ssd2) <- c("bayesianssd",class(ssd2))


  expect_error(print(ssdNotClass))
  expect_no_warning(print(ssd))
  expect_no_warning(print(ssd2))
})

