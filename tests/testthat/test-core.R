### Test 'runSSD'

test_that("test runSSD", {

  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_not_installed("rstanarm")
  skip_if_not(Sys.getenv("RUN_SLOW_TESTS") == "true")

  dataCreationFunction <- function(N){
   group_effects <- c(
     control = 13,
     drug = 7
   )
   treatment <- rep(c("control", "drug"), length.out=N)
   y <- rpois(N, group_effects)

   data <- data.frame(
     treatment = treatment,
     y = y
   )
   data
  }

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())

  goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
                    goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)

  ssd <- runSSD(
   model = model,
   dataCreationFunction = dataCreationFunction,
   powerDesired = 0.8,
   minN = 2,
   maxN = 20,
   goals = list(goal),
   con = 10,
   iParallel = 1)

})



### Test 'checkSettings'

test_that("test checkSettings", {
  testthat::skip_if_not_installed("rstanarm")

  set.seed(123)
  dataCreationFunction <- function(N){
    group_effects <- c(
      control = 9,
      drug = 7
    )
    treatment <- rep(c("control", "drug"), length.out=N)
    y <- rpois(N, group_effects)

    data <- data.frame(
      treatment = treatment,
      y = y
    )
    data
  }

  dataCreationFunctionWrong <- function(N){
    group_effects <- c(
      control = 9,
      drug = 7
    )
    treatment <- rep(c("control", "drug"), length.out=N)
    y <- rpois(N, group_effects)

    data <- data.frame(
      treatment = treatment,
      x = y
    )
    data
  }

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson(), refresh=0, seed=123)
  modelWrong <- glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
  class(modelWrong) <- c("stanmodel", class(modelWrong))

  goal <- createGoal(parametersA="treatmentcontrol", parametersB="treatmentdrug",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  goalWrong <- goal
  goalWrong$parametersA <- "test"

  expect_true(checkSettings(model, dataCreationFunction, 2, list(goal)))
  expect_error(checkSettings(model, dataCreationFunction, 2, list(goalWrong)))
  expect_error(checkSettings(model, dataCreationFunctionWrong, 2, list(goal)))
  expect_error(checkSettings(modelWrong, dataCreationFunction, 2, list(goal)))
})


