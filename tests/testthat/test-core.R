### Test 'runSSD'

test_that("slow test runSSD rstanarm", {

  testthat::skip_if_not_installed("rstanarm")

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20),
                              family=poisson(), refresh=0)

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
   iParallel = 20)

  expect_no_error(ssd)
})

test_that("slow test runSSD brms", {

  testthat::skip_if_not_installed("brms")

  dataCreationFunction <- function(N){
    group_effects <- c(
      a = 12.7,
      b = 7.8
    )
    strain <- rep(c("a", "b"), length.out=N)
    y <- rnbinom(N, mu=group_effects, size=24)

    data <- data.frame(
      strain = strain,
      entries = y
    )
    data
  }

  prior <- brms::get_prior("entries~-1+strain", data=dataCreationFunction(20),
                           family=brms::negbinomial(link="identity"))
  prior[2,1] <- prior[3,1] <- "lognormal(2.3,1)"
  prior[4,1] <- "exponential(0.1)"

  model <- brms::brm("entries~-1+strain", data=dataCreationFunction(20),
                     family=brms::negbinomial(link="identity"),
                     prior = prior, refresh=0)

  goal <- createGoal(parametersA="b_straina", parametersB="b_strainb",
                     goalType="rope", ropeType="exclude", ropeLower=0, ropeUpper = 0,
                     ropeExclusive=T, ci=0.95)

  ssd <- runSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    minN = 2,
    maxN = 25,
    factorN = 5,
    goals = list(goal),
    con = 20,
    iParallel = 20)
})

test_that("slow test runSSD rstan", {

  testthat::skip_if_not_installed("rstan")

  dataCreationFunction <- function(N){
    group_effects <- c(
      control = 2,
      drug = -2
    )
    treatment <- rep(c("control", "drug"), length.out=N)
    y <- rnorm(N, mean=group_effects, sd=1)

    data <- list(
      N = N,
      treatment = rep(c(1,2), length.out=N),
      y = y
    )
    data
  }

  model <- rstan::stan_model(
    model_code =
      "data {
        int<lower=0> N;
        vector[N] y;
        array[N] int treatment;
      }
      parameters {
        array[2] real mu;
        real<lower=0> sigma;
      }
      model {
        y ~ normal(mu[treatment], sigma);
        mu ~ normal(0,2);
        sigma ~ normal(0,1);
      }"
  )

  goal <- createGoal(parametersA = "mu[1]", parametersB = "mu[2]",
                     goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)

  ssd <- runSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    minN = 2,
    maxN = 5,
    goals = list(goal),
    con = 20,
    iParallel = 20)

})


test_that("slow test runSSD precision", {

  testthat::skip_if_not_installed("rstanarm")

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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20),
                              family=poisson(), refresh=0)

  goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
                     goalType = "precision", precisionWidth = 2, ci = 0.95)

  ssd <- runSSD(
    model = model,
    dataCreationFunction = dataCreationFunction,
    powerDesired = 0.8,
    minN = 2,
    maxN = 6,
    goals = list(goal),
    con = 10,
    iParallel = 20)

  expect_no_error(ssd)
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

  model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20),
                              family=poisson(), refresh=0, seed=123)
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


