#' Runs a sample size determination
#'
#' This function determines the sample size required for a well-defined model
#' based on a specific condition or 'goal'.
#'
#'@usage
#'runSSD(
#'  model,
#'  dataCreationFunction,
#'  powerDesired = 0.8,
#'  minN = 2,
#'  maxN = 100,
#'  factorN = 1,
#'  goals,
#'  con = 200,
#'  iParallel = 10)
#'
#' @param model An object of class \code{stanmodel, stanreg or brmsfit}.
#' @param dataCreationFunction A function that accepts a single parameter,
#' \code{N}, and should generates N values in the same manner as the given \code{model}.
#' @param powerDesired The probability of achieving the goal (condition)
#' when the experiment is repeated.
#' @param minN The minimum number of data points. If your data contains e.g. 4
#' groups the minimum number should be at least 4.
#' @param maxN The maximum allowable sample size. Larger values will increase
#' the time required for the determination.
#' @param factorN Set this factor when your data includes groups that should be
#' treated equally.
#' For example, if your \code{dataCreationFunction} generates data for a control
#' group and two treatment groups,
#' you can use a factor of 3 to ensure that each group has the same number of samples.
#' Alternatively, you can provide a vector of permitted N values.
#' @param goals The condition be tested. Use the function \link[bayesianssd]{createGoal}
#' to create such goals.
#' @param con The rough number of simulations needed for each sample size. Default: 200
#' @param iParallel Number of runs in parallel, if an appropriate strategy is chosen
#' (check \link[future]{plan}). Default: 20.
#' @return A named list with the results of the sample size determination.
#' @export
#'
#' @examplesIf rlang::is_installed("rstanarm") && interactive()
#'dataCreationFunction <- function(N){
#'  group_effects <- c(
#'    control = 13,
#'    drug = 7
#'  )
#'  treatment <- rep(c("control", "drug"), length.out=N)
#'  y <- rpois(N, group_effects)
#'
#'  data <- data.frame(
#'    treatment = treatment,
#'    y = y
#'  )
#'  data
#'}
#'
#'model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#'goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
#'                   goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)
#'
#'checkSettings(model, dataCreationFunction, 2, list(goal))
#'
#'ssd <- runSSD(
#'  model = model,
#'  dataCreationFunction = dataCreationFunction,
#'  powerDesired = 0.8,
#'  minN = 2,
#'  maxN = 20,
#'  factorN = 5,
#'  goals = list(goal),
#'  con = 20,
#'  iParallel = 20)
#'
#'plot(ssd)
#'print(ssd)
runSSD <- function(model, dataCreationFunction, powerDesired=0.8, minN=2, maxN=100, factorN=1,
                    goals, con=200, iParallel=10){
  goals <- singleGoalAsList(goals)

  # check inputs
  verifySSDInputs(model, dataCreationFunction, powerDesired, minN, maxN, factorN=1,
                 goals, con, iParallel)

  possN <- c()
  if(length(factorN) == 1){
      possN <- (1:(floor(maxN / factorN)))*factorN
      possN <- possN[possN >= minN]
  }else{
      possN <- factorN
      possN <- possN[possN >= minN & possN <= maxN]
  }


  print("Compiling your model and start sample size determination ...")

  ssd <- tryCatch({
    startSSD(model, dataCreationFunction, powerDesired, possN,
            goals, con, iParallel)
  },
  error = function(e){
    print(e)
    stop("")
  })


  plot(ssd)

  cat("Simulate .")
  while(ssd$extern$continue){
    cat(".")

    ssd2 <- tryCatch({
      updateSSD(ssd, dataCreationFunction)
    },
    error = function(e){
      print(e)
    })
    if(is.null(ssd2)) return(ssd)
    ssd <- ssd2
    print(plot(ssd))
  }

  cat("\n")
  print("Sample size determination finished.")

  print(ssd)

  return(ssd)
}


#' Input validity check
#'
#' Checks if the input for a call to \code{run_sdd} is valid.
#'
#' @param model An object of class \code{stanmodel, stanreg or brmsfit}.
#' @param dataCreationFunction A function that accepts a single parameter,
#' \code{N}, and should generates N values in the same manner as the given \code{model}.
#' @param minN The minimum number of data points. If your data contains e.g. 4
#' groups the minimum number should be at least 4.
#' @param goals The condition be tested. Use the function
#' \link[bayesianssd]{createGoal}  to create such goals.
#'
#' @returns Logical scalar. `TRUE` if all required inputs are valid and consistent.
#' @export
#' @examplesIf rlang::is_installed("rstanarm")
#'dataCreationFunction <- function(N){
#'  group_effects <- c(
#'    control = 13,
#'    drug = 7
#'  )
#'  treatment <- rep(c("control", "drug"), length.out=N)
#'  y <- rpois(N, group_effects)
#'
#'  data <- data.frame(
#'    treatment = treatment,
#'    y = y
#'  )
#'  data
#'}
#'
#'model <- rstanarm::stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#'goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
#'                   goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)
#'
#'checkSettings(model, dataCreationFunction, 2, list(goal))
checkSettings <- function(model, dataCreationFunction, minN, goals){
  goals <- singleGoalAsList(goals)
  # check inputs
  verifySSDInputs.sub(model, dataCreationFunction, minN, goals)

  data <- dataCreationFunction(minN)

  fit <- NULL
  try({
    suppressWarnings({
      if ("stanmodel" %in% class(model)){
        fit <- rstan::sampling(model, data=data, refresh=0)
      }else if ("stanreg" %in% class(model)){
        fit <- stats::update(model, data=data, refresh = 0)
      }else{
        fit <- stats::update(model, newdata=data, refresh = 0)
      }
    })
  })

  if(is.null(fit)) stop("Unable to fit model")

  params <- as.matrix(fit)
  paraNames <- names(params[1,])

  for(goal in goals){
    parametersA <- goal$parametersA
    parametersB <- goal$parametersB
    if (!all(parametersA %in% paraNames))
      stop(paste0("Not all of the parameter names defined in 'ParametersA' exist. ",
                  "Available names: ", paste0(paraNames, collapse=", ")))
    if (!is.null(parametersB) && !all(parametersB %in% paraNames))
      stop(paste0("Not all of the parameter names defined in 'ParametersA' exist. ",
                  "Available names: ", paste0(paraNames, collapse=", ")))
  }
  print("Looks good!")
  return(TRUE)
}
