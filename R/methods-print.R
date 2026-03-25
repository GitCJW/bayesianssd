
#' Summary of sample size determination result
#'
#' @param ssd An object of class `"bayesianssd"` returned by \code{runSSD()}.
#' @export
#' @return A character scalar containing the textual summary of the sample size
#' determination.
#'
#' @examplesIf rlang::is_installed("rstanarm")
#' \dontest{
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
#'model <- stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#'goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
#'                   goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)
#'
#'checkSettings(model, dataCreationFunction, 2, list(goal))
#'
#'plan("multisession")
#'ssd <- runSSD(
#'  model = model,
#'  dataCreationFunction = dataCreationFunction,
#'  powerDesired = 0.8,
#'  minN = 2,
#'  maxN = 20,
#'  goals = list(goal),
#'  con = 200,
#'  iParallel = 20)
#'
#'summary <- format(ssd)
#'print(summary)
#'}
format.bayesianssd <- function(ssd){
  if(is.null(ssd) || !inherits(ssd, "bayesianssd"))
    stop("'ssd' must be a object of class 'bayesianssd'.")

  text <- ""

  intern <- ssd$intern
  res <- intern$resultsSSD

  NLow <- 0
  if (length(res$N[res$tendency<=0])>0){
    NLow <- max(res$N[res$tendency<=0])
  }

  if (NLow == intern$maxN) {
    text <- paste0(text, paste0("The desired power of at least ", intern$powerDesired,
                                " can't be reached with a maximum sample size N = ",
                                intern$maxN, ". ",
                                "\n",
                                "Expected power with N = ",
                                intern$maxN, ": ",
                                res$power[res$N==intern$maxN],
                                "\n",
                                "Estimated N to reach the desired power: ", intern$N, ". "))
  } else {

    pB <- intern$resultsPowerBinomial
    pB <- pB[pB$N == intern$NHigh,]

    text <- paste0(text, paste0("The desired power of ", intern$powerDesired,
                                " can be reached with N = ", pB$N,
                                " with a 90% credible interval of [",
                                round(pB$powerLow,2), ", ", round(pB$powerHigh,2), "]."))
  }
  return(text)
}


#' Prints sample size determination result
#'
#' @param ssd An 'bayesianssd' Object returned by \code{runSSD()}.
#' @export
#' @return The input object, invisibly.
#'
#' @examplesIf rlang::is_installed("rstanarm")
#' \dontest{
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
#'model <- stan_glm("y~-1+treatment", data=dataCreationFunction(20), family=poisson())
#'
#'goal <- createGoal(parametersA = "treatmentcontrol", parametersB = "treatmentdrug",
#'                   goalType = "rope", ropeType = "exclude", ropeLower = 0, ropeUpper = 0, ci = 0.95)
#'
#'checkSettings(model, dataCreationFunction, 2, list(goal))
#'
#'plan("multisession")
#'ssd <- runSSD(
#'  model = model,
#'  dataCreationFunction = dataCreationFunction,
#'  powerDesired = 0.8,
#'  minN = 2,
#'  maxN = 20,
#'  goals = list(goal),
#'  con = 200,
#'  iParallel = 20)
#'
#'print(ssd)
#'}
print.bayesianssd <- function(ssd){
  cat(formatSSD(ssd), "\n")
  return(invisible(ssd))
}

