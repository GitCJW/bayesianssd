
#' @title
#' Single goal for a sample size determination
#'
#' @description
#' Creates a single goal for a sample size determination passed to \code{runSSD}.
#'
#'@usage
#'createGoal(
#'   parametersA,
#'   parametersB = NULL,
#'   goalType = c("rope","precision"),
#'   ci = 0.95,
#'   ropeType = c("exclude","include"),
#'   ropeLower = NULL,
#'   ropeUpper = NULL,
#'   ropeExclusive = TRUE,
#'   precisionWidth = NULL
#')
#'
#' @param parametersA A vector of parameters defining the first 'group' of posteriors.
#' @param parametersB A vector of parameters defining the second 'group' of posteriors.
#' The difference of the two posterior sums defined by \code{parametersA} and \code{parametersB} (if set, otherwise 0) is the target posterior.
#' @param goalType One of the two goal types \code{"rope", "precision"}.
#' @param ci The target credible interval of the posterior.
#' @param ropeType One of the two rope types \code{"exclude", "include"}.
#' For \code{ropeType="exclude"} the credible interval must be outside the ROPE.
#' If \code{ropeType="include"}, the credible interval must be inside the ROPE.
#' Used only when \code{goalType="rope"}.
#' @param ropeLower Lower bound of the ROPE. Used only when \code{goalType="rope"}.
#' @param ropeUpper Upper bound of the ROPE. Used only when \code{goalType="rope"}.
#' @param precisionWidth The maximum allowed width of the credible interval. Used only when \code{goalType="precision"}.
#' @param ropeExclusive Used only for a ROPE and \code{'exclude'}, default TRUE.
#' If set to TRUE: When the posterior extends beyond both bounds of the ROPE, the larger fraction of the posterior outside the ROPE is used.
#' If set to FALSE: The difference between the two fractions outside the ROPE is calculated.
#'
#' @return An Object of class `"bayesianssdgoal"` that can be passed to \code{runSSD}.
#' @export
#'
#' @examples
#' createGoal(
#' parametersA="mu[1]", parametersB="mu[2]",
#' goalType="rope", ropeType="exclude",
#' ropeLower=-0.1, ropeUpper = 0.1, ropeExclusive=TRUE,
#' ci=0.95)
createGoal <- function(
    parametersA, parametersB=NULL,
    goalType=c("rope","precision"), ci=0.95,
    ropeType=c("exclude","include"), ropeLower=NULL, ropeUpper=NULL,
    ropeExclusive=TRUE,
    precisionWidth=NULL){

  if (length(goalType) != 1 || !goalType %in% c("rope","precision"))
    stop("goalType must be one of 'rope' or 'precision'")

  goal <- list(
    parametersA=c(),
    parametersB=c(),
    type = goalType,
    hdi = NULL,
    precWidth = NULL,
    ropeExclude = "",
    ropeLower = NULL,
    ropeUpper = NULL,
    ropeExclusive = NULL
  )

  if (goalType=="rope"){
    if (length(ropeType) != 1 || !ropeType %in% c("exclude","include"))
      stop("ropeType must be one of 'exclude' or 'include'")
    if (is.null(ropeLower)) stop("ropeLower must be set.")
    if (is.null(ropeUpper)) stop("ropeUpper must be set.")
    if (!is.numeric(ropeLower)) stop("ropeLower must be numeric.")
    if (!is.numeric(ropeUpper)) stop("ropeUpper must be numeric.")
    if (ropeLower > ropeUpper) stop("ropeLower must be smaller than ropeUpper.")
    if (ropeType == "exclude"){
      if (!is.logical(ropeExclusive) || is.na(ropeExclusive))
        stop("ropeExclusive must be TRUE or FALSE")
      goal$ropeExclusive <- ropeExclusive
    }
    goal$ropeExclude <- ropeType
    goal$ropeLower <- ropeLower
    goal$ropeUpper <- ropeUpper
  }else{
    if (is.null(precisionWidth)) stop("precisionWidth must be set.")
    if (!is.numeric(precisionWidth)) stop("precisionWidth must be numeric.")
    if (precisionWidth<=0) stop("precisionWidth must be positive.")
    goal$precWidth <- precisionWidth
  }

  if (is.null(ci))  stop("ci must be set.")
  if (!is.numeric(ci)) stop("ci must be numeric.")
  if (ci>1 || ci<0) stop("ci must be in the range [0,1].")
  goal$hdi <- ci

  if (is.null(parametersA)) stop("parametersA must be set.")
  goal$parametersA <- parametersA
  goal$parametersB <- parametersB

  class(goal) <- c("bayesianssdgoal",class(goal))
  return(goal)
}




#' @title
#' Goal list for a sample size determination
#'
#' @description
#' Creates a list of goals for a sample size determination passed to \code{runSSD}.
#'
#'@usage
#'goalList(...)
#'
#' @param ... Object(s) of  class `"bayesianssdgoal"`.
#'
#' @return An Object of class `"bayesianssdgoallist"` that can be passed to \code{plot}.
#' @export
#'
#' @examples
#' goal1 <- createGoal(
#' parametersA="mu[1]", parametersB="mu[2]",
#' goalType="rope", ropeType="exclude",
#' ropeLower=-0.1, ropeUpper = 0.1, ropeExclusive=TRUE,
#' ci=0.95)
#'
#' goal2 <- createGoal(
#' parametersA="mu[1]", parametersB="mu[3]",
#' goalType="rope", ropeType="exclude",
#' ropeLower=-0.1, ropeUpper = 0.1, ropeExclusive=TRUE,
#' ci=0.95)
#'
#' listOfGoals <- goalList(goal1, goal2)
goalList <- function(...){
  goals <- list(...)

  if (length(goals) == 0) {
    stop("At least one goal must be provided.")
  }

  ok <- vapply(goals, inherits, logical(1), "bayesianssdgoal")
  if (!all(ok)) {
    stop("All inputs must be of class 'bayesianssdgoal'.")
  }

  # auto-name if missing
  if (is.null(names(goals))) {
    names(goals) <- paste0("goal", seq_along(goals))
  } else {
    empty <- names(goals) == ""
    names(goals)[empty] <- paste0("goal", which(empty))
  }

  class(goals) <- c("bayesianssdgoallist", "list")
  return(goals)
}
