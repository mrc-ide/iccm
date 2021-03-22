#' Diagnostic result
#'
#' Returns a boolean indicating results of a diagnostic test
#'
#' @param status Child status
#' @param sens Diagnostic sensitivity
#' @param spec Diagnostic specificity
#' @param positive Status categories associated that are true +ves
#'
#' @return Boolean vector, TRUE = positive test, FALSE = negative test
dx <- function(status, sens, spec, positive = c("A", "I", "V")){
  prob <- rep(0, length(status))
  index <- status %in% positive
  prob[index] <- sens # True positive
  prob[!index] <- 1 - spec # False positive
  dx_result <- runif(length(prob), 0, 1) < prob
  return(dx_result)
}

#' Treatment result
#'
#' @param disease Disease
#' @param efficacy Treatment efficacy against target disease
#' @param target_disease target_diseases
#'
#' @return Boolean vector, TRUE = cure, FALSE = no treatment impact
tx <- function(disease, efficacy, target_disease){
  prob <- rep(0, length(disease))
  prob[disease %in% target_disease] <- efficacy
  tx_result <- runif(length(prob), 0, 1) < prob
  return(tx_result)
}

#' Provider result
#'
#' A function to capture the efficacy of the provider - currently encapsulates all aspects of "human error".
#'
#' @param disease Disease
#' @param efficacy Provider efficacy
#'
#' @return
#' @export
#'
#' @examples
px <- function(disease, efficacy){
  prob <- rep(efficacy, length(disease))
  provider_result <- runif(length(prob), 0, 1) < prob
  return(provider_result)
}

treat <- function(variables){
  # Anything severe takes precedent
  #status_dia <-

}
