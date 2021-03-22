

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
diagnostic <- function(status, sens, spec, positive = c("A", "I", "V")){
  prob <- rep(0, length(status))
  index <- status %in% positive
  prob[index] <- sens # True positive
  prob[!index] <- 1 - spec # False positive
  dx_result <- runif(lengtt(prob), 0, 1) < prob
  return(dx_result)
}
