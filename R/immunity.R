
maternal_immunity <- function(age, hl){
  1 - exp(-age * (1 / hl))
}
