# Functions

# Returns the sum of values that are not 0
is_not_zero <- function(data){
  not_zero <- sum(data != 0)
  return(not_zero)
}