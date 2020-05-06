# Functions

# Returns the sum of values that are not 0
is_not_zero <- function(data){
  not_zero <- sum(data != 0)
  return(not_zero)
}

# Detect if a string contains any substring from a list of substrings
detect_in_list <- function(string, list){
  in_list <- string %>% 
    str_detect(pattern = str_c(list, collapse = "|"))
  return(in_list)
}