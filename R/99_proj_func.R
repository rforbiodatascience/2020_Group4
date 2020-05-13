rm(list = ls())

# Functions

# Removes ', %' from a string
rm_percent <- function(string){
  if( endsWith(string, ", %")){
    string <- string %>%
      str_sub(start = 1, end = str_length(string)-3)
  }
  return(string)
}

# Check if any string in list is a substring of the string variable
detect_in_list <- function(string, list){
 in_list <- string %>%
   str_detect(pattern = str_c(list, collapse = "|"))
 return(in_list)
}

# Returns the sum of values that are not 0
is_not_zero <- function(data){
  not_zero <- sum(data != 0)
  return(not_zero)
}

