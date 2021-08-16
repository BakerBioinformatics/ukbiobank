
# Functions related to the main UKB dataset (may be used on other datasets)

library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


#Notes: All functions assume that my_data is a data.frame
 

#Given a field name (must have the start of the field name. ex. treatmentmedication_code)
#or field number (ex. "3421") extract all the rows and all their traits and phenotypes
#such that the given codes are within the especified field
filter_data_by_field_codes <- function(my_data, field, codes, truncated=FALSE) {
  pattern <- "^eid"
  if(is.numeric(field)) {
    pattern <- paste(pattern, "|_f", field, "_", sep="")
  } else {
    pattern <- paste(pattern, "|^", field, sep="")
  }
	my_data[ , grepl(pattern, colnames(my_data))] 
  selection <- my_data[ , grepl(pattern, colnames(my_data))]
	selection$eid <- paste0("#", selection$eid)
	if(truncated) {
		codes <- paste0("^",codes) #starting wit
		selection <- selection %>% filter(rowSums(across(everything(), 
																										 ~ str_detect(.x, paste(codes,collapse = '|'))
																										 ), 
																							na.rm = TRUE) > 0)
	}
	else 
		selection <- selection %>% filter(rowSums(across(everything(), ~ .x %in% codes),  
																							na.rm = TRUE) > 0)
	data <- (my_data %>% filter(paste0("#", eid) %in% selection$eid))
  return(data)
}

#Extract all selected columns field number
#ex: "c(30010, 58)"
extract_columns_by_field_number <- function(my_data, field_numbers) {
  print("extracting columns by field number: ")
  pattern <- "^eid"
  field_numbers <- paste("_f", field_numbers, "_", sep="")
  pattern <- paste(pattern,"|", paste(field_numbers, collapse="|"), sep="")
  print(pattern)
  data <- my_data[ , grepl(pattern, colnames(my_data))]
  return(data)
}

#Extract all selected columns that starts with a given name pattern
#ex: "calcium"
#If we want to extract exact name just use my_data[, colnames]
extract_columns_by_names <- function(my_data, col_names) {
  pattern <- "^eid"
  pattern <- paste(pattern, "|^", paste(col_names, collapse="|^"), sep="")
  data <- my_data[ , grepl(pattern, colnames(my_data))]
  return(data)
}
