
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


#Match dates with codes 
#ex: "type_of_cancer_icd10_f40006_" and "date_of_cancer_diagnosis_f40005_"
get_dates_and_codes <- function(my_data, codes_column, dates_column, codes=NULL, truncated=FALSE) {
	codes <- my_data %>% pivot_longer(cols=starts_with(codes_column), 
																		names_to = "array", names_prefix=codes_column, 
																		values_to= "code", values_drop_na=FALSE) %>% select(eid, array, code)
	dates <- my_data %>% pivot_longer(cols=starts_with(dates_column),
                                    names_to = "array", names_prefix=dates_column,
                                    values_to= "date", values_drop_na=FALSE) %>% select(eid, array, date)
	#Merge and remove NAs
	d_and_c <- inner_join(codes, dates, by = c("eid", "array")) %>% drop_na()
	#Filter by codes
	if(!is.null(codes)) {
		if(truncated) 
			d_and_c <- filter(d_and_c, code %like% paste(codes, collapse = '|'))
		else
			d_and_c <- filter(d_and_c, code %in% codes)
	}
	return(d_and_c)
}



