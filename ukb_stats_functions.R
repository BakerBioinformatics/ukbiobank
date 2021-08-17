
# Functions for plotting and computing simple tables values


#### Simple stats functions

# Returns the percentage and number of occurrences of 'value' in the column 'field'
get_count_per <- function(dataset, field, value, decimals=1) {
  n <- nrow(dataset)  
	count <- dataset[ , c(field)]
	count <- length(count[count == value])
	return(paste0(count, " (", format(round(100*count/n, decimals), nsmall = decimals), ")"))
}


# Returns the mean and sd of the column 'field'. 
get_mean_sd <- function(dataset, field, decimals=1) {
  values <- dataset[ , c(field)]
  m <- format(round(mean(values), decimals), nsmall = decimals)
  s <- format(round(sd(values), decimals), nsmall = decimals)
  return(paste0(m, " \u00b1 ", s))
}


# Returns the df dataframe, but attach an empty row followed by one row per possible value on 
#the column 'field' and its respective percentage and number of occurrences. Field MUST be a 
#factor type 
categorical_field <- function(dataset, df, field){  
  #add empty space
  df[nrow(df) + 1, ] <- " "
  for(i in levels(dataset[ , c(field)]))
    df[nrow(df) + 1, ] <- get_count_per(dataset, field, i, nrow(dataset))
  return(df)
}



# Functions to plot statistical summaries using the UKB data or tables


