
# Functions for plotting and computing simple tables values


#### Simple stats functions

# Generally used for categorical variables, computes the Person Chi-square test between the reference column (x) and the studied parameter (y)
#x and y must be vectors or factors of the same length; cases with missing values are removed, the objects are coerced to factors, and the contingency table is computed from these.
pearson_csq <- function(dataset, x, y) {
  if(nlevels(as.factor(dataset[ ,c(x)])) < 2 | nlevels(as.factor(dataset[ ,c(y)])) < 2) {
		return("-")
	}
	d <- dataset[ ,c(x, y)]
	d <- d[!is.na(d[ ,c(x)]) & !is.na(d[ ,c(y)]), ]
	p <- chisq.test(d[ ,c(x)],  d[ , c(y)])
  p <- p$p.value
	min <- 0.0001
  if(p < min)
    return(paste0("< 0.0001"))
  else
    return(paste0(format(round(p, 4), nsmall = 4, scientific=F)))
}



# Generally used for continuous variables, computes the Welch T-test between the reference column (x) and the studied parameter (y)
welch_t_test <- function(dataset, x, y) {
	d <- dataset[ ,c(x, y)]
	#We assumed that x is a binary value 0,1
  d <- d[!is.na(d[ ,c(x)]) & !is.na(d[ ,c(y)]), ]
	d1 <- d[d[ ,c(x)]==1 , c(y)]
	d2 <- d[d[ ,c(x)]==0 , c(y)]
  p <- t.test(d1,  d2)
  p <- p$p.value
  min <- 0.0001
	if(p < min)
		return(paste0("< 0.0001"))
	else {
    return(paste0(format(round(p, 4), nsmall = 4, scientific=F)))
  }
}


# Returns the percentage and number of occurrences of 'value' in the column 'field'
get_count_per <- function(dataset, field, value, decimals=1) {
  n <- nrow(dataset)  
	count <- dataset[ , c(field)]
	count <- length(count[!is.na(count) & count == value])
	return(paste0(count, " (", format(round(100*count/n, decimals), nsmall = decimals), ")"))
}

get_count_per_na <- function(dataset, field, value, decimals=1) {
  n <- nrow(dataset)
  count <- dataset[ , c(field)]
	nas <- sum(is.na(count))
	count <- count[!is.na(count)]
  count <- length(count[count == value])
	if(nas > 0) 
		return(paste0(count, " (", format(round(100*count/n, decimals), nsmall = decimals), ") / ", nas))
	else
		return(paste0(count, " (", format(round(100*count/n, decimals), nsmall = decimals), ")"))
}


# Returns the mean and sd of the column 'field'. 
get_mean_sd <- function(dataset, field, decimals=1) {
  values <- dataset[ , c(field)]
  m <- format(round(mean(values), decimals), nsmall = decimals)
  s <- format(round(sd(values), decimals), nsmall = decimals)
  return(paste0(m, " \u00b1 ", s))
}


get_median_iqr <- function(dataset, field, decimals=1) {
	values <- dataset[ , c(field)]
	m <- format(round(median(values), decimals), nsmall = decimals)
	q25 <- format(round(quantile(values, 0.25), decimals), nsmall = decimals)
	q75 <- format(round(quantile(values, 0.75), decimals), nsmall = decimals)
	return(paste0(m, " (",q25,"-",q75,")"))
}

get_median_iqr_na <- function(dataset, field, decimals=1) {
  values <- dataset[ , c(field)]
	nas <- sum(is.na(values))
	values <- values[!is.na(values)]
  m <- format(round(median(values), decimals), nsmall = decimals)
  q25 <- format(round(quantile(values, 0.25), decimals), nsmall = decimals)
  q75 <- format(round(quantile(values, 0.75), decimals), nsmall = decimals)
	if(nas > 0)
		return(paste0(m, " (", q25, "-", q75,") / ", nas))
	else
		return(paste0(m, " (", q25, "-", q75, ")"))
}


# Returns the df dataframe, but attach an empty row followed by one row per possible value on 
#the column 'field' and its respective percentage and number of occurrences. Field MUST be a 
#factor type 
categorical_field <- function(dataset, df, field){  
  #add empty space
  df[nrow(df) + 1, ] <- c(" ")
  for(i in levels(dataset[ , c(field)]))
    df[nrow(df) + 1, ] <- get_count_per(dataset, field, i)
  return(df)
}



# Functions to plot statistical summaries using the UKB data or tables


