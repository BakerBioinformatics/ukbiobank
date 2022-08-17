#!/usr/bin/env Rscript
library(ggplot2)
library(data.table)
library(dplyr) # union function for dataframe
library(tidyr)
library(lubridate)
library(stringr)

#list of biomarkers base on 
#http://www.ukbiobank.ac.uk/wp-content/uploads/2018/11/BCM023_ukb_biomarker_panel_website_v1.0-Aug-2015-edit-2018.pdf
biomarkers_all <- c("cholesterol","ldl_direct","lipoprotein_a",
								"hdl_cholesterol","triglycerides", "apolipoprotein",
								"vitamin_d", "rheumatoid_factor","alkaline_phosphatase",
								"calcium","shbg","testosterone","oestradiol",
								"igf1","glycated_haemoglobin_hba1c","glucose",
								"cystatin_c","creatinine","total_protein",
								"urea","phosphate","urate","creatinine_enzymatic_in_urine",
								"sodium_in_urine","microalbumin_in_urine", "potassium",
								"albumin","total_bilirubin","direct_bilirubin",
								"gamma_glutamyltransferase","alanine_aminotransferase",
								"aspartate_aminotransferase","creactive_protein")

biomarkers_value <- c("cholesterol_f30690","ldl_direct_f30780",
										 "lipoprotein_a_f30790","hdl_cholesterol_f30760",
										 "triglycerides_f30870", "apolipoprotein_a_f30630",
										 "apolipoprotein_b_f30640","vitamin_d_f30890", 
										 "rheumatoid_factor_f30820","alkaline_phosphatase_f30610",
										 "calcium_f30680","shbg_f30830",
										 "testosterone_f30850","oestradiol_f30800",
										 "igf1_f30770","glycated_haemoglobin_hba1c_f30750",
										 "glucose_f30740","cystatin_c_f30720",
										 "creatinine_f30700","total_protein_f30860",
										 "urea_f30670","phosphate_f30810",
										 "urate_f30880","sodium_in_urine_f30530",
										 "creatinine_enzymatic_in_urine_f30510",
										 "microalbumin_in_urine_f30500","potassium_in_urine_f30520",
										 "albumin_f30600","total_bilirubin_f30840",
										 "direct_bilirubin_f30660","gamma_glutamyltransferase_f30730",
										 "alanine_aminotransferase_f30620","aspartate_aminotransferase_f30650",
										 "creactive_protein_f30710")
	

#Create a file 'filename' containin all the individuals ids (eid) of my_data
print_individuals <- function(my_data, filename) {
	sink(filename)
	cat(my_data$eid, sep="\n")
	sink()
}

print_data <- function(my_data, filename) {
	write.table(my_data, file=filename, row.names=FALSE, quote=FALSE)
}
#Given a field name (must have the start of the field name. ex. treatmentmedication_code) 
#or field number (ex. "3421") extract all the rows and all their traits and phenotypes 
#such that the given codes are within the especified field
extract_data_given_field_codes <- function(my_data, field, codes) {
	pattern <- "^eid"
	if(is.numeric(field)) {
		pattern <- paste(pattern, "|f", field, "_", sep="")
	}
	else {
		pattern <- paste(pattern, "|^", field, sep="")
	}
  selection <- my_data[ , grepl(pattern, colnames(my_data))]
  selection <- selection %>% filter_all(any_vars(. %in% codes))
  data <- (my_data %>% filter(eid %in% selection$eid))
  return(data)
}

extract_self_reported_nc <- function(my_data, codes, visit = NULL) {
	pattern <- "^eid|^noncancer_illness_code_selfreported_f20002"
	if(is.numeric(visit)){
		if(visit %in% c(0,1,2,3)){
		pattern <- paste(pattern,"_",visit,sep="")
		}
	}
	illness_nc <- my_data[ , grepl(pattern, colnames(my_data))]
#	illness_nc <- illness_nc %>% filter_at(vars(columns), any_vars(. %in% codes)) 
	illness_nc <- illness_nc %>% filter_all(any_vars(. %in% codes))
	#x2 <- illness_nc %>% filter(rowSums(across(everything(), ~ .x %in% codes)) > 0)
	data <- (my_data %>% filter(eid %in% illness_nc$eid))
	return(data)
}

############   Treatment or Medication functions ######################

#check for treatment or medication codes
extract_treatment_medication <- function(my_data, codes, visit=NULL) {
	pattern <- "^eid|^treatmentmedication_code_f20003"
	if(is.numeric(visit)) {
		if(visit %in% c(0,1,2,3)){
			pattern <- paste(pattern,"_",visit,sep="")
		}
  }
	treat_med <- my_data[ , grepl(pattern, colnames(my_data))]
  treat_med <- treat_med %>% filter_all(any_vars(. %in% codes))
  data <- (my_data %>% filter(eid %in% treat_med$eid))
  return(data)
}

#Returns a table witht the following header
# eid treatmed date
#TODO: Change code to use dplyr
get_date_treatmed <- function(my_data, codes){
  df <- data.frame(eid=character(),
                   treatmed=character(),
                   date=character())
  tm_codes <- "treatmentmedication_code_f20003"
  tm_dates <- "date_of_attending_assessment_centre_f53"
  tmp_df <- extract_treatment_medication(my_data, codes)
  tmp_df <- extract_columns_by_names(my_data, c("eid", tm_codes, tm_dates))
  for(v in 0:3) {
		date_col <- paste(tm_dates, "_", v , "_0", sep="")
		for(m in 0:47) {
			tm_col <- paste(tm_codes, "_", v, "_", m, sep="")
			aux <- tmp_df[ , c("eid", tm_col, date_col)]
			aux <- extract_treatment_medication(aux, codes)
			if(nrow(aux)){
				colnames(aux) <- c("eid","treatmed","date_treat")
				df <- rbind(df,aux)
			}
		}
  }
  return(df)
}


#############    ICD9 Functions #######################################

#check for any diagnosis ICD9 codes starting with any string in "codes"
extract_diagnose_icd9_truncated <- function(my_data, codes) {
  diag_icd9 <- my_data[ , grepl("^eid|^diagnoses_icd9_f41271", colnames(my_data))]
	codes <- paste0("^",codes) #starting with
	diag_icd9$eid <- paste0("#",diag_icd9$eid)
  diag_icd9 <- diag_icd9 %>% filter_all(any_vars(str_detect(.,paste(codes,collapse = '|'))))
  data <- (my_data %>% filter(paste0("#",eid) %in% diag_icd9$eid))
  return(data)
}

extract_diagnose_icd9 <- function(my_data, codes) {
  diag_icd9 <- my_data[ , grepl("^eid|^diagnoses_icd9_f41271", colnames(my_data))]
  diag_icd9 <- diag_icd9 %>% filter_all(any_vars(. %in% codes))
  data <- (my_data %>% filter(eid %in% diag_icd9$eid))
  return(data)
}

get_date_icd9_new <- function(my_data, codes){
  df <- data.frame(eid=character(), 
                   icd9=character(),
                   date=character())
  icd_codes <- "diagnoses_icd9_f41271"
  icd_dates <- "date_of_first_inpatient_diagnosis_icd9_f41281"
  tmp_df <- extract_diagnose_icd9_truncated(my_data, codes)
  tmp_df <- extract_columns_by_names(my_data, c("eid", icd_codes, icd_dates))
  for(a in 0:46) {
    date_col <- paste(icd_dates, "_0_", a, sep="")
    icd_col <- paste(icd_codes, "_0_", a, sep="")
    aux <- tmp_df[ , c("eid",icd_col,date_col)]
    aux <- extract_diagnose_icd9_truncated(aux, codes)
    if(nrow(aux)){
      colnames(aux) <- c("eid","icd9","date_diag")
      df <- rbind(df,aux)
    }
  }
  return(df)
}


#############    ICD10 Functions #######################################

#check for diagnosis exact ICD10 codes 
#Data-Field 41270
extract_diagnose_icd10 <- function(my_data, codes) { 
	diag_icd10 <- my_data[ , grepl("^eid|^diagnoses_icd10_f41270", colnames(my_data))]
  diag_icd10 <- diag_icd10 %>% filter_all(any_vars(. %in% codes))
  data <- (my_data %>% filter(eid %in% diag_icd10$eid))
  return(data)
}

#check for any diagnosis ICD10 codes starting with any string in "codes"
extract_diagnose_icd10_truncated <- function(my_data, codes) {
  diag_icd10 <- my_data[ , grepl("^eid|^diagnoses_icd10_f41270", colnames(my_data))]
  diag_icd10 <- diag_icd10 %>% filter_all(any_vars(str_detect(.,paste(codes,collapse = '|'))))
  data <- (my_data %>% filter(eid %in% diag_icd10$eid))
  return(data)
}

#check for any main diagnosis ICD10 codes starting with any string in "codes"
extract_diagnose_main_icd10 <- function(my_data, codes) {
  diag_icd10 <- my_data[ , grepl("^eid|^diagnoses_main_icd10_f41202", colnames(my_data))]
  diag_icd10 <- diag_icd10 %>% filter_all(any_vars(str_detect(.,paste(codes,collapse = '|'))))
  data <- (my_data %>% filter(eid %in% diag_icd10$eid))
  return(data)
}


#based on the date of first visit to the assesment center
#and the date when the icd10 was diagnosed 
#prints on screen the number of different eids and
#returns a dataframe with the following columns
#eid icd10 date_diag first_visit
count_incidents_icd10 <- function(my_data, codes) {
	icds <- get_date_icd10(my_data, codes)
	vis <- get_date_visits(my_data)
	icds$first_visit <- vis[match(icds$eid, vis$eid), c("first_visit")]
  icds <- icds[icds$date_diag >= icds$first_visit ,]
	print(paste("Incidents samples: ", 
							length(levels(as.factor(icds$eid))),
							sep=""))
	return(icds)
}

#returns incident individuals (and all their traits) from 'my_data'  
#based on the date of first visit to the assessment center
#and the date when the icd10 was diagnosed
extract_incidents_icd10 <- function(my_data, codes) {
	icds <- get_date_icd10(my_data, codes)
  vis <- get_date_visits(my_data)
  icds$first_visit <- vis[match(icds$eid, vis$eid), c("first_visit")]
  icds <- icds[icds$date_diag >= icds$first_visit ,]
  data <- my_data[(my_data$eid %in% icds$eid), ]
	return(data)
}

#returns a table containing the number of individuals per icd code
table_individuals_per_icd10 <- function(my_data, codes, incidents=FALSE) {
	data <- matrix("", ncol = 2, nrow = length(codes)) 
	row <- 1
	df <- extract_diagnose_icd10(my_data, codes)
	icds <- get_date_icd10(df, codes)
  vis <- get_date_visits(df)
  icds$first_visit <- vis[match(icds$eid, vis$eid), c("first_visit")]
	if(incidents)
		icds <- icds[icds$date_diag >= icds$first_visit ,]
	for (c in codes) {
		df_c <- icds[icds$icd10 == c, ]
		data[row,1] <- c 
		data[row,2] <- nrow(df_c)
		row <- row + 1 
	}
	data <- as.data.frame(data)
	colnames(data) <- c("icd10","N_individuals")
	return(data)
}


#Returns a table with the following header
# eid icd10 date
#TODO: Change code to use dplyr
get_date_icd10 <- function(my_data, codes){
  df <- data.frame(eid=character(), 
									 icd10=character(),
									 date=character())
  icd_codes <- "diagnoses_icd10_f41270"
  icd_dates <- "date_of_first_inpatient_diagnosis_icd10_f41280"
  tmp_df <- extract_diagnose_icd10(my_data, codes)
  tmp_df <- extract_columns_by_names(my_data, c("eid", icd_codes, icd_dates))
	for(a in 0:212) {
		date_col <- paste(icd_dates, "_0_", a, sep="")
		icd_col <- paste(icd_codes, "_0_", a, sep="")
		aux <- tmp_df[ , c("eid",icd_col,date_col)]
		aux <- extract_diagnose_icd10(aux, codes)
		if(nrow(aux)){
			colnames(aux) <- c("eid","icd10","date_diag")
			df <- rbind(df,aux)
		}
	}
	return(df)
}


################################# General Functions ####################################

#Extract all selected columns that starts with a given name pattern
#ex: "calcium"
#If we want to extract exact name just use my_data[, colnames]
extract_columns_by_names <- function(my_data, col_names) {
	pattern <- "^eid"
	pattern <- paste(pattern, "|^", paste(col_names, collapse="|^"), sep="")
	data <- my_data[ , grepl(pattern, colnames(my_data))]
	return(data)
}

#Extract all selected columns field number
#ex: "c(30010, 58)"
extract_columns_by_field_number <- function(my_data, field_numbers) {
	print("extracting columns by field number: ")
	pattern <- "^eid"
	field_numbers <- paste("f", field_numbers, "_", sep="")
  pattern <- paste(pattern,"|", paste(field_numbers, collapse="|"), sep="")  
	print(pattern)
	data <- my_data[ , grepl(pattern, colnames(my_data))]
  return(data)
}

#Compute month difference between two dates 
elapsed_months <- function(end_date, start_date) {
    ed <- as.POSIXlt(end_date)
    sd <- as.POSIXlt(start_date)
    12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


#Returns a table witht the following header
# eid v0 v1 v2 v3 number_of_visits first_visit last_visit 
#Notice that v0 and first visit may not always the same
get_date_visits <- function(my_data){
	df <- my_data[ , grepl("^eid|^date_of_attending_assessment", colnames(my_data))]
	df$number_of_visits <- rowSums(!is.na(df[,2:5]))
	df$first_visit <- apply(df[,2:5],1,min, na.rm=TRUE)
	df$last_visit <- apply(df[,2:5],1,max, na.rm=TRUE)
	colnames(df) <- c("eid", "v0","v1","v2","v3", "number_of_visits", "first_visit", "last_visit")
	return(df)
}

#density graph
#input:
#my_data: dataframe
#X : column name to be used as factor in the boxplot
#Y : column name to create the boxplot
#returns a ggplot
create_density_plot <- function(my_data, X, Y) {
  d <- my_data[, c(X,Y)]
  d <- d[complete.cases(d), ]
  d[,X] <- as.factor(d[,X])
	d$newX <- ave(d[,c(Y)], d[,c(X)], FUN=function(x)paste0("n=",length(x)))
	d$newX <- paste0(d[,c(X)], " (", d$newX, ")")
	#print(table(d[,X]))
  #only consider non na's values
  n_ind <- paste("Number of samples considered: ",nrow(d),sep='')
  g <- ggplot(d, aes(x=d[,c(Y)], fill=newX))
  g <- g + geom_density(alpha=.3) 
  g <- g + scale_color_brewer(palette="Dark2")
  g <- g + labs(title=n_ind, y=Y, fill=X) 
  g <- g + theme(axis.text.x=element_text(size=10),
                 axis.text.y=element_text(size=10, hjust=1),
                 axis.title.x=element_blank(),  #element_text(size=10),
                 axis.title.y=element_text(size=8, angle=90),
                 title=element_text(size=10),
                 legend.text=element_text(size=7),
                 legend.title=element_text(size=7, hjust=0),
                 #legend.key.size=unit(2, "lines"),
                 legend.background=element_rect(colour=0, fill=0),
                 #legend.key=element_blank(),
                 legend.position = c(0.8, 0.8) # , "none"  #to hide legend
                 )
	return(g)
}


#histogram graph
#input:
#my_data: dataframe
#X : column name to be used as factor in the boxplot
#Y : column name to create the boxplot
#returns a ggplot
create_histogram_plot <- function(my_data, X, Y) {
  d <- my_data[, c(X,Y)]
  d <- d[complete.cases(d), ]
  d[,X] <- as.factor(d[,X])
  d$newX <- ave(d[,c(Y)], d[,c(X)], FUN=function(x)paste0("n=",length(x)))
  d$newX <- paste0(d[,c(X)], " (", d$newX, ")")
  #print(table(d[,X]))
  #only consider non na's values
  n_ind <- paste("Number of samples considered: ",nrow(d),sep='')
  g <- ggplot(d, aes(x=d[,c(Y)], fill=newX))
  g <- g + scale_color_brewer(palette="Dark2")
  g <- g + labs(title=n_ind, y=Y, fill=X)
  g <- g + theme(axis.text.x=element_text(size=10),
                 axis.text.y=element_text(size=10, hjust=1),
                 axis.title.x=element_blank(),  #element_text(size=10),
                 axis.title.y=element_text(size=8, angle=90),
                 title=element_text(size=10),
                 legend.text=element_text(size=7),
                 legend.title=element_text(size=7, hjust=0),
                 #legend.key.size=unit(2, "lines"),
                 legend.background=element_rect(colour=0, fill=0),
                 #legend.key=element_blank(),
                 legend.position = c(0.8, 0.8) # , "none"  #to hide legend
                 )
  return(g)
}

