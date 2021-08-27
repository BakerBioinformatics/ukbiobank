
#All functions related with the single tables of the UKB


#global tables names

death <- NULL
death_cause <- NULL
hesin <- NULL
hesin_critical <- NULL
hesin_delivery <- NULL
hesin_diag <- NULL
hesin_maternity <- NULL
hesin_oper <- NULL
hesin_psych <- NULL


#### Loading Table ####

load_table <- function(dir, tname, fdate=NULL) {
	location <- dir
	if(is.null(fdate))   #get last version
		location <- Sys.glob(file.path(paste0(location, tname, "_2*")))
	else {
		l1 <- Sys.glob(file.path(paste0(location, tname, "_", fdate, ".txt")))
		if(length(l1) == 0) 
			l1 <- Sys.glob(file.path(paste0(location, "Archive/", tname, "_", fdate, ".txt"))) 
		location <- l1
	}
	#check of file exists 
	if(length(location) == 0) { # should be always 1
		if(is.null(fdate))
			print(paste0("Error: Table ", tname, " does not exists"))
		else
			print(paste0("Error: Table ", tname, "_", fdate, " does not exists"))
		return(NULL)
	}
	f <- fread(location, header=TRUE)
	print(paste0("Loaded table ", location))
	return(f)
}

# Loading tables
#if no date is provided, last version is uploaded
load_hesin_tables <- function(date_hesin=NULL, load_hesin=TRUE, 
															date_crit=NULL, load_crit=FALSE,
															date_deli=NULL, load_deli=FALSE,
															date_diag=NULL, load_diag=FALSE, 
															date_mate=NULL, load_mate=FALSE, 
															date_oper=NULL, load_oper=FALSE,
															date_psyc=NULL, load_psyc=FALSE) {
	tables_loc <- "/baker/datasets/ukb55469/Tables_coding_info/Hes/"
	if(load_hesin) {
		hesin <- load_table(tables_loc, "hesin", date_hesin)
		assign('hesin', hesin, envir=.GlobalEnv)
	}

	if(load_crit) {
    hesin_critical <- load_table(tables_loc, "hesin_critical", date_crit)
    assign('hesin_critical', hesin_critical, envir=.GlobalEnv)
  }
	if(load_deli) {
    hesin_delivery <- load_table(tables_loc, "hesin_delivery", date_deli)
    assign('hesin_delivery', hesin_delivery, envir=.GlobalEnv)
  }
	if(load_diag) {
    hesin_diag <- load_table(tables_loc, "hesin_diag", date_diag)
    assign('hesin_diag', hesin_diag, envir=.GlobalEnv)
  }
	if(load_mate) {
    hesin_maternity <- load_table(tables_loc, "hesin_maternity", date_mate)
    assign('hesin_maternity', hesin_maternity, envir=.GlobalEnv)
  }
	if(load_oper) {
    hesin_oper <- load_table(tables_loc, "hesin_oper", date_oper)
    assign('hesin_oper', hesin_oper, envir=.GlobalEnv)
  }
	if(load_psyc) {
    hesin_psych <- load_table(tables_loc, "hesin_psych", date_psyc)
    assign('hesin_psych', hesin_psych, envir=.GlobalEnv)
	}
}

load_death_tables <- function(date_death=NULL) {
	death <- load_table("/baker/datasets/ukb55469/Tables_coding_info/Death_register/", "death", date_death)
	assign('death', death, envir=.GlobalEnv)
	death_cause <- load_table("/baker/datasets/ukb55469/Tables_coding_info/Death_register/", "death_cause", date_death)
  assign('death_cause', death_cause, envir=.GlobalEnv)
}


#### Functions ####


#We will assume that death, death_cause, hesin and hesin_diag tables have been loaded
#Condition: For each disease, at least one ICD-10 code listed as either:
# -primary diagnosis
# -secondary diagnosis
# -death cause
compute_cases_icd10 <- function(data, codes, truncated=FALSE) {
	#Death
  if(truncated) {
    cases_death <- (death_cause[death_cause$cause_icd10 %like%
                    paste(codes, collapse = '|'), ])[ ,c('eid','cause_icd10')]
  }
  else 
    cases_death <- (death_cause[death_cause$cause_icd10 %in% codes, ])[ ,c('eid','cause_icd10')]
  cases_death <- merge(cases_death, death[, c('eid','date_of_death')], 
											 by.x='eid', by.y='eid', all.y=FALSE)
  cases_death$code_type <- 'ICD10'
  cases_death$source <- 'Death'
  names(cases_death)[names(cases_death) == 'cause_icd10'] <- 'code'
  names(cases_death)[names(cases_death) == 'date_of_death'] <- 'date'
  cases_death <- cases_death[cases_death$eid %in% data$eid, ]
  #HES
  if(truncated) {
    cases_hes <- hesin_diag[(hesin_diag$diag_icd10 %like% paste(codes, collapse = '|')) &
                            (hesin_diag$level  %in% c('1','2')), ]
  }
  else {
    cases_hes <- hesin_diag[(hesin_diag$diag_icd10 %in% codes) &
                            (hesin_diag$level  %in% c('1','2')), ]
  }
  cases_hes <- cases_hes[cases_hes$eid %in% data$eid, c('eid','ins_index','diag_icd10')]
  ##get dates
  cases_hes <- merge(cases_hes, hesin[ , c('eid', 'ins_index', 'epistart','admidate','epiend','disdate')],
                     by.x=c('eid','ins_index'),
                     by.y=c('eid','ins_index'),
                     all.y=FALSE)
	#the date of diagnosis would be one of the listed dates (ordered by priority)
  cases_hes$date <- ifelse(!is.na(cases_hes$epistart) & cases_hes$epistart != "", cases_hes$epistart,
                           ifelse(!is.na(cases_hes$admidate) & cases_hes$admidate != "", cases_hes$admidate,
                                  ifelse(!is.na(cases_hes$epiend) & cases_hes$epiend != "", cases_hes$epiend,
                                         cases_hes$disdate)))
  cases_hes <- cases_hes[,c('eid', 'diag_icd10', 'date')]
  names(cases_hes)[names(cases_hes) == 'diag_icd10'] <- 'code'
  cases_hes$code_type <- 'ICD10'
  cases_hes$source <- 'HES'
  #adding all together
  cases <- rbind(cases_death, cases_hes)
  #change to Date format
  cases <- cases %>%
    mutate(date_diag = as.numeric(format(as.POSIXct(as.character(date),
                                                    format="%d/%m/%Y"),
                                         format='%Y%m%d'))) %>% arrange(eid, date_diag)
  cases <- cases %>% distinct()
  return(cases)
}


#At least one OPCS4 code listed as either:
# -main procedure
# -secondary procedure
#we assumed that data have the hesin_oper table have been loaded
compute_cases_opcs4 <- function(data, codes, truncated=FALSE) {
  #OPCS4
  if(truncated) {
    cases_op <- hesin_oper[(hesin_oper$oper4 %like% paste(codes, collapse = '|')) &
                           (hesin_oper$level  %in% c('1','2')), ]
  }
  else {
    cases_op <- hesin_oper[(hesin_oper$oper4 %in% codes) &
                           (hesin_oper$level  %in% c('1','2')), ]
  }
  cases_op <- cases_op[cases_op$eid %in% data$eid, c('eid','ins_index','oper4')]
  ##get dates from hesin (becasue opdate in hesin_oper is inconsistent)
  cases_op <- merge(cases_op, hesin[ , c('eid', 'ins_index', 'epistart','admidate','epiend','disdate')],
                     by.x=c('eid','ins_index'),
                     by.y=c('eid','ins_index'),
                     all.y=FALSE)
  #the date of diagnosis would be one of the listed dates (ordered by priority)
  cases_op$date <- ifelse(!is.na(cases_op$epistart) & cases_op$epistart != "", cases_op$epistart,
                           ifelse(!is.na(cases_op$admidate) & cases_op$admidate != "", cases_op$admidate,
                                  ifelse(!is.na(cases_op$epiend) & cases_op$epiend != "", cases_op$epiend,
                                         cases_op$disdate)))
  cases_op <- cases_op[,c('eid', 'oper4', 'date')]
  names(cases_op)[names(cases_op) == 'oper4'] <- 'code'
  cases_op$code_type <- 'OPCS4'
  cases_op$source <- 'OPER'

  cases_op <- cases_op %>%
    mutate(date_diag = as.numeric(format(as.POSIXct(as.character(date),
                                                    format="%d/%m/%Y"),
                                         format='%Y%m%d'))) %>% arrange(eid, date_diag)
  cases_op <- cases_op %>% distinct()
  return(cases_op)
}

#Get incidents based on first diagnosis and first visit date
#using cases generated with the function above
#and data must have date_of_attending_assessment_centre_f53_0_0
get_incident <- function(data, cases_data) {
  #Get information first diagnose
  cases_in <- cases_data %>%
    group_by(eid)  %>%
    filter(row_number()==1) %>%
    ungroup()
  visit <- data[ , c('eid', 'date_of_attending_assessment_centre_f53_0_0')]
  visit <- visit %>%
    mutate(date_visit = as.numeric(format(as.POSIXct(as.character(date_of_attending_assessment_centre_f53_0_0),
                                               format="%Y-%m-%d"),
                                    format='%Y%m%d')))
  cases_in <- left_join(cases_in, visit, by = c("eid"="eid"))
  cases_in <- cases_in[(cases_in$date_diag - cases_in$date_visit) > 0, ]
  return(cases_in)
}



##############################
#based on https://biobank.ndph.ox.ac.uk/showcase/showcase/docs/first_occurrences_outcomes.pdf
#############################
three_character_icd_cases <- function(data, code, only_first=FALSE){
  gp_date <- '190907'
  gp_clinical <- fread(paste0("/baker/datasets/ukb55469/Tables_coding_info/GP/gp_clinical_",
                             gp_date, ".txt"), header=TRUE)
  #Cases from Death records
  cases_death <- (death_cause[death_cause$cause_icd10 %like% code, ])[ ,c('eid','cause_icd10')]
  cases_death <- merge(cases_death, death, by.x='eid', by.y='eid', all.y=FALSE)
  cases_death$code_type <- 'ICD10'
  cases_death$source <- 'Death'
  names(cases_death)[names(cases_death) == 'cause_icd10'] <- 'code'
  names(cases_death)[names(cases_death) == 'date_of_death'] <- 'date'
  cases_death <- cases_death[cases_death$eid %in% data$eid, ]
  cases_death <- cases_death %>% mutate(date_2 = as.numeric(format(as.POSIXct(as.character(date),
                                                                              format="%d/%m/%Y"),
                                                                   format='%Y%m%d'))) %>% arrange(eid, date_2)
  ##remove death cases < 2006
  cases_death <- cases_death[cases_death$date_2 > 20060000, ]

	 #Cases from Primary Care redords (GP)
  r2_codes <- fread("/baker/datasets/ukb55469/Tables_coding_info/Coding/coding1834.tsv", header=TRUE)
  r3_codes <- fread("/baker/datasets/ukb55469/Tables_coding_info/Coding/coding1835.tsv", header=TRUE)
  r2_codes <- r2_codes[r2_codes$meaning == code, ]
  r3_codes <- r3_codes[r3_codes$meaning == code, ]
  gp_clinical <- gp_clinical[gp_clinical$eid %in% data$eid, ]
  names(gp_clinical)[names(gp_clinical) == 'event_dt'] <- 'date'
  cases_gp <- gp_clinical[(gp_clinical$read_2 %in% r2_codes$coding), ]
  cases_gp <- cases_gp[ , c('eid','date','read_2')]
  names(cases_gp)[names(cases_gp) == 'read_2'] <- 'code'
  cases_gp$code_type <- 'read_2'
  cases_gp3 <- gp_clinical[(gp_clinical$read_3 %in% r3_codes$coding), ]
  cases_gp3 <- cases_gp3[ , c('eid','date','read_3')]
  names(cases_gp3)[names(cases_gp3) == 'read_3'] <- 'code'
  cases_gp3$code_type <- 'read_3'
  cases_gp <- rbind(cases_gp, cases_gp3)
  cases_gp$source <- 'GP'
  cases_gp <- cases_gp %>%
    mutate(date_2 = as.numeric(format(as.POSIXct(as.character(date),
                                                 format="%d/%m/%Y"),
                                         format='%Y%m%d'))) %>% arrange(eid, date_2)
  #cases to remove
  #remove special dates:
  #01/01/1900 <- missing date
  #01/01/1901 <- date before dob participant
  #02/02/1902 <- date matched dob participant
  #03/03/1903 <- date matched yob
  cases_gp <- cases_gp[cases_gp$date_2 > 19030303, ]
  #07/07/2037 <- date was wrongly entered in the future
  cases_gp <- cases_gp[cases_gp$date_2 < 20370000, ]

	#Cases from Hospital Inpatient records (HES)
  ##ICD-10
  cases_hes <- hesin_diag[(hesin_diag$diag_icd10 %like% code) & (hesin_diag$level  %in% c('1','2')), ]
  cases_hes <- cases_hes[cases_hes$eid %in% data$eid, c('eid','ins_index','diag_icd10')]
  ##get dates
  cases_hes <- merge(cases_hes, hesin, by.x=c('eid','ins_index'), by.y=c('eid','ins_index'), all.y=FALSE)
  ##the date of diagnosis would be one of the listed dates (ordered by priority)
  cases_hes$date <- ifelse(!is.na(cases_hes$epistart) & cases_hes$epistart != "", cases_hes$epistart,
                           ifelse(!is.na(cases_hes$admidate) & cases_hes$admidate != "", cases_hes$admidate,
                                  ifelse(!is.na(cases_hes$epiend) & cases_hes$epiend != "", cases_hes$epiend,
                                         cases_hes$disdate)))
  cases_hes <- cases_hes[,c('eid', 'diag_icd10', 'date')]
  names(cases_hes)[names(cases_hes) == 'diag_icd10'] <- 'code'
  cases_hes$code_type <- 'ICD10'
  ##ICD-9
  icd9_codes <- fread("/baker/datasets/ukb55469/Tables_coding_info/Coding/coding1836.tsv", header=TRUE)
  icd9_codes <- icd9_codes[icd9_codes$meaning == code, ]
  cases_hes2 <- hesin_diag[(hesin_diag$diag_icd9 %in% icd9_codes$coding) & (hesin_diag$level  %in% c('1','2')), ]
  cases_hes2 <- cases_hes2[cases_hes2$eid %in% data$eid, c('eid','ins_index','diag_icd9')]
  ##get dates
  cases_hes2 <- merge(cases_hes2, hesin, by.x=c('eid','ins_index'), by.y=c('eid','ins_index'), all.y=FALSE)
  ##the date of diagnosis would be one of the listed dates (ordered by priority)
  cases_hes2$date <- ifelse(!is.na(cases_hes2$epistart) & cases_hes2$epistart != "", cases_hes2$epistart,
                            ifelse(!is.na(cases_hes2$admidate) & cases_hes2$admidate != "", cases_hes2$admidate,
                                   ifelse(!is.na(cases_hes2$epiend) & cases_hes2$epiend != "", cases_hes2$epiend,
                                          cases_hes2$disdate)))
  cases_hes2 <- cases_hes2[,c('eid', 'diag_icd9', 'date')]
  names(cases_hes2)[names(cases_hes2) == 'diag_icd9'] <- 'code'
  cases_hes2$code_type <- 'ICD9'
  cases_hes <- rbind(cases_hes, cases_hes2)
  cases_hes$source <- 'HES'
  cases_hes <- cases_hes %>%
    mutate(date_2 = as.numeric(format(as.POSIXct(as.character(date),
                                                    format="%d/%m/%Y"),
                                         format='%Y%m%d'))) %>% arrange(eid, date_2)

	#Cases from Self Reported records (SR)
  sr_codes <- fread("/baker/datasets/ukb55469/Tables_coding_info/Coding/coding609.tsv", header=TRUE)
  sr_codes <- sr_codes[sr_codes$meaning == code, ]
  sr_codes <- sr_codes$coding
  cases_sr <- extract_self_reported_nc(data, sr_codes)
  df <- data.frame(eid=character(),
                   code=character(),
                   date=character())
  sr_field <- "noncancer_illness_code_selfreported_f20002"
  sr_dates <- "interpolated_year_when_noncancer_illness_first_diagnosed_f20008"
  for(v in 0:3) { #visits for 0 to 3
    date_col <- paste0(sr_dates, "_", v) #, "_0")
    for(a in 0:33) { #maximum number of sr fields per visit
      v_col <- paste0(sr_field, "_", v, "_", a)
      d_col <- paste0(sr_dates, "_", v, "_" ,a)
      aux <- cases_sr[ , c("eid", v_col, d_col)]
      aux <- aux[aux[[v_col]] %in% sr_codes, ]
      if(nrow(aux)){
        colnames(aux) <- c("eid","code","date")
        df <- rbind(df,aux)
      }
    }
  }
  cases_sr <- df
  cases_sr$code_type <- 'SR-code'
  cases_sr$source <- 'Self-Reported'
  #drop cases with date == -1, -3 or <1930
  cases_sr <- cases_sr[!is.na(cases_sr$date) &
                       !(cases_sr$date == "" |
                         cases_sr$date == -1 | cases_sr$date == -3| cases_sr$date < 1930), ]

	#transform date to date format (done by rcanovas)
  gen_sr_date <- function(x) {
    d <- "01/"
    ym <- unlist(strsplit(as.character(x), "\\."))  #must be of size 1 or 2
    if(length(ym) == 1)
      d <- paste0(d,"06/",ym[1]) #maybe cahnge to 06
    else {
      m <- round(as.numeric(ym[2]) * 12 / 9, 0)
      if(m < 10)
        d <- paste0(d,"0")
      d <- paste0(d,m,"/",ym[1])
    }
    return(d)
  }
  cases_sr$date <- lapply(cases_sr$date, gen_sr_date)
  cases_sr <- cases_sr %>%
    mutate(date_2 = as.numeric(format(as.POSIXct(as.character(date),
                                                    format="%d/%m/%Y"),
                                         format='%Y%m%d'))) %>% arrange(eid, date_2)

  #adding all together
  cases <- rbind(cases_death, cases_hes, cases_sr, cases_gp)
  cases <- cases %>% distinct()
  if(only_first){
    cases <- cases %>%
      group_by(eid)  %>%
      filter(row_number()==1) %>%
      ungroup()
  }
  return(cases)
}

