clean_f16 <- function(lab_data){
  # install.packages("ggplot2")
  # install.packages("tidyverse")
  # install.packages("lubridate")
  # install.packages("dplyr")
  # install.packages("tidyr")
  # install.packages("stringr")
  # install.packages("stringi")
  
  
  #---------------------------------------------------------------------------------------------
  #Download libraries and set working directory
  #---------------------------------------------------------------------------------------------
  library(ggplot2)
  library(tidyverse)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  

  #---------------------------------------------------------------------------------------------
  #Read in data and  rename columns
  #---------------------------------------------------------------------------------------------
  
  names(lab_data) <- c("id","name","uni", "date", "fellow", "school", "otherSchool",
                       "major", "affiliation", "year", "program", "programText",
                       "remove1", "remove2", "visitBefore", "workshop", "hearAbout",
                       "comments", "timeIn", "timeOut", "course", "problem",
                       "problemText", "notes")
  
  drops <- c("remove1","remove2")
  lab_data <- lab_data[, !(names(lab_data) %in% drops)]
  
  #Add semester column
  lab_data$semester <- "Fall 2016"
  
  #---------------------------------------------------------------------------------------------
  #MAJOR CLEANING
  #use summary(lab_data$major) to see all majors and add relevant majors to below lists
  #---------------------------------------------------------------------------------------------
  
  anthro_list <- c("anthropology ")
  arch_list <- c("arch", "architecture", "architecture ")
  bio_list <- c("bio", "biology", "biology ")
  biochem_list <- c("biochem")
  chem_list <- c("chem", "chemistry", "Chem")
  clit_list <- c("Comp. Lit", "Comp Lit")
  cs_list <- c("CS")
  econ_list <- c("econ", "Econ", "economics", "economics ", "econ/math", "Economics-Math", "political econ")
  eng_list <- c("English major","english")
  env_list <- c("Envi Sci")
  hist_list <- c("Hist", "history") 
  math_list <- c("Math", "math")
  neuro_list <- c("neuroscience", "nueroscience", "neuro", "Neuro", "Neuroscience & Behavior", "Neuroscience ")
  psci_list <- c("poli sci", "Poli Sci", "poli sci ", "pli sci", "PoliSci")
  psych_list <- c("psych","Psych","psychology")
  soc_list <- c("sociology")
  sd_list <- c("SD", "SDev", "Sus Dev", "SUSDEV")
  thtr_list <- c("Drama")
  und_list <- c("undecided", "undeclared", "Undecided")
  urb_list <- c("urban studies", "urban studies ", "Urbs")
  na_list <- c("N/A", "gen chem 1", "Gen Chem", "gen chem", "gen chem ", "GS", "", 
               "Auditor and life long learner", "phd ", "professional study ")
  
  lab_data$major[lab_data$major %in% anthro_list] <- "Anthropology"
  lab_data$major[lab_data$major %in% arch_list] <- "Architecture"
  lab_data$major[lab_data$major %in% biochem_list] <- "Biochemistry"
  lab_data$major[lab_data$major %in% bio_list] <- "Biology"
  lab_data$major[lab_data$major %in% chem_list] <- "Chemistry"
  lab_data$major[lab_data$major %in% clit_list] <- "Comp Lit"
  lab_data$major[lab_data$major %in% cs_list] <- "Computer Science"
  lab_data$major[lab_data$major %in% econ_list] <- "Economics"
  lab_data$major[lab_data$major %in% eng_list] <- "English"
  lab_data$major[lab_data$major %in% env_list] <- "Environmental Science"
  lab_data$major[lab_data$major %in% hist_list] <- "History"
  lab_data$major[lab_data$major %in% math_list] <- "Mathematics"
  lab_data$major[lab_data$major %in% neuro_list] <- "Neuroscience"
  lab_data$major[lab_data$major %in% psci_list] <- "Political Science"
  lab_data$major[lab_data$major %in% psych_list] <- "Psychology"
  lab_data$major[lab_data$major %in% soc_list] <- "Sociology"
  lab_data$major[lab_data$major %in% sd_list] <- "SD"
  lab_data$major[lab_data$major %in% thtr_list] <- "Theater"
  lab_data$major[lab_data$major %in% und_list] <- "Undeclared"
  lab_data$major[lab_data$major %in% urb_list] <- "Urban Studies"
  lab_data$major[lab_data$major %in% na_list] <- NA
  
  lab_data$major <- factor(lab_data$major)
  # summary(lab_data$major)
  
  
  
  #---------------------------------------------------------------------------------------------
  #YEAR CLEANING
  #NOTE: This cleaning is only relevant for the 2016-2017 data 
  #---------------------------------------------------------------------------------------------
  
  lab_data$year <- as.character(lab_data$year)
  na_yr <- c("1", "n/a", "")
  lab_data$year[lab_data$year %in% na_yr] <- NA
  lab_data$year[with(lab_data, year=="2017" | year=="2107" | year=="SR" | year=="senior ")] <- "Fourth Year"
  lab_data$year[with(lab_data, year=="2018")] <- "Third Year"
  lab_data$year[with(lab_data, year=="2019" | year=="Soph")] <- "Second Year"
  lab_data$year[with(lab_data, year=="2020" | year=="202")] <- "First Year"
  lab_data$year[with(lab_data, year=="2016" | year=="2021")] <- "Other"
  
  lab_data$year <- factor(lab_data$year)
  # summary(lab_data$year)
  
  
  #---------------------------------------------------------------------------------------------
  #SCHOOL CLEANING
  #---------------------------------------------------------------------------------------------
  
  lab_data$school[lab_data$school==""] <- NA
  schools <- c("BC", "CC/SEAS/GS")
  lab_data$school[!lab_data$school %in% schools] <- "Other"
  
  lab_data$school <- factor(lab_data$school)
  # summary(lab_data$school)
  
  
  
  #---------------------------------------------------------------------------------------------
  #VISITBEFORE CLEANING
  #---------------------------------------------------------------------------------------------
  lab_data$visitBefore[lab_data$visitBefore==""] <- NA
  lab_data$visitBefore <- factor(lab_data$visitBefore)
  summary(lab_data$visitBefore)
  
  
  
  #---------------------------------------------------------------------------------------------
  #WORKSHOP CLEANING
  #---------------------------------------------------------------------------------------------
  lab_data$workshop[lab_data$workshop==""] <- NA
  lab_data$workshop <- factor(lab_data$workshop)
  summary(lab_data$workshop)
  
  
  
  #---------------------------------------------------------------------------------------------
  #MAJOR CAT
  #Group majors into categories: Soical Sciences, STEM, Humanities, Other, Undeclared
  #---------------------------------------------------------------------------------------------
  
  lab_data$majorCat <- NULL
  lab_data$majorCat[lab_data$major=="Economics" | lab_data$major=="Political Science"  
                    | lab_data$major=="Sociology" | lab_data$major=="Psychology"  
                    | lab_data$major=="Urban Studies" | lab_data$major=="Sociology and Stats Minor" 
                    | lab_data$major=="Anthropology" | lab_data$major=="Psychology + Economics" 
                    | lab_data$major=="Economics/Political Science"| lab_data$major=="SIPA" 
                    | lab_data$major=="Political Science and Sociology" | lab_data$major=="QMSS"
                    | lab_data$major=="poli sci and econ " | lab_data$major=="soci/busi" 
                    | lab_data$major=="sociology/business" | lab_data$major=="econ / music" 
                    | lab_data$major=="econ & history " | lab_data$major=="econ/music" 
                    | lab_data$major=="Economics and History" | lab_data$major=="Economics, Music"
                    | lab_data$major=="psychology & WGSS"| lab_data$major=="Political Science/Human Rights"
                    | lab_data$major=="Political Science and Jewish Studies"] <- "Social Sciences"
  
  lab_data$majorCat[lab_data$major=="SD" | lab_data$major=="Biology" 
                    | lab_data$major=="Chemistry" | lab_data$major=="Computer Science"
                    | lab_data$major=="Mathematics" | lab_data$major=="Biochemistry" 
                    | lab_data$major=="Neuroscience" | lab_data$major=="Statistics" 
                    | lab_data$major=="Physics" | lab_data$major=="engineering " 
                    | lab_data$major=="Neuroscience " | lab_data$major=="Environmental Science" 
                    | lab_data$major=="Neuroscience & Behavior" | lab_data$major=="Industrial Engineering"
                    | lab_data$major=="E3B" | lab_data$major=="Math Stats and Psych"
                    | lab_data$major=="Sus Dev and Business Management"] <- "STEM"
  
  lab_data$majorCat[lab_data$major=="History" | lab_data$major=="English" 
                    | lab_data$major=="Art History" | lab_data$major=="Theater" 
                    | lab_data$major=="Human Rights" | lab_data$major=="Religion" 
                    | lab_data$major=="Film" | lab_data$major=="Classical Studies" 
                    | lab_data$major=="Architecture" 
                    | lab_data$major=="Archaeology" | lab_data$major=="Afam and sociology"
                    | lab_data$major=="Afam, Sociology" | lab_data$major=="African American Studies, Sociology"
                    | lab_data$major=="archaeology" | lab_data$major=="Dance + Comp Lit"
                    | lab_data$major=="East Asian Studies"| lab_data$major=="South Asian Track"
                    | lab_data$major=="American Studies and Sociology" | lab_data$major=="Dance and Economics"
                    | lab_data$major=="history, economics"| lab_data$major=="History, economics " 
                    | lab_data$major=="English/Pre Med" | lab_data$major=="WGSS and Political Science"] <- "Humanities"
  
  lab_data$majorCat[lab_data$major=="phd " | lab_data$major=="ERSA Ed Policy Masters"
                    | lab_data$major=="professional study "] <- "Other"
  
  lab_data$majorCat[lab_data$major=="Undeclared"] <- NA
  
  # lab_data$majorCat[lab_data$majorCat=="General Studies"] <- NA
  
  
  lab_data$majorCat <- factor(lab_data$majorCat, levels = c("Social Sciences", "STEM", "Humanities", "Other"))
  summary(lab_data$majorCat)
  
  #For double majors, group second major
  lab_data$majorCat2 <- NULL
  
  lab_data$majorCat2[lab_data$major=="American Studies and Sociology" | lab_data$major=="Dance and Economics" 
                     | lab_data$major=="history, economics" | lab_data$major=="History, economics " 
                     | lab_data$major=="Math Stats and Psych" | lab_data$major=="Political Science/Human Rights" 
                     | lab_data$major=="WGSS and Political Science" 
                     | lab_data$major=="Sus Dev and Business Management"] <- "Social Sciences"
  
  lab_data$majorCat2[lab_data$major=="econ / music" | lab_data$major=="econ & history "
                     | lab_data$major=="econ/music" | lab_data$major=="Economics and History" 
                     | lab_data$major=="Economics, Music" | lab_data$major=="psychology & WGSS"
                     | lab_data$major=="Political Science/Human Rights" 
                     | lab_data$major=="Political Science and Jewish Studies"] <- "Humanities"
  
  lab_data$majorCat2[lab_data$major=="English/Pre Med"] <- "STEM"
  
  
  
  # -------------------------------------------------------------------------
  # Program Cleaning and Analysis 
  # -------------------------------------------------------------------------
  
  lab_data$program[(lab_data$program == "")] <- NA
  lab_data$program <- toupper(lab_data$program)
  #remove white sapce
  lab_data$program <- gsub(" ", "", lab_data$program, fixed = TRUE)
  
  #split program into 3 columns
  lab_data <- lab_data %>%
    separate(program, c("program1", "program2", "program3"), ",")
  
  i <- which(lab_data$program1 == "EXCEL AND ARCGIS")
  lab_data[i, "program1"] <- "EXCEL"
  lab_data[i, "program2"] <- "ARCGIS"
  
  GIS <- c("ARCGIS", "GIS", "QGIS")
  lab_data$program1[lab_data$program1 %in% GIS] <- "GIS"
  
  programs <- c("EXCEL", "STATA", "SPSS", "R", "GIS", "LATEX", "MATLAB",
                "PYTHON", "OTHER")
  lab_data$program1[!lab_data$program1 %in% programs & !is.na(lab_data$program1)] <- "OTHER"
  lab_data$program2[!lab_data$program2 %in% programs & !is.na(lab_data$program2)] <- "OTHER"
  lab_data$program3[!lab_data$program3 %in% programs & !is.na(lab_data$program3)] <- "OTHER"
  lab_data$program1 <- factor(lab_data$program1, programs)
  lab_data$program2 <- factor(lab_data$program2, programs)
  lab_data$program3 <- factor(lab_data$program3, programs)
  
  # -------------------------------------------------------------------------
  # Problem Cleaning and Analysis 
  # -------------------------------------------------------------------------
  lab_data$problem[lab_data$problem == ""] <- NA
  lab_data$problem <- factor(lab_data$problem)
  
  #Add number of problems column to data frame
  n_problems <- sapply(gregexpr(",", lab_data$problem), length) + 1
  one_prob <- sapply(gregexpr(",", lab_data$problem), function(l) l[[1]])
  n_problems[which(one_prob == -1)] <- 1
  n_problems[which(is.na(lab_data$problem))] <- NA
  lab_data <- cbind(lab_data, n_problems)
  
  #---------------------------------------------------------------------------------------------
  #Clean dates and day of week
  #only works for 2016...fix to generalize to all years
  #---------------------------------------------------------------------------------------------
  betterDates <- as.Date(lab_data$date, format = "%m/%d/%y")
  lab_data$betterDates <- format(betterDates, "2016-%m-%d")
  lab_data$dow <- wday(betterDates)
  lab_data$dow <- lab_data$dow + 2
  
  lab_data$dow[lab_data$dow == 8] <- "Sunday"
  lab_data$dow[lab_data$dow == 9] <- "Monday"
  lab_data$dow[lab_data$dow == 3] <- "Tuesday"
  lab_data$dow[lab_data$dow == 4] <- "Wednesday"
  lab_data$dow[lab_data$dow == 5] <- "Thursday"
  lab_data$dow[lab_data$dow == 6] <- "Friday"
  lab_data$dow[lab_data$dow == 7] <- "Saturday"
  
  lab_data$dow <- factor(lab_data$dow, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered=TRUE)
  
  
  
  # -------------------------------------------------------------------------
  # Clean and Format timeIn and timeOut
  # -------------------------------------------------------------------------
  
  #Format Times
  lab_data$timeIn[lab_data$timeIn == ""] <- NA
  lab_data$timeOut[lab_data$timeOut == ""] <- NA
  lab_data$timeIn <- as.character(lab_data$timeIn)
  lab_data$timeOut <- as.character(lab_data$timeOut)
  
  # i <- regexpr('[0-9]{1,2}', lab_data$timeIn)
  # lab_data[which(i==1),]$timeIn <- paste0(regmatches(lab_data$timeIn, i), ":00")
  
  i <- which(nchar(lab_data$timeIn) == 1 | nchar(lab_data$timeIn) == 2 )
  lab_data$timeIn[i] <- paste0(lab_data$timeIn[i], ":00")
  
  j <- regexpr('[0-9]{3}', lab_data$timeIn)
  lab_data[which(j==1),]$timeIn <- paste0('0', regmatches(lab_data$timeIn, j))
  
  # k <- regexpr('[0-9]{4}', lab_data$timeIn)
  # lab_data[which(k==1),]$timeIn <- regmatches(lab_data$timeIn, k)
  lab_data$timeIn <- gsub('([0-9]{2})([0-9]{2})', '\\1:\\2', lab_data$timeIn)
  
  lab_data$timeIn <- strptime(lab_data$timeIn, format='%H:%M')
  #lab_data$timeIn <- strftime(lab_data$timeIn, '%H:%M')
  
  
  
  #Repeat same process for time out
  i <- which(nchar(lab_data$timeOut) == 1 | nchar(lab_data$timeOut) == 2 )
  lab_data$timeOut[i] <- paste0(lab_data$timeOut[i], ":00")
  
  j <- regexpr('[0-9]{3}', lab_data$timeOut)
  lab_data[which(j==1),]$timeOut <- paste0('0', regmatches(lab_data$timeOut, j))
  
  # k <- regexpr('[0-9]{4}', lab_data$timeIn)
  # lab_data[which(k==1),]$timeIn <- regmatches(lab_data$timeIn, k)
  lab_data$timeOut <- gsub('([0-9]{2})([0-9]{2})', '\\1:\\2', lab_data$timeOut)
  
  lab_data$timeOut <- strptime(lab_data$timeOut, format='%H:%M')
  #lab_data$timeOut <- strftime(lab_data$timeOut, '%H:%M')
  
  
  #If time is between 1 and 10, add 12 hours to convert to military time
  cutoff1 <- strptime("10:00", format='%H:%M')
  cutoff2 <- strptime("1:00", format='%H:%M')
  
  idx <- which(lab_data$timeIn < cutoff1 & lab_data$timeIn >= cutoff2)
  lab_data$timeIn[idx] <- lab_data$timeIn[idx] + 12*60*60
  
  idx <- which(lab_data$timeOut < cutoff1 & lab_data$timeOut >= cutoff2)
  lab_data$timeOut[idx] <- lab_data$timeOut[idx] + 12*60*60
  
  
  #Add length of stay variable (minutes)
  lab_data$length_Stay <- lab_data$timeOut - lab_data$timeIn
  lab_data$length_Stay[which(lab_data$length_Stay < 0)] <- NA #remove negative values
  
  #Add correct date and store timeIn and timeOut as characters to ease matrix manipulation later
  #first set the date to NA
  lab_data$timeIn <- format(lab_data$timeIn, "%H:%M")
  lab_data$timeOut <- format(lab_data$timeOut, "%H:%M")
  
  lab_data$timeIn <- paste(lab_data$betterDates, lab_data$timeIn)
  lab_data$timeIn <- strptime(lab_data$timeIn, format='%Y-%m-%d %H:%M')
  lab_data$timeOut <- paste(lab_data$betterDates, lab_data$timeOut)
  lab_data$timeOut <- strptime(lab_data$timeOut, format='%Y-%m-%d %H:%M')
  
  lab_data$timeIn <- as.character(lab_data$timeIn)
  lab_data$timeOut <- as.character(lab_data$timeOut)
  
  
  #---------------------------------------------------------------------------------------------
  #Clean Fellow
  #---------------------------------------------------------------------------------------------
  
  lab_data$fellow[(lab_data$fellow == "")] <- NA
  #split program into 3 columns
  lab_data <- lab_data %>%
    separate(fellow, c("fellow1", "fellow2", "fellow3"), ",")
  lab_data$fellow1[(lab_data$fellow1 == "2")] <- "Anna C."
  lab_data$fellow2[(lab_data$fellow2 == "2")] <- "Anna C."
  
  
  
  #---------------------------------------------------------------------------------------------
  #Clean hearAbout
  #---------------------------------------------------------------------------------------------
  lab_data$hearAbout <- toupper(lab_data$hearAbout)
  lab_data$hearAbout[lab_data$hearAbout==""] <- NA
  wkshp <- grep("WORKSHOP|CHEMISTRY WS|PRESENTATION|THEY TAUGHT",lab_data$hearAbout)
  prof <- intersect(grep("PROF|TEACHER|ADVISOR|INSTRUCTOR|FACULTY|KIMMEL|KAZUKI|MORRIS|DAVID|WEIMAN",
                         lab_data$hearAbout),grep("WORKSHOP",lab_data$hearAbout, invert = T))
  class <- intersect(grep("CLASS|SEMINAR|LAB|LECTURE|COURSE|CHEM|SOCIOLOGY|METHODS|INTRO TO",lab_data$hearAbout), 
                     grep("WORKSHOP|PRESENTATION|TAUGHT|TRISH|CLASSMATE|PROF|TEACHER|ADVISOR|DEPARTMENT|TA",lab_data$hearAbout, invert = T))
  student <- grep("FRIEND|CLASSMATE|STUDENT|PEER",lab_data$hearAbout)
  staff <- grep("ANNA|TRISH|SHANNON|KOLI|WORK THERE|I WORK HERE",lab_data$hearAbout) 
  TA <- grep("RECITATION|^TA$| TA$",lab_data$hearAbout)
  wom <- grep("W.O.M.|MOUTH|EVERYONE KNOWS|VERY KNOWN",lab_data$hearAbout)
  online <- intersect(grep("ONLINE|WEB",lab_data$hearAbout), grep("LIB|CLASS",lab_data$hearAbout,invert=T))
  dept <- grep("DEPARTMENT|DEPT",lab_data$hearAbout)
  library <- grep("LIBRARIAN|LIBRARY|LEFRAK|LIB", lab_data$hearAbout)
  
  
  lab_data$hearAbout[wkshp] <- "WORKSHOP"
  lab_data$hearAbout[prof] <- "PROFESSOR"
  lab_data$hearAbout[class] <- "CLASS"
  lab_data$hearAbout[student] <- "STUDENT"
  lab_data$hearAbout[staff] <- "ERC STAFF"
  lab_data$hearAbout[TA] <- "TA"
  lab_data$hearAbout[wom] <- "WORD OF MOUTH"
  lab_data$hearAbout[online] <- "ONLINE"
  lab_data$hearAbout[dept] <- "DEPARTMENT"
  lab_data$hearAbout[library] <- "LIBRARY"
  
  
  hearAbout_cat <- c("WORKSHOP","PROFESSOR","CLASS","STUDENT","ERC STAFF","TA",
                     "WORD OF MOUTH","ONLINE","DEPARTMENT","LIBRARY",NA)
  lab_data$hearAbout[!lab_data$hearAbout %in% hearAbout_cat] <- "OTHER"
  lab_data$hearAbout <- factor(lab_data$hearAbout, levels = c(hearAbout_cat, "OTHER"))
  
  #---------------------------------------------------------------------------------------------
  #Clean Course
  #---------------------------------------------------------------------------------------------
  lab_data$course <- toupper(lab_data$course)
  lab_data$course[lab_data$course==""] <- NA
  
  social_methods <- grep("SOCIAL METHODS|SOCIAL RESEARCH|3010|RESEARCH METHODS|SOCIOLOGY, INTRO/METHODS",lab_data$course)
  psych_stat <- grep("PSYCH.*STAT|PYSCH-STATS|STAT.*PSYCH",lab_data$course)
  intro_gis <- grep("INTRO.*GIS|URBAN STUDIES.*GIS",lab_data$course)
  intro_econ <- grep("INTRO.*ECON|WEIMAN",lab_data$course)
  dev_econ <- intersect(grep("DEVELOPMENT|DEVEL ECON|WOMEN.*DEV",lab_data$course), grep("DEVELOPMENTAL PSYCH",lab_data$course, invert=T))
  miller <- grep("ELECTIONS",lab_data$course)
  dev_psych <- grep("DEV.*PSYCH",lab_data$course)
  gen_chem <- intersect(grep("CHEM",lab_data$course), grep("SPECTROSCOPY",lab_data$course, invert=T))
  gis_methods <- intersect(grep("GIS.*METHODS|METHODS.*GIS",lab_data$course), grep("INTRO",lab_data$course, invert=T))
  spec <- grep("SPECTROSCOPY",lab_data$course)
  ind <- grep("INDEPENDENT",lab_data$course)
  parties <- grep("PARTIES|KRIMMEL",lab_data$course)
  physics <- grep("PHYSICS", lab_data$course)
  reg <- grep("REGRESSION", lab_data$course)
  psych_person <- grep("PERSONALITY PSYCH", lab_data$course)
  law_society <- grep("LAW.*SOC", lab_data$course)
  soc <- grep("INTRO.*SOC",lab_data$course) 
  ps4710 <- grep("4710",lab_data$course)
  
  lab_data$course[social_methods] <- "SOCIAL RESEARCH METHODS"
  lab_data$course[psych_stat] <- "PSYCH STATS"
  lab_data$course[intro_gis] <- "INTRO GIS METHODS"
  lab_data$course[intro_econ] <- "INTRO ECON"
  lab_data$course[dev_econ] <- "DEVELOPMENT ECONOMICS"
  lab_data$course[miller] <- "AMERICAN ELECTIONS"
  lab_data$course[dev_psych] <- "DEVELOPMENTAL PSYCHOLOGY LAB"
  lab_data$course[gen_chem] <- "GENERAL CHEMISTRY LAB"
  lab_data$course[gis_methods] <- "GIS METHODS"
  lab_data$course[parties] <- "AMERICAN POLITICAL PARTIES"
  lab_data$course[spec] <- "CHEMISTRY SPECTROSCOPY LAB"
  lab_data$course[ind] <- "INDEPENDENT STUDY"
  lab_data$course[physics] <- "PHYSICS LAB"
  lab_data$course[reg] <- "APPLIED LINEAR REGRESSION ANALYSIS"
  lab_data$course[psych_person] <- "PERSONALITY PSYCH LAB"
  lab_data$course[law_society] <- "LAW AND SOCIETY"
  lab_data$course[soc] <- "INTRO TO SOCIOLOGY" 
  lab_data$course[ps4710] <- "QUANTITATIVE POLITICAL RESEARCH"
  
  
  # levels(factor(lab_data$course))
  # summary(factor(lab_data$course))
  # summary(factor(lab_data$course))[order(summary(factor(lab_data$course)),decreasing = T)]
  
  
  #---------------------------------------------------------------------------------------------
  #Return important attributes of data frame
  #---------------------------------------------------------------------------------------------
  
  imp_attr <- c("name", "uni", "betterDates", "dow", "timeIn", "timeOut", "length_Stay", "school", "year", 
                "program1", "program2", "program3", "visitBefore", "workshop", "problem", "majorCat", "majorCat2", 
                "fellow1", "fellow2", "fellow3", "affiliation", "hearAbout", "course", "semester","n_problems")
  return(lab_data[,imp_attr])
}