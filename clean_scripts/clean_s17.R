clean_s17 <- function(lab_data) {
  #Clean Spring 17 Data
  # install.packages("ggplot2")
  # install.packages("tidyverse")
  # install.packages("lubridate")
  # install.packages("dplyr")
  # install.packages("tidyr")
  # install.packages("stringr")
  # install.packages("stringi")
  
  
  #rm(list=ls(all=TRUE))
  
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
  lab_data <- lab_data[-c(1:2),17:43]
  
  names(lab_data) <- c("name","uni", "dateMonth", "dateDay", "dateYear", "fellow", "school", "school_text",
                       "major1", "major2", "affiliation", "year", "yearText", "program", "programText",
                       "visitBefore", "workshop", "hearAbout","comments", "timeIn_hour", "timeIn_minute", 
                       "timeOut_hour", "timeOut_minute", "course", "problem","problemText", "notes")
  
  #Add semester column
  lab_data$semester <- "Spring 2017"
  
  #---------------------------------------------------------------------------------------------
  #MAJOR CLEANING
  #use summary(lab_data$major) to see all majors and add relevant majors to below lists
  #---------------------------------------------------------------------------------------------
  lab_data$major1 <- as.character(lab_data$major1)
  
  major <- lab_data[,c("major1","major2")] %>%
    separate(major1, c("major1", "remove"), "\t") %>%
    separate(major2, c("major2", "remove"), "\t")
  lab_data$major1 <- major$major1
  lab_data$major2 <- major$major2
  
  lab_data$major1[lab_data$major1 == "Art History Major, Minor"] <- "Art History"
  lab_data$major2[lab_data$major2 == "Art History Major, Minor"] <- "Art History"
  
  lab_data$major1[lab_data$major1==""] <- NA
  lab_data$major2[lab_data$major2==""] <- NA
  
  majors <- as.matrix(read.csv("majors/Majors.csv", header = FALSE))
  lab_data$major1 <- factor(lab_data$major1, levels=majors)
  lab_data$major2 <- factor(lab_data$major2, levels=majors)
  # summary(lab_data$major1)
  # summary(lab_data$major2)
  
  
  
  #---------------------------------------------------------------------------------------------
  #YEAR CLEANING
  #---------------------------------------------------------------------------------------------
  
  lab_data$year[lab_data$year==""] <- NA
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
  # summary(lab_data$visitBefore)
  
  
  
  #---------------------------------------------------------------------------------------------
  #WORKSHOP CLEANING
  #---------------------------------------------------------------------------------------------
  lab_data$workshop[lab_data$workshop==""] <- NA
  lab_data$workshop <- factor(lab_data$workshop)
  # summary(lab_data$workshop)
  
  
  
  #---------------------------------------------------------------------------------------------
  #MAJOR CAT
  #Group majors into categories: Soical Sciences, STEM, Humanities, Other, Undeclared
  #---------------------------------------------------------------------------------------------
  
  majors_SS <- as.matrix(read.csv("majors/Majors_SS.csv", header = FALSE))
  majors_STEM <- as.matrix(read.csv("majors/Majors_STEM.csv", header = FALSE))
  majors_Hum <- as.matrix(read.csv("majors/Majors_Humanities.csv", header = FALSE))
  
  lab_data$majorCat <- NULL
  lab_data$majorCat2 <- NULL
  lab_data$majorCat[lab_data$major1 %in% majors_SS] <- "Social Sciences"
  lab_data$majorCat[lab_data$major1 %in% majors_STEM] <- "STEM"
  lab_data$majorCat[lab_data$major1 %in% majors_Hum] <- "Humanities"
  lab_data$majorCat[lab_data$major1 == "Other"] <- "Other"
  lab_data$majorCat2[lab_data$major2 %in% majors_SS] <- "Social Sciences"
  lab_data$majorCat2[lab_data$major2 %in% majors_STEM] <- "STEM"
  lab_data$majorCat2[lab_data$major2 %in% majors_Hum] <- "Humanities"
  lab_data$majorCat2[lab_data$major2 == "Other"] <- "Other"
  
  
  lab_data$majorCat <- factor(lab_data$majorCat, levels = c("Social Sciences", "STEM", "Humanities", "Other"))
  lab_data$majorCat2 <- factor(lab_data$majorCat2, levels = c("Social Sciences", "STEM", "Humanities", "Other"))
  # summary(lab_data$majorCat)
  
  
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
  #---------------------------------------------------------------------------------------------
  dates <- paste(lab_data$dateMonth, lab_data$dateDay, lab_data$dateYear, sep = "/")
  dates <- as.Date(dates, format = "%m/%d/%y")
  betterDates <- format(dates, "2017-%m-%d")
  lab_data$betterDates <- betterDates
  
  lab_data$dow <- wday(betterDates)
  lab_data$dow <- lab_data$dow
  
  lab_data$dow[lab_data$dow == 1] <- "Sunday"
  lab_data$dow[lab_data$dow == 2] <- "Monday"
  lab_data$dow[lab_data$dow == 3] <- "Tuesday"
  lab_data$dow[lab_data$dow == 4] <- "Wednesday"
  lab_data$dow[lab_data$dow == 5] <- "Thursday"
  lab_data$dow[lab_data$dow == 6] <- "Friday"
  lab_data$dow[lab_data$dow == 7] <- "Saturday"
  
  lab_data$dow <- factor(lab_data$dow, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), ordered=TRUE)

  
  # -------------------------------------------------------------------------
  # Clean and Format timeIn and timeOut
  # -------------------------------------------------------------------------
  #add 12 hours to those not in military time
  lab_data$timeIn_hour <- as.numeric(as.character(lab_data$timeIn_hour))
  lab_data$timeIn_hour[which(lab_data$timeIn_hour < 10)] <- lab_data$timeIn_hour[which(lab_data$timeIn_hour < 10)] + 12
  lab_data$timeIn_hour <- as.character(lab_data$timeIn_hour)
  lab_data$timeOut_hour <- as.numeric(as.character(lab_data$timeOut_hour))
  lab_data$timeOut_hour[which(lab_data$timeOut_hour < 10)] <- lab_data$timeOut_hour[which(lab_data$timeOut_hour < 10)] + 12
  lab_data$timeOut_hour <- as.character(lab_data$timeOut_hour)
  
  #Format Times
  lab_data$timeIn_minute[with(lab_data, (timeIn_minute == "" | timeIn_minute == "0" ) & 
                           timeIn_hour != "")] <- "00"
  lab_data$timeOut_minute[with(lab_data, (timeOut_minute == "" | timeOut_minute == "0") & 
                                 timeOut_hour != "")] <- "00"
  lab_data$timeIn <- paste0(lab_data$timeIn_hour, ":", lab_data$timeIn_minute)
  lab_data$timeOut <- paste0(lab_data$timeOut_hour, ":", lab_data$timeOut_minute)
  lab_data$timeIn[lab_data$timeIn == ":"] <- NA
  lab_data$timeOut[lab_data$timeOut == ":"] <- NA
  
  
  lab_data$timeIn2 <- lab_data$timeIn
  lab_data$timeOut2 <- lab_data$timeOut
  
  lab_data$timeIn <- paste(lab_data$betterDates, lab_data$timeIn)
  lab_data$timeIn <- strptime(lab_data$timeIn, format='%Y-%m-%d %H:%M')
  lab_data$timeOut <- paste(lab_data$betterDates, lab_data$timeOut)
  lab_data$timeOut <- strptime(lab_data$timeOut, format='%Y-%m-%d %H:%M')
  
  #add 12 hours to those not in military time
  # min_Time <- strptime("9:00", format='%H:%M')
  # lab_data$timeIn[which(lab_data$timeIn < min_Time)] <- lab_data$timeIn[which(lab_data$timeIn < min_Time)] + 12*60*60
  # lab_data$timeOut[which(lab_data$timeOut < min_Time)] <- lab_data$timeOut[which(lab_data$timeOut < min_Time)] + 12*60*60
  # 
  #Add length of stay variable (minutes)
  lab_data$length_Stay <- lab_data$timeOut - lab_data$timeIn
  lab_data$length_Stay[which(lab_data$length_Stay < 0)] <- NA #remove negative values
  
  #Store timeIn and timeOut as characters to ease matrix manipulation later
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
  
  public_opinion <- grep("PUBLIC OPINION|PUBLIC PERCEPTION|KIMMEL POLI SCI|KRIMMEL", lab_data$course)
  prog_behave_sci <- grep("PROGRAMMING.*BEHAVIORAL SCIENCE|PROGRAMING", lab_data$course)
  stat_comp <- grep("STATISTICAL COMPUTING|APPLIED STAT|STAT COMPUTING|STATICAL COMPUTING", lab_data$course)
  thesis <- grep("THESIS", lab_data$course)
  empir_dev <- grep("EMPIRICAL DEV|DEVELOPMENT ECON|DEVELOPMENTAL ECON|ANJA'S CLASS|EMPIRICAL ECONOMICS", lab_data$course)
  env_econ <- grep("ENVIRONMENTAL|ENIRO|ENVIRO ECON",lab_data$course)
  econometrics <- grep("ECONOMETRIC",lab_data$course)
  happiness <- grep("HAPPINESS.*SEMINAR|ECONOMICS HAPPINESS|ECONOMICS SENIOR SEMINAR",lab_data$course)
  
  ind <- grep("INDEPENDENT STUDY|INDEPENDENT RESEARCH|IND. STUDY",lab_data$course)
  ps_methods <- grep("POLITICAL SCIENCE.*METHODS|EMPIRICAL RESEARCH",lab_data$course)
  finance <- grep("FINANCIAL", lab_data$course)
  arch <- grep("ARCHAEOLOGY|SOUTHERN LEVANT", lab_data$course)
  
  lab_data$course[public_opinion] <- "PUBLIC OPINION"
  lab_data$course[prog_behave_sci] <- "PROGRAMMING FOR BEHAVIORAL SCIENCES"
  lab_data$course[stat_comp] <- "APPLIED STATISTICAL COMPUTING"
  lab_data$course[thesis] <- "THESIS"
  lab_data$course[empir_dev] <- "EMPIRICAL DEVELOPMENT ECONOMICS"
  lab_data$course[env_econ] <- "ENVIRONMENTAL ECONOMICS"
  lab_data$course[econometrics] <- "ECONOMETRICS"
  lab_data$course[happiness] <- "HAPPINESS ECONOMICS SEMINAR"
  lab_data$course[ind] <- "INDEPENDENT STUDY"
  lab_data$course[ps_methods] <- "POLITICAL SCIENCE RESEARCH METHODS"
  lab_data$course[finance] <- "FINANCIAL INSTABILITY"
  lab_data$course[arch] <- "ARCHAEOLOGY OF THE SOUTHERN LEVANT"
  econ_list <- c("ECON?","ECON","ECONOMICS")
  lab_data$course[lab_data$course %in% econ_list] <- "ECONOMICS (GENERAL)"
  
  # levels(factor(lab_data$course))
  # summary(factor(lab_data$course))
  # summary(factor(lab_data$course))[order(summary(factor(lab_data$course)),decreasing = T)]
  
  
  #---------------------------------------------------------------------------------------------
  #Return important attributes of data frame
  #---------------------------------------------------------------------------------------------
  
  imp_attr <- c("name", "uni", "betterDates", "dow", "timeIn", "timeOut", "length_Stay", "school", "year", 
                "program1", "program2", "program3", "visitBefore", "workshop", "problem", "majorCat", "majorCat2", 
                "fellow1", "fellow2", "fellow3", "affiliation", "hearAbout","course","semester","n_problems")
  return(lab_data[,imp_attr])
}