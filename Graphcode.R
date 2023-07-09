# clean current work space
rm(list=ls(all=T))
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
#install.packages(c())
library(ggplot2)
library(dplyr)
library(tidyr)
library(rmarkdown)
library(haven)
library(descr)
library(car)
library(RColorBrewer)
library(likert)
library(colorspace)
library(DescTools)
#####
#AAMC Data - was unable to download files as CSV. Will manually put dat in here
#AAMC 2021-2022 Matriculant data
  #Table A9 From AAMC 2021 MAtriculant Data
    A92021AIANonly <- c(39, 44, 36, 40)
    A92021AIANBlackcombo <- c(15, 14, 9, 13)
    A92021AIANWhitecombo <- c(106, 102, 112, 111)
  #Manually use variables from Table 14.3 from 2021 AAMC Data - do not use the year 2017-2018 so tables align
    A14.3AIANincombo <- c(179, 186, 212, 187)
    A14.3AIANtotal <- c(218, 230, 248, 227)
  #Use data from A9 and A14.3 to make a variable of AIAN in a combination other than white
    AIANincomboanotherrace <- A14.3AIANincombo - A92021AIANWhitecombo - A92021AIANBlackcombo

  # Table of raw numbers of Native matriculants by race/ethnicity
    Tablenativematriculants <- rbind(A92021AIANonly, A92021AIANBlackcombo, A92021AIANWhitecombo, AIANincomboanotherrace, A14.3AIANtotal)
      colnames(Tablenativematriculants) <- c("2018-2019", "2019-2020", "2020-2021", "2021-2022")
      rownames(Tablenativematriculants) <- c("AI/AN Only", "AI/AN, Black","AI/AN, White", "AI/AN, Another race", "AI/AN total")
    Tablenativematriculants
    View(Tablenativematriculants)
  
  #Table of Native Matriculates proportionally by race
    Tablenativematriculantsproportional <- rbind(A92021AIANonly, A92021AIANBlackcombo, A92021AIANWhitecombo, AIANincomboanotherrace)
      Tablenativematriculantsproportional[1,] <- 100 * Tablenativematriculantsproportional[1,] / A14.3AIANtotal
      Tablenativematriculantsproportional[2,] <- 100 * Tablenativematriculantsproportional[2,] / A14.3AIANtotal
      Tablenativematriculantsproportional[3,] <- 100 * Tablenativematriculantsproportional[3,] / A14.3AIANtotal
      Tablenativematriculantsproportional[4,] <- 100 * Tablenativematriculantsproportional[4,] / A14.3AIANtotal
      colnames(Tablenativematriculantsproportional) <- c("2018-2019", "2019-2020", "2020-2021", "2021-2022")
     rownames(Tablenativematriculantsproportional) <- c("AI/AN Only", "AI/AN, Black","AI/AN, White", "AI/AN, Another race")
    Tablenativematriculantsproportional
    View(Tablenativematriculantsproportional)
  
  #Table of Native matriculants proportional to all medical students
  Totalmedstudentmatriculants <- c(21662, 21869, 22239, 22665)
  Tablenativematriculantspopulation <- rbind(A92021AIANonly, A92021AIANBlackcombo, 
                                                                                    A92021AIANWhitecombo, AIANincomboanotherrace,                                                              A14.3AIANtotal)
    Tablenativematriculantspopulation
    Tablenativematriculantspopulation[1,] <- 100 * Tablenativematriculantspopulation[1,] / Totalmedstudentmatriculants
    Tablenativematriculantspopulation[2,] <- 100 * Tablenativematriculantspopulation[2,] / Totalmedstudentmatriculants
    Tablenativematriculantspopulation[3,] <- 100 * Tablenativematriculantspopulation[3,] / Totalmedstudentmatriculants
    Tablenativematriculantspopulation[4,] <- 100 * Tablenativematriculantspopulation[4,] / Totalmedstudentmatriculants
    Tablenativematriculantspopulation[5,] <- 100 * Tablenativematriculantspopulation[5,] / Totalmedstudentmatriculants
    Tablenativematriculantspopulation
      colnames(Tablenativematriculantspopulation) <- c("2018-2019", "2019-2020", "2020-2021", "2021-2022")
      rownames(Tablenativematriculantspopulation) <- c("AI/AN Only", "AI/AN, Black","AI/AN, White", "AI/AN, Another race", "AI/AN total")
      Tablenativematriculantspopulation
      View(Tablenativematriculantspopulation)
      
  # Table and graph for paper, less subgroups than above
  # Shows Matriculants AI/AN alone, in combo, and total per year
      Nativematriculantsaloneincombo <- rbind(A92021AIANonly, A14.3AIANincombo, A14.3AIANtotal)
      Nativematriculantsaloneincombo
      Nativematriculantsaloneincombo[1,] <- 100 * Nativematriculantsaloneincombo[1,] / Totalmedstudentmatriculants
      Nativematriculantsaloneincombo[2,] <- 100 * Nativematriculantsaloneincombo[2,] / Totalmedstudentmatriculants
      Nativematriculantsaloneincombo[3,] <- 100 * Nativematriculantsaloneincombo[3,] / Totalmedstudentmatriculants
      Nativematriculantsaloneincombo
      colnames(Nativematriculantsaloneincombo) <- c("2018-2019", "2019-2020", "2020-2021", "2021-2022")
      rownames(Nativematriculantsaloneincombo) <- c("AI/AN Only", "AI/AN in combo", "AI/AN total")
      Nativematriculantsaloneincombo
      View(Nativematriculantsaloneincombo)
      
    # Barplot
      #Reset so raw numbers and not proportions
      Nativematriculantsaloneincombo <- rbind(A92021AIANonly, A14.3AIANincombo, A14.3AIANtotal, deparse.level = 1)
      Nativematriculantsaloneincombo
    barplot(Nativematriculantsaloneincombo, 
            beside = T,
            main = "Native medical school Matriculants by year",
            names.arg = c("2018-2019", "2019-2020", "2020-2021", "2021-2022"),
            legend.text = c("AI/AN alone", "AI/AN in combo", "AI/AN total"),
            args.legend = list(x = "bottomleft", bty = 'n'),
            ylab = "Number of students"
            )

# 2020 Fall Applicant, Matriculant, and Enrollment Data Tables
    years <- c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)
    # Page 6 - applicants 2013-2020
    AIANapplicants <- c(425, 449, 463, 553, 508, 559, 586, 561)
    Totalapplicants <- c(48014, 49480, 52549, 53042, 51680, 52777, 53370, 53030)
    AIANpercentapplicants <- 100 * AIANapplicants / Totalapplicants
    AIANpercentapplicants
    # Page 12 - Matriculants from 2013-2020
    AIANmatriculants <- c(173, 202, 196, 194, 205, 218, 230, 248)
    Totalmatriculants <- c(20055, 20343, 20631, 21030, 21338, 21622, 21869, 22239)
    PercentAIANmatriculants <- 100 * AIANmatriculants / Totalmatriculants
    PercentAIANmatriculants
    
# 2021 Matriculation data Table A-12 Applicants, First Time applicants, Acceptees, Matriculatns 2018-2019 -> 2020-2021 by Race (alone)
    years <- c("2018-2019", "2019-2020", "2020-2021", "2021-2022")
    menAIANapplicants <- c(58, 40, 31, 46)    
    menAIANfirsttimeapplicants <- c(37, 27, 24, 35)
    menAIANacceptees <- c(23, 23, 15, 20)
    menAIANmatriculants <- c(22, 22, 15, 18)

    womenAIANapplicants <- c(51, 49, 41, 59)
    womenAIANfirsttimeapplicants <- c(38, 32, 28, 51)
    womenAIANacceptees <- c(21, 23, 22, 26)
    womenAIANmatriculants <- c(17, 22, 21, 22)
    
    totalAIANapplicants <- menAIANapplicants + womenAIANapplicants
    totalAIANapplicants
    totalaianfirsttimeapplicants <- menAIANfirsttimeapplicants + womenAIANfirsttimeapplicants
    totalaianfirsttimeapplicants
    totalaianacceptees <- menAIANacceptees + womenAIANacceptees
    totalaianacceptees
    totalaianmatriculants <- menAIANmatriculants + womenAIANmatriculants
    totalaianmatriculants
    
    sum(womenAIANmatriculants)
    sum(menAIANmatriculants)
    
    AIANacceptancerate <- 100 * totalaianacceptees / totalAIANapplicants
    AIANacceptancerate
    
    totalmedschoolapplicants <- c(52777, 53370, 53030, 62432)
    totalmedschoolfirsttimeapplicants <- c(38483, 39237, 38581, 46750)
    totalmedschoolacceptees <- c(22483, 22687, 23105, 23712)
    totalmedschoolmatriculants <- c(21622, 21869, 22239, 22665)
    
    totalacceptancerate <- 100 * totalmedschoolacceptees / totalmedschoolapplicants
    totalacceptancerate
    

# Comparing 14-3 (race of matriculants) to Table 14-1 - (applicants) 2017-2018 through 2021-2022
    years <- c("2017-2018", "2018-2019", "2019-2020", "2020-2021", "2021-2022")
    # Table 14.1 - applicants
    AIANapplicantsalone <- c(100, 109, 89, 73, 105)
    AIANapplicantscombo <- c(408, 450, 497, 488, 584)
    totalAIANapplicants <- AIANapplicantsalone + AIANapplicantscombo
    totalapplicants <- c(51580, 52777, 53370, 53030, 62432)
    
    #Table 14.3 - matriculants
    AIANmatriculantsalone <- c(42, 39, 44, 36, 40)
    AIANmatriculantsincombo <- c(163, 179, 186, 212, 187)
    AIANmatriculantstotal <- AIANmatriculantsalone + AIANmatriculantsincombo
    Totalmatriculants <- c(21338, 21622, 21869, 22239, 22665)
    
    AIANmatriculantpercent <-  100 * AIANmatriculantstotal / Totalmatriculants
    AIANmatriculantpercent
    # These "rates" are the matriculants divided by the applicants. This is not a true acceptance rate
    AIANalonerate <- 100 * AIANmatriculantsalone / AIANapplicantsalone
    AIANalonerate
    AIANcomborate <- 100 * AIANmatriculantsincombo / AIANapplicantscombo
    AIANcomborate    

    
# Table A-18 MCAT and GPA for applicants and matriculants by race (alone only) 2021-2022
    MCAT <- c("CPBS", "CARS", "BBLS", "PSBB", "Total")
    AIANapplicantmcat <- c(124, 124.2, 124.6, 125.2, 498)
      AIANapplicantmcatstd <- c(2.7, 2.5, 2.8, 2.9, 9.4)
    totalapplicantmcat <- c(126.3, 125.7, 126.7, 127.3, 505.9)
      totalapplicantmcatstd <- c(2.9, 2.8, 2.8, 2.8, 9.7)    
    
    AIANmatriculantmcat <- c(125.5, 125.4, 126.3, 126.6, 503.8)
      AIANmatriculantmcatstd <- c(2.8, 2.4, 2.4, 2.6, 7.9)
    totalmatriculantmcat <- c(127.9, 127, 128.2, 128.8, 511.9)
      totalmatriculantmcatstd <- c(2.2, 2.3, 2.1, 2, 6.6)
      
    mcattable <- rbind(AIANapplicantmcat, totalapplicantmcat, AIANmatriculantmcat, totalmatriculantmcat)
      rownames(mcattable) <- c("AI/AN Applicants", "All Applicnats", "AI/AN Matriculants", "All Matriculants")
      colnames(mcattable) <- MCAT
    mcattable  
      
    GPA <- c("Science", "Non-Science", "Total")
    AIANapplicantGPA <- c(3.16, 3.58, 3.36)
      AIANapplicantGPAstd <- c(0.58, 0.33, 0.41)
    totalapplicantGPA <- c(3.48, 3.74, 3.59)
      totalapplicantGPAstd <- c(0.44, 0.28, 0.34)
    
    AIANmatriculantgpa <- c(3.34, 3.66, 3.49)
      AIANmatriculantgpastd <- c(0.43, 0.28, 0.33)
    totalmatriculantgpa <- c(3.67, 3.83, 3.74)
      totalmatriculantgpastd <- c(0.31, 0.21, 0.25)
      
    gpatable <- rbind(AIANapplicantGPA, totalapplicantGPA, AIANmatriculantgpa, totalmatriculantgpa)
      rownames(gpatable) <- c("AI/AN Applicants", "All Applicnats", "AI/AN Matriculants", "All Matriculants")
      colnames(gpatable) <- GPA
    gpatable
    

##### 
#ANAMS survey data
#Read in data
setwd('~/ANAMS/IHS paper/Paper/Spreadsheet Downloads/')
ANAMSdata <- read.csv('ANAMS Survey_August 7, 2021 numeric.csv', na.strings = c("", "NA"))
stringdata <- read.csv('ANAMS Survey_August 7, 2021 string.csv', na.strings = c("", "NA"))

# Store a matrix of questions
Questionstring <- stringdata[c(1),]

#Clean data, remove incomplete answers
ANAMSdata <- ANAMSdata[3:nrow(ANAMSdata),] # Remove first two rows of data from qualtrics - this data is not response data
  ANAMSdata <- subset(x = ANAMSdata, subset = as.numeric(ANAMSdata[,5]) == 100) # Remove incomplete responses
  ANAMSdata <- subset(x = ANAMSdata, subset = as.numeric(ANAMSdata[,18]) == 1) # Remove those who did not consent
  ANAMSdata <- subset(x = ANAMSdata, subset = as.numeric(ANAMSdata[,19]) == 1) # Remove those who are not ANAMS members
  View(ANAMSdata)
  
  
dim(ANAMSdata)

stringdata <- stringdata[3:nrow(stringdata),] # Remove first two rows of data from qualtrics
  stringdata <- subset(x = stringdata, subset = as.numeric(stringdata[,5]) == 100) # Remove incomplete responses
  stringdata <- subset(x = stringdata, subset = stringdata[,18] == "Yes") # Remove those who did not consent
  stringdata <- subset(x = stringdata, subset = stringdata[,19] == "Yes") # Remove those who are not ANAMS members
  View(stringdata)

# Number of total responses
numresponses <- length(ANAMSdata[,1])
numresponses

  #For subgroup analysis of only AI/AN
    onlyaian <- subset(x = stringdata, subset = stringdata[,20] == "American Indian or Alaska Native (AI-AN)") # Remove those who are not ANAMS members
    View(onlyaian)
    onlyaiannum <- subset(x = ANAMSdata, subset = ANAMSdata[,20] == 1)
    View(onlyaiannum)
  
  #For subgroup analysis of only AI/AN in combo with another race
    aiancombo <- subset(x = stringdata, subset = stringdata[,20] != "American Indian or Alaska Native (AI-AN)") # Remove those who are not ANAMS members
    View(aiancombo)
    aiancombonum <- subset(x = ANAMSdata, subset = ANAMSdata[,20] != 1)
    View(aiancombonum)
    
  # For subgroup of those who received IHS healthcare - READY TO GO
    onlynativehc <- subset(x = stringdata, subset = stringdata[,24] != "No")
    View(onlynativehc)  
    
  # For subgroup of those who grew up in a native community - READY TO GO
    nativecommunity <- subset(x = stringdata, subset = stringdata[,23] != "I am affiliated with a tribe (descendant of)")
    nativecommunity <- subset(x = nativecommunity, subset = nativecommunity[,23] != "I am an enrolled member of a Native tribe")
    nativecommunity <- subset(x = nativecommunity, subset = nativecommunity[,23] != "I am an enrolled member of a Native tribe,I am affiliated with a tribe (descendant of)")
    View(nativecommunity)    
  
#Q3 - Ethnicity
table(stringdata[,c(20)])
pie(table(stringdata[,c(20)]),main = stringdata[c(1),c(20)])
freq(stringdata[,c(20)])
freq
freq(onlynativehc[,c(20)])
freq(nativecommunity[,c(20)])

# CI for ANAMS population - AI/AN alone
n <- 39
p <- 21/39
margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
lowerinterval <- p - margin
upperinterval <- p + margin
lowerinterval
upperinterval

# CI for ANAMS population - AN/AN in combination
n <- 39
p <- 18/39
margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
lowerinterval <- p - margin
upperinterval <- p + margin
lowerinterval
upperinterval


#Q4 Gender Identity 
table(stringdata[,c(21)])
barchart(stringdata[,c(21)])
freq(stringdata[,c(21)])

freq(onlyaian[,c(21)])
freq(aiancombo[,c(21)])

freq(onlynativehc[,c(21)])
freq(nativecommunity[,c(21)])

  #Categorical stats Testing
  a <- table(onlyaian[,c(21)])
  b <- table(aiancombo[,c(21)])
  gender <- rbind(a,b)
  rownames(gender) <- c("AI/AN alone", "AI/AN in combination")
  gender
  #chisq.test(gender)
  fisher.test(gender)

  # CI for ANAMS population - male
  n <- 39
  p <- 12/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
  # CI for ANAMS population - female
  n <- 39
  p <- 27/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
#Geographic Data - Not used in paper
#Q5, Q8, Q10
#Add in map of IHS territories
#Add in IHS Clinic data from the website
#Q5 - home state
  table(stringdata[,c(22)])
  pie(table(stringdata[,c(22)]),main = Questionstring[,c(22)])
  freq(stringdata[,c(22)])
#Q8 - State medical school
  table(stringdata[,c(25)])
  pie(table(stringdata[,c(25)]),main = Questionstring[,c(25)])
  freq(stringdata[,c(25)])
#Q10 - School in IHS - maybe try to put on a map?
  table(stringdata[,c(27)])
  pie(table(stringdata[,c(27)]), main = Questionstring[27], edges = 200,radius = 0.8, init.angle =  90, density = NULL,angle = 45, col = NULL, border = NULL, lty = NULL)
  freq(stringdata[,c(27)])
  barchart(stringdata[,c(27)])
#Recode so that its state to IHS location
  geodat <- stringdata[, c(22, 25, 27)]
  geodat
  View(geodat)

q5 <- stringdata[,c(22)] # home state
View(q5)
q8 <- stringdata[,c(25)] # state of medical school
View(q8)
q10 <- stringdata[,c(27)] # School in IHS
View(q10)

samestate <- q5 == q8
samestate

length(subset(samestate,x = TRUE))

movedoutofstate <- subset(samestate, x = FALSE)
movedoutofstate



###This part is not automated. You will have to change the row numbers if people from more states answer
#Alaska - Alaska
  #No respondents from Alaska
q5alaska <- 0
q8alaska <- 0

#Albuquerque - Colorado, New Mexico
q5albuquerque <- sum(q5 == "Colorado") + sum(q5 == "New Mexico")
  q5albuquerque
q8albuquerque <- sum(na.omit(q8) == "Colorado") + sum(na.omit(q8) == "New Mexico") # get rid of NAs
  q8albuquerque

#Bemidji - Minnesota, Wisconsin, Michigan
q5bemidji <- sum(q5 == "Minnesota") + sum(q5 == "Wisconsin") + sum(q5 == "Michigan")
  q5bemidji
q8bemidji <- sum(na.omit(q8) == "Minnesota") + sum(na.omit(q8) == "Wisconsin") + sum(na.omit(q8) == "Michigan")
  q8bemidji  

#Billings - Montana, Wyoming
q5billings <- sum(q5 == "Montana") + sum(q5 == "Wyoming")
  q5billings
q8billings <- sum(na.omit(q8) == "Montana") + sum(na.omit(q8) == "Wyoming")
  q8billings  

#California - California
q5california <- sum(q5 == "California")
  q5california
q8california <- sum(na.omit(q8) == "California")
  q8california
  
#Great Plains - Nebraska, Iowa, North Dakota, South Dakota
q5gp <- sum(q5 == "Nebraska") + sum(q5 == "Iowa") + sum(q5 == "North Dakota") + sum(q5 == "South Dakota")
  q5gp
q8gp <- sum(na.omit(q8) == "Nebraska") + sum(na.omit(q8) == "Iowa") + sum(na.omit(q8) == "North Dakota") + sum(na.omit(q8) == "South Dakota")
  q8gp


#Nashville - Texas, Louisiana, Arkansas, Missouri, Illinois, Indiana, Ohio, Kentucky,
#             Tennessee, Alabama, Georgia, Florida, South Carolina, North Carolina,
#             Virginia, West Virginia, Pennsylvania, Delaware, Maryland, DC,
#             New Jersey, New York, Connecticut, Rhode Island, Massachusetts,
#             New Hampshire, Vermont, Maine
q5nashville <- (sum(q5 == "Texas") + sum(q5 == "Louisiana") + sum(q5 == "Arkansas") + sum(q5 == "Missouri")
                + sum(q5 == "Illinois") + sum(q5 == "Indiana") + sum(q5 == "Ohio") 
                + sum(q5 == "Kentucky") + sum(q5 == "Tennessee") + sum(q5 == "Alabama")
                + sum(q5 == "Georgia") + sum(q5 == "Florida") + sum(q5 == "South Carolina")
                + sum(q5 == "North Carolina") + sum(q5 == "Virginia") + sum(q5 == "West Virginia")
                + sum(q5 == "Pennslyvania") + sum(q5 == "Delaware") + sum(q5 == "Maryland")
                + sum(q5 == "DC") + sum(q5 == "New Jersey") + sum(q5 == "New York")
                + sum(q5 == "Connecticut") + sum(q5 == "Rhode Island") + sum(q5 == "Massachusetts")
                + sum(q5 == "New Hampshire") + sum(q5 == "Vermont") + sum(q5 == "Maine")
                )
  q5nashville
  
q8nashville <- (sum(na.omit(q8) == "Texas") + sum(na.omit(q8) == "Louisiana") + sum(na.omit(q8) == "Arkansas") + sum(na.omit(q8) == "Missouri")
                  + sum(na.omit(q8) == "Illinois") + sum(na.omit(q8) == "Indiana") + sum(na.omit(q8) == "Ohio") 
                  + sum(na.omit(q8) == "Kentucky") + sum(na.omit(q8) == "Tennessee") + sum(na.omit(q8) == "Alabama")
                  + sum(na.omit(q8) == "Georgia") + sum(na.omit(q8) == "Florida") + sum(na.omit(q8) == "South Carolina")
                  + sum(na.omit(q8) == "North Carolina") + sum(na.omit(q8) == "Virginia") + sum(na.omit(q8) == "West Virginia")
                  + sum(na.omit(q8) == "Pennslyvania") + sum(na.omit(q8) == "Deleware") + sum(na.omit(q8) == "Maryland")
                  + sum(na.omit(q8) == "DC") + sum(na.omit(q8) == "New Jersey") + sum(na.omit(q8) == "New York")
                  + sum(na.omit(q8) == "Connecticut") + sum(na.omit(q8) == "Rhode Island") + sum(na.omit(q8) == "Massachusetts")
                  + sum(na.omit(q8) == "New Hampshire") + sum(na.omit(q8) == "Vermont") + sum(na.omit(q8) == "Maine")
                )
  q8nashville

  
#Navajo - Parts of Arizona, Utah, New Mexico
q5nav <- 0
q8nav <- 0
  
#Oklahoma City - Kansas, Oklahoma
q5okc <- sum(q5 == "Kansas") + sum(q5 == "Oklahoma")
  q5okc
q8okc <- sum(na.omit(q8) == "Kansas") + sum(na.omit(q8) == "Oklahoma")
  q8okc

#Phoenix - Arizona, Utah, Nevada
q5phoenix <- sum(q5 == "Arizona") + sum(q5 == "Utah") + sum(q5 == "Nevada")
  q5phoenix
q8phoenix <- sum(na.omit(q8) == "Arizona") + sum(na.omit(q8) == "Utah") + sum(na.omit(q8) == "Nevada")
  q8phoenix

#Portland - Washington, Idaho, Oregon
q5portland <- sum(q5 == "Washington") + sum(q5 == "Idaho") + sum(q5 == "Oregon")
  q5portland
q8portland <- sum(na.omit(q8) == "Washington") + sum(na.omit(q8) == "Idaho") + sum(na.omit(q8) == "Oregon")
  q8portland

#Tuscon - Part of Arizona
q5t <- 0
  q5t
q8t <- sum(na.omit(q10 == "Tuscon"))
  q8t
         
#Putting it all together into a graph
geographlabel <- c("Alaska", "Albuquerque", "Bemidji", "Billings",
                   "California", "Great Plains", "Nashville", "Navajo",
                   "Oklahoma City", "Phoenix", "Portland", "Tuscon")
q5graph <- c(q5alaska, q5albuquerque, q5bemidji, q5billings, 
             q5california, q5gp, q5nashville, q5nav, q5okc, q5phoenix, 
             q5portland, q5t) / sum(q5graph) * 100
  q5graph
  View(q5graph)
q8graph <- c(q8alaska, q8albuquerque, q8bemidji, q8billings, 
             q8california, q8gp, q8nashville, q8nav, q8okc, q8phoenix, 
             q8portland, q8t) / sum(q8graph) * 100
  q8graph
  View(q8graph)

localanamschapters <- c(0, 1, 4, 0, 2, 1, 0, 0, 0, 0,2, 1 ) / sum(localanamschapters) * 100
localanamschapters  

geoplot <- rbind(q5graph, q8graph, localanamschapters, deparse.level = 1)
colnames(geoplot) <- geographlabel
  View(geoplot)
  x11()
barplot(geoplot, beside = T, main = "Geographic location by IHS Areas:",
          legend.text = c("Home state", "Medical School", "Local ANAMS chapters"),
          args.legend = list(x = 'topright', bty = 'n'),
          ylab = "Percent"
  )

#Q6 - select all that apply to you - grow up on reservation - named Q3 on R
#Percent that selected any option
table(stringdata[,c(23)])
percent.grewup.native <- freq(stringdata[,c(23)])
percent.grewup.native
pct.rez <- sum(percent.grewup.native[8:10,2]) # Gives total percentage who grew up on a reservation
pct.rez
pct.another.native.community <- sum(percent.grewup.native[c(3:6, 10),2]) # Gives total number of respondents who grew up in a primarily native community
pct.another.native.community
pct.enrolled.tribal.members <- sum(percent.grewup.native[c(2, 3, 6, 7, 9, 10),2])
pct.enrolled.tribal.members

    # Graph not used in paper
    #Want to see the number of people who selected each individual option
    dat <- ANAMSdata[, 23]
    dat <- gsub(",", "", dat)
      num.ans1 <- gsub("2", "", dat)
      num.ans1 <- gsub("3", "", num.ans1)
      num.ans1 <- gsub("4", "", num.ans1)
      num.ans1 <- sum(as.numeric(subset(num.ans1, !is.na(as.numeric(num.ans1)))))
      num.ans1
    num.ans2 <- gsub("1", "", dat)
      num.ans2 <- gsub("3", "", num.ans2)
      num.ans2 <- gsub("4", "", num.ans2)
      num.ans2 <- sum(as.numeric(subset(num.ans2, !is.na(as.numeric(num.ans2)))))/2
      num.ans2
    num.ans3 <- gsub("1", "", dat)
      num.ans3 <- gsub("2", "", num.ans3)
      num.ans3 <- gsub("4", "", num.ans3)
      num.ans3 <- sum(as.numeric(subset(num.ans3, !is.na(as.numeric(num.ans3)))))/3
      num.ans3
    num.ans4 <- gsub("1", "", dat)
      num.ans4 <- gsub("2", "", num.ans4)
      num.ans4 <- gsub("3", "", num.ans4)
      num.ans4 <- sum(as.numeric(subset(num.ans4, !is.na(as.numeric(num.ans4)))))/4
      num.ans4
    num.ans <- c(num.ans1, num.ans2, num.ans3, num.ans4)
    pct.ans.q6 <- num.ans/length(subset(stringdata[3:46,23], !is.na(stringdata[3:46,23])))
    pct.ans.q6
    #Plot
    bar.names.q6 <- c("Grew up on a Native Reservation", 
                  "Grew up in another Native community", 
                  "Are enrolled tribal members", 
                  "Are affiliated with a tribe (descendant of)"
                  )
    library(RColorBrewer)
    x11()
    barplot(pct.ans.q6, main = "Percent of respondents who:", 
        legend.text = bar.names.q6, 
        col = c("#CA0020", "#F4A582", "#92C5DE", "#0571B0"), 
        args.legend = list(x = 'topleft', bty = 'n')
            )
    
    freq(onlyaian[,c(23)])
    table(onlyaian[,c(23)])
    freq(aiancombo[,c(23)])
    table(aiancombo[,c(23)])
    
    freq(onlynativehc[,c(23)])
    freq(nativecommunity[,c(23)])
    

    #Statistics Testing
    a <- table(onlyaian[,c(23)])
     a
    onlyaian.res <- a[7] + a[8]
      onlyaian.res
    onlyaian.nonres <- a[3] + a[4] + a[5] + a[6] + a[8]
      onlyaian.nonres  
    onlyaian.enrolledtm <- a[2] + a[5] + a[6] + a[7] + a[8]
      onlyaian.enrolledtm
    onlyaian.onlyaffiliateddes <- a[1] 
      onlyaian.onlyaffiliateddes
    
    b <- table(aiancombo[,c(23)])
      b
    aiancombo.res <- b[7]
      aiancombo.res
    aiancombo.nonres <- b[4] + b[5] + b[6]
      aiancombo.nonres
    aiancombo.enrolledtm <- b[2] + b[3] + b[6]
      aiancombo.enrolledtm
    aiancombo.onlyaffiliateddes <- b[1]
      aiancombo.onlyaffiliateddes
    
    onlyaian.demographic <- c(onlyaian.res, onlyaian.nonres, onlyaian.enrolledtm, onlyaian.onlyaffiliateddes)
      onlyaian.demographic  
    aiancombo.demographic <- c(aiancombo.res, aiancombo.nonres, aiancombo.enrolledtm, aiancombo.onlyaffiliateddes)
      aiancombo.demographic  
        
    demographic <- rbind(onlyaian.demographic, aiancombo.demographic)
    rownames(demographic) <- c("AI/AN alone", "AI/AN in combination")
    colnames(demographic) <- c("Grew up on a reservation", "Grew up in a non-reservation Native community",
                               "Enrolled tribal Member", "(Only selected) 'Affiliated with a tribe / descendent of'")
    demographic
    chisq.test(demographic)
    fisher.test(demographic)
    
    # CI for ANAMS population - Lived on a rez
    n <- 39
    p <- 5/39
    margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
    lowerinterval <- p - margin
    upperinterval <- p + margin
    lowerinterval
    upperinterval
    
    # CI for ANAMS population - lived on a non-rez native community
    n <- 39
    p <- 9/39
    margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
    lowerinterval <- p - margin
    upperinterval <- p + margin
    lowerinterval
    upperinterval
    
    # CI for ANAMS population - Tribal membership
    n <- 39
    p <- 27/39
    margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
    lowerinterval <- p - margin
    upperinterval <- p + margin
    lowerinterval
    upperinterval
        
#Q7 - Select all that apply - where did you get healthcare growing up - Q4 in R
table(stringdata[c(3:46),c(24)])
freq(stringdata[,c(24)])
#recode out the multiple responses that chose an option and also selected "No"
stringdata$Q4 <- gsub(",No","",stringdata$Q4)
barplot(table(stringdata[c(3:46),c(24)]),main = stringdata[c(1),c(24)])
freq(stringdata[c(3:46),c(24)])

freq(onlyaian[,c(24)])
freq(aiancombo[,c(24)])

freq(onlynativehc[,c(24)])
freq(nativecommunity[,c(24)])

  #Statistics Testing
  a <- table(onlyaian[,c(24)])
  b <- table(aiancombo[,c(24)])
  healthcare <- rbind(a,b)
  rownames(healthcare) <- c("AI/AN alone", "AI/AN in combination")
  healthcare
  chisq.test(healthcare)
  fisher.test(healthcare)

  # CI for ANAMS population - IHS
  n <- 39
  p <- 10/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
  # CI for ANAMS population - non-IHS tribal healthcare
  n <- 39
  p <- 3/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
  # CI for ANAMS population - other access
  n <- 39
  p <- 26/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
#Q9 Degrees to be obtained
table(stringdata[c(3:46),c(26)])
pie(table(stringdata[c(3:46),c(26)]),main = stringdata[c(1),c(26)])
freq(stringdata[,c(26)])

freq(onlyaian[,c(26)])
freq(aiancombo[,c(26)])
freq(onlynativehc[,c(26)])
freq(nativecommunity[,c(26)])


  #Statistics Testing
  a <- table(onlyaian[,c(26)])
  b <- table(aiancombo[,c(26)])
  degree <- rbind(a,b)
  degree[2,4] <- 0 # Manually set number of MD/Ph.D. from AI/AN in combo to zero, which is the result
  rownames(degree) <- c("AI/AN alone", "AI/AN in combination")
  degree
  #chisq.test(degree)
  fisher.test(degree)
  
  # CI for ANAMS population - MD
  n <- 39
  p <- 25/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
  # CI for ANAMS population - DO
  n <- 39
  p <- 3/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
  # CI for ANAMS population - MD + Masters
  n <- 39
  p <- 6/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
  # CI for ANAMS population - MD + PhD
  n <- 39
  p <- 5/39
  margin <- qnorm(0.975)*sqrt(p*(1-p)/n)
  lowerinterval <- p - margin
  upperinterval <- p + margin
  lowerinterval
  upperinterval
  
   
#Q11 - Med school tribal rotations available... multiple responses allowed - Q8 on qualtrics
table(stringdata[,c(28)])
freq(stringdata[,c(28)])
  #Graph not used in paper
  #Want to see the number of people who selected each individual option
  dat11 <- ANAMSdata[,28]
  dat11 <- gsub(",", "", dat11)
  num.ans1 <- gsub("2", "", dat11)
    num.ans1 <- gsub("3", "", num.ans1)
  num.ans1 <- gsub("4", "", num.ans1)
  num.ans1 <- sum(as.numeric(subset(num.ans1, !is.na(as.numeric(num.ans1)))))
    num.ans1
  num.ans2 <- gsub("1", "", dat11)
    num.ans2 <- gsub("3", "", num.ans2)
    num.ans2 <- gsub("4", "", num.ans2)
    num.ans2 <- sum(as.numeric(subset(num.ans2, !is.na(as.numeric(num.ans2)))))/2
    num.ans2
  num.ans3 <- gsub("1", "", dat11)
    num.ans3 <- gsub("2", "", num.ans3)
    num.ans3 <- gsub("4", "", num.ans3)
    num.ans3 <- sum(as.numeric(subset(num.ans3, !is.na(as.numeric(num.ans3)))))/3
    num.ans3
  num.ans4 <- gsub("1", "", dat11)
    num.ans4 <- gsub("2", "", num.ans4)
    num.ans4 <- gsub("3", "", num.ans4)
    num.ans4 <- sum(as.numeric(subset(num.ans4, !is.na(as.numeric(num.ans4)))))/4
    num.ans4
  num.ans.q11 <- c(num.ans1, num.ans2, num.ans3, num.ans4)
  pct.ans.q11 <- num.ans.q11 / length(ANAMSdata[,28])
  pct.ans.q11
  #Plot
  bar.names.q11 <- c("Yes - at IHS", 
                  "Yes - at non-IHS Tribal healthcare sites", 
                  "No", 
                  "I am not sure"
                    )
  library(RColorBrewer)
  x11()
  barplot(pct.ans.q11, main = "Does your medical school offer Tribal rotations?", 
        legend.text = bar.names.q11, 
        col = c("#CA0020", "#F4A582", "#92C5DE", "#0571B0"), 
        args.legend = list(x = 'topright', bty = 'n')
)
  
  freq(onlyaian[,c(28)])
  freq(aiancombo[,c(28)])
  freq(onlynativehc[,c(28)])
  freq(nativecommunity[,c(28)])
  
  
  #Statistics Testing
  a <- table(onlyaian[,c(28)])
    a[3] <- a[3] + a[4]    
    a[4] <- a[4] + a[5]
    a <- a[-5]
    a # Note the above is not automated. It combines answers from multiple responses unique to this dataset
      # Note - this also messes up the names. Will force change it back at the end
  b <- table(aiancombo[,c(28)])
    b[3] <- b[3]+b[4]
    b[4] <- b[4] + b[5]
    b <- b[-5]
    b # Note the above is not automated. It combines answers from multiple responses unique to this dataset
      # Note - this also messes up the names. Will reconcile that at the end
  rotationopp <- rbind(a,b)
  rownames(rotationopp) <- c("AI/AN alone", "AI/AN in combination")
  colnames(rotationopp) <- c("I am not sure", "No", "Yes - at IHS", "Yes - at non-IHS tribal sites")
  rotationopp
  chisq.test(rotationopp)
  fisher.test(rotationopp)
  
#Q12 - will you / did you complete a tribal rotation - Q9 on qualtrics
table(stringdata[,c(29)])
freq(stringdata[,c(29)])
  q12 <- table(stringdata[,29])
  View(q12)
  q12 <- q12 / length(stringdata[,29])
  barplot(
    q12,
    main = "Did (or will) you complete\n a Tribal rotation in Medical school",
    ylab = "Percent %",
    ylim = c(0,0.45)
  )
  freq(onlyaian[,c(29)])
  freq(aiancombo[,c(29)])
  freq(onlynativehc[,c(29)])
  freq(nativecommunity[,c(29)])
  
  
  #Chi Squared Testing
  a <- table(onlyaian[,c(29)])
  b <- table(aiancombo[,c(29)])
  trot <- rbind(a,b)
  rownames(trot) <- c("AI/AN alone", "AI/AN in combination")
  trot
  chisq.test(trot)
  fisher.test(trot)
  
#Q13 - Did ability to complete tribal rotation influence where you went to school - Q10 on qualtrics
table(stringdata[,c(30)])
freq(stringdata[,c(30)])
  q13 <- table(stringdata[, 30])
  View(q13)
  q13 <- q13 / length(stringdata[,30])
  barplot(
    q13,
    main = "Did the ability to complete a tribal rotation\n influence where you went to medical school?",
    ylab = "Percent %",
    ylim = c(0,0.45)
  )
  freq(onlyaian[,c(30)])
  freq(aiancombo[,c(30)])
  freq(onlynativehc[,c(30)])
  freq(nativecommunity[,c(30)])
  
  
  #Chi Squared Testing
  a <- table(onlyaian[,c(30)])
  b <- table(aiancombo[,c(30)])
  trotinf <- rbind(a,b)
  rownames(trotinf) <- c("AI/AN alone", "AI/AN in combination")
  trotinf
  chisq.test(trotinf)
  fisher.test(trotinf)
  
#Make a plot that ranks by strong dis -> somewhat dis -> I don't know -> somewhat agree -> strongly agree
  #Set up the data
matrixorderdat <- ANAMSdata[,31:41] 
    matrixorderdat <- onlyaiannum[,31:41] # For analysis of only AI/AN alone, use this
    matrixorderdat <- aiancombonum[,31:41] # For analysis of only AI/AN alone, use this
View(matrixorderdat)

  #Note - 1 = strongly agree, 2 = somewhat agree, 3 = somewhat disagree, 4 = strongly disagree, 5 = I don't know
q14 <- as.numeric(matrixorderdat$Q12_1)
  q14 <- subset(q14, !is.na(q14))
  q14
  table(q14)
q14strongagree <- gsub("2", "", q14)
  q14strongagree <- gsub("3", "", q14strongagree)
  q14strongagree <- gsub("4", "", q14strongagree)
  q14strongagree <- gsub("5", "", q14strongagree)
  q14strongagree <-sum(as.numeric(subset(q14strongagree, !is.na(as.numeric(q14strongagree)))))
  q14strongagree
q14somewhatagree <- gsub("1", "", q14)
  q14somewhatagree <- gsub("3", "", q14somewhatagree)
  q14somewhatagree <- gsub("4", "", q14somewhatagree)
  q14somewhatagree <- gsub("5", "", q14somewhatagree)
  q14somewhatagree <-sum(as.numeric(subset(q14somewhatagree, !is.na(as.numeric(q14somewhatagree)))))/2
  q14somewhatagree
q14somewhatdisagree <- gsub("1", "", q14)
  q14somewhatdisagree <- gsub("2", "", q14somewhatdisagree)
  q14somewhatdisagree <- gsub("4", "", q14somewhatdisagree)
  q14somewhatdisagree <- gsub("5", "", q14somewhatdisagree)
  q14somewhatdisagree <-sum(as.numeric(subset(q14somewhatdisagree, !is.na(as.numeric(q14somewhatdisagree)))))/3
  q14somewhatdisagree
q14strongdisagree <- gsub("1", "", q14)
  q14strongdisagree <- gsub("2", "", q14strongdisagree)
  q14strongdisagree <- gsub("3", "", q14strongdisagree)
  q14strongdisagree <- gsub("5", "", q14strongdisagree)
  q14strongdisagree <-sum(as.numeric(subset(q14strongdisagree, !is.na(as.numeric(q14strongdisagree)))))/4
  q14strongdisagree
q14idk <- gsub("1", "", q14)
  q14idk <- gsub("2", "", q14idk)
  q14idk <- gsub("3", "", q14idk)
  q14idk <- gsub("4", "", q14idk)
  q14idk <-sum(as.numeric(subset(q14idk, !is.na(as.numeric(q14idk)))))/5
  q14idk
q15 <- as.numeric(matrixorderdat$Q12_2)
  q15 <- subset(q15, !is.na(q15))
  q15
  table(q15)
q15strongagree <- gsub("2", "", q15)
  q15strongagree <- gsub("3", "", q15strongagree)
  q15strongagree <- gsub("4", "", q15strongagree)
  q15strongagree <- gsub("5", "", q15strongagree)
  q15strongagree <-sum(as.numeric(subset(q15strongagree, !is.na(as.numeric(q15strongagree)))))
  q15strongagree
q15somewhatagree <- gsub("1", "", q15)
  q15somewhatagree <- gsub("3", "", q15somewhatagree)
  q15somewhatagree <- gsub("4", "", q15somewhatagree)
  q15somewhatagree <- gsub("5", "", q15somewhatagree)
  q15somewhatagree <-sum(as.numeric(subset(q15somewhatagree, !is.na(as.numeric(q15somewhatagree)))))/2
  q15somewhatagree
q15somewhatdisagree <- gsub("1", "", q15)
  q15somewhatdisagree <- gsub("2", "", q15somewhatdisagree)
  q15somewhatdisagree <- gsub("4", "", q15somewhatdisagree)
  q15somewhatdisagree <- gsub("5", "", q15somewhatdisagree)
  q15somewhatdisagree <-sum(as.numeric(subset(q15somewhatdisagree, !is.na(as.numeric(q15somewhatdisagree)))))/3
  q15somewhatdisagree
q15strongdisagree <- gsub("1", "", q15)
  q15strongdisagree <- gsub("2", "", q15strongdisagree)
  q15strongdisagree <- gsub("3", "", q15strongdisagree)
  q15strongdisagree <- gsub("5", "", q15strongdisagree)
  q15strongdisagree <-sum(as.numeric(subset(q15strongdisagree, !is.na(as.numeric(q15strongdisagree)))))/4
  q15strongdisagree
q15idk <- gsub("1", "", q15)
  q15idk <- gsub("2", "", q15idk)
  q15idk <- gsub("3", "", q15idk)
  q15idk <- gsub("4", "", q15idk)
  q15idk <-sum(as.numeric(subset(q15idk, !is.na(as.numeric(q15idk)))))/5
  q15idk  
q16 <- as.numeric(matrixorderdat$Q12_3)
  q16 <- subset(q16, !is.na(q16))
  q16
  table(q16)
q16strongagree <- gsub("2", "", q16)
  q16strongagree <- gsub("3", "", q16strongagree)
  q16strongagree <- gsub("4", "", q16strongagree)
  q16strongagree <- gsub("5", "", q16strongagree)
  q16strongagree <-sum(as.numeric(subset(q16strongagree, !is.na(as.numeric(q16strongagree)))))
  q16strongagree
q16somewhatagree <- gsub("1", "", q16)
  q16somewhatagree <- gsub("3", "", q16somewhatagree)
  q16somewhatagree <- gsub("4", "", q16somewhatagree)
  q16somewhatagree <- gsub("5", "", q16somewhatagree)
  q16somewhatagree <-sum(as.numeric(subset(q16somewhatagree, !is.na(as.numeric(q16somewhatagree)))))/2
  q16somewhatagree
q16somewhatdisagree <- gsub("1", "", q16)
  q16somewhatdisagree <- gsub("2", "", q16somewhatdisagree)
  q16somewhatdisagree <- gsub("4", "", q16somewhatdisagree)
  q16somewhatdisagree <- gsub("5", "", q16somewhatdisagree)
  q16somewhatdisagree <-sum(as.numeric(subset(q16somewhatdisagree, !is.na(as.numeric(q16somewhatdisagree)))))/3
  q16somewhatdisagree
q16strongdisagree <- gsub("1", "", q16)
  q16strongdisagree <- gsub("2", "", q16strongdisagree)
  q16strongdisagree <- gsub("3", "", q16strongdisagree)
  q16strongdisagree <- gsub("5", "", q16strongdisagree)
  q16strongdisagree <-sum(as.numeric(subset(q16strongdisagree, !is.na(as.numeric(q16strongdisagree)))))/4
  q16strongdisagree
q16idk <- gsub("1", "", q16)
  q16idk <- gsub("2", "", q16idk)
  q16idk <- gsub("3", "", q16idk)
  q16idk <- gsub("4", "", q16idk)
  q16idk <-sum(as.numeric(subset(q16idk, !is.na(as.numeric(q16idk)))))/5
  q16idk    
q17 <- as.numeric(matrixorderdat$Q12_4)
  q17 <- subset(q17, !is.na(q17))
  q17
  table(q17)
q17strongagree <- gsub("2", "", q17)
  q17strongagree <- gsub("3", "", q17strongagree)
  q17strongagree <- gsub("4", "", q17strongagree)
  q17strongagree <- gsub("5", "", q17strongagree)
  q17strongagree <-sum(as.numeric(subset(q17strongagree, !is.na(as.numeric(q17strongagree)))))
  q17strongagree
q17somewhatagree <- gsub("1", "", q17)
  q17somewhatagree <- gsub("3", "", q17somewhatagree)
  q17somewhatagree <- gsub("4", "", q17somewhatagree)
  q17somewhatagree <- gsub("5", "", q17somewhatagree)
  q17somewhatagree <-sum(as.numeric(subset(q17somewhatagree, !is.na(as.numeric(q17somewhatagree)))))/2
  q17somewhatagree
q17somewhatdisagree <- gsub("1", "", q17)
  q17somewhatdisagree <- gsub("2", "", q17somewhatdisagree)
  q17somewhatdisagree <- gsub("4", "", q17somewhatdisagree)
  q17somewhatdisagree <- gsub("5", "", q17somewhatdisagree)
  q17somewhatdisagree <-sum(as.numeric(subset(q17somewhatdisagree, !is.na(as.numeric(q17somewhatdisagree)))))/3
  q17somewhatdisagree
q17strongdisagree <- gsub("1", "", q17)
  q17strongdisagree <- gsub("2", "", q17strongdisagree)
  q17strongdisagree <- gsub("3", "", q17strongdisagree)
  q17strongdisagree <- gsub("5", "", q17strongdisagree)
  q17strongdisagree <-sum(as.numeric(subset(q17strongdisagree, !is.na(as.numeric(q17strongdisagree)))))/4
  q17strongdisagree
q17idk <- gsub("1", "", q17)
  q17idk <- gsub("2", "", q17idk)
  q17idk <- gsub("3", "", q17idk)
  q17idk <- gsub("4", "", q17idk)
  q17idk <-sum(as.numeric(subset(q17idk, !is.na(as.numeric(q17idk)))))/5
  q17idk   
q18 <- as.numeric(matrixorderdat$Q12_5)
  q18 <- subset(q18, !is.na(q18))
  q18
  table(q18)
q18strongagree <- gsub("2", "", q18)
  q18strongagree <- gsub("3", "", q18strongagree)
  q18strongagree <- gsub("4", "", q18strongagree)
  q18strongagree <- gsub("5", "", q18strongagree)
  q18strongagree <-sum(as.numeric(subset(q18strongagree, !is.na(as.numeric(q18strongagree)))))
  q18strongagree
q18somewhatagree <- gsub("1", "", q18)
  q18somewhatagree <- gsub("3", "", q18somewhatagree)
  q18somewhatagree <- gsub("4", "", q18somewhatagree)
  q18somewhatagree <- gsub("5", "", q18somewhatagree)
  q18somewhatagree <-sum(as.numeric(subset(q18somewhatagree, !is.na(as.numeric(q18somewhatagree)))))/2
  q18somewhatagree
q18somewhatdisagree <- gsub("1", "", q18)
  q18somewhatdisagree <- gsub("2", "", q18somewhatdisagree)
  q18somewhatdisagree <- gsub("4", "", q18somewhatdisagree)
  q18somewhatdisagree <- gsub("5", "", q18somewhatdisagree)
  q18somewhatdisagree <-sum(as.numeric(subset(q18somewhatdisagree, !is.na(as.numeric(q18somewhatdisagree)))))/3
  q18somewhatdisagree
q18strongdisagree <- gsub("1", "", q18)
  q18strongdisagree <- gsub("2", "", q18strongdisagree)
  q18strongdisagree <- gsub("3", "", q18strongdisagree)
  q18strongdisagree <- gsub("5", "", q18strongdisagree)
  q18strongdisagree <-sum(as.numeric(subset(q18strongdisagree, !is.na(as.numeric(q18strongdisagree)))))/4
  q18strongdisagree
q18idk <- gsub("1", "", q18)
  q18idk <- gsub("2", "", q18idk)
  q18idk <- gsub("3", "", q18idk)
  q18idk <- gsub("4", "", q18idk)
  q18idk <-sum(as.numeric(subset(q18idk, !is.na(as.numeric(q18idk)))))/5
  q18idk    
q19 <- as.numeric(matrixorderdat$Q12_6)
  q19 <- subset(q19, !is.na(q19))
  q19
  table(q19)
q19strongagree <- gsub("2", "", q19)
  q19strongagree <- gsub("3", "", q19strongagree)
  q19strongagree <- gsub("4", "", q19strongagree)
  q19strongagree <- gsub("5", "", q19strongagree)
  q19strongagree <-sum(as.numeric(subset(q19strongagree, !is.na(as.numeric(q19strongagree)))))
  q19strongagree
q19somewhatagree <- gsub("1", "", q19)
  q19somewhatagree <- gsub("3", "", q19somewhatagree)
  q19somewhatagree <- gsub("4", "", q19somewhatagree)
  q19somewhatagree <- gsub("5", "", q19somewhatagree)
  q19somewhatagree <-sum(as.numeric(subset(q19somewhatagree, !is.na(as.numeric(q19somewhatagree)))))/2
  q19somewhatagree
q19somewhatdisagree <- gsub("1", "", q19)
  q19somewhatdisagree <- gsub("2", "", q19somewhatdisagree)
  q19somewhatdisagree <- gsub("4", "", q19somewhatdisagree)
  q19somewhatdisagree <- gsub("5", "", q19somewhatdisagree)
  q19somewhatdisagree <-sum(as.numeric(subset(q19somewhatdisagree, !is.na(as.numeric(q19somewhatdisagree)))))/3
  q19somewhatdisagree
q19strongdisagree <- gsub("1", "", q19)
  q19strongdisagree <- gsub("2", "", q19strongdisagree)
  q19strongdisagree <- gsub("3", "", q19strongdisagree)
  q19strongdisagree <- gsub("5", "", q19strongdisagree)
  q19strongdisagree <-sum(as.numeric(subset(q19strongdisagree, !is.na(as.numeric(q19strongdisagree)))))/4
  q19strongdisagree
q19idk <- gsub("1", "", q19)
  q19idk <- gsub("2", "", q19idk)
  q19idk <- gsub("3", "", q19idk)
  q19idk <- gsub("4", "", q19idk)
  q19idk <-sum(as.numeric(subset(q19idk, !is.na(as.numeric(q19idk)))))/5
  q19idk  
q20 <- as.numeric(matrixorderdat$Q12_7)
  q20 <- subset(q20, !is.na(q20))
  q20
  table(q20)
q20strongagree <- gsub("2", "", q20)
  q20strongagree <- gsub("3", "", q20strongagree)
  q20strongagree <- gsub("4", "", q20strongagree)
  q20strongagree <- gsub("5", "", q20strongagree)
  q20strongagree <-sum(as.numeric(subset(q20strongagree, !is.na(as.numeric(q20strongagree)))))
  q20strongagree
q20somewhatagree <- gsub("1", "", q20)
  q20somewhatagree <- gsub("3", "", q20somewhatagree)
  q20somewhatagree <- gsub("4", "", q20somewhatagree)
  q20somewhatagree <- gsub("5", "", q20somewhatagree)
  q20somewhatagree <-sum(as.numeric(subset(q20somewhatagree, !is.na(as.numeric(q20somewhatagree)))))/2
  q20somewhatagree
q20somewhatdisagree <- gsub("1", "", q20)
  q20somewhatdisagree <- gsub("2", "", q20somewhatdisagree)
  q20somewhatdisagree <- gsub("4", "", q20somewhatdisagree)
  q20somewhatdisagree <- gsub("5", "", q20somewhatdisagree)
  q20somewhatdisagree <-sum(as.numeric(subset(q20somewhatdisagree, !is.na(as.numeric(q20somewhatdisagree)))))/3
  q20somewhatdisagree
q20strongdisagree <- gsub("1", "", q20)
  q20strongdisagree <- gsub("2", "", q20strongdisagree)
  q20strongdisagree <- gsub("3", "", q20strongdisagree)
  q20strongdisagree <- gsub("5", "", q20strongdisagree)
  q20strongdisagree <-sum(as.numeric(subset(q20strongdisagree, !is.na(as.numeric(q20strongdisagree)))))/4
  q20strongdisagree
q20idk <- gsub("1", "", q20)
  q20idk <- gsub("2", "", q20idk)
  q20idk <- gsub("3", "", q20idk)
  q20idk <- gsub("4", "", q20idk)
  q20idk <-sum(as.numeric(subset(q20idk, !is.na(as.numeric(q20idk)))))/5
  q20idk
q21 <- as.numeric(matrixorderdat$Q12_8)
  q21 <- subset(q21, !is.na(q21))
  q21
  table(q21)
q21strongagree <- gsub("2", "", q21)
  q21strongagree <- gsub("3", "", q21strongagree)
  q21strongagree <- gsub("4", "", q21strongagree)
  q21strongagree <- gsub("5", "", q21strongagree)
  q21strongagree <-sum(as.numeric(subset(q21strongagree, !is.na(as.numeric(q21strongagree)))))
  q21strongagree
q21somewhatagree <- gsub("1", "", q21)
  q21somewhatagree <- gsub("3", "", q21somewhatagree)
  q21somewhatagree <- gsub("4", "", q21somewhatagree)
  q21somewhatagree <- gsub("5", "", q21somewhatagree)
  q21somewhatagree <-sum(as.numeric(subset(q21somewhatagree, !is.na(as.numeric(q21somewhatagree)))))/2
  q21somewhatagree
q21somewhatdisagree <- gsub("1", "", q21)
  q21somewhatdisagree <- gsub("2", "", q21somewhatdisagree)
  q21somewhatdisagree <- gsub("4", "", q21somewhatdisagree)
  q21somewhatdisagree <- gsub("5", "", q21somewhatdisagree)
  q21somewhatdisagree <-sum(as.numeric(subset(q21somewhatdisagree, !is.na(as.numeric(q21somewhatdisagree)))))/3
  q21somewhatdisagree
q21strongdisagree <- gsub("1", "", q21)
  q21strongdisagree <- gsub("2", "", q21strongdisagree)
  q21strongdisagree <- gsub("3", "", q21strongdisagree)
  q21strongdisagree <- gsub("5", "", q21strongdisagree)
  q21strongdisagree <-sum(as.numeric(subset(q21strongdisagree, !is.na(as.numeric(q21strongdisagree)))))/4
  q21strongdisagree
q21idk <- gsub("1", "", q21)
  q21idk <- gsub("2", "", q21idk)
  q21idk <- gsub("3", "", q21idk)
  q21idk <- gsub("4", "", q21idk)
  q21idk <-sum(as.numeric(subset(q21idk, !is.na(as.numeric(q21idk)))))/5
  q21idk
q22 <- as.numeric(matrixorderdat$Q12_9)
  q22 <- subset(q22, !is.na(q22))
  q22
  table(q22)
q22strongagree <- gsub("2", "", q22)
  q22strongagree <- gsub("3", "", q22strongagree)
  q22strongagree <- gsub("4", "", q22strongagree)
  q22strongagree <- gsub("5", "", q22strongagree)
  q22strongagree <-sum(as.numeric(subset(q22strongagree, !is.na(as.numeric(q22strongagree)))))
  q22strongagree
q22somewhatagree <- gsub("1", "", q22)
  q22somewhatagree <- gsub("3", "", q22somewhatagree)
  q22somewhatagree <- gsub("4", "", q22somewhatagree)
  q22somewhatagree <- gsub("5", "", q22somewhatagree)
  q22somewhatagree <-sum(as.numeric(subset(q22somewhatagree, !is.na(as.numeric(q22somewhatagree)))))/2
  q22somewhatagree
q22somewhatdisagree <- gsub("1", "", q22)
  q22somewhatdisagree <- gsub("2", "", q22somewhatdisagree)
  q22somewhatdisagree <- gsub("4", "", q22somewhatdisagree)
  q22somewhatdisagree <- gsub("5", "", q22somewhatdisagree)
  q22somewhatdisagree <-sum(as.numeric(subset(q22somewhatdisagree, !is.na(as.numeric(q22somewhatdisagree)))))/3
  q22somewhatdisagree
q22strongdisagree <- gsub("1", "", q22)
  q22strongdisagree <- gsub("2", "", q22strongdisagree)
  q22strongdisagree <- gsub("3", "", q22strongdisagree)
  q22strongdisagree <- gsub("5", "", q22strongdisagree)
  q22strongdisagree <-sum(as.numeric(subset(q22strongdisagree, !is.na(as.numeric(q22strongdisagree)))))/4
  q22strongdisagree
q22idk <- gsub("1", "", q22)
  q22idk <- gsub("2", "", q22idk)
  q22idk <- gsub("3", "", q22idk)
  q22idk <- gsub("4", "", q22idk)
  q22idk <-sum(as.numeric(subset(q22idk, !is.na(as.numeric(q22idk)))))/5
  q22idk  
q23 <- as.numeric(matrixorderdat$Q12_10)
  q23 <- subset(q23, !is.na(q23))
  q23
  table(q23)
q23strongagree <- gsub("2", "", q23)
  q23strongagree <- gsub("3", "", q23strongagree)
  q23strongagree <- gsub("4", "", q23strongagree)
  q23strongagree <- gsub("5", "", q23strongagree)
  q23strongagree <-sum(as.numeric(subset(q23strongagree, !is.na(as.numeric(q23strongagree)))))
  q23strongagree
q23somewhatagree <- gsub("1", "", q23)
  q23somewhatagree <- gsub("3", "", q23somewhatagree)
  q23somewhatagree <- gsub("4", "", q23somewhatagree)
  q23somewhatagree <- gsub("5", "", q23somewhatagree)
  q23somewhatagree <-sum(as.numeric(subset(q23somewhatagree, !is.na(as.numeric(q23somewhatagree)))))/2
  q23somewhatagree
q23somewhatdisagree <- gsub("1", "", q23)
  q23somewhatdisagree <- gsub("2", "", q23somewhatdisagree)
  q23somewhatdisagree <- gsub("4", "", q23somewhatdisagree)
  q23somewhatdisagree <- gsub("5", "", q23somewhatdisagree)
  q23somewhatdisagree <-sum(as.numeric(subset(q23somewhatdisagree, !is.na(as.numeric(q23somewhatdisagree)))))/3
  q23somewhatdisagree
q23strongdisagree <- gsub("1", "", q23)
  q23strongdisagree <- gsub("2", "", q23strongdisagree)
  q23strongdisagree <- gsub("3", "", q23strongdisagree)
  q23strongdisagree <- gsub("5", "", q23strongdisagree)
  q23strongdisagree <-sum(as.numeric(subset(q23strongdisagree, !is.na(as.numeric(q23strongdisagree)))))/4
  q23strongdisagree
q23idk <- gsub("1", "", q23)
  q23idk <- gsub("2", "", q23idk)
  q23idk <- gsub("3", "", q23idk)
  q23idk <- gsub("4", "", q23idk)
  q23idk <-sum(as.numeric(subset(q23idk, !is.na(as.numeric(q23idk)))))/5
  q23idk
q24 <- as.numeric(matrixorderdat$Q12_11)
  q24 <- subset(q24, !is.na(q24))
  q24
  table(q24)
q24strongagree <- gsub("2", "", q24)
  q24strongagree <- gsub("3", "", q24strongagree)
  q24strongagree <- gsub("4", "", q24strongagree)
  q24strongagree <- gsub("5", "", q24strongagree)
  q24strongagree <-sum(as.numeric(subset(q24strongagree, !is.na(as.numeric(q24strongagree)))))
  q24strongagree
q24somewhatagree <- gsub("1", "", q24)
  q24somewhatagree <- gsub("3", "", q24somewhatagree)
  q24somewhatagree <- gsub("4", "", q24somewhatagree)
  q24somewhatagree <- gsub("5", "", q24somewhatagree)
  q24somewhatagree <-sum(as.numeric(subset(q24somewhatagree, !is.na(as.numeric(q24somewhatagree)))))/2
  q24somewhatagree
q24somewhatdisagree <- gsub("1", "", q24)
  q24somewhatdisagree <- gsub("2", "", q24somewhatdisagree)
  q24somewhatdisagree <- gsub("4", "", q24somewhatdisagree)
  q24somewhatdisagree <- gsub("5", "", q24somewhatdisagree)
  q24somewhatdisagree <-sum(as.numeric(subset(q24somewhatdisagree, !is.na(as.numeric(q24somewhatdisagree)))))/3
  q24somewhatdisagree
q24strongdisagree <- gsub("1", "", q24)
  q24strongdisagree <- gsub("2", "", q24strongdisagree)
  q24strongdisagree <- gsub("3", "", q24strongdisagree)
  q24strongdisagree <- gsub("5", "", q24strongdisagree)
  q24strongdisagree <-sum(as.numeric(subset(q24strongdisagree, !is.na(as.numeric(q24strongdisagree)))))/4
  q24strongdisagree
q24idk <- gsub("1", "", q24)
  q24idk <- gsub("2", "", q24idk)
  q24idk <- gsub("3", "", q24idk)
  q24idk <- gsub("4", "", q24idk)
  q24idk <-sum(as.numeric(subset(q24idk, !is.na(as.numeric(q24idk)))))/5
  q24idk

#Set vectors of answers
strongagree <- c(q14strongagree, q15strongagree, q16strongagree, q17strongagree, 
                 q18strongagree, q19strongagree, q20strongagree, q21strongagree, 
                 q22strongagree, q23strongagree, q24strongagree)
somewhatagree <- c(q14somewhatagree, q15somewhatagree, q16somewhatagree, q17somewhatagree,
                   q18somewhatagree, q19somewhatagree, q20somewhatagree, q21somewhatagree,
                   q22somewhatagree, q23somewhatagree, q24somewhatagree)
somewhatdisagree <- c(q14somewhatdisagree, q15somewhatdisagree, q16somewhatdisagree, q17somewhatdisagree,
                      q18somewhatdisagree, q19somewhatdisagree, q20somewhatdisagree, q21somewhatdisagree,
                      q22somewhatdisagree, q23somewhatdisagree, q24somewhatdisagree)
strongdisagree <- c(q14strongdisagree, q15strongdisagree, q16strongdisagree, q17strongdisagree,
                    q18strongdisagree, q19strongdisagree, q20strongdisagree, q21strongdisagree,
                    q22strongdisagree, q23strongdisagree, q24strongdisagree)
idk <- c(q14idk, q15idk, q16idk, q17idk,
         q18idk, q19idk, q20idk, q21idk,
         q22idk, q23idk, q24idk)
  
graphorderdat <- rbind(strongagree, somewhatagree, idk,somewhatdisagree, strongdisagree)

barplot(graphorderdat)
View(graphorderdat)

#Change order so Strongly Agree + Somewhat Agree is largest
descendingorder <- order(graphorderdat[1,] + graphorderdat[2,], decreasing = FALSE)
descendingorder
  # IF you change order only to strongly agree
    #descendingorder <- order(graphorderdat[1, ], decreasing = FALSE) 
    #descendingorder
matrixplot <- graphorderdat[,descendingorder[1:11]] # New matrix of data based on decreasing order of question 1
matrixplot

#Make from number to proportion
denominator <- length(matrixorderdat[, 1])
denominator
matrixplot <- matrixplot / denominator * 100 # Change from n to percent
matrixplot

#Parameters for graph
legend.text = c("Strongly Agree",
                "Somewhat Agree",
                "I Don't Know", 
                "Somewhat Disagree",
                "Strongly Disagree"
                )
                


#Computer generated graph from R - \n
qnames <- c(
  "I feel a sense of community with Native peers at my institution", #OK
  "I am satisfied with my school's effort to recruit Native students", #OK
  "There is adequate scholarship funding at my school for Native students", #OK
  "I am satisfied with Native faculty representation\n and mentorship at my school",
  "I am satisfied with my school's engagement with Native faculty and\nexperts to contribute to lectures and learning opportunities",
  "I am satisfied with the amount of cultural competency training\nas it relates to AI-AN health care delivery at my institution",
  "I am satisfied with my school's education and training\n that exposes students to Native communities and health",
  "I am satisfied with the amount of education of\n AI-AN health disparities at my institution",
  "I am satisfied with the opportunities provided by my\ninstitution to do clinical electives in tribal communities",
  "I am satisfied with my school's outreach to Native organizations",
  "I am satisfied with my school's outreach to Native communities"
)


  
  
qnames <- qnames[descendingorder[1:11]]
colnames(matrixplot) <- qnames
View(matrixplot)

##### Playing around with stuff
  matrixplot <- matrixplot * denominator / 100
  View(matrixplot)
  justify(qnames)

      # Bigger sized graph \n
      qnames <- c(
        "I feel a sense of community with\n Native peers at my institution",
        "I am satisfied with my school's effort\n to recruit Native students",
        "There is adequate scholarship funding\n at my school for Native students",
        "I am satisfied with Native faculty \n representation and mentorship at my school",
        "I am satisfied with my school's engagement with Native faculty\and experts to contribute to lectures and learning opportunities",
        "I am satisfied with the amount of cultural competency training \n as it relates to AI-AN health care\n delivery at my institution",
        "I am satisfied with my school's education and training \n that exposes students to Native communities and health",
        "I am satisfied with the amount of education\n of AI-AN health disparities at my institution",
        "I am satisfied with the opportunities provided by my\n institution to do clinical electives in tribal communities",
        "I am satisfied with my school's\n outreach to Native organizations",
        "I am satisfied with my school's\n outreach to Native communities"
                  )
      
      # No spacing built in
      qnames <- c(
        "I feel a sense of community with Native peers at my institution",
        "I am satisfied with my school's effort to recruit Native students",
        "There is adequate scholarship funding at my school for Native students",
        "I am satisfied with Native faculty representation and mentorship at my school",
        "I am satisfied with my school's engagement with Native faculty and experts to contribute to lectures and learning opportunities",
        "I am satisfied with the amount of cultural competency training as it relates to AI-AN health care delivery at my institution",
        "I am satisfied with my school's education and training that exposes students to Native communities and health",
        "I am satisfied with the amount of education of AI-AN health disparities at my institution",
        "I am satisfied with the opportunities provided by my institution to do clinical electives in tribal communities",
        "I am satisfied with my school's outreach to Native organizations",
        "I am satisfied with my school's outreach to Native communities"
      )
      
      
      
      
      #Testing stuff
      qnames <- c(
        "I feel a sense of community with Native peers at my institution", #OK
        "I am satisfied with my school's effort to recruit Native students", #OK
        "There is adequate scholarship funding at my school for Native students", #OK
        "I am satisfied with Native faculty representation\r\nand mentorship at my school",
        "I am satisfied with my school's engagement with Native faculty and\rexperts to contribute to lectures and learning opportunities",
        "I am satisfied with the amount of cultural competency training\ras it relates to AI-AN health care delivery at my institution",
        "I am satisfied with my school's education and training\r that exposes students to Native communities and health",
        "I am satisfied with the amount of education of AI-AN health\rdisparities at my institution",
        "I am satisfied with the opportunities provided by my\rinstitution to do clinical electives in tribal communities",
        "I am satisfied with my school's outreach to Native organizations",
        "I am satisfied with my school's outreach to Native communities"
      )

##### stuff that I am working on to justify margins
    a <- ("I am satisfied with the opportunities provided by my\n institution to do clinical electives in tribal communities")
    a
    a <- c("I am satisfied with the opportunities provided by my", "institution to do clinical electives in tribal communities")
    print(a)
    cat(a)

    dim(qnames)
    str(qnames)
    library(stringi)
    a <- stri_pad_left(qnames)
    b <- stri_pad_right(qnames)
    c <- stri_pad_both(qnames)
    d <- stri_wrap(str = qnames,cost_exponent = 2  )
    d
    
    justify(qnames,)
    
qnames <- text(qnames, pos = 2)

testnames <- text(x = -6,
                  adj = c(0,0),
                  labels = qnames,
                  pos = 4
                  )

testnames
View(testnames)


#### End playing, run below code

setEPS()
postscript("Figure 1.eps")

colorscale <- colorspace::diverge_hcl(5, palette = "Blue-Red")
colorscale <- colorspace::sequential_hcl(5, palette = "Grays")
x11()
par(opar) # Reset par
opar = par(oma = c(4,16,0,0)) #makes margins bigger -> c(b,l,t,r) -> bottom, left, top, right
barplot(matrixplot, 
        horiz = TRUE,
        beside = F, 
        #main = c("Answer the following questions about your medical school:"), 
        xlim = c(0,100),
        xlab = "Percent of respondents %",
        #names.arg = justify(qnames, type = "left"),
        #names.arg = strwrap(qnames,width = 30),
        #names.arg = testnames,
        #names.arg = qnames,
        names.arg = StrAlign(qnames, sep = "\\r"),
        #names.arg = text(qnames, adj = c(0,0)),
        cex.names = 0.7,  #Decrease font size of question labels
        cex.axis = 1,
        #cex.main = 2,
        cex.lab = 1,
        las = 1,   #Makes all text vertical
        col = colorscale,
        space = 0.66
)



par(opar) # Reset par
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x ="bottomleft", legend = legend.text, 
       fill = colorscale, 
       bty = 'n', ncol = 1, xjust = 1, yjust = 1.5, inset = -0.05, cex = 1.)
par(opar) # Reset par
dev.off()

#Q25 Ranking challenges
datrank <- ANAMSdata[43:51]
      datrank <- onlyaiannum[43:51] # For evaluating only ai/an data
      datrank <- aiancombonum[43:51] # For evaluating only AI/AN in combo with another race
View(datrank)
#Set up matrices for my data, one question at a time
q1 <- as.numeric(datrank$Q14_0_1_RANK)
q1 <- subset(q1, !is.na(q1))
q1
freq(q1)
q1total <- gsub("2", "1", q1)
  q1total <- gsub("3", "1", q1total)
  q1total <- sum(as.numeric(q1total))
  q1total
q1rank1 <- gsub("2", "", q1)
  q1rank1 <- gsub("3", "", q1rank1)
  q1rank1 <-sum(as.numeric(subset(q1rank1, !is.na(as.numeric(q1rank1)))))
  q1rank1
q1rank2 <- gsub("1", "", q1)
  q1rank2 <- gsub("3", "", q1rank2)
  q1rank2 <-sum(as.numeric(subset(q1rank2, !is.na(as.numeric(q1rank2)))))/2
  q1rank2
q1rank3 <- gsub("1", "", q1)
  q1rank3 <- gsub("2", "", q1rank3)
  q1rank3 <-sum(as.numeric(subset(q1rank3, !is.na(as.numeric(q1rank3)))))/3
  q1rank3
q2 <- as.numeric(datrank$Q14_0_2_RANK)
q2 <- subset(q2, !is.na(q2))
q2
freq(q2)
q2total <- gsub("2", "1", q2)
  q2total <- gsub("3", "1", q2total)  
  q2total <- sum(as.numeric(q2total))
  q2total
q2rank1 <- gsub("2", "", q2)
  q2rank1 <- gsub("3", "", q2rank1)
  q2rank1 <-sum(as.numeric(subset(q2rank1, !is.na(as.numeric(q2rank1)))))
  q2rank1
q2rank2 <- gsub("1", "", q2)
  q2rank2 <- gsub("3", "", q2rank2)
  q2rank2 <-sum(as.numeric(subset(q2rank2, !is.na(as.numeric(q2rank2)))))/2
  q2rank2
q2rank3 <- gsub("1", "", q2)
  q2rank3 <- gsub("2", "", q2rank3)
  q2rank3 <-sum(as.numeric(subset(q2rank3, !is.na(as.numeric(q2rank3)))))/3
  q2rank3
q3 <- as.numeric(datrank$Q14_0_3_RANK)
q3 <- subset(q3, !is.na(q3))
q3
freq(q3)
q3total <- gsub("2", "1", q3)
  q3total <- gsub("3", "1", q3total)
  q3total <- sum(as.numeric(q3total))
  q3total
q3rank1 <- gsub("2", "", q3)
  q3rank1 <- gsub("3", "", q3rank1)
  q3rank1 <-sum(as.numeric(subset(q3rank1, !is.na(as.numeric(q3rank1)))))
  q3rank1
q3rank2 <- gsub("1", "", q3)
  q3rank2 <- gsub("3", "", q3rank2)
  q3rank2 <-sum(as.numeric(subset(q3rank2, !is.na(as.numeric(q3rank2)))))/2
  q3rank2
q3rank3 <- gsub("1", "", q3)
  q3rank3 <- gsub("2", "", q3rank3)
  q3rank3 <-sum(as.numeric(subset(q3rank3, !is.na(as.numeric(q3rank3)))))/3
  q3rank3
q4 <- as.numeric(datrank$Q14_0_4_RANK)
q4 <- subset(q4, !is.na(q4))
q4
freq(q4)
q4total <- gsub("2", "1", q4)
q4total <- gsub("3", "1", q4total)
  q4total <- sum(as.numeric(q4total))
  q4total
q4rank1 <- gsub("2", "", q4)
  q4rank1 <- gsub("3", "", q4rank1)
  q4rank1 <-sum(as.numeric(subset(q4rank1, !is.na(as.numeric(q4rank1)))))
  q4rank1
q4rank2 <- gsub("1", "", q4)
  q4rank2 <- gsub("3", "", q4rank2)
  q4rank2 <-sum(as.numeric(subset(q4rank2, !is.na(as.numeric(q4rank2)))))/2
  q4rank2
q4rank3 <- gsub("1", "", q4)
  q4rank3 <- gsub("2", "", q4rank3)
  q4rank3 <-sum(as.numeric(subset(q4rank3, !is.na(as.numeric(q4rank3)))))/3
  q4rank3
q5 <- as.numeric(datrank$Q14_0_5_RANK)
q5 <- subset(q5, !is.na(q5))
q5
freq(q5)
q5total <- gsub("2", "1", q5)
  q5total <- gsub("3", "1", q5total)
  q5total <- sum(as.numeric(q5total))
  q5total
q5rank1 <- gsub("2", "", q5)
q5rank1 <- gsub("3", "", q5rank1)
  q5rank1 <-sum(as.numeric(subset(q5rank1, !is.na(as.numeric(q5rank1)))))
  q5rank1
q5rank2 <- gsub("1", "", q5)
  q5rank2 <- gsub("3", "", q5rank2)
  q5rank2 <-sum(as.numeric(subset(q5rank2, !is.na(as.numeric(q5rank2)))))/2
  q5rank2
q5rank3 <- gsub("1", "", q5)
  q5rank3 <- gsub("2", "", q5rank3)
  q5rank3 <-sum(as.numeric(subset(q5rank3, !is.na(as.numeric(q5rank3)))))/3
  q5rank3
q6 <- as.numeric(datrank$Q14_0_6_RANK)
q6 <- subset(q6, !is.na(q6))
q6
freq(q6)
q6total <- gsub("2", "1", q6)
  q6total <- gsub("3", "1", q6total)
  q6total <- sum(as.numeric(q6total))
  q6total
q6rank1 <- gsub("2", "", q6)
  q6rank1 <- gsub("3", "", q6rank1)
  q6rank1 <-sum(as.numeric(subset(q6rank1, !is.na(as.numeric(q6rank1)))))
  q6rank1
q6rank2 <- gsub("1", "", q6)
  q6rank2 <- gsub("3", "", q6rank2)
  q6rank2 <-sum(as.numeric(subset(q6rank2, !is.na(as.numeric(q6rank2)))))/2
  q6rank2
q6rank3 <- gsub("1", "", q6)
  q6rank3 <- gsub("2", "", q6rank3)
  q6rank3 <-sum(as.numeric(subset(q6rank3, !is.na(as.numeric(q6rank3)))))/3
  q6rank3
q7 <- as.numeric(datrank$Q14_0_7_RANK)
q7 <- subset(q7, !is.na(q7))
q7
freq(q7)
q7total <- gsub("2", "1", q7)
  q7total <- gsub("3", "1", q7total)
  q7total <- sum(as.numeric(q7total))
  q7total
q7rank1 <- gsub("2", "", q7)
  q7rank1 <- gsub("3", "", q7rank1)
  q7rank1 <-sum(as.numeric(subset(q7rank1, !is.na(as.numeric(q7rank1)))))
  q7rank1
q7rank2 <- gsub("1", "", q7)
  q7rank2 <- gsub("3", "", q7rank2)
  q7rank2 <-sum(as.numeric(subset(q7rank2, !is.na(as.numeric(q7rank2)))))/2
  q7rank2
q7rank3 <- gsub("1", "", q7)
  q7rank3 <- gsub("2", "", q7rank3)
  q7rank3 <-sum(as.numeric(subset(q7rank3, !is.na(as.numeric(q7rank3)))))/3
  q7rank3
q8 <- as.numeric(datrank$Q14_0_8_RANK)
q8 <- subset(q8, !is.na(q8))
q8
freq(q8)
q8total <- gsub("2", "1", q8)
  q8total <- gsub("3", "1", q8total)
  q8total <- sum(as.numeric(q8total))
  q8total
q8rank1 <- gsub("2", "", q8)
  q8rank1 <- gsub("3", "", q8rank1)
  q8rank1 <-sum(as.numeric(subset(q8rank1, !is.na(as.numeric(q8rank1)))))
  q8rank1
q8rank2 <- gsub("1", "", q8)
  q8rank2 <- gsub("3", "", q8rank2)
  q8rank2 <-sum(as.numeric(subset(q8rank2, !is.na(as.numeric(q8rank2)))))/2
  q8rank2
q8rank3 <- gsub("1", "", q8)
  q8rank3 <- gsub("2", "", q8rank3)
  q8rank3 <-sum(as.numeric(subset(q8rank3, !is.na(as.numeric(q8rank3)))))/3
  q8rank3
q9 <- as.numeric(datrank$Q14_0_9_RANK)
q9 <- subset(q9, !is.na(q9))
q9
freq(q9)
q9total <- gsub("2", "1", q9)
  q9total <- gsub("3", "1", q9total)
  q9total <- sum(as.numeric(q9total))
  q9total
q9rank1 <- gsub("2", "", q9)
  q9rank1 <- gsub("3", "", q9rank1)
  q9rank1 <-sum(as.numeric(subset(q9rank1, !is.na(as.numeric(q9rank1)))))
  q9rank1
q9rank2 <- gsub("1", "", q9)
  q9rank2 <- gsub("3", "", q9rank2)
  q9rank2 <-sum(as.numeric(subset(q9rank2, !is.na(as.numeric(q9rank2)))))/2
  q9rank2
q9rank3 <- gsub("1", "", q9)
  q9rank3 <- gsub("2", "", q9rank3)
  q9rank3 <-sum(as.numeric(subset(q9rank3, !is.na(as.numeric(q9rank3)))))/3
  
#Make vectors for each rank
rank1 = c(q1rank1, q2rank1, q3rank1, q4rank1, q5rank1, q6rank1, q7rank1, q8rank1, q9rank1)
rank2 = c(q1rank2, q2rank2, q3rank2, q4rank2, q5rank2, q6rank2, q7rank2, q8rank2, q9rank2)
rank3 = c(q1rank3, q2rank3, q3rank3, q4rank3, q5rank3, q6rank3, q7rank3, q8rank3, q9rank3)
#Bind them together
challengerankmatrix <- rbind(rank1, rank2, rank3)
challengerankmatrix
#Labels for graph
challengetitle <- c("What challenges have you overcome\n to get to where you are today?")
challengeoptions <- c("Financial", 
             "Family related issues", 
             "Distance from family",
             "Loss of culture", 
             "Imposter syndrome", 
             "Lack of role models",
             "Unfamiliarity with applications",
             "First generation college student",
             "First generation\nmedical student"
              ) # In order of presentation in survey

#Change data from n to a percentage
denominator <- length(na.omit(ANAMSdata[3:nrow(ANAMSdata), 42]))
challengerankmatrix <- rbind(rank1, rank2, rank3) / denominator * 100
#colnames(challengerankmatrix) <- challengeoptions
#View(challengerankmatrix)

#challengerankmatrix <- challengerankmatrix * denominator / 100
#View(challengerankmatrix)

challengerankmatrix

colnames(challengerankmatrix) <- challengeoptions
challengerankmatrix
View(challengerankmatrix)

rank1list <- order(challengerankmatrix[1,], decreasing = TRUE)
rank1list
rank2list <- order(challengerankmatrix[2,], decreasing = TRUE)
rank2list
rank3list <- order(challengerankmatrix[3,], decreasing = TRUE)
rank3list

newvector1 <- challengerankmatrix[1,rank1list[1:3]]
newvector1
newvector2 <- challengerankmatrix[2,rank2list[1:3]]
newvector2
newvector3 <- challengerankmatrix[3,rank3list[1:3]]
newvector3

newtable <- cbind(newvector1, newvector2, newvector3)
newtable
colnames(newtable) <- c("Ranked 1st", "Ranked 2nd", "Ranked 3rd")
newtable

newtesttable <- newtable
newtesttable[1:9] <- c("Financial 24%", "Imposter syndrome 21%", "Family issues 24%",
                       "1st generation med student 24%", "Financial 16%", "Imposter syndrome 21%",
                       "Imposter syndrome 16%", "Lack of role models 16%", "1st Gen med student 22%"
                       )
newtesttable
View(newtesttable)


#Change order so descending by ranked first
  challengedescending <- order(challengerankmatrix[1, ], decreasing = FALSE) # Decrease = FALSE because the barplot function will flip it later
  challengedescending
  challengeplot <- challengerankmatrix[,challengedescending[1:9]]
  challengeplot
  descendingchallengeoptions <- challengeoptions[challengedescending[1:9]]
  descendingchallengeoptions
  
#Use only top 3 choices to merge with alleviating factors matrix later
  testa <- challengeplot[1:3,7:9] # Take the top 3 options, all 3 rows
  testa
  testplotchallengeoptions <- descendingchallengeoptions[7:9]
  testplotchallengeoptions
    
x11()
par(opar) # Reset par
opar = par(oma = c(0,10,0,0)) #makes margins bigger -> c(b,l,t,r) -> bottom, left, top, right
colorscale <- colorspace::sequential_hcl(n = 3, palette = "YlOrRd")
plota <- 
  barplot(
        challengeplot, #Use when making graph of all 9 options for challenges
        #testa, #USe when making graph with top 3 challenges and helpers in one
        beside = F, 
        main = challengetitle, 
        xlim = c(0,100),
        xlab = "Percent of respondents %",
        legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
        args.legend = list(x = 'bottomright', bty = 'n'),
        #names.arg = testplotchallengeoptions, # Use for combined graph
        names.arg = descendingchallengeoptions, # Use for graph of 9 challenges
        cex.main = 1.5,
        cex.names = 0.7,  #Decrease font size of x axis options
        cex.lab = 1.5,
        horiz = TRUE,
        col = colorscale,
        las = 1,   #Makes all text vertical
        space = 0.66
  )
par(opar) # Reset par
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
par(opar) # Reset par


#Q26 Alleviating factors ranking
helpingfactors <- ANAMSdata[,53:61]
  helpingfactors <- onlyaiannum[,53:61] # For analysis of only AI/AN individuals
  helpingfactors <- aiancombonum[,53:61] # For analysis of only AI/AN incombo w/ another race
View(helpingfactors)
class(helpingfactors$Q15_0_1_RANK)

#Set up matrices for my data, one question at a time
q1 <- as.numeric(helpingfactors$Q15_0_1_RANK)
q1 <- subset(q1, !is.na(q1))
q1
freq(q1)
q1total <- gsub("2", "1", q1)
  q1total <- gsub("3", "1", q1total)
  q1total <- sum(as.numeric(q1total))
  q1total
q1rank1 <- gsub("2", "", q1)
  q1rank1 <- gsub("3", "", q1rank1)
  q1rank1 <-sum(as.numeric(subset(q1rank1, !is.na(as.numeric(q1rank1)))))
  q1rank1
q1rank2 <- gsub("1", "", q1)
  q1rank2 <- gsub("3", "", q1rank2)
  q1rank2 <-sum(as.numeric(subset(q1rank2, !is.na(as.numeric(q1rank2)))))/2
  q1rank2
q1rank3 <- gsub("1", "", q1)
  q1rank3 <- gsub("2", "", q1rank3)
  q1rank3 <-sum(as.numeric(subset(q1rank3, !is.na(as.numeric(q1rank3)))))/3
  q1rank3

q2 <- as.numeric(helpingfactors$Q15_0_2_RANK)
q2 <- subset(q2, !is.na(q2))
q2
freq(q2)
q2total <- gsub("2", "1", q2)
  q2total <- gsub("3", "1", q2total)  
  q2total <- sum(as.numeric(q2total))
  q2total
q2rank1 <- gsub("2", "", q2)
  q2rank1 <- gsub("3", "", q2rank1)
  q2rank1 <-sum(as.numeric(subset(q2rank1, !is.na(as.numeric(q2rank1)))))
  q2rank1
q2rank2 <- gsub("1", "", q2)
  q2rank2 <- gsub("3", "", q2rank2)
  q2rank2 <-sum(as.numeric(subset(q2rank2, !is.na(as.numeric(q2rank2)))))/2
  q2rank2
q2rank3 <- gsub("1", "", q2)
  q2rank3 <- gsub("2", "", q2rank3)
  q2rank3 <-sum(as.numeric(subset(q2rank3, !is.na(as.numeric(q2rank3)))))/3
  q2rank3

q3 <- as.numeric(helpingfactors$Q15_0_3_RANK)
q3 <- subset(q3, !is.na(q3))
q3
freq(q3)
q3total <- gsub("2", "1", q3)
  q3total <- gsub("3", "1", q3total)
  q3total <- sum(as.numeric(q3total))
  q3total
q3rank1 <- gsub("2", "", q3)
  q3rank1 <- gsub("3", "", q3rank1)
  q3rank1 <-sum(as.numeric(subset(q3rank1, !is.na(as.numeric(q3rank1)))))
  q3rank1
q3rank2 <- gsub("1", "", q3)
  q3rank2 <- gsub("3", "", q3rank2)
  q3rank2 <-sum(as.numeric(subset(q3rank2, !is.na(as.numeric(q3rank2)))))/2
  q3rank2
q3rank3 <- gsub("1", "", q3)
  q3rank3 <- gsub("2", "", q3rank3)
  q3rank3 <-sum(as.numeric(subset(q3rank3, !is.na(as.numeric(q3rank3)))))/3
  q3rank3

q4 <- as.numeric(helpingfactors$Q15_0_4_RANK)
q4 <- subset(q4, !is.na(q4))
q4
freq(q4)
q4total <- gsub("2", "1", q4)
  q4total <- gsub("3", "1", q4total)
  q4total <- sum(as.numeric(q4total))
  q4total
q4rank1 <- gsub("2", "", q4)
  q4rank1 <- gsub("3", "", q4rank1)
  q4rank1 <-sum(as.numeric(subset(q4rank1, !is.na(as.numeric(q4rank1)))))
  q4rank1
q4rank2 <- gsub("1", "", q4)
  q4rank2 <- gsub("3", "", q4rank2)
  q4rank2 <-sum(as.numeric(subset(q4rank2, !is.na(as.numeric(q4rank2)))))/2
  q4rank2
q4rank3 <- gsub("1", "", q4)
  q4rank3 <- gsub("2", "", q4rank3)
  q4rank3 <-sum(as.numeric(subset(q4rank3, !is.na(as.numeric(q4rank3)))))/3
  q4rank3

q5 <- as.numeric(helpingfactors$Q15_0_5_RANK)
q5 <- subset(q5, !is.na(q5))
q5
freq(q5)
q5total <- gsub("2", "1", q5)
  q5total <- gsub("3", "1", q5total)
  q5total <- sum(as.numeric(q5total))
  q5total
q5rank1 <- gsub("2", "", q5)
  q5rank1 <- gsub("3", "", q5rank1)
  q5rank1 <-sum(as.numeric(subset(q5rank1, !is.na(as.numeric(q5rank1)))))
  q5rank1
q5rank2 <- gsub("1", "", q5)
  q5rank2 <- gsub("3", "", q5rank2)
  q5rank2 <-sum(as.numeric(subset(q5rank2, !is.na(as.numeric(q5rank2)))))/2
  q5rank2
q5rank3 <- gsub("1", "", q5)
  q5rank3 <- gsub("2", "", q5rank3)
  q5rank3 <-sum(as.numeric(subset(q5rank3, !is.na(as.numeric(q5rank3)))))/3
  q5rank3

q6 <- as.numeric(helpingfactors$Q15_0_6_RANK)
q6 <- subset(q6, !is.na(q6))
q6
freq(q6)
q6total <- gsub("2", "1", q6)
  q6total <- gsub("3", "1", q6total)
  q6total <- sum(as.numeric(q6total))
  q6total
q6rank1 <- gsub("2", "", q6)
  q6rank1 <- gsub("3", "", q6rank1)
  q6rank1 <-sum(as.numeric(subset(q6rank1, !is.na(as.numeric(q6rank1)))))
  q6rank1
q6rank2 <- gsub("1", "", q6)
  q6rank2 <- gsub("3", "", q6rank2)
  q6rank2 <-sum(as.numeric(subset(q6rank2, !is.na(as.numeric(q6rank2)))))/2
  q6rank2
q6rank3 <- gsub("1", "", q6)
  q6rank3 <- gsub("2", "", q6rank3)
  q6rank3 <-sum(as.numeric(subset(q6rank3, !is.na(as.numeric(q6rank3)))))/3
  q6rank3

q7 <- as.numeric(helpingfactors$Q15_0_7_RANK)
q7 <- subset(q7, !is.na(q7))
q7
freq(q7)
q7total <- gsub("2", "1", q7)
  q7total <- gsub("3", "1", q7total)
  q7total <- sum(as.numeric(q7total))
  q7total
q7rank1 <- gsub("2", "", q7)
  q7rank1 <- gsub("3", "", q7rank1)
  q7rank1 <-sum(as.numeric(subset(q7rank1, !is.na(as.numeric(q7rank1)))))
  q7rank1
q7rank2 <- gsub("1", "", q7)
  q7rank2 <- gsub("3", "", q7rank2)
  q7rank2 <-sum(as.numeric(subset(q7rank2, !is.na(as.numeric(q7rank2)))))/2
  q7rank2
q7rank3 <- gsub("1", "", q7)
  q7rank3 <- gsub("2", "", q7rank3)
  q7rank3 <-sum(as.numeric(subset(q7rank3, !is.na(as.numeric(q7rank3)))))/3
  q7rank3

q8 <- as.numeric(helpingfactors$Q15_0_8_RANK)
  q8 <- subset(q8, !is.na(q8))
  q8
freq(q8)
q8total <- gsub("2", "1", q8)
  q8total <- gsub("3", "1", q8total)
  q8total <- sum(as.numeric(q8total))
  q8total
q8rank1 <- gsub("2", "", q8)
  q8rank1 <- gsub("3", "", q8rank1)
  q8rank1 <-sum(as.numeric(subset(q8rank1, !is.na(as.numeric(q8rank1)))))
  q8rank1
q8rank2 <- gsub("1", "", q8)
  q8rank2 <- gsub("3", "", q8rank2)
  q8rank2 <-sum(as.numeric(subset(q8rank2, !is.na(as.numeric(q8rank2)))))/2
  q8rank2
q8rank3 <- gsub("1", "", q8)
  q8rank3 <- gsub("2", "", q8rank3)
  q8rank3 <-sum(as.numeric(subset(q8rank3, !is.na(as.numeric(q8rank3)))))/3
  q8rank3

q9 <- as.numeric(helpingfactors$Q15_0_9_RANK)
q9 <- subset(q9, !is.na(q9))
q9
freq(q9)
q9total <- gsub("2", "1", q9)
  q9total <- gsub("3", "1", q9total)
  q9total <- sum(as.numeric(q9total))
  q9total
q9rank1 <- gsub("2", "", q9)
  q9rank1 <- gsub("3", "", q9rank1)
  q9rank1 <-sum(as.numeric(subset(q9rank1, !is.na(as.numeric(q9rank1)))))
  q9rank1
q9rank2 <- gsub("1", "", q9)
  q9rank2 <- gsub("3", "", q9rank2)
  q9rank2 <-sum(as.numeric(subset(q9rank2, !is.na(as.numeric(q9rank2)))))/2
  q9rank2
q9rank3 <- gsub("1", "", q9)
  q9rank3 <- gsub("2", "", q9rank3)
  q9rank3 <-sum(as.numeric(subset(q9rank3, !is.na(as.numeric(q9rank3)))))/3
  q9rank3
  
#Make vectors for each rank
rank1 = c(q1rank1, q2rank1, q3rank1, q4rank1, q5rank1, q6rank1, q7rank1, q8rank1, q9rank1)
rank2 = c(q1rank2, q2rank2, q3rank2, q4rank2, q5rank2, q6rank2, q7rank2, q8rank2, q9rank2)
rank3 = c(q1rank3, q2rank3, q3rank3, q4rank3, q5rank3, q6rank3, q7rank3, q8rank3, q9rank3)
#Bind them together
helprankmatrix <- rbind(rank1, rank2, rank3)
  #Change data from n to a percentage
  denominator <- length(na.omit(ANAMSdata[3:nrow(ANAMSdata), 52]))
helprankmatrix <- rbind(rank1, rank2, rank3) / denominator * 100
helprankmatrix

#Labels for graph
helpingtitle <- c("What factors have helped you get\n to where you are today?")
alleviatingfactorsoptions <- c("Outreach programs through\nmedical schools",
                               "Outreach programs through\nundergraduate schools",
                               "AAIP\n(Association of American Indian Physicians)",
                               "Members of my home community",
                               "Family", "Native mentors or role models",
                               "Financial assistance via\nscholarships and grants",
                               "A sense of community\nin my medical school",
                               "A sense of community\nin my undergraduate school"
                                ) # In the order asked on Qualtrics


#colnames(helprankmatrix) <- alleviatingfactorsoptions
#View(helprankmatrix)

#helprankmarix <- helprankmatrix * denominator / 100
#View(helprankmatrix)

#Change the order
helpingdescending <- order(helprankmatrix[1, ], decreasing = FALSE)
helpingdescending
helpplot <- helprankmatrix[,helpingdescending[1:9]] # This is the matrix to plot
helpplot
alleviatingfactorsoptions <- alleviatingfactorsoptions[helpingdescending[1:9]]
alleviatingfactorsoptions

  #Use only top 3 choices to merge with alleviating factors matrix later
    testb <- helpplot[1:3,7:9] # Take the top 3 options, all 3 rows
    testb
    testplothelpoptions <- alleviatingfactorsoptions[7:9]
    testplothelpoptions

#Only use if plotting 9 options from alleviating factors alone
x11()
par(opar) # Reset par
opar = par(oma = c(0,10,0,0)) #makes margins bigger -> c(b,l,t,r) -> bottom, left, top, right
colorscale <- colorspace::sequential_hcl(n = 3, palette = "YlOrRd")
plotb <- barplot(
        helpplot, # USe for plot of 9
        #testb, # Use for combined graph
        beside = F, 
        main = helpingtitle, 
        xlim = c(0,100),
        xlab = "Percent of respondents %",
        legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
        args.legend = list(x = 'bottomright', bty = 'n'),
        #names.arg = testplothelpoptions, # use for combined plot
        names.arg = alleviatingfactorsoptions, #Use for plot of 9
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.names = 0.7,  #Decrease font size of x axis options
        horiz = TRUE,
        las = 1,   #Makes all text vertical
        space = 0.4,
        col = colorscale
        )
par(opar) # Reset par
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
par(opar) # Reset par

testplotchallengeoptions

#Now combine questions 25 and 26 into a single graph
x11()
colorscale <- colorspace::diverge_hcl(3, palette = "Blue-Red")
colorscale <- colorspace::sequential_hcl(1:3, palette = "YlOrRd")

  colorscale <- colorspace::sequential_hcl(n = 3, palette = "YlOrRd")

  colorscale <- colorspace::sequential_hcl(3, palette = "Light Grays")
  
setEPS()
postscript("Figure 2.eps")

par(mfrow = c(2,1))
opar = par(oma = c(0,5,0,0)) #makes margins bigger -> c(b,l,t,r) -> bottom, left, top, right
    plota <- 
      barplot(testa, 
              beside = F, 
              #main = challengetitle, # This is what the survey asked
              main = "Prominent Educational Barriers",
              xlim = c(0,100),
              xlab = "Percent of respondents %",
              legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
              args.legend = list(x = 'bottomright', bty = 'n'),
              names.arg = testplotchallengeoptions,
              cex.main = 1.5,
              cex.names = 0.75,  #Decrease font size of x axis options
              cex.lab = 0.9,
              cex.axis = 0.75,
              horiz = TRUE,
              col = colorscale,
              las = 1,   #Makes all text vertical
              space = 0.66
                    )

    plotb <- 
      barplot(testb, 
                     beside = F, 
                     #main = helpingtitle,# This is what the survey asked
                     main = "Prominent Educational Facilitators",
                     xlim = c(0,100),
                     xlab = "Percent of respondents %",
                     legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
                     args.legend = list(x = 'bottomright', bty = 'n'),
                     names.arg = testplothelpoptions, 
                     cex.main = 1.5, # Main title
                     cex.lab = 0.9, # Axis labels
                     cex.names = 0.75,  #Decrease font size of x axis options
                     cex.axis = 0.75,
                     horiz = TRUE,
                     col = colorscale,
                     las = 1,   #Makes all text vertical
                     space = 0.4
                  )
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
par(mfrow = c(1,1))
dev.off()

#Q27 - Qualitative question
table(stringdata[c(3:46),c(62)])

#Q28 - field of medicine
table(stringdata[,c(63)])
freq(stringdata[,c(63)])
freq(onlyaian[,c(63)])
freq(aiancombo[,c(63)])
freq(onlynativehc[,c(63)])
freq(nativecommunity[,c(63)])

piestring <- sort(table(stringdata[,c(63)]), decreasing = TRUE)
  piestring <- sort(table(onlyaian[,c(63)]), decreasing = TRUE) # Use for only AI/AN
  piestring <- sort(table(aiancombo[,c(63)]), decreasing = TRUE) # Use for only AI/AN in combo
piestring

surg.subspecialty <- piestring[8] + piestring[10] + piestring[11] #Group Urology, ENT, and Other Surgical Subspecialties together as "Surgical subspecialty"
surg.subspecialty #Note - header is named as urology as that is piestring[8], will force graph labels at the end
lowresponsegroup <- piestring[6] + piestring[7] + piestring[9] # Group derm, psych, EM into one for visual aesthetic
lowresponsegroup #Note - header is named as derm as that is piestring[6], will force graph labels at the end

#Changing the order to be more aesthetically pleasing, subjectively group
neworder <- c(piestring[1], piestring[2], piestring[4], #Fammed, IM, peds
              piestring[3], piestring[5], surg.subspecialty, #ob/gyn, gensurg, surg subspecialty
              lowresponsegroup)
              
  neworder
total <- sum(neworder)
  total  
#Setting graph parameters
colors <- colorspace::sequential_hcl(11, palette = "Lajolla")
colors <- colorspace::sequential_hcl(11, palette = "Light Grays")
futureplans <- c("Family Medicine", "IM", "Peds", "Ob/Gyn",
                 "General Surgery", "Surgical sub-specialty",
                 "Other"
                 )
    futureplans <- c("Family Medicine", # For only AIAN
                     "Ob/Gyn", "IM", 
                     "General Surgery", 
                     "Dermatology", 
                     "EM", "ENT", "Peds",
                     "Surgical sub-specialty", "Urology")
    
    futureplans <- c("Family Medicine", # For only AIAN in combo
                      "IM", "Ob/Gyn",
                     "Peds", "Psychiatry",
                     "Dermatology", 
                     "General Surgery", 
                     "Urology")
    

setEPS()        
postscript("Figure 3.eps")

x11()
pie(
  #piestring,
  neworder,
  main = "Future career plans:",
  #labels = paste0(futureplans, " ", round(100 * neworder / sum(neworder),0), "%"),
  labels = paste0(futureplans, " ", round(100 * piestring / sum(piestring),0), "%"),
  edges = 200,
  radius = 0.85, 
  init.angle =  90, 
  density = NULL,
  angle = 45, 
  border = colors, 
  lty = NULL, 
  cex = 1.2,
  cex.main = 2.5,
  col = colors,
  #border = colors
    )
dev.off()

#Q29 and Q30 - Future work plans - Bar plot chart of future career choices. 2 in one. Better than pie chart?
  #Q29 - IHS
    table(stringdata[c(3:46),c(64)])
    pie(table(stringdata[c(3:46),c(64)]),main = stringdata[c(1),c(64)])
    freq(stringdata[c(3:46),c(64)])
    freq(onlyaian[,c(64)])
    freq(onlynativehc[,c(64)])
    freq(nativecommunity[,c(64)])
    
    
  #Q30 - A non-IHS Tribal healthcare site
    table(stringdata[c(3:46),c(65)])
    pie(table(stringdata[c(3:46),c(65)]),main = stringdata[c(1),c(65)])
    freq(stringdata[c(3:46),c(65)])
    freq(onlyaian[,c(65)])
    freq(onlynativehc[,c(65)])
    freq(nativecommunity[,c(65)])
    
  #Remove those who selected full time for both questions 29 and 30
    fpcorrected <- stringdata[,64:65]
    fpcorrected <- subset(x = fpcorrected, subset = !(fpcorrected[,1] == "Yes - full time" & fpcorrected[,2] == "Yes - full time"))
    fpcorrected
    freq(fpcorrected[,1])
    freq(fpcorrected[,2])
    
    # Analysis of AI/AN alone and AI/AN in combination w/ corrected numbers from above ^
      
      # Only AI/AN
        aianfpcorrected <- onlyaian[,64:65]
        aianfpcorrected <- subset(x = aianfpcorrected, subset = !(aianfpcorrected[,1] == "Yes - full time" & aianfpcorrected[,2] == "Yes - full time"))
        aianfpcorrected
          freq(aianfpcorrected[,1])
          freq(aianfpcorrected[,2])
    
      # AI/AN in combination
        cfpcorrected <- aiancombo[,64:65]
        cfpcorrected <- subset(x = cfpcorrected, subset = !(cfpcorrected[,1] == "Yes - full time" & cfpcorrected[,2] == "Yes - full time"))
        cfpcorrected
          freq(cfpcorrected[,1])
          freq(cfpcorrected[,2])
        
      #Chi Squared Testing - question 64 - IHS
        a <- table(aianfpcorrected[,1])
        b <- table(cfpcorrected[,1])
        fpihs <- rbind(a,b)
        rownames(fpihs) <- c("AI/AN alone", "AI/AN in combination")
        fpihs
        chisq.test(fpihs,simulate.p.value = TRUE)
        fisher.test(fpihs)
      
      #Chi Squared Testing - question 65 - non-IHS
        a <- table(aianfpcorrected[,2])
        b <- table(cfpcorrected[,2])
        fpnihs <- rbind(a,b)
        rownames(fpnihs) <- c("AI/AN alone", "AI/AN in combination")
        fpnihs[2,2] <- 0 # Manually set number of AI/AN in combo who answered "No" to zero in agreement w/ survey results
        fpnihs [2,4] <- 0 # Manually set number of AI/AN in combo who answered "Yes - full time" to zero in agreement w/ survey results
        fpnihs
        chisq.test(fpnihs, simulate.p.value = TRUE) 
        fisher.test(fpnihs)
        
  #A graph not used in paper
    q29 <- freq(stringdata[c(3:46),c(64)])
    q30 <- freq(stringdata[c(3:46),c(65)])
    futureplans <- rbind(q29[1:4,2], q30[1:4,2])
    View(futureplans)
    x11()
    barplot(futureplans, beside = T, main = "Do you plan on working for:", 
        legend.text = c("IHS", "Another Non-IHS Tribal Healthcare Center"), 
        args.legend = list(x = 'topright', bty = 'n'),
        ylab = "Percent %"
            )
    freq(onlyaian[,c(64)]) # For only AI/AN
    freq(onlyaian[,c(65)]) # For only AI/AN
    
    freq(aiancombo[,c(64)]) # For only AI/AN in combination
    freq(aiancombo[,c(65)]) # For only AI/AN in combination   

    