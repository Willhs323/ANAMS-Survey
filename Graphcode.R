# clean current work space
rm(list=ls(all=T))
# set options
options(stringsAsFactors = F)         # no automatic data transformation
options("scipen" = 100, "digits" = 4) # suppress math annotation
# install libraries
#install.packages(c())
#Package you'll need
#Things I'll definitely use
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
##### 
#ANAMS survey data
#Set the working directory. Using setwd, use the "Tab" key after each / to find your dataset
#Read in data
setwd('~/ANAMS/IHS paper/Paper/Spreadsheet Downloads/')
ANAMSdata <- read.csv('ANAMS Survey_August 7, 2021 numeric.csv', na.strings = c("", "NA"))
stringdata <- read.csv('ANAMS Survey_August 7, 2021 string.csv', na.strings = c("", "NA"))

# Store a matrix of questions
Questionstring <- stringdata[c(1),]

#View, show data structure, show summaries of variables
str(ANAMSdata)
summary(ANAMSdata)

#Clean data, remove incomplete answers
ANAMSdata <- ANAMSdata[3:nrow(ANAMSdata),] # Remove first two rows of data from qualtrics
  ANAMSdata <- subset(x = ANAMSdata, subset = as.numeric(ANAMSdata[,5]) == 100) # Remove incomplete responses
  ANAMSdata <- subset(x = ANAMSdata, subset = as.numeric(ANAMSdata[,18]) == 1) # Remove those who did not consent
  ANAMSdata <- subset(x = ANAMSdata, subset = as.numeric(ANAMSdata[,19]) == 1) # Remove those who are not ANAMS members
  View(ANAMSdata)

stringdata <- stringdata[3:nrow(stringdata),] # Remove first two rows of data from qualtrics
  stringdata <- subset(x = stringdata, subset = as.numeric(stringdata[,5]) == 100) # Remove incomplete responses
  stringdata <- subset(x = stringdata, subset = stringdata[,18] == "Yes") # Remove those who did not consent
  stringdata <- subset(x = stringdata, subset = stringdata[,19] == "Yes") # Remove those who are not ANAMS members
  View(stringdata)

# Number of total responses
numresponses <- length(ANAMSdata[,1])
numresponses

#Q3 - How will I put ethnicity selections? Histogram? Q1 on qualtrics
#add up all that have values in a category? Say that it includes double mentions
#What is the percent that is just native
#What percent is more than native?
table(stringdata[,c(20)])
pie(table(stringdata[,c(20)]),main = stringdata[c(1),c(20)])
freq(stringdata[,c(20)])

#Q4 Gender Identity 
table(stringdata[,c(21)])
barchart(stringdata[,c(21)])
freq(stringdata[,c(21)])

#Geographic Data
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

###This part is not automated. You will have to change the row numbers if people from more states answer
#Alaska - Alaska
  #No respondants from Alaska
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
pct.another.native.community <- sum(percent.grewup.native[c(3:6, 10),2]) # Gives total number of respondants who grew up in a primarily native community
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
    barplot(pct.ans.q6, main = "Percent of respondants who:", 
        legend.text = bar.names.q6, 
        col = c("#CA0020", "#F4A582", "#92C5DE", "#0571B0"), 
        args.legend = list(x = 'topleft', bty = 'n')
            )

#Q7 - Select all that apply - where did you get healthcare growing up - Q4 in R
table(stringdata[c(3:46),c(24)])
freq(stringdata[,c(24)])
#recode out the multiple responses that chose an option and also selected "No"
stringdata$Q4 <- gsub(",No","",stringdata$Q4)
barplot(table(stringdata[c(3:46),c(24)]),main = stringdata[c(1),c(24)])
freq(stringdata[c(3:46),c(24)])

#Q9 Degrees to be obtained
table(stringdata[c(3:46),c(26)])
pie(table(stringdata[c(3:46),c(26)]),main = stringdata[c(1),c(26)])
freq(stringdata[,c(26)])
    
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

#Make a plot that ranks by strong dis -> somewhat dis -> I don't know -> somewhat agree -> strongly agree
  #Set up the data
matrixorderdat <- ANAMSdata[,31:41] 
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

#Change order so strongly agree is largest
descendingorder <- order(graphorderdat[1, ], decreasing = FALSE) # Change the order of answers by decreasing responses to strongly agree
descendingorder
matrixplot <- graphorderdat[,descendingorder[1:11]] # New matrix of data based on decreasing order of question 1
matrixplot

#Make from number to proportion
denominator <- length(matrixorderdat[, 1])
denominator
matrixplot <- matrixplot / denominator * 100 # Change from n to percent

#Parameters for graph
legend.text = c("Strongly agree", "somewhat agree", "I don't know", "somewhat disagree", "Strongly disagree")
qnames <- c(
  "I feel a sense of community with\n Native peers at my institution",
  "I am satisfied with my school's effort\n to recruit Native students",
  "There is adequate scholarship funding\n at my school for Native students",
  "I am satisfied with Native faculty \n representation and mentorship at my school",
  "I am sastified with my school's engagement with Native faculty \n and experts to contribute to lectures and learning opportunities",
  "I am satisfied with the amount of cultural competancy training \n as it relates to AI-AN health care delivery at my institution",
  "I am satisfied with my school's education and training \n that exposes students to Native communities and health",
  "I am satisfied with the amount of education\n of AI-AN health disparities at my institution",
  "I am satisfied with the opportunities provided by my\n institution to do clinical electives in tribal communities",
  "I am satisfied with my school's\n outreach to Native organizations",
  "I am satisfied with my school's\n outreach to Native communities"
            )
qnames <- qnames[descendingorder[1:11]]
colorscale <- colorspace::diverge_hcl(5, palette = "Blue-Red")
x11()
par(opar) # Reset par
opar = par(oma = c(4,16,2,2)) #makes margins bigger -> c(b,l,t,r) -> bottom, left, top, right
barplot(matrixplot, 
        horiz = TRUE,
        beside = F, 
        main = c("Answer the following questions about your medical school:"), 
        xlim = c(0,100),
        xlab = "Percent of respondants %",
        names.arg = qnames,
        cex.names = 0.75,  #Decrease font size of question labels
        cex.axis = 1.2,
        cex.main = 2,
        cex.lab = 1.5,
        las = 1,   #Makes all text vertical
        col = colorscale,
        space = 0.66
          
)
par(opar) # Reset par
opar =par(oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
legend(x ="bottomleft", legend = legend.text, fill = colorscale, bty = 'n', ncol = 1, xjust = 1, yjust = 1.5, inset = -0.05)
par(opar) # Reset par

#Q25 Ranking challenges
datrank <- ANAMSdata[43:51]
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
View(challengerankmatrix)
#Labels for graph
challengetitle <- c("Ranked challenges")
challengeoptions <- c("Financial", 
             "Family related issues", 
             "Distance from family",
             "Loss of culture", 
             "Imposter syndrome", 
             "Lack of role models",
             "Unfamiliarity with applications",
             "First generation college student",
             "First generation medical student"
              )

#Change data from n to a percentage
denominator <- length(na.omit(ANAMSdata[3:nrow(ANAMSdata), 42]))
challengerankmatrix <- rbind(rank1, rank2, rank3) / denominator * 100
View(challengerankmatrix)

#Change order so descending by ranked first
  challengedescending <- order(challengerankmatrix[1, ], decreasing = FALSE)
  challengedescending
  challengeplot <- challengerankmatrix[,challengedescending[1:9]]
  challengeplot
  challengeoptions <- challengeoptions[challengedescending[1:9]]
x11()
opar = par(oma = c(0,5,0,0)) #makes margins bigger -> c(b,l,t,r) -> bottom, left, top, right
barplot(challengeplot, 
        beside = F, 
        main = challengetitle, 
        xlim = c(0,60),
        xlab = "Percent of respondants %",
        legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
        args.legend = list(x = 'bottomright', bty = 'n'),
        names.arg = challengeoptions,
        cex.main = 1.5,
        cex.names = 0.7,  #Decrease font size of x axis options
        cex.lab = 1.5,
        horiz = TRUE,
        las = 1,   #Makes all text vertical
        space = 0.66
)


#Q26 Alleviating factors ranking
helpingfactors <- ANAMSdata[,53:61]
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

q8 <- as.numeric(helpingfactors$Q15_0_1_RANK)
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
View(helprankmatrix)
#Labels for graph
helpingtitle <- c("Rank the following factors that have helped you")
alleviatingfactorsoptions <- c("Outreach programs through\nmedical schools",
                               "Outreach programs through\nundergraduate schools",
                               "AAIP\n(Association of American Indian Physicians)",
                               "Members of my home community",
                               "Family", "Native mentors or role models",
                               "Financial assistance via\nscholarships and grants",
                               "A sense of community\nin my medical school",
                               "A sense of community\nin my undergraduate school"
                                )

#Change data from n to a percentage
denominator <- length(na.omit(ANAMSdata[3:nrow(ANAMSdata), 52]))
helprankmatrix <- rbind(rank1, rank2, rank3) / denominator * 100

#Change the order
helpingdescending <- order(helprankmatrix[1, ], decreasing = FALSE)
helpingdescending
helpplot <- helprankmatrix[,helpingdescending[1:9]] # This is the matrix to plot
helpplot
alleviatingfactorsoptions <- alleviatingfactorsoptions[helpingdescending[1:9]]
alleviatingfactorsoptions

x11()
opar = par(oma = c(0,10,0,0)) #makes margins bigger -> c(b,l,t,r) -> bottom, left, top, right
barplot(helpplot, 
        beside = F, 
        main = helpingtitle, 
        xlim = c(0,75),
        xlab = "Percent of respondants %",
        legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
        args.legend = list(x = 'bottomright', bty = 'n'),
        names.arg = alleviatingfactorsoptions, 
        cex.main = 1.5,
        cex.lab = 1.5,
        cex.names = 0.7,  #Decrease font size of x axis options
        horiz = TRUE,
        las = 1,   #Makes all text vertical
        space = 0.4
        )

#Q27 - Qualitative question
table(stringdata[c(3:46),c(62)])

#Q28 - field of medicine
table(stringdata[,c(63)])
freq(stringdata[,c(63)])

piestring <- sort(table(stringdata[,c(63)]), decreasing = TRUE)
#Group Urology, ENT, and Other Surgical Subspecialties together as "Surgical subspecialty"
surg.subspecialty <- piestring[8] + piestring[10] + piestring[11]
piestring

#Changing the order to be more aesthetically pleasing, subjectively group
neworder <- c(piestring[1], piestring[2], piestring[4], piestring[9], piestring[7], piestring[3], piestring[6], piestring[5], surg.subspecialty)
  neworder
total <- sum(neworder)
  total  
#Setting graph parameters
colors <- colorspace::sequential_hcl(11, palette = "Lajolla")
futureplans <- c("Family Medicine", "IM", "Peds", "EM", "Psychiatry",
  "Ob/Gyn", "Derm", "General Surgery", "Surgical sub-specialty")

x11()
pie(neworder,
  main = "Future career plans:",
  labels = paste0(futureplans, " ", round(100 * neworder / sum(neworder),0), "%"),
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

#Q29 and Q30 - Future work plans - Bar plot chart of future career choices. 2 in one. Better than pie chart?
  #Q29 - IHS
    table(stringdata[c(3:46),c(64)])
    pie(table(stringdata[c(3:46),c(64)]),main = stringdata[c(1),c(64)])
    freq(stringdata[c(3:46),c(64)])
  #Q30 - A non-IHS Tribal healthcare site
    table(stringdata[c(3:46),c(65)])
    pie(table(stringdata[c(3:46),c(65)]),main = stringdata[c(1),c(65)])
    freq(stringdata[c(3:46),c(65)])

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
            
            