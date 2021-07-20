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
#####
#Citation for code
#@manual{schweinberger2020survey,
#  author = {Schweinberger, Martin},
#  title = {Questionnaires and Surveys: Analyses with R},
#  note = {https://slcladal.github.io/survey.html},
#  year = {2020},
#  organization = "The University of Queensland, Australia. School of Languages and Cultures},
#  address = {Brisbane},
#  edition = {2020/12/11}
#}
#Cite book

##### 
#ANAMS survey data

#Other potential data sources
#IHS has an excel file of facilities available

#Set the working directory. Using setwd, use the "Tab" key after each / to find your dataset
setwd('~/ANAMS/IHS paper/Paper/Spreadsheet Downloads/5-15 download/')
#Read in data
ANAMSdata <- read.csv('ANAMSdatanumericcsv.csv', na.strings = c("", "NA"))
stringdata <- read.csv('ANAMSdatastringcsv.csv', na.strings = c("", "NA"))

#View, show data structure, show summaries of variables
str(ANAMSdata)
summary(ANAMSdata)
View(ANAMSdata)
View(stringdata)

# Store a matrix of questions
Questionstring <- stringdata[c(1),]
View(Questionstring)

#Q3 - How will I put ethnicity selections? Histogram? Q1 on qualtrics
#add up all that have values in a category? Say that it includes double mentions
#What is the percent that is just native
#What percent is more than native? 
table(stringdata[c(3:46),c(20)])
pie(table(stringdata[c(3:46),c(20)]),main = stringdata[c(1),c(20)])
freq(stringdata[c(3:46),c(20)])
#Think about coding it together so the options are Native, Native + white, Native + a different race too

#Q4 Gender Identity 
table(stringdata[c(3:46),c(21)])
pie(table(stringdata[c(3:46),c(21)]),main = stringdata[c(1),c(21)])
barchart(stringdata[c(3:46),c(21)])
freq(stringdata[c(3:46),c(21)])

#Q5 - home state
table(stringdata[c(3:46),c(22)])
pie(table(stringdata[c(3:46),c(22)]),main = stringdata[c(1),c(22)])
freq(stringdata[c(3:46),c(22)])

#Q6 - select all that apply to you - grow up on reservation - named Q3 on R
#Percent that selected any option
table(stringdata[c(3:46),c(23)])
pie(table(stringdata[c(3:46),c(23)]),main = stringdata[c(1),c(23)])
freq(stringdata[c(3:46),c(23)])
#Want to see the number of people who selected each individual option
dat <- na.omit(read.csv('ANAMSdatanumericcsv.csv'))
dat <- dat[3:nrow(dat), 23]
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
#need to still move the legend

#Q7 - Select all that apply - where did you get healthcare growing up - Q4 in R
table(stringdata[c(3:46),c(24)])
freq(stringdata[c(3:46),c(24)])
#recode out the multiple responses that chose an option and also selected "No"
#So looking at everyone who picked each option
#These responses suggest people received healthcare from IHS or a tribal healthcare center, and also from "normal" sources
#Only two respondents did this. People might have interpreted the question differently
stringdata$Q4 <- gsub(",No","",stringdata$Q4)
barplot(table(stringdata[c(3:46),c(24)]),main = stringdata[c(1),c(24)])
freq(stringdata[c(3:46),c(24)])

#Q8 - state medical school
table(stringdata[c(3:46),c(25)])
pie(table(stringdata[c(3:46),c(25)]),main = stringdata[c(1),c(25)])
freq(stringdata[c(3:46),c(25)])

#Q9 Degrees to be obtained
table(stringdata[c(3:46),c(26)])
pie(table(stringdata[c(3:46),c(26)]),main = stringdata[c(1),c(26)])
freq(stringdata[c(3:46),c(26)])

#Q10School in IHS - maybe try to put on a map?
table(stringdata[c(3:46),c(27)])
pie(table(stringdata[c(3:46),c(27)]), main = stringdata[1,27], edges = 200,radius = 0.8, init.angle =  90, density = NULL,angle = 45, col = NULL, border = NULL, lty = NULL)
freq(stringdata[c(3:46),c(27)])
barchart(stringdata[c(3:46),c(27)])
#I had this idea of making a map of the US, and filling in the borders in the different territories. 
#Might need a graphic designer? Might have to at least ask around.
#I can look into it, but I also might not care enough to invest my time into it
#IHS has an excel file of facilities available

#Q11 - Med school tribal rotations available... multiple responses allowed - Q8 on qualtrics
table(stringdata[c(3:46),c(28)])
pie(table(stringdata[c(3:46),c(28)]),main = stringdata[c(1),c(28)])
freq(stringdata[c(3:46),c(28)])
#Want to see the number of people who selected each individual option
dat11 <- na.omit(read.csv('ANAMSdatanumericcsv.csv'))
dat11 <- dat11[3:nrow(dat11), 28]
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
pct.ans.q11 <- num.ans.q11/length(subset(stringdata[3:46,28], !is.na(stringdata[3:46,28])))
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
#clean up graph

#Q12 - will you / did you complete a tribal rotation - Q9 on qualtrics
table(stringdata[c(3:46),c(29)])
rotationlabels = c('Did not answer','Yes, I have completed a tribal rotation', 'I plan on completing a tribal rotation', 'I do not plan on completing a tribal rotation', 'I do not know yet')
pie(table(stringdata[c(3:46),c(29)]), main = stringdata[c(1),c(29)], edges = 200,radius = 0.8, init.angle =  90, density = NULL,angle = 45, col = NULL, border = NULL, lty = NULL)
freq(stringdata[c(3:46),c(29)])
barplot(table(stringdata[c(3:46),c(29)]))

#Q13 - Did ability to complete tribal rotation influence where you went to school - Q10 on qualtrics
table(stringdata[c(3:46),c(30)])
influencelabels = c('Did not answer','It strongly influenced my decision', 'It somewhat influenced my decision', 'It did not influence my decision')
pie(table(stringdata[c(3:46),c(30)]),main = stringdata[c(1),c(30)])
freq(stringdata[c(3:46),c(30)])
barplot(table(stringdata[c(3:46),c(30)]))

#Q14 - Q24: Rank the following. These are questions about your medical school
#Chi squared testing?? need a crosstabs likely
# I threw out 2 questions. Each had a few "I don't know answers" that messed up my final plot
# The other's didn't have any I don't know selections

# Remember to do question 9 - satisfaction with opportunities to do tribal rotations
table(stringdata[c(3:46),c(39)])
pie(table(stringdata[c(3:46),c(39)]),main = stringdata[c(1),c(39)])
freq(stringdata[c(3:46),c(39)])
pctmatrix9 <- table(stringdata[c(3:46),c(39)])/length(subset(stringdata$Q12_9, !is.na(stringdata$Q12_9)))
x11()
barplot(pctmatrix9, 
        main = "I am satisfied with the opportunities provided by my institution to do Tribal clinical electives",
        col = c("#FFFFB2", "#BDD7E7", "#FECC5C", "#3182BD", "#BD0026"),
        )
#Also removed question 3 - scholarship funding
table(stringdata[c(3:46),c(33)])
pie(table(stringdata[c(3:46),c(33)]),main = stringdata[c(1),c(33)])
freq(stringdata[c(3:46),c(33)])
pctmatrix9 <- table(stringdata[c(3:46),c(33)])/length(subset(stringdata$Q12_3, !is.na(stringdata$Q12_3)))
x11()
barplot(pctmatrix9, 
        main = "There is adequate scholarship funding at my school for natives",
        col = c("#FFFFB2", "#BDD7E7", "#FECC5C", "#3182BD", "#BD0026"),
        )

#Matrix
matrixq <- read.csv('ANAMSdatastringcsv.csv',na.strings = c("", "NA") ,blank.lines.skip = TRUE)
# Load colors, labels
lbs <- c("Strongly agree", "somewhat agree",  "somewhat disagree", "Disagree")
maintitle <- c("Answer the following questions about your medical school:")
#Consider only matrix questions
matrixq <- matrixq[c(3:nrow(matrixq)), c(31:41)]
#Get rid of scholarship funding question that had answers with "I don't know"
matrixq <- matrixq[,-3]
#Get rid of matrix question 9, which we will want to analyze separately along with other data
matrixq <- matrixq[,-8]

#Change column names to what I want to graph, doesn't include q3 and q9
colnames(matrixq) <- c("I feel a sense of community with Native peers at my institution",
                       "I am satisfied with my school's effort to recruit Native students",
                       "I am satisfied with Native faculty representation and mentorship at my school",
                       "I am sastified with my school's engagement with Native faculty and experts to contribute to lectures and other learning opportunities",
                       "I am satisfied with the amount of cultural competancy training as it relates to AI-AN health care delivery at my institution",
                       "I am satisfied with my school's education and training that exposes students to Native communities and health",
                       "I am satisfied with the amount of education of AI-AN health disparities at my institution",
                       "I am satisfied with my school's outreach to Native organizations",
                       "I am satisfied with my school's outreach to Native communities"
                       )
#Cleans it up
matrixq <- matrixq %>%
  dplyr::mutate_if(is.character, factor) %>%
  dplyr::mutate_if(is.numeric, factor, levels = 1:4, labels = lbs) %>%
  drop_na() %>%
  as.data.frame()
x11()
plot(likert(matrixq), ordered = T, wrap= 60) + ggtitle(maintitle)


#Q25 Ranking challenges
datrank <- na.omit(read.csv('ANAMSdatanumericcsv.csv'))
datrank <- datrank[3:46,43:51]
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
challengetitle <- c("Rank the following statements")
challengeoptions <- c("1. Financial", 
             "2. Family related issues", 
             "3. Distance from family",
             "4. Loss of culture", 
             "5. Imposter syndrome", 
             "6. Lack of role models",
             "7. Unfamiliarity with the process of applying to medical school",
             "8. First generation college student",
             "9. First generation medical student"
              )
questionlabel <- c("Option 1", "Option 2", "Option 3", 
                   "Option 4", "Option 5", "Option 6", 
                   "Option 7", "Option 8", "Option 9")

x11()
#Where do I put the questions - where to put challengeoptions

barplot(challengerankmatrix, beside = T, main = challengetitle, 
        legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
        args.legend = list(x = 'topleft', bty = 'n'),
        ylab = "n", 
        names.arg = questionlabel
        
)

#Q26 Alleviating factors ranking
helpingfactors <- na.omit(read.csv('ANAMSdatanumericcsv.csv'))
helpingfactors <- helpingfactors[3:46,53:61]
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
  
#Make vectors for each rank
rank1 = c(q1rank1, q2rank1, q3rank1, q4rank1, q5rank1, q6rank1, q7rank1, q8rank1, q9rank1)
rank2 = c(q1rank2, q2rank2, q3rank2, q4rank2, q5rank2, q6rank2, q7rank2, q8rank2, q9rank2)
rank3 = c(q1rank3, q2rank3, q3rank3, q4rank3, q5rank3, q6rank3, q7rank3, q8rank3, q9rank3)
#Bind them together
helprankmatrix <- rbind(rank1, rank2, rank3)
View(challengerankmatrix)
#Labels for graph
helpingtitle <- c("Rank the following statements")
alleviatingfactorsoptions <- c("Outreach programs through medical schools",
                               "Outreach programs through undergraduate schools",
                               "AAIP (Association of American Indian Physicians",
                               "Members of my home community",
                               "Family", "Native mentors or role models",
                               "Financial assistance via scholarships and grants",
                               "A sense of community in my medical school",
                               "A sense of community in my undergraduate school"
                                )
questionlabel <- c("Option 1", "Option 2", "Option 3", 
                   "Option 4", "Option 5", "Option 6", 
                   "Option 7", "Option 8", "Option 9")

x11()
#Where do I put the questions - where to put challengeoptions

barplot(challengerankmatrix, beside = T, main = helpingtitle, 
        legend.text = c("Ranked 1st", "Ranked 2nd", "Ranked 3rd"), 
        args.legend = list(x = 'topleft', bty = 'n'),
        ylab = "n", 
        names.arg = questionlabel
        )

#Q27 - Qualitative question
#Print out on Rmarkdown
table(stringdata[c(3:46),c(62)])

#Q28 - field of medicine
table(stringdata[c(3:46),c(63)])
pie(table(stringdata[c(3:46),c(63)]), main = stringdata[1,63], edges = 200,radius = 0.8, init.angle =  90, density = NULL,angle = 45, col = NULL, border = NULL, lty = NULL)
freq(stringdata[c(3:46),c(63)])

#Q29 and Q30 - Future work plans - Bar plot chart of future career choices. 2 in one. Better than pie chart?
table(stringdata[c(3:46),c(64)])
pie(table(stringdata[c(3:46),c(64)]),main = stringdata[c(1),c(64)])
freq(stringdata[c(3:46),c(64)])
table(stringdata[c(3:46),c(65)])
pie(table(stringdata[c(3:46),c(65)]),main = stringdata[c(1),c(65)])
freq(stringdata[c(3:46),c(65)])
#To do - put into frequency, make plots, maybe put into multiples, color and design

#Finished - View these questions side by side
q29 <- table(stringdata[c(3:46),c(64)])
q30 <- table(stringdata[c(3:46),c(65)])
futureplans <- rbind(q29, q30)
View(futureplans)
x11()
barplot(futureplans, beside = T, main = "Do you plan on working for:", 
        legend.text = c("IHS", "Another Non-IHS Tribal Healthcare Center"), 
        args.legend = list(x = 'topright', bty = 'n')
        )

#Finished - View these questions side by side as percentages
library(descr)
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



#working code - if I use ggplot
#Dont edit this one until the lower one looks what I want it to have
library(ggplot2)
x11()
practice <- subset(stringdata[c(3:46),], !is.na(Q17))
ggplot(data = practice) +
  geom_bar(mapping = aes(Q17),fill = "steelblue") +
  theme_minimal()

#working doc code
library(ggplot2)
library(reshape2)
practice <- subset(stringdata[c(3:46),c(64:65)], !is.na(Q17), !is.na(Q18))
View(practice)
practicegraph <- melt(practice, id.vars = "Q17" )

x11()
ggplot(data = practicegraph, aes(x = )) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") + theme_minimal()

  















