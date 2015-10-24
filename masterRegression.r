setwd("~/Dropbox/PhD/Placebo/Experiments/CaffeinePredictorsOfWIthdrawalSeverity/data")

# doing a regression across mutliple studies, 887, 974, 887B

library(reshape2)
library(plyr)
library(lubridate)
library(stringr)
library(data.table)
library(pathological)
library(dplyr)


########### Amalgamating spreadsheets from the different studies #########################

########################## 887 #############################################

# read in the master file from 887

M887 <- read.csv("~/Dropbox/PhD/Placebo/Experiments/CaffeineDoseReduction_2014887/RCaffeine887Data/data/Caffeine887out.csv")

# remove non-entries

M887 <- M887[!is.na(M887$Paid_Y1N2),]


# create a single employment column

empFrame <- data.frame(M887$DemQ6,
                      M887$DemQ7,
                      M887$DemQ8,
                      M887$DemQ9,
                      M887$DemQ10,
                      M887$DemQ11,
                      M887$DemQ12) 

M887$emp <- do.call(pmax, col(empFrame)*replace(empFrame, is.na(empFrame), 0))

# Now make composite total daily caffeine score

typeCaffNames <- c("DemQ24",
                   "DemQ26", 
                   "DemQ30",
                   "DemQ34")


# replaces NA with 0 for these columns so we can calculate total caffeine scores
M887[, typeCaffNames] <- replace(M887[,typeCaffNames], is.na(M887[,typeCaffNames]), 0)


M887$totCaffeine <- M887$DemQ24*85 +
                    M887$DemQ26*30 +
                    M887$DemQ30*37 +
                    M887$DemQ34*80
  

# Create vector of CWSQ item names

CWSQItems <- c("drowsy",
           "selfconfident",
           "yawning",
           "alert",
           "tiredfatigued",
           "content",
           "diffconc",
           "irritable",
           "heavyfeel",
           "depressedmood",
           "grouchy",
           "urgework",
           "flulike",
           "headache",
           "talkative",
           "sluggish",
           "upsetstomach",
           "clearheaded",
           "desiresoc",
           "energetic",
           "nauseavom",
           "musclepain",
           "discouraged",
           "queasy",
           "nauseaous",
           "vomiting",
           "headachey",
           "anxious",
           "nervous",
           "jittery",
           "cravingcoffee",
           "cravingcaffeine")

CWSQQNames <- paste("Q", 1:32, CWSQItems, sep="")

# Now to rename columns in 887 so they match 974 

oldCols <- c("Name",
             "Email",
             "DateTime_B1",
             "DateTime_B2",
             "DurationCaff",
             "NumQuit",
             "totCaffeine",
             "DemQ24",
             "DemQ3",
             "DemQ5",
             "emp",
             "EQ5",
             "Verbal_CompliedY1N2",
             "DemQ14",
             "DemQ16",
             "DemQ19",
             "Paid_Y1N2",
             "ScreenedY1N2",
             paste("QB1Q", 2:33, sep=""),
             paste("QB2Q", 2:33, sep=""))

newCols <- c("Name",
             "Email",
             "B1testtime",
             "B2testtime",
             "yearsCaffUse",
             "numQuitAttempts",
             "totCaffeine",
             "numCupsCoffee",
             "Gender_F1M0",
             "age",
             "emp",
             "ComplianceTest_Y1N0",
             "Verbal_CompliedY1N0",
             "edLevel",
             "maritalStat",
             "langOtherEng",
             "Paid_Y1N0",
             "Screened_Y1N0",
             paste("B1", CWSQQNames, sep=""),
             paste("B2", CWSQQNames, sep=""))


# change over relevant column names
M887 <- setnames(M887, oldCols, newCols)

M887 <- M887[, which(names(M887) %in% newCols)]




############ Recoding #########################

M887$Paid_Y1N0 <- mapvalues(M887$Paid_Y1N0, from = c(2,1), to = c(0,1))

M887$Screened_Y1N0 <- mapvalues(M887$Screened_Y1N0, from = c(2,1), to = c(0,1))

M887$Gender_F1M0 <- mapvalues(M887$Gender_F1M0, from = c(2,1), to = c(1,0))

M887$Verbal_CompliedY1N0 <- mapvalues(M887$Verbal_CompliedY1N0, from = c(1,2), to = c(1,0))

M887$ComplianceTest_Y1N0 <- mapvalues(M887$ComplianceTest_Y1N0, from = c(2,1), to = c(1,0))


#changing relevant columns to factors

M887$Gender_F1M0 <- factor(M887$Gender_F1M0, 
                         levels = c(1, 0),
                         labels = c("female", "male"),
                         ordered = FALSE)

M887$Paid_Y1N0 <- factor(M887$Paid_Y1N0, 
                              levels = c(1, 0),
                              labels = c("paid", "coursecredit"),
                              ordered = FALSE)


M887$Screened_Y1N0 <- factor(M887$Screened_Y1N0,
                                  levels = c(1, 0),
                                  labels = c("screened", "notScreened"),
                                  ordered = F)

M887$emp <- factor(M887$emp, levels = c(1,2,3,4,5,6,7),
                             labels = c("fulltime",
                                        "casual",
                                        "volunteer",
                                        "unemployed",
                                        "student",
                                        "retired",
                                        "other"),
                             ordered = F)

M887$ComplianceTest_Y1N0 <- factor(M887$ComplianceTest_Y1N0,
                                   levels = c(1,0),
                                   labels = c("complied", "didNotComply"),
                                   ordered = F)



M887$Verbal_CompliedY1N0 <- factor(M887$Verbal_CompliedY1N0,
                                        levels = c(1,0),
                                        labels = c("complied", "didNotComply"),
                                        ordered = F) 


M887$edLevel <- factor(M887$edLevel,
                                 levels = c(1, 2, 3),
                                 labels = c("Other", "Secondary", "Tertiary"),
                                 ordered = F)

M887$maritalStat <- factor(M887$maritalStat, levels = c(1,2,3,4,5,6),
                           labels = c("Single",
                                      "inRelationship",
                                      "Married",
                                      "Divorced",
                                      "Widowed",
                                      "Other"),
                           ordered = F)

M887$langOtherEng <- factor(M887$langOtherEng,
                       levels = c(1, 2),
                       labels = c("Yes", "No"),
                       ordered = F)

# convert dates from factors to POSIX.ct vectors

require(lubridate) # this allows you to use parse_date_time


M887$B1testtime <- as.POSIXct(M887$B1testtime, format = "%d/%m/%Y %H:%M")
M887$B1testtime <- parse_date_time(M887$B1testtime, "%Y/%m/%d %H:%M:%S") 
M887$B2testtime <- as.POSIXct(M887$B2testtime, format = "%d/%m/%Y %H:%M")
M887$B2testtime <- parse_date_time(M887$B2testtime, "%Y/%m/%d %H:%M:%S") 




############################### 974 #####################################

M974 <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Caffeine Experiment Number 2_Genes and Withdrawal_2014_974/data/974_Qualtrics_R/factoredMaster974.csv")

# turn non-factors into factors
M974$ComplianceTest_Y1N0 <- factor(M974$ComplianceTest_Y1N0,
                                   levels = c(1,0),
                                   labels = c("complied", "didNotComply"),
                                   ordered = F)





# change column names from T1 to B2 so that the two dataframes are the same
M974 <- setnames(M974, paste("T1", CWSQQNames, sep=""),
                       paste("B2", CWSQQNames, sep=""))

M974 <- setnames(M974, "T1testtime", "B2testtime")
                         

# extract only the columns we are interested in. The columns that are the same as M887
M974 <- M974[, which(names(M974) %in% newCols)]

# convert dates from factors to POSIX.ct vectors

require(lubridate) # this allows you to use parse_date_time


# notice the format is slightly different to 887 above. Namely the original date format was a two-digit year. This is why it wasn't working. 
M974$B1testtime <- as.POSIXct(M974$B1testtime, format = "%d/%m/%y %H:%M")
M974$B1testtime <- parse_date_time(M974$B1testtime, "%Y/%m/%d %H:%M:%S") 
M974$B2testtime <- as.POSIXct(M974$B2testtime, format = "%d/%m/%y %H:%M")
M974$B2testtime <- parse_date_time(M974$B2testtime, "%Y/%m/%d %H:%M:%S") 


# write.csv(M974, "~/Dropbox/PhD/Placebo/Experiments/CaffeinePredictorsOfWIthdrawalSeverity/data/M974.csv", row.names = F)




################### 887B ##############################################

M887B <- read.csv("~/Dropbox/PhD/Placebo/Experiments/Experiment887B/data/887B_R_Qualtrics_Files/887BMaster.csv")

setnames(M887B, "ComplianceTest_Y1N0", "ComplianceT_Y1N0")

colsCaffTot <- c("numCupsCoffee",
                 "numCupsTea",
                 "numCola",
                 "numEnergyDrinks")

# replace NA in the following columns so you can use them for calculating total caffeine score
M887B[, colsCaffTot] <- replace(M887B[, colsCaffTot], is.na(M887B[, colsCaffTot]), 0)
  
# now create total caffeine score column
M887B$totCaffeine <- M887B$numCupsCoffee*85 +
                     M887B$numCupsTea*30 +
                     M887B$numCola*37 +
                     M887B$numEnergyDrinks*80 

# create single column for employment status

empFrame887B <- data.frame(M887B$emp_FT,
                           M887B$emp_Casual,
                           M887B$emp_Vol,
                           M887B$emp_UnEmp,
                           M887B$emp_Student,
                           M887B$emp_Retired,
                           M887B$emp_Other)

M887B$emp <- do.call(pmax, col(empFrame887B)*replace(empFrame887B, is.na(empFrame887B),0))

  
 oldCols887B <- c("Name", 
                 "Email",
                 "B1testtime",
                 "B2testtime",
                 "yearsCaffUse",
                 "numQuitAttempts",
                 "totCaffeine",
                 "numCupsCoffee",
                 "genderM1F0",
                 "age",
                 "emp",
                 "ComplianceBaseline_Y1N0",
                 "Verbal_CompliedY1N0",
                 "edLevel",
                 "maritalStat",
                 "langOtherEng",
                 "Paid_Y1N0",
                 "ScreenedY1N0",
                 paste("B1", CWSQQNames, sep=""),
                 paste("B2", CWSQQNames, sep=""))

setnames(M887B, oldCols887B, newCols)


# assign and reassign levels of factors in M887B

# reassign levels
M887B$Gender_F1M0 <- mapvalues(M887B$Gender_F1M0, from = c("F", "M"), to = c("female", "male"))

M887B$Verbal_CompliedY1N0 <- mapvalues(M887B$Verbal_CompliedY1N0,
                                       from = c("complied", "notComplied"), 
                                       to = c("complied", "didNotComply"))

M887B$Paid_Y1N0 <- mapvalues(M887B$Paid_Y1N0,
                             from = c("Paid", "NotPaid"),
                             to = c("paid", "coursecredit"))

M887B$Screened_Y1N0 <- mapvalues(M887B$Screened_Y1N0,
                                  from = c("Screened", "NotScreened"),
                                  to = c("screened", "notScreened"))


# assign levels
M887B$ComplianceTest_Y1N0 <- factor(M887B$ComplianceTest_Y1N0,
                                    levels = c(1, 0),
                                    labels = c("complied", "didNotComply"),
                                    ordered = F)

M887B$emp <- factor(M887B$emp, levels = c(1,2,3,4,5,6,7),
                    labels = c("fulltime",
                               "casual",
                               "volunteer",
                               "unemployed",
                               "student",
                               "retired",
                               "other"),
                    ordered = F)

M887B <- M887B[, which(names(M887B) %in% newCols)]

# convert dates from factors to dates. Notice the format is slightly different again to 974 and 887 above. This time what is different is that there are dashes instead of / in the original character string. This date business is pretty tricky

M887B$B1testtime <- as.POSIXct(M887B$B1testtime, format = "%Y-%m-%d %H:%M:%S")
M887B$B1testtime <- parse_date_time(M887B$B1testtime, "%Y/%m/%d %H:%M:%S") 
M887B$B2testtime <- as.POSIXct(M887B$B2testtime, format = "%Y-%m-%d %H:%M:%S")
M887B$B2testtime <- parse_date_time(M887B$B2testtime, "%Y/%m/%d %H:%M:%S") 




################## Convert so that columns of each dataframe are identical ###### 


######### Add dataset identifier ########################

M887$dSetID <- 1
M974$dSetID <- 2
M887B$dSetID <- 3


# convert to data table because I think the select command below only works on data tables
M974 <- data.table(M974)
M887 <- data.table(M887)
M887B <- data.table(M887B)


# this command is from dplyr. It orders the columns according to the order you specify in the brackets, and then orders everything after that as is with the 'everything()' command
M974 <- M974 %>% select(Name, Email, dSetID,
                                     yearsCaffUse,
                                     numQuitAttempts,
                                     totCaffeine,
                                     numCupsCoffee,
                                     Gender_F1M0,
                                     Paid_Y1N0,
                                     Screened_Y1N0,
                                     age,
                                     emp,
                                     ComplianceTest_Y1N0,
                                     Verbal_CompliedY1N0,
                                     edLevel,
                                     maritalStat,
                                     langOtherEng,
                                     B1testtime,
                                     B2testtime,
                                     everything())



M887 <- M887 %>% select(Name, Email, dSetID,
                                     yearsCaffUse,
                                     numQuitAttempts,
                                     totCaffeine,
                                     numCupsCoffee,
                                     Gender_F1M0,
                                     Paid_Y1N0,
                                     Screened_Y1N0,
                                     age,
                                     emp,
                                     ComplianceTest_Y1N0,
                                     Verbal_CompliedY1N0,
                                     edLevel,
                                     maritalStat,
                                     langOtherEng,
                                     B1testtime,
                                     B2testtime,
                                     everything())

M887B <- M887B %>% select(Name, Email, dSetID,
                        yearsCaffUse,
                        numQuitAttempts,
                        totCaffeine,
                        numCupsCoffee,
                        Gender_F1M0,
                        Paid_Y1N0,
                        Screened_Y1N0,
                        age,
                        emp,
                        ComplianceTest_Y1N0,
                        Verbal_CompliedY1N0,
                        edLevel,
                        maritalStat,
                        langOtherEng,
                        B1testtime,
                        B2testtime,
                        everything())



#just a check that everything is the same
# colnames(M887) == colnames(M974)
# colnames(M887B) == colnames(M974)



########## bind three sets together ##########################

mRFrame <- data.frame(rbind(M887, M974, M887B))



# write.csv(mRFrame, "~/Dropbox/PhD/Placebo/Experiments/CaffeinePredictorsOfWIthdrawalSeverity/data/mRframe.csv", row.names = F)


# make the data set ID a factor
mRFrame$dSetID <- factor(mRFrame$dSetID, levels = c(1,2,3),
                         labels = c("M887", "M974", "M887B"),
                         ordered = F)



######### now we need to make composite columns for factors and totals for the CWSQ data

# Making CWSQ factors


#factor vectors
DrowsyFatigued <- c("Q1drowsy", "Q3yawning",  "Q5tiredfatigued", "Q16sluggish")

DecreasedAlertDiffConc <- c("Q2selfconfident", "Q4alert", "Q6content", "Q7diffconc", "Q18clearheaded")

Mood <- c("Q8irritable", "Q10depressedmood", "Q11grouchy", "Q23discouraged")

DecreasedSocMotiv <- c("Q12urgework", "Q15talkative", "Q19desiresoc", "Q20energetic")

Nausea <- c("Q17upsetstomach", "Q21nauseavom", "Q24queasy", "Q25nauseaous", "Q26vomiting")

FluLike <- c("Q9heavyfeel", "Q13flulike", "Q22musclepain")

Headache <- c("Q14headache", "Q27headachey")

Acute <- c("Q28anxious", "Q29nervous", "Q30jittery")

Craving <- c("Q31cravingcoffee", "Q32cravingcaffeine")

###### B1

B1DrowsyFacNames <- paste("B1", DrowsyFatigued, sep="")
mRFrame$B1DrowsyFac <- rowSums(mRFrame[, B1DrowsyFacNames], na.rm=F)

B1DecreasedAlertDiffConcFacNames <- paste("B1", DecreasedAlertDiffConc, sep="")
mRFrame$B1DecAlertFac <- rowSums(mRFrame[, B1DecreasedAlertDiffConcFacNames], na.rm=F)

B1MoodFacNames <- paste("B1", Mood, sep="")
mRFrame$B1MoodFac <- rowSums(mRFrame[, B1MoodFacNames], na.rm=F)

B1DecreasedSocMotivFacNames <- paste("B1", DecreasedSocMotiv, sep="")
mRFrame$B1DecreasedSocMotivFac <- rowSums(mRFrame[, B1DecreasedSocMotivFacNames], na.rm=F)

B1NauseaFacNames <- paste("B1", Nausea, sep="")
mRFrame$B1NauseaFac <- rowSums(mRFrame[, B1NauseaFacNames], na.rm=F)

B1FluLikeFacNames <- paste("B1", FluLike, sep="")
mRFrame$B1FluLikeFac <- rowSums(mRFrame[, B1FluLikeFacNames], na.rm=F)

B1HeadacheFacNames <- paste("B1", Headache, sep="")
mRFrame$B1HeadacheFac <- rowSums(mRFrame[, B1HeadacheFacNames], na.rm=F)

B1AcuteFacNames <- paste("B1", Acute, sep="")
mRFrame$B1AcuteFac <- rowSums(mRFrame[, B1AcuteFacNames], na.rm=F)

B1CravingFacNames <- paste("B1", Craving, sep="")
mRFrame$B1CravingFac <- rowSums(mRFrame[, B1CravingFacNames], na.rm=F)

#### B2

B2DrowsyFacNames <- paste("B2", DrowsyFatigued, sep="")
mRFrame$B2DrowsyFac <- rowSums(mRFrame[, B2DrowsyFacNames], na.rm=F)

B2DecreasedAlertDiffConcFacNames <- paste("B2", DecreasedAlertDiffConc, sep="")
mRFrame$B2DecAlertFac <- rowSums(mRFrame[, B2DecreasedAlertDiffConcFacNames], na.rm=F)

B2MoodFacNames <- paste("B2", Mood, sep="")
mRFrame$B2MoodFac <- rowSums(mRFrame[, B2MoodFacNames], na.rm=F)

B2DecreasedSocMotivFacNames <- paste("B2", DecreasedSocMotiv, sep="")
mRFrame$B2DecreasedSocMotivFac <- rowSums(mRFrame[, B2DecreasedSocMotivFacNames], na.rm=F)

B2NauseaFacNames <- paste("B2", Nausea, sep="")
mRFrame$B2NauseaFac <- rowSums(mRFrame[, B2NauseaFacNames], na.rm=F)

B2FluLikeFacNames <- paste("B2", FluLike, sep="")
mRFrame$B2FluLikeFac <- rowSums(mRFrame[, B2FluLikeFacNames], na.rm=F)

B2HeadacheFacNames <- paste("B2", Headache, sep="")
mRFrame$B2HeadacheFac <- rowSums(mRFrame[, B2HeadacheFacNames], na.rm=F)

B2AcuteFacNames <- paste("B2", Acute, sep="")
mRFrame$B2AcuteFac <- rowSums(mRFrame[, B2AcuteFacNames], na.rm=F)

B2CravingFacNames <- paste("B2", Craving, sep="")
mRFrame$B2CravingFac <- rowSums(mRFrame[, B2CravingFacNames], na.rm=F)


##### total CWSQ scores ##########

mRFrameCWSQNames <- c("Q1drowsy",
                      "Q2selfconfident",
                      "Q3yawning",
                      "Q4alert",
                      "Q5tiredfatigued",
                      "Q6content",
                      "Q7diffconc",
                      "Q8irritable",
                      "Q9heavyfeel",
                      "Q10depressedmood",
                      "Q11grouchy",
                      "Q12urgework",
                      "Q13flulike",
                      "Q14headache",
                      "Q15talkative",
                      "Q16sluggish",
                      "Q17upsetstomach",
                      "Q18clearheaded",
                      "Q19desiresoc",
                      "Q20energetic",
                      "Q21nauseavom",
                      "Q22musclepain",
                      "Q23discouraged",
                      "Q24queasy",
                      "Q25nauseaous",
                      "Q26vomiting",
                      "Q27headachey",
                      "Q28anxious",
                      "Q29nervous",
                      "Q30jittery",
                      "Q31cravingcoffee",
                      "Q32cravingcaffeine")


# B1 Total

B1Names <- paste("B1", mRFrameCWSQNames, sep="")
mRFrame$B1Total <- rowSums(mRFrame[, B1Names], na.rm = F) 

# B2 Total

B2Names <- paste("B2", mRFrameCWSQNames, sep="")
mRFrame$B2Total <- rowSums(mRFrame[, B2Names], na.rm = F) 


# change non-grouping variables to dates or chracter vectors


mRFrame$Name <- as.character(mRFrame$Name)
mRFrame$Email <- as.character(mRFrame$Email)


# add ID column
mRFrame$ID <- rownames(mRFrame)

# reorder columns so that ID goes first
mRFrame <- mRFrame %>% select(ID, everything())


write.csv(mRFrame, "~/Dropbox/PhD/Placebo/Experiments/CaffeinePredictorsOfWIthdrawalSeverity/data/masterRegression.csv", row.names = FALSE)



