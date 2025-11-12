## Stauder et al. 2023, Examining cross-sectional and longitudinal relationships between multidomain physical fitness metrics, education, and cognitions in Black older adults
## Aging, Neuropsychology, and Cognition, Volume 31, Issue 7, Published 03 July 2024
## Part I: Script for downloading and cleaning HRS RAND dataset 
## Part II: Script for analyses
## 03 January 2025

################################################################################
# PART I - DATASET CREATION
################################################################################

# Download your libraries

library('foreign') 
library('readxl')
library('dplyr')
library("sas7bdat")

# set your directory. wherever RAND data is saved.
setwd("C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/HRS_onedrive_orig/RawData/randhrs1992_2018v1_SPSS/")

# Load the data into R. 
# the extension for an spss dataframe is .sav

RAND = read.spss("randhrs1992_2018v1.sav", to.data.frame = TRUE)

# Predictor variable names
# 'w' is a place holder for wave #
# Breathing test
# - RwPUFF:       R10PUFF, R11PUFF, R12PUFF, R13PUFF
# Grip Strength
# - RwGRP:        R10GRP, R11GRP, R12GRP, R13GRP
# Timed Walk
# - RwTIMWALK:    R10TIMWALK, R11TIMWALK, R12TIMWALK, R13TIMWALK
# BMI
# - RwPMBMI:      R10PMBMI, R11PMBMI, R12PMBMI, R13PMBMI

# Outcome variable name(s)
# 'w' is a place holder for wave #
# Total word recall summary score: immediate + delayed word recall scores
# - RwTR20:       R10TR20, R11TR20, R12TR20, R13TR20
# Mental status summary score: serial 7s, backwards counting, object-, date-, and president/vice president-naming tasks 
# - RwMSTOT:      R10MSTOT, R11MSTOT, R12MSTOT, R13MSTOT
#Total cognition score: sum total word recall and mental status summary score
# - RwCOGTOT:     R10COGTOT, R11COGTOT, R12COGTOT, R13COGTOT


# Exclusion Criteria variable names
# 1. Diagnosis of AD or dementia
# 2. Hx of stroke
# 3. Depressive symptom burden
# 4. Psychiatric diagnosis
# 5. Excessive binge drinking
# 6. >3 SD from mean on fitness variables - WILL BE DONE LATER

#R8 + R9 did not ask participants if they have ever had a doctor diagnose them with AD/dementia - only if a doctor has told them they have a memory problem
#R14 did not have COGTOT variable
#restricted dataset to waves R10-R13


# see the dataframe 
View(RAND)

# search just colnames
colnames(RAND)
View(colnames(RAND))

#Age/Gender - if/when it ever comes up
# RAGENDER
# 1.Male
# 2.Female
# RwAGEY_B- age in years at beginning of month of visit
# RAEDYRS - years of education
# RwIWEND - end date of interview

#Extract population of interest = only Black Older Adults (BOA)

RAND_BOA <- RAND %>% filter(RARACEM == "2.Black/African American")
# n=8038

################################################################################
#Filter by exclusion criteria

#Apply Exclusion Criteria

# 1. Diagnosis of AD or dementia
#R10 - R13: RwALZHEE or RwDEMENE, ever had AD/Dementia diagnosis
#0 = No, 1 = Yes

RAND_BOA_R10 <- RAND_BOA %>% filter(RAND_BOA$R10ALZHEE == "0.No")
# n=4185
RAND_BOA_R10 <- RAND_BOA_R10 %>% filter(RAND_BOA_R10$R10DEMENE == "0.No")
# n=4118

RAND_BOA_R11 <- RAND_BOA %>% filter(RAND_BOA$R11ALZHEE == "0.No")
# n=3889
RAND_BOA_R11 <- RAND_BOA_R11 %>% filter(RAND_BOA_R11$R11DEMENE == "0.No")
# n=3804

RAND_BOA_R12 <- RAND_BOA %>% filter(RAND_BOA$R12ALZHEE == "0.No")
# n=3611
RAND_BOA_R12 <- RAND_BOA_R12 %>% filter(RAND_BOA_R12$R12DEMENE == "0.No")
# n=3520

RAND_BOA_R13 <- RAND_BOA %>% filter(RAND_BOA$R13ALZHEE == "0.No")
# n=4444
RAND_BOA_R13 <- RAND_BOA_R13 %>% filter(RAND_BOA_R13$R13DEMENE == "0.No")
# n=4338

# 2. Hx of stroke
#R10 - R13: RwSTROKE, ever had doctor diagnosed stroke
#0 = No, 1 = Yes, 2 = TIA/possible stroke

RAND_BOA_R10 <- RAND_BOA_R10 %>% filter(RAND_BOA_R10$R10STROKE == "0.No")
# n=3752

RAND_BOA_R11 <- RAND_BOA_R11 %>% filter(RAND_BOA_R11$R11STROKE == "0.No")
# n=3453

RAND_BOA_R12 <- RAND_BOA_R12 %>% filter(RAND_BOA_R12$R12STROKE == "0.No")
# n=3183

RAND_BOA_R13 <- RAND_BOA_R13 %>% filter(RAND_BOA_R13$R13STROKE == "0.No")
# n=3948


# 3. Depressive symptom burden
# R10 - R13: RwCESD
# Cutoff: >= 4

RAND_BOA_R10 <- RAND_BOA_R10 %>% filter(RAND_BOA_R10$R10CESD < 5)
# n=3177
RAND_BOA_R11 <- RAND_BOA_R11 %>% filter(RAND_BOA_R11$R11CESD < 5)
# n=2943
RAND_BOA_R12 <- RAND_BOA_R12 %>% filter(RAND_BOA_R12$R12CESD < 5)
# n=2698
RAND_BOA_R13 <- RAND_BOA_R13 %>% filter(RAND_BOA_R13$R13CESD < 5)
# n=3448                                     

# 4. Psychiatric diagnosis
#R10 - R13: RwPSYCHE
#0 = No, 1 = Yes

RAND_BOA_R10 <- RAND_BOA_R10 %>% filter(RAND_BOA_R10$R10PSYCHE == "0.No")
# n=2890
RAND_BOA_R11 <- RAND_BOA_R11 %>% filter(RAND_BOA_R11$R11PSYCHE == "0.No")
# n=2646
RAND_BOA_R12 <- RAND_BOA_R12 %>% filter(RAND_BOA_R12$R12PSYCHE == "0.No")
# n=2401
RAND_BOA_R13 <- RAND_BOA_R13 %>% filter(RAND_BOA_R13$R13PSYCHE == "0.No")
# n=3045

# 5. Excessive binge drinking
#RwDRINKD - # days/week drinks
#RwDRINKN - # drinks/day when drinks

RAND_BOA_R10 <- RAND_BOA_R10 %>% filter((R10DRINKN == "3") | (R10DRINKN == "2") | (R10DRINKN == "1") | (R10DRINKN == "0.0 or doesnt drink") | is.na(R10DRINKN) == TRUE)
# n=2744
RAND_BOA_R11 <- RAND_BOA_R11 %>% filter((R11DRINKN == "3") | (R11DRINKN == "2") | (R11DRINKN == "1") | (R11DRINKN == "0.0 or doesnt drink") | is.na(R11DRINKN) == TRUE)
# n=2542
RAND_BOA_R12 <- RAND_BOA_R12 %>% filter((R12DRINKN == "3") | (R12DRINKN == "2") | (R12DRINKN == "1") | (R12DRINKN == "0.0 or doesnt drink") | is.na(R12DRINKN) == TRUE)
# n=2310
RAND_BOA_R13 <- RAND_BOA_R13 %>% filter((R13DRINKN == "3") | (R13DRINKN == "2") | (R13DRINKN == "1") | (R13DRINKN == "0.0 or doesnt drink") | is.na(R13DRINKN) == TRUE)
# n=2894


################################################################################
# making a dataframe for each wave 

RAND_BOA_R10 <- RAND_BOA_R10 %>% select(HHIDPN, RAGENDER, RAEDYRS, R10CONDE, R10AGEM_B, R10IWEND, R10BMI, R10GRP, R10PUFF, R10TIMWLK, R10TR20, R10IMRC, R10DLRC, R10MSTOT, R10COGTOT, H10ATOTB, H10ICAP, R10CENREG, R10VGACTX, R10MDACTX, R10LTACTX)
RAND_BOA_R11 <- RAND_BOA_R11 %>% select(HHIDPN, RAGENDER, RAEDYRS, R11CONDE, R11AGEM_B, R11IWEND, R11BMI, R11GRP, R11PUFF, R11TIMWLK, R11TR20, R11IMRC, R11DLRC, R11MSTOT, R11COGTOT, H11ATOTB, H11ICAP, R11CENREG, R11VGACTX, R11MDACTX, R11LTACTX)
RAND_BOA_R12 <- RAND_BOA_R12 %>% select(HHIDPN, RAGENDER, RAEDYRS, R12CONDE, R12AGEM_B, R12IWEND, R12BMI, R12GRP, R12PUFF, R12TIMWLK, R12TR20, R12IMRC, R12DLRC, R12MSTOT, R12COGTOT, H12ATOTB, H12ICAP, R12CENREG, R12VGACTX, R12MDACTX, R12LTACTX)
RAND_BOA_R13 <- RAND_BOA_R13 %>% select(HHIDPN, RAGENDER, RAEDYRS, R13CONDE, R13AGEM_B, R13IWEND, R13BMI, R13GRP, R13PUFF, R13TIMWLK, R13TR20, R13IMRC, R13DLRC, R13MSTOT, R13COGTOT, H12ATOTB, H12ICAP, R13CENREG, R13VGACTX, R13MDACTX, R13LTACTX)

################################################################################
# Create variables to eventually group waves together for analysis 
# '1012' = participants who participated in Waves 10 & 12
# '1113' = participants who participated in Waves 11 & 13
# reminder: protocol had participants complete testing in alternating waves. waves were completed every 2 years.

RAND_BOA_R10$Group <- 1012
RAND_BOA_R11$Group <- 1113
RAND_BOA_R12$Group <- 1012
RAND_BOA_R13$Group <- 1113

# make a dataframe of only people that have all the variables
RAND_BOA_R10_comp <- RAND_BOA_R10[complete.cases(RAND_BOA_R10[, c(1:15)]),]
RAND_BOA_R11_comp <- RAND_BOA_R11[complete.cases(RAND_BOA_R11[, c(1:15)]),]
RAND_BOA_R12_comp <- RAND_BOA_R12[complete.cases(RAND_BOA_R12[, c(1:15)]),]
RAND_BOA_R13_comp <- RAND_BOA_R13[complete.cases(RAND_BOA_R13[, c(1:15)]),]

# now see if those people from two waves are the same 
RAND1012 <- inner_join(RAND_BOA_R10_comp,RAND_BOA_R12_comp,by=c('HHIDPN', 'RAGENDER', 'RAEDYRS', 'Group'))
RAND1113 <- inner_join(RAND_BOA_R11_comp,RAND_BOA_R13_comp,by=c('HHIDPN', 'RAGENDER', 'RAEDYRS', 'Group'))


sum(complete.cases(RAND1012))
# n=170
# between wave 10 and wave 12, for fitness and cognition we have 170 with full data
sum(complete.cases(RAND1113))
# n=170
# between wave 11 and wave 13, for fitness and cognition we have 170 with full data

################################################################################
# Inner joining participants from Waves 10/12 and 11/13 

All_BOA <- full_join(RAND1012, RAND1113, by=c('HHIDPN', 'RAGENDER', 'RAEDYRS', 'Group'))

#save(All_BOA,file="C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HRS/BOA/Stauder_FYP_BOA_Long-Fit-Cog/All_BOA")

################################################################################
# Data Exploration - confirming interjoining didn't impose concerns

table(RAND1012$R10COGTOT)
table(RAND1012$R10BMI)
table(RAND1012$R10GRP)
table(RAND1012$R10PUFF)
table(RAND1012$R10TIMWLK)

table(RAND1012$R12COGTOT)
table(RAND1012$R12BMI)
table(RAND1012$R12GRP)
table(RAND1012$R12PUFF)
table(RAND1012$R12TIMWLK)

table(RAND1113$R11COGTOT)
table(RAND1113$R11BMI)
table(RAND1113$R11GRP)
table(RAND1113$R11PUFF)
table(RAND1113$R11TIMWLK)

table(RAND1113$R13COGTOT)
table(RAND1113$R13BMI)
table(RAND1113$R13GRP)
table(RAND1113$R13PUFF)
table(RAND1113$R13TIMWLK)
################################################################################



################################################################################
# PART II - ANALYSIS
################################################################################

### Analysis Script ###

library('foreign') 
library('readxl')
library('dplyr')
library('sas7bdat')
library('ggplot2')
library('ggsignif')
library("plotrix")

load("C:/Users/staud/OneDrive - The Ohio State University/BBAL/Projects/BADS_2021E0199/HRS/BOA/Stauder_FYP_BOA_Long-Fit-Cog/All_BOA")


################################################################################
# Inner joining Waves 10/11 and 12/13 into Timepoints 1 + 2 

# Years for slope
All_BOA$Time_Bw <- ifelse(All_BOA$Group=="1012", All_BOA$R12IWEND - All_BOA$R10IWEND, All_BOA$R13IWEND - All_BOA$R11IWEND)
All_BOA$Time_Bw_Y <- All_BOA$Time_Bw / 365

# BMI (body mass index)
All_BOA$T1BMI <- ifelse(All_BOA$Group=="1012",All_BOA$R10BMI,All_BOA$R11BMI)
All_BOA$T2BMI <- ifelse(All_BOA$Group=="1012",All_BOA$R12BMI,All_BOA$R13BMI)

# GRP (grip strength)
All_BOA$T1GRP <- ifelse(All_BOA$Group=="1012",All_BOA$R10GRP,All_BOA$R11GRP)
All_BOA$T2GRP <- ifelse(All_BOA$Group=="1012",All_BOA$R12GRP,All_BOA$R13GRP)

# PUFF (lung capacity)
All_BOA$T1PUFF <- ifelse(All_BOA$Group=="1012",All_BOA$R10PUFF,All_BOA$R11PUFF)
All_BOA$T2PUFF <- ifelse(All_BOA$Group=="1012",All_BOA$R12PUFF,All_BOA$R13PUFF)

# TIMWALK (timed walk)
All_BOA$T1TIMWALK <- ifelse(All_BOA$Group=="1012",All_BOA$R10TIMWLK,All_BOA$R11TIMWLK)
All_BOA$T2TIMWALK <- ifelse(All_BOA$Group=="1012",All_BOA$R12TIMWLK,All_BOA$R13TIMWLK)

# COGTOT (TICS total score)
All_BOA$T1COGTOT <- ifelse(All_BOA$Group=="1012",All_BOA$R10COGTOT,All_BOA$R11COGTOT)
All_BOA$T2COGTOT <- ifelse(All_BOA$Group=="1012",All_BOA$R12COGTOT,All_BOA$R13COGTOT)

# MSTOT (mental status total score)
All_BOA$T1MSTOT <- ifelse(All_BOA$Group=="1012",All_BOA$R10MSTOT,All_BOA$R11MSTOT)
All_BOA$T2MSTOT <- ifelse(All_BOA$Group=="1012",All_BOA$R12MSTOT,All_BOA$R13MSTOT)

# TR20 (total recall score)
All_BOA$T1TR20 <- ifelse(All_BOA$Group=="1012",All_BOA$R10TR20,All_BOA$R11TR20)
All_BOA$T2TR20 <- ifelse(All_BOA$Group=="1012",All_BOA$R12TR20,All_BOA$R13TR20)

# IMRC (immediate recall score)
All_BOA$T1IMRC <- ifelse(All_BOA$Group=="1012",All_BOA$R10IMRC,All_BOA$R11IMRC)
All_BOA$T2IMRC <- ifelse(All_BOA$Group=="1012",All_BOA$R12IMRC,All_BOA$R13IMRC)

# DLRC (delayed recall score)
All_BOA$T1DLRC <- ifelse(All_BOA$Group=="1012",All_BOA$R10DLRC,All_BOA$R11DLRC)
All_BOA$T2DLRC <- ifelse(All_BOA$Group=="1012",All_BOA$R12DLRC,All_BOA$R13DLRC)


# AGE
All_BOA$T1AGE <- ifelse(All_BOA$Group=="1012",All_BOA$R10AGEM_B,All_BOA$R11AGEM_B)
All_BOA$T2AGE <- ifelse(All_BOA$Group=="1012",All_BOA$R12AGEM_B,All_BOA$R13AGEM_B)

# IWEND (interview end date)
All_BOA$T1IWEND <- ifelse(All_BOA$Group=="1012",All_BOA$R10IWEND,All_BOA$R11IWEND)
All_BOA$T2IWEND <- ifelse(All_BOA$Group=="1012",All_BOA$R12IWEND,All_BOA$R13IWEND)

# CONDE (Health Conditions)
All_BOA$T1CONDE <- ifelse(All_BOA$Group=="1012",All_BOA$R10CONDE,All_BOA$R11CONDE)
All_BOA$T2CONDE <- ifelse(All_BOA$Group=="1012",All_BOA$R12CONDE,All_BOA$R13CONDE)


################################################################################
# Variables included for descriptives / sensitivity analyses suggested by reviewers

# ICAP (Income)
All_BOA$T1INCOME <- ifelse(All_BOA$Group=="1012",All_BOA$H10ICAP,All_BOA$H11ICAP)

# ATOTB (Wealth)
All_BOA$T1WEALTH <- ifelse(All_BOA$Group=="1012",All_BOA$H10ATOTB,All_BOA$H11ATOTB)

# CENREG (Census Region)
All_BOA$T1CENREG <- ifelse(All_BOA$Group=="1012",All_BOA$R10CENREG,All_BOA$R11CENREG)

# LTACTX (Light Activity)
All_BOA$T1LTACT <- ifelse(All_BOA$Group=="1012",All_BOA$R10LTACTX,All_BOA$R11LTACTX)
All_BOA$T2LTACT <- ifelse(All_BOA$Group=="1012",All_BOA$R12LTACTX,All_BOA$R13LTACTX)

# MDACT (Moderate Activity)
All_BOA$T1MDACT <- ifelse(All_BOA$Group=="1012",All_BOA$R10MDACTX,All_BOA$R11MDACTX)
All_BOA$T2MDACT <- ifelse(All_BOA$Group=="1012",All_BOA$R12MDACTX,All_BOA$R13MDACTX)

# VGACT (Vigorous Activity)
All_BOA$T1VGACT <- ifelse(All_BOA$Group=="1012",All_BOA$R10VGACTX,All_BOA$R11VGACTX)
All_BOA$T2VGACT <- ifelse(All_BOA$Group=="1012",All_BOA$R12VGACTX,All_BOA$R13VGACTX)


################################################################################
# Select Columns want to keep

#Final_BOA <- All_BOA %>% select('HHIDPN', 'RAGENDER', 'RAEDYRS', 'T1BMI', 'T2BMI', 'T1GRP', 'T2GRP', 'T1PUFF', 'T2PUFF', 'T1TIMWALK', 'T2TIMWALK', 'T1COGTOT', 'T2COGTOT', 'T1AGE', 'T2AGE', 'T1IWEND', 'T2IWEND')

################################################################################
# Check + remove outlier values

#install.packages("ggstatsplot")
#library(ggstatsplot)

#install.packages("purrr")
#library(purrr)

scattOut <- function(VOI) {
  upper3 = mean(VOI,na.rm=TRUE) + 3*sd(VOI,na.rm=TRUE)
  lower3 =  mean(VOI,na.rm=TRUE) - 3*sd(VOI,na.rm=TRUE)
  upper5 = mean(VOI,na.rm=TRUE) + 5*sd(VOI,na.rm=TRUE)
  lower5 =  mean(VOI,na.rm=TRUE) - 5*sd(VOI,na.rm=TRUE)
  plot(VOI)
  abline(h=upper3,col="blue")
  abline(h=lower3,col="blue")
  abline(h=upper5,col="red")
  abline(h=lower5,col="red")
}

################################################################################
# variable transformations

# setting education variable to be bounded 0-17 years
levels(All_BOA$RAEDYRS)[19] <- 0
levels(All_BOA$RAEDYRS)[20] <- 17
All_BOA$RAEDYRS[All_BOA$RAEDYRS=="17.17+ yrs"] <- 17 
All_BOA$RAEDYRS[All_BOA$RAEDYRS=="0.None"] <- 0
All_BOA$RAEDYRS <- as.numeric(as.character(All_BOA$RAEDYRS))

# Male/Female orinally coded as 1/2, subtracting 1 for males to be reference group labeled '0'
All_BOA$RAGENDER <- as.numeric(All_BOA$RAGENDER)-1
#male = 0, female = 1
All_BOA$RAGENDER_Label <- ifelse(All_BOA$RAGENDER=="0", "Male", "Female")

# age originally reported in months, dividing by 12 to set age in years
All_BOA$T1AGE_Y <- All_BOA$T1AGE / 12
All_BOA$T2AGE_Y <- All_BOA$T2AGE / 12

################################################################################
# Variable Slopes:  Timepoint 2 value minus Timepoint 1 value divided by time between timepoints

All_BOA$COGTOT_Slope <- (All_BOA$T2COGTOT - All_BOA$T1COGTOT) / All_BOA$Time_Bw_Y
All_BOA$COGTOT_Slope_Bin <- ifelse(All_BOA$COGTOT_Slope >= 0,1,0)
# COGTOT_Slope_Bin = establishing maintainers (0 or positive slope) vs decliners (negative slope)
All_BOA$MSTOT_Slope <- (All_BOA$T2MSTOT - All_BOA$T1MSTOT) / All_BOA$Time_Bw_Y
All_BOA$TR20_Slope <- (All_BOA$T2TR20 - All_BOA$T1TR20) / All_BOA$Time_Bw_Y
All_BOA$IMRC_Slope <- (All_BOA$T2IMRC - All_BOA$T1IMRC) / All_BOA$Time_Bw_Y
All_BOA$DLRC_Slope <- (All_BOA$T2DLRC - All_BOA$T1DLRC) / All_BOA$Time_Bw_Y
All_BOA$GRP_Slope <- (All_BOA$T2GRP - All_BOA$T1GRP) / All_BOA$Time_Bw_Y
All_BOA$PUFF_Slope <- (All_BOA$T2PUFF - All_BOA$T1PUFF) / All_BOA$Time_Bw_Y
All_BOA$TIMWALK_Slope <- (All_BOA$T2TIMWALK - All_BOA$T1TIMWALK) / All_BOA$Time_Bw_Y
All_BOA$BMI_Slope <- (All_BOA$T2BMI - All_BOA$T1BMI) / All_BOA$Time_Bw_Y

# Variable Change: Timepoint 2 value minus Timepoint 1 value

All_BOA$COGTOT_Change <- (All_BOA$T2COGTOT - All_BOA$T1COGTOT)
All_BOA$COGTOT_Change_Bin <- ifelse(All_BOA$COGTOT_Change >= 0,1,0)
# COGTOT_Change_Bin = establishing maintainers (0 or positive change) vs decliners (negative change)
All_BOA$MSTOT_Change <- (All_BOA$T2MSTOT - All_BOA$T1MSTOT)
All_BOA$TR20_Change <- (All_BOA$T2TR20 - All_BOA$T1TR20)
All_BOA$GRP_Change <- (All_BOA$T2GRP - All_BOA$T1GRP)
All_BOA$PUFF_Change <- (All_BOA$T2PUFF - All_BOA$T1PUFF)
All_BOA$TIMWALK_Change <- (All_BOA$T2TIMWALK - All_BOA$T1TIMWALK)
All_BOA$BMI_Change <- (All_BOA$T2BMI - All_BOA$T1BMI)

################################################################################
#Z-score all predictor and outcome variables for analysis interpretation

All_BOA$T1BMI_Z <- (All_BOA$T1BMI - mean(All_BOA$T1BMI))/sd(All_BOA$T1BMI)
All_BOA$T2BMI_Z <- (All_BOA$T2BMI - mean(All_BOA$T2BMI))/sd(All_BOA$T2BMI)

All_BOA$T1GRP_Z <- (All_BOA$T1GRP - mean(All_BOA$T1GRP))/sd(All_BOA$T1GRP)
All_BOA$T2GRP_Z <- (All_BOA$T2GRP - mean(All_BOA$T2GRP))/sd(All_BOA$T2GRP)

All_BOA$T1PUFF_Z <- (All_BOA$T1PUFF - mean(All_BOA$T1PUFF))/sd(All_BOA$T1PUFF)
All_BOA$T2PUFF_Z <- (All_BOA$T2PUFF - mean(All_BOA$T2PUFF))/sd(All_BOA$T2PUFF)

All_BOA$T1TIMWALK_Z <- (All_BOA$T1TIMWALK - mean(All_BOA$T1TIMWALK))/sd(All_BOA$T1TIMWALK)
All_BOA$T2TIMWALK_Z <- (All_BOA$T2TIMWALK - mean(All_BOA$T2TIMWALK))/sd(All_BOA$T2TIMWALK)

All_BOA$T1COGTOT_Z <- (All_BOA$T1COGTOT - mean(All_BOA$T1COGTOT))/sd(All_BOA$T1COGTOT)
All_BOA$T2COGTOT_Z <- (All_BOA$T2COGTOT - mean(All_BOA$T2COGTOT))/sd(All_BOA$T2COGTOT)

All_BOA$T1MSTOT_Z <- (All_BOA$T1MSTOT - mean(All_BOA$T1MSTOT))/sd(All_BOA$T1MSTOT)
All_BOA$T2MSTOT_Z <- (All_BOA$T2MSTOT - mean(All_BOA$T2MSTOT))/sd(All_BOA$T2MSTOT)

All_BOA$T1TR20_Z <- (All_BOA$T1TR20 - mean(All_BOA$T1TR20))/sd(All_BOA$T1TR20)
All_BOA$T2TR20_Z <- (All_BOA$T2TR20 - mean(All_BOA$T2TR20))/sd(All_BOA$T2TR20)

All_BOA$T1IMRC_Z <- (All_BOA$T1IMRC - mean(All_BOA$T1IMRC))/sd(All_BOA$T1IMRC)
All_BOA$T2IMRC_Z <- (All_BOA$T2IMRC - mean(All_BOA$T2IMRC))/sd(All_BOA$T2IMRC)

All_BOA$T1DLRC_Z <- (All_BOA$T1DLRC - mean(All_BOA$T1DLRC))/sd(All_BOA$T1DLRC)
All_BOA$T2DLRC_Z <- (All_BOA$T2DLRC - mean(All_BOA$T2DLRC))/sd(All_BOA$T2DLRC)

All_BOA$T1AGE_Y_Z <- (All_BOA$T1AGE_Y - mean(All_BOA$T1AGE_Y))/sd(All_BOA$T1AGE_Y)
All_BOA$T2AGE_Y_Z <- (All_BOA$T2AGE_Y - mean(All_BOA$T2AGE_Y))/sd(All_BOA$T2AGE_Y)

All_BOA$RAEDYRS_Z <- (All_BOA$RAEDYRS - mean(All_BOA$RAEDYRS))/sd(All_BOA$RAEDYRS)

All_BOA$T1CONDE_Z <- (All_BOA$T1CONDE - mean(All_BOA$T1CONDE))/sd(All_BOA$T1CONDE)
All_BOA$T2CONDE_Z <- (All_BOA$T2CONDE - mean(All_BOA$T2CONDE))/sd(All_BOA$T2CONDE)



#Z-score Slopes
All_BOA$COGTOT_Slope_Z <- (All_BOA$COGTOT_Slope - mean(All_BOA$COGTOT_Slope)) / sd(All_BOA$COGTOT_Slope)
All_BOA$MSTOT_Slope_Z <- (All_BOA$MSTOT_Slope - mean(All_BOA$MSTOT_Slope)) / sd(All_BOA$MSTOT_Slope)
All_BOA$TR20_Slope_Z <- (All_BOA$TR20_Slope - mean(All_BOA$TR20_Slope)) / sd(All_BOA$TR20_Slope)
All_BOA$IMRC_Slope_Z <- (All_BOA$IMRC_Slope - mean(All_BOA$IMRC_Slope))/sd(All_BOA$IMRC_Slope)
All_BOA$DLRC_Slope_Z <- (All_BOA$DLRC_Slope - mean(All_BOA$DLRC_Slope))/sd(All_BOA$DLRC_Slope)

All_BOA$GRP_Slope_Z <- (All_BOA$GRP_Slope - mean(All_BOA$GRP_Slope)) / sd(All_BOA$GRP_Slope)
All_BOA$PUFF_Slope_Z <- (All_BOA$PUFF_Slope - mean(All_BOA$PUFF_Slope)) / sd(All_BOA$PUFF_Slope)
All_BOA$TIMWALK_Slope_Z <- (All_BOA$TIMWALK_Slope - mean(All_BOA$TIMWALK_Slope)) / sd(All_BOA$TIMWALK_Slope)
All_BOA$BMI_Slope_Z <- (All_BOA$BMI_Slope - mean(All_BOA$BMI_Slope)) / sd(All_BOA$BMI_Slope)

#Z-score Change
All_BOA$COGTOT_Change_Z <- (All_BOA$COGTOT_Change - mean(All_BOA$COGTOT_Change)) / sd(All_BOA$COGTOT_Change)
All_BOA$MSTOT_Change_Z <- (All_BOA$MSTOT_Change - mean(All_BOA$MSTOT_Change)) / sd(All_BOA$MSTOT_Change)
All_BOA$TR20_Change_Z <- (All_BOA$TR20_Change - mean(All_BOA$TR20_Change)) / sd(All_BOA$TR20_Change)
All_BOA$GRP_Change_Z <- (All_BOA$GRP_Change - mean(All_BOA$GRP_Change)) / sd(All_BOA$GRP_Change)
All_BOA$PUFF_Change_Z <- (All_BOA$PUFF_Change - mean(All_BOA$PUFF_Change)) / sd(All_BOA$PUFF_Change)
All_BOA$TIMWALK_Change_Z <- (All_BOA$TIMWALK_Change - mean(All_BOA$TIMWALK_Change)) / sd(All_BOA$TIMWALK_Change)
All_BOA$BMI_Change_Z <- (All_BOA$BMI_Change - mean(All_BOA$BMI_Change)) / sd(All_BOA$BMI_Change)

################################################################################
#Remove T1 outliers for fitness predictors and cognitive outcomes

T1BMI_out <- which(All_BOA$T1BMI > (mean(All_BOA$T1BMI) + 3*sd(All_BOA$T1BMI)) | All_BOA$T1BMI < (mean(All_BOA$T1BMI) - 3*sd(All_BOA$T1BMI)))
T1GRP_out <- which(All_BOA$T1GRP > (mean(All_BOA$T1GRP) + 3*sd(All_BOA$T1GRP)) | All_BOA$T1GRP < (mean(All_BOA$T1GRP) - 3*sd(All_BOA$T1GRP)))
T1PUFF_out <- which(All_BOA$T1PUFF > (mean(All_BOA$T1PUFF) + 3*sd(All_BOA$T1PUFF)) | All_BOA$T1PUFF < (mean(All_BOA$T1PUFF) - 3*sd(All_BOA$T1PUFF)))
T1TIMWALK_out <- which(All_BOA$T1TIMWALK > (mean(All_BOA$T1TIMWALK) + 3*sd(All_BOA$T1TIMWALK)) | All_BOA$T1TIMWALK < (mean(All_BOA$T1TIMWALK) - 3*sd(All_BOA$T1TIMWALK)))
T1COGTOT_out <- which(All_BOA$T1COGTOT > (mean(All_BOA$T1COGTOT) + 3*sd(All_BOA$T1COGTOT)) | All_BOA$T1COGTOT < (mean(All_BOA$T1COGTOT) - 3*sd(All_BOA$T1COGTOT)))
COGTOT_Slope_out <- which(All_BOA$COGTOT_Slope > (mean(All_BOA$COGTOT_Slope) + 3*sd(All_BOA$COGTOT_Slope)) | All_BOA$COGTOT_Slope < (mean(All_BOA$COGTOT_Slope) - 3*sd(All_BOA$COGTOT_Slope)))

Outliers_out <- c(T1BMI_out, T1GRP_out, T1PUFF_out, T1TIMWALK_out, T1COGTOT_out, COGTOT_Slope_out)

All_BOA = All_BOA[-c(Outliers_out),]

################################################################################
# - Sample Descriptive Statistics (Table 1)

mean(All_BOA$T1AGE_Y)
# 72.81931
sd(All_BOA$T1AGE_Y)
# 4.812538
range(All_BOA$T1AGE_Y)
# 65-90

sum(All_BOA$RAGENDER)
# number of women = 194/321, 60.43%

mean(All_BOA$RAEDYRS)
# 12.3053
sd(All_BOA$RAEDYRS)
# 2.921943
range(All_BOA$RAEDYRS)
# 0-17

mean(All_BOA$T1CONDE)
# 2.137072
sd(All_BOA$T1CONDE)
# 1.214456
range(All_BOA$T1CONDE)
# 0-6

mean(All_BOA$T1BMI)
# 29.06916
sd(All_BOA$T1BMI)
# 5.187836
range(All_BOA$T1BMI)
# 15.2-47.3

mean(All_BOA$T1GRP)
# 31.54361
sd(All_BOA$T1GRP)
# 9.042156
range(All_BOA$T1GRP)
# 8.0-55.5

mean(All_BOA$T1PUFF)
# 343.2679
sd(All_BOA$T1PUFF)
# 111.2224
range(All_BOA$T1PUFF)
# 90-650

mean(All_BOA$T1TIMWALK)
# 3.530343
sd(All_BOA$T1TIMWALK)
# 1.027638
range(All_BOA$T1TIMWALK)
# 2.00-7.27

mean(All_BOA$T1COGTOT)
# 20.5514
sd(All_BOA$T1COGTOT)
# 4.0564
range(All_BOA$T1COGTOT)
# 10 30

mean(All_BOA$T1MSTOT)
# 11.7352
sd(All_BOA$T1MSTOT)
# 2.442138
range(All_BOA$T1MSTOT)
# 4-15

mean(All_BOA$T1TR20)
# 8.816199
sd(All_BOA$T1TR20)
# 2.595763
range(All_BOA$T1TR20)
# 1-18

mean(All_BOA$T2COGTOT)
# 19.83178
sd(All_BOA$T2COGTOT)
# 4.694716
range(All_BOA$T2COGTOT)
# 5-31

mean(All_BOA$T2MSTOT)
# 11.64798
sd(All_BOA$T2MSTOT)
# 2.536743
range(All_BOA$T2MSTOT)
# 4-15

mean(All_BOA$T2TR20)
# 8.183801
sd(All_BOA$T2TR20)
# 2.904821
range(All_BOA$T2TR20)
# 0-17

mean(All_BOA$COGTOT_Slope)
# -0.180176
sd(All_BOA$COGTOT_Slope)
# 0.8247317
range(All_BOA$COGTOT_Slope)
# -2.552448 -1.957105

################################################################################
# ANC Paper Resubmission Requested Descriptive Statistics

table(All_BOA$T1CENREG)
# 1:Northeast - 45 (14.02%)
# 2:Midwest - 69 (21.50%)
# 3:South - 179 (55.76%)
# 4:West - 28 (8.72%)

mean(All_BOA$T1WEALTH)
# $202,581.3
sd(All_BOA$T1WEALTH)
# $337,622

table(All_BOA$T1VGACT)
# 1:Every Day -   3 (0.93%)
# 2:>1x/week -    87 (27.10%)
# 3:1x/week -     39 (12.15%)
# 4:1-3x/month -  30 (9.35%)
# 5:Never -       162 (50.47%)

table(All_BOA$T1MDACT)
# 1:Every Day -   23 (7.17%)
# 2:>1x/week -    120 (37.38%)
# 3:1x/week -     62 (19.31%)
# 4:1-3x/month -  52 (16.20%)
# 5:Never -       64 (19.94%)

table(All_BOA$T1LTACT)
# 1:Every Day -   14 (4.36%)
# 2:>1x/week -    134 (41.74%)
# 3:1x/week -     106 (33.02%)
# 4:1-3x/month -  43 (13.40%)
# 5:Never -       24 (7.48%)

################################################################################
# Exploratory zero-order correlations between predictors of interest and cognitive outcomes
################################################################################

# Delayed Recall Score

cor.test(All_BOA$T1DLRC_Z, All_BOA$RAEDYRS_Z)
# r = 0.22, p < 0.001
cor.test(All_BOA$T1DLRC_Z, All_BOA$T1TIMWALK_Z)
# r = -0.10, p = 0.07
cor.test(All_BOA$T1DLRC_Z, All_BOA$T1PUFF_Z)
# r = 0.08, p = 0.14
cor.test(All_BOA$T1DLRC_Z, All_BOA$T1GRP_Z)
# r = -0.07, p = 0.20
cor.test(All_BOA$T1DLRC_Z, All_BOA$T1BMI_Z)
# r = 0.07, p = 0.24

# Delayed Recall Slope

cor.test(All_BOA$DLRC_Slope_Z, All_BOA$TIMWALK_Slope_Z)
# r = -0.05, p = 0.35
cor.test(All_BOA$DLRC_Slope_Z, All_BOA$PUFF_Slope_Z)
# r = 0.008, p = 0.88 
cor.test(All_BOA$DLRC_Slope_Z, All_BOA$GRP_Slope_Z)
# r = 0.01, p = 0.83
cor.test(All_BOA$DLRC_Slope_Z, All_BOA$BMI_Slope_Z)
# r = 0.01, p = 0.86

################################################################################

# Total Recall Score

cor.test(All_BOA$T1TR20_Z, All_BOA$RAEDYRS_Z)
# r = 0.30, p < 0.001 
cor.test(All_BOA$T1TR20_Z, All_BOA$T1TIMWALK_Z)
# r = -0.09, p = 0.11 
cor.test(All_BOA$T1TR20_Z, All_BOA$T1PUFF_Z)
# r = 0.07, p = 0.22 
cor.test(All_BOA$T1TR20_Z, All_BOA$T1GRP_Z)
# r = -0.11, p = 0.05
cor.test(All_BOA$T1TR20_Z, All_BOA$T1BMI_Z)
# r = 0.05, p = 0.42

# Total Recall Slope

cor.test(All_BOA$TR20_Slope_Z, All_BOA$TIMWALK_Slope_Z)
# r = -0.05, p = 0.37
cor.test(All_BOA$TR20_Slope_Z, All_BOA$PUFF_Slope_Z)
# r = -0.04, p = 0.49  
cor.test(All_BOA$TR20_Slope_Z, All_BOA$GRP_Slope_Z)
# r = 0.02, p = 0.78
cor.test(All_BOA$TR20_Slope_Z, All_BOA$BMI_Slope_Z)
# r = -0.02, p = 0.62

################################################################################

# Mental Status Score

cor.test(All_BOA$T1MSTOT_Z, All_BOA$RAEDYRS_Z)
# r = 0.51, p < 0.001
cor.test(All_BOA$T1MSTOT_Z, All_BOA$T1TIMWALK_Z)
# r = -0.23, p < 0.001 
cor.test(All_BOA$T1MSTOT_Z, All_BOA$T1PUFF_Z)
# r = 0.10, p = 0.08
cor.test(All_BOA$T1MSTOT_Z, All_BOA$T1GRP_Z)
# r = 0.10, p = 0.08
cor.test(All_BOA$T1MSTOT_Z, All_BOA$T1BMI_Z)
# r = -0.01, p = 0.85

# Mental Status Slope

cor.test(All_BOA$MSTOT_Slope_Z, All_BOA$TIMWALK_Slope_Z)
# r = -0.02, p = 0.70 
cor.test(All_BOA$MSTOT_Slope_Z, All_BOA$PUFF_Slope_Z)
# r = -0.06, p = 0.26 
cor.test(All_BOA$MSTOT_Slope_Z, All_BOA$GRP_Slope_Z)
# r = 0.003, p =  0.95
cor.test(All_BOA$MSTOT_Slope_Z, All_BOA$BMI_Slope_Z)
# r = -0.01, p = 0.84



################################################################################
# Manuscript Main Analyses
################################################################################


################################################################################
# Aim 1
# Hierarchical Multiple Linear Regression Models & Relative Importance Analyses

library(psych)
library(tidyverse)
library(Hmisc)
library(caret)
library(lmtest)
library(corrplot)


# Hierarchical Multiple Linear Regressions 
Aim1_Model1.1 <- lm(All_BOA$T1COGTOT_Z ~ All_BOA$RAGENDER + All_BOA$T1AGE_Y_Z + All_BOA$T1CONDE_Z)
summary(Aim1_Model1.1)

Aim1_Model1.2 <- lm(All_BOA$T1COGTOT_Z ~ All_BOA$RAGENDER + All_BOA$T1AGE_Y_Z + All_BOA$T1CONDE_Z + All_BOA$T1BMI_Z + All_BOA$T1GRP_Z + All_BOA$T1PUFF_Z)
summary(Aim1_Model1.2)

Aim1_Model1.3 <- lm(All_BOA$T1COGTOT_Z ~ All_BOA$RAGENDER + All_BOA$T1AGE_Y_Z + All_BOA$T1CONDE_Z + All_BOA$T1BMI_Z + All_BOA$T1GRP_Z + All_BOA$T1PUFF_Z + All_BOA$RAEDYRS_Z)
summary(Aim1_Model1.3)

Aim1_Model1.4 <- lm(All_BOA$T1COGTOT_Z ~ All_BOA$RAGENDER + All_BOA$T1AGE_Y_Z + All_BOA$T1CONDE_Z + All_BOA$T1BMI_Z + All_BOA$T1GRP_Z + All_BOA$T1PUFF_Z  + All_BOA$RAEDYRS_Z + All_BOA$T1TIMWALK_Z)
summary(Aim1_Model1.4)

anova(Aim1_Model1.1, Aim1_Model1.2, Aim1_Model1.3, Aim1_Model1.4)

################################################################################

# Regression Assumptions 

# Linearity
plot(Aim1_Ass_Model1.4, 1)

# Normality of Residuals
plot(Aim1_Ass_Model1.4, 2)

# Multicollinearity  - Variance Inflation Factors (VIF)
library(car)
car::vif(Aim1_Ass_Model1.4)

# Influential Cases - Cook's Distance
plot(Aim1_Ass_Model1.4, 4)

# Influential Cases - Residuals vs Leverage
plot(Aim1_Ass_Model1.4, 5)

################################################################################

# RELAIMPO 

library(relaimpo)
relamp = calc.relimp(Aim1_Ass_Model1.4)
summary(relamp)

################################################################################

# Plots
library("ggpubr")

# Figure 1 Plot
fig1 <- barplot(relamp$lmg, ylab="Relative Importance", xlab="Variable", main = "Relative Importance of Explanatory Variables")
fig1

# Figure 2a
cor.test(All_BOA$T1COGTOT_Z, All_BOA$RAEDYRS_Z)

fig2a <- ggscatter(All_BOA, x = "RAEDYRS_Z", y = "T1COGTOT_Z", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = FALSE, cor.method = "pearson",
                   xlab = "Education (Z-score)", ylab = "Baseline Cognition (Z-score)")
fig2a + font("xlab", size = 18) + font("ylab", size = 18)

# Figure 2b
cor.test(All_BOA$T1COGTOT_Z, All_BOA$T1TIMWALK_Z)
fig2b <- ggscatter(All_BOA, x = "T1TIMWALK_Z", y = "T1COGTOT_Z", 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = FALSE, cor.method = "pearson",
                   xlab = "Timed Walk (Z-score)", ylab = "Baseline Cognition (Z-score)")
fig2b + font("xlab", size = 18) + font("ylab", size = 18)


################################################################################
# Aim 2
# Total Cognition Slope Scores by Timepoint 1 Predictors

# Table 1 Values

mean(All_BOA$T1AGE_Y[which(All_BOA$COGTOT_Slope_Bin==0)])
# 73.24211
sd(All_BOA$T1AGE_Y[which(All_BOA$COGTOT_Slope_Bin==0)])
# 4.953289
mean(All_BOA$T1AGE_Y[which(All_BOA$COGTOT_Slope_Bin==1)])
# 72.34923
sd(All_BOA$T1AGE_Y[which(All_BOA$COGTOT_Slope_Bin==1)])
# 4.621915
t.test(formula = All_BOA$T1AGE_Y~All_BOA$COGTOT_Slope_Bin)
# t-value = 1.6704, p-value = 0.09582


sum(All_BOA$RAGENDER[which(All_BOA$COGTOT_Slope_Bin==0)])
# 98 / 169, 57.99%
sum(All_BOA$RAGENDER[which(All_BOA$COGTOT_Slope_Bin==1)])
# 96, 152, 63.16%
chisq.test(All_BOA$RAGENDER, y = All_BOA$COGTOT_Slope_Bin)
# X-squared = 0.69133, p-value = 0.4057


mean(All_BOA$RAEDYRS[which(All_BOA$COGTOT_Slope_Bin==0)])
# 11.85207
sd(All_BOA$RAEDYRS[which(All_BOA$COGTOT_Slope_Bin==0)])
# 3.146523
mean(All_BOA$RAEDYRS[which(All_BOA$COGTOT_Slope_Bin==1)])
# 12.80921
sd(All_BOA$RAEDYRS[which(All_BOA$COGTOT_Slope_Bin==1)])
# 2.567586
t.test(formula = All_BOA$RAEDYRS~All_BOA$COGTOT_Slope_Bin)
# t-value = -2.9976, p-value = 0.002937


mean(All_BOA$T1CONDE[which(All_BOA$COGTOT_Slope_Bin==0)])
# 2.242604
sd(All_BOA$T1CONDE[which(All_BOA$COGTOT_Slope_Bin==0)])
# 1.33865
mean(All_BOA$T1CONDE[which(All_BOA$COGTOT_Slope_Bin==1)])
# 2.019737
sd(All_BOA$T1CONDE[which(All_BOA$COGTOT_Slope_Bin==1)])
# 1.05146
t.test(formula = All_BOA$T1CONDE~All_BOA$COGTOT_Slope_Bin)
# t-value = 1.6669, p-value = 0.09654


mean(All_BOA$T1BMI[which(All_BOA$COGTOT_Slope_Bin==0)])
# 29.18107
sd(All_BOA$T1BMI[which(All_BOA$COGTOT_Slope_Bin==0)])
# 5.415772
mean(All_BOA$T1BMI[which(All_BOA$COGTOT_Slope_Bin==1)])
# 28.94474
sd(All_BOA$T1BMI[which(All_BOA$COGTOT_Slope_Bin==1)])
# 4.936924
t.test(formula = All_BOA$T1BMI~All_BOA$COGTOT_Slope_Bin)
# t-value = 0.40898, p-value = 0.6828


mean(All_BOA$T1GRP[which(All_BOA$COGTOT_Slope_Bin==0)])
# 31.38757
sd(All_BOA$T1GRP[which(All_BOA$COGTOT_Slope_Bin==0)])
# 8.622149
mean(All_BOA$T1GRP[which(All_BOA$COGTOT_Slope_Bin==1)])
# 31.71711
sd(All_BOA$T1GRP[which(All_BOA$COGTOT_Slope_Bin==1)])
# 9.513092
t.test(formula = All_BOA$T1GRP~All_BOA$COGTOT_Slope_Bin)
# t-value = -0.32387, p-value = 0.7463


mean(All_BOA$T1PUFF[which(All_BOA$COGTOT_Slope_Bin==0)])
# 345.7041
sd(All_BOA$T1PUFF[which(All_BOA$COGTOT_Slope_Bin==0)])
# 112.7111
mean(All_BOA$T1PUFF[which(All_BOA$COGTOT_Slope_Bin==1)])
# 340.5592
sd(All_BOA$T1PUFF[which(All_BOA$COGTOT_Slope_Bin==1)])
# 109.8517
t.test(formula = All_BOA$T1PUFF~All_BOA$COGTOT_Slope_Bin)
# t-value = 0.41384, p-value = 0.6793


mean(All_BOA$T1TIMWALK[which(All_BOA$COGTOT_Slope_Bin==0)])
# 3.537278
sd(All_BOA$T1TIMWALK[which(All_BOA$COGTOT_Slope_Bin==0)])
# 1.096908
mean(All_BOA$T1TIMWALK[which(All_BOA$COGTOT_Slope_Bin==1)])
# 3.522632
sd(All_BOA$T1TIMWALK[which(All_BOA$COGTOT_Slope_Bin==1)])
# 0.948253
t.test(formula = All_BOA$T1TIMWALK~All_BOA$COGTOT_Slope_Bin)
# t-value = 0.12828, p-value = 0.898


mean(All_BOA$T1COGTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 21.04734
sd(All_BOA$T1COGTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 4.28565
mean(All_BOA$T1COGTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 20
sd(All_BOA$T1COGTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 3.722137
t.test(formula = All_BOA$T1COGTOT~All_BOA$COGTOT_Slope_Bin)
# t-value = 2.3429, p-value = 0.01975


mean(All_BOA$T1MSTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 11.76923
sd(All_BOA$T1MSTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 2.478479
mean(All_BOA$T1MSTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 11.69737
sd(All_BOA$T1MSTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 2.408702
t.test(formula = All_BOA$T1MSTOT~All_BOA$COGTOT_Slope_Bin)
# t-value = 0.26325, p-value = 0.7925


mean(All_BOA$T1TR20[which(All_BOA$COGTOT_Slope_Bin==0)])
# 9.278107
sd(All_BOA$T1TR20[which(All_BOA$COGTOT_Slope_Bin==0)])
# 2.781667
mean(All_BOA$T1TR20[which(All_BOA$COGTOT_Slope_Bin==1)])
# 8.302632
sd(All_BOA$T1TR20[which(All_BOA$COGTOT_Slope_Bin==1)])
# 2.272902
t.test(formula = All_BOA$T1TR20~All_BOA$COGTOT_Slope_Bin)
# t-value = 3.4537, p-value = 0.0006282


mean(All_BOA$T2COGTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 17.94083
sd(All_BOA$T2COGTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 4.563969
mean(All_BOA$T2COGTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 21.93421
sd(All_BOA$T2COGTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 3.883521
t.test(formula = All_BOA$T2COGTOT~All_BOA$COGTOT_Slope_Bin)
# t-value = -8.4664, p-value = 9.447e-16


mean(All_BOA$T2MSTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 10.86391
sd(All_BOA$T2MSTOT[which(All_BOA$COGTOT_Slope_Bin==0)])
# 2.521184
mean(All_BOA$T2MSTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 12.51974
sd(All_BOA$T2MSTOT[which(All_BOA$COGTOT_Slope_Bin==1)])
# 2.261015
t.test(formula = All_BOA$T2MSTOT~All_BOA$COGTOT_Slope_Bin)
# t-value = -6.2036, p-value = 1.711e-09


mean(All_BOA$T2TR20[which(All_BOA$COGTOT_Slope_Bin==0)])
# 7.076923
sd(All_BOA$T2TR20[which(All_BOA$COGTOT_Slope_Bin==0)])
# 2.886751
mean(All_BOA$T2TR20[which(All_BOA$COGTOT_Slope_Bin==1)])
# 9.414474
sd(All_BOA$T2TR20[which(All_BOA$COGTOT_Slope_Bin==1)])
# 2.3905
t.test(formula = All_BOA$T2TR20~All_BOA$COGTOT_Slope_Bin)
# t-value = -7.9294, p-value = 3.804e-14


mean(All_BOA$COGTOT_Slope[which(All_BOA$COGTOT_Slope_Bin==0)])
# -0.8003266
sd(All_BOA$COGTOT_Slope[which(All_BOA$COGTOT_Slope_Bin==0)])
# 0.4976208
mean(All_BOA$COGTOT_Slope[which(All_BOA$COGTOT_Slope_Bin==1)])
# 0.5093334
sd(All_BOA$COGTOT_Slope[which(All_BOA$COGTOT_Slope_Bin==1)])
# 0.5068903
t.test(formula = All_BOA$COGTOT_Slope~All_BOA$COGTOT_Slope_Bin)
# t-value = -23.314, p-value < 2.2e-16


################################################################################
# - Logistic Regression
# 0 = decline, 1 = maintain/improve

All_BOA$COGTOT_Slope_Bin <- ifelse(All_BOA$COGTOT_Slope >= 0,1,0)

Model1 <- glm(All_BOA$COGTOT_Slope_Bin ~ All_BOA$T1AGE_Y_Z + All_BOA$RAEDYRS_Z + All_BOA$T1CONDE_Z + All_BOA$T1COGTOT_Z, family = "binomial")
summary(Model1)

All_BOA$COGTOT_Slope_Bin_Label <- ifelse(All_BOA$COGTOT_Slope_Bin=="0", "Decliner", "Maintainer")

boxplot(All_BOA$RAEDYRS~All_BOA$COGTOT_Slope_Bin_Label, main = "Cognition Slope and Education", xlab = "Cognitive Slope Status", ylab = "Years of Education")

#create function to run all the stats on the unconditional logistic regressions
library(DescTools)
library(epiDisplay)

run_logreg_stats <- function(input){
  conf <- confint(input)
  or <- exp(cbind(OR = coef(input), confint(input)))
  chisq <- with(input, null.deviance - deviance)
  df <- with(input, df.null - df.residual)
  p <- with(input, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
  r2 <- PseudoR2(input, which = "Nagelkerke")
  auc <- lroc(input)$auc
  output <- list(conf, or, chisq, df, p, r2, auc)
  return(output)
}

#run stats on unconditional logistic regressions
Model1_stats <- run_logreg_stats(Model1)
Model1_stats

################################################################################
# Aim 3
# Relationships between Fitness Slopes and Total Cognition Slopes


cor.test(All_BOA$COGTOT_Slope_Z, All_BOA$BMI_Slope_Z)
# r=-0.02827226, p-value = 0.6138

cor.test(All_BOA$COGTOT_Slope_Z, All_BOA$GRP_Slope_Z)
# r=0.0142998, p-value = 0.7986

cor.test(All_BOA$COGTOT_Slope_Z, All_BOA$PUFF_Slope_Z)
# r=-0.0665223, p-value = 0.2346

cor.test(All_BOA$COGTOT_Slope_Z, All_BOA$TIMWALK_Slope_Z)
# r=-0.05128701, p-value = 0.3597

