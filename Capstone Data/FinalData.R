library(dplyr)
library(ggplot2)

#read in each dataset by year
N6N7 <- read.csv("CollegeScorecard_Raw_Data/MERGED1996_97_PP.csv", stringsAsFactors = FALSE)
N7N8 <- read.csv("CollegeScorecard_Raw_Data/MERGED1997_98_PP.csv", stringsAsFactors = FALSE)
N8N9 <- read.csv("CollegeScorecard_Raw_Data/MERGED1998_99_PP.csv", stringsAsFactors = FALSE)
N9T0 <- read.csv("CollegeScorecard_Raw_Data/MERGED1999_00_PP.csv", stringsAsFactors = FALSE)
T0T1 <- read.csv("CollegeScorecard_Raw_Data/MERGED2000_01_PP.csv", stringsAsFactors = FALSE)
T1T2 <- read.csv("CollegeScorecard_Raw_Data/MERGED2001_02_PP.csv", stringsAsFactors = FALSE)
T2T3 <- read.csv("CollegeScorecard_Raw_Data/MERGED2002_03_PP.csv", stringsAsFactors = FALSE)
T3T4 <- read.csv("CollegeScorecard_Raw_Data/MERGED2003_04_PP.csv", stringsAsFactors = FALSE)
T4T5 <- read.csv("CollegeScorecard_Raw_Data/MERGED2004_05_PP.csv", stringsAsFactors = FALSE)
T5T6 <- read.csv("CollegeScorecard_Raw_Data/MERGED2005_06_PP.csv", stringsAsFactors = FALSE)
T6T7 <- read.csv("CollegeScorecard_Raw_Data/MERGED2006_07_PP.csv", stringsAsFactors = FALSE)
T7T8 <- read.csv("CollegeScorecard_Raw_Data/MERGED2007_08_PP.csv", stringsAsFactors = FALSE)
T8T9 <- read.csv("CollegeScorecard_Raw_Data/MERGED2008_09_PP.csv", stringsAsFactors = FALSE)
T9TE <- read.csv("CollegeScorecard_Raw_Data/MERGED2009_10_PP.csv", stringsAsFactors = FALSE)
TETL <- read.csv("CollegeScorecard_Raw_Data/MERGED2010_11_PP.csv", stringsAsFactors = FALSE)
TLTT <- read.csv("CollegeScorecard_Raw_Data/MERGED2011_12_PP.csv", stringsAsFactors = FALSE)
TTTR <- read.csv("CollegeScorecard_Raw_Data/MERGED2012_13_PP.csv", stringsAsFactors = FALSE)
TRTF <- read.csv("CollegeScorecard_Raw_Data/MERGED2013_14_PP.csv", stringsAsFactors = FALSE)
TFTI <- read.csv("CollegeScorecard_Raw_Data/MERGED2014_15_PP.csv", stringsAsFactors = FALSE)
TITS <- read.csv("CollegeScorecard_Raw_Data/MERGED2015_16_PP.csv", stringsAsFactors = FALSE)
TSTV <- read.csv("CollegeScorecard_Raw_Data/MERGED2016_17_PP.csv", stringsAsFactors = FALSE)

# Add academic years to each dataset
N6N7$year <- "96-97"
N7N8$year <- "97-98"
N8N9$year <- "98-99"
N9T0$year <- "99-00"
T0T1$year <- "00-01"
T1T2$year <- "01-02"
T2T3$year <- "02-03"
T3T4$year <- "03-04"
T4T5$year <- "04-05"
T5T6$year <- "05-06"
T6T7$year <- "06-07"
T7T8$year <- "07-08"
T8T9$year <- "08-09"
T9TE$year <- "09-10"
TETL$year <- "10-11"
TLTT$year <- "11-12"
TTTR$year <- "12-13"
TRTF$year <- "13-14"
TFTI$year <- "14-15"
TITS$year <- "15-16"
TSTV$year <- "16-17"

#Combine each of the datasets into our large set
BigTest <- rbind(N6N7, N7N8, N8N9, N9T0, T0T1, T1T2, T2T3, T3T4, T4T5, T5T6, T6T7, T7T8,
                 T8T9, T9TE, TETL, TLTT, TTTR, TRTF, TFTI, TITS, TSTV)

#Select schools that focus on Bachelor's Degrees
BigBach <- subset(BigTest, PREDDEG == 3)

# Selecting some columns to remove to try to make the data frame more
# manageable and less noisy.
removeCols1<- c(1:3, 5, 7:12) 
BigBach <- BigBach[-removeCols1]

removeCols2<- c(4, 10:23)
BigBach <- BigBach[-removeCols2]

# Removing specific program related variables
BigBach <- BigBach %>% select(-starts_with("PCIP"), -starts_with("CIP"))

# Removing more Misc variables, mostly demographics enrollment stats
BigBach <- BigBach %>% select(-"DISTANCEONLY", -starts_with("UGDS_"),
                              -starts_with("UG_"), -starts_with("PPTUG_"),-"CURROPER")

# Removing demographics based cost information
BigBach <- BigBach %>% select(-starts_with("NPT41"), -starts_with("NPT42"), -starts_with("NPT43"), -starts_with("NPT44"),
                              -starts_with("NPT45"), -starts_with("NPT4_0"), -starts_with("NPT4_3"),-starts_with("NPT4_7"))

# Removing more demographics based enrollment information, and more misc
BigBach <- BigBach %>% select(-starts_with("NUM4"), -starts_with("TUITIONFEE"), -"TUITFTE", -"INEXPFTE", -"PFTFAC", -"PCTPELL",
                              -starts_with("D150_"), -starts_with("C150_4_"), -"PFTFTUG1_EF", - starts_with("C150_L4_"))

# More misc variables
BigBach <- BigBach %>% select(-starts_with("C200_"), -starts_with("D200_"), - starts_with("RET_"), -"POOLYRS200", -"PCTFLOAN", -"UG25ABV", 
                              -"CDR2", -"CDR3", -starts_with("DEATH_"), -starts_with("COMP_"), -starts_with("WDRAW_"), -starts_with("ENRL_"),
                              -starts_with("UNKN_"), -starts_with("LO_INC_"), -starts_with("MD_INC_"), -starts_with("HI_INC_"), -starts_with("DEP_"),
                              -starts_with("IND_"), -starts_with("FEMALE_"), -starts_with("MALE_"), -starts_with("PELL_"), -starts_with("NOPELL_"))

#More misc variables: financial
BigBach <- BigBach %>% select(-starts_with("LOAN_"), -starts_with("NOLOAN_"), -starts_with("FIRSTGEN_"), -starts_with("NOT1STGEN_"))

#removing repayment variables:
BigBach <- BigBach %>% select(-starts_with("RPY_"), -starts_with("COMPL_"), -starts_with("NONCOM_"), -starts_with("NOTFIRSTGEN_"))

#removing more financial variables
BigBach <- BigBach %>% select(-"INC_PCT_LO", -'PAR_ED_PCT_1STGEN', -'INC_PCT_M1', -'INC_PCT_M2', -'INC_PCT_H1', -'INC_PCT_H1', -'INC_PCT_H2',
                              -starts_with("APPL_SCH_PCT"), -starts_with("PAR_ED_PCT"), -starts_with("OVERALL_"))

#removing more misc
BigBach <- BigBach %>% select(-starts_with('OMA'), -starts_with("OME"))

#more: Values I kept tha tmight be interesting, but have no useful info/all NA
BigBach <- BigBach %>% select(-"POOLYRS", -"MENONLY", -"WOMENONLY", -"RELAFFIL")

#getting rid of numbers of students in debt categories.
BigBach <- BigBach %>% select(-"DEBT_N", -"GRAD_DEBT_N", -"CUML_DEBT_N",-"CUML_DEBT_P90",-"CUML_DEBT_P75",-"CUML_DEBT_P25",-"CUML_DEBT_P10",
                              -"INC_N", -"PAR_ED_N", -"APPL_SCH_N")

#getting rid of numbers of students
BigBach <- BigBach %>% select(-starts_with("COUNT_WNE_"))

#turning various values into numerics for calculations.
BigBach$SAT_AVG <- as.numeric(BigBach$SAT_AVG)
BigBach$C150_4 <- as.numeric(BigBach$C150_4)
BigBach$C100_4 <- as.numeric(BigBach$C100_4)
BigBach$SATVRMID <- as.numeric(BigBach$SATVRMID)
BigBach$SATMTMID <- as.numeric(BigBach$SATMTMID)
BigBach$ACTCMMID <- as.numeric(BigBach$ACTCMMID)
BigBach$ACTENMID <- as.numeric(BigBach$ACTENMID)
BigBach$ACTMTMID <- as.numeric(BigBach$ACTMTMID)


