

# 0. LOADING DATA AND PACKAGES --------------------------------------------

library(psych)
library(tidyverse)
library(mirt)
library(fastGHQuad)
library(msm)
library(ggplot2)
library(forcats)

#dataset with grades
data <- read.csv("N:/durable/students/p1708-sverrbo/data/registers/W21_4952_TAB_KAR_VG.csv")
        


# 1. PREPARING GRADE DATASET -------------------------------------------------



data19 <- data[data$SKOLEAR == 20182019,] # only selecting year 2019
data19 <- data19[!duplicated(data19),] #removing duplicate rows
data19 <- data19[!data19$FAGSTATUS == "V",] # removing adults
        

# Some times, end of the term grades were listed as 2nd term grades. Hence, 2nd term grades are moved to STP 
data19$STP[data19$STP == ""] <- data19$TERMIN2[data19$STP ==""] 


#Removing unnecessary columns     
gradedf <- data19[c("w21_4952_lopenr_person","STP","FAGKODE")] 

#reshaping
gradedf <-  reshape(gradedf, dir = "wide", sep="",
                    idvar = "w21_4952_lopenr_person",  timevar = "FAGKODE")
  names(gradedf) <- gsub(pattern = "STP", replacement = "", x = names(gradedf))
  
#Creating a dataset needed for descriptives
full_gradedf <- gradedf
#save(full_gradedf, file = "full_gradedf)

        ##### 1.1 exclusion of sample not included in the analysis ####
        
#include only those taking mandatory  classes
gradedf <- gradedf[!is.na(gradedf$KRO1006),]
gradedf <- gradedf[!is.na(gradedf$HIS1002),]
gradedf <- gradedf[!is.na(gradedf$NOR1211),]
gradedf <- gradedf[!is.na(gradedf$NOR1212),]
gradedf <- gradedf[!is.na(gradedf$NOR1213),]
gradedf <- gradedf[!is.na(gradedf$REL1001),]

#Removing students that are in the Media and Communication and Art, Design, Architecture programs
gradedf <- gradedf[-which(!is.na(gradedf$MOK2003) & !is.na(gradedf$MOK2007)),]
gradedf <- gradedf[-which(!is.na(gradedf$KDA2003) & !is.na(gradedf$KDA2006)),]

#Creating a dataset needed for descriptives, with all students in the general studies program
gstudies_data <- gradedf 
#save(gstudies_data, file = "gstudies_data)
  
  #Removing those who have started prior to 2016
old_school <- unique(data$w21_4952_lopenr_person[!data$SKOLEAR %in% c(20162017,20172018, 20182019,20192020)] )
  gradedf <- gradedf[!gradedf$w21_4952_lopenr_person %in% old_school,]


          ###### 1.11. Identifying those with three or more electives ####
# List 131 non-language subjects available to third year students, taken from https://www.vilbli.no/en/en/no/distribution-of-periods-and-subjects-specialization-in-general-studies/program/v.st/v.stusp1----_v.stssa2----_v.stssa3----_/p2?kurs=v.stusp1----_v.stssa2----_v.stssa3----_
electives <- read.csv("all_courses.csv")
#Removing mandatory subjects
electives <- subset(electives, !FAGKODE %in% c("NOR1211", "NOR1212", "NOR1213","KRO1006","HIS1002","REL1001"))
  
  
#Since there are hundreds of language electives they were identified differently:

#A dataframe with all subject codes and names scraped from https://www.udir.no/innstillinger/fagkoder-til-sok/
language_electives <- read.csv("course_description_all_courses.csv", sep = ";")
names(language_electives)[1] <- "description"
language_electives <- language_electives %>% relocate(FAGKODE, .before = description)


#removing subjects no one in the sample took
  gradedf <- gradedf[, !colSums(is.na(gradedf)) == nrow(gradedf)]  
  
#each subject group has a subject code consisting of a three-letter combination
#we identify which subjects are language subjects
  unique(substr(names(gradedf),1,3))
  
#Extracting column names of language electives and making list of language electives in gradedf
  language_codes <- colnames(gradedf)[grep("^(FSP|ENG|SPR|PSP)", colnames(gradedf))]
    language_electives <- language_electives[language_electives$FAGKODE %in% language_codes,]
  
  #List of subjects available to students.
  electives <- rbind(electives, language_electives)

#the electives df includes oral grades for language subjects.
#To see how many subjects students have taken, oral grades are removed.
oral_grades <- electives[grep(electives$description, pattern= "muntlig"),] #oral grades have the word "muntlig" in its name

#list of electives, excluding oral grades
electives <- electives[!electives$FAGKODE %in% oral_grades$FAGKODE,]

#calculating how many electives each student took

#the electives list is now the list of electives that are in gradedf
electives <- intersect(electives$FAGKODE, colnames(gradedf))

#dataset used for counting how many electives a student has taken
n_electives <- gradedf
n_electives <- n_electives[, colnames(n_electives) %in% c(electives, "w21_4952_lopenr_person")]
n_electives$n_electives <- rowSums(!is.na(n_electives[2:ncol(n_electives)]))

#We only want to include those that took 3 electives or more electives
included_sample <- n_electives$w21_4952_lopenr_person[n_electives$n_electives >= 3]

#Removing those taking fewer than three electives
gradedf <- gradedf[gradedf$w21_4952_lopenr_person %in% included_sample,]

#Checking if all remaining students are in the General Studies program
v_data <- read.csv("N:/durable/students/p1708-sverrbo/data/registers/W21_4952_VITNEMAL_VG.csv")
v_data <- v_data[v_data$w21_4952_lopenr_person %in% gradedf$w21_4952_lopenr_person,]
v_data$w21_4952_lopenr_person[v_data$STUDRETN_3 != 62] # there are no students not in general studies

#ID of the sample, used for subsequent indexing.
ID <- gradedf$w21_4952_lopenr_person

        #### 1.2 inclusion of subjects ####
subject_n <- gradedf[, -c(which(names(gradedf)=="w21_4952_lopenr_person"),
                          which(names(gradedf)=="KRO1006"),
                          which(names(gradedf)=="NOR1212"),
                          which(names(gradedf)=="REL1001"),
                          which(names(gradedf)=="NOR1211"),
                          which(names(gradedf)=="NOR1213"),
                          which(names(gradedf)=="HIS1002"))]


#summing n students in each subject
subject_n[!is.na(subject_n)] <- 1
subject_n[is.na(subject_n)] <- 0
for(i in 1:ncol(subject_n))subject_n[,i] <- as.numeric(subject_n[,i])
subject_n <- as.data.frame(colSums(subject_n))
names(subject_n) <- "n"
subject_n <- subject_n[order(rev(subject_n$n)),]


#limiting to 40 subjects and removing oral english
subject_n <- subject_n[c(1:40)]
subject_n <- subject_n[-9]

#plotting
plot(subject_n$n, type = "l", ylab = "students enrolled in subject", xlab = "subjects")+
  abline(v = 11, col = "red")

#Creating a list of subjects to be included in the study
included_subjects <- c("NOR1212",	"REL1001",	"NOR1211",	"NOR1213",	"HIS1002",	"SAM3038",	"REA3024",	"REA3028",	"REA3012",	"SAM3023",	"SAM3020",	"SAM3016",	"SPR3010","SPR3011",	"REA3002",	"REA3005",	"SAM3006")

#excluded subjects are removed
gradedf <- gradedf[,names(gradedf) %in% c(included_subjects, "w21_4952_lopenr_person")]





# 2. PREPARING CHOICE VARIABLE, d_ni --------------------------------------
testdf <- gradedf
gradedf <- testdf
xtable_student.temp <- gradedf

#Did student i enroll in subject n? cell gets a 1 if it is not NA, and a 0 if it is NA
for(i in 2:ncol(xtable_student.temp))xtable_student.temp[i][!is.na(xtable_student.temp[i])] <- 1
for(i in 2:ncol(xtable_student.temp))xtable_student.temp[i][is.na(xtable_student.temp[i])] <- 0


#did the school offer the subject? extracting school-level data
xtable_school.temp <-data19[,c("w21_4952_lopenr_orgnr","FAGKODE","STP")]
xtable_school.temp <- xtable_school.temp[!xtable_school.temp$w21_4952_lopenr_orgnr =="",] #removing NA School IDs
xtable_school.temp <- xtable_school.temp[xtable_school.temp$FAGKODE %in% included_subjects,] #included subjects only

#reshaping
xtable_school.temp <- reshape(xtable_school.temp, dir = "wide", sep="", idvar = "w21_4952_lopenr_orgnr",  timevar = "FAGKODE")
names(xtable_school.temp) <- gsub(pattern = "STP", replacement = "", x = names(xtable_school.temp))

#100 if the school offered subjects, 10 if it does not
for(i in 2:ncol(xtable_school.temp))xtable_school.temp[i][!is.na(xtable_school.temp[i])] <- 100
for(i in 2:ncol(xtable_school.temp))xtable_school.temp[i][is.na(xtable_school.temp[i])] <- 10


#student_school links the students in the sample with their school
student_school <- data19[data19$w21_4952_lopenr_person %in% ID,]
student_school <- student_school[,c("w21_4952_lopenr_orgnr","w21_4952_lopenr_person")]
student_school <- student_school[!duplicated(student_school$w21_4952_lopenr_person),] # removing duplicates
  length(unique(student_school$w21_4952_lopenr_orgnr))# 308 schools are included in the study
  sum(student_school$w21_4952_lopenr_orgnr == "") #20 students have NA in their school code.

#Students with NA school codes receive the school code from a previous year
  student_school$w21_4952_lopenr_orgnr[student_school$w21_4952_lopenr_orgnr == ""] <- NA #recoding "" to NA

missing_school <- student_school[is.na(student_school$w21_4952_lopenr_orgnr),] #indexing those missing school ID
  missing_school <- data[data$w21_4952_lopenr_person %in% missing_school$w21_4952_lopenr_person,] #extracting all data on these students
  missing_school <- missing_school[c("w21_4952_lopenr_person", "w21_4952_lopenr_orgnr")] # removing unnecessary columns
  missing_school <- missing_school[!missing_school$w21_4952_lopenr_orgnr == "",] # removing NA school IDs
  missing_school <- missing_school[!duplicated(missing_school$w21_4952_lopenr_person),] # removing duplicates

#connecting the dfs containing those that had missing school IDs and the other students
student_school <- left_join(student_school,missing_school, by = "w21_4952_lopenr_person")
student_school$w21_4952_lopenr_orgnr.x[is.na(student_school$w21_4952_lopenr_orgnr.x)] <- student_school$w21_4952_lopenr_orgnr.y[is.na(student_school$w21_4952_lopenr_orgnr.x)]
student_school <- student_school[1:2]
names(student_school)[1] <- "w21_4952_lopenr_orgnr"

student_df <- xtable_student.temp
school_df <- xtable_school.temp

#connecting student IDs and school IDs for the school DF
school_df<- left_join(student_school,school_df, by = "w21_4952_lopenr_orgnr")

#connecting student IDs and school IDs for the student DF
student_df <- left_join(student_school,student_df,by = "w21_4952_lopenr_person")

for(i in 3:ncol(student_df))student_df[,i] <- as.numeric(student_df[,i])
for(i in 3:ncol(school_df))school_df[,i] <- as.numeric(school_df[,i])
school_df <- school_df[names(student_df)]# reordering so columns match

choicedf <- cbind(student_df$w21_4952_lopenr_person, student_df[,3:ncol(student_df)] + school_df[,3:ncol(school_df)])
names(choicedf)[1] <- "w21_4952_lopenr_person"

# 100 = course offered, not taken
# 101 = course offered, and taken
# 10 = course not offered, course not taken
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 101] <- 1
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 100] <- 0
for(i in 2:ncol(choicedf))choicedf[i][choicedf[i] == 10] <- NA


# 3. PREPARING FINAL DF ---------------------------------------------------


#reordering names in choicedf to match gradedf
choicedf <- choicedf[names(gradedf)]

#changing names of both dataframes

names(gradedf) <- c("w21_4952_lopenr_person",
                    "psychology",   # SAM3038
                    "norwegian_s",  # NOR1212
                    "religion",     # REL1001
                    "norwegian_w",  # NOR1211
                    "norwegian_o",  # NOR1213
                    "history",      # HIS1002
                    "jurisprudence",# SAM3023
                    "politics",     # SAM3020
                    "english_w",    # SPR3010
                    "english_o",    # SPR3011
                    "biology",      # REA3002
                    "math_s",       # REA3028
                    "chemistry",    # REA3012
                    "physics",      # REA3005
                    "sociology",    # SAM3016
                    "math_n",       # REA3024
                    "marketing"     # SAM3006
)    


names(choicedf) <- names(gradedf)


        #### 3.1 Recoding ####

#Frequency of different letter grades
table(unlist(gradedf[2:ncol(gradedf)]))

#All letter grades are recoded to NA, except "No basis for assisment (IV)", which is recoded to 1
gradedf[gradedf == "IV"] <- 1
for(i in 2:ncol(gradedf))gradedf[,i] <- as.numeric(gradedf[,i]) # changing to numeric and rest of letter grades to NA

        #### 3.2 last cleaning ####

#removing mandatory subjects and oral english
choicedf <- select(choicedf, !c(history,religion,norwegian_w,norwegian_o,norwegian_s,english_o))
names(choicedf)[2:ncol(choicedf)] <- paste0("C", names(choicedf)[2:ncol(choicedf)])

#Merging choice and grade datasets, and removing identifier
final_df <- left_join(choicedf,gradedf, by = "w21_4952_lopenr_person")
final_df <- select(final_df, !w21_4952_lopenr_person)

#removing ID from choice and grade data sets
gradedf <- select(gradedf, !w21_4952_lopenr_person)
choicedf <- select(choicedf, !w21_4952_lopenr_person)


# 4. Descriptive Statistics -----------------------------------------------


        #### 4.1 percentage students in general studies taking the included subjects (p.16) ####

#load("gstudies_data")
gstudies <- gstudies_data[,names(gstudies_data) %in% included_subjects]# removing subjects not part of the analysis
gstudies <- select(gstudies, !c(NOR1212,REL1001,NOR1211,NOR1213,HIS1002)) # removing mandatory subjects
gstudies[!is.na(gstudies)] <- 1
gstudies[is.na(gstudies)] <- 0
for(i in 1:ncol(gstudies))gstudies[,i] <- as.numeric(gstudies[,i])
gstudies$nelectives <- rowSums(gstudies[1:ncol(gstudies)]) 
table(gstudies$nelectives) # how many students in general studies took the included electives in this study?


        #### 4.2 observed correlation table ####
cor_table <- gradedf
cor_table <- round(cor(cor_table, use = "pairwise.complete.obs"),2)
#  write.table(cor_table, "cor_table", quote = F, sep= "\t")



        #### 4.3 Descriptives table ####
desc_table <- gradedf
desc_table <- describe(gradedf, na.rm = T) 
desc_table$na <- 100-desc_table$n/nrow(gradedf)*100 # percentage NA
desc_table[18,2] <- sum(desc_table$n, na.rm = T) # total N
desc_table[18,14] <- sum(desc_table$n, na.rm = T)/(21832*17)*100 # total NA
desc_table[18,3] <- mean(as.matrix(gradedf), na.rm = T) # grand mean
desc_table[18,5] <- median(as.matrix(gradedf), na.rm = T) # grand median
desc_table[18,4] <- sd(as.matrix(gradedf), na.rm = T) # grand SD
desc_table[18,11] <- skew(rowSums(gradedf, na.rm = T)) # total skew
desc_table[18,12] <- kurtosi(rowSums(gradedf, na.rm = T)) # total kurtosis
desc_table <- round(desc_table, 2)
#write.table(desc_table, "desc_table", quote = F, row.names = T, sep = "\t", col.names = T)


#### 4.4 observed correlation between missing and GPA ####

#calculating the GPA of the sample
gpa_df <- full_gradedf
gpa_df <- gpa_df[gpa_df$w21_4952_lopenr_person %in% ID,] #Reordering gpa_df to choicedf
gpa_df <- gpa_df[order(ID), ]
gpa_df$N_SUBJECTS <- rowSums(!is.na(gpa_df[2:ncol(gpa_df)])) # how many subjects did they receive grades in?
gpa_df[gpa_df == "IV"] <- 1 #No basis for assessment equals 1
#Other letter grades equal NA
for(i in 2:ncol(gpa_df))gpa_df[,i] <- as.numeric(gpa_df[,i])

gpa_df$GPA <- rowSums(gpa_df[2:575], na.rm = T) / gpa_df$N_SUBJECTS # GPA of students

#removing those with only NAs
gpa_df <- gpa_df[(!is.nan(gpa_df$GPA)),]

obs_missing_cor <- data.frame(matrix(ncol = 11, nrow = 1))
colnames(obs_missing_cor) <- colnames(choicedf)[1:11]

cor(choicedf[,1], gpa_df$GPA, use = "pairwise.complete.obs")

for(i in 1:11)obs_missing_cor[,i] <- cor(choicedf[,i], gpa_df$GPA, use = "pairwise.complete.obs")
obs_missing_cor <- round(obs_missing_cor, 2)
#write.table(obs_missing_cor, "obs_missing_cor", quote = F, row.names = F, col.names = T, sep = "\t")

#checking significance level
for(i in 1:11){k <-cor.test(gpa_df$GPA, choicedf[,i], method = "pearson")
print(k)
}  



# 5. Models ---------------------------------------------------------------

        #### 5.1 Competency dimension ####

#uni-dimensional GPCM (Model 0)
uni_gpcm_model <- mirt(gradedf, model = 1, itemtype = "gpcm", SE = T,quadpts = 20)

#uni-dimensional GRM (Model 1)
uni_graded_model <- mirt(gradedf, model = 1, itemtype = "graded", SE = T, quadpts = 20)

#two-dimensional model (Model 2)
two_dim_spec <- '
humanities = history,religion, psychology,jurisprudence,politics,marketing,sociology,norwegian_w,norwegian_o,norwegian_s,english_w,english_o
STEM = biology,math_n,math_s,chemistry,physics
COV = humanities*STEM
'

two_dim_model <- mirt(data = final_df[12:28], model = two_dim_spec,itemtype = "graded",
                      method = "EM",  parallel = T,
                      quadpts = 20,
                      SE = T,
                      technical = list(NCYCLES = 2000))

#three-dimensional model (Model 3)
three_dim_spec <- '
SS = history,religion, psychology,jurisprudence,politics,marketing,sociology
languages = norwegian_w,norwegian_o,norwegian_s,english_w,english_o
STEM = biology,math_n,math_s,chemistry,physics
COV = SS*languages*STEM
'

three_dim_model <- mirt(data = final_df[12:28], model = three_dim_spec, itemtype = "graded",
                        method = "EM",
                        quadpts = 20,
                        SE = T,
                        technical = list(NCYCLES = 10000,MAXQUAD =40000))

#Model comparison table

prof_model_comp <- anova(uni_graded_model,two_dim_model,three_dim_model)
round(prof_model_comp, 0)
prof_model_comp$BIC_dif <- NA
for(i in 2:nrow(prof_model_comp))prof_model_comp[i,9] <- prof_model_comp[i,4] -prof_model_comp[i-1,4]
#write.table(prof_model_comp, "prof_model_comp", quote = F, sep = "\t")


        #### 5.2 Selection Model Model ####

#### selection model ####

#Constrained t-parameters and discrimination parameters
choice_spec_constrained <- '
choice = Cpsychology,Cjurisprudence,Cpolitics,Cmarketing,Csociology,Cenglish_w,Cbiology,Cmath_n,Cmath_s,Cchemistry,Cphysics
CONSTRAIN =  (1-11, t1), (1-11, a1)
'

choice_model_constrained <- mirt(data = choicedf, model = choice_spec_constrained, itemtype = "ggum",
                                 method = "EM",SE = T, quadpts = 20)

choice_sv <- mod2values(choice_model_constrained)

#freely estimated t parameters, but constrained discrimination
choice_spec_free <- '
choice = Cpsychology,Cjurisprudence,Cpolitics,Cmarketing,Csociology,Cenglish_w,Cbiology,Cmath_n,Cmath_s,Cchemistry,Cphysics
CONSTRAIN =  (1-11, a1)
'

choice_model_free <- mirt(data = choicedf, model = choice_spec_free, itemtype = "ggum",
                            method = "EM",SE = T, 
                            pars = choice_sv,
                            quadpts = 20)

M2(choice_model_free_t, na.rm = T)


        #### 5.3 Joint model (Model 4) ####

three_dim_joint_spec <- '
choice = Cpsychology,Cjurisprudence,Cpolitics,Cmarketing,Csociology,Cenglish_w,Cbiology,Cmath_n,Cmath_s,Cchemistry,Cphysics
humanities = history,religion,psychology,jurisprudence,politics,marketing,sociology,norwegian_w,norwegian_o,norwegian_s,english_w,english_o
STEM = biology,math_n,math_s,chemistry,physics
COV = choice*humanities*STEM
CONSTRAIN =  (1-11, a1)
'
final_three_dim_model <- mirt(data = final_df, model = three_dim_joint_spec,
                              itemtype = c(rep("ggum",11),rep("graded",17)),
                              method = "EM",
                              SE = T,
                              pars = sv_pars,
                              quadpts = 20,
                              technical = list(NCYCLES = 10000,MAXQUAD =40000))

#logLik comparison between Model 2 and 4
Model_4_logLik <- extract.mirt(final_three_dim_model, what = "logLik")
  Model_2_logLik <- extract.mirt(two_dim_model, what = "logLik")
  selection_model_logLik <- extract.mirt(choice_model_free_t, what = "logLik")
  Model_4_logLik - (Model_2_logLik+selection_model_logLik)

  



# 6. Expected Grades ------------------------------------------------------

  
  ##### Compute probabilities from mirtobject
  mirtprobs <- function(mirtmod, nquad, ndim, Mu, Sigma){
    GHgrid <- gaussHermiteData(nquad)
    GHxXD <- as.matrix(expand.grid(rep(list(GHgrid$x), ndim)))
    GHwXD <- as.matrix(expand.grid(rep(list(GHgrid$w), ndim)))
    
    #Adjust grid
    GHxXDN <- t(t(chol(Sigma)) %*% t(GHxXD) + Mu)
    
    J <- length(extract.mirt(mirtmod, "itemtype"))
    itemprobs <- vector("list", J)
    #Compute probabilities for all categories for all latent variable values, from mirt object
    for(i in 1:J) itemprobs[[i]] <- probtrace(extract.item(mirtmod, i), GHxXDN)
    return(list(itemprobs = itemprobs, GHxXDN = GHxXDN, GHwXD = GHwXD, GHxXD = GHxXD))
  }
  
  #### 6.2 Compute expected grades for missing values ####
  
  #mirtobj - mirt object
  #obsdata - dataframe with grades
  #nquad - number of quadrature points per dimension
  #ndim - number of latent variables
  #Mu - mean vector of latent variables
  #Sigma - covariance matrix of latent variables
  #Jelect - number of electives
  #Jcat - vector with number of response categories for choice and grades
  
  computeExpected <- function(mirtobj, obsdata, nquad, ndim, Mu, Sigma, Jelect, Jcat){
    ##### Compute conditional probabilities at quadrature points
    myprobs <- mirtprobs(mirtobj, nquad = nquad, ndim = ndim, Mu = Mu, Sigma = Sigma)
    
    ##### We have N number of respondents
    ##### Jall is the number of elective grades plus the number of total grades
    N <- nrow(obsdata)
    Jall <- ncol(obsdata)
    Jcat <- c(rep(2, Jelect), Jcat)
    
    ##### Create a matrix to fill with observed or expected grades
    newgrade <- matrix(NA, N, Jall)
    ##### Loop through each respondent
    for(k in 1:N){ 
      ##### Compute normalizing constant for the posterior distribution
      ##### Conditional probabilities for selection (binary) and for grade (ordinal), given a combination of latent variables
      conditionalprob <- rep(1.0, nrow(myprobs$GHxXDN))
      for(i in 1:Jall){
        if(is.na(obsdata[k,i])) next
        conditionalprob <- conditionalprob * myprobs$itemprobs[[i]][,obsdata[k,i] + 1]
      }
      
      marginalprob <- numeric(1)
      ##### Weight each quadrature point and accumulate
      ##### Adjust weights to standard normal distribution instead of exp(-x^2)
      for(i in 1:nrow(myprobs$GHxXDN)) marginalprob <- conditionalprob[i] * prod(myprobs$GHwXD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxXD[i,]^2 / 2.0)) + marginalprob
      
      ##### Loop through each course
      for(l in 1:Jall){		
        if(!is.na(obsdata[k,l])){
          newgrade[k, l] <- obsdata[k, l]
          next
        }
        
        ##### Compute expected value for missing grade
        conditionalexp <- matrix(0, nrow(myprobs$GHxXDN), ncol = Jcat[l])
        for(j in 1:Jcat[l]) conditionalexp[, j] <- as.numeric((j - 1)) * myprobs$itemprobs[[l]][,j] * conditionalprob / marginalprob
        
        myexpgrade <- numeric(1)
        for(j in 1:Jcat[l]) for(i in 1:nrow(myprobs$GHxXDN)) myexpgrade <- conditionalexp[i,j] * prod(myprobs$GHwXD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxXD[i,]^2 / 2.0)) + myexpgrade
        
        newgrade[k, l] <- myexpgrade
      }
    }
    ##### Output is a matrix of same size as the data matrix, but with expected values instead of missing values
    return(newgrade)
  }
  
        ##### 6.21 Defining input needed for the function ####
  
joint_free_mean_vecc <- c(0,0,0)
joint_free_theta_cov <- data.frame(c(1,.369,.604),
                                     c(.369,1,.82),
                                     c(.604,.82,1))
  
two_dim_mean_vecc <- c(0,0)
two_dim_theta_cov <- data.frame(c(1, .817),
                                  c(.817,1))
  
car_vec <- rep(c(1:6),18)
  
#changing grades to 0-5, a requirement for the function
final_df[12:28][] <- final_df[12:28][]-1
  
  
#compute expected grades for missing cells for Model 4
ex_joint_free <- computeExpected(final_three_dim_model,
                                   obsdata = final_df, 
                                   nquad = 15,
                                   ndim = 3,
                                   Mu = joint_free_mean_vecc,
                                   Sigma = joint_free_theta_cov,
                                   Jelect = 11,
                                   Jcat = car_vec)
  
#changing grades back to 1-6 and matching names with the grade df.
ex_joint_free[12:28] <- ex_joint_free[12:28][]+1
ex_joint_free <- as.data.frame(ex_joint_free)
names(ex_joint_free) <- names(final_df)
  
#compute expected grades for missing cells for Model 2
ex_two_dim <- computeExpected(two_dim_model,
                                obsdata = final_df[12:28], 
                                nquad = 15,
                                ndim = 2,
                                Mu = two_dim_mean_vecc,
                                Sigma = two_dim_theta_cov,
                                Jelect = 0,
                                Jcat = car_vec)
  
#changing grades back to 1-6 and matching names with the grade df.
ex_two_dim <- ex_two_dim[]+1
ex_two_dim <- as.data.frame(ex_two_dim)
names(ex_two_dim) <- names(ex_two_dim)



        #### 6.3 Compute expected grades for all cells  ####

#The function takes the same inputs as computeExpected

computeExpectedAll <- function(mirtobj, obsdata, nquad, ndim, Mu, Sigma, Jelect, Jcat){
  ##### Compute conditional probabilities at quadrature points
  myprobs <- mirtprobs(mirtobj, nquad = nquad, ndim = ndim, Mu = Mu, Sigma = Sigma)
  
  ##### We have N number of respondents
  ##### Jall is the number of elective grades plus the number of total grades
  N <- nrow(obsdata)
  Jall <- ncol(obsdata)
  Jcat <- c(rep(2, Jelect), Jcat)
  
  ##### Create a matrix to fill with observed or expected grades
  newgrade <- matrix(NA, N, Jall)
  ##### Loop through each respondent
  for(k in 1:N){ 
    ##### Compute normalizing constant for the posterior distribution
    ##### Conditional probabilities for selection (binary) and for grade (ordinal), given a combination of latent variables
    conditionalprob <- rep(1.0, nrow(myprobs$GHxXDN))
    for(i in 1:Jall){
      if(is.na(obsdata[k,i])) next
      conditionalprob <- conditionalprob * myprobs$itemprobs[[i]][,obsdata[k,i] + 1]
    }
    
    marginalprob <- numeric(1)
    ##### Weight each quadrature point and accumulate
    ##### Adjust weights to standard normal distribution instead of exp(-x^2)
    for(i in 1:nrow(myprobs$GHxXDN)) marginalprob <- conditionalprob[i] * prod(myprobs$GHwXD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxXD[i,]^2 / 2.0)) + marginalprob
    
    ##### Loop through each course
    for(l in 1:Jall){		
      ##### Compute expected value for missing grade
      conditionalexp <- matrix(0, nrow(myprobs$GHxXDN), ncol = Jcat[l])
      for(j in 1:Jcat[l]) conditionalexp[, j] <- as.numeric((j - 1)) * myprobs$itemprobs[[l]][,j] * conditionalprob / marginalprob
      
      myexpgrade <- numeric(1)
      for(j in 1:Jcat[l]) for(i in 1:nrow(myprobs$GHxXDN)) myexpgrade <- conditionalexp[i,j] * prod(myprobs$GHwXD[i,] / sqrt(2.0 * pi) * exp(myprobs$GHxXD[i,]^2 / 2.0)) + myexpgrade
      
      newgrade[k, l] <- myexpgrade
    }
  }
  ##### Output is a matrix of same size as the data matrix, but with expected values instead of missing values
  return(newgrade)
}

#compute all expected grades for Model 4
ex_joint_free_all <- computeExpectedAll(final_three_dim_model,
                                        obsdata = final_df, 
                                        nquad = 10,
                                        ndim = 3,
                                        Mu = joint_free_mean_vecc,
                                        Sigma = joint_free_theta_cov,
                                        Jelect = 11,
                                        Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df
ex_joint_free <- as.data.frame(ex_joint_free)
ex_joint_free[12:28] <- ex_joint_free[12:28][] + 1
names(ex_joint_free) <- names(final_df)

#compute all expected grades for Model 2
ex_two_dim_all <- computeExpectedAll(two_dim_model,
                                     obsdata = final_df[12:28], 
                                     nquad = 10,
                                     ndim = 2,
                                     Mu = two_dim_mean_vecc,
                                     Sigma = two_dim_theta_cov,
                                     Jelect = 0,
                                     Jcat = car_vec)

#changing grades back to 1-6 and matching names with the grade df
ex_two_dim_all <- as.data.frame(ex_two_dim_all)
ex_two_dim_all <- ex_two_dim_all[] + 1
names(ex_two_dim_all) <- names(ex_two_dim_all)


#changing grades back to 1-6
final_df[12:28] <- final_df[12:28] +1



        #### 6.4 Descriptive Statistics with Expected grades from Model 4 ####

###### average grades of STEM and HUM subjects for Model 4 ######  


STEM_avg <- sum(colSums(Ex_grades_joint_free[24:28])) # last 5 columns are STEM
STEM_avg <- STEM_avg/(nrow(Ex_grades_joint_free)*5)

HUM_avg <- sum(colSums(Ex_grades_joint_free[c("sociology","psychology","jurisprudence",
                                              "politics","marketing","english_w",
                                              "english_o")]))
HUM_avg <- HUM_avg/(nrow(Ex_grades_joint_free)*7)


Ex_grades <- Ex_grades_joint_free
Ex_grades$GPA <- rowSums(Ex_grades)

obs_missing_cor <- data.frame(matrix(ncol = 11, nrow = 1))
colnames(obs_missing_cor) <- colnames(choicedf)[1:11]

for(i in 1:11)obs_missing_cor[,i] <- cor(choicedf[,i], Ex_grades$GPA, use = "pairwise.complete.obs")
obs_missing_cor <- round(obs_missing_cor, 2)
#write.table(obs_missing_cor, "obs_missing_cor", quote = F, row.names = F, col.names = T, sep = "\t")

#checking significance level
for(i in 1:11){k <-cor.test(choicedf[,i], Ex_grades$GPA, method = "pearson")
print(k)
} 



# 7. Comparing Model 2 and 4 ----------------------------------------------
load("final_df")
load("Ex_grades_joint_free_all")
load("Ex_grades_two_all")
gradef <- final_df[12:28]

ex_grades4 <- Ex_grades_joint_free_all[12:28]
ex_grades2 <- Ex_grades_two_all

#removing expected values that are unobserved in gradedf 
gradef <- final_df[12:28]
ex_grades2[is.na(gradef)] <- NA
ex_grades4[is.na(gradef)] <- NA

#function that computes the average grade on subject i for dns = 0 and dns = 1
model_fit <- function(grades){
  
  fit_frame <- as.data.frame(matrix(NA, nrow = ncol(gradedf), ncol = ncol(gradedf)*2))
  names(fit_frame)[1:17] <- paste0(names(gradedf),".1") # s_1
  names(fit_frame)[18:34] <- paste0(names(gradedf),".0")# s_0
  row.names(fit_frame) <- names(ex_grades2)  
  
  for(s in 1:17) {
    s_1 <- which(!is.na(grades[,s])) # which students recieved a grade in subject s?
    s_0 <- which(is.na(grades[,s])) #which student did not recieve a grade in subject s?
    
    #what is the mean grade in subject i of students who did  recieve  a grade in subject s?
    for(i in 1:17){
      fit_frame[s,i] <- sum(grades[s_1,i], na.rm = T)/(sum(!is.na(grades[s_1,i])))
      
      #what is the mean grade in subject i of students who did not recieve  a grade in subject s?
    }
    for(i in 1:17){
      fit_frame[s,(i+17)] <- sum(grades[s_0,i], na.rm = T)/(sum(!is.na(grades[s_0,i])))
    }
  }
  return(fit_frame)
}

# Observed subject means and Expected subject means for Models 2 and 4
obs_means <- model_fit(gradedf)
m2_means <- model_fit(ex_grades2) 
m4_means <- model_fit(ex_grades4)

#there are no students taking math_n and math_s, so these become NA
obs_means["math_n","math_s.1"] <- NA
obs_means["math_s","math_n.1"] <- NA

m2_means["math_n","math_s.1"] <- NA
m2_means["math_s","math_n.1"] <- NA

m4_means["math_n","math_s.1"] <- NA
m4_means["math_s","math_n.1"] <- NA

m2_s1 <- rowSums(m2_means[,1:17], na.rm = T)
m2_s0 <- rowSums(m2_means[,18:34], na.rm = T)
m2_means <- as.data.frame(rbind(m2_s1,m2_s0))

m4_s1 <- rowSums(m4_means[,1:17], na.rm = T)
m4_s0 <- rowSums(m4_means[,18:34], na.rm = T)
m4_means <- as.data.frame(rbind(m4_s1,m4_s0))

obs_s1 <- rowSums(obs_means[,1:17], na.rm = T)
obs_s0 <- rowSums(obs_means[,18:34], na.rm = T)
obs_means <- as.data.frame(rbind(obs_s1,obs_s0))

diff <- m4_means - m2_means

model_fit_2 <- (m2_means-obs_means)^2
model_fit_2 <- model_fit_2[1,] + model_fit_2[2,]

model_fit_4 <- (m4_means-obs_means)^2
model_fit_4 <- model_fit_4[1,] + model_fit_4[2,]

model_fit_2 <- rbind(model_fit_2[1:17],model_fit_2[18:34])
model_fit_2 <- colSums(model_fit_2)

model_fit_table <- rbind(model_fit_4, model_fit_2)
model_fit_table <- round(model_fit_table, 3)



# 8. Model Tables ---------------------------------------------------------

        #### 8.1 Factor loadings ####

uni_graded_sum <- summary(uni_graded_model)
uni_graded_f <- as.data.frame(uni_graded_sum$rotF)
uni_graded_f <- round(uni_graded_f, 2)
#write.table(uni_graded_f, "uni_graded_f", row.names = T, quote = F, sep = "\t")

two_dim_sum <- summary(two_dim_model)
two_dim_f <- as.data.frame(two_dim_sum$rotF)
two_dim_f <- round(two_dim_f, 2)
#write.table(two_dim_f, "two_dim_f", row.names = T, quote = F,sep = "\t")

three_dim_sum <- summary(three_dim_model)
three_dim_f <- as.data.frame(three_dim_sum$rotF)
three_dim_f <- round(three_dim_f, 2)
#write.table(three_dim_f, "three_dim_f", row.names = T, quote = F,sep = "\t")

final_dim_sum <- summary(final_three_dim_model)
final_dim_f <- as.data.frame(final_dim_sum$rotF)
final_dim_f <- round(final_dim_f,2)
#write.table(final_dim_f, "final_dim_f", row.names = T, quote = F,sep = "\t")


        #### 8.2 Difficulty parameters ####

#uni-dimensional (Model 1)
uni_graded_coef <- coef(uni_graded_model, IRTpars = T, simplify = T)
uni_graded_p <- as.data.frame(uni_graded_coef$items)
uni_graded_p <- round(uni_graded_p,2)

#two-dimensional (Model 2)
two_dim_coef <- coef(two_dim_model, IRTpars = T, simplify = T)
two_dim_p <- as.data.frame(two_dim_coef$items)
two_dim_p <- round(two_dim_p, 2)

#three-dimensional (Model 3)
three_dim_coef <- coef(three_dim_model, IRTpars = T, simplify = T)
three_dim_p <- as.data.frame(three_dim_coef$items)
three_dim_p <- round(three_dim_p, 2)


#joint model (Model 4)
final_three_dim_coef <- coef(final_three_dim_model, simplify = T)
final_three_dim_coef <- final_three_dim_coef$items
final_three_dim_coef <- as.data.frame(final_three_dim_coef)




# 9. Preparing Plots ----------------------------------------------------------------


        #### 9.1 Preparing difficulty datasets for Models 0-3 ####


##### uni-dimensional model ####
uni_graded_coef <- coef(uni_graded_model, IRTpars = T)
uni_graded_coef <- as.data.frame(uni_graded_coef[1:17])
uni_graded_coef <- uni_graded_coef[, !grepl("\\.a\\d+", colnames(uni_graded_coef))] # removing discrimination parameters
uni_graded_coef <- as.data.frame(t(uni_graded_coef))
uni_graded_coef$subject <- gsub("\\..*", "", row.names(uni_graded_coef))
  uni_graded_coef$index <- as.factor(rep(1:5,17)) #index needed for plotting
  uni_graded_coef$model <- as.factor(rep(1, 85))  # model index

##### two-dimensional model ####
two_dim_coef <- coef(two_dim_model, IRTpars = T)
two_dim_coef <- as.data.frame(two_dim_coef[1:17])
two_dim_coef <- two_dim_coef[, !grepl("\\.a\\d+", colnames(two_dim_coef))]
two_dim_coef <- as.data.frame(t(two_dim_coef))
two_dim_coef$subject <- gsub("\\..*", "", row.names(two_dim_coef))
  two_dim_coef$index <- as.factor(rep(6:10, 17))
  two_dim_coef$model <- as.factor(rep( 2, 85))

##### three-dimensional model ####
three_dim_coef <- coef(three_dim_model, IRTpars = T)
three_dim_coef <- as.data.frame(three_dim_coef[1:17])
three_dim_coef <- three_dim_coef[, !grepl("\\.a\\d+", colnames(three_dim_coef))]
three_dim_coef <- as.data.frame(t(three_dim_coef))
three_dim_coef$subject <- gsub("\\..*", "", row.names(three_dim_coef))
  three_dim_coef$index <- as.factor(rep(11:15,17))
  three_dim_coef$model <- as.factor(rep(3, 85))

        #### 9.2 Preparing difficulty datasets for Model 4 ####

#Parameters to transform paramters into standard parameterization must be done manually
final_three_dim_coef <- coef(final_three_dim_model)
final_three_dim_coef <- as.data.frame(final_three_dim_coef)

#removing discrimination parameters that are zero or NA
final_three_dim_coef[final_three_dim_coef == 0] <- NA
na_cols <- which(apply(is.na(final_three_dim_coef), 2, all))
final_three_dim_coef <- final_three_dim_coef[, -na_cols]

#Removing standard errors
final_three_dim_coef <- final_three_dim_coef[1,]
final_three_dim_coef <- t(final_three_dim_coef)

#removing parameters of the GGUM model
final_three_dim_coef <- final_three_dim_coef[34:135]

        ##### 9.21 Transforming point estimates ####
        
#vector containing the position of the discrimination parameters
a_vec <- seq(from = 1, to = 102, by = 6)

#applying the formula -d/a to each difficulty parameter
for(i in a_vec){
  for(k in 1:5){
    parameter <- i + k
    final_three_dim_coef[parameter] = -final_three_dim_coef[parameter]/final_three_dim_coef[i]
  }
}


        ##### 9.22 Transforming Standard errors ####

#list of parameters
parvec <- (extract.mirt(final_three_dim_model, 'parvec'))

#parameter covariance matrix
vcov <- vcov(final_three_dim_model)

partransformed <- data.frame(
  a = NA,
  b = NA)

#Transforming standard errors using the delta method
for (i in a_vec) {
  for (k in 1:5) {
    
    parameter <- i + k
    pick <- c(i, parameter)
    ad <- final_three_dim_coef[pick]
    v <- vcov[pick, pick]
    SEs <- deltamethod(list(~ x1, ~ -x2/x1), ad, v)
    partransformed <- rbind(partransformed,SEs)
    
  }
}

partransformed <- partransformed[-1,] # removing first NA row
partransformed$subject <- rep(colnames(gradedf), each = 5) # taking subject names from gradedf

#removing discrimination parameters from the list of transformed point estimates
final_three_dim_coef <- final_three_dim_coef[-a_vec]


#creating 95% CIs
partransformed$CIa <- partransformed$a*1.96 
partransformed$CIb <- partransformed$b*1.96
partransformed$par <- final_three_dim_coef
partransformed$CI_2.5 <- partransformed$par-partransformed$CIb
partransformed$CI_97.5 <- partransformed$par+partransformed$CIb

partransformed <- partransformed[c("par","CI_2.5","CI_97.5","subject")]

partransformed$index <- as.factor(rep(16:20, 17))
partransformed$model <- as.factor(rep(4, 85))



# 10. Plotting ------------------------------------------------------------


plot_names <- as.data.frame(subject_ordered)
names(plot_names) <- "R_name"
plot_names$plot_names <- c("History","Religion","Psychology","Law","Politics",
                           "Marketing","Sociology","Norwegian, written",
                           "Norwegian, oral","Norwegian, secondary",
                           "English, written","English, oral","Biology",
                           "Math, STEM","Math, sociological", "Chemistry","Physics")

        #### 10.1 difficulty parameters for Models 0-3 ####

one_two_three_coef <- rbind(uni_graded_coef,two_dim_coef,three_dim_coef)
one_two_three_coef$subject <- rep(plot_names$plot_names, each = 5, times = 3)
one_two_three_coef$subject <- factor(one_two_three_coef$subject, levels = plot_names$plot_names)


ggplot(one_two_three_coef, aes(x = subject, y = par, color = factor(model), group = index)) +
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.3) +
  geom_point(size = 1.5)+
  geom_line(linewidth = 0.7)+
  labs(x = "Subject", y="θ")+
  scale_color_manual(values = c( "#85dc5d","#4459f8", "#b04d9e"), name = "Model",
                     labels = c("1","2", "3"))+
  guides(color = guide_legend(override.aes = list(shape = 15))) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11), # adjusting size of the x-axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, size = 13),  # centers the plot title and changes its font size
    panel.background = element_rect(fill = NA), # setting a white background for the plots
    panel.grid.major.y = element_line(colour = "grey80", size = 1/4), # Changing the size and color of the horizontal grid lines
    panel.grid.major.x = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.8, "cm"))


        #### 10.2 difficulty parameters of Models 2 and 4 ####

two_joint_coef <- rbind(two_dim_coef, partransformed)
two_joint_coef$subject <- rep(plot_names$plot_names, each = 5, times = 2)
two_joint_coef$subject <- factor(two_joint_coef$subject, levels = plot_names$plot_names)

ggplot(two_joint_coef, aes(x = subject, y = par, color = factor(model), group = index)) +
  geom_errorbar(aes(ymin = CI_2.5, ymax = CI_97.5), width = 0.3) +
  geom_point(size = 1.5) +
  geom_line(size = 0.7)+
  scale_x_discrete(labels=c(plot_names))+
  labs(x = "Subject", y="θ",)+
  scale_color_manual(values = c("#4459f8","#e02941"), name = "Model",
                     labels = c("2","4"))+
  guides(color = guide_legend(override.aes = list(shape = 15))) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11), # adjusting size of the x-axis
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    plot.title = element_text(hjust = 0.5, size = 13),  # centers the plot title and changes its font size
    panel.background = element_rect(fill = NA), # setting a white background for the plots
    panel.grid.major.y = element_line(colour = "grey80", size = 1/4), # Changing the size and color of the horizontal grid lines
    panel.grid.major.x = element_blank(),
    legend.key = element_blank(),
    legend.key.size = unit(0.8, "cm")) # removing the vertical grid lines



        #### 10.3 Expected subject means for Model 4 ####

#DF with observed grades and Model 4 expected grades
gg_grades4 <- as.data.frame(rbind(as.matrix(colMeans(final_df[12:28],na.rm = T)),
                                 as.matrix(colMeans(ex_joint_free[12:28]))))

gg_grades4$subject_name <- rep(plot_names[match(row.names(grades_o4)[1:17], plot_names$R_name), "plot_names"],2)

gg_grades4$index <- as.factor(c(rep("observed grades",17),
                                rep("expected grades",17)))
names(gg_grades4)[1] <- "grade"
gg_grades4$grade <- round(gg_grades4$grade, 2)

gg_grades4$width <- ifelse(gg_grades4$index == "observed grades", 0.5, 1)

# reverse the order of the factor levels
gg_grades4$subject_name_ordered <- factor(gg_grades4$subject_name,
                                         levels = unique(gg_grades4$subject_name))
gg_grades4$subject_name_ordered <- fct_rev(gg_grades4$subject_name_ordered)


#Plot
ggplot(gg_grades4, aes(x = subject_name_ordered , y = grade, fill = index)) +
  geom_col(position = position_dodge2(width = 0.2), width = gg_grades$width[1:34]) +
  geom_text(data = subset(gg_grades4, index == "expected grades"), aes(label = grade), 
            position = position_dodge2(width = 0.2), vjust = 0.8,hjust = -0.15, size = 2.5) + # add labels to the relevant bars
  coord_flip(ylim = c(0,5)) +
  scale_fill_manual(values = c("#e02941","#ffc78a" ), name = "Grade Type") + 
  labs(x = "Subject", y = "Mean grade") +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11), # adjusting size of the x-axis
    axis.text.x = element_text( size = 9),
    axis.text.y = element_text( size = 9),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = NA),
    legend.key.size = unit(0.8, "lines"), # increases the size of the legend icons
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)) # setting a white background for the legend

        #### 10.4 Expected subject means for Model 2 ####


#DF with observed grades and Model 2 expected grades
gg_grades2 <- as.data.frame(rbind(as.matrix(colMeans(final_df[12:28],na.rm = T)),
                                  as.matrix(colMeans(ex_grades_two_dim))))

gg_grades2$subject_name <- rep(plot_names[match(row.names(grades_o4)[1:17], plot_names$R_name), "plot_names"],2)

gg_grades2$index <- as.factor(c(rep("observed grades",17),
                                rep("expected grades",17)))
names(gg_grades2)[1] <- "grade"
gg_grades2$grade <- round(gg_grades2$grade, 2)

gg_grades2$width <- ifelse(gg_grades2$index == "observed grades", 0.5, 1)

# reverse the order of the factor levels
gg_grades2$subject_name_ordered <- factor(gg_grades2$subject_name,
                                          levels = unique(gg_grades2$subject_name))
gg_grades2$subject_name_ordered <- fct_rev(gg_grades2$subject_name_ordered)



#Plot
ggplot(gg_grades2, aes(x = subject_name_ordered , y = grade, fill = index)) +
  geom_col(position = position_dodge2(width = 0.2), width = gg_grades$width[1:34]) +
  geom_text(data = subset(gg_grades4, index == "expected grades"), aes(label = grade), 
            position = position_dodge2(width = 0.2), vjust = 0.8,hjust = -0.15, size = 2.5) + # add labels to the relevant bars
  coord_flip(ylim = c(0,5)) +
  scale_fill_manual(values = c("#e02941","#ffc78a" ), name = "Grade Type") + 
  labs(x = "Subject", y = "Mean grade") +
  guides(fill = guide_legend(reverse = T)) +
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11), # adjusting size of the x-axis
    axis.text.x = element_text( size = 9),
    axis.text.y = element_text( size = 9),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = NA),
    legend.key.size = unit(0.8, "lines"), # increases the size of the legend icons
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)) # setting a white background for the legend




        #### 10.5 Expected Items ####


# Expected item for Model 2
theta_two <- cbind((seq(-3,3, length.out = 200)),
                   seq(-3,3, length.out = 200))

biology_2 <- extract.item(two_dim_model, 13)
math_n_2 <- extract.item(two_dim_model, 14)
math_s_2 <- extract.item(two_dim_model, 15)
chemistry_2 <- extract.item(two_dim_model, 16)
physics_2 <- extract.item(two_dim_model, 17)


# Expected item for Model 4


biology_4 <- extract.item(final_three_dim_model, 24)
math_n_4 <- extract.item(final_three_dim_model, 25)
math_s_4 <- extract.item(final_three_dim_model, 26)
chemistry_4 <- extract.item(final_three_dim_model, 27)
physics_4 <- extract.item(final_three_dim_model, 28)

theta_three <- cbind((seq(-3,3, length.out = 200)),
                   seq(-3,3, length.out = 200),
                   seq(-3,3, length.out = 200))



expected_items <- data.frame(matrix(NA, nrow=2000, ncol=10))

expected_items <- data.frame(biology2 = expected.item(biology_2,theta_two, min = 1),
                             math_n2 = expected.item(math_n_2,theta_two, min = 1),
                             math_s2 = expected.item(math_s_2,theta_two, min = 1),
                             chemistry2 = expected.item(chemistry_2,theta_two, min = 1),
                             physics2 = expected.item(physics_2,theta_two, min = 1),
                             biology4 = expected.item(biology_4,theta_three, min = 1),
                             math_n4 = expected.item(math_n_4,theta_three, min = 1),
                             math_s4 = expected.item(math_s_4,theta_three, min = 1),
                             chemistry4 = expected.item(chemistry_4,theta_three, min = 1),
                             physics4 = expected.item(physics_4,theta_three, min = 1),
                             theta = seq(-3,3, length.out = 200))
                             


names(expected_items) <- c("biology2","Math_n2","math_s2","chemistry2","physics2"
                           ,"biology4","Math_n4","math_s4","chemistry4","physics4")
save(expected_items, file = "expected_items.Rdata")

biology <- ggplot(data = expected_items, aes(x = theta ))+
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))+
  geom_line(aes(y = biology2), color = "#4459f8", size = 1)+
  geom_line(aes(y = biology4), color = "#e02941", size = 1)+
  labs(x = "θ", y="Expected Grade")+
  ggtitle("Biology")+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11),
    axis.text.x = element_text( size = 10),
    axis.text.y = element_text( size = 10),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    legend.key.size = unit(0.8, "lines"), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)) 

Math_n <- ggplot(data = expected_items, aes(x = theta ))+
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))+
  geom_line(aes(y = math_n2), color = "#4459f8", size = 1)+
  geom_line(aes(y = math_n4), color = "#e02941", size = 1)+
  labs(x = "θ", y="Expected Grade")+
  ggtitle("Math, STEM")+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11),
    axis.text.x = element_text( size = 10),
    axis.text.y = element_text( size = 10),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    legend.key.size = unit(0.8, "lines"), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)) 

Math_s <-ggplot(data = expected_items, aes(x = theta ))+
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))+
  geom_line(aes(y = math_s2), color = "#4459f8", size = 1)+
  geom_line(aes(y = math_s4), color = "#e02941", size = 1)+
  labs(x = "θ", y="Expected Grade")+
  ggtitle("Math, sociological")+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11),
    axis.text.x = element_text( size = 10),
    axis.text.y = element_text( size = 10),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    legend.key.size = unit(0.8, "lines"), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)) 

chemistry <- ggplot(data = expected_items, aes(x = theta ))+
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))+
  geom_line(aes(y = chemistry2), color = "#4459f8", size = 1)+
  geom_line(aes(y = chemistry4), color = "#e02941", size = 1)+
  labs(x = "θ", y="Expected Grade")+
  ggtitle("Chemistry")+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11),
    axis.text.x = element_text( size = 10),
    axis.text.y = element_text( size = 10),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    legend.key.size = unit(0.8, "lines"), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)) 

physics <- ggplot(data = expected_items, aes(x = theta ))+
  scale_y_continuous(limits = c(1, 6), breaks = seq(1, 6, 1))+
  geom_line(aes(y = physics2), color = "#4459f8", size = 1)+
  geom_line(aes(y = physics4), color = "#e02941", size = 1)+
  labs(x = "θ", y="Expected Grade")+
  ggtitle("Physics")+
  theme(
    text = element_text(family = "Times New Roman"),
    axis.title.x = element_text(size = 11),
    axis.text.x = element_text( size = 10),
    axis.text.y = element_text( size = 10),
    plot.title = element_text(hjust = 0.5, size = 13),
    panel.background = element_rect(fill = NA),
    panel.border = element_rect(color = "grey", fill = NA, size = 1),
    legend.key.size = unit(0.8, "lines"), 
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)) 




