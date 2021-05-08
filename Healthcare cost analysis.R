#############------Pinku Pandey------########
                ##--April 2021--##
############------Assessment Project-7---#####

####---problem statement for project---######

#Healthcare cost analysis

#Project 7

#DESCRIPTION

#Background and Objective:

#A nationwide survey of hospital costs conducted by the US Agency for Healthcare consists of hospital records of inpatient samples. The given data is restricted to the city of Wisconsin and relates to patients in the age group 0-17 years. The agency wants to analyze the data to research on healthcare costs and their utilization.

#Domain: Healthcare

#Dataset Description:

#Here is a detailed description of the given dataset:
#Attribute 	Description
#Age  	Age of the patient discharged
#Female  	A binary variable that indicates if the patient is female
#Los 	Length of stay in days
#Race  	

#Race of the patient (specified numerically)
#Totchg 	Hospital discharge costs
#Aprdrg 	All Patient Refined Diagnosis Related Groups

#Analysis to be done: 

#1. To record the patient statistics, the agency wants to find the age category of people who frequently visit the hospital and has the maximum expenditure.

#2. In order of severity of the diagnosis and treatments and to find out the expensive treatments, the agency wants to find the diagnosis-related group that has maximum hospitalization and expenditure.

#3. To make sure that there is no malpractice, the agency needs to analyze if the race of the patient is related to the hospitalization costs.

#4. To properly utilize the costs, the agency has to analyze the severity of the hospital costs by age and gender for the proper allocation of resources.

#5. Since the length of stay is the crucial factor for inpatients, the agency wants to find if the length of stay can be predicted from age, gender, and race.

#6. To perform a complete analysis, the agency wants to find the variable that mainly affects hospital costs.

#Disclaimer: In Business Analytics, there are different ways of solving the same set of problems. Feel free to explore other ways of answering these questions.

##########---END-------#################

############----Main Entry----################
library("readxl")
library(ggplot2)

hospital_cost<-read_excel("1555054100_hospitalcosts.xlsx")
#View(hospital_cost)
head(hospital_cost)
summary(hospital_cost)
##########-----Analysis Part -1 ----##########
# --- Select Age of patient ---##
age <- hospital_cost$AGE
head(age)
summary(age)
table(age)
##--- Plot Histogram to show age of patient---###
#hist(age)
ggplot(hospital_cost, aes(x=age)) +
  geom_histogram(color="darkblue", fill="lightblue")+
  ggtitle("Patient Age") +
  theme(plot.title = element_text(hjust = 0.5))

summary(as.data.frame(age))
max(table(age))  
max(summary(as.factor(age)))
which.max(table(age))
aggregate_age <- aggregate(TOTCHG ~ AGE, data = hospital_cost, sum)
max(aggregate_age)


#########------#Analysis part -2----######
treatment  <- table(hospital_cost$APRDRG) 
treatment
diagnosis <- as.data.frame(treatment)
names(diagnosis)[1] = 'Diagnosis Group'
diagnosis
which.max(table(hospital_cost$APRDRG))
which.max(treatment)
#which.max(diagnosis)          
result <- aggregate(TOTCHG ~ APRDRG, data = hospital_cost, sum)
result
which.max(result$TOTCHG)
result[which.max(result$TOTCHG),]

#########------#Analysis part -3----######
table(hospital_cost$RACE)
#class(hospital_cost)
# make factor....
hospital_cost$RACE <- as.factor(hospital_cost$RACE)
fit <- lm(TOTCHG ~ RACE,data=hospital_cost)
fit
summary(fit)
fit1 <- aov(TOTCHG ~ RACE,data=hospital_cost)
summary(fit1)
hospital_cost <- na.omit(hospital_cost)

#########------#Analysis part -4----######

table(hospital_cost$FEMALE)
fit_analysis  <- aov(TOTCHG ~ AGE+FEMALE,data=hospital_cost)
summary(fit_analysis)
fit_linear <- lm(TOTCHG ~ AGE+FEMALE,data=hospital_cost)
summary(fit_linear)

#########------#Analysis part -5----######
table(hospital_cost$LOS)
fit_analysis  <- aov(TOTCHG ~ AGE+FEMALE+RACE,data=hospital_cost)
summary(fit_analysis)
fit_linear <- lm(TOTCHG ~ AGE+FEMALE+RACE,data=hospital_cost)
summary(fit_linear)

#########------#Analysis part -6----######
aov(TOTCHG ~.,data=hospital_cost)
mod <- lm(TOTCHG ~ .,data=hospital_cost)
summary(mod)


