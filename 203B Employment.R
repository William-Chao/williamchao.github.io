rm(list=ls())
setwd ("/Users/williamchao/Desktop/Grad/Semester 2/ECON 203B/Research Paper 203B")
library(broom)
library(tidyr)
library(ggplot2)
library(finalfit)
library(dplyr)
library(stargazer)
library(tidyverse)
employment <- read.csv(file = "emp.csv")
summary(Employment)
glimpse(Employment)
Employment <- subset(employment, select = -c(RACED, EMPSTATD, EDUCD))
Employment <- subset(Employment, EMPSTAT!= 0 & EMPSTAT !=3)
Employment$SEX <- ifelse(Employment$SEX == 1, 1, 0)
Employment$EMPSTAT <- ifelse(Employment$EMPSTAT == 1, 1, 0)
Employment$Asian <- 0
Employment$Asian[Employment$RACE == 4 | Employment$RACE == 5 | Employment$RACE == 6] <- 1
Employment$Asian_M <- Employment$Asian*Employment$SEX
Employment$Black <- ifelse(Employment$RACE == 2, 1, 0)
Employment$Black_M <- ifelse(Employment$Black == 1 & Employment$SEX == 1, 1, 0)
Employment$White <- ifelse(Employment$RACE == 1, 1, 0)
Employment$Natives <- ifelse(Employment$RACE == 3, 1, 0)
Employment$Unspecified <- ifelse(Employment$RACE > 6, 1, 0)
Employment$WORKEDYR <- ifelse(Employment$WORKEDYR == 3, 1, 0)
Employment$RaceSpecificed <- ifelse(Employment$Asian == 1, 2, 0)
Employment$RaceSpecificed[Employment$Black == 1] <- 3
Employment$RaceSpecificed[Employment$White == 1] <- 1
Employment$RaceSpecificed[Employment$Natives == 1] <- 4
write.csv(Employment, file = 'employment1.csv')
myreg <- lm(EMPSTAT ~ YEAR + AGE + EDUC + Asian + Asian*SEX + Black + Black*SEX
            + Natives + Unspecified
            + WORKEDYR, 
            data = Employment)
summary(myreg)
myprobit <- glm(EMPSTAT ~ YEAR + AGE + SEX + EDUC + Asian + Asian_M + Black + Black_M 
                + Natives + Unspecified
                + WORKEDYR, 
                data = Employment, family=binomial(link = 'probit'))
summary(myprobit)
myregfixed <- glm(EMPSTAT ~ AGE + SEX + EDUC + Asian + Asian_M + Black + Black_M 
                  + Natives + Unspecified
                  + WORKEDYR + factor(YEAR), data = Employment, 
                  family = binomial(link='probit'))
summary(myregfixed)
library(stargazer)
table2008 <- stargazer(myreg, myprobit, myregfixed,
          column.labels = c('OLS Regression', 'Probit Model',
                            'Probit Model with Fixed Effect'),
          type = 'text',
          omit.stat = c("rsq","ser"), 
          digits = 2 
          )
write.table(table2008, file = "2008-2010EMP.txt", sep = ',', quote = FALSE, row.names = F)
library(ggplot2)
library(finalfit)
library(dplyr)
Employment %>%
  ggplot(aes(x = RaceSpecificed)) +
  geom_histogram(bins = 20, alpha=0.6, color = 'blue', fill = 'blue') +
  ggtitle('Race and Employment')

Employment %>%
  ggplot(aes(x = EDUC)) +
  geom_histogram(bins = 55, alpha=0.6, color = 'blue', fill = 'blue') +
  ggtitle('Education and Employment')

sumstat <- Employment %>%
  # Select and rename variables
  select(
    `Sex (0 = Female, 1 = Male)` = SEX,
    `Age` = AGE,
    `Education Level` = EDUC,
    `Worked Last Year` = WORKEDYR,
    `Asian (0 = No, 1 = Yes)` = Asian,
    `Mixed (0 = No, 1 = Yes)` = Unspecified,
    `Black (0 = No, 1 = Yes)` = Black,
    `Native American (0 = No, 1 = Yes)` = Natives
  ) %>%
  
  # Find mean, standard deviation, min, and max for each value
  summarise_each(tibble::lst(mean, sd, min, max)) %>%
  
  #Move summary stats into columns
  gather(key, value, everything()) %>%
  separate(key, into = c('variable', 'stat'), sep = '_') %>%
  spread(stat, value) %>%
  
  #Set order of summary stats
  select(variable, mean, sd, min, max)

sumstat
write.table(sumstat, file = "summarystat2008-2010.txt", sep = ',', quote = FALSE, row.names = F)

##Measuring Probit
P1Asian_M <- predict(myprobit, type = "response")
newEMP_M = Employment
newEMP_M$Asian_M <- newEMP_M$Asian_M + 1
head(newEMP_M)
P2Asian_M <- predict(myprobit,newEMP_M, type = "response")
mean (P2Asian_M-P1Asian_M)

P1Asian <- predict(myprobit, type = "response")
newEMP = Employment
newEMP$Asian <- newEMP$Asian + 1
head(newEMP)
P2Asian <- predict(myprobit,newEMP, type = "response")
mean (P2Asian-P1Asian)
##Asian: 0.0053M, 0.0051F
P1White_W <- predict(myprobit, type = "response")
newEMP1_W = Employment
newEMP1_W$White_W <- newEMP1_W$White_W + 1
head(newEMP1_W)
P2White_W <- predict(myprobit,newEMP1_W, type = "response")
mean (P2White_W-P1White_W)
##White: -0.0124F
P1Black_M <- predict(myprobit, type = "response")
newEMP2_M = Employment
newEMP2_M$Black_M <- newEMP2_M$Black_M + 1
head(newEMP2_M)
P2Black_M <- predict(myprobit,newEMP2_M, type = "response")
mean (P2Black_M-P1Black_M)

P1Black <- predict(myprobit, type = "response")
newEMP2 = Employment
newEMP2$Black <- newEMP2$Black + 1
head(newEMP2)
P2Black <- predict(myprobit,newEMP2, type = "response")
mean (P2Black-P1Black)
##Black: -0.00024M, -0.024F
P1Natives <- predict(myprobit, type = "response")
newEMP3 = Employment
newEMP3$Natives <- newEMP3$Natives + 1
head(newEMP3)
P2Natives <- predict(myprobit,newEMP3, type = "response")
mean (P2Natives-P1Natives)

##Natives: -0.0295
P1Unspecified <- predict(myprobit, type = "response")
newEMP4 = Employment
newEMP4$Unspecified <- newEMP4$Unspecified + 1
head(newEMP4)
P2Unspecified <- predict(myprobit,newEMP4, type = "response")
mean (P2Unspecified-P1Unspecified)

##Mixed: -0.0017
P1Educ <- predict(myprobit, type = "response")
newEmp5 = Employment
newEmp5$EDUC <- newEmp5$EDUC+ sd(Employment$EDUC)
head(newEmp5)
P2Educ <- predict(myprobit,newEmp5, type = "response")
mean (P2Educ-P1Educ)
##Education: 0.0145
P1Age <- predict(myprobit, type = "response")
newEmp6 = Employment
newEmp6$AGE <- newEmp6$AGE+ 1
head(newEmp6)
P2Age <- predict(myprobit,newEmp6, type = "response")
mean (P2Age-P1Age)
##Age: 0.001
P1work <- predict(myprobit, type = "response")
newEmp7 = Employment
newEmp7$WORKEDYR <- newEmp7$WORKEDYR+ 1
head(newEmp7)
P2work <- predict(myprobit,newEmp7, type = "response")
mean (P2work-P1work)
##Work: 0.081


##Fixed effect
F1Asian_M <- predict(myregfixed, type = "response")
newFix_M = Employment
newFix_M$Asian_M <- newFix_M$Asian_M + 1
head(newFix_M)
F2Asian_M <- predict(myregfixed,newFix_M, type = "response")
mean (F2Asian_M-F1Asian_M)

F1Asian <- predict(myregfixed, type = "response")
newFix = Employment
newFix$Asian <- newFix$Asian + 1
head(newFix)
F2Asian <- predict(myregfixed,newFix, type = "response")
mean (F2Asian-F1Asian)
##Asian: 0.004M, -0.0003F


F1White_W <- predict(myregfixed, type = "response")
newFix1_W = Employment
newFix1_W$White_W <- newFix1_W$White_W + 1
head(newFix1_W)
F2White_W <- predict(myregfixed,newFix1_W, type = "response")
mean (F2White_W-F1White_W)
##White: -0.0124F
F1Black_M <- predict(myregfixed, type = "response")
newFix2_M = Employment
newFix2_M$Black_M <- newFix2_M$Black_M + 1
head(newFix2_M)
F2Black_M <- predict(myregfixed,newFix2_M, type = "response")
mean (F2Black_M-F1Black_M)

F1Black <- predict(myregfixed, type = "response")
newFix2 = Employment
newFix2$Black <- newFix2$Black + 1
head(newFix2)
F2Black <- predict(myregfixed,newFix2, type = "response")
mean (F2Black-F1Black)
##Black: -0.025M, -0.04F
F1Natives <- predict(myregfixed, type = "response")
newFix3 = Employment
newFix3$Natives <- newFix3$Natives + 1
head(newFix3)
F2Natives <- predict(myregfixed,newFix3, type = "response")
mean (F2Natives-F1Natives)

##Natives: -0.022M, -0.054F
F1Unspecified <- predict(myregfixed, type = "response")
newFix4 = Employment
newFix4$Unspecified <- newFix4$Unspecified + 1
head(newFix4)
F2Unspecified <- predict(myregfixed,newFix4, type = "response")
mean (F2Unspecified-F1Unspecified)

##Mixed: -0.007M, -0.009F

F1Educ <- predict(myregfixed, type = "response")
newFix4 = Employment
newFix4$EDUC <- newFix4$EDUC + sd(Employment$EDUC)
head(newFix4)
F2Educ <- predict(myregfixed,newFix4, type = "response")
mean (F2Educ-F1Educ)
##Educ: 0.0148
F1WY <- predict(myregfixed, type = "response")
newFix5 = Employment
newFix5$WORKEDYR <- newFix5$WORKEDYR + 1
head(newFix5)
F2WY <- predict(myregfixed,newFix5, type = "response")
mean (F2WY-F1WY)
##Worked Year: 0.08106
F1age <- predict(myregfixed, type = "response")
newFix6 = Employment
newFix6$AGE <- newFix6$AGE + 1
head(newFix6)
F2age <- predict(myregfixed,newFix6, type = "response")
mean (F2age-F1age)
##Age: 0.001

summary(Employment$WORKEDYR)

ggplot(Employment, aes(x = EDUC, y = EMPSTAT)) + 
  geom_point() +
  geom_smooth(method = "glm", method.args=list(family=binomial(link = "probit")), se = F) +
  geom_smooth(method = "lm", se = F) + 
  ggtitle("LPM Vs. Probit")

Employment %>%
  ggplot(aes(x = EDUC, y = EMPSTAT, group = RaceSpecificed, color = RaceSpecificed)) +
  geom_point() +
  geom_smooth(method = "glm", method.args=list(family=binomial(link = "probit")), se = F) +
  geom_smooth(method = "lm", se = F) + 
  ggtitle("LPM Vs. Probit")
