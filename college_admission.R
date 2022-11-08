#Loading packages and librabies

if(!require(lubridate))install.packages("lubridate")
if(!require(readr))install.packages("readr")
if(!require(ggplot2))install.packages("ggplot2")
if(!require(tidyverse))install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(rpart)) install.packages("rpart")
if(!require(dplyr)) install.packages("dplyr")

library(caret)
library(tidyverse)
library(rpart)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)


#Set working directory
getwd()
setwd("C:/Users/eboley/OneDrive - Partners In Health/Desktop/Data/project")

#Set Relative folder path

College <- read.csv("R/College_admission.csv")


# 3 significant digits
options(digits = 3)

#Recoding categorical & numerical variables
College_clean <- College %>%
  mutate(GreLevels=ifelse(gre<440,"Low",ifelse(gre<580,"Medium","High"))) %>%
  mutate(Gender=ifelse(Gender_Male == 0, "female","male")) %>%
  mutate(Demo = recode(Race,
                       "1" = "Hispanics",
                       "2" = "Asian",
                       "3" = "African-American"))%>%
  mutate(Socioeco = recode(ses,
                           "1" = "Low",
                           "2" = "Medium",
                           "3" = "High"))


#Summary of data
summary(College_clean)
str(College_clean)

#Data type as numeric (admit, ses, race, gender, and rank), GreLevels as character variable

#Transforming categorical variables (admit, ses, gender, rank) 
College_clean$admit <- as.factor(College_clean$admit)
College_clean$ses <- as.factor(College_clean$ses)
College_clean$Race <- as.factor(College_clean$Race)
College_clean$Gender_Male <- as.factor(College_clean$Gender_Male)
College_clean$rank <- as.factor(College_clean$rank)
College_clean$GreLevels <- as.factor(College_clean$GreLevels)
College_clean$Gender <- as.factor(College_clean$Gender)
College_clean$Demo <- as.factor(College_clean$Demo)
College_clean$Socioeco <- as.factor(College_clean$Socioeco)

#Finding missing data
is.na(College_clean)
sum(is.na(College_clean))
#there is no missing values in dataset


#Distribution of gre & gpa scores among gender

College_clean %>%
  ggplot(aes(Gender,gre))+
  geom_boxplot()

College_clean %>%
  ggplot(aes(Gender,gpa))+
  geom_boxplot()

#Variation in gre & gpa by gender, race and socioenomic status

College_clean %>%
  ggplot(aes(Gender,gre, fill = Gender))+
  geom_boxplot()+
  facet_grid(~Demo)

College_clean %>%
  ggplot(aes(Gender,gpa, fill = Gender))+
  geom_boxplot()+
  facet_grid(~Demo)

College_clean %>%
  ggplot(aes(Socioeco, gre, fill = Gender))+
  #geom_density(alpha = 0.5)+
  geom_boxplot()+
  facet_wrap(~Demo)+
  theme_bw()

College_clean %>%
  ggplot(aes(Socioeco, gpa, fill = Gender))+
  #geom_density(alpha = 0.5)+
  geom_boxplot()+
  facet_wrap(~Demo)+
  theme_bw()


#Find outlers in data set
boxplot(College_clean)
boxplot(College_clean$gre)
boxplot(College_clean$gpa)

#Identifying of outliers

boxplot.stats(College_clean$gpa)$out
boxplot.stats(College_clean$gre)$out
#GPA outlier is 2.26
#GRE outlier are 300 300 220 300

#Removing outliers

College_clean <- subset(College_clean, gpa != 2.26)
College_clean <- subset(College_clean, gre != 300 & gre !=220)

#Confirmation outliers removed

boxplot(College_clean$gpa)
boxplot(College_clean$gre)

#Creating train and test sets
set.seed(42, sample.kind = "Rounding")

test_index <- createDataPartition(College_clean$admit, times = 1, p = 0.2, list = FALSE)
College_test <- College_clean[test_index,]
College_train <- College_clean[-test_index,]

nrow(College_train)
nrow(College_test)

#Proportion of individuals in the training set admitted

mean(College_train$admit == 1)

#Accuracy of guessing method
set.seed(3, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(College_test), replace = TRUE)
mean(guess == College_test$admit)


#Proportion of females admitted
College_train %>%
  group_by(Gender_Male) %>%
  summarize(Admit = mean(admit == 1)) %>%
  filter(Gender_Male == "0") %>%
  pull(Admit)

#Proportion of males admitted
College_train %>%
  group_by(Gender_Male) %>%
  summarize(Admit = mean(admit == 1)) %>%
  filter(Gender_Male == "1") %>%
  pull(Admit)

#Predicting admission by sex
# predict admission = 1 if male, 0 if female

sex_model <- ifelse(College_test$Gender == "female", 0, 1)

# Calculate accuracy
mean(sex_model == College_test$admit)

# Predicting admission by socioeconomic class

College_train %>%
  group_by(Socioeco) %>%
  summarize(Admit = mean(admit == 1))

#Accuracy of class-based prediction method
# predict admission only if socioeconomic class is low

class_model <- ifelse(College_test$ses == 1, 1, 0)

#Calculate accuracy
mean(class_model == College_test$admit)

#Prediction of admission based on class and sex
College_train %>%
  group_by(Gender, Socioeco) %>%
  summarize(admit = mean(admit == 1))
  #filter(admit > 0.5)

#
#Prediction of admission based on class and sex
College_train %>%
  group_by(Gender, Race) %>%
  summarize(admit = mean(admit == 1))
#filter(admit > 0.5)

#Confusion matrix
confusionMatrix(data = factor(sex_model), reference = factor(College_test$admit))
confusionMatrix(data = factor(class_model), reference = factor(College_test$admit))

#F means score

F_meas(data = factor(sex_model), reference = College_test$admit)
F_meas(data = factor(class_model), reference = College_test$admit)

# Admission by gre and gpa using LDA and QDA

#The accuracy on the test set for the LDA model?
set.seed(1, sample.kind = "Rounding")

train_lda <- train(admit ~ gpa, method = "lda", data = College_train)
lda_preds <- predict(train_lda, College_test)
mean(lda_preds == College_test$admit)

train_lda_a <- train(admit ~ gre, method = "lda", data = College_train)
lda_preds_a <- predict(train_lda_a, College_test)
mean(lda_preds_a == College_test$admit)

#The accuracy on the test set for the QDA model

train_qda <- train(admit ~ gpa, method = "qda", data = College_train)
qda_preds <- predict(train_qda, College_test)
mean(qda_preds == College_test$admit)

train_qda_a <- train(admit ~ gre, method = "qda", data = College_train)
qda_preds_a <- predict(train_qda_a, College_test)
mean(qda_preds_a == College_test$admit)

#The accuracy of your model (using gre as the only predictor) on the test set 
train_glm_gre <- train(admit ~ gre, method = "glm", data = College_train)
glm_preds_gre <- predict(train_glm_gre, College_test)
mean(glm_preds_gre == College_test$admit)

#The accuracy of your model (using these four predictors) on the test set?
train_glm <- train(admit ~ gre + gpa + ses + Race, method = "glm", data = College_train)
glm_preds <- predict(train_glm, College_test)
mean(glm_preds == College_test$admit)

#The accuracy of your model (using all predictors) on the test set?
str(College_train)
college_train_all <- College_train %>% select (admit:rank)
college_test_all <- College_test %>% select (admit:rank)


train_glm_all <- train(admit ~ ., method = "glm", data = college_train_all)
train_glm_all

glm_all_preds <- predict(train_glm_all, college_test_all)
glm_all_preds

mean(glm_all_preds == college_test_all$admit)

#kNN model

