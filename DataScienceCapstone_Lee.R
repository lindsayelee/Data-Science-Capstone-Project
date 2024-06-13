#Load packages ----------
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(summarytools)) install.packages("summarytools", repos = "http://cran.us.r-project.org")
if(!require(papaja)) install.packages("papaja", repos = "http://cran.us.r-project.org")

library(here)
library(readxl)
library(haven)
library(tidyr)
library(dplyr)
library(ggplot2)
library(summarytools)
library(papaja)

#Import Dataset -----------
#Access original PISA 2022 dataset from 
#https://www.oecd.org/pisa/data/2022database/
#This dataset was from the SPSS (TM) Data Files (compressed) - Student questionnaire data file
#Webscraping encountered a HTTP 403 error and could not access website even when specifying user agent for my browser. 

#Option 1 for Accessing condensed Dataset
#Automatically download condensedStudentQQQ from my Github account: lindsayelee

#I asked ChatGPT the following prompt "How to automatically download condensedStudentQQQ.xlsx from https://github.com/lindsayelee/Data-Science-Capstone-Project in R"
#OpenAI. (2023). ChatGPT (Mar 14 version) [Large language model]. https://chat.openai.com/chat 

#ChatGPT gave the following solution of code:
# Define the URL of the file
file_url <- "https://github.com/lindsayelee/Data-Science-Capstone-Project/raw/main/condensedStudentQQQ.xlsx"
# Define the destination file path
dest_file <- "condensedStudentQQQ.xlsx"
# Download the file
download.file(file_url, dest_file, mode = "wb")
# Check if the file has been downloaded successfully
if (file.exists(dest_file)) {
  print("File downloaded successfully.")
} else {
  print("File download failed.")
}

# Read the Excel file with readxl package
bcpwb_data <- readxl::read_excel(dest_file)
# Print the first few few rows of dataset to verify
head(bcpwb_data)

#Option 2 for accessing dataset
#Manually download from PISA website and setwd()
#use local file path if you download from the website (I used this to create a condensed version of the dataset)
#I have a Mac and these were my original file paths and how I cleaned the dataset to create the xlsx version
#rawdata <- haven::read_sav((path=here("~/Documents/GitHub/Data Science Capstone Project/Data", "CY08MSP_STU_QQQ.sav")))
#View(rawdata)
#
#data = rawdata |> select("Country", "STRATUM", "ST001D01T","ST003D03T","ST004D01T","ST267Q01JA","ST267Q02JA","ST267Q03JA","ST267Q04JA", "PAREDINT","HISCED",
#                         "STRESAGR","FAMSUP", "PROBSELF","BELONG","CURIOAGR","PERSEVAGR","EXPWB")
#
#bcpwb_data = data |>
#  select("Country","ST003D03T", "ST004D01T","ST001D01T","PAREDINT","HISCED","STRESAGR","FAMSUP", "PROBSELF","BELONG","CURIOAGR","PERSEVAGR","EXPWB")

#remove missing values through listwise deletion
#bcpwb_data = na.omit(bcpwb_data)

#rename variables
#bcpwb_data = bcpwb_data %>% rename(
#  "BirthYear" = "ST003D03T",
#  "Grade" = "ST001D01T",
#   "Gender" = "ST004D01T",
#  "HighestParentalEd" = "HISCED",
#  "HighestParentalEd_Index" = "PAREDINT",
#  "Belongingness" = "BELONG",
#  "Curiousity" = "CURIOAGR",
#  "Perseverance" = "PERSEVAGR",
#  "StressResistance" = "STRESAGR",
#  "FamilySupport" = "FAMSUP",
#  "ProblemSelfDirectedLearn" = "PROBSELF",
#  "WellBeing"= "EXPWB")

#determine age from birth year 
#bcpwb_data$BirthYear = as.numeric(bcpwb_data$BirthYear)
#
#bcpwb_data = bcpwb_data %>% 
#  mutate(Age = (2022 - BirthYear))
##only 15 & 16 year olds at time of survey?

#remove 96 from Grade
#bcpwb_data = subset(bcpwb_data, Grade != "96")
#bcpwb_data  = droplevels(bcpwb_data)
#
#exclude Birth Year
#bcpwb_data = bcpwb_data %>% 
#  select(-BirthYear)

#writexl::write_xlsx(bcpwb_data, "condensedStudentQQQ.xlsx")

#Main Research Question of Interest -----------------
#To what extent does student self-beliefs of belongingness, curiosity, stress resistance, perseverence, family support, problems with self-directed learning predict well-being?
#Does this differ by grade level, gender, and/or country?

#Start running code from here after you load from my GitHub Repository!!!
#check class of each variable
sapply(bcpwb_data, class)

#recode Gender to Female and Male for easier interpretation
#recode Country to Country and each level to each respective country name for easier plot interpretation
#https://www.oecd.org/pisa/aboutpisa/pisa-2022-participants.htm

#recode Highest level of parental education to ISCED levels described by UNESCO/PISA documentation 
#https://www.oecd-ilibrary.org/sites/1e412e2e-en/index.html?itemId=/content/component/1e412e2e-en#section-d1e18977-71ac67bf62
bcpwb_data = bcpwb_data %>% mutate(
  Gender = case_when(
    Gender == "1" ~ "Female",
    Gender == "2" ~ "Male"),
  Country = case_when(
    CNT == "BRA" ~ "Brazil",
    CNT == "ESP" ~ "Spain",
    CNT == "FRA" ~ "France",
    CNT == "HKG" ~ "Hong Kong",
    CNT == "HUN" ~ "Hungary",
    CNT == "IRL"~ "Ireland",
    CNT == "MAC" ~ "Macao",
    CNT == "NLD" ~ "Netherlands",
    CNT == "NZL" ~ "New Zealand",
    CNT == "SVN" ~ "Slovenia"),
  HighestParentalEd = case_when(
    HighestParentalEd == "1" ~ "No Formal Education",
    HighestParentalEd == "2" ~ "Primary Education",
    HighestParentalEd == "3" ~ "Lower Secondary",
    HighestParentalEd == "4" ~ "Upper Secondary No Access to Tertiary",
    HighestParentalEd == "5" ~ "Upper Secondary w Access to Tertiary",
    HighestParentalEd == "6" ~ "Post Secondary non Tertiary",
    HighestParentalEd == "7" ~ "Two Years of Tertiary",
    HighestParentalEd == "8" ~ "Bachelor's or equivalent",
    HighestParentalEd == "9" ~ "Master's or equivalent",
    HighestParentalEd == "10" ~ "Doctoral or equivalent"))

#remove unnecessary columns
bcpwb_data = bcpwb_data %>% 
  select(-CNT, -HighestParentalEd_Index)

#create factor variables
factor.col = c("Country", "Gender","Grade", "Age","HighestParentalEd")
bcpwb_data[factor.col] <- lapply(bcpwb_data[factor.col], factor)

#create numeric variables
numeric.col = c("Belongingness","Curiousity","Perseverance","WellBeing","StressResistance","FamilySupport","ProblemSelfDirectedLearn")
bcpwb_data[numeric.col] <- lapply(bcpwb_data[numeric.col], as.numeric)

#inspect the dataset with dfSummary 
#I like to initially overview data imported with dfSummary function from the summarytools package before I work. 
view(dfSummary(bcpwb_data))
#Comtois, D. (2022). summarytools: Tools to quickly and neatly summarize data. R Package Version 1.0.1. https://cran.r-project.org/web/packages/summarytools/index.html

#data looks as though it has already been converted to Z-Score metrics

#Inspect data & distributions -------
##Demographic Predictors ---------
AgeGender_tbl = bcpwb_data %>% group_by(Age, Gender) %>%
  summarize(count = n(), 
            meanWellBeing = mean(WellBeing, na.rm = TRUE), 
            sdWellBeing = sd(WellBeing, na.rm = TRUE))

knitr::kable(AgeGender_tbl, summary=TRUE, rownames=TRUE)

##Age & Gender-------
bcpwb_data %>% 
  ggplot(aes(x = Age, fill = Gender)) + 
  geom_bar() + ggtitle("Figure 1: Gender & Age") + theme_bw()

prop.table(table(bcpwb_data$Age))
prop.table(table(bcpwb_data$Gender))

bcpwb_data %>% 
  ggplot(aes(x = WellBeing, y = Gender)) + 
  geom_point() + ggtitle("Figure 2: Gender & Well-Being") + theme_bw()

##Grade-------
bcpwb_data$Grade <- factor(bcpwb_data$Grade, levels=c("7","8","9","10", "11","12"))

Grade_tbl = bcpwb_data %>% group_by(Grade) %>%
  summarise(count = n(), 
            meanWellBeing = mean(WellBeing, na.rm = TRUE), 
            sdWellBeing = sd(WellBeing, na.rm = TRUE)) %>%
  arrange(desc(count))

knitr::kable(Grade_tbl, summary=TRUE, rownames=TRUE)

bcpwb_data %>% 
  ggplot(aes(x = Grade)) + 
  geom_bar(fill = "darkblue") + ggtitle("Figure 3: Grade") + theme_bw()

###Grade & Well Being--------
bcpwb_data %>% 
  ggplot(aes(x = WellBeing, y = Grade)) + 
  geom_point() + ggtitle("Figure 4: Grade & Well-Being") + theme_bw()

#Parental Education---------
#Highest level of Parental Education is based on ISCED Indices 
levels(bcpwb_data$HighestParentalEd)
prop.table(table(bcpwb_data$HighestParentalEd))

ParentalEdu_tbl = bcpwb_data %>% group_by(HighestParentalEd) %>%
  summarise(count = n(), 
            meanWellBeing = mean(WellBeing, na.rm = TRUE), 
            sdWellBeing = sd(WellBeing, na.rm = TRUE)) %>%
  arrange(desc(meanWellBeing))

knitr::kable(ParentalEdu_tbl, summary=TRUE, rownames=TRUE)

##Highest Parental Education
bcpwb_data %>% 
  ggplot(aes(x = HighestParentalEd)) + 
  geom_bar(fill = "darkblue") + ggtitle("Figure 5: Highest Parental Education") + theme_bw() + 
  theme(axis.text.x = element_text(angle = 10)) 


###Highest Parental Education & Well Being---------
bcpwb_data %>% 
  ggplot(aes(x = WellBeing, y = HighestParentalEd)) + 
  geom_point() + ggtitle("Figure 6: Highest Parental Education & Well-Being") + theme_bw()

###Country of Origin -------
levels(bcpwb_data$Country)
prop.table(table(bcpwb_data$Country))
#Spain has the most responses - use as reference group

bcpwb_data %>% 
  ggplot(aes(x = Country)) + 
  geom_bar(fill = "darkblue") + ggtitle("Figure 7: Country of Origin") + theme_bw()

Country_tbl = bcpwb_data %>% group_by(Country) %>%
  summarise(count = n(), 
            meanWellBeing = mean(WellBeing, na.rm = TRUE), 
            sdWellBeing = sd(WellBeing, na.rm = TRUE)) %>%
  arrange(desc(meanWellBeing))
#Spain has the most responses - use as reference group

knitr::kable(Country_tbl, summary=TRUE, rownames=TRUE)

##Highest Country of Origin & Well Being
bcpwb_data %>% 
  ggplot(aes(x = WellBeing, y = Country)) + 
  geom_point() + ggtitle("Figure 8: Country of Origin & Well-Being") + theme_bw()


#Psychosocial Variables ---------
###Well-Being --------
bcpwb_data %>% select(WellBeing) %>% summary()
hist(bcpwb_data$WellBeing)
#well-being looks skewed 

bcpwb_data %>% 
  ggplot(aes(x = WellBeing, color = WellBeing)) +
  geom_density(alpha = .5, fill = "purple") + ggtitle("Well Being") 

###Belongingness -------
belong_tbl = bcpwb_data %>% select(Belongingness) %>% summary()

knitr::kable(belong_tbl, summary=TRUE)

bcpwb_data %>% 
  ggplot(aes(x = Belongingness, color = Belongingness)) +
  geom_density(alpha = .5, fill = "purple") + ggtitle("Figure 9: Belongingness") 

bcpwb_data %>% 
  ggplot(aes(x = Belongingness, y = WellBeing)) + 
  geom_smooth(method = "gam") + ggtitle("Figure 10: Belongingness & Well-Being") + theme_bw()

###Curiosity 
#I realized I mispelled curiosity for my data label after making my dataset 
curiosity_tbl = bcpwb_data %>% select(Curiousity) %>% summary()

knitr::kable(curiosity_tbl, summary=TRUE)

bcpwb_data %>% 
  ggplot(aes(x = Curiousity, color = Curiousity)) +
  geom_density(alpha = .5, fill = "purple") + ggtitle("Figure 11: Curiosity") 


bcpwb_data %>% 
  ggplot(aes(x = Curiousity, y = WellBeing)) + 
  geom_smooth(method = "gam") + ggtitle("Figure 12: Curiosity & Well-Being") + theme_bw()

###Perseverance------------
Perseverance_tbl = bcpwb_data %>% select(Perseverance) %>% summary()

knitr::kable(Perseverance_tbl, summary=TRUE)

bcpwb_data %>% 
  ggplot(aes(x = Perseverance, color = Perseverance)) +
  geom_density(alpha = .5, fill = "purple") + ggtitle("Figure 13: Perseverance") 


bcpwb_data %>% 
  ggplot(aes(x = Perseverance, y = WellBeing)) + 
  geom_smooth(method = "gam") + ggtitle("Figure 14: Perseverance & Well-Being") + theme_bw()
###Problems with Self Directed Learning-----
ProblemSelfDirectedLearn_tbl = bcpwb_data %>% select(ProblemSelfDirectedLearn) %>% summary()

knitr::kable(ProblemSelfDirectedLearn_tbl, summary=TRUE)

bcpwb_data %>% 
  ggplot(aes(x = ProblemSelfDirectedLearn, color = ProblemSelfDirectedLearn)) +
  geom_density(alpha = .5, fill = "purple") + ggtitle("Figure 15: Problems w/ Self Directed Learning") 


bcpwb_data %>% 
  ggplot(aes(x = ProblemSelfDirectedLearn, y = WellBeing)) + 
  geom_smooth(method = "gam") + ggtitle("Figure 16: Problems w/ Self Directed Learning & Well-Being") + theme_bw()
###Stress Resistance -------
StressResistance_tbl = bcpwb_data %>% select(StressResistance) %>% summary()

knitr::kable(StressResistance_tbl, summary=TRUE)

bcpwb_data %>% 
  ggplot(aes(x = StressResistance, color = StressResistance)) +
  geom_density(alpha = .5, fill = "purple") + ggtitle("Figure 17: Stress Resistance") 


bcpwb_data %>% 
  ggplot(aes(x = StressResistance, y = WellBeing)) + 
  geom_smooth(method = "gam") + ggtitle("Figure 18: Stress Resistance & Well-Being") + theme_bw()
###Family Support--------
FamilySupport_tbl = bcpwb_data %>% select(FamilySupport) %>% summary()

knitr::kable(FamilySupport_tbl, summary=TRUE)

bcpwb_data %>% 
  ggplot(aes(x = FamilySupport, color = FamilySupport)) +
  geom_density(alpha = .5, fill = "purple") + ggtitle("Figure 19: FamilySupport") 

bcpwb_data %>% 
  ggplot(aes(x = FamilySupport, y = WellBeing)) + 
  geom_smooth(method = "gam") + ggtitle("Figure 20: FamilySupport & Well-Being") + theme_bw()

# refactor reference level
#all regression results are based on a comparison to Spain and Grade 10 
df1$Country <- relevel(df1$Country, ref = "Spain")
df1$Grade <- relevel(df1$Grade, ref = "10")

#Hierarchical Multiple Linear Regression ------------
#set reference group for country to the U.S. or the largest sample
#What is the relationship between the demographic variables to well being?
model1 = lm(WellBeing ~ Grade + Gender + Country + HighestParentalEd, data = bcpwb_data)
summary(model1)
plot(model1)

#What is the relationship between all psychosocial variables to well being?
model2 = lm(WellBeing ~ Belongingness + Curiousity + Perseverance + StressResistance + FamilySupport + ProblemSelfDirectedLearn, data = bcpwb_data)
summary(model2)
plot(model2)

#Final model - To what extent does grade level, gender, country, belongingness, curiosity, and perseverance predict well-being?
model3 = lm(WellBeing ~ Grade + Gender + Country + HighestParentalEd + Belongingness + Curiousity + Perseverance + FamilySupport + ProblemSelfDirectedLearn + StressResistance, data = bcpwb_data)
summary(model3)
plot(model3)

#Machine Learning - Random Forest ----------
#The data do not seem to be linear when looking at figures. Random forest may be more appropriate to analyze features and outcomes.
#Machine Learning to determine the predictors with the highest effect on well-being
set.seed(2024, sample.kind="Rounding") #
test_index <- createDataPartition(y = df1$WellBeing, times = 1, p = 0.1, list = FALSE)
train <- df1[-test_index,]
test <- df1[test_index,]

#tuning with caret package
control <- trainControl(method="cv", number=3)
#tuning to determine mtry
#this will take time to run
tune_edx <- train(WellBeing ~ .,
                  data= train, 
                  method="rf", 
                  metric="RMSE", 
                  ntree = 100, 
                  trControl=control)

print(tune_edx)


#tuning said that 16 was the optimal mtry
rf_model <- randomForest(WellBeing ~ ., 
                         data= train,  
                         Importance = TRUE, 
                         ntree = 100, mtry = 16)
print(rf_model)
plot(rf_model, log="y")

# trees lowest MSE
which.min(rf_model$mse)

# RMSE optimal random forest
sqrt(rf_model$mse[which.min(rf_model$mse)])

# Get feature importance
model_importance <- importance(rf_model)
Feature_importance <- data.frame(Feature = rownames(model_importance), Importance = model_importance[, 1])
print(Feature_importance)

# Select top N important features (e.g., top predictors for well-being)
top_n <- 5
top_features <- Feature_importance |>
  arrange(desc(Importance)) |>
  slice(1:top_n) |>
  pull(Feature)
top_features

#Create ggplot
Feature_importance %>% 
  ggplot(aes(x = reorder(Feature, -Importance), y = Importance)) + 
  geom_bar(stat = "identity", fill = "darkblue") + xlab("Features") + ggtitle("Figure 21: Feature Importance") + theme_bw() +
  theme(axis.text.x = element_text(angle = 20)) 

## Train a new random forest model on the reduced dataset------------
# Reduce the dataset to include only top important features
set.seed(2024)
train <- train |>select(WellBeing, all_of(top_features))
test <- test |> select(WellBeing, all_of(top_features))

rf_model_re <- randomForest(WellBeing ~ ., data = train, ntree = 100, mtry = 16)

# Predict and evaluate on the test set
predictions <- predict(rf_model_re, newdata = test)
test_RMSE <- sqrt(mean((predictions - test$WellBeing)^2))

# What is the RMSE of the Test?
print(paste("Test RMSE:", test_RMSE))

#There are likely more variables that need to be accounted for. I'd like to try a conditional random forest model to account for family, teacher, and school-level variance that could impact well-being in the classroom. 
#Next steps would be to analyze the item level variance from each measure with PCA to assess the dimensionality of the data prior to analyzing a structural equation model



