#*******************************************************************
#LOAD THE DATASET
#*******************************************************************

#Install any required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(DescTools)) install.packages("DescTools", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")

library(tidyverse)

# Read in the data from the Github repository
df <- read.csv("https://raw.githubusercontent.com/JeanSinger/Capstone-CYO-Predicting-Loneliness/master/AARPdata_for_capstone.csv", fileEncoding = 'UTF-8-BOM')

#-------------------------------------------------------------------
#Create and add the variable for Complex Social Integration (CSI)
#-------------------------------------------------------------------
#CSI starts with the value for DiversitySupportive and adds point for marital satisfaction,
#employment status, volunteer work, and group membership.

#Create variable with points to be added for marital: marital satisfaction
#greater than 3 gets a point.
df <- df %>% mutate(CSImarital = ifelse(Q22_marital_satn>3, 1, 0))

#Create variable with points to be added for employed: 1 or 2 gets a point
df <- df %>% mutate(CSIemployed = ifelse(employ==1 |employ==2, 1, 0))

#Create variable with points to be added for volunteer: 1 gets a point
df <- df %>% mutate(CSIvolunteer = ifelse(Q48_volunteer==1, 1, 0))

#Create variable with points to be added for groups: 2 or more (meaning 1-3 groups)gets a point
df <- df %>% mutate(CSIgroups = ifelse(Q50_groups > 1, 1, 0))

#Creat the index by adding the variables above to DiversitySupportive
df$DiversitySupportive <- as.numeric(df$DiversitySupportive)
df <- df %>% mutate(CSI = rowSums(across(c(DiversitySupportive,CSImarital, CSIemployed,
                                          CSIvolunteer, CSIgroups)),na.rm = TRUE))

#convert 0s to NAs
df$CSI[df$CSI==0] <- NA

#remove the point calculations so they don't show up as variables
df <- df %>% select (-c(CSImarital, CSIemployed, CSIvolunteer, CSIgroups))

#*******************************************************************
#EXPLORATORY ANALYSIS AND VARIABLE SELECTION
#*******************************************************************

#Examine the structure of the data
str(df)

#-------------------------------------------------------------------
#Anaysis of outcome (loneliness) variable
#-------------------------------------------------------------------

#create a histogram of the loneliness variable
hist(df$UCLA_index, main = "Distribution of Loneliness Scores",
     xlab = "UCLA Loneliness Index", ylab = "Count", col = "sky blue", border = "black")

#create a Q-Q plot of loneliness
qqnorm(df$UCLA_index)
qqline(df$UCLA_index)

#compute the mean, sd, median and mode for loneliness
mean_loneliness <- mean(df$UCLA_index)
sd_loneliness <- sd(df$UCLA_index)
median_loneliess <- median(df$UCLA_index)
library(DescTools)
mode_loneliness <- Mode(df$UCLA_index)

#place the stats in a table
descriptive_stats <- tibble(Statistic = c("Mean", "SD", "Median", "Mode"),
                            Value = c(mean_loneliness, sd_loneliness, 
                                      median_loneliess,mode_loneliness))
descriptive_stats %>% knitr::kable()

#compute the percentage of people who would be considered lonely (UCLA index >=44)
mean(df$UCLA_index >=44)

#Percentage of people classified as "lonely" by age
df %>% group_by(age_group) %>% 
  summarise(Percentage_lonely = mean(UCLA_index >= 44)) %>%
  ggplot(aes(age_group, Percentage_lonely)) + geom_bar(stat = "identity", 
                                                       col = "black", fill = "sky blue") +
  xlab("Age Group") + ylab("Percentage Lonely (UCLA Index >=44)") +
  labs(title = "Loneliness by Age") + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  scale_x_continuous(breaks = 5:8, labels = c("45 - 54", "55 - 64", "65 - 74", "75 and over"))

#-------------------------------------------------------------------
#Analysis of associations between predictor variables and loneliness: 
#correlations, ANOVAs, t-tests
#-------------------------------------------------------------------

#CORRELATIONS
#Create a correlation matrix for the variables of interest, which are
#those that are indexes or ordinal with 5 or more response options
variables <- df %>% select(-c(respondent, ethnic,
                                            Q22_marital_satn, Q38_more_less_friends,
                                            Q50_groups,
                                            Q90_tradeoffs_family, Q91_tradeoffs_intimate_convo,
                                            age_group, employ, gender,
                                            Q39_caregiver, Q48_volunteer, Q8_disability))

#Generate the correlation matrix and p values
library(Hmisc)
cor_variables <- rcorr(as.matrix(variables))
cor_variables_r <- round(cor_variables[["r"]],2) 
cor_variables_p <- round(cor_variables[["P"]],3) 

#In the r-value matrix, select the variables with correlations >.20 or <-0.2
new_variables <- as.data.frame(cor_variables_r) %>% 
  filter(UCLA_index>0.2 | UCLA_index<(-.2)) %>% 
  select(UCLA_index)

#create a column with the names of all the variables
new_variables$variable_names <- row.names(new_variables)

#remove the first row that contains UCLA_index as a variable
new_variables <- new_variables[-1, ]

#Now take a look at the p-values
#In the p-value matrix, add variable names as a column 
new_variables_p <- as.data.frame(cor_variables_p)
new_variables_p$variable_names <- row.names(new_variables_p)

#select the relevant variables from the p-value matrix
relevant_variables <- c("NeighborIndex", "DiversityDiscussImportant",
                        "DiversitySupportive", "Q30_supportive_num",
                        "income", "Q2_health_overall", 
                        "Q27_41_friend_inperson",
                        "Q27_43_friend_phone", "Q28_discuss_important_matters_num",
                        "Q77_hrs_alone", "Q89_5_internet_sentiment",
                        "Q89_7_internet_sentiment", "CSI")
new_variables_p <- new_variables_p %>% filter(variable_names %in% relevant_variables) %>%
  select(UCLA_index, variable_names)

#add three decimal places to the p-value
new_variables_p[,'UCLA_index']=format(round(new_variables_p[,'UCLA_index'],3),nsmall=3)

#create a table with the variables, correlations and p-values for all variables
# with r>0.20
correlation_table <- tibble(Variable_name = new_variables$variable_names,
                            Pearsons_r = new_variables$UCLA_index,
                            p_value = new_variables_p$UCLA_index)
#sort the table
correlation_table <- correlation_table %>% arrange(desc(Pearsons_r))

#format the table
correlation_table %>% knitr::kable()

#ANOVAs
#Below is the code for running ANOVA on 7 variables

#Convert all the variables to factor
df$Q22_marital_satn <- as.factor(df$Q22_marital_satn)
df$age_group <- as.factor(df$age_group)
df$Q38_more_less_friends <- as.factor(df$Q38_more_less_friends)
df$Q90_tradeoffs_family <- as.factor(df$Q90_tradeoffs_family)
df$Q91_tradeoffs_intimate_convo <- as.factor(df$Q91_tradeoffs_intimate_convo)
df$Q50_groups <- as.factor(df$Q50_groups)
df$ethnic <- as.factor(df$ethnic)

#ANOVA of Q22_marital_satn vs loneliness
df_marital_satn <- df %>% 
  filter(Q22_marital_satn != "NA" & UCLA_index != "NA") 
ANOVA_marital_satn_loneliness <- aov(UCLA_index ~ Q22_marital_satn, 
                                     data = df_marital_satn)
summary(ANOVA_marital_satn_loneliness)

#ANOVA of ethnicity vs loneliness
df_ethnic <- df %>% 
  filter(ethnic != "NA" & UCLA_index != "NA") 
ANOVA_ethnic <- aov(UCLA_index ~ ethnic, 
                    data = df_ethnic)
summary(ANOVA_ethnic)

#ANOVA of age group vs loneliness 
df_agegroup <- df %>% 
  filter(age_group != "NA" & UCLA_index != "NA") 
ANOVA_agegroup_loneliness <- aov(UCLA_index ~ age_group, 
                                 data = df_agegroup)
summary(ANOVA_agegroup_loneliness)

#ANOVA of Q38_more_less_friends (changes in number of friends
#over the last 5 years) vs loneliness
df_friends <- df %>% 
  filter(Q38_more_less_friends != "NA" & UCLA_index != "NA") 
ANOVA_friends_loneliness <- aov(UCLA_index ~ age_group, 
                                data = df_friends)
summary(ANOVA_friends_loneliness)

#ANOVA of Q90_tradeoffs_family (as a result of technology, spending
#less, same or more time with family) vs loneliness
df_tradeoffsfam <- df %>% 
  filter(Q90_tradeoffs_family != "NA" & UCLA_index != "NA") 
ANOVA_tradeoffsfam_loneliness <- aov(UCLA_index ~ Q90_tradeoffs_family, 
                                     data = df_tradeoffsfam)
summary(ANOVA_tradeoffsfam_loneliness)

#ANOVA of Q91_tradeoffs_intimate_convo (as a result of technology, spending
#less, same or more time in intimate conversations) vs loneliness 
df_tradeoffsint <- df %>% 
  filter(Q91_tradeoffs_intimate_convo != "NA" & UCLA_index != "NA") 
ANOVA_tradeoffsint_loneliness <- aov(UCLA_index ~ Q91_tradeoffs_intimate_convo, 
                                     data = df_tradeoffsint)
summary(ANOVA_tradeoffsint_loneliness)

#ANOVA of Q50_groups (number of groups you belong to with 
#1=0, 2=1, 3=2, 4=3 or more) vs loneliness
df_groups <- df %>% 
  filter(Q50_groups != "NA" & UCLA_index != "NA") 
ANOVA_groups_loneliness <- aov(UCLA_index ~ Q50_groups, 
                               data = df_groups)
summary(ANOVA_groups_loneliness)

#sample boxplot showing the association between loneliness 
#and change in the number of friends over the last five years.
df %>% filter(Q38_more_less_friends != "NA") %>% 
  ggplot(aes(as.factor(Q38_more_less_friends), UCLA_index)) + geom_boxplot() +
  xlab("Change in Number of Friends over Past 5 Years") + ylab("Loneliness (UCLA Index)") +
  labs(title = "Loneliness by Change in Number of Friends")+
  scale_x_discrete(breaks = c("1", "2", "3"), 
                   labels = c("Fewer", "Same", "More"))

#T-TESTS
#t-test on mean loneliness for Q48_volunteer (whether or not you have 
#in the last 12 months.)
df_volunteer <- df %>% 
  filter(Q48_volunteer != "NA" & UCLA_index != "NA") 
t.test(UCLA_index ~ Q48_volunteer, data = df_volunteer)

#t-test on mean loneliness for Q8_disabled (whether or not you are disabled)
df_disabled <- df %>% 
  filter(Q8_disability != "NA" & UCLA_index != "NA") 
t.test(UCLA_index ~ Q8_disability, data = df_disabled)

#t-test on mean loneliness for Q39_caregiver (whether or not you are
#providing unpaid care to an adult)
df_caregiver <- df %>% 
  filter(Q39_caregiver != "NA" & UCLA_index != "NA") 
t.test(UCLA_index ~ Q39_caregiver, data = df_caregiver)

#t-test on mean loneliness for gender
df_gender <- df %>% 
  filter(gender != "NA" & UCLA_index != "NA") 
t.test(UCLA_index ~ gender, data = df_gender)

#t-test on mean loneliness for whether or not you're employed
df_employed <- df %>% mutate(CSIemployed = ifelse(employ==1 |employ==2, 1, 0))%>%
  filter(CSIemployed != "NA" & UCLA_index != "NA") 
t.test(UCLA_index ~ CSIemployed, data = df_employed)

#boxplot showing higher level of loneliness when you have a disability
df %>% filter(Q8_disability != "NA") %>% 
  ggplot(aes(as.factor(Q8_disability), UCLA_index)) + geom_boxplot() +
  xlab("Do you have a disability?") + ylab("Loneliness (UCLA Index)") +
  labs(title = "Comparing Loneliness by Disability Status") +
  scale_x_discrete(breaks = c("0", "1"),
                   labels = c("No", "Yes"))

#boxplot showing no significant difference in loneliness between genders.
df %>% filter(gender != "NA") %>% 
  ggplot(aes(as.factor(gender), UCLA_index)) + geom_boxplot() +
  xlab("Gender") + ylab("Loneliness (UCLA Index)") +
  labs(title = "Comparing Loneliness by Gender") +
  scale_x_discrete(breaks = c("1", "2"),
                   labels = c("Male", "Female"))

#*******************************************************************
#MODELS AND RESULTS
#*******************************************************************

#-------------------------------------------------------------
#Create the relevant data frame with 23 predictor variables and the loneliness variable
#-------------------------------------------------------------

variables <- df %>% select(UCLA_index, Q77_hrs_alone, Q27_43_friend_phone,
                           income, Q30_supportive_num, DiversityDiscussImportant,
                           Q89_7_internet_sentiment, Q89_5_internet_sentiment,
                           Q28_discuss_important_matters_num,Q2_health_overall,
                           Q27_41_friend_inperson, NeighborIndex, DiversitySupportive,
                           CSI, 
                           ethnic, Q22_marital_satn, Q38_more_less_friends, 
                           Q50_groups, Q90_tradeoffs_family,
                           Q91_tradeoffs_intimate_convo, age_group,
                           Q48_volunteer, Q8_disability,
                           Q39_caregiver)
                           
#convert binary and nominal variables to factor class 
variables$ethnic <- as.factor(variables$ethnic)
variables$Q22_marital_satn <- as.factor(variables$Q22_marital_satn)
variables$Q38_more_less_friends <- as.factor(variables$Q38_more_less_friends)
variables$Q50_groups <- as.factor(variables$Q50_groups)
variables$Q90_tradeoffs_family <- as.factor(variables$Q90_tradeoffs_family)
variables$Q91_tradeoffs_intimate_convo <- as.factor(variables$Q91_tradeoffs_intimate_convo)
variables$age_group <- as.factor(variables$age_group)
variables$Q48_volunteer <- as.factor(variables$Q48_volunteer)
variables$Q8_disability <- as.factor(variables$Q8_disability)
variables$Q39_caregiver <- as.factor(variables$Q39_caregiver)

#convert to a data frame
variables <- as.data.frame(variables)

#Make sure the outcome variable is in the last column
variables <- variables %>% select(-UCLA_index, UCLA_index)

#-------------------------------------------------------------
#Create the training and test sets
#-------------------------------------------------------------
#Split the data 80/20 into training and test sets.
library(caret)
set.seed(2, sample.kind="Rounding")
train_index <- createDataPartition(variables$UCLA_index, times = 1, p=.8, list = FALSE)
train <- variables[train_index, ]
test <- variables[-train_index, ]

#-------------------------------------------------------------
#Build the linear regression model using the training set
#-------------------------------------------------------------
LM <- lm(UCLA_index ~ ., data = train)

#print out the summary of the model
LMsummary <- summary(LM)
LMsummary

#Create a function to compute RMSE
RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2, na.rm = TRUE))
}

#create predictions on the test set
predict_LM <- predict(LM, newdata = test)

#compute RMSE for the linear regression model
RMSE_LM <- RMSE(test$UCLA_index, predict_LM)
RMSE_LM

#Store the linear regression results in a table and compare
#to the AARP performance
model_results <- tibble(Model = c("AARP", "Linear Regression"), 
                        Adjusted_R2 = c(.213, LMsummary$adj.r.squared),
                        RMSE = c(NA, RMSE_LM))
model_results %>% knitr::kable()

#-------------------------------------------------------------
#Fit and tune the XGBoost model using the training set
#-------------------------------------------------------------

#load the xgboost library
library(xgboost)

#Create separate objects for the predictor and outcome variables in the training set
train_x <- data.matrix(train[ ,-ncol(train)])
train_y <- train[ , ncol(train)]

#Create separate objects for the predictor and response variables in the test set
test_x <- data.matrix(test[ ,-ncol(test)])
test_y <- test[ , ncol(test)]

#Define the final training and testing sets. 
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

#Fit and tune the model. First define the watchlist
watchlist = list(train=xgb_train, test=xgb_test)

#Fit the XGBoost model and display the training and testing data at each round
set.seed(2, sample.kind="Rounding")
model_XG = xgb.train(data = xgb_train, max.depth = 3, watchlist=watchlist, nrounds = 70)

#Find the lowest RMSE and insert in the code for nrounds. 
set.seed(2, sample.kind="Rounding")
final_model = xgboost(data = xgb_train, max.depth = 3, nrounds = 51, verbose = 0)

#Make predictions on the test set and compute RMSE
predict_XG <- predict(final_model, newdata = xgb_test)
RMSE_XG <- caret::RMSE(test_y, predict_XG)

#Add the XGBoost model to the table
model_results <- bind_rows(model_results, 
                           tibble(Model = "XGBoost", 
                                  Adjusted_R2 = NA,
                                  RMSE = RMSE_XG))
model_results %>% knitr::kable()

#-------------------------------------------------------------
#Fit and tune the PCR model using the training set
#-------------------------------------------------------------

#Fit the PCR model
library(pls)
model_PCR <- pcr(UCLA_index ~., data = train, scale=TRUE, validation="CV")

#View summary of the model fitting and select number of components
summary(model_PCR)

#view plots
validationplot(model_PCR)

#use model to make predictions on a test set
predict_PCR <- predict(model_PCR, test, ncomp=33)

#compute RMSE for model PCR model
RMSE_PCR <- RMSE(test$UCLA_index, predict_PCR)
RMSE_PCR

#Add the PCR model to the table
model_results <- bind_rows(model_results, 
                           tibble(Model = "PCR", 
                                  Adjusted_R2 = NA,
                                  RMSE = RMSE_PCR))
model_results %>% knitr::kable()

#-------------------------------------------------------------
#Build the ensemble model
#-------------------------------------------------------------

#Create a dataframe with the predictions from the LM, XGBoost and PCR models
ensemble_preds <- data.frame(LM_preds = predict_LM, 
                             XG_preds = predict_XG,
                             PCR_preds = as.vector(predict_PCR),
                             Avg = as.vector((predict_LM + predict_XG +
                                                predict_PCR)/3))

#compute RMSE for ensemble
RMSE_ensemble <- RMSE(test$UCLA_index, ensemble_preds$Avg)
RMSE_ensemble

#Add the ensemble to the table
model_results <- bind_rows(model_results, 
                           tibble(Model = "Ensemble", 
                                  Adjusted_R2 = NA,
                                  RMSE = RMSE_ensemble))
model_results %>% knitr::kable()

#-------------------------------------------------------------
#Identify variable importance for linear regression and XGBoost models
#-------------------------------------------------------------

#Extract the absolute value of the t-values from the linear regression model
#summary and convert to a data frame
LM_tvalues <- as.data.frame(LMsummary$coefficients[-1 ,3])
LM_tvalues <- data.frame(Variable = rownames(LM_tvalues), LM_tvalues)
rownames(LM_tvalues) <- NULL
names(LM_tvalues)[2] <- "t_values"

#Take the absolute value of the t-values and sort highest to lowest
LM_tvalues$t_values <- abs(LM_tvalues$t_values)
LM_tvalues <- arrange(LM_tvalues, desc(t_values))
#Extract the top five predictors fromthe linear regression model
top_five_LM <- head(LM_tvalues, 5)
top_five_LM

# Generate the feature importance matrix for the XGBoost model
XGimportance_matrix = xgb.importance(colnames(xgb_train), model = final_model)
XGimportance_matrix <- XGimportance_matrix[ , 1:2]

#The matrix is already sorted in descending order
#Extract the top five predictors from the XGBoost model
top_five_XG <- head(XGimportance_matrix, 5)
top_five_XG

