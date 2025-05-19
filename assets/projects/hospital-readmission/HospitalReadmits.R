library(tidyverse)
library(rpart)
library(MLmetrics)
library(randomForest)
library(caret)
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(C50)
library(earth)
library(neuralnet)
library(pROC)
library(mice)

collapse_diagnosis_column <- function(diagnosis_column) {
  newDiag <- diagnosis_column
  
  newDiag[newDiag == "V57"] <- 57 # Rehab
  my_entries <- c('E909', 'V25', 'V26', 'V43', 'V45', 'V51', 'V53', 'V54', 'V55', 'V56', 'V58', 'V63', 'V66', 'V67', 'V71')
  for (i in my_entries) {
    newDiag[newDiag == i] <- 930
  }
  newDiag <- as.numeric(newDiag)
  newDiag[(newDiag >= 1) & (newDiag < 140)] <- 1 # infectious
  newDiag[(newDiag >= 140) & (newDiag < 240)] <- 160 # Neoplasms
  newDiag[(newDiag>= 240) & (newDiag < 247)] <- 240 # thyroid
  newDiag[(newDiag >= 249) & (newDiag < 251)] <- 250 # Diabetes
  newDiag[(newDiag >= 251) & (newDiag < 280)] <- 270 # Endocrine
  newDiag[(newDiag >= 280) & (newDiag < 290)] <- 285 # Blood
  newDiag[(newDiag >= 290) & (newDiag < 320)] <- 300 # Mental
  newDiag[(newDiag >= 320) & (newDiag < 390)] <- 350 # Nervous system
  newDiag[(newDiag >= 390) & (newDiag < 460)] <- 428 # Heart disease
  newDiag[(newDiag >= 460) & (newDiag < 520)] <- 480 # Respiration
  newDiag[(newDiag >= 520) & (newDiag < 580)] <- 550 # Digestion
  newDiag[(newDiag >= 580) & (newDiag < 630)] <- 600 # Genito-urinary
  newDiag[(newDiag >= 630) & (newDiag < 680)] <- 650 # childbirth
  newDiag[(newDiag >= 680) & (newDiag < 710)] <- 695 # Skin
  newDiag[(newDiag >= 710) & (newDiag < 740)] <- 730 # Musculo
  newDiag[(newDiag >= 740) & (newDiag < 780)] <- 760 # genetic / birth
  newDiag[(newDiag >= 780) & (newDiag <= 999)] <- 930 # Misc. / injury
  
  
  return(as.factor(newDiag))
}
factor_Collapse <- function(x) {
  x <- fct_lump_n(x, n=6)
  x <- fct_explicit_na(x, na_level = "Other")
  return(x)
}
#---------------calculation function for missing values and percentage------------------------
calculate_missing_values <- function(data) {
  missing_values <- sapply(data, function(x) sum(is.na(x)))
  percentage_missing <- sapply(data, function(x) sum(is.na(x)) / length(x) * 100)
  
  missing_data_summary <- data.frame(
    Column = names(data),
    Missing_Values = missing_values,
    Percentage_Missing = percentage_missing
  )
  
  return(missing_data_summary)
}
get_mode <- function(data, category_column) {
  mode_value <- data %>%
    count({{category_column}}) %>%
    slice(which.max(n)) %>%
    pull({{category_column}})
  
  return(mode_value)
}

# ----------------Data Reading--------------------------------------
Train1 <- read.csv("~/IDA/HW07/hm7-Train-2023.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))
Train <- Train1

colnames_vector <- c("admission_type", "discharge_disposition", "admission_source")
Train[colnames_vector] <- lapply(Train[colnames_vector], as.factor)
Train$readmitted <- as.logical(Train$readmitted)


Test1 <- read.csv("~/IDA/HW07/hm7-Test-2023.csv", stringsAsFactors = FALSE, na.strings = c("", "NA")) 
Test1 <- Test1[1:38568,]
Test <- Test1
Test[colnames_vector] <- lapply(Test[colnames_vector], as.factor)


#------------------------------Data Exploration (Data Quality)----------------------------
# summary(Train)
# 
# colSums(is.na(Train))
# 
# unique(Train,race)
# 
# correlation_matrix <- cor(Train)
# 
# data_matrix <- as.matrix(Train)
# heatmap(data_matrix)

##Converting categorical columns which are integers to factors

#for admission_type
Train$admission_type<-as.factor(Train$admission_type)

#for readmitted
Train$readmitted<-as.factor(Train$readmitted)
#For discharge_disposition
Train$discharge_disposition<-as.factor(Train$discharge_disposition)

#For admission_source
Train$admission_source<-as.factor(Train$admission_source)

##Creating a tibble named trainNumeric using dplyr package, which contains all numeric variables from train data
TrainNumeric <- Train %>%
  select_if(is.numeric)

##Creating a tibble named trainFactor using dplyr package, which contains all non -numeric variables from train data
TrainFactor <- Train %>%
  select_if(is.factor)

#DATA QUALITY REPORT FOR NUMERIC DATA

#Creating two new functions, Q1 and Q3, respectively.
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

#Creating a new function, 'myNumericSummary' that will apply several summary statistics to data which includes the use of Q1 and Q3 functions respectively
myNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}
#Using summarize command together with the new 'myNumericSummary' function to apply the new function to all variables in the 'trainNumeric' data set
numericSummary <- TrainNumeric %>%
  summarize(across(.cols = everything(), .fns = myNumericSummary))

numericSummary

# Using 'cbind' to bind the required labels to the 'numericSummary' 
numericSummary <-cbind(
  stat=c("n","unique","missing","mean","min","Q1","median","Q3","max","sd"),
  numericSummary)

glimpse(numericSummary)

# Pivoting the data and adding computed values for percent missing and percent unique fields along with using kable() function to produce first part of Data Quality Report,'numericsummaryFinal'
numericSummaryFinal <- numericSummary %>%
  pivot_longer("patientID":"number_diagnoses", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*missing/n, unique_pct = 100*unique/n) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

library(knitr)
options(digits=3)
options(scipen=99)
numericSummaryFinal %>% kable()

##DATA QUALITY REPORT FOR NON NUMERIC DATA
library(dplyr)
library(tidyverse)

getmodes <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (names(m1)) #1st mode
  }
  else if (type==2) {
    return (names(which.max(tbl[-m1]))) #2nd mode
  }
  else if (type==-1) {
    return (names(which.min(tbl))) #least common mode
  }
  else {
    stop("Invalid type selected")
  }
}

getmodesCnt <- function(v,type=1) {
  tbl <- table(v)
  m1<-which.max(tbl)
  if (type==1) {
    return (max(tbl)) #1st mode freq
  }
  else if (type==2) {
    return (max(tbl[-m1])) #2nd mode freq
  }
  else if (type==-1) {
    return (min(tbl)) #least common freq
  }
  else {
    stop("Invalid type selected")
  }
}

mynonNumericSummary <- function(x){
  c(length(x), n_distinct(x), sum(is.na(x)),as.numeric(getmodes(x,type=1))/as.numeric(getmodes(x,type=2)),
    getmodes(x,type=1),getmodesCnt(x,type=1),getmodes(x,type=2),getmodesCnt(x,type=2),
    getmodes(x,type=-1),getmodesCnt(x,type=-1))
}



nonnumericSummary <- TrainFactor %>%
  summarize(across(.cols = c(race,gender,age,admission_type,discharge_disposition,admission_source,payer_code,medical_specialty,diagnosis,max_glu_serum,A1Cresult,metformin,repaglinide,nateglinide,chlorpropamide,glimepiride,acetohexamide,glipizide,glyburide,tolbutamide,pioglitazone,rosiglitazone,acarbose,miglitol,troglitazone,tolazamide,insulin,glyburide.metformin,glipizide.metformin,metformin.rosiglitazone,metformin.pioglitazone,diabetesMed,readmitted), .fns = mynonNumericSummary))

nonnumericSummary1 <- TrainFactor %>%
  summarize(across(.cols = c(examide,citoglipton,glimepiride.pioglitazone), .fns = mynonNumericSummary))


nonnumericSummary <-cbind(
  stat=c("n","unique","missing","freqRatio","1st mode","1st mode freq","2nd mode","2nd mode freq","least common","least common freq"),
  nonnumericSummary)

nonnumericSummary1 <-cbind(
  stat=c("n","unique","missing","1st mode","1st mode freq","2nd mode" ,"least common","least common freq"),
  nonnumericSummary1)

glimpse(nonnumericSummary)

nonnumericSummaryFinal <- nonnumericSummary %>%
  pivot_longer("race":"readmitted", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*as.numeric(missing)/as.numeric(n), unique_pct = 100*as.numeric(unique)/as.numeric(n)) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

nonnumericSummaryFinal1 <- nonnumericSummary1 %>%
  pivot_longer("examide":"glimepiride.pioglitazone", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(missing_pct = 100*as.numeric(missing)/as.numeric(n), unique_pct = 100*as.numeric(unique)/as.numeric(n)) %>%
  select(variable, n, missing, missing_pct, unique, unique_pct, everything())

library(knitr)
options(digits=3)
options(scipen=99)
nonnumericSummaryFinal %>% kable()
nonnumericSummaryFinal1 %>% kable()



Train<- Train%>%dplyr::select(-c(payer_code,indicator_2_level,))
Test<- Test%>%dplyr::select(-c(payer_code,indicator_2_level))

# Train imputation
calculate_missing_values(Train)
Train$race[is.na(Train$race)] <- get_mode(Train, race)
Train$gender[is.na(Train$gender)] <- get_mode(Train, gender)
Train$age[is.na(Train$age)] <- get_mode(Train, age)
Train$medical_specialty[is.na(Train$medical_specialty)] <- 'Other'
Train$time_in_hospital[is.na(Train$time_in_hospital)] <- mean(Train$time_in_hospital, na.rm = TRUE)
Train$indicator_level[is.na(Train$indicator_level)] <- mean(Train$indicator_level, na.rm = TRUE)
Train$num_lab_procedures[is.na(Train$num_lab_procedures)] <- mean(Train$num_lab_procedures, na.rm = TRUE)
Train$diagnosis<-collapse_diagnosis_column(Train$diagnosis)
Train$diagnosis[is.na(Train$diagnosis)] <-get_mode(Train, diagnosis)


#Test Imputation
calculate_missing_values(Test)
Test$race[is.na(Test$race)] <- get_mode(Test, race)
Test$gender[is.na(Test$gender)] <- get_mode(Test, gender)
Test$age[is.na(Test$age)] <- get_mode(Test, age)
Test$medical_specialty[is.na(Test$medical_specialty)] <- 'Other'
Test$time_in_hospital[is.na(Test$time_in_hospital)] <- mean(Test$time_in_hospital, na.rm = TRUE)
Test$indicator_level[is.na(Test$indicator_level)] <- mean(Test$indicator_level, na.rm = TRUE)
Test$num_lab_procedures[is.na(Test$num_lab_procedures)] <- mean(Test$num_lab_procedures, na.rm = TRUE)
Test$diagnosis<-collapse_diagnosis_column(Test$diagnosis)
Test$diagnosis[is.na(Test$diagnosis)] <-get_mode(Test, diagnosis)

# Columns to remove as it has the same values of readmission
columns_to_remove <- c("examide", "citoglipton", "glimepiride.pioglitazone")
Train <- Train %>% select(-one_of(columns_to_remove))
Test <- Test %>% select(-one_of(columns_to_remove))

Train$gender[Train$gender == "Unknown/Invalid"] <- "Female"

Test$gender[Test$gender == "Unknown/Invalid"] <- "Female"

# Factor collapsion
PreprocessTrain <- Train %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), factor_Collapse))

PreprocessTest <- Test %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.factor), factor_Collapse))


PreprocessTrain <- PreprocessTrain %>%
  select(-nearZeroVar(Traincollapsed))
factor(Traincollapsed$gender)

PreprocessTrain <- PreprocessTrain %>%
  mutate(readmitted=ifelse(readmitted==TRUE, "Yes", "No"))


#--------------------Modelling------------------------------------------------
## Model 1:  Neural Network
#since neuralnet works with only numeric values
PreprocessTrain$admission_type <- as.numeric(PreprocessTrain$admission_type)
PreprocessTrain$diagnosis <- as.numeric(PreprocessTrain$diagnosis)

NN<-neuralnet(readmitted~number_diagnoses+diagnosis+admission_type,data = PreprocessTrain, hidden = 3,linear.output = FALSE)
PreprocessTest$admission_type<-as.numeric(PreprocessTest$admission_type)
PreprocessTest$diagnosis<-as.numeric(PreprocessTest$diagnosis)
Predict<-compute(NN,PreprocessTest)
Predic_result<-Predict$net.result
Predic_result


confusionMatrix(as.factor(Predic_result), as.factor(PreprocessTrain$readmitted), mode="everything")


matrix_con = confusion.matrix(obs,pred,threshold=0.5)


## Model 2: XGBoost
trControl <- trainControl(method = "repeatedcv", number = 6, repeats= 4)

xgb_grid <- expand.grid(nrounds = 500,
                        max_depth = 5,
                        eta = 0.05,
                        gamma = 0.01,
                        colsample_bytree = 0.5,
                        min_child_weight = 0,
                        subsample = 0.75)


xgb <- train(as.factor(readmitted) ~.-patientID, 
                 data = PreprocessTrain, method = "xgbTree", trControl=trControl,verbose = F, verbosity = 0,
                 tuneGrid=xgb_grid)
xgb_preds_train <- predict(xgb, newdata = PreprocessTrain)
numPredictions <- ifelse(xgb_preds_train == "Yes", 1, 0)

LogLoss(numPredictions, ifelse(PreprocessTrain$readmitted=="Yes", 1, 0))

confusionMatrix(xgb_preds_train,as.factor(PreprocessTrain$readmitted),mode = "everything")

xgb_preds_test <- predict(xgb, newdata = PreprocessTest, type = "prob")
xgb_submissions <- data.frame(patientID=PreprocessTest$patientID, 
                         predReadmit=xgb_preds_test$Yes)

write.csv(xgb_submissions, "xgb.csv", row.names = F)

# Model 3:  C50 Tree
readmit <- as.factor(PreprocessTrain$readmitted)
c50Tree <- C5.0.default(PreprocessTrain, readmit, trials = 100)
c50Tree
summary(c50Tree)
plot(c50Tree)
# assess performance (overall) on training data
c50pred <- predict(c50Tree, newdata = PreprocessTrain, type = "prob")
c50result <- LogLoss(c50pred[,2], PreprocessTrain$readmitted)
c50result

perf<- performance(c50pred)

# also compute kappa
c50pred <- c50pred[,2]
c50pred[c50pred < 0.5] <- 0
c50pred[c50pred >= 0.5] <- 1
cohen.kappa(x = cbind(c50pred, PreprocessTrain$readmitted))
confusionMatrix(as.factor(c50pred), as.factor(PreprocessTrain$readmitted), positive="1", mode="everything")
c50pred <- c50pred[,2]
r <- multiclass.roc(PreprocessTrain$readmitted, c50pred, percent = TRUE)
roc<-r[['rocs']]
r1<-roc[[1]]
plot.roc(r1,
         print.auc=TRUE,
         auc.polygon=TRUE,
         grid=c(0.1, 0.2),
         grid.col=c("green", "red"),
         max.auc.polygon=TRUE,
         auc.polygon.col="lightblue",
         print.thres=TRUE,
         main= 'ROC Curve')

# Tree for data insights
inTree <- C5.0.default(PreprocessTrain[,-21], noMeds$readmitted, trials = 1)
summary(inTree)
levels(PreprocessTrain$readmitted)

# Model 4: Logistic Regression
PreprocessTrain$readmitted <- ifelse(PreprocessTrain$readmitted == "Yes", 1, 0)

logReg1 <- glm(readmitted ~ ., data = PreprocessTrain, family = "binomial")
summary(logReg1)

# test efficacy of model on training data
logPred <- predict(logReg1, type = "response")
log_result <- LogLoss(logPred, PreprocessTrain$readmitted)

# Compute kappa
logPred[logPred >= 0.5] <- 1
logPred[logPred < 0.5] <- 0
cohen.kappa(x = cbind(logPred, PreprocessTrain$readmitted))

confusionMatrix(as.factor(logPred), as.factor(PreprocessTrain$readmitted), positive="1", mode="everything")


# Model 5: MARS for Classification
# Set tuning parameters
myControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
myGrid <- expand.grid(
  degree = 1:2,
  nprune = seq(2, 30, 2)
)

# Change "readmitted" to factor ensure classification model
PreprocessTrain$readmitted <- as.factor(PreprocessTrain$readmitted)

marsClass <- train(readmitted ~ ., 
                   data = PreprocessTrain,
                   method = "earth",
                   trControl = myControl,
                   tuneGrid = myGrid,
                   glm=list(family='binomial'))

# Check how well the model did on test data
marspred <- predict(marsClass, newdata = PreprocessTrain, type = "prob")
marsout <- LogLoss(marspred[,2],PreprocessTrain$readmitted)
marspred <- marspred[,2]
marspred[marspred >= 0.5] <- 1
marspred[marspred < 0.5] <- 0
cohen.kappa(x = cbind(marspred, PreprocessTrain$readmitted))

# What's the best output?
marsClass$bestTune

