#Initial Data Report
#installing necessary packages
library(mice)
library(party)
#library(MASS)
library(VIM)
library(pls)
library(glmnet)
library(caret)
library(earth)
library(car)
library(partykit)
library(knitr)
library(rpart)
library("Ckmeans.1d.dp")
library(xgboost)
library(nnet)
library(corrplot)

library(doParallel)
registerDoParallel(cores=8)
#reading Data
SmokingData <- read.csv("C:/Users/srihari/Downloads/SmD/smoking_driking_dataset_Ver01.csv")
#SmokingData <- read.csv("/Users/srihari/Downloads/smoking_driking_dataset_Ver01.csv")
View(SmokingData)

#Splitting into Numeric and non-numeric Dataframes
SmokingNumeric <- SmokingData %>%
  select_if(is.numeric)
SmokingNumeric <- SmokingData[, sapply(SmokingData, function(x) is.numeric(x) && length(unique(x)) > 8)]
#view(SmokingNumeric)

#we'll consider numeric variables with less than 8 levels as factor Variables
SmokingDiscrete <- SmokingData[, sapply(SmokingData, function(x) length(unique(x)) < 8)]
#view(SmokingDiscrete)

#Function to find 1st and 3rd Quartile
Q1<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[2]
}
Q3<-function(x,na.rm=TRUE) {
  quantile(x,na.rm=na.rm)[4]
}

#Function to find different summary parameters for the numeric table
SmokingNumericSummary <- function(x){
  c(length(x), n_distinct(x),((n_distinct(x)/length(x))*100), sum(is.na(x)),((sum(is.na(x))/length(x))*100), mean(x, na.rm=TRUE),
    min(x,na.rm=TRUE), Q1(x,na.rm=TRUE), median(x,na.rm=TRUE), Q3(x,na.rm=TRUE),
    max(x,na.rm=TRUE), sd(x,na.rm=TRUE))
}

#Using Summarize to apply the function to entire table
SmokingNumericTableSummary <- SmokingNumeric %>%
  summarize(across(everything(), SmokingNumericSummary))

view(SmokingNumericTableSummary)


#adding column names to summary table
SmokingNumericTableSummary <-cbind(
  stat=c("n","unique","Unique_percentage","missing","missing_Percentage", "mean","min","Q1","median","Q3","max","sd"),
  SmokingNumericTableSummary)
view(SmokingNumericTableSummary)


#applying Kable to create final summary report
SmokingNumericSummaryFinal <- SmokingNumericTableSummary %>%
  pivot_longer("age":"gamma_GTP", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)%>% 
  #mutate(missing_pct = 100*missing/n,
  #unique_pct = 100*unique/n) %>%
  select(variable, n, missing,  unique, everything())
view(SmokingNumericSummaryFinal)

#library(knitr)
options(digits=3)
options(scipen=99)

SmokingNumericSummaryFinal %>% kable()

#Displaying Final Summary report
view(SmokingNumericSummaryFinal)

#for Discrete Data
#function to get modes
getmodes <- function(v,type=1) {
  if(sum(is.na(v))==length(v)){
    return(NA)
  }
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

#function to get modes count
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

#Function to find different summary parameters for the Discrete table
TrainDiscreteSummary <- function(x){
  c(length(x), n_distinct(x),(n_distinct(x)*100/length(x)), sum(is.na(x)),(sum(is.na(x))*100/length(x)),  getmodes(x, type=1), getmodesCnt(x, type =1),
     getmodes(x, type= -1), getmodesCnt(x, type = -1), (getmodesCnt(x, type =1)*100/getmodesCnt(x, type = -1)))
}

##Using Summarize to apply the function to entire table
result1 <- lapply(SmokingDiscrete, TrainDiscreteSummary)
result_matrix <- do.call(cbind, result1)

# Convert the matrix into a dataframe
SmokingDiscreteTableSummary <- as.data.frame(result_matrix)

#adding column names to summary table
SmokingDiscreteTableSummary <-cbind(
  stat=c("n","unique","Unique_percentage","missing","missing_percentage","1st mode", "first_mode_freq", 
         "least common", "least common freq","Freq_Ratio"),
  SmokingDiscreteTableSummary)
view(SmokingDiscreteTableSummary)

#applying Kable to create final summary report
DiscreteFactorSummaryFinal <- SmokingDiscreteTableSummary %>%
  pivot_longer("sex":"DRK_YN", names_to = "variable", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)
  

#Displaying Final Summary Report
view(DiscreteFactorSummaryFinal)

#displaying Histogram of waistline column to explain the Data problem
hist(SmokingData$waistline)
table(SmokingData$waistline)

#displaying Histogram of sight_left and sight_right to explain the Data problem
par(mfrow = c(1, 2))
hist(SmokingData$sight_left)
hist(SmokingData$sight_right)

#visualisations
ggplot(data = SmokingData) + 
  geom_boxplot(aes(x = DRK_YN, y = age, fill = sex))  + 
  labs(x = "Drinker Yes or No", y = "Age", title = "Comparison of Male/Female Drinkers")                            
#From the plot we can say that there are more female drinkers around age 20-30 when compared to male. Whereas male drinkers are falling 
#in more among 30-50 age group. So, we can conclude that in the average age life cycle there are more male drinkers 
#whereas there are more young female drinkers. 

ggplot(data = SmokingData, mapping=aes(x= sex, group = LDL_chole, fill = LDL_chole)) +
  geom_density()+
  facet_wrap(~DRK_YN)
# From the plot we can observe that the male drinkers are having more bad cholesterol. So, we can 
#conclude that Drinking is affecting the individual health.



#cor_matrix <- cor(SmokingData)

#library(GGally)

# Calculate correlation matrix
#cor_matrix <- cor(SmokingData[, c("SBP", "DBP", "BLDS", "tot_chole", "HDL_chole", "triglyceride")])

# Bubble matrix plot
#ggcorrplot(cor_matrix, method = "circle", # Use 'circle' method for bubble plot
     #      title = "Bubble Matrix Plot of Physiological Measurements")

# Faceted violin plot
ggplot(data=SmokingData, aes(x = factor(SMK_stat_type_cd), y = weight, fill = DRK_YN)) +
  geom_violin() +
  labs(title = "Faceted Violin Plot of Weight by Smoking and Drinking Status",
       x = "Smoking Status", y = "Weight") +
  facet_wrap(~DRK_YN) +
  theme_minimal()

SmokingData$BMI <- SmokingData$weight / ((SmokingData$height / 100) ^ 2)

# Bubble chart
ggplot(data=SmokingData, aes(x = weight, y = height, size = BMI, color = factor(SMK_stat_type_cd))) +
  geom_point(alpha = 0.7) +
  labs(title = "Bubble Chart of Weight, Height, and BMI with Smoking Status",
       x = "Weight", y = "Height", size = "BMI", color = "Smoking Status") +
  scale_color_manual(values = c("blue", "green", "red", "lightyellow")) +
  theme_minimal()
##DATA PREPROCESSING

#Values to be treated as missing
SmokingData$waistline[SmokingData$waistline == 999] <- NA
SmokingData$HDL_chole[SmokingData$HDL_chole == 8110] <- NA
SmokingData$LDL_chole[SmokingData$LDL_chole == 5119] <- NA
SmokingData$SGOT_AST[SmokingData$SGOT_AST == 7000] <- NA
SmokingData$SGOT_AST[SmokingData$SGOT_AST == 9999] <- NA
SmokingData$SGOT_ALT[SmokingData$SGOT_ALT == 7210] <- NA

# Specifying the variables to impute
variables_to_impute <- c("HDL_chole", "waistline", "LDL_chole", "triglyceride", "SGOT_AST","SGOT_ALT")

# Creating an imputation model
imputation_model <- mice(SmokingData[, variables_to_impute])

# doing the imputation
imputed_data <- complete(imputation_model)

# Replacing the imputed values in the original data
SmokingData[, variables_to_impute] <- imputed_data[, variables_to_impute]



# Set a threshold value
threshold <- 4

# Replace values greater than the threshold with the threshold value for sight_left
SmokingData$sight_left[SmokingData$sight_left > threshold] <- threshold

# Repeat the process for sight_right
SmokingData$sight_right[SmokingData$sight_right > threshold] <- threshold

# Check unique values in sight_left and sight_right after replacement
unique_values_left <- unique(SmokingData$sight_left)
unique_values_right <- unique(SmokingData$sight_right)

print("Unique values in sight_left after replacement:")
print(unique_values_left)

print("Unique values in sight_right after replacement:")
print(unique_values_right)

# Plot histograms after handling outlier values
par(mfrow = c(1, 2))

hist(SmokingData$sight_left, main = "sight_left", xlab = "Value")
hist(SmokingData$sight_right, main = "sight_right", xlab = "Value")

#Splitting the Data set into Train and Test
total_rows <- nrow(SmokingData)

# Specify the number of rows for the training set
num_train_rows <- 70000

# Set seed for reproducibility
set.seed(123)

# Generate random indices for the training set
train_indices <- sample(1:total_rows, num_train_rows)

# Create the training set
train_data <- SmokingData[train_indices, ]

# Create the test set by excluding the training set indices
test_data <- SmokingData[-train_indices, ]

# Print the dimensions of the training and test sets
cat("Dimensions of Train Data:", dim(train_data), "\n")
cat("Dimensions of Test Data:", dim(test_data), "\n")

train_data_smk <- train_data[, !colnames(train_data) %in% 'DRK_YN']

# Remove 'DRK_YN' column from test_data
test_data_smk <- test_data[, !colnames(test_data) %in% 'DRK_YN']

train_data_drk<- train_data[, !colnames(train_data) %in% 'SMK_stat_type_cd']

test_data_drk <- test_data[, !colnames(test_data) %in% 'SMK_stat_type_cd']

# Specify the target variable
target_variable <- "SMK_stat_type_cd"

train_data[[target_variable]] <- as.factor(train_data[[target_variable]])
test_data[[target_variable]] <- as.factor(test_data[[target_variable]])

par(mfrow = c(1, 1))
hist(train_data_smk$waistline)


#################Feature Selection
# Assuming your dataset is named 'data', replace it with your actual dataset name
# Assuming 'SMK_stat_type_cd' is the target variable, replace it with your actual target variable
target_variable <- "SMK_stat_type_cd"

# Create a data frame with only predictor variables
predictors <- subset(SmokingData, select = -which(names(data) == target_variable))

# Define a control function for caret's feature selection
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 10)

# Perform recursive feature elimination (RFE) using Random Forest
result <- rfe(predictors, SmokingData[[target_variable]], sizes = c(1:ncol(predictors)),
              rfeControl = ctrl)

#########Clustering###########
Clustertable <- train_data_smk %>%
  select_if(is.numeric) %>%
  na.omit()
set.seed(123)
Clustertable$triglyceride
k <- 2  # Change the number of clusters as needed
cluster_assignments <- kmeans(Clustertable, centers = k)$cluster

clustered_data <- cbind(Clustertable, Cluster = cluster_assignments)

clustered_data <- cbind(train_data_smk, Cluster = cluster_assignments)
library(factoextra)
#Create a scatter plot with different colors for each cluster
ggplot(clustered_data, aes(x = triglyceride, y = hemoglobin, color = factor(Cluster))) +
 geom_point() +
labs(title = "Cluster Plot", x = "Variable1", y = "Variable2") +
  theme_minimal()



cbind(dt, pcomps, cluster=KM_clust$cluster) %>%
  as_tibble() %>%
  ggplot() +
  geom_point(mapping=aes(x=PC1, y=PC2, color=factor(cluster))) +
  ggtitle("K-means Clustering") +
  guides(color = g, size = g, shape = g)

set.seed(456)
train_indices <- createDataPartition(clustered_data$SMK_stat_type_cd, p = 0.8, list = FALSE)
Clustered_train_data <- clustered_data[train_indices, ]
Clustered_test_data <- clustered_data[-train_indices, ]

classification_model <- train(
  SMK_stat_type_cd ~ .,
  data = Clustered_train_data,
  method = "rf",
  trcontrol=train_control
)
train_control = trainControl(method = "cv", number = 5, search = "grid")

##########
# Fit k-means model
kmeans_model2 <- kmeans(Clustertable, centers = 2, nstart = 10)

# Visualize clusters
fviz_cluster(kmeans_model2, data = Clustertable)

# Function to plot the elbow plot
wssplot <- function(data, nc = 15) {
  wss <- numeric(nc)
  pctExp <- numeric(nc)
  
  for (k in 1:nc) {
    kclus <- kmeans(data, centers = k)
    wss[k] <- kclus$tot.withinss
    pctExp[k] <- 1 - wss[k] / kclus$totss
  }
  
  par(mfrow = c(1, 2))
  
  # Plot the Within groups sum of squares
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
  
  # Plot the Pct Explained
  plot(1:nc, pctExp, type = "b", xlab = "Number of Clusters", ylab = "Pct Explained")
  
  par(mfrow = c(1, 1))
}

# Plot the elbow plot
wssplot(Clustertable, nc = 10)


# Determine optimal k using silhouette method
#optimal_k_silhouette <- fviz_nbclust(Clustertable, kmeans, method = "silhouette")
optimal_k_silhouette <-2
# Extract the optimal k from the silhouette method
optimal_k <- 2

# Perform k-means clustering with the optimal k
kmeans_model <- kmeans(Clustertable, centers = optimal_k, nstart = 10)
Clustertable$cluster <- as.factor(kmeans_model$cluster)

# Create a contingency table
cluster_table <- table(Clustertable$SMK_stat_type_cd, Clustertable$cluster)

# Add cluster assignments to the original data
data_with_clusters <- cbind(Clustertable, Cluster = as.factor(kmeans_model$cluster))

# Print the cluster assignments
print(data_with_clusters$cluster)

# Visualize clusters
fviz_cluster(kmeans_model, data = as.numeric(Clustertable))
fviz_cluster(kmeans_model, data = as.data.frame(as.numeric(Clustertable)))

#########Feature Selection###########
# Create a decision tree model for predicting features for SMoking data
decision_tree_model1 <- rpart(as.factor(SMK_stat_type_cd) ~ ., data = train_data_smk, method = "class")
decision_tree_model1
# Extract variable importance
feature_importance <- varImp(decision_tree_model1)
plot(feature_importance, main = "The most Important Variables for Predictions")


# Assuming your data frame is named 'train_data_smk'
# Extract row names and values
row_names <- rownames(feature_importance)
values <- feature_importance$Overall

# Create a data frame
data_frame <- data.frame(rownames = row_names, values = values)

# Rearrange the data frame by values in descending order
data_frame <- data_frame[order(-data_frame$values), ]

smk_features_plot<-ggplot(data_frame, aes(x=reorder(rownames, -values), y = values)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Bar Plot of Variable Values", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
smk_features_plot
# Display variable importance
print(feature_importance)

# Display the decision tree plot
plot(decision_tree_model)
text(decision_tree_model, cex = 0.8)

# Print the results
print(result)

# Get the best predictors
best_predictors <- names(result$optVariables)
cat("Best predictors: ", paste(best_predictors, collapse = ", "), "\n")

#########Decision Tree for Deinking 
# Create a decision tree model for predicting features for SMoking data
decision_tree_model2 <- rpart(as.factor(DRK_YN) ~ ., data = test_data_drk, method = "class")

# Extract variable importance
feature_importance <- varImp(decision_tree_model2)
plot(feature_importance, main = "The most Important Variables for Predictions")


# Assuming your data frame is named 'train_data_smk'
# Extract row names and values
row_names <- rownames(feature_importance)
values <- feature_importance$Overall

# Create a data frame
data_frame <- data.frame(rownames = row_names, values = values)

# Rearrange the data frame by values in descending order
data_frame <- data_frame[order(-data_frame$values), ]

drk_features_plot<-ggplot(data_frame, aes(x=reorder(rownames, -values), y = values)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Bar Plot of Variable Values", x = "Variable", y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

drk_features_plot
# Display variable importance
print(feature_importance)

# Display the decision tree plot
plot(decision_tree_model)
text(decision_tree_model, cex = 0.8)

# Print the results
print(result)

# Get the best predictors
best_predictors <- names(result$optVariables)
cat("Best predictors: ", paste(best_predictors, collapse = ", "), "\n")

############### MODEL BUILDING #######################
# Building Models to predict/classify the outcome 


#### Logistic Regression 
Ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)

# Training logistic regression model with cross-validation
glm_model <- train( DRK_YN ~ ., 
                    data = train_data_drk, 
                    method = "glm", 
                    family = "binomial",  
                    trControl = Ctrl)

# Print the results
print(glm_model)

glm_model


####MARS Model
trControl <- trainControl(method = "repeatedcv", number = 10, repeats= 3)
mars_model <- train(DRK_YN ~.,
                    data = train_data_drk, method = "earth", trControl=trControl,
                    preProc= c("center","scale"))
mars_model


#Descision Tree
dt.fit <- train(DRK_YN ~.,
                data = train_data_drk, method = "rpart", trControl=trControl)
dt.fit

## Extreme gradient boosting (XGBoost) ===================================
Ctrl <- trainControl(method = "repeatedcv", number = 10, repeats= 3)

XGBGrid <- expand.grid(nrounds = c(400),
                       max_depth = c(3),
                       eta = c(0.05),
                       gamma = 0.01,
                       colsample_bytree = 0.6,
                       min_child_weight = 0,
                       subsample = 0.75)


XGBoost_Model <- train( DRK_YN ~., 
                        data = train_data_drk, method = "xgbTree", trControl=Ctrl,verbose = F, verbosity = 0,
                        tuneGrid=XGBGrid, preProc= c("center","scale"))

XGBoost_Model






##### models for Multiclass Classification


#Logistic Regression with multinom
#model <- multinomial_glm(as.factor(SMK_stat_type_cd) ~., data = train_data_smk, family = "multinomial")



model <- multinom(as.factor(SMK_stat_type_cd) ~., data = Clustered_train_data, family = "multinomial")
summary(model)
predicted.classes <- model %>% predict(Clustered_train_data)
predicted.classes <- model %>% predict(Clustered_test_data)
head(predicted.classes)
mean(predicted.classes == test_data_smk$SMK_stat_type_cd)

confusionMatrix(predicted.classes,as.factor(Clustered_test_data$SMK_stat_type_cd),mode = "everything")
Impvar<-varImp(model)
plot(Impvar, main = "The most Important Variables for Predictions")




# Decision Tree model
# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 5, search = "grid")

## Customsing the tuning grid (ridge regression has alpha = 0)
multi_classification_Tree_Grid =  expand.grid(maxdepth = c(1,3,5,7,9))

set.seed(50)


Df_model = train(as.factor(SMK_stat_type_cd) ~., data = Clustered_train_data, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid)


# summarising the results

summary(Df_model)
predicted.classes <- Df_model %>% predict(test_data_smk)
head(predicted.classes)
mean(predicted.classes == test_data_smk$SMK_stat_type_cd)


####MARS Model

train_data_smk$SMK_stat_type_cd <- as.factor(train_data_smk$SMK_stat_type_cd)

# Build the MARS model for multiclass classification
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = multiClassSummary)

# Check factor levels
levels(train_data_smk$SMK_stat_type_cd)

# Modify factor levels if needed
levels(train_data_smk$SMK_stat_type_cd) <- make.names(levels(train_data_smk$SMK_stat_type_cd))

# MARS model
mars_model <- train(SMK_stat_type_cd ~ .,
                    data = train_data_smk,
                    method = "earth",
                    trControl = trControl,
                    preProcess = c("center", "scale"))
mars_model


###XGBoost Model
# Assuming your response variable is a factor with multiple classes
train_data_smk$SMK_stat_type_cd <- as.factor(train_data_smk$SMK_stat_type_cd)

# Extreme Gradient Boosting (XGBoost) for Multiclass Classification
Ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE, summaryFunction = multiClassSummary)

XGBGrid <- expand.grid(
  nrounds = c(400),
  max_depth = c(3),
  eta = c(0.05),
  gamma = 0.01,
  colsample_bytree = 0.6,
  min_child_weight = 0,
  subsample = 0.75
)

XGBoost_Model <- train(
  SMK_stat_type_cd ~ .,
  data = Clustered_train_data,
  method = "xgbTree",
  trControl = Ctrl,
  verbose = FALSE,
  verbosity = 0,
  tuneGrid = XGBGrid,
  preProcess = c("center", "scale")
)

# Print the XGBoost model
print(XGBoost_Model)
