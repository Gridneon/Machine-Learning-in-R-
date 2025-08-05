
#load data into r
data1<-read.csv("fraud_claimdata.csv")
View(data1)

#exploratory data analysis 
str(data1)
#summary of numeric features
summ  ary(data1$policy_ageinmonth)
summary(data1$claimant_age)
summary(data1$numpre_claim)
summary(data1$freq_claim)
summary(data1$claimamt1)


#summary of categorical features 
table(data1$witness)
table(data1$witness)/sum(table(data1$witness))*100

table(data1$claimant_sex)                         
table(data1$claimant_sex)/sum(table(data1$claimant_sex))*100

table(data1$claimant_occup) 
table(data1$claimant_occup)/sum(table(data1$claimant_occup))*100

table(data1$policy_type)
table(data1$policy_type)/sum(table(data1$policy_type))*100


table(data1$time_ofinci)
table(data1$time_ofinci)/sum(table(data1$time_ofinci))*100

table(data1$policer_fil)
table(data1$policer_fil)/sum(table(data1$policer_fil))*100


table(data1$claimant_es)
table(data1$claimant_es)/sum(table(data1$claimant_es))*100


#contigency table of features in comparison to fraud
table(data1$fraud,data1$claimant_es)/sum(table(data1$fraud,data1$claimant_es))*100
table(data1$fraud,data1$doc_subm)/sum(table(data1$fraud,data1$doc_subm))*100
table(data1$fraud,data1$time_ofinci)/sum(table(data1$fraud,data1$time_ofinci))*100
table(data1$fraud)/sum(table(data1$fraud))*100

#ggplot display of continuous variables 
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

plot1 <- ggplot(data1, aes(x =  policy_ageinmonth)) +
  geom_boxplot(outlier.shape = NA,color = "purple") +  # Boxplot without outliers
  labs(title = "Policy Age in Months", x = "Policy Age (Months)", y = "") +
  theme_minimal()

plot2 <- ggplot(data1, aes(x = claimant_age)) +
  geom_boxplot(outlier.shape = NA,color = "red") +
  labs(title = "Claimant Age", x = "Claimant Age (Years)", y = "") +
  theme_minimal()

plot3 <- ggplot(data1, aes(x  = numpre_claim)) +
     geom_boxplot(outlier.shape = NA,color = "green") +
     
     labs(title = "Number of Previous Claims", x = "Previous Claims", y = "") +
     theme_minimal()

plot4 <- ggplot(data1, aes(x =  freq_claim)) +
  geom_boxplot(outlier.shape = NA, color = "purple") +
 
  labs(title = "Frequency of Claims", x = "Claim Frequency", y = "") +
  theme_minimal()


# combining the plots 
combined_plots <- grid.arrange(plot1, plot2,plot3, plot4, ncol = 2)
print(combined_plots)


# Summarizing the counts of fraud categories
fraud_summary <- as.data.frame(table(data1$fraud))
colnames(fraud_summary) <- c("Fraud", "Count")  # Rename columns for clarity

# Creating the pie chart
pie_chart <- ggplot(fraud_summary, aes(x = "", y = Count, fill = Fraud)) +
  geom_bar(stat = "identity", width = 1) +  # Create a bar chart
  coord_polar(theta = "y") +  # Convert to pie chart
  labs(title = "Distribution of Fraudulent Claims", 
       fill = "Fraudulent", 
       x = NULL, y = NULL) +  # Add title and labels
  theme_void() + 
  scale_fill_manual(values = c("grey", "grey39"))+ # Remove background and axes
  geom_text(aes(label = paste(round(Count / sum(Count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5))  # Add percentage labels

print(pie_chart)


#creating a stackbar chart 

# Creating a table of fraud vs. policy_type
table1 <- table(data1$fraud, data1$policy_type)

# Converting the table to a data frame
fraud_summary <- as.data.frame(table1)
colnames(fraud_summary) <- c("Fraud", "Policy_Type", "Count")  # Rename columns for clarity

# Creating the stacked bar chart
stackedbar <- ggplot(fraud_summary, aes(x = Policy_Type, y = Count, fill = Fraud)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  labs(title = "Fraud Distribution by Policy Type", 
       x = "Policy Type", 
       y = "Count", 
       fill = "Fraudulent") +  # Add titles and labels
  theme_minimal() +  # Use a minimal theme
  scale_fill_manual(values = c("grey", "grey39")) +
  geom_text(aes(label = paste(round(Count / sum(Count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5))# Customize fill colors

# Display of stacked bar chart
print(stackedbar)




# Creating a table of fraud vs. policy_type
table2 <- table(data1$fraud, data1$claimant_sex)

# Converting the table to a data frame
fraud_summary2 <- as.data.frame(table2)
colnames(fraud_summary2) <- c("Fraud", "gender", "Count")  # Rename columns for clarity

# Creating the stacked bar chart
stackedbar <- ggplot(fraud_summary2, aes(x =gender , y = Count, fill = Fraud)) +
  geom_bar(stat = "identity", position = "stack") +  # Stacked bar chart
  labs(title = "Fraud Distribution by gender", 
       x = "gender", 
       y = "Count", 
       fill = "Fraudulent") +  # Add titles and labels
  theme_minimal() +  # Use a minimal theme
  scale_fill_manual(values = c("grey", "grey39"))+
  geom_text(aes(label = paste(round(Count / sum(Count) * 100, 1), "%")), 
            position = position_stack(vjust = 0.5))# Customize fill colors

# Display of stacked bar chart
print(stackedbar)

# Function to remove outliers iteratively
remove_outliers <- function(data, column) {
  repeat {
    Q1 <- quantile(data[[column]], 0.25)
    Q3 <- quantile(data[[column]], 0.75)
    IQR <- Q3 - Q1
    upper_limit <- Q3 + 1.5 * IQR
    lower_limit <- Q1 - 1.5 * IQR
    
    outliers <- data[[column]] < lower_limit | data[[column]] > upper_limit
    if (sum(outliers) == 0) break
    data <- data[!outliers, ]
  }
  return(data)
}

# Remove outliers from the dataset
data_clean <- remove_outliers(data1, "claimamt1")


# Visualize the cleaned data
boxplot(data_clean$claimamt1, main = "Boxplot of claimamt1 (After Outlier Removal)")
hist(data_clean$claimamt1, main = "Histogram of claimamt1 (After Outlier Removal)")

# Summary statistics
summary(data_clean$claimamt1)
#correction 1
data_clean$policy_type<-factor(data_clean$policy_type)       

data_clean$payme_meth <-factor(data_clean$payme_meth)
data_clean$change_policy <-factor(data_clean$change_policy)
data_clean$claimant_sex <-factor(data_clean$claimant_sex )   
data_clean$claimant_occup<-factor(data_clean$claimant_occup )      
data_clean$claimant_es <-factor(data_clean$claimant_es)    
data_clean$engine_capacity <-factor(data_clean$engine_capacity)
data_clean$time_ofinci <-factor(data_clean$time_ofinci)
       
data_clean$witness <-factor(data_clean$witness)     
data_clean$doc_subm  <-factor(data_clean$doc_subm )        
data_clean$policer_fil<-factor(data_clean$policer_fil )       
         
data_clean$policy_year<-factor(data_clean$policy_year)
data_clean$fraud <- factor(data_clean$fraud)
data_clean<-data_clean[,-12]

# structure  of the data after transforming 
str(data_clean)
 
#upsampling to keep the target varuiable balanced 
library(caret) # load the caret package 
mach_datause<-upSample(data_clean,data_clean$fraud,yname ="fraud")


# using Kaplan-Meier curve for fraudulent claim prediction
library(survival)
# Fit the Kaplan-Meier curve
km_fit <- survfit(Surv(time = policy_ageinmonth, event = fraud_indicator) ~ fraud, data = mach_datause)

# Plot the Kaplan-Meier curve
plot(km_fit, xlab = "Time (Policy Age in Months)", ylab = "Probability of Fraud Detection", 
     col = c("blue", "red"), lwd = 2)

# Add a legend
legend("topright", legend = c("Non-Fraud", "Fraud"), col = c("blue", "red"), lty = 1, lwd = 2)

# Add a horizontal line at 50% probability (optional)
abline(h = 0.5, col = "gray", lty = 2)
################

########################################

#Machine learning Models

#Machine learning Models
#upsampling to keep the target variable balanced 

set.seed(1234)
library(caret)

mach_datause<-upSample(data_clean,data_clean$fraud,yname ="fraud")





# Installing package pROC
library(pROC)

#split of the data
split_index <- createDataPartition(mach_datause$fraud, p = 0.8, list = FALSE)

train_data <-mach_datause[split_index, ][,-20]
test_data <- mach_datause[-split_index, ][,-20]


# Fit logistic regression model
logistic_model <- glm(
  fraud ~ .,
  data = train_data,
  family = binomial(link = "logit"),
 
)



# Predict probabilities on the test set
predicted_probslr <- predict(logistic_model, newdata = test_data, type = "response")

# Convert probabilities to predicted classes (using a threshold of 0.05)
predicted_classeslr <- ifelse(predicted_probslr > 0.4, "Yes", "No")
predicted_classeslr <- factor(predicted_classeslr, levels = c("No", "Yes"))

# Confusion matrix
conf_matrixlr <- confusionMatrix(predicted_classeslr, test_data$fraud, positive = "Yes")
print(conf_matrixlr)

#for the ROc Curve of logistic regression
log_roc <- roc(test_data$fraud, predicted_probslr )

###################################
#random forest
library(randomForest)
library(dplyr)
library(caret)  # For confusion matrix

# Train the Random Forest model
rf_model <- randomForest(
  fraud ~ .,  # Use all predictors
  data = train_data,  # Training data
  ntree = 500,  # Number of trees in the forest
  mtry = sqrt(ncol(train_data) - 1),  # Corrected sqrt usage
  importance = TRUE,  # Calculate variable importance
  classwt = c("No" = 1, "Yes" = 10)  # Adjust class weights for imbalance (optional)
)

# View model summary
print(rf_model)

# Generate predictions on test data
predictions <- predict(rf_model, newdata = test_data, type = "class")

# Create confusion matrix
confusion_matrix <- confusionMatrix(predictions, test_data$fraud)
print(confusion_matrix)



# model Importance 
# Extracting importance scores
importance_scores<- importance(rf_model)

# Converting to data frame
importance_df <- as.data.frame(importance_scores)

importance_df <- importance_df %>% 
  mutate(Variable = rownames(importance_df)) %>%
  arrange(desc(MeanDecreaseGini))  # Sort by MeanDecreaseGini

# Ploting variable importance
ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance (Mean Decrease in Gini)",
       x = "Variables",
       y = "Mean Decrease in Gini") +
  theme_minimal()


#for the ROc Curve of random forest 
rf_roc <- roc(test_data$fraud, predicted_probsrf)


################################################
#support vector Machine Model 
library(e1071)

svm_model <- svm(fraud ~ ., 
                 data = train_data, 
                 kernel = "radial", 
                 cost = 1, 
                 gamma = 1 / ncol(train_data), 
                 probability = TRUE)  # Enable probability predictions

# Predicting class probabilities on test data
svm_probs <- attr(predict(svm_model, test_data, probability = TRUE), "probabilities")[, "Yes"]

# Computing  confusion matrix 
predicted_classes <- ifelse(svm_probs > 0.5, "Yes", "No")
predicted_classes <- factor(predicted_classes, levels = c("No", "Yes"))

conf_matrix <- confusionMatrix(predicted_classes, test_data$fraud, positive = "Yes")
print(conf_matrix)

#for the ROc Curve of svm
svm_roc <- roc(test_data$fraud, svm_probs)

#########################################

# Loading  necessary libraries
library(pROC)
library(ggplot2)

# Step 1: Extract FPR and TPR values
log_roc_fpr <- 1 - log_roc[['specificities']]  # FPR = 1 - Specificity
log_roc_tpr <- log_roc[['sensitivities']]      # TPR = Sensitivity

rf_roc_fpr <- 1 - rf_roc[['specificities']]    # FPR = 1 - Specificity
rf_roc_tpr <- rf_roc[['sensitivities']]        # TPR = Sensitivity

svm_roc_fpr <- 1 - svm_roc[['specificities']]  # FPR = 1 - Specificity
svm_roc_tpr <- svm_roc[['sensitivities']]      # TPR = Sensitivity

# Step 2: Create data frames for each model's ROC curve
log_roc_df <- data.frame(
  fpr = log_roc_fpr,
  tpr = log_roc_tpr,
  model = "LRM"
)

rf_roc_df <- data.frame(
  fpr = rf_roc_fpr,
  tpr = rf_roc_tpr,
  model = "RFM"
)

svm_roc_df <- data.frame(
  fpr = svm_roc_fpr,
  tpr = svm_roc_tpr,
  model = "SVM"
)

# Step 3: Combine all data frames into one
combined_roc_df <- rbind(log_roc_df, rf_roc_df, svm_roc_df)

# Step 4: Extract AUC values for each model
auc_values <- data.frame(
  model = c("LRM", "RFM", "SVM"),
  auc = c(auc(log_roc), auc(rf_roc), auc(svm_roc))
)

# Step 5: Plot the ROC curves with AUC values
ggplot(combined_roc_df, aes(x = fpr, y = tpr, color = model)) +
  geom_line(linewidth = 1) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") + 
  geom_text(data = auc_values, aes(x = 0.6, y = 0.3 - 0.1 * as.numeric(factor(model)), 
                                   label = paste(model, "AUC =", round(auc, 3))), 
            hjust = 0, vjust = 0, size = 4, color = "black") +
  theme_minimal() + 
  labs(
    title = "ROC Curves for Logistic Regression, Random Forest, and SVM",
    x = "False Positive Rate (FPR)",
    y = "True Positive Rate (TPR)",
    color = "Model"
  ) +
  theme(legend.position = "bottom") 

