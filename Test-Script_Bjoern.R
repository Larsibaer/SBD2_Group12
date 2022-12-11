libraries = c("readr", "ggplot2","Boruta", "dlookr", "ROCR", "caret", "pROC", "dplyr", "ROSE", "corrplot", "DescTools", "ggpubr", "tidyverse", "RColorBrewer", "ggcorrplot", "PerformanceAnalytics", "corrr", "networkD3", "reshape", "knitr")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rm(list=ls())

set.seed(7)

setwd('C:/Git/SBD/SBD2_Group12')
loan <- read_csv("loan_sample_12.csv")
data <- data.frame(loan)




#----------------------------------------------------------------------------
#--------- Exploration -------------------------------------------------------
#----------------------------------------------------------------------------


#check data
dim(data)
str(data)
head(data)
tail(data)

#duplicates
sum(duplicated(data))

#summarize data
summary(data)
overview <- overview(data)
list(overview)

#----------------------------------------------------------------------------
#---------TARGET SAMPLE------------------------------------------------------
#----------------------------------------------------------------------------

#Target varible
table(data$Status)
barplot(table(data$Status))


# Undersampling

set.seed(7)
data_balanced <- ovun.sample(Status ~ ., data=data, method = "under")
data_under <- data.frame(data_balanced[["data"]])

table(data_under$Status)
barplot(table(data_under$Status))


#----------------------------------------------------------------------------
#---------OUTLIERS-----------------------------------------------------------
#----------------------------------------------------------------------------

#Dimensions of new set
dim(data_under)

#Numeric Variables
data_under_num <- data_under %>%
  select_if(is.numeric)
data_under_num <- as.data.frame(data_under_num)
dim(data_under_num)


#Categorical Variables
data_under_cat <- data_under %>%
  select_if(is.character)
data_under_cat <- as.data.frame(data_under_cat)
dim(data_under_cat)



boxplot(scale(data_under_num), xaxt = "n") 
text(x = 1:length(data_under_num),
     y = par("usr")[3] - 0.8,
     labels = names(data_under_num),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 0.8,
     adj = 1)

#Diagnose Outliers
diagnose_outlier(data_under_num) 
diagnose_numeric(data_under)



#Visualize With And Without Outliers
data_under_num %>%
  plot_outlier(diagnose_outlier(data_under_num) %>%
                 filter(outliers_ratio >= 0.5) %>%          # dplyr
                 select(variables) %>%
                 unlist())


# Define Outlier functions
#Cap
outlier_cap <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

#Remove
outlier_remove <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- NA
  x[x > quantiles[2]] <- NA
  x
}


# ----Use this to clean up data set further: -------------------------------
#diagnose_web_report(data_under_num)


#----------CAPPING OUTLIERS--------------------------------
#Lets cap and see:
# Apply outlier function to data set
data_under_num_capped <- map_df(data_under_num, outlier_cap)


boxplot(scale(data_under_num_capped), xaxt = "n") 
text(x = 1:length(data_under_num_capped),
     y = par("usr")[3] - 0.8,
     labels = names(data_under_num_capped),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 0.8,
     adj = 1)

# New data set with capped outliers
data_under_capped <- cbind(data_under_num_capped, data_under_cat)

#Looks good !



#----------REMOVING OUTLIERS--------------------------------
#Lets remove and compare:
# Apply outlier function to data set
data_under_num_removed <- map_df(data_under_num, outlier_remove)
data_under_num_removed <- data_under_num_removed[complete.cases(data_under_num_removed),]


boxplot(scale(data_under_num_removed), xaxt = "n") 
text(x = 1:length(data_under_num_removed),
     y = par("usr")[3] - 0.8,
     labels = names(data_under_num_removed),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 0.8,
     adj = 1)

# New data set without outliers

data_under_removed <- cbind(data_under_num_removed, data_under_cat)

#Looks good !

#impute only single cols
diagnose_numeric(data_under_num)
imp_income <- imputate_outlier(data_under, annual_inc, method = "median")
imp_income
summary(imp_income)

#-----------------------------------------------------------------------
#--------- FEATURES SELECTION ------------------------------------------
#-----------------------------------------------------------------------

#Dimensions of new sets
dim(data_under_capped)
dim(data_under_num_capped)

dim(data_under_removed)
dim(data_under_num_removed)

#shorter variable names
#---------HERE CAPPED OUTLIERS WERE CHOSEN ---------------------------------
clean <- data.frame(data_under_capped)
clean_num <- data.frame(data_under_num_capped)
dim(clean)

#or go with outliers removed...
#clean <- data_under_removed
#clean_num <- data_under_num_removed




#----------Correlation of numeric values to Status---------------------------
corr <- cor(clean_num)
ggcorrplot(corr)
corrplot(corr)
p_value_mat <- cor_pmat(clean_num)
ggcorrplot(corr, type = "lower", p.mat = p_value_mat) 



#----------Burata Analyses -----------------------------------------------
clean$Status <- as.factor(clean$Status)
dim(clean)

#----takes 10-15min!!!!------
boruta_output <- Boruta(Status~., data = clean, doTrace=2)

boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
print(boruta_signif)

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

#---------------------------------------------------------------
#---------------REMOVING UNSIGNIFICANT VARIABLES -----------------
#---------------------------------------------------------------


Status <- clean$Status

#Remove useless columns:
features <- cbind(clean[boruta_signif], Status)

dim(features)


#----------------------------------------------------------------------------
#--------- TRAIN THE MODEL---------------------------------------------------
#----------------------------------------------------------------------------


#--------Divide Sample in train and test ----------------------------
set.seed(7)
div <- createDataPartition(y = features$Status, p = 0.8, list = F)

data.train <- features[div,]

data.test <- features[-div,]

# -----See the proportions -------------
PercTable(features$Status)
PercTable(data.train$Status)
PercTable(data.test$Status)


#All good!


#Train classifier
fit1 <- glm(Status ~ ., data=data.train,family=binomial())
summary(fit1)


#only show significant variables:
significant.variables <- summary(fit1)$coeff[-1,4] < 0.05
names(significant.variables)[significant.variables == TRUE]


#Plot ROC
data.test$fit1_score <- predict(fit1,type='response',data.test)
fit1_pred <- prediction(data.test$fit1_score, data.test$Status)
fit1_roc <- performance(fit1_pred, "tpr", "fpr")
plot(fit1_roc, lwd=1, colorize = TRUE, main = "Fit1: Logit - ROC Curve")
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1, lty=3)

# --> could be better..

#precision/ recall curve
fit1_precision <- performance(fit1_pred, measure = "prec", x.measure = "rec")
plot(fit1_precision, main="Fit1: Logit - Precision vs Recall")




#Confusion matrix
confusionMatrix(as.factor(round(data.test$fit1_score)), data.test$Status)

#Accuracy could be better, now at 60%


#AUC
fit1_auc <- performance(fit1_pred, measure = "auc")
cat("AUC: ",fit1_auc@y.values[[1]]*100)







#-------------------------------------------------------------------------------
#--------- Reference: no cleaning of the data ----------------------------------
#-------------------------------------------------------------------------------

## Training and testing without any data quality checks and feature selection

# Split the data 
set.seed(7)
data$Status <- as.factor(data$Status)
div_2 <- createDataPartition(y = data$Status, p = 0.7, list = F)

# Training Sample
data.train_2 <- data[div,] # 70% here

# Test Sample
data.test_2 <- data[-div,] # rest of the 30% data goes here



# Fit the model
fit2 <- glm(Status ~ ., data=data.train_2,family=binomial())
summary(fit2)




# Extract and compare the significant variables 
significant.variables_2 <- summary(fit2)$coeff[-1,4] < 0.05
names(significant.variables)[significant.variables == TRUE]
names(significant.variables_2)[significant.variables == TRUE]



# Test the perfromance 
data.test_2$fit2_score <- predict(fit2,type='response',data.test_2)
fit2_pred <- prediction(data.test_2$fit2_score, data.test_2$Status)
fit2_roc <- performance(fit2_pred, "tpr", "fpr")
plot(fit2_roc, lwd=1, colorize = TRUE, main = "Fit2: Logit - ROC Curve")
lines(x=c(0, 1), y=c(0, 1), col="black", lwd=1, lty=3)




# Extract the confusion matrix 
confusionMatrix(as.factor(round(data.test_2$fit2_score)), data.test_2$Status)



# Extract the AUC value 
fit2_auc <- performance(fit2_pred, measure = "auc")
cat("AUC: ",fit2_auc@y.values[[1]]*100)






