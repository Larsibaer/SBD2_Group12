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
#--------- Exercise 1 -------------------------------------------------------
#----------------------------------------------------------------------------


#check data
dim(data)
str(data)
head(data)
tail(data)

#summarize data
summary(data)
overview <- overview(data)
list(overview)

#---------TARGET SAMPLE------------------------------------------------------

#Target varible
table(data$Status)
barplot(table(data$Status))


# equalizing the sample

set.seed(7)
data_balanced <- ovun.sample(Status ~ ., data=data, method = "under")
data_under <- data.frame(data_balanced[["data"]])

table(data_under$Status)
barplot(table(data_under$Status))



#---------OUTLIERS------------------------------------------------------

#Dimensions of new set
dim(data_under)

#Numeric Variables
data_under_num <- data_under %>%
  select_if(is.numeric)


#Categorical Variables
data_under_cat <- data_under %>%
  select_if(is.character)


#Visualize it
hist(data_under_num)

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

#Visualize With And Without Outliers
data_under_num %>%
  plot_outlier(diagnose_outlier(data_under_num) %>%
                 filter(outliers_ratio >= 0.5) %>%          # dplyr
                 select(variables) %>%
                 unlist())


# Define Outlier function
outlier <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

# Apply outlier function to data set
data_under_num_without <- map_df(data_under_num, outlier)

#Visualize it again
hist(data_under_num_without)

boxplot(scale(data_under_num_without), xaxt = "n") 
text(x = 1:length(data_under_num_without),
     y = par("usr")[3] - 0.8,
     labels = names(data_under_num_without),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 0.8,
     adj = 1)

# New data set without outliers

data_under_without <- cbind(data_under_num_without, data_under_cat)

#Looks good !

#(Too good... where are the new outliers?)

#---------FEATURES------------------------------------------------------

#Dimensions of new sets
dim(data_under_without)
dim(data_under_num_without)

#Correlation of numeric values to Status
# ---- Code here ---- 
#Correlation of Categorical values to Status
# ---- Code here ---- 


#Plot income to loan amount, interactive
# ---- Code here ---- 





#--------------------------------------
#--------- Exercise 2 -----------------
#--------------------------------------



