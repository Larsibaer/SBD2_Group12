libraries = c("readr", "ggplot2","Boruta", "dlookr", "ROCR", "caret", "pROC", "dplyr", "ROSE", "corrplot", "DescTools", "ggpubr", "tidyverse", "RColorBrewer", "ggcorrplot", "PerformanceAnalytics", "corrr", "networkD3", "reshape", "knitr")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)

rm(list=ls())

set.seed(7)

#setwd("C:/Users/Student/SBD2/SBD2_Group12")
setwd('C:/Git/SBD/SBD2_Group12')
loan <- read_csv("loan_sample_12.csv")
data <- data.frame(loan)




#--------------------------------------
#--------- Exercise 1 -----------------
#--------------------------------------


#check data
dim(data)
str(data)
head(data)
tail(data)

#summarize data
summary(data)
overview <- overview(data)
list(overview)



#Target varible
table(data$Status)
barplot(table(data$Status))


ggplot(data, aes(x = Status, fill = Status)) +
  geom_bar() +
  ylab("Count") +
  xlab("Status of the loan")


# equalizing the sample

set.seed(7)
data_balanced <- ovun.sample(Status ~ ., data=data, method = "under")
data_under <- data.frame(data_balanced[["data"]])

ggplot(data_under, aes(x = Status, fill = Status)) +
  geom_bar() +
  ylab("Count") +
  xlab("Status of the loan")


#over-writing data variable
data <- data_under


#Numeric Variables
data_num <- data %>%
  select_if(is.numeric)
data_cat <- data %>%
  select_if(is.character)


hist(data_num)

boxplot(scale(data_num), xaxt = "n") 
text(x = 1:length(data_num),
     y = par("usr")[3] - 0.8,
     labels = names(data_num),
     xpd = NA,
     ## Rotate the labels by 35 degrees.
     srt = 35,
     cex = 0.8,
     adj = 1)

diagnose_outlier(data) 

data %>%
  plot_outlier(diagnose_outlier(data) %>%
                 filter(outliers_ratio >= 0.5) %>%          # dplyr
                 select(variables) %>%
                 unlist())

# A Lot of outliers are present in the anual income.  This must 
# be corrected: (remove!)


outlier <- function(x){
  quantiles <- quantile(x, c(.05, .95))
  x[x < quantiles[1]] <- quantiles[1]
  x[x > quantiles[2]] <- quantiles[2]
  x
}

data_num_without <- map_df(data_num, outlier)

cols <- data_num
data_without <- cbind(data, cols)

data_without_num <- data_without %>%
  select_if(is.numeric)


diagnose_outlier(data_without_num)



boxplot(data_without_num)
hist(data_new_under)

#Looks good !




#Correlation of numeric values to Status
# ---- Code here ---- 




#Correlation of Categorical values to Status
# ---- Code here ---- 


#Plot income to loan amount, interactive
# ---- Code here ---- 





#--------------------------------------
#--------- Exercise 2 -----------------
#--------------------------------------



