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

#duplicates
sum(duplicated(data))

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
data_under_num <- as.data.frame(data_under_num)


#Categorical Variables
data_under_cat <- data_under %>%
  select_if(is.character)
data_under_cat <- as.data.frame(data_under_cat)



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



#diagnose_web_report(data_under_num)

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


#---------FEATURES------------------------------------------------------

#Dimensions of new sets
dim(data_under_capped)
dim(data_under_num_capped)

dim(data_under_removed)
dim(data_under_num_removed)

#shorter variable names
clean <- data.frame(data_under_capped)
clean_num <- data.frame(data_under_num_capped)

#or go with outliers removed...
#clean <- data_under_removed
#clean_num <- data_under_num_removed



#Correlation of numeric values to Status
# ---- Code here ---- 


corr <- cor(clean_num)
corr

ggcorrplot(corr)
# --> no highly correlated data! thats good

data_key <- clean %>%
  as_tibble() %>%
  select_if(is.numeric) %>%
  gather(key = "variable", value = "value")

data_key

for (i in 1:length(clean_num)) {
  print(ggplot(clean, aes(y = clean[,i], color = Status)) + 
          geom_boxplot() + 
          ylab(names(clean[i])) + 
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank()))
}




#Correlation of Categorical values to Status
# ---- Code here ---- 


#Plot income to loan amount, interactive
# ---- Code here ---- 





#--------------------------------------
#--------- Exercise 2 -----------------
#--------------------------------------



