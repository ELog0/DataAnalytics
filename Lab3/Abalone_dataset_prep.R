####################################
##### Abalone Data Preparation #####
####################################


# read dataset
abalone.data <- read.csv("abalone_dataset.csv")

## add new column age.group with 3 values based on the number of rings 
abalone.data$age.group <- cut(abalone.data$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
