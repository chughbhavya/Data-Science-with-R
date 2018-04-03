
#Load Dataset
train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv",header = TRUE)

#Add a survived variable in test set to allow combining datasets
test.survived <- data.frame(Survived = rep('None', nrow(test)),test[,])
test.survived <- test.survived[c(2,1,3,4,5,6,7,8,9,10,11,12)]

#Combine both the dataframes with rbind
data.combined <- rbind(train, test.survived)

#Determining the datatype of the columns in frame. Factor is basically a category variable in R which has limited levels.
str(data.combined)

#Making the survived and pclass variable as factors
data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#Realizing the data is a little skewed as people who survived are more than people who did not.
table(data.combined$Survived)

#Loading up ggplot for visualization
library(ggplot2)

#Hypothesis testing > Rich people survive more
train$Pclass = as.factor(train$Pclass)
ggplot(train, aes(x=Pclass, fill=factor(Survived))) + stat_count(width=0.5) + xlab('Pclass') + ylab('Total Count') +
  labs(fill="Survived")

head(as.character(train$Name))

#Getting all the unique names in the combined dataset
length(unique(as.character(data.combined$Name)))
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#Check the duplicate name records
data.combined[which(data.combined$Name %in% dup.names),]

#Correlation between titles and any other variables
library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

#Correlation between  Mrs. titles and any other variables
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs")),]
mrses[1:5,]

males <- data.combined[which(train$Sex == 'male'),]

extractTitle <- function(name){
  name <- as.character(name)
  
  if (length(grep('Miss.', name)) > 0){
    return ('Miss.')
  } else if (length(grep('Master.', name)) > 0){
    return ('Master.')
  } else if (length(grep('Mrs.', name)) > 0){
    return ('Mrs.')
  } else if (length(grep('Mr.', name)) > 0){
    return ('Mr.')
  } else {
    return ('Other')
  }
}

title <- NULL
for (i in 1:nrow(data.combined)){
  title <- c(title, extractTitle(data.combined[i,'Name']))
}
data.combined$title <- as.factor(title)

#Train dataset work
ggplot(data.combined[1:891,], aes(x=title, fill=Survived)) + stat_count(width = 0.5) + facet_wrap(~Pclass)













