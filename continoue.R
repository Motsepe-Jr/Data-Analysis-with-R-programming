# Data Science Introduction (Titanic Data set) 

# Load data
train <- read.csv('train.csv', header = TRUE)
test <- read.csv("test.csv", header = TRUE)

# Cleaning the data set
train <- train[, -c(1)]
test <- test[, -c(1)]


# Add the survive variable to the test dataframe in order to rbind the set
Test_survived <- data.frame(Survived = rep('None', nrow(test)), test[,])

# Combine the test survive with train data set
Combined_data <- rbind(train, Test_survived)
View(Combined_data)
str(Combined_data)

# Change variables to factors
Combined_data$Sex <- as.factor(Combined_data$Sex)
Combined_data$Pclass <- as.factor(Combined_data$Pclass)

# T
# View the gross survival rate
table(Combined_data$Survived)

#View the distribution across the class 
table(Combined_data$Pclass)

# Visualise the data
library(tidyverse)

# Paassenger in the first class survived at a higher rate "Hypothesis"
train$Pclass <- as.factor(train$Pclass)
ggplot(data = train, mapping = aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(width = .6) +
  xlab('Pclass') +
  ylab('Total Count') +
  labs(fill = 'Survived')


# Examine the first names in the training data set
head(as.character(train$Name), 10)
tail(as.character(train$Name), 10)

# Unique name in the train and test set
length(unique(Combined_data$Name)) # the entire set is 1309 with 1307 unique name. Duplication, Coincidence? 

# Take a close look at dupluicate name,
dup_name <- as.character(Combined_data[which(duplicated(as.character(Combined_data$Name))), "Name"])
# examine the combined data set.
Combined_data[Combined_data$Name %in% dup_name,]

# Any meaning from 'Miss', 'Mr', Master, Mrs?

misses <- Combined_data[str_detect(Combined_data$Name, 'Miss. '),]
tail(misses); misses[1:5,]
mrses <- Combined_data[str_detect(Combined_data$Name, 'Mrs. '), ]
mrses[1:6,]
# look at male to see any pattern
males <- Combined_data[Combined_data$Sex == 'male', ] 
males[1:5,]

# Create a title value by adding title variable

extractTitle <- function(Name) {
  Name <- as.character(Name)
  
  if (length(grep('Miss.', Name)) > 0) {
    return('Miss.')
  } else if (length(grep('Master.', Name)) > 0) {
    return('Master.')
  } else if (length(grep('Mrs.', Name)) > 0) {
    return('Mrs.')
  } else if (length(grep('Mr.', Name)) > 0) {
    return('Mr.')
  } else {
    return('Other')
  }
}


title <- NULL

for (i in 1:nrow(Combined_data)) {
  title <- c(title, extractTitle(Combined_data[i, 'Name']))
}

Combined_data$Title <- as.factor(title)

library(ggplot2)

ggplot(Combined_data[1:891,], aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_grid(~Pclass) +
  ggtitle('Pclass') +
  labs(fill = 'Survived') +
  xlab('Title') +
  ylab('Total Count')

prop.table(table(data_combined$Sex))

#Look at Sex
                                                                                                                                                                                                                                                                                                                                                                                              
ggplot(data_combined[1:891,], aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle('Pclass') +
  xlab('Sex') +
  ylab('Total Count') +
  labs(fill = 'Survived')

# Look at Age (histogram)
summary(data_combined[1:891, 'Age'])

summary(data_combined$Age)

ggplot(data = data_combined[1:891,], mapping = aes(x = (Age), fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Sex +Pclass) +
  xlab('Age') +
  ylab('Total Count')

# VALIDATE THE MASTER (MALE CHILDREN)
boys <- Combined_data[Combined_data$Title == 'Master.',]
summary(boys$Age)
misses <- Combined_data[Combined_data$Title == 'Miss.',]
summary(misses$Age)
# Examine Misses Further 
ggplot(misses[misses$Survived != 'None',], aes(x = Age, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass) +
  ggtitle('Age for Miss by Pclass') +
  xlab('Age') +
  ylab('Total Count') +
  theme(plot.title = element_text(hjust = 0.5))
  
?theme
misses_alone = misses[misses$SibSp == 0 & misses$Parch == 0,]
summary(misses_alone$Age)
length(which(misses_alone$Age <= 14.5))

# Look at Sibsp
summary(Combined_data$SibSp)

# treat sibsp as a factor
length(unique(Combined_data$SibSp))
max(data_combined$SibSp)
Combined_data$SibSp <- as.factor(Combined_data$SibSp)
ggplot(Combined_data[1:891,], aes(x = SibSp, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_bar() + 
  ggtitle('Plass and Title') +
  xlab('SibSp') +
  ylab('Total Count') +
  ylim(0, 300) +
  labs(fill = 'Survived')

# Take a look at Parch, treat Parch as a factor
length(unique(Combined_data$Parch))
max(data_combined$Parch)
Combined_data$Parch <- as.factor(Combined_data$Parch)
ggplot(Combined_data[1:891,], aes(x = Parch, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_bar() + 
  ggtitle('Plass and Title') +
  xlab('Parch') +
  ylab('Total Count') +
  ylim(0, 300) +
  labs(fill = 'Survived')

# Create a family size feature 
full_sibsp <- c(train$SibSp, test$SibSp)
full_parch <- c(train$Parch, test$Parch)
Combined_data$family_Size <- as.factor(full_sibsp + full_parch + 1)
# visuialise to see if it is predictive
ggplot(Combined_data[1:891,], aes(x = family_Size, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_bar() + 
  ggtitle('Plass and Title') +
  xlab('Family Size') +
  ylab('Total Count') +
  ylim(0, 300) +
  labs(fill = 'Survived')

# take a look at the ticket variable
str(Combined_data$Ticket)
# convert to character
Combined_data$Ticket <- as.character(Combined_data$Ticket)
Combined_data$Ticket[1:20]


# analyse the ticket data strcuture
ticket_first_character <- ifelse(Combined_data$Ticket == "", " ", substr(Combined_data$Ticket, 1, 1))
head(ticket_first_character)
unique(ticket_first_character)

# turn the ticket_first_character into a factor to see any patterns with ticket number
Combined_data$ticket_first_character <- as.factor(ticket_first_character)

library(ggplot2)
#look for pattern in this ticket_first_character 

ggplot(Combined_data[1:891,], aes(x = ticket_first_character, fill = Survived)) +
  geom_bar() +
  ggtitle('Survavability by ticket_first_character')  +
  xlab('ticket_first_character') +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = 'Survived') +
  theme(plot.title = element_text(hjust = 0.5))


# dive in with the ticket analyses

ggplot(Combined_data[1:891,], aes(x = ticket_first_character, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_bar() +
  ggtitle('Survavability by ticket_first_character')  +
  xlab('ticket_first_character by Pclass') +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = 'Survived') +
  theme(plot.title = element_text(hjust = 0.5))

# Check if the is a pattern when using Pclass and Title


ggplot(Combined_data[1:891,], aes(x = ticket_first_character, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
  geom_bar() +
  ggtitle('Survavability by ticket_first_character')  +
  xlab('ticket_first_character by Pclass and Title') +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = 'Survived') +
  theme(plot.title = element_text(hjust = 0.5))


# The no lot of signal in ticket charcaters

# NEXT UP THE FARES, titanic passengers paid
summary(Combined_data$Fare)
length(unique(Combined_data$Fare))

# The fare data looks skewed , validate
 ggplot(data = Combined_data, aes(x = Fare)) +
   geom_histogram(binwidth = 5) +
   ggtitle(' Combined Fare Distribution') +
   xlab('Fare') +
   ylab(' Total Count') +
   ylim(0, 200)

# Lets check to see if the fare has predictive power
ggplot(data = Combined_data[1:891,], aes(x = Fare, fill = Survived)) +
  facet_wrap(~Pclass + Title) +
   geom_histogram(binwidth = 5) +
   ggtitle('Fare by Pclass and Title') +
   xlab('Fare') +
   ylab(' Total Count') +
   ylim(0, 50) +
  labs(fill = 'Survived')

# No predictive power
# Analysis of the cabin variable
str(Combined_data$Cabin)

# Cabin really isnt a factor, make a string and the display first 100
Combined_data$Cabin <- as.character(Combined_data$Cabin)
Combined_data$Cabin[1:100]

# Replace empty combins with U
Combined_data[which(Combined_data$Cabin == ""), "Cabin"] <- "U"
Combined_data$Cabin[1:100]

# Take a look at just the first char as a factor
Cabin_first_chara <- as.factor(substr(Combined_data$Cabin, 1, 1))
str(Cabin_first_chara)
levels(Cabin_first_chara)
?substr


























































































































































