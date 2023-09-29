library(tidyverse)
library(Hmisc)
library(reshape2)
library(egg)
library(corrplot) #correlation matrix and plots
library(caret) #regression
library(fastDummies)

#load data
train <- read.csv("./Data/train.csv")
test <- read.csv("./Data/test.csv")

###### Exploratory Data Analysis----
#look at data structure
str(train)

summary(train)

# Frequency of independent variables
train %>% 
  select(Survived) %>% 
  group_by(Survived) %>% 
  summarise(n = n()) %>% 
  mutate(Freq = paste0(round(100 * n/sum(n), 1), "%"))

train %>% 
  summarise_all(list(~sum(is.na(.))))

# Recode 0 values in Fare to NAs
train$Fare[train$Fare == 0] = NA

# Replace any blank cells and those containing only spaces with NAs
train <- train %>% 
  mutate(across(everything(), ~gsub("^[[:space:]]*$", NA, .)))

# check NAs again
train %>% 
  summarise_all(list(~sum(is.na(.))))

# Drop Cabin column due to too man missing values
train <- train %>% select(!Cabin)

# Check whether numerical variables are continuous or 
train %>% 
  select(where(is.numeric)) %>% 
  sapply(., function(x) length(unique(x)))

# Split variables into categorical and numerical
train_continuous <- train %>% 
  select(c(Fare, Age, Survived))

train_categorical <- train %>% 
  select(PassengerId, Pclass, SibSp, Parch, Embarked, Sex, Survived)


# Plot distributions of continuous variables as histograms/boxplots
train_continuous %>% 
  melt() %>% 
  ggplot(aes(x=value))+
  geom_histogram()

# Distribution of numeric variables as bar plots
train %>% 
  select(where(is.numeric))%>%
  hist.data.frame(.)

train %>% filter(Age != is.na(Age)) %>% ggplot()

# We can either filter missing values or use imputation.
# First, we will look for relations between the independent (outcome) and
# the dependent variables (features) by plotting.
# we plot discrete and ordinal variables as bar or box plots.
# Continuous variables are plotted as 

#plot age as stacked barplot with suvived as categorical variable
plot1 <- ggplot(data = data, aes(x = Age, fill = factor(Survived))) +
  geom_bar(stat="count", width = 1, position = "stack")+
  theme(legend.position = "none")

plot2 <- ggplot(data = data, aes(x = Age, fill = factor(Survived))) +
  geom_boxplot()+
  theme(legend.position = "none")

#plot sex and suvival as bar plot
plot3 <- ggplot(data = data, aes(x = Sex, fill = factor(Survived))) +
  geom_bar(stat="count", width = 0.5, position = "dodge")+
  theme(legend.position = "none")

#plot passenger class and survival as bar plot
plot4 <- ggplot(data = data, aes(x = Pclass, fill = factor(Survived))) +
  geom_bar(stat="count", width = 0.5, position = "dodge")+
  theme(legend.position = "none")

# density plot of surivval vs. fare
plot5 <- ggplot(data = data, aes(x = Fare, fill = factor(Survived))) +
  geom_bar(stat="density", width = 0.5)+
  theme(legend.position = "none")

# To make the output nice, we will combine the plots into one figure.
grid.arrange(plot1, plot5, plot3, plot4, plot2)
grid.arrange(grobs = list(plot1, plot2, plot3, plot4, plot5),
             widhts = c(1,1,1),
             layout_matrix = rbind(c(1,2,3),
                                   c(4,5,NA)),
             common.legend = TRUE, theme(legend.position = "bottom"))

# library(cowplot)
# pgrid <- plot_grid(plot1 + theme(legend.position = "none"),
#           plot5 + theme(legend.position = "none"),
#           plot2 + theme(legend.position = "none"),
#           plot3 + theme(legend.position = "none"),
#           plot4 + theme(legend.position = "none"),
#           align = "vh",
#           nrow = 2,
#           ncol = 3)
# plot_legend <- get_legend(pgrid + theme(legend.position = "bottom"))
# comp_plot <- plot_grid(pgrid, plot_legend, rel_widths = )
# comp_plot

# extract titles from Name column
data <- data %>% 
  mutate(Name = gsub(" ", "", Name),
         Title = str_extract(Name, ",([a-zA-Z]*)"),
         Title = gsub(",", "", Title)) %>% 
  select(!Name)
# try: ,(.*?)\\.

data %>% filter(Title %in%  c("Mr", "Mrs", "Miss", "Master")) %>% 
group_by(Title) %>% 
  summarise(n      = n(), 
            mean   = mean(Age, na.rm = TRUE), 
            median = median(Age, na.rm = TRUE), 
            min    = min(Age, na.rm = TRUE), 
            max    = max(Age, na.rm = TRUE), 
            sd     = sd(Age, na.rm = TRUE), 
            surv   = sum(Survived))

data <- data %>% 
  mutate(Sex_m = case_when(
    Title %in% c("Capt", "Col", "Don", "Dr", "Jonkheer", "Major", "Master", "Mr", "Rev", "Sir") ~ 1,
    .default =  0))

hist(data$Age)
ggplot(data = data %>% group_by(Sex), aes(x = Age, fill = factor(Sex), alpha = 0.5))+
  geom_histogram()
# create data frame for age imputation  
  filter(Title == c("Mr", "Mrs", "Miss", "Master"),
         Age != is.na(Age)) %>% 
  group_by(Title) %>% 
  mutate(Age_mean = mean(Age),
         Age_sd = sd(Age),
         Age_zscore = (Age-Age_mean)/Age_sd)

data %>% 
  mutate(Title = if_else(!(Title %in% c("Mr", "Mrs", "Miss", "Master")), "Other_title", Title)) %>% 
  distinct(Title)
# Next, we will check for correlation between variables
## How to encode categorical variables?
# create dummy variables (0 or 1) for n-1 levels
  
corr_matrix <- cor(as.matrix(data_age_groups %>% select(is.numeric)), method = "spearman")
corrplot(corr_matrix, method = "number")

data_imputed <- data %>%
  group_by(Title) %>% 
    fill(Age) %>% 
  ungroup()

# corr btw age and suvival
# Alter in Quantile teilen

# Create age groups 
data_age_groups <- data %>% 
  filter(Age != is.na(Age)) %>% 
  mutate(Age_group = case_when(
    Age <= 15            ~ 1,
    between(Age, 16, 49) ~ 2,
    Age >= 50            ~ 3
  ))



# Dummies erstellen
# Correlation

###### Check assumptions for logistic regression

# 1) Independent variable is binary.
# 2) Observations are independent, i.e. not repeated measures of same individuals
# 3) No multicolinearity.
# 4) No extreme outliers
# 5) Linear relationship between the explanatory variables and logit of repsonse var
# 6) Sample size is sufficiently large.


# How to check these assumptions?

# 1) How many categories does the independent variable contain?
  unique(data[c("Survived")])
# 2) All observations are individuals. However, some passengers embarked with their children and/or spouses.
#    We will need to check for this relationship. See also 3)
# 3) We will check for multicolinearity by plotting a correlation matrix.
# 4) We can check for outliers by z-scoring.
# 5) ?
# 6) Rule of thumb: 10 observations of least frequent outcome/probability of least frequent outcome

  
# Missing value imputation.
library(mice)

###### Continue with feature engineering ######