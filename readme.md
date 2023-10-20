Titanic
================
Michal Rackiewicz
2023-09-29

### Load packages and data

``` r
library(tidyverse)
library(Hmisc)
library(corrplot)
library(ggpol)
library(gridExtra)
library(rlang)
library(wesanderson)
library(knitr)
```

``` r
options(digits = 2,
        warn = -1,
        scipen = 999)
```

``` r
train <- read_csv("./Data/train.csv", col_types = "nffcfnnncnf")
test <- read_csv("./Data/test.csv", col_types = "nffcfnnncnf")
```

## Exploratory data analysis

First we will look at the data structures and summary statistics for
each variable.

``` r
str(train)
```

    ## spc_tbl_ [891 × 12] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ PassengerId: num [1:891] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : Factor w/ 2 levels "0","1": 1 2 2 2 1 1 1 1 2 2 ...
    ##  $ Pclass     : Factor w/ 3 levels "3","1","2": 1 2 1 2 1 1 2 1 1 3 ...
    ##  $ Name       : chr [1:891] "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : Factor w/ 2 levels "male","female": 1 2 2 2 1 1 1 1 2 2 ...
    ##  $ Age        : num [1:891] 22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : num [1:891] 1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : num [1:891] 0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr [1:891] "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num [1:891] 7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : Factor w/ 147 levels "C85","C123","E46",..: NA 1 NA 2 NA NA 3 NA NA NA ...
    ##  $ Embarked   : chr [1:891] "S" "C" "S" "S" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   PassengerId = col_number(),
    ##   ..   Survived = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
    ##   ..   Pclass = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
    ##   ..   Name = col_character(),
    ##   ..   Sex = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
    ##   ..   Age = col_number(),
    ##   ..   SibSp = col_number(),
    ##   ..   Parch = col_number(),
    ##   ..   Ticket = col_character(),
    ##   ..   Fare = col_number(),
    ##   ..   Cabin = col_factor(levels = NULL, ordered = FALSE, include_na = FALSE),
    ##   ..   Embarked = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
summary(train) %>% 
  kable()
```

|     | PassengerId | Survived | Pclass | Name             | Sex        | Age        | SibSp       | Parch       | Ticket           | Fare        | Cabin          | Embarked         |
|:----|:------------|:---------|:-------|:-----------------|:-----------|:-----------|:------------|:------------|:-----------------|:------------|:---------------|:-----------------|
|     | Min. : 1    | 0:549    | 3:491  | Length:891       | male :577  | Min. : 0   | Min. :0.0   | Min. :0.0   | Length:891       | Min. : 0    | G6 : 4         | Length:891       |
|     | 1st Qu.:224 | 1:342    | 1:216  | Class :character | female:314 | 1st Qu.:20 | 1st Qu.:0.0 | 1st Qu.:0.0 | Class :character | 1st Qu.: 8  | C23 C25 C27: 4 | Class :character |
|     | Median :446 | NA       | 2:184  | Mode :character  | NA         | Median :28 | Median :0.0 | Median :0.0 | Mode :character  | Median : 14 | B96 B98 : 4    | Mode :character  |
|     | Mean :446   | NA       | NA     | NA               | NA         | Mean :30   | Mean :0.5   | Mean :0.4   | NA               | Mean : 32   | F33 : 3        | NA               |
|     | 3rd Qu.:668 | NA       | NA     | NA               | NA         | 3rd Qu.:38 | 3rd Qu.:1.0 | 3rd Qu.:0.0 | NA               | 3rd Qu.: 31 | E101 : 3       | NA               |
|     | Max. :891   | NA       | NA     | NA               | NA         | Max. :80   | Max. :8.0   | Max. :6.0   | NA               | Max. :512   | (Other) :186   | NA               |
|     | NA          | NA       | NA     | NA               | NA         | NA’s :177  | NA          | NA          | NA               | NA          | NA’s :687      | NA               |

Let us also check the distribution of the dependent variable. By doing
this we get an idea of how balanced the data set is and whether we have
to do something to balance groups before modeling.

``` r
train %>% 
  select(Survived) %>% 
  group_by(Survived) %>% 
  summarise(Count = n()) %>% 
  mutate(Freq = paste0(round(100 * Count/sum(Count), 1), "%")) %>% 
  kable()
```

| Survived | Count | Freq  |
|:---------|------:|:------|
| 0        |   549 | 61.6% |
| 1        |   342 | 38.4% |

We also want to check for missing values.

``` r
train %>% 
  summarise_all(list(~sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing") %>% 
  arrange(desc(Missing)) %>% 
  kable("simple")
```

| Variable    | Missing |
|:------------|--------:|
| Cabin       |     687 |
| Age         |     177 |
| Fare        |      15 |
| Embarked    |       2 |
| PassengerId |       0 |
| Survived    |       0 |
| Pclass      |       0 |
| Name        |       0 |
| Sex         |       0 |
| SibSp       |       0 |
| Parch       |       0 |
| Ticket      |       0 |

We will impute missing values later on. For now, we want to proceed with
data visualization.

## Data visualization

We will divide the variables into numeric and categorical and visualize
each group.

``` r
numeric_vars <- train %>% 
  select(Age, Fare, SibSp, Parch, PassengerId) %>% 
  names()

categorical_vars <- train %>% 
  select(Survived, Pclass, Sex, Embarked) %>% 
  names()
```

#### Numeric variables

![](readme_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Quick glance at the total numbers of passengers travelling with their
families.

| SibSp | Count |
|------:|------:|
|     0 |   608 |
|     1 |   209 |
|     2 |    28 |
|     3 |    16 |
|     4 |    18 |
|     5 |     5 |
|     8 |     7 |

| Parch | Count |
|------:|------:|
|     0 |   678 |
|     1 |   118 |
|     2 |    80 |
|     3 |     5 |
|     4 |     4 |
|     5 |     5 |
|     6 |     1 |

#### Categorical variables

![](readme_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

First, we will plot a correlation map to get an idea of the
relationships between the variables.

![](readme_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

PassengerID shows no correlations with any other variable. It is evenly
distributed across passenger class and sex - the two strongest
correlations with the target variable.

![](readme_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

![](readme_files/figure-gfm/unnamed-chunk-19-1.png)<!-- --> We will
remove it as it holds no predictive value.

![](readme_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

![](readme_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

It is not apparent from the correlation map that age has any influence
on survival. We do know however that children (and women) were more
likely to be saved. From this information we can already infer that the
relationship between survival and age is not linear as the likelihood of
survival does not increase with age. Let us take a closer look at the
distribution of survival rates across age for each sex.

![](readme_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

Let us look at the age distribution for both sexes in each passenger
class.

![](readme_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

![](readme_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

We can infer that females - in general - are more likely to survive. We
also see a large proportion of deceased female passengers in the 3rd
class, although absolute numbers are roughly equal between all three
classes.

Taken together this indicates that passenger class is a good predictor
of survival for females. For males the proportion of survivors is
smaller irrespective of class. Male passengers in first class were much
more likely to survive than in other classes.

Distribution of survivors based on the port of embarkation.

![](readme_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

It is apparent that the amount of survivors among the passengers
embarking in Cherbourg is relatively higher than in Queenstown or
Southhampton. From the correlation plot we know that this variable is
correlated with passenger class.

![](readme_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

We see that among the passengers embarking in Cherbourg, the majority
belonged to class 1, while the vast majority of passengers embarking in
Queensland (with the lowest proportion of survivors) belonged to the 3rd
class.

The ticket column contains numeric and mixed entries. We will
investigate whether there are any differences in the ticket types,
i.e. numeric vs. non-numeric, between different starting integers,
different strings of the non-numeric, etc. First we will separate
numeric and non-numeric types and group the ticket types based on
strings and first integers.

``` r
train <- train %>%
  # transform values to numeric. if not possible, it will default to NA
  mutate(Numeric_ticket = !is.na(as.numeric(Ticket)),
         # create categorical values based on evaluation of numeric_ticket.
         Ticket_group = case_when(
           #  extract string if non-numeric
         Numeric_ticket == FALSE~ str_extract(Ticket, "^[^\\s]+"),
         # extract first integer if numeric
         Numeric_ticket == TRUE ~ str_extract(Ticket, "^\\d")
         ),
         Ticket_group = gsub("\\.", "", Ticket_group)
         )
```

``` r
train %>% 
  count(Survived, Numeric_ticket, name = "Count") %>% 
  group_by(Numeric_ticket) %>% 
  mutate(Percentage = round(Count*100/sum(Count))) %>% 
  arrange(Numeric_ticket)
```

    ## # A tibble: 4 × 4
    ## # Groups:   Numeric_ticket [2]
    ##   Survived Numeric_ticket Count Percentage
    ##   <fct>    <lgl>          <int>      <dbl>
    ## 1 0        FALSE            142         62
    ## 2 1        FALSE             88         38
    ## 3 0        TRUE             407         62
    ## 4 1        TRUE             254         38

There is no difference in survival rate between numerical and
non-numerical tickets.

We have also divided each class further.

``` r
train %>% 
  
  distinct(Ticket_group)
```

    ## # A tibble: 43 × 1
    ##    Ticket_group
    ##    <chr>       
    ##  1 A/5         
    ##  2 PC          
    ##  3 STON/O2     
    ##  4 1           
    ##  5 3           
    ##  6 2           
    ##  7 PP          
    ##  8 CA          
    ##  9 7           
    ## 10 SC/Paris    
    ## # ℹ 33 more rows

### Imputation of missing values

Let’s take a quick look at the variables with missing values. But first
we will remove the Cabin and PassengerId columns.

``` r
train_cln <- train %>% select(!c(Cabin, PassengerId))
```

| Variable | Missing |
|:---------|--------:|
| Age      |     177 |
| Fare     |      15 |
| Embarked |       2 |

For Embarked we will impute the mode. Since there is no mode function in
base R, we will create our own.

``` r
impute_mode <- function(df, var, na.rm = FALSE){
  var_mode <- names(which.max(table(df[[var]])))
  return(var_mode)
}
```

We can compare the missing values before and after imputation.

``` r
train_cln %>% 
  count(Embarked, name = "Count") %>% 
  kable("simple")
```

| Embarked | Count |
|:---------|------:|
| C        |   168 |
| Q        |    77 |
| S        |   644 |
| NA       |     2 |

``` r
train_cln %>% 
  mutate(Embarked = case_when(
    is.na(Embarked) ~ impute_mode(train_cln, "Embarked"),
    TRUE ~ as.character(Embarked)
  )) %>% 
  count(Embarked, name = "Count") %>% 
  kable("simple")
```

| Embarked | Count |
|:---------|------:|
| C        |   168 |
| Q        |    77 |
| S        |   646 |

``` r
train_imputed <- train_cln %>% 
  mutate(Embarked = case_when(
    is.na(Embarked) ~ impute_mode(train_cln, "Embarked"),
    TRUE ~ as.character(Embarked)
  ))
```

For the imputation of fare we can look again at the correlation matrix.
Passenger class and fare show the greatest correlation across all
variables, followed by Parch/SibSp.
![](readme_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

![](readme_files/figure-gfm/unnamed-chunk-38-1.png)<!-- --> Let us look
at these variables for the missing Fare values.

``` r
train %>% 
  select(Fare, Pclass, SibSp, Parch) %>% 
  filter(is.na(Fare)) %>% 
  kable("simple")
```

|    Fare | Pclass    |   SibSp |                                                Parch |
|--------:|:----------|--------:|-----------------------------------------------------:|
|      NA | 3         |       0 |                                                    0 |
|      NA | 1         |       0 |                                                    0 |
|      NA | 3         |       0 |                                                    0 |
|      NA | 2         |       0 |                                                    0 |
|      NA | 3         |       0 |                                                    0 |
|      NA | 2         |       0 |                                                    0 |
|      NA | 2         |       0 |                                                    0 |
|      NA | 2         |       0 |                                                    0 |
|      NA | 3         |       0 |                                                    0 |
|      NA | 1         |       0 |                                                    0 |
|      NA | 2         |       0 |                                                    0 |
|      NA | 2         |       0 |                                                    0 |
|      NA | 1         |       0 |                                                    0 |
|      NA | 1         |       0 |                                                    0 |
|      NA | 1         |       0 |                                                    0 |
| Since b | oth SibSp | and Par | ch = 0, we will impute based on the passenger class. |

``` r
train_imputed %>% 
  filter(!is.na(Fare)) %>% 
  group_by(Pclass) %>% 
  summarise(Count = n(),
            Mean_fare = round(mean(Fare), 1), 
            Median_fare = median(Fare)) %>% 
  kable("simple")
```

| Pclass | Count | Mean_fare | Median_fare |
|:-------|------:|----------:|------------:|
| 1      |   211 |        86 |          61 |
| 2      |   178 |        21 |          15 |
| 3      |   487 |        13 |           8 |

From the differences between mean and median as well as the box plot
above we

### EDA summary

Quick review of the actions we have taken: - Remove columns Cabin,
Ticket and PassengerId - Impute missing values in - Embarked: mode -
Age: mean of class + sex + family (SibSp/Parch) - Fare: mean of
passenger class + family (SibSp) - Remove highly correlated variables
(Fare and Passenger class)

\`\`\`{r} train \<- train %\>% select(!Cabin)
