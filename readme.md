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

![](readme_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

We will remove it as it holds no predictive value.

``` r
train %>%
  group_by(Survived, Sex) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  mutate(percentage = round(n*100/sum(n), 2),
         label = paste0(percentage,"%"),
         Survived = case_match(Survived,
                               "0" ~ "deceased",
                               "1" ~ "survived")) %>% 
  ggplot(aes(x = Sex,
             y = n,
             fill = Sex,
             label = label)) +
  geom_col() +
  facet_grid(~Survived) +
  labs(title = "Survival ratios for each sex",
       y = "Passenger count",
       x = "Sex",
       fill = "Sex") +
  theme_minimal()+
  scale_fill_manual(values = wes_palette("GrandBudapest1")) +
  theme(legend.position = "none")
```

![](readme_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
train %>% 
  group_by(Pclass, Survived) %>% 
  summarise(n = n(), .groupes = "drop") %>% 
  mutate(percentage = round(n*100/sum(n), 2), 
         label = paste0(percentage,"%"),
         Survived = case_match(Survived,
                              "0" ~ "deceased",
                              "1" ~ "survived")) %>% 
  ggplot(aes(x = Pclass,
             y = n,
             fill = Survived,
             label = label)) +
    geom_col() +
    labs(title = "Survival across classes",
         y = "Passenger count",
         x = "Passenger class",
         fill = "Survival status") +
  geom_label(position = position_stack(0.5),
             show.legend = FALSE) +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("GrandBudapest1"))
```

    ## `summarise()` has grouped output by 'Pclass'. You can override using the
    ## `.groups` argument.

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

``` r
options(tibble.print_max = 12, tibble.print_min = 4)

train %>% 
  mutate(Survived = case_match(Survived,
                              "0" ~ "deceased",
                              "1" ~ "survived")) %>% 
  group_by(Parch, Survived) %>% 
  filter(!is.na(Age)) %>% 
  summarise(Mean_age = round(mean(Age), 1),
            Median_age = median(Age),
            .groups = "drop") %>% 
  kable("simple", caption = "Mean and median age across levels of parents/children and survival status")
```

| Parch | Survived | Mean_age | Median_age |
|------:|:---------|---------:|-----------:|
|     0 | deceased |       32 |         29 |
|     0 | survived |       32 |         30 |
|     1 | deceased |       27 |         26 |
|     1 | survived |       23 |         22 |
|     2 | deceased |       17 |         11 |
|     2 | survived |       17 |         18 |
|     3 | deceased |       32 |         32 |
|     3 | survived |       34 |         24 |
|     4 | deceased |       44 |         42 |
|     5 | deceased |       40 |         39 |
|     5 | survived |       38 |         38 |
|     6 | deceased |       43 |         43 |

Mean and median age across levels of parents/children and survival
status

### Imputation of missing values

Let’s take a quick look at the variables with missing values. But first
we will remove the Cabin and PassengerId columns.

``` r
train_cln <- train %>% select(!c(Cabin, PassengerId, Ticket))
```

``` r
train_cln %>% 
  summarise_all(list(~sum(is.na(.)))) %>% 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Missing") %>% 
  arrange(desc(Missing)) %>%
  filter(Missing > 0) %>% 
  kable("simple")
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
  count(Embarked) %>% 
  kable("simple")
```

| Embarked |   n |
|:---------|----:|
| C        | 168 |
| Q        |  77 |
| S        | 644 |
| NA       |   2 |

``` r
train_cln %>% 
  mutate(Embarked = case_when(
    is.na(Embarked) ~ impute_mode(train_cln, "Embarked"),
    TRUE ~ as.character(Embarked)
  )) %>% 
  count(Embarked) %>% 
  kable("simple")
```

| Embarked |   n |
|:---------|----:|
| C        | 168 |
| Q        |  77 |
| S        | 646 |

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
![](readme_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
train %>% 
  filter(!is.na(Fare)) %>% 
  ggplot(aes(x = factor(SibSp),
             y = Fare,
             fill = factor(SibSp))) +
  geom_boxplot() +
  labs(title = "Fares for passengers travelling with family",
       y = "Fare",
       x = "Siblings/Spouse") +
  theme_minimal() +
  scale_fill_manual(values = wes_palette("GrandBudapest1", 8, type = "continuous"))
```

![](readme_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->

``` r
train %>% 
  group_by(SibSp) %>% 
  filter(!is.na(Fare)) %>% 
  summarise(Fare_mean = mean(Fare),
            Fare_median = median(Fare)) %>% 
  kable("simple")
```

| SibSp | Fare_mean | Fare_median |
|------:|----------:|------------:|
|     0 |        26 |           9 |
|     1 |        44 |          26 |
|     2 |        51 |          24 |
|     3 |        68 |          25 |
|     4 |        31 |          31 |
|     5 |        46 |          46 |
|     8 |        69 |          69 |

``` r
train_imputed %>% 
  filter(!is.na(Fare)) %>% 
  group_by(Pclass) %>% 
  summarise(Mean_fare = mean(Fare),
            Median_fare = median(Fare))
```

    ## # A tibble: 3 × 3
    ##   Pclass Mean_fare Median_fare
    ##   <fct>      <dbl>       <dbl>
    ## 1 1           85.7          61
    ## 2 2           21.1          15
    ## 3 3           13.3           8

``` r
train_imputed %>% 
  filter(is.na(Fare))
```

    ## # A tibble: 15 × 9
    ##   Survived Pclass Name                    Sex     Age SibSp Parch  Fare Embarked
    ##   <fct>    <fct>  <chr>                   <fct> <int> <int> <int> <int> <chr>   
    ## 1 0        3      "Leonard, Mr. Lionel"   male     36     0     0    NA S       
    ## 2 0        1      "Harrison, Mr. William" male     40     0     0    NA S       
    ## 3 1        3      "Tornquist, Mr. Willia… male     25     0     0    NA S       
    ## 4 0        2      "Parkes, Mr. Francis \… male     NA     0     0    NA S       
    ## # ℹ 11 more rows

### EDA summary

Quick review of the actions we have taken: - Remove columns Cabin,
Ticket and PassengerId - Impute missing values in - Embarked: mode -
Age: mean of class + sex + family (SibSp/Parch) - Fare: mean of
passenger class + age + family (SibSp/Parch) - Remove highly correlated
variables (Fare and Passenger class)

We will drop the following columns: PassengerId - no predictive value
Cabin - too many missing values

\`\`\`{r} train \<- train %\>% select(!Cabin)
