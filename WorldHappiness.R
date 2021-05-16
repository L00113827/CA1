library(readr) # reading dataset
library(dplyr, warn.conflicts = F) # for Data manipulation
library(ggplot2) # for data visualization
library(ggthemes) # for some nice themes and automatic colors
library(psych) # for summary, etc
library(olsrr)  # ols stepwise regression
library(ggcorrplot)# for correlation plot
library(rcompanion) # to plot histogram with density plot on real values
library(nortest)   # Statistical test for the Normality of the dataset


data <- readr::read_csv("finaldata.csv")
data <- data %>% select(-X1)
head(data)  # top six rows

dim(data)   # rows and columns in dataset
# Our data is consist of 1949 observations/rows and 11 variables/columns.
colnames(data)# %>% cat() # what are the variables we have
unique(data$'Country name')%>% length() # How many number of countries in out dataset
range(data$year) # time period of datsets
# we have data of **166 countries**, time period between **2005 to 2020**
complete.cases(data) %>% table()
is.na(data) %>% table()
colSums(is.na(data))

### We have total `r 241+1708` cases, in which *241 cases have missing values*. Total number of missing values are 373.


## How many missing values in each Variables ?
colSums(is.na(data))
## General Summary/Description of dataset
## data %>% select(-c(`Country name`, year)) %>% 
data %>% select(-c(`Country name`, year)) %>% 
    psych::describe(na.rm = T, type = 2, fast = T) %>% 
    select(-se)

## Top 10 countries in GDP per capita (Taking the average of 15 Years starting from 2005 to 2020)
(data %>% select(`Country name`, `Log GDP per capita`) %>%
        na.omit() %>%  # Removing the country have NA data in any 15 year
        group_by(`Country name`) %>% 
        summarise(gdp= mean(`Log GDP per capita`)) %>% 
        arrange(-gdp) %>% .[1:10,] -> top10gdp)

## Bottom 10 countries in GDP per capita (Taking the average of 15 Years starting from 2005 to 2020)

(data %>% select(`Country name`, `Log GDP per capita`) %>%
        na.omit() %>%  # Removing the country have NA data in any 15 year
        group_by(`Country name`) %>% 
        summarise(gdp= mean(`Log GDP per capita`)) %>% 
        arrange(gdp) %>% .[1:10,] -> bottom10gdp)

## Top 10 countries in Happiness (taking mean average of 15 year)

(data %>% select(`Country name`, `Life Ladder`) %>%
        group_by(`Country name`) %>% 
        summarise(happiness= mean(`Life Ladder`)) %>% 
        arrange(-happiness) %>% .[1:10,] -> top10happiness)


## Bottop  10 countries in Happiness (taking mean average of 15 year)

(data %>% select(`Country name`, `Life Ladder`) %>% 
        group_by(`Country name`) %>% 
        summarise(happiness= mean(`Life Ladder`)) %>% 
        arrange(happiness) %>% .[1:10,] -> bottom10hapiness)


# Comparing Ranks


# **On compare the ranks of top 10 countries in happiness and GDP, 2 countries remain in common list**
# 
inner_join(top10gdp, top10happiness, by=  "Country name")

# **Similarly on comparing the ranks of bottom 10 countries in happiness and GDP, 3 Countries remain in common list**

inner_join(bottom10hapiness, bottom10gdp, by= "Country name")


# Data preparation for Hypothesis testing

data %>% na.omit %>%            # Removing all rows with Missing values in dataset
    group_by(`Country name`) %>% 
    summarise(happy= mean(`Life Ladder`),   # Taking mean of all variables Country wise; it is mean value of 15 years
              gdp= mean(`Log GDP per capita`),
              soci.s= mean(`Social support`),
              hlexp.b= mean(`Healthy life expectancy at birth`),
              freedom= mean(`Freedom to make life choices`),
              generosity= mean(Generosity),
              p.corruption= mean(`Perceptions of corruption`),
              p.affect= mean(`Positive affect`),
              n.affect= mean(`Negative affect`)) %>% 
    ungroup() %>%
    select(-`Country name`)-> dataset     # Dataset is ready for Regression Analysis
dataset %>% head()

# Data Preperation (Checking Outliars, Normality, Removing Outliars, etc)



# Boxplots

par(mfrow= c(3,3))
for(i in c(1:9)) {
    boxplot(dataset[,i], col= i+2)
    title(main= names(dataset[i]))
}


# **only perception of corruption have extreme values**
    

# Histogram with Normal Plot


par(mfrow=c(3,3))
for(i in c(1:9)) {
    rcompanion::plotNormalHistogram(dataset[,i],
                                    col=i+2,
                                    xlab= "",
                                    main= names(dataset[i]))
}
# From Histograms all data seems Normality distributed, except of perception of corruption and generosity


# Data Cleaning we are removing "Extreme values" form dataset

par(mfrow=c(1,2))
boxplot(dataset$p.corruption, main= "p.corruption", col= F)$out %>% range()
boxplot(dataset$generosity, main= "generosity", col= F)$out %>% range()


# Total 15 values in p.corruption, and 1 value form p.corruption are outliar**, Removing them

dim(dataset)
dataset %>% filter(p.corruption>0.47650000, generosity<.3)-> dataset

# After removing outliar, checking again for extreame value and Normality

par(mfrow= c(3,3))
for(i in c(1:9)) {
    boxplot(dataset[,i], col= i+2)
    title(main= names(dataset[i]))
}

par(mfrow=c(3,3))
for(i in c(1:9)) {
    rcompanion::plotNormalHistogram(dataset[,i],
                                    col=i+2,
                                    xlab= "",
                                    main= names(dataset[i]))
}

# QQPLOT (another graphical way to check normality)
### Note: **Don't know Why for() loop is NOT working for qqnorm, so have to write too much codes"**

par(mfrow= c(3,3))
qqnorm(dataset$happy, col= "blue", pch= 19, main= "happy")
qqline(dataset$happy, col= "red", lwd=3)
qqnorm(dataset$gdp, col= "blue", pch= 19, main= "gdp")
qqline(dataset$gdp, col= "red", lwd=3)
qqnorm(dataset$soci.s, col= "blue", pch= 19, main= "soci.s")
qqline(dataset$soci.s, col= "red", lwd=3)
qqnorm(dataset$hlexp.b, col= "blue", pch= 19, main= "hlexp.b")
qqline(dataset$hlexp.b, col= "red", lwd=3)
qqnorm(dataset$freedom, col= "blue", pch= 19, main= "freedom")
qqline(dataset$freedom, col= "red", lwd=3)
qqnorm(dataset$generosity, col= "blue", pch= 19, main= "generosity")
qqline(dataset$generosity, col= "red", lwd= 3)
qqnorm(dataset$p.corruption, col= "blue", pch= 19, main= "p.corruption")
qqline(dataset$p.corruption, col= "red", lwd=3, main= "")
qqnorm(dataset$p.affect, col= "blue", pch= 19, main= "p.affect")
qqline(dataset$p.affect, col= "red", lwd=3)
qqnorm(dataset$n.affect, col= "blue", pch= 19, main= "n.affect")
qqline(dataset$n.affect, col= "red", lwd=3)



# From the **graph of qqnorm()** it seems Social support (soci.s), and perception of corruption is not Normal...  But we can ignore them because majority of variables are following normal distribution in this dataset.

# Descriptive Statistics


## Pearson's Correlation Coefficient Table

cor(dataset) %>% round(2)

describe(dataset) %>% select(mean, sd)


## Correlation Plot

ggcorrplot::ggcorrplot(cor(dataset),
                       method = "circle", type = "upper", ggtheme = theme_foundation(), legend.title = "Correlaiton\nCoefficient", outline.color = "black")


# Multiple Liniar Regression



# We are taking happy (Happiness) as a criterian variable, and happy gdp, soci.s, hlexp.b, freedom, p.corruption, p.affect, and n.affect as Predictor variable.

full_model <- lm(happy~., data= dataset)
summary(full_model)


# 4 predictor variables are insignificant in explaining criterion variable**, to get most effective variable I am using Stepwise regression

# OLS Stepwise Regression

olsrr::ols_step_both_p(full_model, pent= .05, prem= .10)
# 5 best predictor variables are separated using ols regression

### Making step Model
lm(happy~gdp+p.affect+hlexp.b+soci.s+generosity, data= dataset) -> final_model
summary(final_model)

par(mfrow= c(2,2), lwd= 2.2)
plot(final_model, col="red")


# From the graphs our model is perfectly fine, No issue of Heteroscedasticity (unequal variance) regression model is linear, mean of residual is zero.



# Our model passes all the assumption of linear regression.

# F value
null_model <- lm(happy~1, data= dataset)
anova(null_model, final_model)

# overall model is significant**


# Standardize beta value
lm.beta::lm.beta(final_model) %>% print(digits= 2)




######################### END #############################