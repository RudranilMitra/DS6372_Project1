# Load the libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(corrplot)
library(leaps)
library(car)
library(glmnet)

###################################################################################################################
# Change scientific notation to general notation
options(scipen = 999, digits = 4)

###################################################################################################################
# Clear Environment
rm(list = ls())


###################################################################################################################
# Set current working directory in which all the files are present
setwd('D:\\SMU_MSDS\\MSDS_6372_Applied_Statistics\\Project 1')

# load file WHO data file
life = read.csv('Life Expectancy Data.csv')

# Many countries are missing population, so instead of using imputation techniques we collected the data from
# World Bank repository.
missing_population = read.csv('Missing_Population.csv')

# Many countries are missing GDP per capita, so instead of using imputation techniques we collected the data
# from World Bank repository.
missing_gdp = read.csv('Missing_GDP.csv')

# Many countries are missing Healthcare percentage expenditure to GDP, so instead of using imputation techniques
# we collected the data from World Bank repository.
missing_exp = read.csv('Missing_Percent_Expenditure.csv')

###################################################################################################################
# Helper function to generate quick summary stats
# input_dataset -> dataset that we want to generate summary stats for
# rounding_factor -> how many digits to round to
# imputation_threshold_pct -> threshold to determine whether to impute a column or not
###################################################################################################################
generate_summary = function(input_dataset,
                            rounding_factor = 2,
                            imputation_threshold_pct = 10) {
  #
  colnames = names(input_dataset)
  total_rows = nrow(input_dataset)
  
  #
  summary_dataset = data.frame()
  
  #
  for (i in 1:length(colnames)) {
    #
    na_count = sum(is.na(input_dataset[colnames[i]]))
    percent_na = round((na_count / total_rows) * 100, rounding_factor)
    col_type = typeof(input_dataset[1, colnames[i]])
    #
    if (col_type != 'character') {
      mean_value = round(mean(data.matrix(input_dataset[colnames[i]]), na.rm =
                                T), rounding_factor)
      median_value = round(median(data.matrix(input_dataset[colnames[i]]), na.rm =
                                    T), rounding_factor)
      min_value = round(min(data.matrix(input_dataset[colnames[i]]), na.rm =
                              T), rounding_factor)
      max_value = round(max(data.matrix(input_dataset[colnames[i]]), na.rm =
                              T), rounding_factor)
      levels = "Not Applicable"
    } else{
      mean_value = -1
      median_value = -1
      min_value = -1
      max_value = -1
      levels = nrow(unique(input_dataset[colnames[i]]))
    }
    #
    if (col_type != 'character' &
        percent_na > 0 & percent_na < imputation_threshold_pct) {
      impute_flag = "Yes"
    } else{
      impute_flag = "No"
    }
    #
    summary_dataset = rbind(
      summary_dataset,
      c(
        colnames[i],
        col_type,
        na_count,
        percent_na,
        mean_value,
        median_value,
        min_value,
        max_value,
        levels,
        impute_flag
      )
    )
  }
  
  #
  names(summary_dataset) = c(
    "Column Name",
    "DataType",
    "NA Count",
    "NA %",
    "Mean",
    "Median",
    "Min",
    "Max",
    "Number Of Levels",
    "Impute Flag"
  )
  
  # return summary dataset
  return(summary_dataset)
}

##################################################################################################################
# Helper function to plot correlation plots for response vs individual predictors
# input_dataset -> dataset that we want to generate summary stats for
# predictors -> vector of predictor columns to include in the correlation plot
# rouding_factor -> how many digits to round to
# sequence_length -> smoothing factor for linear regression line
##################################################################################################################
generate_pair_plots = function(input_dataset,
                               predictors,
                               rounding_factor = 2,
                               sequence_length = 100) {
  #
  response = "Life Expectancy"
  #
  par(mfrow = c(5, 5))
  #
  for (i in 1:length((predictors))) {
    temp_pairplot_df = input_dataset %>% select(Life.expectancy, predictors[i])
    temp_pairplot_df = na.omit(temp_pairplot_df)
    #
    x_var = temp_pairplot_df[[2]]
    y_var = temp_pairplot_df[[1]]
    
    #
    names(temp_pairplot_df) = c("response", "predictor")
    #
    plot(x_var, y_var, ylab = response, xlab = predictors[i])
    cor_results = cor.test(x_var, y_var)
    mtext(paste(
      "Cor=",
      round(cor_results$estimate, rounding_factor),
      "p-value=",
      round(cor_results$p.value, rounding_factor),
      sep = " "
    ))
    #
    generated_xvars = seq(min(x_var), max(x_var), length.out = sequence_length)
    newdata = data.frame(predictor = generated_xvars)
    #
    lines(generated_xvars,
          predict(lm(
            response ~ predictor, temp_pairplot_df
          ), newdata),
          col = "blue",
          lwd = 2)
  }
}


###################################################################################################################
# Helper function to plot histograms with density plots
# input_dataset -> dataset that we want to generate summary stats for
# predictors -> vector of predictor columns to include in the correlation plot
###################################################################################################################
generate_hist_plots = function(input_dataset, predictors) {
  par(mfrow = c(5, 5))
  for (i in 1:length(predictors)) {
    temp_density_df = input_dataset %>% select(predictors[i])
    temp_density_df = na.omit(temp_density_df)
    density_function = density(temp_density_df[[1]])
    plot(density_function, main = predictors[i], xlab = "")
    polygon(density_function, col = "dark green", border = "blue")
  }
}

###################################################################################################################
# Generate summary of unclean dataset
###################################################################################################################
View(generate_summary(life))

###################################################################################################################
# Vector that contains the different predictors. This is an initial last that will serve as the baseline
# for data analysis.
###################################################################################################################
predictors = c(
  "Adult.Mortality",
  "infant.deaths",
  "Alcohol",
  "percentage.expenditure",
  "Hepatitis.B",
  "Measles",
  "BMI",
  "under.five.deaths",
  "Polio",
  "Total.expenditure",
  "Diphtheria",
  "HIV.AIDS",
  "GDP",
  "Population",
  "thinness..1.19.years",
  "thinness.5.9.years",
  "Income.composition.of.resources",
  "Schooling"
)


###################################################################################################################
# There are predictors that have NA values in the given dataset. The goal of the below code is to find
# the median value of a predictor by country. We will use these values to impute predictors that have NA
# for a given country
###################################################################################################################
median_predictors_by_country = life %>% group_by(Country) %>% summarise(
  median(Adult.Mortality, na.rm = T),
  median(Polio, na.rm = T),
  median(Diphtheria, na.rm = T),
  median(thinness.5.9.years, na.rm = T),
  median(thinness..1.19.years, na.rm = T),
  median(BMI, na.rm = T),
  median(Hepatitis.B, na.rm = T),
  median(Schooling, na.rm = T),
  median(Income.composition.of.resources, na.rm = T),
  median(Alcohol, na.rm = T),
  median(Total.expenditure, na.rm = T),
  .groups = "keep"
)

colnames(median_predictors_by_country) = c(
  "Country",
  "Adult.Mortality",
  "Polio",
  "Diphtheria",
  "thinness..1.19.years",
  "thinness.5.9.years",
  "BMI",
  "Hepatitis.B",
  "Schooling",
  "Income.composition.of.resources",
  "Alcohol",
  "Total.expenditure"
)

###################################################################################################################
# There are predictors that have NA values in the given dataset. The goal of the below code is to find
# the median value of a predictor overall. The reason why we are doing this in addition to finding median per country
# is because there are countries which have NA for all rows. In those scenarios we cannot find the median per country
# We will use these values after we have imputed by median values per country
###################################################################################################################
median_predictors_overall = life %>% summarise(
  median(Adult.Mortality, na.rm = T),
  median(Polio, na.rm = T),
  median(Diphtheria, na.rm = T),
  median(thinness.5.9.years, na.rm = T),
  median(thinness..1.19.years, na.rm = T),
  median(BMI, na.rm = T),
  median(Hepatitis.B, na.rm = T),
  median(Schooling, na.rm = T),
  median(Income.composition.of.resources, na.rm = T),
  median(Alcohol, na.rm = T),
  median(Total.expenditure, na.rm = T),
  .groups = "keep"
)

colnames(median_predictors_overall) = c(
  "Adult.Mortality",
  "Polio",
  "Diphtheria",
  "thinness..1.19.years",
  "thinness.5.9.years",
  "BMI",
  "Hepatitis.B",
  "Schooling",
  "Income.composition.of.resources",
  "Alcohol",
  "Total.expenditure"
)

###################################################################################################################
# Analysis for life expectancy
# Will drop the columns for life expectancy as this is a response variable. This
# will also drop 10 NA records in Adult Mortality
###################################################################################################################
life = subset(life,!is.na(Life.expectancy))


###################################################################################################################
# Check quantile values for infant deaths, Number of Infant Deaths per 1000 population
# We can see over here that there are records for which the infant deaths are greater than 1000, We dont think
# this is realistic therefore we suggest that any value greater than the 99% quantile value be capped by using the
# 99% quantile value.
###################################################################################################################
quantile(life$infant.deaths, probs = seq(0, 1, .01))
life = life %>% mutate(infant.deaths = replace(infant.deaths, infant.deaths >
                                                 496, 495.84))

###################################################################################################################
# Measles - number of reported cases per 1000 population
# We can see over here that there are records for which the value is greater than 1000, We dont think
# this is realistic therefore we suggest that any value greater than 1000 be capped at 1000
###################################################################################################################
life = life %>% mutate(Measles = replace(Measles, Measles > 1000, 1000))

###################################################################################################################
# Check quantile values for under five deaths
# Number of under-five deaths per 1000 population
# We can see over here that there are records for which the infant deaths are greater than 1000, We dont think
# this is realistic therefore we suggest that any value greater than the 99% quantile value be capped by using the
# 99% quantile value.
###################################################################################################################
quantile(life$under.five.deaths, probs = seq(0, 1, .01))
life = life %>% mutate(under.five.deaths = replace(under.five.deaths, under.five.deaths >
                                                     770, 769))

###################################################################################################################
# Check quantile values for under five deaths
# Deaths per 1000 live births HIV/AIDS (0-4 years)
# There is a big jump from the 99%
###################################################################################################################
quantile(life$HIV.AIDS, probs = seq(0, 1, .01))
life = life %>% mutate(HIV.AIDS = replace(HIV.AIDS, HIV.AIDS > 30, 29.338))


###################################################################################################################
# Percentage expenditure cannot be more than 100, so any value more than 100 we are capping those at 100
###################################################################################################################
life = life %>% mutate(percentage.expenditure = replace(percentage.expenditure, percentage.expenditure >
                                                          100, 100))


###################################################################################################################
# For rows that have Percentage expenditure set to NA we are setting those to 9.884
# which is the world average for Percentage expenditure on healthcare w.r.t GDP
# Data Source -> https://data.worldbank.org/indicator/SH.XPD.CHEX.GD.ZS
# Data Source -> https://databank.worldbank.org/reports.aspx?source=2&series=SH.XPD.CHEX.GD.ZS&country=
###################################################################################################################
quantile(life$percentage.expenditure, probs = seq(0, 1, .05))
life = life %>% mutate(percentage.expenditure = replace(percentage.expenditure, is.na(percentage.expenditure), 9.884))

###################################################################################################################
# Fill missing population data from world bank repo
# Data Source -> https://databank.worldbank.org/reports.aspx?source=2&series=SP.POP.TOTL&country=
###################################################################################################################
fill_missing_population = function(input_dataset, population_dataset) {
  for (i in 1:nrow(population_dataset)) {
    country = population_dataset[i, 1]
    year = population_dataset[i, 2]
    population = population_dataset[i, 3]
    input_dataset[(input_dataset$Country == country &
                     input_dataset$Year == year), 'Population'] = population
  }
  return(input_dataset)
}
life = fill_missing_population(life, missing_population)

###################################################################################################################
# Fill missing GDP per capita information from world bank repo
# Data Source -> https://databank.worldbank.org/reports.aspx?source=2&series=NY.GDP.PCAP.CD&country=
# Following countries will still have NA GDP after missing value imputation
# Democratic People's Republic of Korea
# Eritrea
# Sao Tome and Principe
# Somalia
# South Sudan
# It is better to drop the rows for the above countries
###################################################################################################################
fill_missing_gdp = function(input_dataset, gdp_dataset) {
  for (i in 1:nrow(gdp_dataset)) {
    country = gdp_dataset[i, 1]
    year = gdp_dataset[i, 2]
    gdp = gdp_dataset[i, 3]
    input_dataset[(input_dataset$Country == country &
                     input_dataset$Year == year), 'GDP'] = gdp
  }
  return(input_dataset)
}

life = fill_missing_gdp(life, missing_gdp)
life = subset(life,!is.na(GDP))
###################################################################################################################
# Correct values for countries that have percent.expenditure set to 0.
# Data Source -> https://databank.worldbank.org/reports.aspx?source=2&series=SH.XPD.CHEX.GD.ZS&country=
###################################################################################################################
fill_zero_exp = function(input_dataset, exp_dataset) {
  for (i in 1:nrow(exp_dataset)) {
    country = exp_dataset[i, 1]
    year = exp_dataset[i, 2]
    pe = exp_dataset[i, 3]
    input_dataset[(input_dataset$Country == country &
                     input_dataset$Year == year), 'percentage.expenditure'] = pe
  }
  return(input_dataset)
}
life = fill_zero_exp(life, missing_exp)
###################################################################################################################
# Below are the columns that we have to impute. The vector impute columns have the predictors that we
# want to impute. We will pass this vector into the function median_imputer_by_country so that
# NA values can be imputed. We will run median_imputer_overall so that any remaining NA values after
# median imputation by country are removed
###################################################################################################################
impute_columns = c(
  "Adult.Mortality",
  "Polio",
  "Diphtheria",
  "thinness..1.19.years",
  "thinness.5.9.years",
  "BMI",
  "Hepatitis.B",
  "Schooling",
  "Income.composition.of.resources",
  "Alcohol",
  "Total.expenditure"
)

median_imputer_by_country = function(input_dataset,
                                     impute_columns,
                                     median_predictors_by_country) {
  #
  for (i in 1:nrow(median_predictors_by_country)) {
    country = median_predictors_by_country$Country[i]
    for (j in 1:length(impute_columns)) {
      #
      impute_value = median_predictors_by_country %>% filter(Country == country) %>% pull(impute_columns[j])
      #
      input_dataset[input_dataset$Country == country, impute_columns[j]] =
        replace_na(input_dataset[input_dataset$Country == country, impute_columns[j]], impute_value)
      
      
    }
  }
  #
  return(input_dataset)
}

median_imputer_overall = function(input_dataset,
                                  impute_columns,
                                  median_predictors_overall) {
  for (i in 1:length(impute_columns)) {
    impute_value = median_predictors_overall %>% pull(impute_columns[i])
    input_dataset[, impute_columns[i]] = replace_na(input_dataset[, impute_columns[i]], impute_value)
  }
  return(input_dataset)
}

life = median_imputer_by_country(life, impute_columns, median_predictors_by_country)
life = median_imputer_overall(life, impute_columns, median_predictors_overall)


###################################################################################################################
# Imputation zero columns to 1 so that the model do not fail while log transformation these variables
###################################################################################################################
life[life$infant.deaths == 0, "infant.deaths"] = 1
life[life$Alcohol == 0, "Alcohol"] = 1
life[life$percentage.expenditure == 0, "percentage.expenditure"] = 1
life[life$Measles == 0, "Measles"] = 1
life[life$Income.composition.of.resources == 0, "Income.composition.of.resources"] = 1
life[life$Schooling == 0, "Schooling"] = 1

###################################################################################################################
# List correlations between predictors. The goal over here is to find predictors that are correlated so that
# we can filter them before building the model
###################################################################################################################
dev.off()
corrplot(
  cor(life[5:22]),
  type = "lower",
  order = "hclust",
  tl.col = "black",
  tl.srt = 65,
  method = "number"
)

###################################################################################################################
# Generate visualizations
###################################################################################################################
generate_hist_plots(life, predictors)
generate_pair_plots(life, predictors, 3, 250)

###################################################################################################################
# Drop columns not needed in model
###################################################################################################################
life.model = life[, -c(1:3)]

####################################### AUTOMATIC VARIABLE SELECTION###############################################
###################################################################################################################
# Create training and test datasets
###################################################################################################################
set.seed(1234)
data_length = dim(life.model)[1]
index <- sample(1:data_length, data_length * .7, replace = F)
train <- life.model[index, ]
test <- life.model[-index, ]
###################################################################################################################

###################################################################################################################
# Helper function to predict new results
# Source: ISLR
###################################################################################################################
predict.regsubsets = function (object , newdata , id , ...) {
  form = as.formula (object$call [[2]])
  mat = model.matrix(form , newdata)
  coefi = coef(object , id = id)
  xvars = names(coefi)
  mat[, xvars] %*% coefi
}

dev
par(mfrow=c(1,3))

###################################################################################################################
# Perform CV for forward selection(10 fold)
###################################################################################################################
k <- 10
predictors_size = 18

folds <- sample(1:k, nrow(life.model), replace = T)
cv.errors <-
  matrix(NA, k, predictors_size, dimnames = list(NULL, paste(1:predictors_size)))
for (j in 1:k) {
  ## Training set
  best.fit <-
    regsubsets(
      Life.expectancy ~ .,
      data = life.model[folds != j, ],
      nvmax = predictors_size,
      method = "forward" ## Selection Criteria
    )
  for (i in 1:predictors_size) {
    pred <- predict.regsubsets(best.fit, life.model[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((life.model$Life.expectancy[folds == j] - pred) ^ 2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
plot(
  mean.cv.errors,
  type = 'b',
  xlab = "Number of Predictors",
  ylab = "Cross Validation ASE",
  main = "Forward Selection"
)
index <- which(mean.cv.errors == min(mean.cv.errors))
points(index, mean.cv.errors[index], col = "red", pch = 10)
mtext(paste0('Min CV ASE = ',round(min(mean.cv.errors),2),sep=' '))

###################################################################################################################
# Perform CV for backward elimination(10 fold)
###################################################################################################################
k <- 10

folds <- sample(1:k, nrow(life.model), replace = T)
cv.errors <-
  matrix(NA, k, predictors_size, dimnames = list(NULL, paste(1:predictors_size)))
for (j in 1:k) {
  ## Training set
  best.fit <-
    regsubsets(
      Life.expectancy ~ .,
      data = life.model[folds != j, ],
      nvmax = predictors_size,
      method = "backward" ## Selection Criteria
    )
  for (i in 1:predictors_size) {
    pred <- predict.regsubsets(best.fit, life.model[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((life.model$Life.expectancy[folds == j] - pred) ^ 2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
plot(
  mean.cv.errors,
  type = 'b',
  xlab = "Number of Predictors",
  ylab = "Cross Validation ASE",
  main = "Backward Elimination"
)
index <- which(mean.cv.errors == min(mean.cv.errors))
points(index, mean.cv.errors[index], col = "red", pch = 10)
mtext(paste0('Min CV ASE = ',round(min(mean.cv.errors),2),sep=' '))

###################################################################################################################
# Perform CV for stepwise selection(10 fold)
###################################################################################################################
k <- 10

folds <- sample(1:k, nrow(life.model), replace = T)
cv.errors <-
  matrix(NA, k, predictors_size, dimnames = list(NULL, paste(1:predictors_size)))
for (j in 1:k) {
  ## Training set
  best.fit <-
    regsubsets(
      Life.expectancy ~ .,
      data = life.model[folds != j, ],
      nvmax = predictors_size,
      method = "seqrep" ## Selection Criteria
    )
  for (i in 1:predictors_size) {
    pred <- predict.regsubsets(best.fit, life.model[folds == j, ], id = i)
    cv.errors[j, i] <-
      mean((life.model$Life.expectancy[folds == j] - pred) ^ 2)
  }
}
mean.cv.errors <- apply(cv.errors, 2, mean)
plot(
  mean.cv.errors,
  type = 'b',
  xlab = "Number of Predictors",
  ylab = "Cross Validation ASE",
  main = "Stepwise Selection"
)
index <- which(mean.cv.errors == min(mean.cv.errors))
points(index, mean.cv.errors[index], col = "red", pch = 10)
mtext(paste0('Min CV ASE = ',round(min(mean.cv.errors),2),sep=' '))


###################################################################################################################
# Run forward selection, backward elimination and stepwise regression
###################################################################################################################
model.fwd = regsubsets(
  Life.expectancy ~ .,
  data = train,
  method = "forward",
  nvmax = 18,
  nbest = 1
)
model.back = regsubsets(
  Life.expectancy ~ .,
  data = train,
  method = "backward",
  nvmax = 18,
  nbest = 1
)
model.step = regsubsets(Life.expectancy ~ .,
                        data = train,
                        method = "seqrep",
                        nvmax = 18)
###################################################################################################################


###################################################################################################################
# Output
###################################################################################################################
model.fwd.results = data.frame(with(summary(model.fwd), data.frame(rsq, adjr2, cp, rss, outmat)))
model.back.results = data.frame(with(summary(model.back), data.frame(rsq, adjr2, cp, rss, outmat)))
model.step.results = data.frame(with(summary(model.step), data.frame(rsq, adjr2, cp, rss, outmat)))

model.fwd.results[16,1:4]
model.back.results[18,1:4]
model.step.results[18,1:4]

coef(model.fwd,16)
coef(model.back,18)
coef(model.step,18)
###################################################################################################################

###################################################################################################################
# Model from forward selection
###################################################################################################################
model.response <- "Life.expectancy"
model.fwd.predictors  <-   c(
  "Adult.Mortality",
  "infant.deaths",
  "Alcohol",
  "percentage.expenditure",
  "Hepatitis.B",
    "BMI",
  "under.five.deaths",
  "Polio",
  "Total.expenditure",
  "Diphtheria",
  "HIV.AIDS",
  "GDP",
  "Population",
  "thinness..1.19.years",
  "Income.composition.of.resources",
  "Schooling"
)

# This creates the appropriate formula string that is passed in the MLR function below
final.fwd.model.formula = as.formula(paste(model.response, paste(model.fwd.predictors, collapse =
                                                            " + "), sep = " ~ "))
final.fwd.model = lm(final.fwd.model.formula, data = life.model)

# Print the summary of the MLR model
summary(final.fwd.model)
par(mfrow = c(3, 3))
# Plot Residuals and other diagnostic plots
plot(final.fwd.model,which=c(1))
plot(final.fwd.model,which=c(2))
plot(final.fwd.model,which=c(3))
plot(final.fwd.model,which=c(4))
plot(final.fwd.model,which=c(5))
plot(final.fwd.model,which=c(6))
hist(final.fwd.model$residuals, breaks = 20,main="Histograms of Residuals")
plot(final.fwd.model$fitted.values, life.model$Life.expectancy)
###################################################################################################################

###################################################################################################################
# Model from backward elimination / stepwise selection
###################################################################################################################
model.response <- "Life.expectancy"
model.back.predictors  <-   c(
  "Adult.Mortality",
  "infant.deaths",
  "Alcohol",
  "percentage.expenditure",
  "Hepatitis.B",
  "Measles",
  "BMI",
  "under.five.deaths",
  "Polio",
  "Total.expenditure",
  "Diphtheria",
  "HIV.AIDS",
  "GDP",
  "Population",
  "thinness..1.19.years",
  "Income.composition.of.resources",
  "Schooling"
)
final.back.model.formula = as.formula(paste(model.response, paste(model.back.predictors, collapse =
                                                                   " + "), sep = " ~ "))
final.back.model = lm(final.fwd.model.formula, data = life.model)

# Print the summary of the MLR model
summary(final.back.model)
par(mfrow = c(3, 3))
# Plot Residuals and other diagnostic plots
plot(final.back.model,which=c(1))
plot(final.back.model,which=c(2))
plot(final.back.model,which=c(3))
plot(final.back.model,which=c(4))
plot(final.back.model,which=c(5))
plot(final.back.model,which=c(6))
hist(final.back.model$residuals, breaks = 20,main="Histograms of Residuals")
plot(final.back.model$fitted.values, life.model$Life.expectancy)
###################################################################################################################

############################################## MANUAL MODEL #######################################################
###################################################################################################################
# Variables for manual model with transformations. We will further apply automatic selection techniques to
# filter out non important predictors
###################################################################################################################
lm_response <- "Life.expectancy"
lm_predictors  <-   c(
  "Adult.Mortality",
  "I(Adult.Mortality^2)",
  "I(Adult.Mortality^3)",
  "log(infant.deaths)",
  "Alcohol",
  "percentage.expenditure",
  "Hepatitis.B",
  "log(Measles)",
  "BMI",
  "Polio",
  "Total.expenditure",
  "Diphtheria",
  "log(HIV.AIDS)",
  "log(GDP)",
  "log(Population)",
  "thinness..1.19.years",
  "Income.composition.of.resources",
  "Schooling"
)

# This creates the appropriate formula string that is passed in the MLR function below
final.model.formula = as.formula(paste(lm_response, paste(lm_predictors, collapse =
                                                            " + "), sep = " ~ "))
final.model = lm(final.model.formula, data = life.model)

# Print the summary of the MLR model
summary(final.model)
par(mfrow = c(3, 3))
# Plot Residuals and other diagnostic plots
plot(final.model,which=c(1))
plot(final.model,which=c(2))
plot(final.model,which=c(3))
plot(final.model,which=c(4))
plot(final.model,which=c(5))
plot(final.model,which=c(6))
hist(final.model$residuals, breaks = 20,main="Histograms of Residuals")
plot(final.model$fitted.values, life.model$Life.expectancy)

###################################################################################################################
# Find CV ASE of manual model
###################################################################################################################
# get the data from somewhere and specify number of folds
kfolds = 10
# generate array containing fold-number for each sample (row)
folds <- rep_len(1:kfolds, nrow(life.model))
# randomize dataset
folds <- sample(folds, nrow(life.model))

ase_holder = c()
# actual cross validation
for (k in 1:kfolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- life.model[-fold, ]
  data.test <- life.model[fold, ]
  
  # train and test your model with data.train and data.test
  cv_model = lm(final.model.formula, data = data.train)
  cv_prediction = predict(cv_model, newdata = data.test)
  ase = mean((data.test$Life.expectancy - cv_prediction) ^ 2)
  ase_holder[k] = ase
}

print(paste0("ASE of Manual Model using 10 Fold Cross Validation = ",round(mean(ase_holder),2)))

###################################################################################################################
# Using Lasso to find out ASE
###################################################################################################################
dev.off()
x = model.matrix(Life.expectancy ~ ., life.model)[, -1]
y = life$Life.expectancy

xtest <- model.matrix(Life.expectancy ~ ., life.model)[, -1]
ytest <- life.model$Life.expectancy

grid = 10 ^ seq(10, -2, length = 100)
lasso.mod = glmnet(x, y, alpha = 1, lambda = grid)

cv.out = cv.glmnet(x, y, alpha = 1) #alpha=1 performs LASSO
plot(cv.out)

bestlambda <-
  cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
lasso.pred = predict (lasso.mod , s = bestlambda , newx = xtest)

testMSE_LASSO <- mean((ytest - lasso.pred) ^ 2)
testMSE_LASSO

coef(lasso.mod, s = bestlambda)
###################################################################################################################

###################################################################################################################
# Plot Residuals vs Predictors plot to see if there are any patterns (MANUAL MODEL)
###################################################################################################################
par(mfrow = c(4, 5))
plot(life.model$Adult.Mortality, final.model$residuals)
plot(log(life.model$infant.deaths), final.model$residuals)
plot(life.model$Alcohol, final.model$residuals)
plot(life.model$percentage.expenditure, final.model$residuals)
plot(life.model$Hepatitis.B, final.model$residuals)
plot(log(life.model$Measles), final.model$residuals)
plot(life.model$BMI, final.model$residuals)
plot(life.model$Polio, final.model$residuals)
plot(life.model$Total.expenditure, final.model$residuals)
plot(life.model$Diphtheria, final.model$residuals)
plot(log(life.model$HIV.AIDS), final.model$residuals)
plot(log(life.model$GDP), final.model$residuals)
plot(log(life.model$Population), final.model$residuals)
plot(life.model$thinness..1.19.years, final.model$residuals)
plot(life.model$Income.composition.of.resources,
     final.model$residuals)
plot(life.model$Schooling, final.model$residuals)



