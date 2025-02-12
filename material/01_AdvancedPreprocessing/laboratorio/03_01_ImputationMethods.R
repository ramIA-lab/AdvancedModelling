# ==============================================================================
#                      PREPROCESSING: Imputació de dades mancants
# Author(s):     Sergi Ramírez i Dante Conti
#                        IDEAI (c)
# Date:               08 Febrer 2025
# Description:   Aquest script permet la detecció de outliers de la nostra 
#                base de dades
#
# ==============================================================================
# Cargar librerias  ============================================================
library(tidyverse)  
library(missForest)
library(VIM)
library(pool)
library(mice, warn.conflicts = FALSE)
library(Hmisc)
library(mi)

# 1.1 Basic Imputation  ========================================================
#seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

# impute with mean value
iris.mis$imputed_age <- with(iris.mis, impute(Sepal.Length, mean))

# impute with random value
iris.mis$imputed_age2 <- with(iris.mis, impute(Sepal.Length, 'random'))

# similarly you can use min, max, median to impute missing value

# using argImpute
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                           Species, data = iris.mis, n.impute = 5)
impute_arg

# check imputed variable Sepal.Length
impute_arg$imputed$Sepal.Length

# 1.1.1 MI method --------------------------------------------------------------
# seed missing values ( 10% )
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

# imputing missing value with mi
mi_data <- mi(iris.mis, seed = 335)
summary(mi_data)

# 1.1.2 MEAN WITH TARGET VARS --------------------------------------------------
## define target vars
target = "Species"

## Define Na's in the dataset
data <- subset(iris, select = -c(get(target)))
data <- prodNA(data, noNA = 0.1)
data$Species <- iris[, target]

## define var to imputation
varImp = colnames(data)[which(!colnames(data) %in% target)]

# Create the means
means <- aggregate(data[, varImp], list(data[, target]), mean, na.rm = TRUE)

# Apply this table in the original data
for (c in varImp) {
  for (g in means[, "Group.1"]) {
    cond <- which(data[, target] == g)
    data[which(is.na(data[cond, c])), c] <- means[which(means[, "Group.1"] == g), c]
  }
}

# 1.2 Multiple Imputation by Chained Equations (MICE) ==========================
### https://amices.org/mice/
# load data
data <- iris

# Get summary
summary(iris)

#Generate 10% missing values at Random
iris.mis <- missForest::prodNA(iris, noNA = 0.1)

# Check missing values introduced in the data
summary(iris.mis)

# Remove categories vars
quiCat <- which(lapply(iris.mis, class) %in% c("character", "factor"))
categories <- names(iris.mis)[quiCat]
iris.mis <- subset(iris.mis, select = -c(get(categories)))
summary(iris.mis)

# show the missing data pattern
library(mice)
md.pattern(iris.mis)

# Look the NA's with VIM packages
mice_plot <- aggr(iris.mis, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# multiple impute the missing values
imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# inspect quality of imputations
stripplot(imputed_Data, Sepal.Width, pch = 19, xlab = "Imputation number")
imputed_Data$imp$Sepal.Width

# get complete data ( 2nd out of 5)
completeData <- complete(imputed_Data, 2)

# build predictive model
fit <- with(data = iris.mis, exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width)) 

# combine results of all 5 models
#summary(pool(fit))

# 1.3 missForest ===============================================================
# seed 10% missing values
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)

# impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis)

# check imputed values
iris.imp$ximp

# check imputation error
iris.imp$OOBerror
## NRMSE is normalized mean squared error. It is used to represent error derived 
## from imputing continuous values. 
## PFC (proportion of falsely classified) is used to represent error derived from 
## imputing categorical values.

# comparing actual data accuracy
iris.err <- mixError(iris.imp$ximp, iris.mis, iris)
iris.err

# ==============================================================================