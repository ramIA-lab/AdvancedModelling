# ==============================================================================
#                      PREPROCESSING: Imputació de dades mancants
# Author(s):     Sergi Ramírez i Dante Conti
#                        IDEAI (c)
# Date:               17 Febrer 2025
# Description:   Aquest script permet la imputació dels missings de la nostra 
#                base de dades
#
# ==============================================================================
# Carreguem les llibreries =====================================================
list.of.packages = c("ggplot2", "plotly", "DMwR2", "dplyr", "tidyverse", "naniar",
                     "mi", "visdata", "Hmisc", "mice", "pool", "VIM", "missForest") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages);gc()



# Generate data with NA's ======================================================
iris.mis <- missForest::prodNA(iris, noNA = 0.1)

## Other form to create a missings in the dataframe
## iris.mis <- mi::create.missing(iris, pct.mis = 10)

# Little Test ==================================================================
naniar::mcar_test(iris.mis)

## If the test p-value is less than 0 this means that the data does not have 
## completely randomly generated missing

# Descriptive NA's patterns in a databases =====================================
## exploring missingness relationships
library(visdat)
vis_dat(airquality);
vis_dat(iris.mis)
vis_miss(airquality);
vis_miss(iris.mis)

ggplot(airquality, aes(x = Solar.R,y = Ozone)) + 
  geom_point()
ggplot(airquality, aes(x = Solar.R,  y = Ozone)) + 
  geom_miss_point()

ggplot(airquality, aes(x = Solar.R, y = Ozone)) + 
  geom_miss_point() + 
  facet_wrap(~Month)

ggplot(airquality, aes(x = Solar.R, y = Ozone)) + 
  geom_miss_point() + 
  facet_wrap(~Month) + 
  theme_dark()

## Visualising missings in variables
gg_miss_var(airquality) + labs(y = "Look at all the missing ones")

## Detect NA in Dataframe
aq_shadow <- bind_shadow(airquality)

## Print the graph with diference in to NA's and not NA
airquality %>%
  bind_shadow() %>%
  group_by(Ozone_NA) %>%
  summarise_at(.vars = "Solar.R",
               .funs = c("mean", "sd", "var", "min", "max"),
               na.rm = TRUE)

ggplot(aq_shadow,
       aes(x = Temp,
           colour = Ozone_NA)) + 
  geom_density()

## Extract statistics with NAs in Data Frame
prop_miss_case(airquality)
pct_miss_case(airquality)
miss_case_summary(airquality)
miss_case_table(airquality)
prop_miss_var(airquality)
pct_miss_var(airquality)
miss_var_summary(airquality)
miss_var_table(airquality)

# 1.1 Basic Imputation  ========================================================
# impute with mean value
iris.mis[, "imputed_Sepal.Length"] <- with(iris.mis, impute(Sepal.Length, mean))

# impute with random value
iris.mis[, "imputed_Sepal.Length2"] <- with(iris.mis, impute(Sepal.Length, 'random'))
# similarly you can use min, max, median to impute missing value

# Represented the distribution to real and imputation variables
df_long <- iris.mis %>%
  select(Sepal.Length, imputed_Sepal.Length, imputed_Sepal.Length2) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Crear el gráfico de densidad con ggplot2
ggplot(df_long, aes(x = Valor, fill = Variable)) +
  geom_density(alpha = 0.3) +  # Transparencia para mejor visualización
  labs(title = "Densidad de las tres variables",
       x = "Valor",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red", "green"))

iris.mis[, c("imputed_Sepal.Length", "imputed_Sepal.Length2")] <- NULL
# ------------------------------------------------------------------------------
# using argImpute
impute_arg <- aregImpute(~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width +
                           Species, data = iris.mis, n.impute = 5)
impute_arg

# check imputed variable Sepal.Length
impute_arg$imputed$Sepal.Length

# Calculate the mean of the 5 simulations 
imputed_Sepal.Length <- rowMeans(impute_arg$imputed$Sepal.Length)
new_var_imputed <- iris$Sepal.Length
new_var_imputed[as.numeric(names(imputed_Sepal.Length))] <- imputed_Sepal.Length

## Looking a diference with two imputation 
newBD <- data.frame(real = iris[, "Sepal.Length"], imputed = new_var_imputed)
df_long <- newBD %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

ggplot(df_long, aes(x = Valor, fill = Variable)) +
  geom_density(alpha = 0.3) +  # Transparencia para mejor visualización
  labs(title = "Densidad de las tres variables",
       x = "Valor",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

# 1.1.1 Multiple Iterative Regression Imputation (MI method) -------------------
## imputing missing value with mi
mi_data <- mi(iris.mis, seed = 335)

## Look the results to the imputation 
summary(mi_data)
plot(mi_data)
par(ask = FALSE)

## Review the iterations data
mi_data@data

# 1.1.2 MEAN WITH TARGET VARS --------------------------------------------------
## define target vars
target = "Species"

## Define Na's in the dataset
data <- subset(iris, select = -c(get(target)))
data <- prodNA(data, noNA = 0.1)
data$Species <- iris[, target]

# Definir variables a imputar (excluyendo la variable target)
varImp <- colnames(data)[which(!colnames(data) %in% target)]

# Calcular las medias por grupo
means <- aggregate(data[, varImp], list(data[, target]), mean, na.rm = TRUE)

# Imputar valores faltantes
for (c in varImp) {
  for (g in means[, "Group.1"]) {
    cond <- data[, target] == g  # Condición booleana en vez de which()
    na_index <- is.na(data[, c]) & cond  # Seleccionar NA dentro del grupo
    
    # Asignar valores imputados
    data[na_index, c] <- means[means[, "Group.1"] == g, c]
  }
}

summary(data)

## Looking a diference with two imputation 
iris[, "Tipo"] <- "original"
data[, "Tipo"] <- "imputed"

# merge two dataframes
data_long <- bind_rows(iris, data)
cols_numeric <- names(data_long)[sapply(data_long, is.numeric) & names(data_long) != "Tipo"]

# Convert a large data
data_long <- data_long %>%
  pivot_longer(cols = all_of(cols_numeric), names_to = "Variable", values_to = "Valor")

# Crear el gráfico con ggplot
ggplot(data_long, aes(x = Valor, fill = Tipo)) +
  geom_density(alpha = 0.3) +  # Transparencia para comparación
  facet_wrap(~Variable, scales = "free") +  # Un gráfico por variable
  labs(title = "Comparación de Distribuciones: Original vs Imputado",
       x = "Valor",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

## Remove the  tipology var
iris.mis[, "Tipo"] <- NULL

# 1.2 Multiple Imputation by Chained Equations (MICE) ==========================
### https://amices.org/mice/
# Remove categories vars
quiCat <- which(lapply(iris.mis, class) %in% c("character", "factor"))
categories <- names(iris.mis)[quiCat]
iris.mis2 <- subset(iris.mis, select = -c(get(categories)))
summary(iris.mis2)

# show the missing data pattern
par(mfrow = c(1, 1))
md.pattern(iris.mis2, rotate.names = TRUE)

# Look the NA's with VIM packages
mice_plot <- aggr(iris.mis2, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(iris.mis), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

# multiple impute the missing values
imputed_Data <- mice(iris.mis2, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# inspect quality of imputations
stripplot(imputed_Data, Sepal.Width, pch = 19, xlab = "Imputation number")
imputed_Data$imp$Sepal.Width

# get complete data ( 2nd out of 5)
completeData <- mice::complete(imputed_Data, action = "long")

## Exercices: 
## Deploy the multiple plot to compare te imputation and not imputation with all
## numeric vars in dataframe

# 1.3 KNN  =====================================================================
tipos <- sapply(iris.mis, class)
varNum <- names(tipos)[which(tipos %in% c("numeric", "integer"))]
data_knn_imputation <- knnImputation(iris.mis[, varNum], k = 1)
summary(data_knn_imputation)

## Looking a diference with two imputation 
newBD <- data.frame(real = iris[, "Sepal.Length"], imputed = data_knn_imputation[, "Sepal.Length"])
df_long <- newBD %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

ggplot(df_long, aes(x = Valor, fill = Variable)) +
  geom_density(alpha = 0.3) +  # Transparencia para mejor visualización
  labs(title = "Densidad de las tres variables",
       x = "Valor",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

## Exercices: 
## Deploy the multiple plot to compare the imputation and not imputation with all
## numeric vars in dataframe

# 1.4 missForest ===============================================================
# impute missing values, using all parameters as default values
iris.imp <- missForest(iris.mis, variablewise = T, verbose = T) 

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

## Looking a diference with two imputation 
newBD <- data.frame(real = iris[, "Sepal.Length"], imputed =  iris.imp$ximp[, "Sepal.Length"])
df_long <- newBD %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

ggplot(df_long, aes(x = Valor, fill = Variable)) +
  geom_density(alpha = 0.3) +  # Transparencia para mejor visualización
  labs(title = "Densidad de las tres variables",
       x = "Valor",
       y = "Densidad") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))

## Exercises: 
## Deploy the multiple plot to compare te imputation and not imputation with all
## numeric vars in dataframe

# ==============================================================================
# Extras: 
# When you are dealing with missing values, you might want to replace values with 
# a missing values (NA). This is useful in cases when you know the origin of the 
# data and can be certain which values should be missing. For example, you might 
# know that all values of “N/A”, “N A”, and “Not Available”, or -99, or -1 are 
# supposed to be missing.
#
# naniar provides functions to specifically work on this type of problem using 
# the function replace_with_na. This function is the compliment to tidyr::replace_na, 
# which replaces an NA value with a specified value, whereas naniar::replace_with_na 
# replaces a value with an NA:
#  
# tidyr::replace_na: Missing values turns into a value (NA –> -99)
# naniar::replace_with_na: Value becomes a missing value (-99 –> NA)

# ==============================================================================
# Bibliografia: 
# http://naniar.njtierney.com/articles/replace-with-na.html
# https://cran.r-project.org/web/packages/visdat/vignettes/using_visdat.html
# https://cran.r-project.org/web/packages/DMwR2/DMwR2.pdf
# https://ltorgo.github.io/DMwR2/RintroDM.html#data_pre-processing
# https://www.rdocumentation.org/packages/DMwR/versions/0.4.1/topics/knnImputation
