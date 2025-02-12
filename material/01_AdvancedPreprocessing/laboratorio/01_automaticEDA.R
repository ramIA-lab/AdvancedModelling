# ==============================================================================
#                      PREPROCESSING: Descriptiva Automàtica
# Author(s):     Sergi Ramírez i Dante Conti
#                        IDEAI (c)
# Date:               08 Febrer 2025
# Description:   Aquest script permet la descripció de la base de dades
#
# ==============================================================================
# Carreguem les llibreries =====================================================
library(visdat)
library(inspectdf)
library(skimr)
library(tidyverse)

# Carreguem les bases de dades =================================================
dades <- read.csv("./datos/valentine_dataset.csv")

# Paquete Skim  ================================================================
## Podem visualitzar un descriptiu de les dades 
skim(dades)

# Visualitzem exclusivament les variables numériques
skim(dades) %>% yank("numeric")

skim(dades) %>% yank("character")
# Paquete Vis_  ================================================================
## Busquem per a variables numériques o categóriques si hi ha NA's
vis_dat(dades)

## Visualitzem percentatges de NA's en les variables
vis_miss(dades)

## Generem la matriu de correlacions
dades %>% select(where(is.numeric)) %>% 
  vis_cor()

## Podem visualitzar condicionants de les dades. En aquest cas, mirem si tenim mes de 
## 2 clases
vis_expect(dades, ~ .x > 2)

# Inspectdf ====================================================================
## Tipus de dades
inspect_types(dades) %>% show_plot()

## Utilització de la memoria
inspect_mem(dades) %>% show_plot()

## Comprovem NA's
data_price_dummy <- dades %>% 
  mutate(price_dummy = if_else(Income > 10000, "High", "Low"))

inspect_na(data_price_dummy %>% filter(price_dummy == "High"),
           data_price_dummy %>% filter(price_dummy == "Low")) %>%
  show_plot()

## Comprovem la distribució de les variables 
inspect_num(dades) %>% show_plot()

## check categorical variable distribution
inspect_imb(dades) %>% show_plot()

## check two categorical
inspect_imb(data_price_dummy %>% filter(price_dummy == "High"),
            data_price_dummy %>% filter(price_dummy == "Low")) %>%
  show_plot() + theme(legend.position = "none")

## similiar to inspect_imb, but for all levels
inspect_cat(dades) %>% show_plot()

inspect_cor(dades) %>% show_plot()

# dataReporter (antiguo dataMaid) ==============================================
library("dataReporter")
dataReporter::makeDataReport(dades, output = "html", file = "/Users/ramitjans/Downloads/Preprocessing/report.Rmd")
dataReporter::makeCodebook(data = dades, file = "/Users/ramitjans/Downloads/Preprocessing/codebook.Rmd")

# DataExplorer =================================================================
library(DataExplorer)
plot_str(dades)
introduce(dades)
plot_intro(dades)

plot_missing(dades)

plot_bar(dades)
plot_bar(dades, with = "Age")
plot_bar(dades, by = "Gender")
plot_histogram(dades)
plot_correlation(na.omit(dades), maxcat = 5L)

# SmartEDA =================================================================
library("SmartEDA")
## Overview of the data
ExpData(data = dades,type = 1)

## structure of the data    
ExpData(data = dades,type = 2)

# Summary of numerical variables................................................
## Summary statistics by – overall
ExpNumStat(dades,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)

## Summary statistics by – overall with correlation 
ExpNumStat(dades,by="A",gp="Age",Qnt=seq(0,1,0.1),MesofShape=1,Outlier=TRUE,round=2)

## Summary statistics by – category
ExpNumStat(dades,by="GA",gp="Gender",Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2)

# Graphical representation of all numeric features .............................
## Generate Boxplot by category
ExpNumViz(dades, target="Gender",type=2,nlim=25)
## Generate Density plot
ExpNumViz(dades,target=NULL,type=3,nlim=25)
## Generate Scatter plot
ExpNumViz(dades,target="Age",type=3,nlim=25)

# Summary of Categorical variables .............................................
## Frequency or custom tables for categorical variables
ExpCTable(dades,Target=NULL,margin=1,clim=10,nlim=5,round=2,bin=NULL,per=T)
## Summary statistics of categorical variables
ExpCatStat(dades,Target="Valentine_Date",result = "Stat",clim=10,nlim=5,Pclass="Yes")
## Inforamtion value and Odds value
ExpCatStat(dades,Target="Valentine_Date",result = "IV",clim=10,nlim=5,Pclass="Yes")

# Graphical representation of all categorical variables ........................
## column chart
ExpCatViz(dades,target="Valentine_Date",fname=NULL,clim=10,col=NULL,margin=2,Page = c(2,1),sample=2)
## Stacked bar graph
ExpCatViz(dades, target="Valentine_Date",fname=NULL,clim=10,col=NULL,margin=2,Page = c(2,1),sample=2)

# Variable importance based on Information value ...............................
ExpCatStat(dades,Target="Valentine_Date",result = "Stat",clim=10,nlim=5,bins=10,Pclass="Yes",plot=TRUE,top=10,Round=2)
 
# Bibliografia =================================================================
### https://www.analyticsvidhya.com/blog/2022/10/three-r-libraries-for-automated-eda/
### https://cran.r-project.org/web/packages/dlookr/vignettes/EDA.html
### https://cran.r-project.org/web/packages/DataExplorer/vignettes/dataexplorer-intro.html
### https://daya6489.github.io/SmartEDA/

