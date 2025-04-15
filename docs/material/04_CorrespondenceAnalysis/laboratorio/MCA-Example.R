###### MCA Analysis #############

library("FactoMineR")
library("factoextra")
library("corrplot")

########## DATASET --> POISON
###  The data used here refer to a survey carried out on a sample of children of primary school
###  who suffered from food poisoning. They were asked about their symptoms and about what they ate.


data(poison)
dim(poison)
str(poison)
summary(poison)

###### Subsetting features for MCA Analysis
poison.active <- poison[1:55, 5:15]

#### IMPORTANT -->These graphs can be used to identify variable categories with a very low frequency.
####              These types of variables can distort the analysis and should be removed. 

for (i in 1:ncol(poison.active)) {
  plot(poison.active[,i], main=colnames(poison.active)[i],
       ylab = "Count", col="steelblue", las = 2)
}

#### MCA Function
help(MCA)
res.mca <- MCA(poison.active, method="Indicator", graph = FALSE)
#res.mca2 <- MCA(poison.active, method="Burt", graph = FALSE)
print(res.mca)

##### Results of MCA function --> Visualization and interpretation

####STEP 1 --Eingenvalues and Variance

eig.val <- get_eigenvalue(res.mca)
eig.val
## Check number of dimensions and select the best number to keep
### For method="Indicator" use elbow method or dimensions where eigenvalues>1/p 
### For method="Burt" use elbow method o where accumulated inertia is higher than 75 or 80%
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))

##Decide the number of dimensions to keep for the analysis
## Logical table (method="Indicator")---> detect eigenvalues > 1/p, here 1/11 = 0.0909
## Burt --> 3 dimensions

## However, for the analysis in this script, 2 dimensions will be used.(You
## should complete the analysis by considering the 3rd dimensions as well)

####STEP 2 --- Checking Individuals & Features in 2 dimensions
##### This step summarizes the results in only one graph.

#fviz_mca_biplot(res.mca,repel = TRUE, gtheme = theme_minimal())

####STEP 3 ---Graph of variables - Results
var <- get_mca_var(res.mca)
var

### Pseudo-Correlation between variables and principal dimensions
### To visualize the Pseudo-correlation between variables and MCA principal dimensions, use this:

fviz_mca_var(res.mca, choice = "mca.cor",repel = TRUE,ggtheme = theme_minimal())

### Coordinates of categories
fviz_mca_var(res.mca, repel = TRUE, ggtheme = theme_minimal())

### Quality of representation of variable categories
### Please, note that The two dimensions 1 and 2 are sufficient to retain 46%
### of the total inertia (variation) contained in the data. 

fviz_mca_var(res.mca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE, ggtheme = theme_minimal())
corrplot(var$cos2, is.corr=FALSE)
fviz_cos2(res.mca, choice = "var", axes = 1:2)

###Note that, variable categories Fish_n, Fish_y, Icecream_n and Icecream_y are not very well represented by the first two dimensions.
### This implies that the position of the corresponding points on the
###scatter plot should be interpreted with some caution. A higher dimensional solution is probably necessary. 

###Contribution of variable categories to the dimensions
##### The variable categories with the larger value, contribute the most to the definition of the dimensions. Variable categories that contribute the most to Dim.1
##### and Dim.2 are the most important in explaining the variability in the data set.  

fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
fviz_mca_var(res.mca, col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE,ggtheme = theme_minimal())
corrplot(var$contrib, is.corr=FALSE)

### Before moving to Step 4. decide the latency of the dimensions
####STEP 4 ---Graph of Individuals --Results
ind <- get_mca_ind(res.mca)
ind

### Plots: quality and contribution
fviz_mca_ind(res.mca, col.ind = "cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE,ggtheme = theme_minimal())
fviz_mca_ind(res.mca, col.ind = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE,ggtheme = theme_minimal())
fviz_contrib(res.mca, choice = "ind", axes = 1:2, top = 20)

###Color individuals by groups
########## it’s possible to color the individuals using any of the qualitative variables in the initial data table (poison),
########## here, feature--> "Vomiting"
fviz_mca_ind(res.mca, label = "none", habillage = "Vomiting", palette = c("#00AFBB", "#E7B800"),addEllipses = TRUE, ellipse.type = "confidence", ggtheme = theme_minimal())

### Multiple Graphs

fviz_ellipses(res.mca, c("Vomiting", "Fever"), geom = "point")
fviz_ellipses(res.mca, 1:4, geom = "point")

#### STEP 5 --> Dimension description
res.desc <- dimdesc(res.mca, axes = c(1,2))
res.desc
# Description of dimension 1
res.desc[[1]]
# Description of dimension 2
res.desc[[2]]

######ADDITIONAL TOPICS
#### (1) supplementary continuous variables (quanti.sup = 1:2, columns 1 and 2 corresponding to the columns age and time, respectively) 
#### (2) supplementary qualitative variables (quali.sup = 3:4, corresponding to the columns Sick and Sex, respectively). This factor variables are used to color
#### individuals by groups.
#### (3) The data doesn’t contain supplementary individuals. However, for demonstration, we’ll use the individuals 53:55 as supplementary individuals.
#### **** IMPORTANT **** 
#### Supplementary variables and individuals are not used for the determination of the principal dimensions. Their coordinates are predicted using only
#### the information provided
#### by the performed multiple correspondence analysis on active variables/individuals.  

res.mca <- MCA(poison, ind.sup = 53:55, quanti.sup = 1:2, quali.sup = 3:4,  graph=FALSE)

# Supplementary qualitative variable categories
res.mca$quali.sup
# Supplementary quantitative variables
res.mca$quanti
# Supplementary individuals
res.mca$ind.sup

fviz_mca_biplot(res.mca, repel = TRUE,ggtheme = theme_minimal())

fviz_mca_var(res.mca, choice = "mca.cor", repel = TRUE) ### Correlation between variables (active & supplementary) and dimensions
fviz_mca_var(res.mca, repel = TRUE,ggtheme= theme_minimal())
fviz_mca_var(res.mca, choice = "quanti.sup",ggtheme = theme_minimal())
fviz_mca_ind(res.mca, label = "ind.sup", ggtheme = theme_minimal()) ###Supplementary Individuals
# top 5 contributing individuals and variable categories
fviz_mca_biplot(res.mca, select.ind = list(contrib = 5),select.var = list(contrib = 5),ggtheme = theme_minimal())

######SUMMARY
res.mca <- MCA(poison, ind.sup = 53:55, quanti.sup = 1:2, quali.sup = 3:4,  graph=TRUE)

####IMPORTANT CHECK WITH:
####          ******* BURT TABLE ********##RESULTS ARE SIMILAR (EQUIVALENTS)


