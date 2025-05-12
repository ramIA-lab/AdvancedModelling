############################
TEXTUAL DATA ANALYSIS WITH R (CA APPROACH)

########## LOAD FACTOMINER
install.packages("FactoMineR")
library(FactoMineR)

########## EXAMPLE 1: DEFINITON OF HEALTH
data(health)
?health
head(health)
str(health)
########## PERFORM CORRESPONDENCE ANALYSIS (CA)
?CA
res.ca<-CA(health[,1:115], graph=FALSE)
res.ca

########## EIGENVALUES
res.ca$eig

########## RESPONDENTS
names(res.ca$row)
res.ca$row$coord
res.ca$row$cos2
res.ca$row$contr
res.ca$row$inertia

########## WORDS
names(res.ca$col)
res.ca$col$coord
res.ca$col$cos2
res.ca$col$contr
res.ca$col$inertia


########## SUMMARY
summary(res.ca)

########## HIGHER CONTRIBUTIONS OF THE FREQUENCIES
res.ca$col$contr[order(apply(res.ca$col$contr[,1:2],1,sum),decreasing=TRUE)[1:10],1:2]
res.ca$col$contr[order(apply(res.ca$col$contr[,3:4],1,sum),decreasing=TRUE)[1:10],3:4]

########## CA PLOTS
?plot.CA
plot.CA(res.ca,invisible="row")
plot.CA(res.ca,invisible="row",autoLab="yes")

plot.CA(res.ca,invisible="col")
plot.CA(res.ca,invisible="col",autoLab="yes")


plot.CA(res.ca,invisible="row",axes=c(3,4),autoLab="yes")
plot.CA(res.ca,invisible="col",axes=c(3,4))

plot.CA(res.ca)
########## PERFORM CORRESPONDENCE ANALYSIS ON GENERALISED AGGREGATED LEXICAL TABLE (CA-GALT)
?CaGalt
res.cagalt<-CaGalt(Y=health[,1:115],X=health[,116:118],type="n")
res.cagalt

########## EIGENVALUES
res.cagalt$eig

########## RESPONDENTS
names(res.cagalt$ind)
res.cagalt$ind$coord
res.cagalt$ind$cos2

########## WORDS
names(res.cagalt$freq)
res.cagalt$freq$coord
res.cagalt$freq$cos2
res.cagalt$freq$contr

########## QUALITATIVE VARIABLES
names(res.cagalt$quali.var)
res.cagalt$quali.var$coord
res.cagalt$quali.var$cos2

########## ELLIPSES
names(res.cagalt$ellip)
res.cagalt$ellip$freq
res.cagalt$ellip$var

########## SUMMARY
summary(res.cagalt)

########## CA-GALT PLOTS
?plot.CaGalt
plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2))
plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2),select = "contrib 49")
plot.CaGalt(res.cagalt,choix="quali.var",axes=c(1,2))
plot(res.cagalt, choix = "quali.var", conf.ellip = TRUE, axes = c(1,2))
plot(res.cagalt, choix = "freq", cex = 1.5, col.freq = "darkgreen",select = "contrib 10")
par(mfrow=c(1,3))
plot.CaGalt(res.cagalt)
plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2))
plot(res.cagalt, choix = "quali.var", conf.ellip = TRUE, axes = c(1,2))
par(mfrow=c(1,1))


