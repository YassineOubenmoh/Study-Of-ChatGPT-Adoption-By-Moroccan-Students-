
#Theme Of The project : The adoption of ChatGPT by students
#Population : GI and GIND First Year And Second Year
#Size of the sample : 31
#Type of study : Confirmatory study
options(warn = -1)
#To get rid Of warnings

#1.1 : Import data :

library(readxl)
data <- read_excel("C:/ChatGPT Project/chatgpt.xlsx")
View(data)

#Pre-Processing :
#1.2 : Codification :

if(!is.factor(data$Sexe)){
  data$Sexe <- as.factor(data$Sexe)
}
levels(data$Sexe) = c("Homme" , "Femme")

if(!is.factor(data$Filière)){
  data$Filière <- as.factor(data$Filière)
}
levels(data$Filière) = c("GI" , "GIND")

if(!is.factor(data$`Année d'étude`)){
  data$`Année d'étude`<- as.factor(data$`Année d'étude`)
}
levels(data$`Année d'étude`) = c("1ère année cycle d'ingénieur",
                                 "2ème année cycle d'ingénieur")

if(!is.numeric(data$Age)){
  data$Age <- as.integer(data$Age)
}


#1.3 : Cleaning the data
#1.3.1 : Outliers
#


boxplot(data$Age,
        ylab = "Age",
        main = "Boite à moustache de la population selon l'age",
        col = "#04A777",
        sub = "Donnees : Adoption de la technologie ChatGPT",
        notch = TRUE
)
outliers=boxplot.stats(data$Age)$out
for (out in outliers) {
  for (i in 1:length(data$Age)) {
    if(data$Age[i]==out && !is.na(data$Age[i])){
      data$Age[i]=NA
    }
  }
}


pro=sum(is.na(data$Age))/length(data$Age)


#This is the proportion of NA values in our sample <=> No na values

#Incase we had NA values here's how we will treat the columns
# 3 Cases : Add ,Estimate ,Delete

if(pro<0.05 && (length(data$Age)-sum(is.na(data$Age))>=30)){
  print("Delete")
  
  #if delete
  for (i in 1:length(data$Age)) {
    if(is.na(data$Age[i])){
      data=(data[-c(i),])
    }
  }
} else{
  print("Estimate")
  
  #if estimate
  for (i in 1:length(data$Age)) {
    if(is.na(data$Age[i])){
      data$Age[i]=mean(data$Age,na.rm=TRUE)
    }
  }
}
boxplot(data$Age,
        ylab = "Age",
        main = "Boite à moustache de la population selon l'age",
        col = "#04A777",
        sub = "Donnees : Adoption de la technologie ChatGPT",
        notch = TRUE
)
#Normality Test :

#La seule valeur quantitative est l'age
shapiro.test(data$Age)
#p-value = 4.217e-06 < 5% , donc il y'à une difference significative entre la distribution normale et
#La distribution de l'age
#Verifiant s'il ya une quasi-normalité
library(moments)
skewness(data$Age)
kurtosis(data$Age)
#The variable follows a quasi normality

#Quiz Processing

j=0
for (i in 5:35) {
  j=j+1
  names(data)[i]=paste("item",j,sep = "")
}


library(ltm)
library(dplyr)
index = c(5:35)
items = data.frame(data[,index])
View(items)

for (i in 1:length(items)) {
  items[,i]=as.character(items[,i])
}

items = items%>% mutate_at(colnames(items[ , ]),
                           funs(recode(.,"Tout à fait d'accord"=5,"D'accord"=4,"Neutre"=3,"Pas d'accord"=2,"Pas du tout d'accord"=1)))


items[ , ] = lapply(items, as.numeric) # Convert all columns to numeric
sapply(items)


#PU : Perceived Usefulness
#PEU : Perceived Ease Of Use
#EV : External Variables
#ATC : Attitude Towards Use
#BI : Behavioral Intention

j=0
for(i in 1:6){
  j=j+1
  names(items)[i]=paste("PU",j,sep = "")
}
j=0
for(i in 7:12){
  j=j+1
  names(items)[i]=paste("PEU",j,sep = "")
}
j=0
for(i in 13:15){
  j=j+1
  names(items)[i]=paste("ATU",j,sep = "")
}
j=0
for(i in 16:26){
  j=j+1
  names(items)[i]=paste("EV",j,sep = "")
}
j=0
for(i in 27:30){
  j=j+1
  names(items)[i]=paste("BI",j,sep = "")
}

names(items)[31] =paste("AD",sep = "")

#Univariate Analysis
summary(items)
summary(data$Sexe)
summary(data$Age)
summary(data$Filière)
summary(data$`Année d'étude`)

library(ggplot2)
library(ggdensity)
#Age is quantitative , so we will visualize it using a histogram


with(data, hist(Age, scale="frequency", breaks="Sturges", col="orange"))

#Gender is qualitative , nominal so we used the pie Chart
pie(table(data$Sexe),labels = c("Homme","Femme"))

#For this two variables we want to see one modality because of our sample choice 
pie(table(data$Filière))

pie(table(data$`Année d'étude`))




#Reliability Tests


cronbach.alpha(items)
#alpha = 0.866 so overall we have a strong correspondance between all items
#we have internal coherence between items
bartlett.test(items)
#p-value < 5% so there is a correspondance between items

#Theme 1 - Perceived Usefulness :
cronbach.alpha(items[,1:6])
bartlett.test(items[,1:6])
#D'apres Bartlett , il n'ya pas de correspondance entre les items(PU)

#Theme 2 - Perceived Ease Of Use :
cronbach.alpha(items[,7:12])
bartlett.test(items[,7:12])
#According to Bartlett, there is no correspondance between items of this construct(PEU)

#Theme 3 - Attitude Towards Use :
cronbach.alpha(items[,13:15])
bartlett.test(items[,13:15])
#According to Bartlett, there is no correspondance between items of this construct(ATU)

#Theme 4 - External Variables :
cronbach.alpha(items[,16:26])
bartlett.test(items[,16:26])
#According to Bartlett, there is a correspondance between items of this construct(EV)

#Theme 5 - BI :
cronbach.alpha(items[,27:31])
bartlett.test(items[,27:31])
#According to Bartlett, there is no correspondance between items of this construct(BI)

#$$$$$$$$$$$$$THERES NO INTERNAL RELIABILITY when no correspondance£££££££££££££££££#
#Bivariate Analysis

#Comparaison Test:

#Age~Sexe :
library(car)
leveneTest(data$Age~data$Sexe)
#p-value > 5% : so there is no difference in variance between Age and Sexe
t.test(data$Age~data$Sexe)
#p-value >> 5% : so there is no difference between sexe and Age
wilcox.test(data$Age~data$Sexe)
#p-value >>5% : SO there is no difference between sexe and Age
#The two tests converge to the same results , so we take into account parametric tests results

prop.table(table(data$Sexe,data$Age))

#Age~Filiere :
library(car)
leveneTest(data$Age~data$Filière)
t.test(data$Age~data$Filière)
#p-value < 5% :so there is a difference between speciality and Age
wilcox.test(data$Age~data$Filière)
#p-value <5% : so there is a difference between speciality and Age
#the two tests converge to the same conclusions so we take into account 
#Parametric results

prop.table(table(data$Filière,data$Age))

#Age~Annee d'etude :
library(car)
leveneTest(data$Age~data$`Année d'étude`)
t.test(data$Age~data$`Année d'étude`)
#p-value < 5% :so there is a difference between Age and year of study
wilcox.test(data$Age~data$`Année d'étude`)
#p-value < 5% :so there is a difference between Age and year of study

prop.table(table(data$`Année d'étude`,data$Age))

#Association Tests :

#Sexe~Filiere :
chisq.test(data$Sexe,data$Filière)
#p-value <5% : there is a correspondance between Sexe and major

library(ggplot2)
# stacked bar chart
ggplot(data, 
       aes(x = Filière, 
           fill = Sexe)) + 
  geom_bar(position = "stack")

#Sexe~Annee d'etudes :
chisq.test(data$Sexe,data$`Année d'étude`)
#p-value >5% : there is no correspondance between sexe and years of studies

library(ggplot2)
# stacked bar chart
ggplot(data, 
       aes(x = `Année d'étude`, 
           fill = Sexe)) + 
  geom_bar(position = "stack")



#Annee~Filiere :
chisq.test(data$`Année d'étude`,data$Filière)
#p-value < 5% : there is no correspondance between Major and year of study


#p-value > 5% , so there is no correspondance between Major and year of study


#Conclusions :
#Our sample is not representative 100% because we have some bias in some variables , like for example
#The correspondance that we have between sexe and Major




#Items and Gender:
output=0
for(i in 1:length(items)){
  genre = chisq.test(items[,i],data$Sexe)
  output = output+genre$p.value 
}
li=length(items)
m = output/li
m
#P-value = 0.5136 , There is no correspondance between sexe and items answers

#Items and Age
library(car)
output1 = 0
for (i in ncol(items)) {
  comp1 = aov(data$Age~items[,i])
  comp2 = kruskal.test(data$Age~items[,i])
  if(summary(comp1)[[1]][["Pr(>F)"]][1]>0.05 && comp2$p.value>0.05){
    output1 = output1 + summary(comp1)[[1]][["Pr(>F)"]][1]
  }
  if(summary(comp1)[[1]][["Pr(>F)"]][1]>0.05 && comp2$p.value<0.05){
    output1 = output1 + comp2$p.value
  }
  if(summary(comp1)[[1]][["Pr(>F)"]][1]<0.05 && comp2$p.value<0.05){
    output1 = output1 + summary(comp1)[[1]][["Pr(>F)"]][1]
  }
  if(summary(comp1)[[1]][["Pr(>F)"]][1]<0.05 && comp2$p.value>0.05){
    output1 = output1 + comp2$p.value
  }
}
#anova et kruskal and no t test car plus de 2 modalités pour reponses items donc pas de t test#
m1 = output1/ncol(items) 
m1
#There is a significant difference between answers and age


#Items and Year of study :
output3=0
for(i in 1:ncol(items)){
  year_corr = chisq.test(items[,i],data$Sexe)
  output3 = output3+year_corr$p.value 
  l=ncol(items)
}
m = output3/l
m
#p-value > 5% : There is no correspondance between items answers and year of study

#Items And Major
output4=0
for(i in 1:ncol(items)){
  speciality_corr = chisq.test(items[,i],data$Sexe)
  output4 = output3+speciality_corr$p.value 
  ls=ncol(items)
}
m = output4/ls
m

#p-value = 0.526 > 5% : so there is no correspondance between 


#Items validation: 
for (i in 1:ncol(items)) {
  print(colnames(items[,i]))
  test = chisq.test(table(items[,i]))
  if(test$p.value>0.05){
    cat("H0 : There is no difference in the frequency of answers for item",i,"p-value =",test$p.value)
  }
  else{
    cat("H1 : There is a difference in the frequency of answers for item",i,"p-value =",test$p.value)
  }
}

#TAM

#TAM Hypothesis :
#H1 : External Variables influence the perceived usefulness 
#H2 : External variables influence the perceived ease of use 
#H3 : Perceived Ease of use influence Perceived Usefulness 
#H4 : Perceived Usefulness influence Attitude Towards Use 
#H5 : Perceived Ease of use influence Attitude Towards Use 
#H6 : Perceived Usefulness influence Behavioral Intention 
#H7 : Attitude Towards Usefulness influence Behavioral Intention 
#H8 : Behavioral Intention influences Adoption 

#Perceived Usefulness = PU
#Perceived Ease Of Use = PEU
#Attitude Towards Use = ATU
#Behavioral Intention = BI
#Adoption Of Technology = AD
#External Variables = EV

#cbind rassemble les items dans un vecteur#
EV <- cbind(items$EV1,items$EV2,items$EV3,items$EV4,items$EV5,items$EV6,items$EV7,items$EV8,items$EV9,items$EV10,items$EV11)
BI = cbind(items$BI1,items$BI2,items$BI3,items$BI4)
ATU = cbind(items$ATU1,items$ATU2,items$ATU3)
PEU <- cbind(items$PEU1,items$PEU2,items$PEU3,items$PEU4,items$PEU5,items$PEU6)
PU <- cbind(items$PU1,items$PU2,items$PU3,items$PU4,items$PU5,items$PU6)
score_PU = rowSums(PU)
score_PEU = rowSums(PEU)
score_ATU = rowSums(ATU)
score_BI = rowSums(BI)
score_EV = rowSums(EV)

#Test Of Normality For Score_PU :
shapiro.test(score_PU)
#p-value > 5% : so it is normal in the strict sense of Shapiro
library(moments)
#Test of Normality For Score_PEU :
shapiro.test(score_PEU)
#p-value < 5% : there is no normality in the strict sense
skewness(score_PEU)
kurtosis(score_PEU)
#It doesn't follow the quasi normality nor the normality
#We will use only non parametric tests
library(moments)
#Test of Normality For Score_ATU :
shapiro.test(score_ATU)
#We don't have a normality in the strict sense
skewness(score_ATU)
kurtosis(score_ATU)
#We don't have quasi normality nor normality 
#We will use only non parametric tests
library(moments)
#Test of Normality For Score_EV :
shapiro.test(score_EV)
#We don't have quasi normality nor normality 
skewness(score_EV)
kurtosis(score_EV)
#We will use only non parametric tests
library(moments)
#Test of normality for BI :
shapiro.test(score_BI)
#We don't have a normality
skewness(score_BI)
kurtosis(score_BI)
#We have quasi normality

#Validation of the link between EV and PU :
cor.test(score_EV,score_PU,method = "spearman")
#p-value > 5% : So we have no correlation between score_EV and score_PU


S0=0
for(i in 1:ncol(EV)){
  for(j in 1:ncol(PU)){
    res = chisq.test(EV[,i],PU[,j])
    S0 = S0+res$p.value 
    if(res$p.value>0.05){
      cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res$p.value,"\n")
    }
    else{
      cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res$p.value,"\n")
    }
  }
  l=ncol(EV)*ncol(PU)
}
mean = S0/l
cat("The Average association between the items of EV and PU is",mean,"\n")



#The p-value = 0.331 > 5% , we deduce that there is no correspondance between External Variables and Perceived Usefulness

#Validation of the link between EV et PEU :
cor.test(score_EV,score_PEU,method = "spearman")

#There is no correlation between EV and PEU

S1=0
for(i in 1:ncol(EV)){
  for(j in 1:ncol(PEU)){
    res1 = chisq.test(EV[,i],PEU[,j])
    S1 = S1+res1$p.value
    if(res1$p.value>0.05){
      cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res1$p.value,"\n")
    }
    else{
      cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res1$p.value,"\n")
    }
  }
  l1=ncol(EV)*ncol(PEU)
}
mean1 = S1/l1
cat("The Average association between the items of BI and Adoption is",mean1,"\n")

#p-value = 0.1510964 > 5% so there is no strong correspondance between External variables and Perceived Ease Of use

#Validation of The link between PEU and PU :
cor.test(score_PEU,score_PU,method = "spearman")
#There is no correlation between PEU and PU

S2=0
for(i in 1:ncol(PEU)){
  for(j in 1:ncol(PU)){
    res2 = chisq.test(PEU[,i],PU[,j])
    S2 = S2+res2$p.value 
    if(res$p.value>0.05){
      cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res2$p.value,"\n")
    }
    else{
      cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res2$p.value,"\n")
    }
  }
  l2=ncol(PEU)*ncol(PU)
}
mean2 = S2/l2
cat("The Average association between the items of BI and Adoption is",mean2,"\n")

#p-value = 0.342 > 5% : there is no correspondance between the answers in Perceived Ease of Use and Perceived usefulness items
#The link is not validated

#Perceived Ease Of use ~ Attitude Towards Use
cor.test(score_PEU,score_ATU,method = "spearman")
#There is no correlation between ATU and PEU
S3=0
for(i in 1:ncol(PEU)){
  for(j in 1:ncol(ATU)){
    res3 = chisq.test(PEU[,i],ATU[,j])
    S3 = S3+res3$p.value
    if(res3$p.value>0.05){
      cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res3$p.value,"\n")
    }
    else{
      cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res3$p.value,"\n")
    }
  }
  l3=ncol(PEU)*ncol(ATU)
}
mean3 = S3/l3
cat("The Average association between the items of BI and Adoption is",mean3,"\n")

#p-value = 0.476 >5% : There is no correspondance between PEU and ATU

#Perceived Usefulness ~ Attitude Towards Use
cor.test(score_PU,score_ATU,method = "spearman")
#There is no correspondance

S4=0
for(i in 1:ncol(PU)){
  for(j in 1:ncol(ATU)){
    res4 = chisq.test(PU[,i],ATU[,j])
    S4 = S4+res4$p.value 
    if(res4$p.value>0.05){
      cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res4$p.value,"\n")
    }
    else{
      cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res4$p.value,"\n")
    }
  }
  l4=ncol(PU)*ncol(ATU)
}
mean4 = S4/l4
cat("The Average association between the items of BI and Adoption is",mean4,"\n")

#p-value = 0.256 : there is no correspondance between PU and ATU

#Perceived Usefulness ~ Behavioral Intention

S5=0
for(i in 1:ncol(PU)){
  for(j in 1:ncol(BI)){
    res5 = chisq.test(PU[,i],BI[,j])
    S5 = S5+res5$p.value 
    if(res5$p.value>0.05){
      cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res5$p.value,"\n")
    }
    else{
      cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res5$p.value,"\n")
    }
  }
  l5=ncol(PU)*ncol(BI)
}
mean5 = S5/l5
cat("The Average association between the items of BI and Adoption is",mean5,"\n")

#p-value=0.265 > 5% :There is no correspondance between PU items and BI items 
#Attitude Towards Use ~ Behavioral Intention
cor.test(score_ATU,score_BI,method = "spearman")
cor.test(score_ATU,score_BI,method = "pearson")
#There is no correlation between ATU and BI


S6=0
for(i in 1:ncol(ATU)){
  for(j in 1:ncol(BI)){
    res6 = chisq.test(BI[,j],ATU[,i])
    S6 = S6+res6$p.value 
    if(res6$p.value>0.05){
      cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res6$p.value,"\n")
    }
    else{
      cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res6$p.value,"\n")
    }
  }
}
l6=ncol(ATU)*ncol(BI)
mean6 = S6/l6
cat("The Average association between the items of BI and Adoption is",mean6,"\n")



#Behavioral Intention ~ Adoption Of ChatGPT :

S7=0
for(i in 1:ncol(BI)){
  res7 = chisq.test(items$AD,BI[,i])
  S7 = S7+res7$p.value
  if(res7$p.value>0.05){
    cat("H0 : There is no association between item",i,"and item",j,"our p-value =",res7$p.value,"\n")
  }
  else{
    cat("H1 : There is an association between item",i,"and item",j,"our p-value =",res7$p.value,"\n")
  }
}
l7=ncol(BI)
mean7 = S7/l7
cat("The Average association between the items of BI and Adoption is",mean7,"\n")

#Technology Acceptance Model is not validated based on our dataset, this is due maybe
#to the fact that our dataset is not adequate




#Factor Analysis :

library(psych)
library(readxl)
library(REdaS)

bart_spher(items)
KMO(items)
#We cannot study PCA for all the items because
#Overall MSA )0.5


###################### Theme 1 : EV ######################################
EV <- data.frame(items$EV1,items$EV2,items$EV3,items$EV4,items$EV5,items$EV5,items$EV6,items$EV7,items$EV8,items$EV9,items$EV10,items$EV11)
bart_spher(EV)
KMO(EV)
#Overall MSA < 0.5
#p-value < 2.22e-16
###################### Theme2 : PU #######################################
PU <- data.frame(items$PU1,items$PU2,items$PU3,items$PU4,items$PU5,items$PU6)
bart_spher(PU)
KMO(PU)
#we have the p-value < 5%
#Overall MSA : 0.69<0.7
###################### Theme3 : PEU ######################################
PEU <- data.frame(items$PEU1,items$PEU2,items$PEU3,items$PEU4,items$PEU5,items$PEU6)
bart_spher(PEU)
KMO(PEU)
#Overall MSA = 0.75
#p-value < 2.22e-16
#We can PCA and Factor analysis for the items of this theme

local({
  .PC <- princomp(~PEU1+PEU2+PEU3+PEU4+PEU5+PEU6, cor=TRUE, data=items)
  cat("\nComponent loadings:\n")
  print(unclass(loadings(.PC)))
  cat("\nComponent variances:\n")
  print(.PC$sd^2)
  cat("\n")
  print(summary(.PC))
})


.FA <- factanal(~PEU1+PEU2+PEU3+PEU4+PEU5+PEU6, factors=1, rotation="varimax", scores="none", data=items)
print(.FA)

#Factor1
#PEU1 0.768  
#PEU2 0.694  
#PEU3 0.690  
#PEU4 0.816  
#PEU5 0.904  
#PEU6 0.763  

library(Rcmdr)

###################### Theme 4 : ATU #####################################
ATU <- data.frame(items$ATU1,items$ATU2,items$ATU3)
bart_spher(ATU)
KMO(ATU)
#Overall MSA : 0.45 < 0.7
#p-value > 5%
###################### Theme 5 : BI ######################################
BI <- data.frame(items$BI1,items$BI2,items$BI3,items$BI4)
bart_spher(BI)
KMO(BI)
#Overall MSA = 0.78>0.7
#p-value << 2.22e-16


local({
  .PC <- princomp(~BI1+BI2+BI3+BI4, cor=TRUE, data=items)
  cat("\nComponent loadings:\n")
  print(unclass(loadings(.PC)))
  cat("\nComponent variances:\n")
  print(.PC$sd^2)
  cat("\n")
  print(summary(.PC))
})


.FA <- factanal(~BI1+BI2+BI3+BI4, factors=1, rotation="varimax", scores="none", data=items)
print(.FA)


#Factor1
#BI1 0.827  
#BI2 0.513  
#BI3 0.859  
#BI4 0.794  


#H1 rejected
#H2 rejected
#H3 rejected
#H4 rejected
#H5 rejected
#H6 rejected
#H7 rejected
#H8 accepted


