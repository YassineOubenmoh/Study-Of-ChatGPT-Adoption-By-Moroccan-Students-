#-----------------------------------------------Step 1-----------------------------------------------------------------------------------------------------------
#Importing libraries :
library(readxl)
library(dplyr)
library(moments)
library(psych)
library(BioStatR)
library(car)
library(rcompanion)
library(ggplot2)

#-----------------------------------------------STEP 2------------------------------------------------------------------------------------------------------------
#Importing Data :
D <- read_excel("D:/info ci/S6/R/Projet/Formulaire .xlsx")
head(D)

names(D)[3]="name"
names(D)[2]="mail"
names(D)[4]="gender"
names(D)[5]="age"
names(D)[6]="major"

for (i in 7:ncol(D)) {
  colnames(D)[i] <- paste0("Q", i-6) #paste0() function simply concatenates the two input strings together without any separator.
}
View(D)
#-----------------------------------------------STEP 3------------------------------------------------------------------------------------------
#Pre-Processing phase:

D$gender <- factor(D$gender, levels = c("Male", "Female"), labels = c(1, 2))

D$major <- factor(D$major, levels = c("GI", "GIn","GMeca", "Civil","RST" ,"GE"), labels = c(1, 2 , 3 ,4 , 5, 6))
D$major <- as.numeric(D$major)
D <- D %>%
  mutate_at(vars(7:ncol(D)), ~factor(., levels = c("totally agree","agree", "neutral","disagree", "totally disagree" ,"Totally disagree"), labels = c(1, 2, 3 , 4 , 5 , 5)))
D <- D %>%
  mutate_at(vars(7:ncol(D)), ~as.numeric(as.character(.)))

# NA values 
boxplot(D$gender)
boxplot(D$age)
out = boxplot.stats(D$age)$out
for(i in 1:length(D$age)){
  if(D$age[i]==out){
    D$age[i]=NA
    break
  }
}
# testing the proportion of NA values in our simple 
pna=(length(out)/length(D$age))*100
pna
# 2 % < 5% so we drop this value 
#But first we have to test our sample length .
for(i in 1:length(D$age)){
  if(is.na(D$age[i])){
    if(pna<5){
      if(length(D$age)-length(out)>=30){
        D <- na.omit(D) # omit() function to remove the row that contains the NA values  
        print("na dropped")
      }else{
        print("na estimeted")
        D$age[i]=as.integer(mean(D$age,na.rm=TRUE))
      }
    }else{
      D$age[i]=as.integer(mean(D$age,na.rm=TRUE))
    }
  }
}
boxplot(D$age)$out
# Normality Test : 
#___________________________________________________ age ____________________________________________________________________________________________________
shapiro.test(D$age)
# p -value = 9.908e-08 < 5% -----> H1 : there is a significant difference between 
# our age distribution and the Gaussian distribution . 
# nearly-Normality 
skewness(D$age)
# 0.6914168 
kurtosis(D$age)
# 2.296308
# the skewness and kurtosis values are in the [-3 ; 3 ] range .
# So the age variable follows the Nearly-Normality 
#__________________________________________________ gender __________________________________________________________________________________________________
# gender has only two possible values (F/M) , so we can't perform the normality test.

#__________________________________________________ Major ___________________________________________________________________________________________________
barplot(table(D$major),col = blues9 , xlab = "MAJORS" , ylab = "number of students" )
chisq.test(table(D$major))
#p-value = 1.067e-05 < 5% -----> H1 : there is a significant difference between 
# our major distribution and the Gaussian distribution . 

#------------------------------------------------ Coherence of our form -------------------------------------------------------------------------------------
# perceived usefulness

items.usefulness <- data.frame(D$Q1,D$Q2,D$Q3,D$Q4,D$Q5)
psych::alpha(items.usefulness)
#raw_alpha = 0.89  ----> good Coherence 
# no item to delete in order to increase the Reliability of the usefulness theme .

items.ease <- data.frame(D$Q6,D$Q7,D$Q8,D$Q9,D$Q10,D$Q11)
psych::alpha(items.ease)
#raw_alpha = 0.94  ----> good Coherence 
# no item to delete in order to increase the Reliability of the ease of use theme .

items.attitude <- data.frame(D$Q12,D$Q13,D$Q14)
psych::alpha(items.attitude)
#raw_alpha =  0.75  ----> good Coherence 
# no item to delete in order to increase the Reliability of the attitude towards using the generative ai theme .

items.intention <- data.frame(D$Q15,D$Q16,D$Q17)
psych::alpha(items.intention)
#raw_alpha = 0.87  ----> weak Coherence (acceptable)
# Reliability increases if we drop the Q15 (alpha =  0.93)

items.external <- data.frame(D$Q18,D$Q19,D$Q20)
psych::alpha(items.external)
#raw_alpha = 0.75  ----> good Coherence 
# no item to delete in order to increase the Reliability of the external variables . 

# Representative test

prop.table(table(D$gender)) # Our sample is made of 48% males , 51% females
chisq.test(table(D$gender)) #p_value =  0.8864 > 5% : the sample is representative

t.test(D$age) # 95 percent confidence interval: the sample is representative
# ----------------------------------------STEP 4 : Processing----------------------------------------------------------------------- 
# Mono-Variable Descriptive Statistics : 
summary(D)

#Data-visualization : 

hist(D$age , xlab = "AGE" ,col = "orange" )
plot(D$gender ,xlab= "GENDER" , ylab = "FREQUENCY" , col = c("#139AE8", "pink"))
hist(D$major , xlab = "MAJOR", col = "gold4")

# Hypothesis testing : 
# variance test : 
leveneTest(D$age~D$gender) 
# p-value= 0.5393 > 5% ----> there is no significant difference between the variance of age and gender.
# Bi-Variable Analysis :

#Comparison test : 
wilcox.test(D$age~D$gender, exact = FALSE) #the exact attribute is false to avoid ex tied values
# p-value = 0.1804 > 0.05 -----> there is no significant difference between the age and gender variables .

# association test :  
eta2(D$age, D$gender)
plot(D$gender, D$age , xlab= "GENDER" , ylab= "AGE" , col = c("#139AE8", "pink"))

# hypothesis : 

#H1 : Students who perceive the usefulness of Generative AI tools for higher education
#    are likely to adopt them.

#H2 : Students who perceive less difficulties related to the use of Generative AI in
#     higher education are likely to adopt them . 

#H3 : Students between 20 and 22 years old are likely to adopt the Generative 
#     AI tools in their studies . 

#H4 : People with a positive attitude towards the use of Generative AI are likely to adopt it .

#H5 : Students between 20 and 22 years old are likely to find the Generative 
#     AI tools easier to use in their studies .

#H6 : Students between 20 and 22 years old are likely to find the Generative 
#     AI tools useful in their studies . 


# Usefulness row means : 
usefulness <- cbind(D$Q1, D$Q2, D$Q3 , D$Q4 , D$Q5)
usefulness_mean <- rowMeans(usefulness)
usefulness_mean

shapiro.test(usefulness_mean)
#p-value = 2.91e-05
# usefulness_mean is significantly different than the Gaussian distribution .
skewness(usefulness_mean) # 1.466201
kurtosis(usefulness_mean) # 5.13237
#so usefulness_mean does not follow the Gaussian distribution .

# ease of use row means 
ease <- cbind(D$Q6,D$Q7,D$Q8,D$Q9,D$Q10,D$Q11)
ease_mean <- rowMeans(usefulness)
ease_mean

shapiro.test(ease_mean)
#p-value = 2.91e-05
# ease_mean is significantly different than the Gaussian distribution .
skewness(ease_mean) # 1.466201
kurtosis(ease_mean) # 5.13237
#so ease_mean does not follow the Gaussian distribution .

# Attitude row means : 
attitude <- cbind(D$Q12,D$Q13,D$Q14)
attitude_mean <- rowMeans(attitude)
attitude_mean

shapiro.test(attitude_mean)
#p-value = 0.0001476
#attitude_mean is significantly different than the Gaussian distribution .
skewness(attitude_mean)# 1.067102
kurtosis(attitude_mean)# 4.31725
#so attitude_mean does not follow the Gaussian distribution .



# Intention row means : 
intention <- cbind(D$Q15,D$Q16,D$Q17)
intention_mean <- rowMeans(intention)
intention_mean

shapiro.test(intention_mean)
#p-value = 5.778e-06
skewness(intention_mean) # 1.451659
kurtosis(intention_mean) # 5.724516
#so intention_mean does not follow the Gaussian distribution .


# External variables row means 
external <- cbind(D$Q18,D$Q19,D$Q20)
external_mean <- rowMeans(external)
external_mean

shapiro.test(external_mean)
#p-value = 0.007731 , so external_mean does not follow the Gaussian distribution. 
kurtosis(external_mean) # 3.815234
#so external_mean does not follow the Gaussian distribution.



# Hypothesis Testing : 
#H1 : ACCEPTED
chisq.test(usefulness_mean, intention_mean)
plot(usefulness_mean, intention_mean , col = ("#B70A7E"))
#p-value = 8.12e-06 < 0.05 , so we reject the null hypothesis H0, that means there is an association between the usefulness and intention of use . 
cramerV(table(usefulness_mean, intention_mean))
# v = 0.6764 ---> Strong association .

#H2 : ACCEPTED
plot(ease_mean,intention_mean , col = ( "#0000FF"))
chisq.test(ease_mean,intention_mean)
#p-value = 8.12e-06 , so we reject the null hypothesis H0, that means there is an association between the ease of use and intention of use .
cramerV(table(ease_mean,intention_mean))
# v = 0.6764 ---> Strong association .

#H3 : ACCAPTED 
kruskal.test(D$age~intention_mean)
# p=0.6326 ---> so there is no difference between variance of age and the variance of intention of use. 

#H4 : ACCEPTED
chisq.test(attitude_mean, intention_mean)
#p-value = 1.101e-11 , so we refuse the null hypothesis , that means there is an association between the attitude and the intention of use .
cramerV(table(attitude_mean,intention_mean))
# v = 0.6784 ---> Strong association . 

#H5 : ACCEPTED
kruskal.test(D$age~ease_mean)
#p-value =  0.9569 > 0.05 , so we accept the null hypothesis , that means there is no significant difference between the variance of age and the ease of use .

#H6 : ACCEPTED
kruskal.test(D$age~usefulness_mean)
#p-value = 0.9569 > 0.05 , so we accept the null hypothesis , that means there is no significant difference between the variance of age and the perceived usefulness of the technology . 










