library(tidyverse)
library(apaTables)
library(haven)
library(dplyr)

#Load data
bfi_data <- psych::bfi

#Labelling categorical data 
categorical_variables <- select(bfi_data,gender)
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2)

#Creating scales 
agree_items <- select(bfi_data,A1,A2,A3,A4,A5) 
extra_items <- select(bfi_data,E1,E2,E3,E4,E5)
neuro_items <- select(bfi_data,N1,N2,N3,N4,N5)
age <- bfi_data$age
education <- bfi_data$education

#Reverse score
agree_items <- mutate(agree_items,A1=7-A1)
extra_items <- mutate(extra_items,E1=7-E1)
extra_items <- mutate(extra_items,E2=7-E2)

#Calculating scores
agreeableness <- psych::alpha(as.data.frame(agree_items),check.keys=FALSE)$scores
extraversion <- psych::alpha(as.data.frame(extra_items),check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuro_items),check.keys=FALSE)$scores

#Create analytic data
analytic_data <- cbind(agreeableness,extraversion,neuroticism,categorical_variables,education,age)

#Save data set 
write_csv(analytic_data,path="analytic_data.csv")

#No.1: Correlation table - no gender
analytic_data_no.gender <- select(analytic_data, -gender)
apa.cor.table(analytic_data_no.gender,filename="Table1.doc",table.number=1)

#No.2: Correlation table - only on men over 40 
analytic_data_over.40 <- filter(analytic_data, age>40) 
analytic_data_over.40 <- select(analytic_data_over.40, -gender)
apa.cor.table(analytic_data_over.40,filename="Table2.doc",table.number=2)

#No.3: Scatterplot of relation between agreeableness and extraversion for men over 40 
my.plot.agree.extra <- qplot(agreeableness,extraversion,data=analytic_data_over.40)
my.plot.agree.extra <- my.plot.agree.extra + geom_smooth(method = "lm", se = FALSE, color='black')
my.plot.agree.extra <- my.plot.agree.extra + theme_classic()
my.plot.agree.extra <- my.plot.agree.extra + theme(axis.line.x = element_line(colour='black',size=0.5,linetype='solid'),axis.line.y = element_line(colour='black',size=0.5,linetype='solid'))
my.plot.agree.extra <- my.plot.agree.extra + labs(title="",x="Agreeableness",y="Extraversion")
my.plot.agree.extra <- my.plot.agree.extra + coord_cartesian(xlim=c(0,6),ylim=c(0,6))
print(my.plot.agree.extra)
ggsave(filename="Figure1.pdf",plot=my.plot.agree.extra,width=6,height=6)

#Write CSVs for all other subsets
write_csv(analytic_data_no.gender,path="analytic_data_no.gender.csv")
write_csv(analytic_data_over.40,path="analytic_data_over.40.csv")
