library(tidyverse)
library(apaTables)
library(haven)
library(dplyr)

bfi_data <- psych::bfi #load data from psych library 

categorical_variables <- select(bfi_data,gender) # categorical data as factors 
categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"Female"=2)

agree_items <- select(bfi_data,A1,A2,A3,A4,A5) #scaling 
extra_items <- select(bfi_data,E1,E2,E3,E4,E5)
neuro_items <- select(bfi_data,N1,N2,N3,N4,N5)
age <- bfi_data$age #for proper binding later 
education <- bfi_data$education #technically categorical but can keep numerical form for correlation analysis

agree_items <- mutate(agree_items,A1=7-A1) #reverse score 
extra_items <- mutate(extra_items,E1=7-E1)
extra_items <- mutate(extra_items,E2=7-E2)

agreeableness <- psych::alpha(as.data.frame(agree_items),check.keys=FALSE)$scores #calculate scores
extraversion <- psych::alpha(as.data.frame(extra_items),check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuro_items),check.keys=FALSE)$scores

analytic_data <- cbind(agreeableness,extraversion,neuroticism,categorical_variables,education,age) #cbind into one dataset

write_csv(analytic_data,path="analytic_data.csv") #save data set



# TABLES AND GRAPHS --------------------------------------------------------------------------------------------------


#No.1: Correlation table - no gender
analytic_data_no.gender <- select(analytic_data, -gender) #delete gender column 
apa.cor.table(analytic_data_no.gender,filename="Table1.doc",table.number=1)

#No.2: Correlation table - only on men over 40 
analytic_data_males <- filter(analytic_data, gender=="Male") #filter means only include these ROWS
analytic_data_males.over.40 <- analytic_data_males %>% filter(age>40) %>% select(-gender) #filter for ROWS of over 40 then take out column of gender 
## for over and equal to 40, filter(age>=40) 
apa.cor.table(analytic_data_males.over.40,filename="Table2.doc",table.number=2)

#No.3: Scatterplot of relation between agreeableness and extraversion for men over 40 
my.plot.agree.extra <- qplot(x=agreeableness,y=extraversion,data=analytic_data_males.over.40)
my.plot.agree.extra <- my.plot.agree.extra + geom_smooth(method = "lm", se = FALSE, color='black')
my.plot.agree.extra <- my.plot.agree.extra + theme_classic()
my.plot.agree.extra <- my.plot.agree.extra + theme(axis.line.x = element_line(colour='black',size=0.5,linetype='solid'),
                                                   axis.line.y = element_line(colour='black',size=0.5,linetype='solid'))
my.plot.agree.extra <- my.plot.agree.extra + labs(title="Agreeableness and Extraversion for Men over the Age of 40",x="Agreeableness",y="Extraversion")
my.plot.agree.extra <- my.plot.agree.extra + coord_cartesian(xlim=c(0,6),ylim=c(0,6))
print(my.plot.agree.extra)
ggsave(filename="Figure1.pdf",plot=my.plot.agree.extra,width=6,height=6) #save plot to pdf

#Write CSVs for all other subsets
write_csv(analytic_data_no.gender,path="analytic_data_no.gender.csv")
write_csv(analytic_data_males.over.40,path="analytic_data_over.40.csv")
