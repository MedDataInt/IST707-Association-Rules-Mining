# homework 3_ by Jie Wang
library(plyr)
library(dplyr)
library(arules)
library(arulesViz)
library(qwraps2) 


data <-read.csv('bankdata_csv_all.csv')
head(data)

summary(data)
options(qwraps2_markup = 'markdown')
summary_table(data[,-1])

data$age <- cut(data$age, breaks = c(0,10,20,30,40,50,60,Inf),
              labels=c("child","teens","twenties","thirties","fourties","fifties","old"))

min_income <- min(data$income)
max_income <- max(data$income)
bins = 3 



data$income = cut(data$income, breaks=seq(min_income, max_income, width), labels = c('high','medium','low'))
data$children=factor(data$children)
data$married=as.factor(data$married)
data$car=as.factor(data$car)
data$save_act=as.factor(data$save_act)
data$current_act=as.factor(data$current_act)
data$mortgage=as.factor(data$mortgage)
data$pep=as.factor(data$pep)

str(data)
data <- data[,-1]
trans_1 <- as(data,'transactions')

class(trans_1)
View(trans_1)

fre <- itemFrequencyPlot(trans_1, topN =20, type = 'absolute')

myRules <- apriori(data, parameter = list(supp = 0.05, conf= 0.9, minlen = 2))
options(digits =2)
inspect(myRules[1:10])

plot(myRules, shading="order", control=list(main = "Two-key plot", 
                                          col=rainbow(5)))
rules <- myRules[1:10]
plot(rules, method = "graph", engine =  "htmlwidget")

plot(rules, method = "graph")
plot(myRules, method = "matrix", measure = c('lift', 'confidence'))

myRules <- apriori(data, parameter = list(supp = 0.001, conf= 0.9))
myRules <- apriori(data, parameter = list(supp = 0.01, conf= 0.9))

myRules <- sort (myRules, by = 'confidence', decreasing = T)

# set rhs is PEP
Myrules_1 <- apriori(data = data, parameter = list(supp = 0.05, conf = 0.9, minlen = 2), 
                appearance = list (default = 'lhs', rhs ='pep=YES'),
                control = list(verbose =F))

Rules <- sort(Myrules_1, decreasing =TRUE, by = 'confidence')
inspect(Rules[1:10])
plot(Rules[1:10], method="graph", engine =  "htmlwidget")


Rules_1 <- sort(Myrules_1, decreasing =TRUE, by = 'support')
inspect(Rules_1[1:10])
plot(Rules_1[1:10], method="graph", engine =  "htmlwidget")


Rules_2 <- sort(Myrules_1, decreasing =TRUE, by = 'lift')
inspect(Rules_2[1:10])
plot(Rules_2[1:10], method="graph", engine =  "htmlwidget")