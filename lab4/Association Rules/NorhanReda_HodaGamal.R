#1-
rm(list=ls())
setwd(getwd())

#2-
#install.packages("arules")
#install.packages("arulesViz")

# Load the arules library
library(arules)

# Load the arulesViz library
library(arulesViz)

#3-

transactions = read.transactions("AssociationRules.csv", header=FALSE)
#4-
inspect(transactions[1:100])
#-5
summary(transactions)

#item13   item5  
#4948    3699


#6-
itemFrequencyPlot(transactions, topN=5)


#7-
rules = apriori(transactions, parameter = list(supp = 0.01, conf = 0.5 , minlen = 2))

#8-
sorted_rules_support = sort(rules, descreasing=TRUE, by="support")
inspect(head(sorted_rules_support, 6))

#9-
sorted_confidence = sort(rules, descreasing=TRUE, by="confidence")
inspect(head(sorted_confidence, 6))

#10-

sorted_lift = sort(rules, descreasing=TRUE, by="lift")
inspect(head(sorted_lift, 6))


#11-
plot(rules, measure = c("support", "confidence"), shading = "lift",jitter = 0)

#12- Based on (8-11), Can you tell now what are the most interesting rules that are really useful and
#provide a real business value and an insight to the concerned corporate?


# the most interesting and valuable rules 
  
#Rule 1: {item15, item30, item56} => {item49} (High lift and reasonable support and reasonable confidence)
#Rule 2: {item30, item56, item84} => {item49} (High lift and reasonable support and reasonable confidence)
#Rule 3: {item15, item30, item49} => {item56} (High lift, high confidence, and reasonable support)
#Rule 4: {item15, item49} => {item56} (High lift, high confidence, and reasonable support)
#Rule 5: {item5} => {item13} (High support and reasonable confidence)
#Rule 6: {item30} => {item13} (High support and reasonable confidence)
