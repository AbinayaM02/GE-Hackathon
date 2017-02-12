library(bnlearn)
setwd("D:\\Others\\GE Heath hack")
diabetes_numerical<-as.data.frame(read.csv("diabetes_Numerical.csv"))
diabetes_numerical<-subset(diabetes_numerical,select=-c(1))
#scatterplot with categorical data
#pairs(~gender+Age+frame+WH_Category+BMI+BP+CholestrolRatio+Diabetic., data = diabetes_categorized1,main = "Scatterplot Matrix")

#scatterplot with numerical data 
pairs(~chol+stab.glu+hdl+ratio+glyhb+age+gender+frame+bp.1s+bp.1d+BMI+WHRatio, data=diabetes_numerical)
#Categorizing and summarizing
#Summary of the data
diabetes_categorized3<-read.csv("diabetes_categorized3.csv")
diabetes_categorized3_filtered<-subset(diabetes_categorized3,select=-c(1,9,10))
summary(diabetes_categorized3_filtered)
#checking the bayesian network of the entire dataset
diabetes_bn<-hc(diabetes_categorized3_filtered)
arcs=boot.strength(diabetes_categorized3_filtered,algorithm="hc")
arcs<-arcs[(arcs$strength>0.7),] #& (arcs$direction>0.5),]
arcs_matrix<-as.matrix(arcs[,1:2])

highlight<-list(arcs=arcs_matrix)
graphviz.plot(diabetes_bn, highlight = highlight)
#plot(diabetes_bn)

library(SDEFSR)

##operating on full dataset
#Create SDEFSR dataset object
df <- data.frame(diabetes_categorized3_filtered)
SDEFSR_DatasetObject <- SDEFSR_DatasetFromDataFrame(df, relation = "random")

#Run MESDIF algorithm to get the rule set
ruleSet <- MESDIF(training = SDEFSR_DatasetObject)


#List rules by significance
rulesOrderedBySignificance <- sort(x = ruleSet, decreasing = TRUE, by = "Significance")
rulesOrderedBySignificance
filteredRules <- rulesOrderedBySignificance[Unusualness > 0.05]
plotRules(filteredRules)


##operating on train dataset
diabetes_train<-read.csv("diabetes_train.csv")
diabetes_train<-subset(diabetes_train,select=-c(1))

#checking the bayesian network of the train dataset
diabetes_bn_train<-hc(diabetes_train)
arcs=boot.strength(diabetes_train,algorithm="hc")
arcs<-arcs[(arcs$strength>0.7),] #& (arcs$direction>0.5),]
arcs_matrix<-as.matrix(arcs[,1:2])

highlight<-list(arcs=arcs_matrix)
graphviz.plot(diabetes_bn, highlight = highlight)
#plot(diabetes_bn)
#creating fuzzy rules
#Create SDEFSR dataset object
df <- data.frame(diabetes_train)
SDEFSR_DatasetObject <- SDEFSR_DatasetFromDataFrame(df, relation = "random")

#Run MESDIF algorithm to get the rule set
ruleSet <- MESDIF(training = SDEFSR_DatasetObject)


#List rules by significance
rulesOrderedBySignificance <- sort(x = ruleSet, decreasing = TRUE, by = "Significance")
rulesOrderedBySignificance
filteredRules <- rulesOrderedBySignificance[Unusualness > 0.05]


SDEFSR_GUI()

