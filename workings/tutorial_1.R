## CausalML: tutorial 1

library(bnlearn)
library(Rgraphviz)


# empty graph
dag <- empty.graph(nodes = c("A","S","E","O","R","T"))
arc.set <- matrix(c("A", "E",
                    "S", "E",
                    "E", "O",
                    "E", "R",
                    "O", "T",
                    "R", "T"),
                  byrow = TRUE, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag) <- arc.set
nodes(dag)
graphviz.plot(dag)

## v structures
vstructs(dag)

#Markov blanket of node E in survey data set
mb(dag, node = "E")

## d-separation
dsep(dag, x = "S", y = "A", z="E")  # TRUE
dsep(dag, x = "S", y = "R")  # FALSE --> because E is dependent on A
dsep(dag, x = "S", y = "O")  # FALSE --> same as S-R
dsep(dag, "S", "T",c("O","R"))

## from the data
surveyData <- read.table("tutorials/data/survey.txt", header = TRUE)
surveyData[] <- lapply(surveyData, function(x) as.factor(x)) #convert column types from character to factors
head(surveyData)

summary(surveyData)

## conditional dependence
ci.test("S","T",c("O","R"), data = surveyData, debug = T)

# data:  S ~ T | O + R
# mi = 13.207, df = 8, p-value = 0.1049
# alternative hypothesis: true value is greater than 0
# p-value > 0.05, therefore ACCEPT Null hypothesis that S is INDP of T given O/R

ci.test("T","S",c("O","R"), data = surveyData)
