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


## Specifying the prob distributions
A.lv <- c("young", "adult", "old")
A.prob <- array(c(0.3,0.5,0.2), dim = 3, dimnames = list(A = A.lv))

S.lv <- c("M", "F")
S.prob <- array(c(0.6,0.4), dim = 2, dimnames = list(S = S.lv))

E.lv <- c("high", "uni")
E.prob <- array(c(0.75,0.25,0.72,0.28,0.88,0.12,0.64,0.36,0.70,0.30,0.90,0.10),
                dim = c(2,3,2),
                dimnames = list(E = E.lv, A = A.lv, S = S.lv))

O.lv <- c("emp", "self")
O.prob <- array(c(0.96,0.04,0.92,0.08),
                dim = c(2,2),
                dimnames = list(O = O.lv, E = E.lv))

R.lv <- c("small", "big")
R.prob <- array(c(0.25,0.75,0.2,0.8),
                dim = c(2,2), dimnames = list(R = R.lv, E = E.lv))

T.lv <- c("car", "train", "other")
T.prob <- array(c(0.48,0.42,0.10,0.56,0.36,0.08,0.58,0.24,0.18,0.70,0.21,0.09),
                dim = c(3,2,2),
                dimnames = list(T = T.lv, O = O.lv, R = R.lv))

cpt <- list(A = A.prob, S = S.prob, E = E.prob, O = O.prob, R = R.prob, T = T.prob)

# fit cpt table to network
bn <- custom.fit(dag, cpt)


## we want to estimate the parameters of the conditional distribution (i.e. cpt)
## given we know the DAG
## all this does is calculates the conditional probs from the data
bn.mle <- bn.fit(dag, data = surveyData, method = "mle")
bn.mle


## estimate the parameters
