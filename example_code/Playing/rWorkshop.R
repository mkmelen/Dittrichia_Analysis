1+1
mean(1:5)

# This is a comment

rm(list = ls())
?mean

####

setwd("/Users/Miranda/Documents/Miranda/Education/R Directory")
getwd()


#####
1:5 + 6:10

####

A <- 2
a = 3

A
B
A + 3
A * A
pi
A <- 3
(C <-7)
remove (a)

####

B <- "R Workshop"; B

###
C <- c(100,200,300,400,550)
C
C <- c("red","blue","black")
C
rep(2,10)
seq(1,10)
seq(1,10,2)
C[2]
C[4]
C[c(2:4)]
C[-2]
length(C)
C <- c(1:10)
names(C) <- c("Stanford", "Harvard", "MIT", "Princeton", "Berkeley", "Columbia", "NYU", 
              "Oxford", "Cambridge", "Notre Dame")
C
####
x <- 1:10; x  
x >=5 
(y<- 1:10 %% 2)
y == 0
x >=5 & y==0
x >= 5| y ==0
####
x <- 1:1000
y <- 1/x
w <- arctan(y)
?Trig
w <- atan(y)
z <- 1/w
x == z
x[1] == z[1]
######
matrix(c(10,20,30,40), 2, 2)
D <- matrix(c(10,20,30,40), nrow=2,ncol=2)
D

D[2,1]
D[,]
D[2,]
nrow(D)
length(D)
dim(D)

####

E <- data.frame(
  age = c(20,24,26,23,29),
  sex = c("Female","Male","Female","Male","Female"),
  treatment = c(1,0,0,1,1),
  income = c(1000,1500,2000,2500,3000)
)
E

E[4,4]
E$age
E[,"age"]

###

A <- 5
B <- "R workshop"
C <- c(1:10)
D <- matrix(c(10,20,30,40), nrow=2,ncol=2)
E <- data.frame(
  age = c(20,24,26,23,29),
  sex = c("Female","Male","Female","Male","Female"),
  treatment = c(1,0,0,1,1),
  income = c(1000,1500,2000,2500,3000)
)
global.list <- list(A,B,C,D,E)
global.list
names(global.list) <- c("number", "string", "vector", "matrix", "data.frame")
global.list
global.list$vector

####

class(A)
class(A)
class(B)
class(C)
class(D)
class(E)
class(global.list)

####

as.character(A)
D
as.vector(D)
C
as.matrix(C)

####

diag(10:0,21,21)

####

mean(C)
log(C)
D <- matrix(c(10,20,30,40), nrow=2,ncol=2)
D
t(D)

D*D

####

E <- data.frame(
  age = c(20,24,26,23,29),
  sex = c("Female","Male","Female","Male","Female"),
  treatment = c(1,0,0,1,1),
  income = c(1000,1500,2000,2500,3000)
)
dim(E)
head(E,3)
summary(E)

####

hypothenuse <- function(x,y){sqrt(x^2+y^2)}
hypothenuse(2,4)

