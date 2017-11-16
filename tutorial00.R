ggplot(data=mydata[mydata$carat<2.5,],
       aes(x=carat, y=price, colour=clarity)) +
  geom_point(alpha=0.1) +
  geom_smooth()


#integer
x <-2
typeof(x)

#double
y <- 2.5
typeof(y)

#complex
z <- 3 + 2i
typeof(z)

#character
a <- "h"
typeof(a)

#logical
q1 <- T
typeof(q1)

A <- 10
B <- 5

C <- A + B
C

#variable 1
var1 <- 2.5
#variable 2
var2 <- 4

result <- var1 /var2
result

answer <- sqrt(var2) 
answer


greeting <- "Hello"
name <- "Bob"
message <- paste(greeting, name)
message

#Logical:
#TRUE T
#FALSE F

4 < 5
10 > 100
4 == 5
# ==
# !=
# <
# >
# <=
# >=
# !
# |
# &
# isTRUE(x)

result <- 4 < 5
result
typeof(result)

result2 <- !(5 > 1)
result2

result | result2
result &  result2

isTRUE(result)




while(TRUE){
  print("Hello")
}


counter <- 1
while(counter < 12) {
  print(counter)
  counter <- counter + 1
}




counter <- 1
while(counter < 12) {
  print(counter)
  counter <- counter + 1
}

for(i in 1:5) {
  print("Hello R")
}

greeting <- "Hello R"
for(i in 5:10) {
  message <- paste(greeting, i)
  print(message)
}




#---- -2 ---- -1 ---- 0 ---- 1 ---- 2 ----

rm(answer)
x <- rnorm(1)
if(x > 1){
  answer <-"Greater than 1"
} else if(x >= -1){
    answer <- "Betwen -1 and 1"
} else{
    answer <- "Less than -1"
}




MyFirstVector <- c(3, 45, 56, 732)
MyFirstVector
is.numeric(MyFirstVector)
is.integer(MyFirstVector)
is.double(MyFirstVector)

V2 <- c(3L, 12L, 243L, 0L)
is.numeric(V2)
is.integer(V2)
is.double(V2)

V3 <- c("a", "B23", "Hello", 7)
V3
is.character(V3)
is.numeric(V3)

seq() #sequence - like ':'
rep() #replicate

seq(1,15)
1:15

seq(1,15,2)
z <- seq(1,15,4)
z

rep(3, 50)
d <- rep(3, 50)
rep("a",5)

x <- c(80,20)
y <- rep(x, 10)
y



x <- c(1, 123, 534, 13, 4)  #combine
y <- seq(201, 250, 11)   #sequence
z <- rep("Hi!", 3)   #replicate


w <- c("a", "b", "c", "d", "e")
w

w[1]
w[2]
w[3]
w[-1]
w[-3]
v <- w[-3]
w[1:3]
w[3:5]
w[c(1,3,5)]
w[c(-2,-4)]
w[-3:-5]
w[11]


q <- c("a","b","c")
r <- c("one","two","three")
s <- c(q, r)

r1



x <- rnorm(5)
x

#R-specific programming loop (shortcut becuase vectors)
for(i in x){
  print(i)
}

print(x[1])
print(x[2])
print(x[3])
print(x[4])
print(x[5])

#conventional programming loop
for(j in 1:5){
  print(x[j])
}

#--------2nd part of lesson

N <- 1000000
a <- rnorm(N)
b<- rnorm(N)

#vectorized approach
c <- a * b

#de-vectorized approach
d <- rep(NA,N)
for(i in 1:N){
  d[i] <- a[i] * b[i]
}





rnorm(5, sd=8)

x <- c("a", "b", "c")

c()
A <- seq(from=10, to=20, along.with=x)
rep(5:6, each=10)

?print()

is.numeric()
is.integer()
is.double()
is.character()

typeof()

B <- sqrt(A)
paste()

#?



install.packages("ggplot2")


?qplot()
?ggplot()
?diamonds

library(ggplot2)


qplot(data=diamonds, carat, price, colour=clarity, facets=.~clarity)

Games



#matrix()
?matrix
my.data <- 1:20
my.data

A <- matrix(my.data, 4, 5)
A
A[2,3]

B <- matrix(my.data, 4, 5, byrow=T)
B
B[2,5]


#rbind()
r1 <- c("I", "am", "happy")
r2 <- c("what","a","day")
r3 <- c(1,2,3)
C <- rbind(r1, r2, r3)
C


#cbind()
c1 <- 1:5
c2 <- -1:-5
D <- cbind(c1, c2)
D

#Named Vectors
Charlie <- 1:5
Charlie

#give names
names(Charlie) <- c("a","b","c","d","e")
Charlie
Charlie["d"]
names(Charlie)

#clear names
names(Charlie) <- NULL
Charlie

#Naming matrix dimensions 1
temp.vec <- rep(c("a","B","zZ"), each=3)
temp.vec

Bravo <- matrix(temp.vec,3, 3)
Bravo

rownames(Bravo) <- c("How", "are", "you?")
Bravo

colnames(Bravo) <- c("X","Y","Z")
Bravo

Bravo["are","Y"] <- 0


rownames(Bravo)





Games
rownames(Games)
colnames(Games)
Games["LeBronJames","2012"]

FieldGoals

round(FieldGoals / Games,1)

round(MinutesPlayed / Games)

round(FieldGoals / FieldGoalAttempts, 2)

round(Points / MinutesPlayed, 2)

round(Points / Games, 0)

round(Salary / Games, 0)

round(Salary / MinutesPlayed, 0)

round(Salary / Points, 0)

round(Salary / FieldGoals, 0)
Salary["KobeBryant",]/1000
Games["KobeBryant",]
Games





FieldGoals
t(FieldGoals)

?matplot
matplot(t(FieldGoals/FieldGoalAttempts), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)

matplot(t(FieldGoals / Games), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)

matplot(t(MinutesPlayed / Games), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)

matplot(t(Points / MinutesPlayed), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)

matplot(t(Points / Games), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)

matplot(t(Salary / Games), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)

matplot(t(Salary / MinutesPlayed), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)

matplot(t(Salary / Points), type="b", pch=15:18, col=c(1:4,6))
legend("bottomleft", inset=0.01, legend=Players, col=c(1:4,6), pch=15:18, horiz=F)




x <- c("a","b","c","d","e")
x
x[c(1,5)]
x[1]


Games
Games[1:3,6:10]
Games[c(1,10),]
Games[,c("2008","2009")]
Games[1,]   #named vector, not a matrix
Games[1,5]   #vector of 1

is.matrix(Games[1,])
is.vector(Games[1,])


Games[1,,drop=F] #now it's a matrix
Games[1,5,drop=F] #now it's a matrix


myplot <- function(data, rows=1:10){
  Data <- data[rows,,drop=F]
  matplot(t(Data), type="b", pch=15:18, col=c(1:4,6))
  legend("bottomleft", inset=0.01, legend=Players[rows], col=c(1:4,6), pch=15:18, horiz=F)
}

myplot(Salary)


