getwd()
setwd(".\\udemyR")

movies <- read.csv("Movie-Ratings.csv")
colnames(movies) <- c("Film", "Genre", "CriticRating", "AudienceRating", "BudgetMillions", "Year")
head(movies)
tail(movies)
str(movies)
summary(movies)
factor(movies$Year)
movies$Year <- factor(movies$Year)

library(ggplot2)


#------------Aesthetics
library(ggplot2)
library(knitr)
library(markdown)
library(ghibli)


ggplot(data=movies, aes(x=CriticRating, y=AudienceRating))

#add geometry
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating)) + 
  geom_point()

#add color
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, 
                        colour=Genre)) + 
  geom_point()

#add size
ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, 
                        colour=Genre, size=BudgetMillions)) + 
  geom_point()


#-------plotting with layers

p <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, 
                             colour=Genre, size=BudgetMillions))

p + geom_point()

#lines
p + geom_line()


#multiple layers
p + geom_point(size=) + geom_line(size=1) 
p  + geom_line(size=1) + geom_point(size=3)


#--------------overriding aesthetics

q <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating,
                             colour=Genre, size=BudgetMillions))

#add geom layer
q + geom_point()

#overriding aes
#example 1
q + geom_point(aes(size=CriticRating))


#example 2
q + geom_point(aes(colour=BudgetMillions))

#reduce line size
q + geom_line(size=1) + geom_point()

#ex3
q + geom_point(aes(x=BudgetMillions)) + 
  xlab("Budget Millions $$$")


#ex4
p  + geom_line() + geom_point()
#reduce line size
p  + geom_line(size=1) + geom_point()

##------function
roundup <- function(df1){
  genre <- levels(df1$Genre)
  z <- rep(0, length(genre))
  #create matrix
  m1 <- matrix(z, nrow=length(genre), ncol=1)
  rownames(m1) <- genre
  colnames(m1) <- "Total"
  #fill matrix
  for(i in 1:length(genre)){
    m1[i,1] <- nrow(df1[df1$Genre==genre[i],])
  }
 
  m2 <- m1[order(m1[,1],decreasing=TRUE),]
  return(m2)
}

roundup(movies)


##------function
roundup2 <- function(df1){
  genre <- levels(df1$Genre)
  years <- levels(df1$Year)
  colz <- c(years, "Total")
  z <- rep(0, length(genre)*length( colz))
  #create matrix
  m1 <- matrix(z, nrow=length(genre), ncol=length(colz))
  rownames(m1) <- genre
  colnames(m1) <- colz
  #fill matrix
  for(i in 1:length(genre)){
    for(j in 1:length(years)){
      m1[i,j] <- nrow(df1[df1$Genre==genre[i]&df1$Year==years[j],])
    }
    m1[i,length(colz)] <- nrow(df1[df1$Genre==genre[i],])
  }
  

  return(m1)
}

roundup2(movies)



#---- mapping vs setting

r <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating))
r + geom_point()

#add color
#1 mapping(what we've done so far)
r + geom_point(aes(color=Genre))
#2 Setting
r + geom_point(color="DarkGreen")

#1 mapping
r + geom_point(aes(size=BudgetMillions))
#2 setting
r + geom_point(size=10)


#----------histograms and density charts
s <- ggplot(data=movies, aes(x=BudgetMillions))
s + geom_histogram(binwidth=10)


#add color

s + geom_histogram(binwidth=10, aes(fill=Genre))
#add a border
s + geom_histogram(binwidth=10, aes(fill=Genre), color="Black")

#sometimes you may need density charts;
s + geom_density(aes(fill=Genre))
s + geom_density(aes(fill=Genre), position="stack")

#-------------starting layer tips
t <- ggplot(data=movies, aes(x=AudienceRating))
t + geom_histogram(binwidth = 10, fill="White", color="Blue")

#another way:
t<- ggplot(data=movies)
t + geom_histogram(binwidth = 10,
                   aes(x=AudienceRating),
                   fill="White", color="Blue")
t + geom_histogram(binwidth = 10,
                   aes(x=CriticRating),
                   fill="White", color="Blue")
#  chart 4

#-------Statistical transformations
?geom_smooth
u <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, color=Genre))
u + geom_point() + geom_smooth(fill=NA)


#boxplots
u <- ggplot(data=movies, aes(x=Genre, y=AudienceRating, color=Genre))
u + geom_boxplot()
u + geom_boxplot(size=1.2)
u + geom_boxplot(size=1.2) + geom_point()
#tip/hack
u + geom_boxplot(size=1.2) + geom_jitter()
#another way:
u + geom_jitter() + geom_boxplot(size=1.2, alpha=0.5)


u <- ggplot(data=movies, aes(x=Genre, y=CriticRating, color=Genre))


#----- using facets
v <- ggplot(data=movies, aes(x=BudgetMillions))
v + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black")

#facets:
v + geom_histogram(binwidth = 10, aes(fill=Genre), colour="Black") +
  facet_grid(Genre~., scales="free")

#scatterplots
w <- ggplot(data=movies, aes(x=CriticRating, y = AudienceRating, color=Genre))
w + geom_point(size=3)
#facets
w + geom_point(size=3) + facet_grid(Genre~.)
w + geom_point(size=3) + facet_grid(.~Year)
w + geom_point(size=3) + facet_grid(Genre~Year)
w + geom_point(size=3) + geom_smooth() + facet_grid(Genre~Year)
w + geom_point(aes(size=BudgetMillions)) + geom_smooth() + facet_grid(Genre~Year)


#coordinates
#limits
#zoom

m <- ggplot(data=movies, aes(x=CriticRating, y=AudienceRating, size=BudgetMillions, color=Genre))
m + geom_point()
m + geom_point() + xlim(50,100) + ylim(50, 100)

#won't work well always
n <- ggplot(data=movies, aes(x=BudgetMillions))
n + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black")
n + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black") + ylim(0,50)

#instead, zoom
n <- ggplot(data=movies, aes(x=BudgetMillions))
n + geom_histogram(binwidth = 10, aes(fill=Genre), color="Black") + coord_cartesian(ylim=c(0,50))

w + geom_point(aes(size=BudgetMillions)) + geom_smooth() + facet_grid(Genre~Year) +
  coord_cartesian(ylim=c(0,100))


#-----------THEME
o <- ggplot(data=movies, aes(x=BudgetMillions))
h <- o + geom_histogram(binwidth=10, aes(fill=Genre), color="Black")
h

# axes labels
h + xlab("Money Axis") +
  ylab("Number of Movies")

#label formatting
h + xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=30),
        axis.title.y = element_text(color="Red", size=30))

#tick mark formatting
h + xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=30),
        axis.title.y = element_text(color="Red", size=30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20))

?theme

#legend formatting
h + xlab("Money Axis") +
  ylab("Number of Movies") +
  theme(axis.title.x = element_text(color="DarkGreen", size=30),
        axis.title.y = element_text(color="Red", size=30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=30),
        legend.text = element_text(size=20),
        legend.position = c(1,1),
        legend.justification = c(1,1))

#title
h + xlab("Money Axis") +
  ylab("Number of Movies") +
  ggtitle("Movie Budget Distribution") +
  theme(axis.title.x = element_text(color="DarkGreen", size=30),
        axis.title.y = element_text(color="Red", size=30),
        axis.text.x = element_text(size=20),
        axis.text.y = element_text(size=20),
        legend.title = element_text(size=30),
        legend.text = element_text(size=20),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="DarkBlue", size=40, family="Courier"))
