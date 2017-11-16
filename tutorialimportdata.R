
?read.csv

#method1:select the file manually
stats <- read.csv(file.choose())
stats

#method 2: set WD and read data
getwd()
#windows:
setwd(".\\udemyR")
#mac:
setwd("./udemyR")

rm(stats)
stats <- read.csv("DemographicData.csv")
stats


#--------------------------------exploringdata
stats
nrow(stats)
#imported 195 rows
ncol(stats)
head(stats, n=10)
tail(stats)
str(stats)
summary(stats)


#------- using the $
head(stats)
stats[3,3]
stats[3, "Birth.rate"]
stats$Internet.users
stats$Internet.users[2]
stats[,"Internet.users"]    #same as stats$Internet.users
levels(stats$Income.Group)

#--------------- basic operations with a data frame
stats[1:10,] #subsetting

is.data.frame(stats[1,])  #it's still a dataframe if you extract just one row
is.data.frame(stats[,1,drop=F]) #need the drop=F for one column to make it a dataframe

#multiply columns
stats$Birth.rate * stats$Internet.users
stats$Birth.rate + stats$Internet.users
#add column
stats$MyCalc <- stats$Birth.rate * stats$Internet.users
head(stats)
#test of knowledge
stats$xyz <- 1:5
head(stats, n=12)
#remove a column
stats$MyCalc <- NULL
stats$xyz <- NULL

for(i in 1:nrow(stats)){
  if(stats$Birth.rate[i]>20){
    stats$xyz[i] <- 0
  } else{
    stats$xyz[i] <- 1
  }
}

for(i in 1:nrow(stats)){
  if(stats$Country.Name[i]=="Aruba"){
    stats$abc[i] <- "ARUBA"
  }
}



#--------------------------filtering data frames
head(stats)
filter <- stats$Internet.users < 2
stats[filter,]


stats[stats$Birth.rate > 40 & stats$Internet.users < 2,]
stats[stats$Income.Group == "High income",]
levels(stats$Income.Group)

stats[stats$Country.Name == "Malta",]


#----------------introduction to qplot()
install.packages("ggplot2")
library(ggplot2)
?qplot
qplot(data=stats, x=Internet.users)

qplot(data=stats, x=Income.Group, y=Birth.rate, size=I(3),
      colour=I("blue"))
qplot(data=stats, x=Income.Group, y=Birth.rate, geom="boxplot")

#----------------VISUALIZING WHAT WE NEED
qplot(data=stats, x=Internet.users, y=Birth.rate,
      colour=I("red"),size=I(4))

qplot(data=stats, x=Internet.users, y=Birth.rate,
      colour=Income.Group,size=I(5))


#----------------creating data frames
#mydf <- data.frame(Countries_2012_Dataset, Codes_2012_Dataset, Regions_2012_Dataset)
#head(mydf)
#colnames(mydf) <- c("Country","Code","Region")
rm(mydf)

mydf <- data.frame(Country=Countries_2012_Dataset, Codes=Codes_2012_Dataset, 
                   Regions=Regions_2012_Dataset)
head(mydf)
tail(mydf)
summary(mydf)

#----------------merging data frames
head(stats)
head(mydf)

merged <- merge(stats, mydf,by.x="Country.Code", by.y="Codes")
head(merged)

merged$Country <- NULL

#---------------- visualizing with new split
qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Regions,size=I(5))

#1. Shapes
qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Regions,size=I(5),shape=I(8))
#2. Transparency
qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Regions,size=I(5),shape=I(19),
      alpha=I(0.6))
#3. Titles
qplot(data=merged, x=Internet.users, y=Birth.rate,
      colour=Regions,size=I(5),shape=I(19),
      alpha=I(0.6), main="Birth rate vs. internet users")



