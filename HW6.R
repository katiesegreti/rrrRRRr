getwd()
setwd(".\\udemyR")

movies <- read.csv("Section6-Homework-Data.csv")
head(movies)
colnames(movies) <- c("Day", "Director", "Genre", "Title", "ReleaseDate", "Studio", "AdjustedGrossMillions",
                      "BudgetMillions", "GrossMillions", "IMDBrating", "MovieLensRating", "OverseasMillions", "OverseasPercent",
                      "ProfitMillions", "ProfitPercent", "Runtime", "USMillions", "GrossUSPercent")
tail(movies)
str(movies)
summary(movies)

library(ggplot2)

levels(movies$Studio)
levels(movies$Genre)

summary(movies$Genre)
summary(movies$Studio)

studios <- c("Buena Vista Studios", "Fox", "Paramount Pictures", "Sony", "Universal", "WB")
genres <- c("action", "adventure", "animation", "comedy", "drama")

#filter dataframe to needed studios and genres
moviesFiltered <- movies[movies$Studio %in% studios & movies$Genre %in% genres,] 


#
u <- ggplot(data=moviesFiltered, aes(x=Genre, y=GrossUSPercent))




u + geom_jitter(aes(color=Studio, size=BudgetMillions))

u + geom_boxplot(alpha=0.5)

u + geom_jitter(aes(color=Studio, size=BudgetMillions)) + geom_boxplot(alpha=0.5, size=0.5)


u + geom_jitter(aes(size=BudgetMillions, color=Studio)) + geom_boxplot(size=.05, alpha=0.5)




