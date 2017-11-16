
ages <- c(1:100)

minimumAge <- ages / 2 + 7

ageDifference <- ages - minimumAge

ageMatrix <- data.frame(ages, minimumAge, ageDifference)

ageMatrixfiltered <- ageMatrix[ageMatrix$ages > 19,]

isThisAppropriate <- function(yourAge, theirAge){
  diff <- yourAge - theirAge
  if(theirAge >= yourAge / 2 + 7){
    return("They are old enough for you now")
  }else{
    for(i in yourAge+1:100){
      if(diff ==  i - (i / 2 + 7)){
        years <- i - yourAge
        return(sprintf("They will be old enough for you in %i years when you are %i and they are %i", 
                       years, i, (theirAge + years)) )
      }
    }
  }
}

isThisAppropriate(35,24)
isThisAppropriate(36,24)








