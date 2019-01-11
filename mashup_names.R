library(stringr)
library(babynames)
library(rebus)
library(dplyr)
library(Hmisc)

babynames1 <- babynames
# Extracting vectors for boys' and girls' names
babynames_2015 <- filter(babynames1, year == 2015)
all_names <- tolower(babynames_2015$name)
boy_names <- tolower(filter(babynames_2015, sex == "M")$name)
girl_names <- tolower(filter(babynames_2015, sex == "F")$name)

# Use `negated_char_class()` for everything but e
not_e <- negated_char_class("e")
# Vowels 
vowels <- char_class("aeiouAEIOU")
# Use `negated_char_class()` for everything but vowels
not_vowels <- negated_char_class("aeiouAEIOU")


#create last n letters pattern
last_n_pattern <- function(name1, n) {
  name1 = tolower(name1)
  if(n == 2) {
    str_extract(name1, pattern = LOWER %R% LOWER %R% END)
  } else if(n == 3) {
    str_extract(name1, pattern = LOWER %R% LOWER %R% LOWER %R% END)
  } else if(n == 4) {
    str_extract(name1, pattern = LOWER %R% LOWER %R% LOWER %R% LOWER %R% END)
  }
}



#create first n letters pattern
first_n_pattern <- function(name1, n) {
  name1 = tolower(name1)
  if(n == 2) {
    str_extract(name1, pattern = START %R% LOWER %R% LOWER)
  } else if(n == 3) {
    str_extract(name1, pattern = START %R% LOWER %R% LOWER %R% LOWER)
  } else if(n == 4) {
    str_extract(name1, pattern = START %R% LOWER %R% LOWER %R% LOWER %R% LOWER)
  }
}

#make names by calling the pattern functions
pattern_names <- function(startname, first_or_last, n) {
  if(first_or_last == "first") {
    pttrn = first_n_pattern(startname, n)
    match_names = str_subset(all_names, pattern = pttrn)
    names_splt = str_split(match_names, pattern = pttrn, simplify = TRUE)
    if(length(names_splt) > 0) {name_start = names_splt[,1]
    name_start = names_splt[str_length(name_start) > 0, 1]
    return(capitalize(unique(str_c(name_start, startname))))}
  } else if(first_or_last == "last") {
    pttrn = last_n_pattern(startname, n)
    match_names = str_subset(all_names, pattern = pttrn)
    names_splt = str_split(match_names, pattern = pttrn, simplify = TRUE)
    if(length(names_splt > 0)) {name_end = names_splt[,2]
    name_end = names_splt[str_length(name_end) > 0, 2]
    return(capitalize(unique(str_c(startname, name_end))))
    }
  }
}
#curate names depending on name length
mashup_names <- function(startname) {
  if(str_length(startname) > 4) {
    results <- unique(c(pattern_names(startname, "last", 4), pattern_names(startname, "last", 3),
             pattern_names(startname, "first", 4), pattern_names(startname, "first", 3)))
  }
  else if(str_length(startname) > 3){
    results <- unique(c(pattern_names(startname, "last", 3), pattern_names(startname, "first", 3)))
  }
  else if(str_length(startname) > 2) {
    results <- unique(c(pattern_names(startname, "last", 2), pattern_names(startname, "first", 2)))
  }
  if(length(results) == 0) {
    return("Sorry, I didn't find any names to mash that up with! Try a different name?")
  }
  else {
    return(results)
  }
} 
