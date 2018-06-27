# Examine hank_years and hank_charts
hank_years
hank_charts

hank_years %>% 
  # Reorder hank_years alphabetically by song title
  arrange(song) %>% 
  # Select just the year column
  select(year) %>% 
  # Bind the year column
  bind_cols(hank_charts) %>% 
  # Arrange the finished dataset
  arrange(year, song)

# Make combined data frame using data_frame()
data_frame("year" = hank_year, "song" = hank_song, "peak" = hank_peak) %>% 
  # Extract songs where peak equals 1
  filter(peak == 1)

# Convert the hank list into a data frame
as_data_frame(hank) %>% 
  # Extract songs where peak equals 1
  filter(peak == 1)

# Examine the contents of michael
michael

bind_rows(michael, .id = "album") %>% 
  group_by(album) %>% 
  mutate(rank = min_rank(peak)) %>% 
  filter(rank == 1) %>% 
  select(-rank, -peak)

seventies %>% 
  # Coerce seventies$year into a useful numeric
  mutate(year = as.numeric(as.character(year))) %>% 
  # Bind the updated version of seventies to sixties
  bind_rows(sixties) %>% 
  arrange(year)

# Load the tibble package
library(tibble)

stage_songs %>% 
  # Add row names as a column named song
  rownames_to_column("song") %>% 
  # Left join stage_writers to stage_songs
  left_join(stage_writers, by = "song")
library(dplyr)
?rename


movie_years %>% 
  # Left join movie_studios to movie_years
  left_join(movie_studios, by = "movie") %>% 
  # Rename the columns: artist and studio
  rename(artist = name.x, studio = name.y)

# Identify the key columns
movie_directors
movie_years

movie_years %>% 
  # Left join movie_directors to movie_years
  left_join(movie_directors, by = c("movie" = "name")) %>% 
  # Arrange the columns using select()
  select(year, movie, artist = name, director, studio)

# Load the purrr library
library(purrr)

# Place supergroups, more_bands, and more_artists into a list
list(supergroups, more_bands, more_artists) %>% 
  # Use reduce to join together the contents of the list
  reduce(left_join, by = c("first", "last"))

list(more_artists, more_bands, supergroups) %>% 
  # Return rows of more_artists in all three datasets
  reduce(semi_join,by = c("first", "last"))

install.packages("Lahman")
library(Lahman)

# Examine lahmanNames
lahmanNames

# Find variables in common
lahmanNames %>%
  reduce(intersect)

lahmanNames %>%  
  # Bind the data frames in lahmanNames
  bind_rows(.id = "dataframe") %>%
  # Group the result by var
  group_by(var) %>%
  # Tally the number of appearances
  tally() %>%
  # Filter the data
  filter(n > 1) %>% 
  # Arrange the results
  arrange(desc(n))

lahmanNames %>% 
  # Bind the data frames
  bind_rows(.id = "dataframe") %>%
  # Filter the results
  filter(var == "playerID") %>% 
  # Extract the dataframe variable
  `$`(dataframe)

players <- Master %>% 
  # Return one row for each distinct player
  distinct(playerID, nameFirst, nameLast)

players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>%
  # Count them
  count()

players %>% 
  anti_join(Salaries, by = "playerID") %>% 
  # How many unsalaried players appear in Appearances?
  semi_join(Appearances, by = "playerID") %>% 
  count()


players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>% 
  # Join them to Appearances
  left_join(Appearances, by = "playerID") %>% 
  # Calculate total_games for each player
  group_by(playerID) %>%
  summarize(total_games = sum(G_all, na.rm = TRUE)) %>%
  # Arrange in descending order by total_games
  arrange(desc(total_games))

players %>%
  # Find unsalaried players
  anti_join(Salaries, by = "playerID") %>% 
  # Join Batting to the unsalaried players
  left_join(Batting, by = "playerID") %>% 
  # Group by player
  group_by(playerID) %>% 
  # Sum at-bats for each player
  summarize(total_at_bat = sum(AB, na.rm = TRUE)) %>% 
  # Arrange in descending order
  arrange(desc(total_at_bat))

