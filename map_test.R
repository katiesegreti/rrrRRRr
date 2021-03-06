library(ggmap)
corvallis <- c(lon = -123.2620, lat = 44.5646)

key <- ""

register_google(key = key)

# Get map at zoom level 5: map_5
map_5 <- get_map(corvallis, zoom = 5, scale = 1)

# Plot map at zoom level 5
ggmap(map_5)

# Get map at zoom level 13: corvallis_map
corvallis_map <- get_map(corvallis, zoom = 13, scale = 1, api_key = api_key)

# Plot map at zoom level 13
ggmap(corvallis_map)

has_goog_account()
has_goog_key()
goog_account()
has_goog_signature()

# Add source and maptype to get toner map from Stamen Maps
corvallis_map_bw <- get_map(corvallis, zoom = 13, source = "stamen", maptype = "toner")





if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")
ggmap(get_googlemap())
register_google(key = "")
geocode("waco texas")
