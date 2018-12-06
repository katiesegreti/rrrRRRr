library(ggmap)

corvallis <- c(lon = -123.2620, lat = 44.5646)
lynbrook <- c(lon = -73.6718, lat = 40.6548)

key <- "AIzaSyAEJYmlp-jKgR1SQGuv-lbcZMYNuJ9Xh5g"

register_google(key = key)

# Get map at zoom level 5: map_5
map_5 <- get_map(corvallis, zoom = 5, scale = 1)
lb_map <- get_map(lynbrook, zoom = 13, scale = 2)

# Plot map at zoom level 5
ggmap(map_5)
ggmap(lb_map)
# Get map at zoom level 13: corvallis_map
corvallis_map <- get_map(corvallis, zoom = 13, scale = 1, api_key = api_key)

# Plot map at zoom level 13
ggmap(corvallis_map)

# Map color to year_built
ggmap(corvallis_map) +
  geom_point(aes(lon, lat, color = year_built), data = sales)

# Map size to bedrooms
ggmap(corvallis_map) +
  geom_point(aes(lon, lat, size = bedrooms), data = sales)

# Map color to price / finished_squarefeet
ggmap(corvallis_map) +
  geom_point(aes(lon, lat, color = price / finished_squarefeet), data = sales)



corvallis <- c(lon = -123.2620, lat = 44.5646)

# Add a maptype argument to get a satellite map
corvallis_map_sat <- get_map(corvallis, zoom = 13, maptype = "satellite")

# Edit to display satellite map
ggmap(corvallis_map_sat) +
  geom_point(aes(lon, lat, color = year_built), data = sales)

# Add source and maptype to get toner map from Stamen Maps
corvallis_map_bw <- get_map(corvallis, zoom = 13, source = "stamen", maptype = "toner")

# Edit to display toner map
ggmap(corvallis_map_bw) +
  geom_point(aes(lon, lat, color = year_built), data = sales)


# Use base_layer argument to ggmap() to specify data and x, y mappings
ggmap(corvallis_map_bw,
      base_layer = ggplot(sales, aes(lon, lat))) +
  geom_point(aes(color = year_built))

# Plot house sales using qmplot()
qmplot(lon, lat, data = sales,
       geom = "point", color = bedrooms) +
  facet_wrap(~month)

# Add a point layer with color mapped to ward
ggplot(ward_sales, aes(lon, lat)) +
  geom_point(aes(color = ward))


# Add a point layer with color mapped to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_point(aes(color = group))


# Add a path layer with group mapped to group
ggplot(ward_sales, aes(lon, lat))  +
  geom_path(aes(group = group))


# Add a polygon layer with fill mapped to ward, and group to group
ggplot(ward_sales, aes(lon, lat)) +
  geom_polygon(aes(fill = ward, group = group))

# Fix the polygon cropping
ggmap(corvallis_map_bw, 
      base_layer = ggplot(ward_sales, aes(lon, lat)), extent = "normal", maprange = FALSE) +
  geom_polygon(aes(group = group, fill = ward))

# Repeat, but map fill to num_sales
ggmap(corvallis_map_bw, 
      base_layer = ggplot(ward_sales, aes(lon, lat)), extent = "normal", maprange = FALSE) +
  geom_polygon(aes(group = group, fill = num_sales))

# Repeat again, but map fill to avg_price
ggmap(corvallis_map_bw, 
      base_layer = ggplot(ward_sales, aes(lon, lat)), extent = "normal", maprange = FALSE) +
  geom_polygon(alpha = 0.8, aes(group = group, fill = avg_price))

preds <- readRDS("~/datacamp_R/01_corv_predicted_grid.rds")

# Add a geom_point() layer
ggplot(preds, aes(lon, lat)) +
  geom_point()

# Add a tile layer with fill mapped to predicted_price
ggplot(preds, aes(lon, lat)) +
  geom_tile(aes(fill = predicted_price))

# Use ggmap() instead of ggplot()
ggmap(corvallis_map_bw) +
  geom_tile(data = preds, alpha = 0.8, aes(lon, lat, fill = predicted_price))

#SP package
library(sp)
countries_sp <- readRDS("~/datacamp_R/02_countries_sp.rds")
# Print countries_sp
print(countries_sp)

# Call summary() on countries_sp
summary(countries_sp)

# Call plot() on countries_sp
plot(countries_sp)
# Call str() on countries_sp
str(countries_sp)

# Call str() on countries_sp with max.level = 2
str(countries_sp, max.level = 2)

countries_spdf <- readRDS("~/datacamp_R/02_countries_spdf.rds")

# Call summary() on countries_spdf and countries_sp
summary(countries_sp)
summary(countries_spdf)

# Call str() with max.level = 2 on countries_spdf
str(countries_spdf, max.level = 2)

# Plot countries_spdf
plot(countries_spdf)

# Subset the 169th object of countries_spdf: usa
usa <- countries_spdf[169, ]

# Look at summary() of usa
summary(usa)

# Look at str() of usa
str(usa, max.level = 2)

# Call plot() on usa
plot(usa)
str(usa@polygons)

# Call head() and str() on the data slot of countries_spdf
head(countries_spdf@data)
str(countries_spdf@data)

# Pull out the name column using $
countries_spdf$name

# Pull out the subregion column using [[
countries_spdf[["subregion"]]

# Create logical vector: is_nz
is_nz <- countries_spdf$name == "New Zealand"

# Subset countries_spdf using is_nz: nz
nz <- countries_spdf[is_nz, ]

# Plot nz
plot(nz)

library(sp)
library(tmap)

# Use qtm() to create a choropleth map of gdp
qtm(shp = countries_spdf, fill = "gdp")

# Add style argument to the tm_fill() call
tm_shape(countries_spdf) +
  tm_fill(col = "population", style = "quantile") +
  # Add a tm_borders() layer 
  tm_borders(col = "burlywood4")

# New plot, with tm_bubbles() instead of tm_fill()
tm_shape(countries_spdf) +
  tm_bubbles(size = "population", style = "quantile") +
  # Add a tm_borders() layer 
  tm_borders(col = "burlywood4")

# Switch to a Hobo–Dyer projection
tm_shape(countries_spdf, projection ="hd") +
  tm_grid(n.x = 11, n.y = 11) +
  tm_fill(col = "population", style = "quantile")  +
  tm_borders(col = "burlywood4") 

# Switch to a Robinson projection
tm_shape(countries_spdf, projection = "robin") +
  tm_grid(n.x = 11, n.y = 11) +
  tm_fill(col = "population", style = "quantile")  +
  tm_borders(col = "burlywood4") 

# Add tm_style_classic() to your plot

tm_shape(countries_spdf, projection = "robin") +
  tm_grid(n.x = 11, n.y = 11) +
  tm_fill(col = "population", style = "quantile")  +
  tm_borders(col = "burlywood4") +
  tm_style_classic()

library(sp)
library(tmap)

# Plot from last exercise
tm_shape(countries_spdf) +
  tm_grid(n.x = 11, n.y = 11, projection = "longlat") +
  tm_fill(col = "population", style = "quantile")  +
  tm_borders(col = "burlywood4")

# Save a static version "population.png"
save_tmap(filename = "population.png")

# Save an interactive version "population.html"
save_tmap(filename = "population.html")

library(raster)
pop <- readRDS("~/datacamp_R/03-population.rds")

# Print pop
print(pop)

# Call str() on pop, with max.level = 2
str(pop, max.level = 2)

# Call summary on pop
summary(pop)

# Call plot() on pop
plot(pop)

# Call str() on values(pop)
str(values(pop))

# Call head() on values(pop)
head(values(pop))

#population by age
pop_by_age <- readRDS("~/datacamp_R/03-population-by-age.rds")
# Print pop_by_age
print(pop_by_age)

# Subset out the under_1 layer using [[
pop_by_age[["under_1"]]

# Plot the under_1 layer
plot(pop_by_age[["under_1"]])

# Specify pop as the shp and add a tm_raster() layer
tm_shape(shp = pop) +
  tm_raster()

# Plot the under_1 layer in pop_by_age
tm_shape(shp = pop_by_age) +
  tm_raster(col = "under_1")


library(rasterVis)
# Call levelplot() on pop
levelplot(pop)

library(RColorBrewer)
# 9 steps on the RColorBrewer "BuPu" palette: blups
blups <- brewer.pal(9, name = "BuPu")

# Add scale_fill_gradientn() with the blups palette
ggplot(preds) +
  geom_tile(aes(lon, lat, fill = predicted_price), alpha = 0.8) +
  scale_fill_gradientn(colors = blups)


library(viridisLite)
# viridisLite viridis palette with 9 steps: vir
vir <- viridis(n = 9)

# Add scale_fill_gradientn() with the vir palette
ggplot(preds) +
  geom_tile(aes(lon, lat, fill = predicted_price), alpha = 0.8) +
  scale_fill_gradientn(colors = vir)

# mag: a viridisLite magma palette with 9 steps
mag = magma(n = 9)

# Add scale_fill_gradientn() with the mag palette
ggplot(preds) +
  geom_tile(aes(lon, lat, fill = predicted_price), alpha = 0.8) +
  scale_fill_gradientn(colors = mag)

prop_by_age <- readRDS("~/datacamp_R/03-proportion-by-age.rds")
# Use the blups palette
tm_shape(prop_by_age) +
  tm_raster("age_18_24", palette = blups) +
  tm_legend(position = c("right", "bottom"))

# Use the vir palette
tm_shape(prop_by_age) +
  tm_raster("age_18_24", palette = rev(vir)) +
  tm_legend(position = c("right", "bottom"))

# Use the mag palette but reverse the order
tm_shape(prop_by_age) +
  tm_raster("age_18_24", palette = rev(mag)) +
  tm_legend(position = c("right", "bottom"))

mag <- viridisLite::magma(7)

library(classInt)

# Create 5 "pretty" breaks with classIntervals()
classIntervals(values(prop_by_age[["age_18_24"]]), n = 5, style = "pretty")


# Create 5 "quantile" breaks with classIntervals()
classIntervals(values(prop_by_age[["age_18_24"]]), n = 5, style = "quantile")


# Use 5 "quantile" breaks in tm_raster()
tm_shape(prop_by_age) +
  tm_raster("age_18_24", palette = mag, n = 5, style = "quantile") +
  tm_legend(position = c("right", "bottom"))

# Create histogram of proportions
hist(values(prop_by_age[["age_18_24"]]))

# Use fixed breaks in tm_raster()
tm_shape(prop_by_age) +
  tm_raster("age_18_24", palette = mag,
            style = "fixed", breaks = c(0.025, 0.05, 0.1, 0.2, 0.25, 0.3, 1))

# Save your plot to "prop_18-24.html"
save_tmap(filename = "prop_18-24.html")

