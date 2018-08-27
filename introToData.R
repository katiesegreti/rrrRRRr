library(openintro)

data(hsb2)
str(hsb2)
glimpse(hsb2)

table(hsb2$schtyp)

hsb2_public <- hsb2 %>%
  filter(schtyp == "public")

table(hsb2_public$schtyp)
hsb2_public$schtyp <- droplevels(hsb2_public$schtyp)
table(hsb2_public$schtyp)

#discretize a variable
#ie convert score to aboveavg or belowavg

(avg_read <- mean(hsb2$read))

hsb2 <- hsb2 %>%
  mutate(read_cat = ifelse(read < avg_read,
                           "below average", "at or above average"))

# # Create number_yn column in email50
# email50_fortified <- email50 %>%
#   mutate(number_yn = case_when(
#     number == "none" ~ "no", # if number is "none", make number_yn "no"
#     number != "none" ~ "yes"  # if number is not "none", make number_yn "yes"
#   )
#   )
# 
# 
# # Visualize number_yn
# ggplot(email50_fortified, aes(x = number_yn)) +
#   geom_bar()

ggplot(data = hsb2, aes(x = science, y = math, color = prog)) + 
  geom_point()

# OBSERVATIONAL STUDIES VS EXPERIMENTS
# observational: dont directly interfere when collecting data
# observational:only correlation can be inferred
# experiment: causation can be inferred
