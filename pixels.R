library(ggplot2)
library(dplyr)
library(gridExtra)

#blank theme
pix_theme <- theme(panel.grid = element_blank(), 
                       panel.background = element_rect(fill = "white"),
                       axis.text = element_blank(), 
                       axis.ticks = element_blank(), 
                       axis.title = element_blank(),
                       legend.position = "none")

x <- rep(-7:7, each = 13)
y <- rep(0:12, times = 15)
c <- rep(0, 195)
heart <- data.frame(x, y, c)

ones_1 <- 1
zeroes_1 <- 7
ones_2 <- 5
zeroes_2a <- 2
zeroes_2b <- 1
for(i in 1:11) {
  if (i < 10) {
    heart$c[y == i] <- c(rep(0, zeroes_1), 
                    rep(1, ones_1), rep(0, zeroes_1))
    if(ones_1 < 13) {ones_1 <- ones_1 + 2}
    if(zeroes_1 > 1) {zeroes_1 <- zeroes_1 - 1}
    }
  if(i >= 10) {
    heart$c[y == i] <- c(rep(0, zeroes_2a), rep(1, ones_2),
                      rep(0, zeroes_2b), rep(1, ones_2),
                      rep(0, zeroes_2a))
    ones_2 <- ones_2 - 2
    zeroes_2a <- zeroes_2a + 1
    zeroes_2b <- zeroes_2b + 2
  }
}



ggplot(df, aes(x, y, fill = factor(c))) +
  geom_point(size = 14, shape = 21) +
  scale_fill_manual(values = c("white", "chartreuse")) +
  pix_theme


df %>%
  filter(c == 1) %>%
  ggplot(aes(x, y)) +
  geom_point(size = 15, shape = 22, fill = "dodgerblue") +
  pix_theme +
  xlim(-7, 7) +
  ylim(0, 12)

x <- rep(0:15, each = 16)
y <- rep(0:15, times = 16)
c <- rep(0, 256)
df <- data.frame(x, y, c)

df$c[y == 1] <- c(0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0)
df$c[y == 2] <- c(0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0)
for(i in 3:6) {
  df$c[y == i] <- c(0, rep(1, 14), 0)
}
df$c[y == 7] <- c(0, 1, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 3, 1, 1, 0)
df$c[y == 8] <- c(0, 1, 1, 1, 3, 3, 2, 2, 1, 1, 3, 3, 2, 2, 1, 0)
df$c[y == 9] <- c(0, 0, 1, 1, 3, 3, 2, 2, 1, 1, 3, 3, 2, 2, 0, 0)
df$c[y == 10] <- c(0, 0, 1, 1, 3, 3, 3, 3, 1, 1, 3, 3, 3, 3, 0, 0)
df$c[y == 11] <- c(0, 0, 1, 1, 1, 3, 3, 1, 1, 1, 1, 3, 3, 1, 0, 0)
df$c[y == 12] <- c(0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0)
df$c[y == 13] <- c(0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0)
df$c[y == 14] <- c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0)

df %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 13, shape = 22) +
  scale_fill_manual(values = c("red", "darkblue", "white")) +
  pix_theme +
  xlim(0, 15) +
  ylim(0, 15)

##jackolantern

x <- rep(1:13, each = 14)
y <- rep(1:14, times = 13)
c <- rep(0, 182)
df_pumpkin <- data.frame(x, y, c)

df_pumpkin$c[y == 1] <- c(0, 0, rep(1, 9), 0, 0)
df_pumpkin$c[y == 2] <- c(0, 1, 1, rep(2, 7), 1, 1, 0)
df_pumpkin$c[y == 3] <- c(1, 1, 2, 2, 1, 2, 2, 2, 1, 2, 2, 1, 1)
df_pumpkin$c[y == 4] <- c(1, 1, 2, rep(1, 7), 2, 1, 1)
df_pumpkin$c[y == 5] <- c(rep(1, 6), 2, rep(1, 6))
df_pumpkin$c[y == 6] <- c(1, 1, 1, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1)
df_pumpkin$c[y == 7] <- c(1, 1, 1, 2, rep(1, 5), 2, 1, 1, 1)
df_pumpkin$c[y == 8] <- c(0, rep(1, 11), 0)
df_pumpkin$c[y == 9] <- c(0, 0, 1, 1, rep(2, 5), 1, 1, 0, 0)
df_pumpkin$c[y == 10] <- c(0, 0, 0, 2, rep(3, 5), 2, 0, 0, 0)
df_pumpkin$c[y == 11] <- c(0, 0, 0, 0, rep(2, 5), 0, 0, 0, 0)
df_pumpkin$c[y == 12] <- c(rep(0, 5), 2, 2, 2, rep(0, 5))
df_pumpkin$c[y == 13] <- c(rep(0, 6), 2, 2, rep(0, 5))
df_pumpkin$c[y == 14] <- c(rep(0, 7), 2, 2, rep(0, 4))

df_pumpkin %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 13, shape = 22) +
  scale_fill_manual(values = c("orange", "black", "magenta")) +
  pix_theme +
  xlim(-1, 14) +
  ylim(0, 15)

####alien
x <- rep(1:19, each = 21)
y <- rep(1:21, times = 19)
c <- rep(0, 399)
alien <- data.frame(x, y, c)

alien$c[y == 1] <- c(rep(0, 8), 1, 1, 1, rep(0,8))
blank <- 7
green <- 3
for(i in 2:7) {
  alien$c[y == i] <- c(rep(0, blank), 1, rep(2, green), 1, rep(0, blank))
  blank <- blank - 1
  green <- green + 2
}
alien$c[y == 8] <- c(0, 0, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 0, 0)
alien$c[y == 9] <- c(0, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 2, 1, 0)
alien$c[y == 10] <- c(0, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 1, 0)
alien$c[y == 11] <- c(1, 2, rep(1, 6), 2, 2, 2, rep(1, 6), 2, 1)
alien$c[y == 12] <- c(1, 2, rep(1, 5), rep(2, 5), rep(1, 5), 2, 1)
alien$c[y == 13] <- c(1, 2, rep(1, 4), rep(2, 7), rep(1, 4), 2, 1)
alien$c[y == 14] <- c(1, rep(2, 17), 1)
alien$c[y == 15] <- c(0, 1, rep(2, 15), 1, 0)
alien$c[y == 16] <- c(0, 1, rep(2, 15), 1, 0)
alien$c[y == 17] <- c(0, 0, 1, rep(2, 13), 1, 0, 0)
alien$c[y == 18] <- c(0, 0, 0, 1, rep(2, 11), 1, 0, 0, 0)
alien$c[y == 19] <- c(0, 0, 0, 0, 1, rep(2, 9), 1, 0, 0, 0, 0)
alien$c[y == 20] <- c(0, 0, 0, 0, 0, 1, 1, rep(2, 5), 1, 1, 0, 0, 0, 0, 0)
alien$c[y == 21] <- c(rep(0, 7), rep(1, 5), rep(0, 7))

alien_green <- alien %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 12, shape = 22) +
  scale_fill_manual(values = c("black", "#00ff40")) +
  pix_theme +
  xlim(1, 19) +
  ylim(1, 21)

alien_blue <- alien %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 10, shape = 22) +
  scale_fill_manual(values = c("black", "#00FFE4")) +
  pix_theme +
  xlim(1, 19) +
  ylim(1, 21)

alien_pink <- alien %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 12, shape = 22) +
  scale_fill_manual(values = c("black", "#F978D8")) +
  pix_theme +
  xlim(1, 19) +
  ylim(1, 21)

#C39BD3
alien_purple <- alien %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 12, shape = 22) +
  scale_fill_manual(values = c("black", "#C39BD3")) +
  pix_theme +
  xlim(1, 19) +
  ylim(1, 21)

grid.arrange(alien_green, alien_blue, alien_pink, alien_purple, nrow = 2, ncol = 2)


#leaf
x <- rep(1:27, each = 26)
y <- rep(1:26, times = 27)
c <- rep(0, 702)
leaf <- data.frame(x, y, c)

leaf$c[y == 1] <- c()
leaf$c[y == 2] <- c()
leaf$c[y == 3] <- c()
leaf$c[y == 4] <- c()
leaf$c[y == 5] <- c()
leaf$c[y == 6] <- c(rep(0, 7), rep(1, 4), rep(2, 8), rep(1, 6), 0, 0)
leaf$c[y == 7] <- c(rep(0, 5), rep(1, 6), rep(2, 12), 1, 0, 0, 0)
leaf$c[y == 8] <- c(0, 0, 0, 1, 1, rep(3, 5), rep(1, 6), rep(2, 7), 1, 0, 0, 0)
leaf$c[y == 9] <- c(0, 0, 1, 1, 4, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 0, 0)
leaf$c[y == 10] <- c(0, 1, 4, 4, 4, 3, 3, 3, 3, 1, 3, 1, rep(2, 7), 1, 2, 2, 2, 2, 1, 1, 0)
leaf$c[y == 11] <- c(1, 4, 4, 4, 4, 3, 3, 3, 1, 1, 3, 1, 1, rep(2, 12), 1, 0)  
leaf$c[y == 12] <- c(1, 1, 1, 4, 4, 3, 3, 3, 1, 3, 3, 3, 1, rep(2, 11), 1, 1, 1)
leaf$c[y == 13] <- c(0, 0, 0, 1, 4, 3, 3, 1, 1, 3, 3, 3, 1, 1, rep(2, 10), 1, 0, 0)
leaf$c[y == 14] <- c(0, 0, 0, 1, 4, 3, 3, 1, 3, 3, 3, 3, 2, 1, rep(2, 5), rep(1, 5), 0, 0, 0)  
leaf$c[y == 15] <- c(0, 0, 1, 1, 4, 3, 1, 1, 3, 3, 3, 3, 2, 2, 1, 2, 2, 2, 2, 1, rep(0,7))
leaf$c[y == 16] <- c(0, 0, 1, 4, 4, 3, 1, rep(3, 5), 2, 2, 1, 1, 2, 2, 2, 1, rep(0, 7))
leaf$c[y == 17] <- c(0, 1, 4, 4, 4, rep(3, 7), 2, 2, 2, 1, 2, 2, 2, 1, 1, rep(0, 6))
leaf$c[y == 18] <- c(0, 1, 1, 1, 4, 4, rep(3, 7), 2, 2, 1, 1, 2, 2, 2, 1, 1, rep(0, 5))
leaf$c[y == 19] <- c(0, 0, 0, 1, 4, 4, 3, 3, 3, 1, 1, 1, 3, rep(2, 8), 1, 1, 0, 0, 0, 0)
leaf$c[y == 20] <- c(0, 0, 0, 1, 4, 4, 3, 3, 3, 1, 0, 1, 1, rep(2, 7), 1, 1, 1, 0, 0, 0, 0)
leaf$c[y == 21] <- c(0, 0, 0, 1, 4, 3, 3, 3, 1, 0, 0, 0, 1, rep(2, 7), 1, rep(0, 6))
leaf$c[y == 22] <- c(0, 0, 0, 1, 1, 3, 1, 1, 1, 0, 0, 0, 1, 1, rep(2, 6), 1, rep(0, 6))
leaf$c[y == 23] <- c(0, 0, 0, 0, 1, 1, 1, rep(0, 6), 1, rep(2, 5), 1, 1, rep(0, 6))
leaf$c[y == 24] <- c(0, 0, 0, 0, 1, 1, rep(0, 7), rep(1, 5), 2, 1, 1, rep(0, 6))
leaf$c[y == 25] <- c(rep(0, 13), 1, 1, 0, 0, 1, 1, 1, 1, rep(0, 6))
leaf$c[y == 26] <- c(rep(0, 19), 1, 1, rep(0, 6))


leaf1 %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 8, shape = 22) +
  scale_fill_manual(values = c("brown", "red", "orange", "yellow")) +
  pix_theme +
  xlim(1, 27) +
  ylim(1, 26)

leaf %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 8, shape = 22) +
  scale_fill_manual(values = c("brown", "red", "orange", "yellow")) +
  pix_theme +
  xlim(1, 27) +
  ylim(1, 26)

#leaf
x <- rep(1:27, each = 26)
y <- rep(1:26, times = 27)
c <- rep(0, 702)
leaf1 <- data.frame(x, y, c)

leaf1$c[y == 1] <- c(0, 0, 0, 1, 1, 1, rep(0, 21))
leaf1$c[y == 2] <- c(0, 0, 0, 1, 1, 1, 1, rep(0, 10), 1, 1, rep(0, 8))
leaf1$c[y == 3] <- c(rep(0, 5), 1, 1, 1, rep(0, 6), rep(1, 5), rep(0, 8))
leaf1$c[y == 4] <- c(rep(0, 6), 1, 1, 1, 0, 0, 0, 1, 1, 2, 2, 2, 1, rep(0,9))
leaf1$c[y == 5] <- c(rep(0, 7), rep(1, 5), rep(2, 5), 1, 1, 1, rep(0, 7))
leaf1$c[y == 6] <- c(rep(0, 7), rep(1, 4), rep(2, 8), rep(1, 6), 0, 0)
leaf1$c[y == 7] <- c(rep(0, 5), rep(1, 6), rep(2, 12), 1, 0, 0, 0)
leaf1$c[y == 8] <- c(0, 0, 0, 1, 1, rep(3, 5), rep(1, 6), rep(2, 7), 1, 0, 0, 0)
leaf1$c[y == 9] <- c(0, 0, 1, 1, 4, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 0, 0)
leaf1$c[y == 10] <- c(0, 1, 4, 4, 4, 3, 3, 3, 3, 1, 3, 1, rep(2, 7), 1, 2, 2, 2, 2, 1, 1, 0)
leaf1$c[y == 11] <- c(1, 4, 4, 4, 4, 3, 3, 3, 1, 1, 3, 1, 1, rep(2, 12), 1, 0)  
leaf1$c[y == 12] <- c(1, 1, 1, 4, 4, 3, 3, 3, 1, 3, 3, 3, 1, rep(2, 11), 1, 1, 1)
leaf1$c[y == 13] <- c(0, 0, 0, 1, 4, 3, 3, 1, 1, 3, 3, 3, 1, 1, rep(2, 10), 1, 0, 0)
leaf1$c[y == 14] <- c(0, 0, 0, 1, 4, 3, 3, 1, 3, 3, 3, 3, 2, 1, rep(2, 5), rep(1, 5), 0, 0, 0)  
leaf1$c[y == 15] <- c(0, 0, 1, 1, 4, 3, 1, 1, 3, 3, 3, 3, 2, 2, 1, 2, 2, 2, 2, 1, rep(0,7))
leaf1$c[y == 16] <- c(0, 0, 1, 4, 4, 3, 1, rep(3, 5), 2, 2, 1, 1, 2, 2, 2, 1, rep(0, 7))
leaf1$c[y == 17] <- c(0, 1, 4, 4, 4, rep(3, 7), 2, 2, 2, 1, 2, 2, 2, 1, 1, rep(0, 6))
leaf1$c[y == 18] <- c(0, 1, 1, 1, 4, 4, rep(3, 7), 2, 2, 1, 1, 2, 2, 2, 1, 1, rep(0, 5))
leaf1$c[y == 19] <- c(0, 0, 0, 1, 4, 4, 3, 3, 3, 1, 1, 1, 3, rep(2, 8), 1, 1, 0, 0, 0, 0)
leaf1$c[y == 20] <- c(0, 0, 0, 1, 4, 4, 3, 3, 3, 1, 0, 1, 1, rep(2, 7), 1, 1, 1, 0, 0, 0, 0)
leaf1$c[y == 21] <- c(0, 0, 0, 1, 4, 3, 3, 3, 1, 0, 0, 0, 1, rep(2, 7), 1, rep(0, 6))
leaf1$c[y == 22] <- c(0, 0, 0, 1, 1, 3, 1, 1, 1, 0, 0, 0, 1, 1, rep(2, 6), 1, rep(0, 6))
leaf1$c[y == 23] <- c(0, 0, 0, 0, 1, 1, 1, rep(0, 6), 1, rep(2, 5), 1, 1, rep(0, 6))
leaf1$c[y == 24] <- c(0, 0, 0, 0, 1, 1, rep(0, 7), rep(1, 5), 2, 1, 1, rep(0, 6))
leaf1$c[y == 25] <- c(rep(0, 13), 1, 1, 0, 0, 1, 1, 1, 1, rep(0, 6))
leaf1$c[y == 26] <- c(rep(0, 19), 1, 1, rep(0, 6))


#menorah
x <- rep(1:17, each = 17)
y <- rep(1:17, times = 17)
c <- rep(0, 289)
menorah <- data.frame(x, y, c)

menorah$c[y == 1] <- c(0, 1, rep(2, 13), 1, 0)
menorah$c[y == 2] <- c(0, rep(1, 7), 2, rep(1, 7), 0)
menorah$c[y == 3] <- c(0, rep(1, 7), 2, rep(1, 7), 0)
for(i in 4:7) {
  menorah$c[y == i] <- c(rep(0, 6), 1, 1, 2, 1, 1, rep(0, 6))
}
menorah$c[y == 8] <- c(rep(2, 17))
menorah$c[y == 9] <- c(2, rep(1, 7), 2, rep(1, 7), 2)
menorah$c[y == 10] <- c(2, 1, rep(2, 13), 1, 2)
menorah$c[y == 11] <- c(2, 1, 2, rep(1, 5), 2, rep(1, 5), 2, 1, 2)
menorah$c[y == 12] <- c(2, 1, 2, 1, rep(2, 9), 1, 2, 1, 2)
menorah$c[y == 13] <- c(2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1, 2, 1, 2 ,1 ,2)
menorah$c[y == 14] <- c(2, 1, 2, 1, 2, 1, rep(2, 5), 1, 2, 1, 2, 1, 2)
menorah$c[y == 15] <- c(2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2)
menorah$c[y == 16] <- c(3, 1, 3, 1, 0, 1, 0, 1, 2, 1, 0, 1, 0, 1, 0, 1, 0)
menorah$c[y == 17] <- c(rep(0, 8), 3, rep(0, 8))

menorah %>%
  filter(c > 0) %>%
  ggplot(aes(x, y, fill = factor(c))) +
  geom_point(size = 13, shape = 22) +
  scale_fill_manual(values = c("#ff9224", "#2491ff", "yellow")) +
  pix_theme +
  xlim(1, 17) +
  ylim(1, 17)


#mixtape madness
x <- rep(1:17, each = 11)
y <- rep(1:11, times = 17)
c <- rep(0, 187)
mixtape <- data.frame(x, y, c)

mixtape <- mixtape %>%
  mutate(c = case_when(x %in% c(1, 17) | y %in% c(1, 11) |
                         (x %in% c(6:12) & y == 2) |
                         (x %in% c(5:13) & y %in% c(5, 8)) |
                         (x %in% c(4, 7, 11, 14) & y %in% c(6, 7)) |
                         (x %in% c(7:11) & y == 7) ~ 1,
                        (x %in% c(5, 6, 12, 13) & y %in% c(6, 7)) |
                         (x %in% c(8:10) & y == 6) ~ 0,
                       (x %in% c(2:4, 14:16) & y == 5) ~ 2,
                       (x %in% c(2:3, 15:16) & y == 6) ~ 3,
                       (x %in% c(2:3, 15:16) & y == 7) ~ 4,
                       (x %in% c(2:4, 14:16) & y == 8) ~ 5,
                       TRUE ~ 6))

mixtape %>%
  ggplot(aes(x = x, y = y, fill = as.factor(c))) +
  geom_point(size = 17, shape = 22) +
  scale_fill_manual(values = c("grey", "black", "green", "magenta", "yellow",
                               "purple", "cyan")) +
  pix_theme +
  xlim(0, 18) +
  ylim(-1, 13)

#mets
# 0: white, 1: orange, 2: blue
x <- rep(0:25, each = 31)
y <- rep(0:30, times = 26)
c <- rep(0, 806)
mets <- data.frame(x, y, c)

mets$c[y==0] <- c(rep(0, 9), rep(1, 8), rep(0, 9))
mets$c[y==1] <- c(rep(0, 9), 1, rep(2, 6), 1, rep(0, 9))
mets$c[y==2] <- c(rep(0, 9), 1, 2, 1, 2, 2, 1, 2, 1, rep(0, 9))
mets$c[y==3] <- c(rep(0, 9), 1, 1, 1, 2, 2, 1, 1, 1, rep(0, 9))
mets$c[y==4] <- c(rep(0, 11), 1, 2, 2, 1, rep(0, 11))
mets$c[y==5] <- c(rep(0, 4), rep(1, 4), rep(0, 3), 1, 2, 2, 1, rep(0, 3), rep(1, 4), rep(0, 4))
mets$c[y==6] <- c(0, 0, 1, 1, rep(2, 4), 1, 1, 0, 1, 2, 2, 1, 0, 1, 1, rep(2, 4), 1, 1, 0, 0)
mets$c[y==7] <- c(0, 0, 1, rep(2, 6), 1, 0, 1, 2, 2, 1, 0, 1, rep(2, 6), 1, 0, 0)
mets$c[y==8] <- c(0, 0, 1, 2, 1, 2, 2, 1, 2, 1, 0, 1, 2, 2, 1, 0, 1, 2, 1, 2, 2, 1, 2, 1, 0, 0)
mets$c[y==9] <- c(0, 0, 1, 1, 1, 2, 2, 1, 1, 1, 0, 1, 2, 2, 1, 0, 1, 1, 1, 2, 2, 1, 1, 1, 0, 0)
mets$c[y==10] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, rep(0,4))
mets$c[y==11] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 0, 0, 1, 2, 2, 2, 1, rep(0,4))
mets$c[y==12] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 0, 1, 2, 2, 1, 0, 1, 2, 2, 2, 2, 1, rep(0,4))
mets$c[y==13] <- c(rep(0, 4), 1, 2, 2, 1, 0, 0, 1, 2, 2, 2, 1, 1, 2, 2, 2, 2, 2, 1, rep(0,4))
mets$c[y==14] <- c(rep(0, 4), 1, 2, 2, 1, 0, 1, rep(2, 11), 1, rep(0, 4))
mets$c[y==15] <- c(1, 1, 0, 0, 1, 2, 2, 1, 0, 1, 2, 2, 2, 1, 2, 2, 2, 2, 1, 2, 2, 1, 0, 1, 1, 0)
mets$c[y==16] <- c(1, 2, 1, 0, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 2, 2, 1, 1, 2, 2, 1, 0, 1, 2, 1)
mets$c[y==17] <- c(1, 2, 2, 1, 1, 2, 2, 1, 2, 2, 2, 1, 1, rep(2, 5), 1, 2, 2, 1, 1, 2, 2, 1)
mets$c[y==18] <- c(1, rep(2, 10), 1, rep(2, 6), 1, rep(2, 6), 1)
mets$c[y==19] <- c(1, rep(2, 9), 1, 1, 2, 2, 2, 1, 1, rep(2, 8), 1)
mets$c[y==20] <- c(0, 1, rep(2, 8), 1, 2, 2, 2, rep(1, 4), rep(2, 6), 1, 0)
mets$c[y==21] <- c(0, 0, 1, rep(2, 6), 1, 2, 2, 2, 1, 1, 0, 0, 1, rep(2, 5), 1, 1, 0)
mets$c[y==22] <- c(0, 0, 0, 1, 1, 2, 2, 1, 1, 2, 2, 2, 1, 1, rep(0, 4), 1, 2, 2, 1, 1, 1, 0, 0)
mets$c[y==23] <- c(rep(0, 4), 1, 2, 2, 1, 2, 2, 2, 1, 1, rep(0, 5), 1, 2, 2, 1, rep(0,4))
mets$c[y==24] <- c(rep(0, 4), 1, rep(2, 5), 1, 1, rep(0, 6), 1, 2, 2, 1, rep(0,4))
mets$c[y==25] <- c(rep(0, 4), 1, rep(2, 4), 1, 1, rep(0, 7), 1, 2, 2, 1, rep(0, 4))
mets$c[y==26] <- c(rep(0, 4), 1, 2, 2, 2, 1, 1, rep(0, 8), 1, 2, 2, 1, rep(0,4))
mets$c[y==27] <- c(0, 0, 1, 1, 1, 2, 2, 1, 1, 1, rep(0, 6), 1, 1, 1, 2, 2, 1, 1, 1, 0, 0)
mets$c[y==28] <- c(0, 0, 1, 2, 1, 2, 2, 1, 2, 1, rep(0, 6), 1, 2, 1, 2, 2, 1, 2, 1, 0, 0)
mets$c[y==29] <- c(0, 0, 1, rep(2, 6), 1, rep(0, 6), 1, rep(2, 6), 1, 0, 0)
mets$c[y==30] <- c(0, 0, rep(1, 8), rep(0, 6), rep(1, 8), 0, 0)

mets %>%
  ggplot(aes(x = x, y = y, fill = as.factor(c))) +
  geom_point(size = 17, shape = 22) +
  scale_fill_manual(values = c("white", "orange", "blue")) +
  pix_theme +
  xlim(0, 25) +
  ylim(0, 30)
