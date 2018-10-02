library(ggplot2)
library(dplyr)

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
df <- data.frame(x, y, c)

ones_1 <- 1
zeroes_1 <- 7
ones_2 <- 5
zeroes_2a <- 2
zeroes_2b <- 1
for(i in 1:11) {
  if (i < 10) {
    df$c[y == i] <- c(rep(0, zeroes_1), 
                    rep(1, ones_1), rep(0, zeroes_1))
    if(ones_1 < 13) {ones_1 <- ones_1 + 2}
    if(zeroes_1 > 1) {zeroes_1 <- zeroes_1 - 1}
    }
  if(i >= 10) {
    df$c[y == i] <- c(rep(0, zeroes_2a), rep(1, ones_2),
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


