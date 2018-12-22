
# 학습목표
# 1. 시각화의 필요성을 인지한다.
# 2. 기본 시각화 4종(분포, 관계, 비교, 중요도)을 연습한다.

library(tidyverse)
library(httr)
library(XML)
library(skimr)


url <- "https://en.wikipedia.org/wiki/Anscombe's_quartet"
url <- GET(url)

mytable <- rawToChar(url$content)
mytable <- readHTMLTable(
  mytable, 
  which = 2, 
  header = T, 
  skip.rows = 1,
  stringsAsFactors = F)

glimpse(mytable)

mytable <- mytable %>% 
  mutate_all(as.double) %>% 
  rename(x1 = V1, y1 = V2, x2 = V3, y2 = V4,
         x3 = V5, y3 = V6, x4 = V7, y4 = V8)

skim(mytable)

library(gridExtra)

b1 <- ggplot(mytable) +
  aes(x1, y1) +
  geom_point()

b2 <- ggplot(mytable) +
  aes(x2, y2) +
  geom_point()

b3 <- ggplot(mytable) +
  aes(x3, y3) + 
  geom_point()

b4 <- ggplot(mytable) +
  aes(x4, y4) + 
  geom_point()

gridExtra::grid.arrange(b1, b2, b3, b4, ncol = 2)
