
# 학습목표
# 1. 시각화의 필요성을 인지한다.
# 2. 기본 시각화 4종(분포, 관계, 비교, 중요도)을 연습한다.

library(tidyverse)

## 분포 ----
data(iris)

# 히스토그램 
ggplot(iris, aes(x = Petal.Length)) +
  geom_histogram(color = "grey")

ggplot(iris, aes(Petal.Length)) +
  geom_histogram(col = "grey", binwidth = 0.2)

# 여러개의 히스토그램
iris2 <- iris %>% 
  gather(key, value, -Species)

head(iris2)

iris2 %>% 
  ggplot(aes(value)) +
  geom_histogram(color = "grey") +
  facet_wrap(~ key)

iris2 %>% 
  ggplot(aes(value, ..density..)) +
  geom_histogram(color = "grey") +
  facet_wrap(~ key) +
  geom_density(color = "red")


# 박스 플랏
iris %>% 
  ggplot(aes(factor(0), Petal.Length)) +
  stat_boxplot(geom = 'errorbar', width = 0.2) +
  geom_boxplot() +
  geom_jitter(alpha = 0.5, width = 0.2, aes(col = Species))

iris2 %>% 
  ggplot(aes(key, value, group = key)) +
  geom_boxplot() +
  facet_wrap(~ Species) +
  theme(axis.text.x = element_text(angle = 90))

iris2 %>% 
  ggplot(aes(Species, value, group = key)) +
  geom_boxplot() +
  facet_grid(key ~ Species)


## 비교 1 ----
data(Cars93, package = "MASS")
glimpse(Cars93)

mycars <- Cars93 %>% 
  select(Type, Origin) %>% 
  table() %>% 
  as_tibble()

# ggplot2
p <- ggplot(mycars, aes(Type, n, fill = Origin))

p + geom_bar(position = "stack", stat = "identity")

p + geom_bar(position = "dodge", stat = "identity")


## 비교 2 ----
data("ChickWeight")

chicks <- ChickWeight
head(chicks)

# weight : 병아리 체중(mg)
# Time   : 성장일수(일)
# Chick  : 병아리 id
# Diet   : 모이

summary(chicks)

temp <- chicks %>% 
  group_by(Diet, Chick) %>%
  select(Diet, Chick) %>% 
  distinct()

temp %>% group_by(Diet) %>% count()

# 무엇을 알고 싶은 것일까요?

chicks %>% 
  ggplot(aes(Time, weight, group = Chick, col = Diet)) +
  geom_line()

chicks %>% 
  group_by(Diet) %>% 
  ggplot(aes(Time, weight, col = Diet)) +
  geom_line(aes(group = Chick)) +
  facet_wrap(~ Diet)

chicks %>% 
  group_by(Diet) %>% 
  ggplot(aes(Time, weight, col = Diet)) +
  geom_line(aes(group = Chick)) +
  facet_wrap(~ Diet) +
  geom_smooth(col = "black")

chicks %>% 
  ggplot(aes(Time, weight, col = Diet)) +
  geom_smooth(method = "loess")

chicks %>% 
  ggplot(aes(Time, weight, col = Diet)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "loess")


## 관계 ----
# 한번에 여러 그래프 보기
library(GGally)
ggpairs(iris)

iris %>%
  ggplot(aes(Petal.Length, Petal.Width)) +
  geom_point(aes(col = Species)) +
  geom_smooth(method = lm, se = F, col = "black") +
  geom_smooth(method = loess, se = F, col = "yellow") +
  geom_smooth(method = lm, formula = y ~ I(x^2), se = F, col = "purple") 


## 구성비 ----
data("diamonds")
glimpse(diamonds)

levels(diamonds$cut)

diamonds %>% 
  group_by(cut, color, clarity) %>% 
  summarise(count = n()) %>% 
  mutate(sums = sum(count)) %>% 
  ggplot(aes(factor(0), count, fill = cut)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(color ~ clarity)

diamonds %>% 
  group_by(cut, color, clarity) %>% 
  summarise(count = n()) %>% 
  mutate(sums = sum(count)) %>% 
  ggplot(aes(color, count, fill = cut)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_grid(. ~ clarity)
