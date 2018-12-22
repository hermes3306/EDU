
# 학습목표
# 1. 시각화에서 의미를 발견한다.
# 2. 다양한 시각화 방법을 학습한다.

## 테이블 및 변수 정의가 모호할 때 ----
data(diamonds)

glimpse(diamonds)
levels(diamonds$cut)
levels(diamonds$color)

diamonds %>% 
  ggplot(aes(carat, price, color = color)) +
  geom_point()

diamonds %>% 
  ggplot(aes(carat, price, color = color)) +
  geom_smooth(method = "loess")


## 가중치를 반영한 그래프 ---
data(midwest)
glimpse(midwest)

p <- ggplot(midwest) + 
  aes(percwhite, percbelowpoverty)

a <- p + geom_point() + geom_smooth()

b <- p + geom_point(aes(size = poptotal/1e6)) + 
  geom_smooth(aes(weight = poptotal/1e6))

gridExtra::grid.arrange(a, b, ncol = 2)

p2 <- ggplot(midwest) +
  aes(percblack, percbelowpoverty)

p2 + geom_point() + geom_smooth()

p2 + geom_point(aes(size = poptotal/1e6)) +
  geom_smooth(aes(weight = poptotal/1e6)) +
  scale_y_continuous(limits = c(0, 60))


## device 데이터를 그리고 싶을 때 ----
set.seed(1234)
n  <- 10000

c1 <- matrix(rnorm(n, mean = 0, sd = .05), ncol = 2)
c2 <- matrix(rnorm(n, 3, 2), ncol = 2)

mydata <- rbind(c1, c2)
mydata <- as.data.frame(mydata)
names(mydata) <- c("x", "y")

ggplot(mydata) + 
  aes(x, y) + 
  geom_point(alpha = 0.05)


## 시계열 데이터 그래프 ----
data(economics)
glimpse(economics)

econ <- economics
econ <- econ %>% gather(key, value, -date)
head(econ)

econ %>%
  ggplot(aes(date, value, group = key)) +
  geom_line(aes(color = key)) + 
  facet_grid(key ~ ., scale = "free_y") +
  theme(legend.position = "none")

ggplot(economics, aes(date)) +
  geom_line(aes(y = unemploy, color = "unemploy")) +
  geom_line(aes(y = uempmed,  color = "uempmed"))

range_f <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / diff(rng)
}

econ %>% 
  filter(date >= "2000-01-01", date <= "2010-12-31") %>% 
  filter(key %in% c("unemploy", "uempmed")) %>%
  group_by(key) %>%
  mutate(value2 = range_f(value)) %>%
  ggplot(aes(date, value2, color = key)) + 
  geom_line()


## heat map ----
data(presidents)
class(presidents)
plot(presidents)

pres <- data.frame(
  rating  = as.numeric(presidents),
  year    = as.numeric(floor(time(presidents))),
  quarter = as.numeric(cycle(presidents))
)

head(pres)

p <- ggplot(pres) + 
  aes(year, quarter, fill = rating)

p + geom_tile()
p + geom_raster()


## correlation plot ----
data(mtcars)
mtcars.cor <- cor(mtcars)
mtcars.cor <- round(mtcars.cor, digits = 2)
mtcars.cor[ , 1, drop = F]

library(corrplot)
corrplot(mtcars.cor,
         method = "shade",
         shade.col = NA,
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         order = "AOE")


## 3D plot ----
# 3D scatter plot
library(scatterplot3d)

attach(mtcars)
scatterplot3d(wt, disp, mpg)

s3d <- scatterplot3d(
  wt, 
  disp, 
  mpg, 
  pch = 16,
  highlight.3d = T,
  type = "h")

fit <- lm(mpg ~ wt + disp)
s3d$plane3d(fit)

library(rgl)
plot3d(wt, 
       disp,
       mpg,
       col = "red", 
       size = 5)

install.packages("p3d", repos="http://R-Forge.R-project.org")
library(p3d) 

Init3d(family = "serif", cex = 1)
fit <- lm(mpg ~ disp + wt + I(wt^2), data = mtcars)
Plot3d(mpg ~ disp + wt, mtcars) 
Axes3d()
Fit3d(fit)
