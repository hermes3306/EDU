
# 학습목표
# 1. 회귀분석의 관련용어 및 개념을 이해한다.
# 2. 수치 예측을 한다.


## 기본용어 이해

# 잔차란?
set.seed(2018)

data <- data.frame(
  x = c(0:10),
  y = c(5:15) + rnorm(11, 0, 1)
)

(p <- ggplot(data, aes(x, y)) + 
    geom_point())

model <- lm(y ~ x, data = data)

data$yhat <- predict(model)

p + geom_smooth(method = lm, col = "blue", se = F) + 
  geom_segment(data = data, aes(xend = x, yend = yhat), col = "red")

# 최대우도 추정 : 모수 선택
bs <- seq(0.1, 2, 0.01)

sse <- function(i) {
  # e = y - yhat, e = y - a - bx, sum(e^2)
  sum((y - 4.8 - bs[i] * x)^2) 
}

mydata <- data.frame(
  slope = bs,
  sse = sapply(1:length(bs), sse)
)

ggplot(mydata, aes(slope, sse)) + 
  geom_line() +
  xlim(c(0, 2.5))

# 일탈도(deviance)란?, log(y) 이유는? 
data <- read.csv("https://raw.githubusercontent.com/lmmx/crawley-r-statistics-book-notes/master/code/decay.csv", header = T)

str(data)
with(data, plot(time, amount))
abline(lm(amount ~ time, data), col = "blue")

m_nlog <- lm(amount ~ time, data)
m_log  <- lm(log(amount) ~ time, data)

ts <- seq(0, 30, 0.02)
y  <- exp(predict(m_log, list(time = ts)))
with(data, plot(time, amount))
lines(ts, y, col = "red")

deviance(m_nlog) ; sum(m_nlog$residuals^2)
newdata <- cbind(data, fitted(m_nlog), exp(fitted(m_log)))

newdata$d1 <- newdata[ , 2] - newdata[ , 3]
newdata$d2 <- newdata[ , 2] - newdata[ , 4]

newdata$d1_square <- newdata$d1^2
newdata$d2_square <- newdata$d2^2

colSums(newdata[7:8])



## 단순회귀 (simple linear model)

# 데이터 재정의
mydata <- iris %>% 
  select(revenue = Petal.Length,
         cost    = Petal.Width,
         ads     = Species)

mydata$ads <- 
  ifelse(mydata$ads == "setosa", "잡지",
  ifelse(mydata$ads == "versicolor", "라디오", "TV")) %>%
  as.factor()

summary(mydata)

# 상관관계 확인
with(mydata, cor.test(cost, revenue)) 

p <- ggplot(mydata, aes(cost, revenue)) +
  geom_point() +
  geom_smooth(method = lm, se = F)
p

# 단순회귀 모형 작성
m1 <- lm(revenue ~ cost, data = mydata)
summary(m1)
mydata$pred <- predict(m1)
head(mydata)

# 잔차 검정
par(mfrow = c(2, 2))
plot(m1)

# 잔차 확인
p2 <- p + 
  geom_segment(aes(xend = mydata$cost, yend = mydata$pred), col = "red")

p2

# 잔차 비교 (SSE)
library(gridExtra)
o <- ggplot(mydata, aes(cost, revenue)) +
  geom_point() +
  geom_hline(yintercept = mean(mydata$revenue), col = "red") +
  geom_segment(aes(xend = cost, yend = mean(revenue)))

grid.arrange(o, p2, ncol = 2)

sse_null <- with(mydata, sum((revenue - mean(revenue))^2))
sse_m1   <- with(mydata, sum((revenue - pred)^2))
sse_null
sse_m1


## 분산분석
# 범주형 변수 레벨 재정의
mydata$ads <- factor(mydata$ads, levels = c("잡지", "라디오", "TV"))

# 분산분석
mod_aov <- lm(revenue ~ ads, data = mydata)
summary(mod_aov)

# 계수의 의미
means <- mydata %>% 
  group_by(ads) %>% 
  summarise(means = mean(revenue)) 

ggplot(mydata, aes(cost, revenue)) +
  geom_point(aes(col = ads)) +
  geom_hline(means, yintercept = means$means) +
  theme(text = element_text(family = "AppleGothic"))

# 분산분석 테이블
summary(aov(revenue ~ ads, data = mydata))

# 제곱합 비교
# 총제곱합(TSS)
# attach(mydata)
# TSS <- sum((revenue - mean(revenue))^2)
# 
# # 그룹내 제곱합(SSE)
# 잡지   <- mydata[mydata$ads == "잡지",   ] 
# 라디오 <- mydata[mydata$ads == "라디오", ] 
# TV     <- mydata[mydata$ads == "TV",     ] 
# 
# SSE_잡지   <- sum((잡지$revenue - mean(잡지$revenue))^2)
# SSE_라디오 <- sum((라디오$revenue - mean(라디오$revenue))^2)
# SSE_TV     <- sum((TV$revenue - mean(TV$revenue))^2)
# 
# SSE <- SSE_잡지 + SSE_라디오 + SSE_TV
# 
# # 그룹간 제곱합(SSR)
# SSR <- TSS - SSE
# SSR


## 공분산 분석(ANCOVA)
# 모형 작성
m2 <- lm(revenue ~ cost + ads, data = mydata)
summary(m2)

# 예측치
mydata$pred2 <- predict(m2)
head(mydata)

# 그래프 확인
ggplot(mydata, aes(cost, revenue, col = ads)) +
  geom_point() +
  theme(text = element_text(family = "AppleGothic")) +
  geom_smooth(method = lm, se = F)

# 모델 비교(m1 vs. m2)
sse_m2 <- with(mydata, sum((revenue - pred2)^2))
sse_m1 ; sse_m2

# 예측
predict(m2, data.frame(cost = 3, ads = "TV"))

# 잔차 검정
par(mfrow = c(2, 2))
plot(m1)
plot(m2)
par(mfrow = c(1, 1))


## 범주형 변수 더미 변수화
mydata <- mydata[ , c(1:3)]
mydata$ads <- factor(mydata$ads, levels = c("잡지", "라디오", "TV"))

library(caret)
dummy <- dummyVars(~ ., fullRank = T, data = mydata)
mydata2 <- predict(dummy, newdata = mydata)
head(mydata2)
mydata2 <- as.data.frame(mydata2)

m2_2 <- lm(revenue ~ ., data = mydata2)
summary(m2_2)

coef(m2) ; coef(m2_2)

# 범주형 변수가 많을 경우는 더미변수를 활용할 것


## 다항회귀
m3 <- lm(revenue ~ cost + I(cost^2), data = mydata)
pred3 <- predict(m3)
sse_m3 <- with(mydata, sum((revenue - pred3)^2))

models_df <- data.frame(
  model = c("linear", "ancova", "poly.n"),
  SSE = c(sse_m1, sse_m2, sse_m3))

models_df

# models <- list(m1, m2, m3)
# models <- models %>% set_names(c("m1", "m2", "m3"))
# models %>% map_df(~ broom::glance(.x), .id = "model_name")


## 다중회귀(multiple regression)
library(car)
options(digits = 2)

data(state)
states <- as.data.frame(state.x77)
glimpse(states)
  
names(states)

pairs(states, upper.panel = panel.smooth)

states <- states %>%
  select("Murder",
         "Population",
         "Illiteracy",
         "Income",
         "Frost")


# library(GGally)
# ggpairs(states)


# 방법 1
m1 <- lm(Murder ~ ., data = states)
summary(m1)

m2 <- step(m1, direction = "both") # backward, forward
summary(m2)

anova(m2, m1, test = "Chisq")


# 방법 2
# 2차변수 포함 여부
library(mgcv)
par(mfrow = c(2, 2))
attach(states)
model <- gam(Murder ~ s(Population) + 
               s(Illiteracy) + 
               s(Income) + 
               s(Frost), 
             data = states)
plot(model)

Income2 <- Income^2
Frost2  <- Frost^2


# 교호작용 포함 여부
library(tree)
par(mfrow = c(1, 1))
model2 <- tree(Murder ~ ., data = states)
plot(model2)
text(model2)

IP  <- Illiteracy * Population
PF  <- Population * Frost
IPF <- Illiteracy * Population * Frost

# model fitting
fit1 <- lm(Murder ~ . + Income2 + Frost2 + IP + PF + IPF, 
           data = states)
summary(fit1)

fit2 <- update(fit1, ~ . -IPF, data = states)
summary(fit2)

fit3 <- update(fit2, ~ . -PF, data = states)
summary(fit3)

fit4 <- update(fit3, ~ . -IP, data = states)
summary(fit4)

fit5 <- update(fit4, ~ . -Frost2, data = states)
summary(fit5)

fit6 <- update(fit5, ~ . -Income2, data = states)
summary(fit6)

fit7 <- update(fit6, ~ . -Frost, data = states)
summary(fit7)

fit8 <- update(fit7, ~ . -Income, data = states)
summary(fit8)


# 모형비교
anova(fit8, fit7) 

# 수기확인
sum(fit8$residuals^2) ; sum(fit7$residuals^2) 


# step function vs. manual
fit9 <- step(fit1, direction = "both")
summary(fit9)

# 인구 500, 문맹률 1.5일 경우 살인사건은?
df <- data.frame(Population = 500, Illiteracy = 1.5)
predict(fit8, df)


# 가정 검정
# library(corrgram)
# corrgram(cor(states), upper.panel = panel.conf)  # 상관성
# 
# res <- fit8$residuals
# par(mfrow = c(2, 2))
# plot(res) ; acf(res)             # 독립성_방법1
# durbinWatsonTest(fit8)           # 독립성_방법2 
# 
# library(lmtest)
# dwtest(fit8, alt = "two.sided")  # 독립성_방법3 
# 
# qqnorm(res) ; qqline(res)        # 정규성_방법1
# shapiro.test(res)                # 정규성_방법2
# 
# plot(fitted(fit8), res)          # 등분산_방법1
# ncvTest(fit2)                    # 등분산_방법2
# 
# plot(fit8)                       # 한번에 보기 
# 
# vif(fit8) ; sqrt(vif(fit8)) > 2  # 다중공선성 
# 
# library(QuantPsyc)
# lm.beta(fit8)                    # 표준 회귀계수 

