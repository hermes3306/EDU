
# 학습목표
# 1. 로지스틱 회귀분석의 관련용어 및 개념을 이해한다.
# 2. 분류 예측을 한다.


## logistic regression

death <- read.csv("~/R Lecture/data/DeathPenalty.csv")
str(death)

# Agg(범죄, 6이 흉악범죄) 
# VRace(피해자, 백인 = 1) 
# Death(사형 = 1)

names(death) <- tolower(names(death))

par(mfrow = c(1, 3))

lapply(names(death), function(x) {
  hist(death[ , x], main = paste("Histogram of", x), xlab = x)
})

## 방법 1
m1 <- glm(death ~ agg + vrace, family = binomial, data = death)

summary(m1)

exp(coef(m1))

death <- data.frame(death, prob = fitted(m1))

head(death)

victim <- ifelse(death$vrace == "1", "white", "black")

ggplot(death) + 
  aes(agg, prob, group = vrace) +
  geom_line(aes(color = victim)) 

predict(m1, data.frame(agg = 4, vrace = 1), type = "response")
predict(m1, data.frame(agg = 4, vrace = 0), type = "response")


## 방법 2
death2 <- death[ , -4]

death2$vrace <- factor(
  death2$vrace, 
  levels = c(0, 1),
  labels = c("black", "white")
  )

death2$death <- factor(
  death2$death, 
  levels = c(0, 1),
  labels = c("alive","dead")
  )

str(death2)

m2 <- glm(death ~ vrace + agg, family = binomial, data = death2)

summary(m2)        

models <- list(m1, m2)

lapply(models, AIC)

lapply(models, function(x) {
  exp(coef(x))
})



## 연습)
data(Affairs, package = "AER")
summary(Affairs)
table(Affairs$affairs)

Affairs$ynaffair[Affairs$affairs  > 0] <- 1
Affairs$ynaffair[Affairs$affairs == 0] <- 0

Affairs$ynaffair <- factor(Affairs$ynaffair,
                           levels = c(0, 1),
                           labels = c("No", "Yes"))
table(Affairs$ynaffair)

fit.full <- glm(ynaffair ~ . - affairs,  
                data = Affairs, 
                family = binomial)

summary(fit.full)

fit.redu <- glm(ynaffair ~ age + 
                     yearsmarried + 
                     religiousness + 
                     rating, 
                   data = Affairs,
                   family = binomial)

summary(fit.redu)

anova(fit.redu, fit.full, test = "Chisq")

exp(coef(fit.redu)) 


## 변수 살펴보기
testdata <- data.frame(
  rating = c(1, 2, 3, 4, 5),
  age = mean(Affairs$age),
  yearsmarried = mean(Affairs$yearsmarried),
  religiousness = mean(Affairs$religiousness) )

testdata$prob <- predict(fit.redu, 
                         testdata,
                         type = "response")
testdata


testdata2 <- data.frame(
  rating = 4,
  age = 51,
  yearsmarried = 22,
  religiousness = 1)

testdata2$prob <- predict(fit.redu,
                          testdata2,
                          type = "response")
testdata2


## overdispersion(종속변수 실제분산 > 예측분산)
# 방법1
fit.quasi <- glm(ynaffair ~ age + 
                   yearsmarried + 
                   religiousness + 
                   rating, 
                 data = Affairs, 
                 family = quasibinomial)

pchisq(summary(fit.quasi)$dispersion * 
         fit.redu$df.residual, 
       fit.redu$df.residual, 
       lower = F) 

# p-value < 0.05이면 과산포


# 방법2
library(qcc)
qcc.overdispersion.test(
  Affairs$affairs, 
  rep(1, length(Affairs$affairs)), 
  type = "binomial")

