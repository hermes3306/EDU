
# 학습목표
# 1. 기초 통계 함수를 이해한다.
# 2. 기초 검정 방법을 이해한다.


## 함수
# r : 난수 발생
# d : 확률밀도 함수
# p : 누적확률 함수 
# q : 확률에 대응하는 데이터 값

# 예제1) 고객수 정규 분포 가정
# 하루 평균 고객수가 200명이고 표준편차가 20명인
# 커피숍에 230명 넘게 찾아올 확률은?

set.seed(1234)
x <- rnorm(1000000, 200, 20) 
plot(density(x))  
abline(v = 230, lty = 3)           
pnorm(230, 200, 20)          
1 - pnorm(230, 200, 20)

# 예제2) 토익점수 정규분포 가정
# 토익점수 평균이 700점, 표준편차가 100점일때 
# 상위 5%에 해당하는 점수는?

qnorm(0.95, 700, 100)

# 예제3) 전구수명을 정규분포 가정
# 전구의 평균 수명이 3,000시간, 표준편차가 
# 80시간 일때 임의로 선택한 전구 1개의 수명이
# 2,948시간에서 3,080시간일 확률은?

pnorm(3080, 3000, 80) - pnorm(2948, 3000, 80)



## t-test : (xbar - mu) / (sd(x) / sqrt(n))
# 평균의 차이가 오차 범위를 벗어나는지 검정

# 예제1) 모평균 추정
# 500ml 생수를 10개 추출하여 생수병에 있는 
# 세균수를 말하고자 한다.

x <- c(175, 190, 215, 198, 184, 207, 210, 193, 196, 180)
t.test(x, mu = 200) 
t.test(x, mu = 200, alt = "less")

# x %>% t.test(mu = 200)
# x %>% t.test(mu = 200, alt = "less")

# 수기계산
# xbar <- mean(x)
# mu   <- 200
# df   <- length(x) - 1
# t.value <- (xbar - mu) / { sd(x) / sqrt(length(x)) }
# t.value
# p.value <- 2 * pt(-abs(t.value), df) # 양측검정이므로
# p.value


# 예제2) 두 그룹의 평균 비교
# "대기업과 중소기업의 신입사원 연봉은 차이가 없다"는 주장을 검정
data(iris)

# 대기업 샘플 연봉 (단위 천만원)
l.pay <- iris %>% 
  filter(Species == "setosa") %>% 
  select(large = Sepal.Width) 

# 중소기업 샘플 연봉 (단위 천만원)
s.pay <- iris %>%
  filter(Species == "virginica") %>% 
  select(small = Sepal.Width) 
       
# 평균의 차이 유무 검정
t.test(l.pay$large, s.pay$small)
t.test(l.pay$large, s.pay$small, var.equal = T)

pay <- data.frame(l.pay, s.pay)
pay %>% 
  ggplot(aes(size, pay, group = size)) +
  geom_boxplot(aes(fill = size)) +
  stat_summary(geom = "point", fun.y = mean, col = "purple") +
  theme(legend.position = "none")
      
# 정규성 검정
shapiro.test(l.pay$large)          
shapiro.test(s.pay$small)

ggplot(l.pay, aes(large)) + geom_histogram()
ggplot(s.pay, aes(small)) + geom_histogram()

# 등분산 검정
var.test(l.pay$large, s.pay$small) 

pay %>% 
  group_by(size) %>% 
  ggplot(aes(pay, ..density..)) +
  geom_histogram(col = "grey", aes(fill = size)) +
  geom_density() +
  facet_grid(size ~ .) +
  theme(legend.position = "none") +
  xlim(c(0, 5))

# 수기 계산
# mean.diff <- mean(l.pay$large) - mean(s.pay$small)
# df <- 50 + 50 - 2
# pooled.var <- ((var(l.pay$large) * 49 + var(s.pay$small) * 49)) / df
# se.diff <- sqrt(pooled.var / 50 + pooled.var / 50)
# t.obs <- mean.diff / se.diff
# t.obs
# p.value <- 2 * pt(abs(t.obs), df = df, lower.tail = F)
# p.value


## 3군의 평균 비교(일원분산분석, 그룹간 변동/그룹내 변동)
data(iris)
result <- aov(Petal.Length ~ Species, data = iris)
summary(result)

# 총제곱합(TSS)
TSS <- with(iris, sum((Petal.Length - mean(Petal.Length))^2))
TSS

# 그룹내 제곱합(SSE)
g1 <- iris[iris$Species == "setosa",     ] 
g2 <- iris[iris$Species == "versicolor", ] 
g3 <- iris[iris$Species == "virginica",  ] 

SSE1 <- with(g1, sum((Petal.Length - mean(Petal.Length))^2))
SSE2 <- with(g2, sum((Petal.Length - mean(Petal.Length))^2))
SSE3 <- with(g3, sum((Petal.Length - mean(Petal.Length))^2))

SSE <- SSE1 + SSE2 + SSE3

# 그룹간 제곱합(SSR)
SSR <- TSS - SSE
SSR

# 그래프로 확인
iris %>%
  group_by(Species) %>%
  ggplot(aes(Petal.Length, col = Species)) +
  geom_density() +
  xlim(c(0, 8))

iris %>% 
  filter(Species %in% c("setosa", "virginica")) %>% 
  select(Petal.Length, Species) %>% 
  aov(Petal.Length ~ Species, data = .) %>% 
  summary()

iris %>% 
  filter(Species %in% c("versicolor", "virginica")) %>% 
  select(Petal.Length, Species) %>% 
  aov(Petal.Length ~ Species, data = .) %>% 
  summary()
  
# 사후 검정 (Tukey, bonferroni, Holm)              
# 방법 1
TukeyHSD(result)                                 
plot(TukeyHSD(result))

# 방법 2
with(iris, pairwise.t.test(Petal.Length, Species, p.adj = "bonf")) 

# 방법 3
with(iris, pairwise.t.test(Petal.Length, Species, p.adj = "holm"))       


## 비율검정

# 하나의 비율 
binom.test(44, 100) # 방법1
prop.test(44, 100)  # 방법2

# 두(세)개의 비율
prop.test(c(44, 55), c(100, 100)) 
prop.test(c(44, 55, 60), c(100, 100, 100))

# 예제1)
# A 기업에 연봉 5천만원 이상인 직원이 120명 중 32명, B기업에 연봉 5천만원 이상인 직원이 270명 중 107명이다. 비율에 차이가 있을까?
prop.test(c(32, 107), c(120, 270))

# 예제2)
# 주사위를 150번 던졌더니 다음과 같은 결과가 나왔다. 
# 6이 나올 확률은 여전히 1/6인가?
freq <- c(22, 21, 22, 27, 22, 36)
probs <- rep(1 / 6, 6)
chisq.test(freq, p = probs)



## 카이제곱 검정 sum{(관측빈도-기대빈도)^2 /기대빈도}
# 카이제곱 분포를 가정하는 범주형 데이터에 대한 통계적 검정

# 분포의 모양이 자유도에 의해 결정되는지 보면...
x <- rchisq(100, 5)
hist(x, probability = T)
curve(dchisq(x, 5), add = T)
curve(dchisq(x, 7), add = T, col = "blue")
curve(dchisq(x, 10), add = T, col = "red")

# 여성은 120명중 18명이 편두통이 있고, 남성은 120명 중 10명이 편두통을 앓고 있다. 편두통은 남/녀에 따라 차이가 있는가? 
prop.test(c(18, 10), c(120, 120)) # 방법 1

female <- c(18, 102)
male   <- c(10, 110)  
migraine <- cbind(female, male)
chisq.test(migraine)              # 방법 2
