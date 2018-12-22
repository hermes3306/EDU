
# 학습목표
# 1. 기본 명령어를 연습한다.
# 2. 데이터 읽기와 쓰기를 연습한다. (내부 vs. 외부데이터)
# 3. 객체 개념을 이해한다.


## load package ----
library(readxl)
library(tidyverse)
library(rio)
library(DT)

search() # 로드된 패키지 확인


## R words ----
getwd()         # 경로 확인
data(iris)      # 샘플데이터 불러오기
summary(iris)   # 데이터 요약
names(iris)     # 열이름, 행이름은?
str(iris)       # character와 factor의 차이는?
head(iris)      # 3행을 보려면? 뒤에서 3행은?
ls()            # 객체 확인
View(iris)      # 엑셀시트처럼 보기
datatable(iris) # 동적시트 보기


## data export / import ----
# 저장
export(iris, "new.xlsx")
export(iris, "new.csv")

# 읽기
new <- import("new.xlsx")
new <- import("new.csv")


## 메모리 정리 ----
rm(iris)                                    # 1개 지울때
rm("newiris", "newiris2")                   # 2개 지울때
rm(list = paste0("newiris", seq(1, 3, 1)))  # 구간 지울때
rm(list = ls(pattern = "new"))              # 특정 패턴을 지울때
rm(list = ls())                             # 한번에 모두 지울때


## 객체 만들기, 객체 이해하기 ----
# vector
v1 <- c(1:10)
v2 <- c(letters[1:10])
v3 <- rep(c(T, F), times = 5)

class(v1) ; v1
class(v2) ; v2
class(v3) ; v3

# vector words
length(v1)
range(v1)
rep(c(T, F), 2)
rep(c(T, F), each = 2)
seq(1, 10)
seq(1, 10, by = 2)
seq(1, 10, length = 5)
all(v1 > 0)
any(v1 < 0)
paste("iris", v1)
paste0("iris", v1)


# matrix
m1 <- matrix(1:12, ncol = 3)
m2 <- matrix(letters[1:12], ncol = 3)
m3 <- matrix(rep(c(T, F), 6), nrow = 4)

class(m1) ; m1
class(m2) ; m2
class(m3) ; m3

# matrix words
apply(m1, 1, sum)
apply(m1, 2, sum)


# array
a1 <- array(1:12, c(2, 3, 2))
a2 <- array(letters[1:12], c(2, 3, 2))

class(a1) ; a1
class(a2) ; a2


# data frame
df <- data.frame(v1, v2, v3)
class(df)
df

# data.frame words
dim(df)
colnames(df) 
nrow(df)
ncol(df)


# list 
my_list <- list(v1, m1, df)
class(my_list)
str(my_list, 1)

my_list
my_list[1]
my_list[[1]]
my_list[[1]][1]

my_list[3]
my_list[[3]][1]
my_list[[3]][[1]]

# list words
my_list2 <- list(c(1:3), c(4:6), c(7:9))
my_list2

lapply(my_list2, mean)
lapply(my_list2, magrittr::extract, 1)
lapply(my_list2, function(x) append(x, 1, after = 0))
lapply(my_list2, function(x) append(x, 10, after = length(x)))

