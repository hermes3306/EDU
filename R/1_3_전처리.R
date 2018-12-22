
# 학습목표
# 1. factor 데이터를 이해한다.
# 2. 테이블 조인 방법을 학습한다.


## factor data
# 주의 사항 : 범주의 순서를 바꿀 때 값마저 바뀌지 않도록 주의

sizes <- factor(
  c("small", "large", "large", "small", "medium")
  )

sizes

levels(sizes)

sizes <- factor(
  sizes, 
  levels = c("small", "medium", "large")
  )

sizes  


## 대/중/소를 factor로 만들고 레벨순서를 변경해 보세요.
size <- factor(c("대", "대", "소", "중"))
size
size <- factor(size, levels = c("대", "중", "소"))


## 요일을 연습해 보세요.
요일 <- factor(c("월", "화", "수", "목", "금", "토", "일"))
levels(요일) 

day <- factor(
  요일, 
  levels = c("월", "화", "수", "목", "금", "토", "일")
  )
levels(day)


## 수치형데이터를 범주형으로 변환
(d <- data.frame(
  gender = c("M", "M", "F", "M", "F", "F"),
  age    = c(47, 59, 21, 32, 33, 24),
  income = c(55000, 88000, 32450, 76500, 123000, 45650),
  grade  = c(letters[1:6]) )
  )

d %>% 
  mutate(over25 = ifelse(age >= 25, 1, 0)) %>% 
  mutate(gender_over25 = interaction(gender, over25)) %>% 
  mutate(age2 = cut(age,
                    breaks = c(1, 20, 30, 40, 50, Inf),
                    right = F,
                    labels = c("20대 미만", 
                               "20대",
                               "30대",
                               "40대",
                               "50대 이상") )
         )


## 테이블 조인하기

## merging df
kids <- c("철수", "순이", "동수", "영희")
address <- c("서울", "인천", "대전", "부산")
df1 <- data.frame(kids, address)
df1$kids <- as.character(df1$kids) 

str(df1)

df2 <- data.frame(
  kids = c("순이","영철","길동"),
  ages = c(10, 7, 12),
  stringsAsFactors = F
  ) 

str(df2)

# inner join
inner_join(df1, df2)

# left join
left_join(df1, df2)

# right join
right_join(df1, df2)
  
# full join    
full_join(df1, df2)

# anti join
anti_join(df1, df2)


# 필드명이 다를 때
df3 <- data.frame(
  pals = c("길동","동수","영희"),
  ages = c(12, 11, 9),
  stringsAsFactors = F
  )

str(df3)

inner_join(df1, df3, by = c("kids" = "pals"))

full_join(df1, df3, by = c("kids" = "pals"))

left_join(df1, df3, by = c("kids" = "pals"))


# tips data에 관하여 다음 질문에 답해 보세요. ----
data(tips, package = "reshape2")

# 어느 요일에 팁을 가장 많이 받았나요?
# 요일별 평균적으로 받은 팁은 얼마인가요?
# 남성고객과 여성 고객중 어느쪽이 팁에 후한편인가요?
# 흡연 여부에 따른 성별 고객의 팁은?
