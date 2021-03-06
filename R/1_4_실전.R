
## Quiz 
library(tidyverse)

Student <- c("김길동", "나동수", "다함께", "라구요",
             "박명수", "이동근", "송창식", "진정한",
             "정찬용", "한명회" )

Math    <- c(502, 600, 412, 358, 495,
             512, 410, 625, 573, 522)

Science <- c(95, 99, 80, 82, 75,
             85, 80, 95, 89, 86)

English <- c(25, 22, 18, 15, 20,
             28, 15, 30, 27, 18)

roster  <- data.frame(Student, Math, 
                      Science, English, 
                      stringsAsFactors = F)
roster
roster2 <- roster

##  상기 데이터에서 수학, 과학, 영어 3과목의 점수 합계에 대한 평가를 상위 20%는 "A" 다음 20%는 "B" 그 다음 20%는 "C" 마지막 20%는 "D"로 평가한 평가표를 만들어 보시오.  

## 방법 1
(z <- scale(roster[ , 2:4]))

roster$score <- rowMeans(z) ; roster

y <- quantile(roster$score, c(0.8, 0.6, 0.4, 0.2)) ; y

roster$grade[roster$score >= y[1]] <- "A"

roster$grade[roster$score <  y[1] &
             roster$score >= y[2]] <- "B"

roster$grade[roster$score <  y[2] &
             roster$score >= y[3]] <- "C"

roster$grade[roster$score <  y[3] &
             roster$score >= y[4]] <- "D"

roster$grade[roster$score <  y[4]] <- "F"

roster


## 방법 2
roster2 <- roster2 %>% 
  select(-Student) %>% 
  map_df(scale) %>% 
  mutate(score = rowMeans(.)) %>% 
  cbind(Student = roster$Student, .)

quant <- quantile(roster2$score, c(0.8, 0.6, 0.4, 0.2))

roster2 <- roster2 %>% 
  mutate(rank = case_when(
    score >= quant[1] ~ "A",
    score >= quant[2] & score < quant[1] ~ "B",
    score >= quant[3] & score < quant[2] ~ "C",
    score >= quant[4] & score < quant[3] ~ "D",
    TRUE ~ "F"))

roster2


## 방법 3
roster3 <- roster

tmp <- scale(roster3[ , 2:4])

roster3$score <- rowMeans(tmp)

y <- quantile(roster3$score, c(0.8, 0.6, 0.4, 0.2))

score <- roster3$score

finalgrade <- c()

for(i in score) {
if(i >= 0.74) {
  grade = "A"
} else if (i >= 0.44) {
  grade = "B"
} else if (i >= -0.36) {
  grade = "C"
} else if (i >= -0.89) {
  grade = "D"
} else {
  grade = "F"
}
  finalgrade = c(finalgrade, grade) # grade 결과를 하나씩 추가
}

roster3 <- cbind(roster3, grade = finalgrade)

roster3
