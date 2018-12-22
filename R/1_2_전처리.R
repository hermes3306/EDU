
# 학습목표
# 1. tidyverse 패키지를 이해한다.
# 2. 인터넷에 있는 테이블 데이터를 읽어온다.


## iris data 연습

# 행추출
iris %>% 
  filter(Species == "setosa") %>% 
  head()

iris %>%
  filter(Petal.Length >= mean(Petal.Length)) %>% 
  head()


# 열추출
iris %>% 
  select(1, 3, 5) %>% 
  head()

iris %>% 
  select(-Species) %>% 
  head()

iris %>% 
  select(starts_with("Petal")) %>% 
  head()

iris %>% 
  select(ends_with("Length")) %>% 
  head()


# 변수 생성
iris %>% 
  mutate(Lengths = Petal.Length * Petal.Width) %>% 
  head()


# 정렬
iris %>% 
  arrange(desc(Petal.Length)) %>% 
  head()


# 요약
iris %>% 
  group_by(Species) %>% 
  summarise(means = mean(Petal.Length), meds = median(Petal.Length))

iris %>% 
  group_by(Species) %>% 
  summarise_at(vars(-Species), funs(mean))

iris %>% 
  group_by(Species) %>% 
  summarise_if(is.numeric, mean)

iris %>% 
  summarise_all(length)


# 행 id 추가 및 행 자르기
iris %>% 
  rownames_to_column(var = "id") %>% 
  slice(c(1, 3, 5))

iris %>% 
  rownames_to_column(var = "id") %>% 
  slice(seq(1, nrow(iris), 2))


## mtcars 데이터로 연습하세요.
data(mtcars)

# 1. 행추출 : 4기통 자동차 추출
# 2. 열추출 : 연비와 실린더 변수 선택
# 3. 정렬   : 연비 내림차순으로 정렬
# 4. 요약   : 실린더별 연비 요약



## 외부 데이터 읽어오기(html table) ----
library(httr)
library(XML)

# 미국 국방력은 어느 정도인가요?
url <- "http://en.wikipedia.org/wiki/List_of_countries_by_military_expenditures"

url <- GET(url)
mytable <- rawToChar(url$content)

tbl <- readHTMLTable(
  mytable,
  which = 3,
  stringsAsFactors = F)

glimpse(tbl)

mytbl <- tbl %>% 
  rename(rank = V1, country = V2, spending = V3) %>% 
  slice(-1) %>% 
  mutate(rank = as.numeric(rank), 
         spending = as.numeric(spending))

mytbl

mytbl$spending[2:10] %>% sum()
