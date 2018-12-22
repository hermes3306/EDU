
## 자료 준비
library(arules)

# POS자료 읽어오기
read.transactions(file = "", sep = ",") 

data(Groceries)
gro <- Groceries
summary(gro)

inspect(gro[1:5]) # 5건의 거래(영수증)


## 상품 분류 보기
unique(itemInfo(gro)[["level1"]])
unique(itemInfo(gro)[["level2"]])

itemFrequency(gro[, 1:3]) # 3개 상품의 지지도
itemFrequencyPlot(gro, 
                  support = 0.1,
                  col = "dark red",
                  horiz = T)

itemFrequencyPlot(gro,
                  topN = 20,
                  col = "dark red",
                  horiz = T)

image(gro[1:5])
image(sample(gro,100))


## 규칙 찾기
apriori(gro) 
# default support=0.1, confidence=0.8

gro_rule <- apriori(gro,
   parameter = list(support = 0.006,
                    confidence = 0.25,
                    minlen = 2)
                    )
gro_rule 
# 퍼래미터 기준은 무엇일까요?
# 60/9835, 2개 상품 이상 조건

summary(gro_rule)
inspect(gro_rule[1:3]) # rule을 한번 보자
inspect(sort(gro_rule, by = "lift")[1:5])

berryrule <- subset(gro_rule, items %in% "berries") 
# %in% c("berries","yogurt") : 둘 중 하나
# %pin% "fruit" : tropical fruit or citrus fruit
# %ain% c("berries,"yogurt") : 둘다
inspect(berryrule)


## 그래프로 표현
library(arulesViz) ; library(RColorBrewer)

plot(gro_rule,
     measure = c("support", "confidence"),
     shading = "lift",
     interactive = F)
     
plot(berryrule,
     measure = c("support", "confidence"),
     shading = "lift",
     interactive = F)

# plot(berryrule,
#      measure = c("support", "confidence"), 
#      shading = "lift",
#      interactive = T)
     
plot(
  gro_rule,
  method = "grouped",
  control = list(col=rev(brewer.pal(9,"Greens")[4:9]))
     )

plot(berryrule,
     method = "graph",
     control = list(type = "items"),
     shading = "lift")


## saving rules (문법에 주의!)
write(gro_rule,
      file = "groceryrule.csv",
      sep = ",",
      row.names = F)
      
gro_rule_df <- as(gro_rule, "data.frame")

