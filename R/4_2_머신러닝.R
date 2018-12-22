
## clustering with iris data

## load packages
pkgs <- c("factoextra", 
          "NbClust", 
          "FactoMineR", 
          "cluster",
          "tidyverse")

sapply(pkgs, require, character.only = T)


## k-menas clustering
scaled_data <- scale(iris[ , 1:4])
k.means <- kmeans(scaled_data, 3)
k.means

plot(iris, col = k.means$cluster)

kmeans_list <- as.list(k.means)

kmeans_list$centers %>% 
  as_tibble() %>%
  rownames_to_column(var = "Group") %>% 
  gather(key, value, -Group) %>% 
  ggplot(aes(Group, value, fill = key)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Group) +
  theme(text = element_text(family = "AppleGothic"))

my_data <- bind_cols(iris, cluster = kmeans_list$cluster)
my_data %>% 
  group_by(Species, cluster) %>% 
  count()

nb <- NbClust(scaled_data, min.nc = 2, max.nc = 10, method = "kmeans")

fviz_nbclust(nb)

km.result <- kmeans(scaled_data, 3)

fviz_cluster(km.result, 
             data = scaled_data,
             ellipse.type = "convex",
             palette = "jco", # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal())


# distance measure
# r.dist <- get_dist(iris[1:4], stand = T)
# fviz_dist(r.dist, lab_size = 5)


# 최적의 k는 어떻게 구할 것인가?
# 1. elbow method : wss이 작을 수록 좋음
# 2. Average silhouette method : 값이 클수록 좋음
# 3. Gap statistic method : 값이 클수록 좋음

# fviz_nbclust(scaled_data, kmeans, method = "wss") +
#   geom_vline(xintercept = 3, linetype = 2)+
#   labs(subtitle = "Elbow method")
# 
# fviz_nbclust(scaled_data, kmeans, method = "silhouett") +
#   labs(subtitle = "Silhouette method")
# 
# fviz_nbclust(scaled_data, kmeans, method = "gap_stat", nboot = 500) +
#   labs(subtitle = "Gap statistic method")


## hierarchical clustering
dist_iris <- dist(scaled_data)
hier <- hclust(dist_iris, method = "ward.D2")
plot(hier)
rect.hier <- rect.hclust(hier, k = 3, border = "red")

clusters <- cutree(hier, 3)
clusters
plot(iris, col = clusters)


## model based clustering
library(mclust)
m.clust <- Mclust(scaled_data)
m.clust
plot(m.clust)


## clustering 활용 예
data("USArrests")

mydata <- USArrests
head(mydata)
mydata <- mydata %>% scale() %>% as.data.frame()

fviz_nbclust(mydata, kmeans, method = "wss")
fviz_nbclust(mydata, kmeans, method = "silhouett")
fviz_nbclust(mydata, kmeans, method = "gap_stat", nboot = 500)
nb <- NbClust(mydata, min.nc = 2, max.nc = 10, method = "kmeans")
fviz_nbclust(nb)

set.seed(123)
km.arrest <- kmeans(mydata, 4)
fviz_cluster(km.arrest, 
             data = mydata,
             ellipse.type = "convex",
             palette = "jco", # Color palette see ?ggpubr::ggpar
             ggtheme = theme_minimal())

mydata <- cbind(mydata, cluster = km.arrest$cluster)

usa <- map_data("state")
usa$region <- factor(usa$region)
mydata$name <- tolower(rownames(mydata))


usa <- usa %>% 
  left_join(mydata, by = c("region" = "name"))

ggplot(usa) + 
  aes(long, lat, group = group, fill = as.factor(cluster)) + 
  geom_polygon() +
  theme(legend.position = "none")

library(ggmap)
us.map <- get_map("USA", zoom = 3, maptype = "hybrid")
ggmap(us.map) +
  geom_polygon(data = usa, 
               aes(long, 
                   lat,
                   group = group,
                   fill = as.factor(cluster)),
               alpha = 0.7) +
  theme(legend.position = "none")
