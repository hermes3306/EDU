
## 실전

## load packages
pkgs <- c("tidyverse",
          "wbstats",
          "data.table",
          "plotly",
          "highcharter",
          "quantmod",
          "TTR")

sapply(pkgs, require, character.only = T)


# variable definition
# SP.POP.TOTL --> population
# NY.GDP.PCAP.CD --> gdp per capita
# SP.DYN.LE00.IN --> life expectancy at birth


## Pull in data
population <- wb('SP.POP.TOTL', country = 'countries_only') %>%
  mutate(date = as.numeric(date), 
         value = round(value / 1000000, 2), 
         indicator = 'Population') %>%
  select(-indicatorID)

gdp <- wb('NY.GDP.PCAP.CD', country = 'countries_only') %>% 
  mutate(date = as.numeric(date), 
         value = round(value, 2), 
         indicator = 'GDP per Capita') %>%
  select(-indicatorID)

life.expectancy <- wb('SP.DYN.LE00.IN', country = 'countries_only') %>%  
  mutate(date = as.numeric(date), 
        value = round(value, 2), 
        indicator = 'Life Expectancy') %>%
  select(-indicatorID)


## wrangle data
df2 <- population %>% 
  bind_rows(gdp) %>% 
  bind_rows(., life.expectancy)

df2 <- df2 %>% 
  spread(indicator, value)

df2 <- df2 %>% 
  na.omit %>% 
  rename(gdp = "GDP per Capita", life = "Life Expectancy", pop = "Population")


## plotly
g <- ggplot(data = df2 %>% filter(date == 2016)) + 
  geom_point(aes(x = gdp,
                 y = life,
                 color = pop,
                 size = pop,
                 text = country)) +
  ggtitle(label = "1인당 국내소득과 기대수명의 관계") + 
  viridis::scale_color_viridis() +
  scale_x_continuous(labels = scales::dollar)

ggplotly(g)


p <- df2 %>% 
  filter(country %in% c("China", "Korea, Rep.", "Japan", "United States")) %>% 
  ggplot() + 
  geom_point(aes(x = gdp,
                 y = life,
                 color = pop,
                 frame = date,
                 text = paste("Country: ", country, "<br>",
                              "GDP per capita: ", gdp, "<br>",
                              "Life Expectancy: ", life, "<br>",
                              "Population: ", pop, "MM", sep = ""),
                 size = pop)) + 
  ggtitle(label = "1인당 국내소득과 기대수명의 관계") +  
  viridis::scale_color_viridis() + 
  scale_x_continuous(labels = scales::dollar)

ggplotly(p, tooltip = "text")
