library(tidyverse)
library(readxl)
data <- read_xlsx("E:\\金融\\国际金融学\\hw2\\美国经济.xlsx")
names(data)[1] <- "时间"

data2 <- data%>%
  mutate(季度 = str_extract(时间,"^\\w+-"))%>%
  mutate(季度 = str_extract(季度,"\\w+"))%>%
  mutate(季度 = case_when(
    季度=="I"~1,
    季度=="II"~2,
    季度=="III"~3,
    季度=="IV"~4))%>%
  mutate(年份 = str_extract(时间,"\\d+"))%>%
  arrange(年份,季度)%>%
  select(年份,季度,`直接税收总额（本国货币单位）`,`税收和社会保障金占GDP比重（%）`,`季节性调整后季度水平--现价（百万本国货币单位）`)
names(data2) <- c("year","season","tax","social","gdp")
data2 <- data2%>%
  mutate_all(~as.numeric(.))%>%
  group_by(year)%>%
  reframe(tax = sum(tax),social = mean(social), gdp = sum(gdp))%>%
  mutate(year = make_date(year,1,1))

data2%>%
  ggplot()+
  geom_line(aes(x = year,y = tax,group = 1))
data2%>%
  ggplot()+
  geom_line(aes(x = year,y = social,group = 1),color = "blue") 

data2%>%
  ggplot()+
  geom_line(aes(x = year,y = gdp,group = 1),color = "red")
  
