library(ggplot2)
library(readr)
NewIncom <- Incom
View(NewIncom)

qplot(data=NewIncom, x=age, y=salary,geom = "point")
ggsave("plotagevssalary.png", width = 5, height = 5)
qplot(data=NewIncom, x=zipcode, y=brand_pred,geom = "point")
ggsave("plotzipvsbrand_pred.png", width = 5, height = 5)
qplot(data=NewIncom, x=elevel, y=brand_pred,geom = "point")
ggsave("plotelevelvsbrand_pred.png", width = 5, height = 5)
qplot(data=NewIncom, x=car, y=brand_pred,geom = "point")
ggsave("plotcarvsbrand_pred.png", width = 5, height = 5)

ggplot(data = NewIncom) +
  geom_point(aes(x = age, y = salary, col = brand_pred))
ggsave("plotagevssalary_col_brand_pred.png", width = 5, height = 5)

ggplot(data = NewIncom) +
  geom_point(aes(x = age, y = salary, col = brand_pred))
ggsave("plotagevssalary_col_brand_pred.png", width = 5, height = 5)

ggplot(Incom) +
  geom_violin(aes(x = age, y = salary, col = brand_pred))
ggsave("plot_Violin_agevssalary_brand_pred.png", width = 5, height = 5)

ggplot(Incom, aes(factor(brand_pred))) + 
         geom_bar(fill="white", colour = "red")
ggsave("barplotbrand_pred.png", width = 5, height = 5) 

ggplot(Incom, aes(x = factor(brand_pred), fill = elevel)) + 
  geom_bar() +
ggsave("barplotbrand_pred+fillelevel.png", width = 5, height = 5)

ggplot(Incom, aes(x = factor(brand_pred), fill = elevel)) + 
  geom_bar() +
  facet_wrap(~ zipcode)
ggsave("barplotbrand_pred+fillelevel_facetwrapzipcode.png", width = 5, height = 5)
