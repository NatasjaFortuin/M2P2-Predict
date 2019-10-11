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

#Original data scatterplot----
ggplot(data = CompleteResponses) +
  geom_point(aes(x = age, y = salary, col = brand))
  labs(title = "Scatterplot complete data", subtitle = "brandpreference per age group")
ggsave("plotagevssalary_col_brand.png", width = 5, height = 5)

#Predicteddata scatterplot----
ggplot(data = NewIncom) +
  geom_point(aes(x = age, y = salary, col = brand_pred))
labs(title = "Scatterplot predicted data", subtitle = "brandpreference per age group")
ggsave("plotagevssalary_col_brand_pred.png", width = 5, height = 5)

ggplot(Incom) +
  geom_violin(aes(x = age, y = salary, col = brand_pred))
ggsave("plot_Violin_agevssalary_brand_pred.png", width = 5, height = 5)

ggplot(Incom, aes(factor(brand_pred))) + 
         geom_bar(fill="white", colour = "red") +
          labs(x = "brand pref") + 
          labs(y = "count of brand pref")
ggsave("barplotbrand_pred.png", width = 5, height = 5) 

ggplot(Incom, aes(x = factor(brand_pred), fill = elevel)) + 
  geom_bar() + 
  labs(x = "brand pref") + 
  labs(y = "total count per level of education")
ggsave("barplotbrand_pred+fillelevel.png", width = 5, height = 5)

ggplot(Incom, aes(x = factor(brand_pred), fill = elevel)) + 
  geom_bar() +
  facet_wrap(~ zipcode)
ggsave("barplotbrand_pred+fillelevel_facetwrapzipcode.png", width = 5, height = 5)
