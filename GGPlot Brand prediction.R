library(ggplot2)

ggplot(dat, aes(x=xvar, y=yvar)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)   # Add linear regression line 
#  (by default includes 95% confidence region)

ggplot(data = Incom) +
  geom_point(aes(x = age, y = salary))

ggplot(Incom) +
  geom_violin(aes(x = age, y = salary))

ggplot(data = Incom) +
  geom_point(
    aes(x = age, y = salary, color = cut)) +
  scale_color_brewer(palette = "Set1") +
  theme_bw(10)

ggplot(data = Incom) +
  geom_point(
    aes(x = age, y = salary, color = cut)) +
  scale_color_manual(
    values = c("orange", "yellow", "red", "green"))