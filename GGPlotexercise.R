
library(ggplot2)

# load data ---------------------------------------------------------------

temp <- readRDS("Blackwell_Hist_Sample.rds")

# plot --------------------------------------------------------------------

ggplot(data = temp) +
  geom_point(aes(x = amount, y = age, color = (region == "4")))

#laad data
data(mtcars)
View(mtcars)
mtcarsplot <- mtcars

#opbouw x en y as, grafiek zonder data
ggplot(mtcarsplot, aes(x = hp, y = mpg))

#visualiseer de datapunten met geom_point()
ggplot(data = mtcarsplot) + 
  geom_point (aes(x = hp, y = mpg))

#kleur de punten in (3e dimensie) met het gewicht van de auto (kolom wt)
qplot(data=mtcarsplot, x=hp, y=mpg,geom = "point") 





