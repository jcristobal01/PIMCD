library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(tibble)
data(mtcars)
df = mtcars %>%
  rownames_to_column( var = "car" ) %>% 
  mutate_each(funs(rescale), -car) %>% 
  melt(id.vars=c('car'), measure.vars=colnames(mtcars)) %>% 
  arrange(car)

line_plot = df %>%
  filter(variable=='mpg') %>%
  ggplot(aes(x=car, y=value, group=1)) + 
  geom_line(color = 'purple')
print(line_plot + coord_polar())

polygon_plot = df %>% 
  filter(variable=='mpg') %>%
  ggplot(aes(x=car, y=value, group=1)) + 
  geom_polygon(color = 'purple', fill=NA)
print(polygon_plot + coord_polar())
