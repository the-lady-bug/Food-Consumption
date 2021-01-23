
# Link to the article of the dataset
# https://www.theguardian.com/news/datablog/2011/jun/17/food-prices-projections-oecd
# Link to the dataset
# https://docs.google.com/spreadsheets/d/1TgbO0GDG986FQN65B5QquiOT0jfIfuUB36XhZ_KaPms/edit?hl=en_US&hl=en_US#gid=2

#Install Packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("ggplot2")

#Load the dataset
data <- read.csv("data_try.csv")

# Libraries
library(tidyverse)
library(ggplot2)
library(dplyr) 
library(plyr)

# To arrange the plot
tmp<-data %>%
  filter(!is.na(data$Growth)) %>%
  arrange(data$Growth) %>%
  mutate(Food=factor(data$Food, data$Food))

# Data Graphics
plot<-ggplot(tmp , aes(x = Food, y =Growth, fill = Growth)) + 
  geom_bar(stat = "identity") +
  geom_col(position = "dodge") + 
  coord_polar() +  
  geom_bar(aes(x = Food, y = Growth, fill = Growth),stat = "identity")+ 
  geom_text(aes(label = Growth , hjust = 0.5, angle = data$angle, y = ifelse(Growth<5,8 ,Growth + 5)),color="black",fontface="bold",size = 3)+
  theme_minimal()+
  ylim(-2, 40)+ 
  labs(title = "Growth in food consumption", subtitle = "Growth in average food consumption in 2011-2020 as compared to 2001-2010 (in %)")+ 
  scale_fill_gradient(low = "light blue", high = "dodgerblue4",limits=c(0, 40))+

theme(
  plot.margin = margin(t = 0, r = 80, b = 0, l = 0, unit = "pt"),
  plot.title = element_text(size = rel(2),hjust = 0.5,face="bold",margin=margin(t = 20,b=10, unit = "pt")),
  plot.subtitle = element_text(size = rel(1.5),hjust = 0.5,face="italic",margin=margin(b=5, unit = "pt")),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  axis.text.x = element_text(size = rel(1.1),face="bold"),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_line(colour = "black",0.05),
  panel.ontop = FALSE,
  legend.position = c(1.1,0.5),
  legend.title = element_text(size = rel(1),face="bold",margin = margin(t = 0, r = 0, b = 10, l = 0)),
  legend.text.align =0.5,
  legend.margin = margin(10,10,10,10),
  legend.box.background = element_rect(),
  legend.box.margin=margin(10,10,10,10),
) 
plot + labs(fill = "Growth %")
# Note: Please view the data graph on Plot -> Zoom for better visuals

