analyzer <- read.csv("c:/Users/Albert/Documents/Data_set_6_Fifa_Rankings/fifa_ranking.csv", header = TRUE)
analyzerlollipopchart <- analyzer[57583:57652,c(2,4)]
analyzerlollipopchart1 <- analyzer[57653:57722,c(2,4)]
analyzerlollipopchart2 <- analyzer[57723:57793,c(2,4)]
library(ggplot2)
library(scales)
theme_set(theme_bw())
ggplot(analyzerlollipopchart, aes(x=country_full, y=total_points)) + 
geom_point(size=2) + 
geom_segment(aes(x=country_full, 
                 xend=country_full, 
                 y=0, 
                 yend=total_points)) + 
labs(title="Lollipop Chart", 
     subtitle="A Statistical display of Total points for the first\n70 Footballing countries", 
     caption="source: analyzerlollipopchart") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(analyzerlollipopchart1, aes(x=country_full, y=total_points)) + 
  geom_point(size=2) + 
  geom_segment(aes(x=country_full, 
                   xend=country_full, 
                   y=0, 
                   yend=total_points)) + 
  labs(title="Lollipop Chart", 
       subtitle="A Statistical display of Total points for the second\n70 Footballing countries", 
       caption="source: analyzerlollipopchart") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(analyzerlollipopchart2, aes(x=country_full, y=total_points)) + 
  geom_point(size=2) + 
  geom_segment(aes(x=country_full, 
                   xend=country_full, 
                   y=0, 
                   yend=total_points)) + 
  labs(title="Lollipop Chart", 
       subtitle="A Statistical display of Total points for the last\n66 Footballing countries", 
       caption="source: analyzerlollipopchart") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.5))