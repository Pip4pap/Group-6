analyzer <- read.csv("c:/Users/Albert/Documents/Data_set_6_Fifa_Rankings/fifa_ranking.csv", header = TRUE)
analyzerboxplot <- analyzer[57583:57793,c(1:4,15)]
analyzerboxplot

require("RColorBrewer")
boxplot(analyzerboxplot$total_points~analyzerboxplot$confederation,
        col = brewer.pal(12,"Set3"),
        names = c("AFC",
                  "CAF",
                  "CONCACAF",
                  "CONMEBOL",
                  "OFC",
                  "UEFA"),
        boxwex = 0.5,
        whisklty = 1,
        staplelty = 0,
        outpch = 16,
        outcol = brewer.pal(12,"Set3"),
        main = "Total Points as per Confederations",
        xlab = "Confederations",
        ylab = "Total Points"
)

library(ggplot2)
theme_set(theme_bw())

# plot
g <- ggplot(analyzerboxplot, aes(confederation, total_points))
g + geom_boxplot() + 
  geom_dotplot(binaxis='y', 
               stackdir='center', 
               dotsize = .5, 
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Box plot + Dot plot", 
       subtitle="Total Points vs Confederation: Each dot represents 1 row in source data",
       caption="Source: analyzerboxplot",
       x="Confederations",
       y="Total Points")