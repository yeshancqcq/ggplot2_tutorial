# change this to your own directory
setwd("C:/Users/user1/Documents/GitHub/ggplot2_tutorial/time_ranges")

library(readr)
library(ggplot2)

data <- read_csv("data/data.csv")

p <- ggplot()

for(i in 1:nrow(data)){
  x <- c(data$min_year[i],data$max_year[i])
  y <- c(data$arrangement[i],data$arrangement[i])
  plot.data <- data.frame(x = x,
                          y = y,
                          type = data$class[i])
  p <- p +
    geom_line(data=plot.data, aes(x=x,y=y,color=type))
  
  cat("finishing sample",i,"\n")
}


p <- p +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_rect(colour="black", fill = NA), 
        axis.line = element_line(color = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.justification = c(0, 0),
        axis.text.x = element_text(size = 14),
        #axis.ticks.x = element_blank(),
        axis.title = element_text(size = 20),
        legend.position = "right",
        text = element_text(family='Kai'),
        legend.background = element_rect(colour=NA, fill = NA),
        legend.key = element_rect(colour = "white", fill = NA)
  )+
  #custome your colors here, change the Type1, Type2, Type3 into Best, Shorter, Problem, etc.
  scale_color_manual( values = c(
    "Type1" = "red",
    "Type2" = "blue",
    "Type3" = "green"
  ))+
  #---------------------------
  #if you want to have a shaded verticle bar, use this section; otherwise, delete this section

  annotate("rect", fill = "tan", xmin = 11700, xmax = 12900, ymin = -Inf, ymax = Inf,
         alpha = .3)+
  annotate("text", x = 12300, y = 0, label = "YD", size = 4, color = 'orange3')+

  #--------------------------
  
  labs(y = "Samples",
       x = "Years BP",
       color = "Types")+
  scale_x_continuous( expand = c(0.025, 0.025),breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(limits=c(0,nrow(data)),expand = c(0.025, 0.02),breaks = scales::pretty_breaks(n = 10))

p
