library(ggplot2)
library(maps)
library(ggmap)
library(readr)

# import football data of Wisconsin, Iowa and Minnesota
wisconsin <- read_csv("data/wisconsin_football.csv")
iowa <- read_csv("data/iowa_football.csv")
minnesota <- read_csv("data/minnesota_football.csv")

# Add a new data frame for the reference box
ref <- data.frame(xmin = 1970, xmax = 1982, ymin = -Inf, ymax = Inf)

# plot Wisconsin data: number of wins each year
plot1 <- ggplot()+
  geom_line(data=wisconsin,aes(Year, W), colour = "dark red") +
  geom_hline(yintercept = 5, size = 0.6, colour = "orange") + # Add 5-win reference
  geom_vline(xintercept = 2005, size = 0.6, linetype = 'dotted') + # Add 2005 reference
  geom_rect(data=ref, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", 
            alpha=0.1, inherit.aes = FALSE)+ # Add the green reference box w/ 10% transparency (alpha)
  annotate("text", x = 2005, y = 15, 
           label = "2005 season", size = 2.8, colour = "black")+ # Add label for 2005 season
  ggtitle("Wisconsin Football: Number of Wins Each Season") +
  theme(panel.grid.major = element_blank(), # Get rid of major grid lines
        panel.grid.minor = element_blank(), # Get rid of minor grid lines
        panel.background = element_blank(),  # Set a light blue backgorund 
        panel.border = element_rect(colour = "black", fill=NA), # Add solid borders
        axis.line.x = element_line(colour = "black"), # Have a solid x axis in black color
        axis.title.x=element_text(size=10), # Set the size of texts on x axis to be 10 points
        axis.line.y= element_line(colour = "black"), # Have a solid y axis in black color
        axis.title.y=element_text(size=10) # Set the size of texts on y axis to be 10 points
        # Note: no "," at the end of the last line in the theme() function
  ) +
  #Let's reverse the x axis:
  scale_x_reverse(name = expression("Wisconsin Football Seasons"), # Label on x axis
                  # Remember to change the following line:
                  limits = c(2010, 1920), # Lower and upper limits of x axis
                  breaks = scales::pretty_breaks(n = 10) # Number of tick marks on x axis
                  ) +
  scale_y_continuous(name = expression("Number of Wins"), # Label on y axis
                     limits = c(0, 20), # Lower and upper limits of y axis
                     breaks = scales::pretty_breaks(n = 5) # Number of tick marks on y axis
  )

plot1
