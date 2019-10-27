library(ggplot2)
library(maps)
library(ggmap)
library(readr)
library(magrittr)
library(multipanelfigure)

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
        panel.background = element_blank(),  # Set a blank backgorund 
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

plot2 <- ggplot()+
  geom_line(data=wisconsin,aes(Year, W, colour = "Wisconsin")) +
  geom_line(data=iowa,aes(Year, W, colour = "Iowa")) +
  geom_line(data=minnesota,aes(Year, W, colour = "Minnesota")) +
  # Pair teams with colors
  scale_colour_manual(values = c('Wisconsin' = 'red', 'Iowa' = 'blue', 'Minnesota' = 'tan'))+
  labs(colour = 'Football Teams') + # Title of the Legend
  ggtitle("Number of Wins: Three Teams Comparison", "Wisconsin, Iowa and Minnesota") + # Subtitle
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), 
        axis.line.x = element_line(colour = "black"), axis.line.y= element_line(colour = "black"),
        legend.position = "bottom", # Move the legend to the bottom
        legend.spacing.x = unit(10, 'pt'), # 10 point spacing between legend elements
        legend.background = element_rect(colour = NA), # Remove the gray background of legend
        legend.key = element_rect(colour = NA, fill = NA) # Do not add boxes around legend
  ) +
  scale_x_continuous(name = expression("Football Seasons"), limits = c(1920, 2010)) +
  scale_y_continuous(name = expression("Number of Wins"), limits = c(0, 15))

plot2
  
# Initializing a new data frame named average_data
average_data <- data.frame(
  (matrix(vector(), 123, 5, 
          dimnames=list(
            c(), # This is for row names. Since we don't have row names, leave it blank
            c("Year","Wisconsin_Wins", "Iowa_Wins", "Minnesota_Wins", "Average_Wins") # Column names
            )
          )
   ))

average_data$Year <- wisconsin$Year
average_data$Wisconsin_Wins <- wisconsin$W
average_data$Iowa_Wins <- iowa$W
average_data$Minnesota_Wins <- minnesota$W

# Subsetting the data frame
average_data <- average_data[1:99,1:5] # Keep rows 1 to 99 and columns 1 to 5; drop everything else

# Average wins calculation
average_data$Average_Wins <- rowMeans(average_data[,2:4], # Only average columns 2 to 4 in average_data
                                      na.rm = TRUE, # Ignoring missing values
                                      dims = 1)

plot3 <- ggplot()+
  geom_line(data=average_data,aes(Year, Average_Wins)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA)) +
  ggtitle("Average wins of Wisconsin, Iowa and Minnesota") +
  scale_x_continuous(name = expression("Football Seasons"), limits = c(1920, 2010)) +
  scale_y_continuous(name = expression("Average Number of Wins"), limits = c(0, 15))

plot3

# Preparing data for plot4: calculating winning percentages of Wisconsin
# initializing 2 empty columns for wisconsin
wisconsin$Total_Games <- NA
wisconsin$Percentage <- NA

# calculate total games and percentages
wisconsin$Total_Games <- rowSums (wisconsin[,2:4], na.rm = FALSE, dims = 1)  
wisconsin$Percentage <- wisconsin$W / wisconsin$Total_Games

plot4 <- ggplot()+
  geom_line(data=wisconsin,aes(Year, Percentage*15+5, colour = "Percentage")) + # Plot percentage * 10
  geom_line(data=wisconsin,aes(Year, W, colour = "Wins")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA),
        axis.line.y.left = element_line(colour = "blue"), # set left axis elements to be blue
        axis.title.y.left = element_text(colour = "blue"),
        axis.text.y.left = element_text(colour = "blue"),
        axis.ticks.y.left = element_line(colour = "blue"),
        axis.line.y.right = element_line(colour = "red"), # set right axis elements to be red
        axis.title.y.right = element_text(colour = "red"),
        axis.text.y.right = element_text(colour = "red"),
        axis.ticks.y.right = element_line(colour = "red"),
        legend.position = "bottom", legend.spacing.x = unit(10, 'pt'), 
        legend.background = element_rect(colour = NA), legend.key = element_rect(colour = NA, fill = NA)
        ) +
  ggtitle("Wins and Winning Percentage of Wisconsin") +
  labs(colour = "Legend")+
  scale_colour_manual(values = c('Percentage' = 'red', 'Wins' = 'blue'))+
  scale_x_continuous(name = expression("Football Seasons"), limits = c(1920, 2010)) +
  scale_y_continuous(name = expression("Number of Wins"), limits = c(0, 25), 
                     sec.axis = sec_axis(~(.-5)/15, name = "Winning Percentage")) # scale down the 2nd axis

plot4

# set up the layout of plot 5 with 2 rows and 2 columns (for plots 1 ~ 4)
plot5 <- multi_panel_figure(columns = 2, rows = 2, panel_label_type = "upper-roman")
 
plot5 %<>%
  fill_panel(plot1, column = 1, row = 1) %<>%
  fill_panel(plot2, column = 2, row = 1) %<>%
  fill_panel(plot3, column = 1, row = 2) %<>%
  fill_panel(plot4, column = 2, row = 2)
plot5

# set up a new layout with 2 columns and 3 rows
plot6 <- multi_panel_figure(columns = 2, rows = 5, panel_label_type = "upper-roman")

plot6 %<>%
  fill_panel(plot1, column = 1, row = 1:2) %<>% # plot 1 occupies 2 upper left panels
  fill_panel(plot3, column = 2, row = 1:2) %<>% # plot 3 occupies 2 upper right panels
  fill_panel(plot4, column = 1:2, row = 3:5) # plot 4 occupies the 4 panels in rows 3, 4 and 5
plot6

# automatically plot
for(col in 2:4){
  # Get the column name (for the plot title)
  this_name <- names(average_data)[col]
  # construct a temporary data frame
  temp_df <-data.frame(Year=average_data$Year, Wins=average_data[,col])
  # plot the data
  this_plot <- ggplot()+
    geom_line(data=temp_df,aes(Year, Wins), colour = "red") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA), 
          axis.line.x = element_line(colour = "black"), axis.title.x=element_text(size=10), 
          axis.line.y= element_line(colour = "black"), axis.title.y=element_text(size=10)
    ) +
    ggtitle(paste("Plot for column: ", as.character(this_name)))
  
  this_plot
  
  # save the plot to the directory
  ggsave(paste(as.character(this_name), ".jpg"), plot = last_plot(), path = "img",
         width = 6, height = 4, units = "in",
         dpi = 300)
}

# import big ten data
bigten <- read_csv("data/bigten.csv")

# add a base map layer (this is provided by the "maps" package):
base <- map_data("state")

# plot the map:

map <- ggplot() + 
  geom_polygon(data = base, aes(x=long, y = lat, group = group), 
               fill = "white", color = "black", size = 0.05) +
  coord_fixed(xlim = c(-96, -73),  ylim = c(38, 46), ratio = 1.4)+
  geom_point(data = bigten, aes(x = lon, y = lat, colour=Division), size = 2.2) +
  scale_colour_manual(values = c('W' = 'red', 'E' = 'blue'))+
  geom_text(data = bigten, aes(x = lon, y = lat, label = paste("  ", as.character(University), sep="")), fontface = "bold",angle = 0, hjust = 0, color = "black", size = 3.5)+
  theme(
    axis.line = element_blank(),
    axis.text = element_text(colour = "black"),
    axis.ticks = element_line(colour = "black"),
    panel.border = element_rect(colour = "black", fill = NA),
    panel.grid = element_blank(),
    axis.title = element_blank(),
    legend.position = "bottom", 
    legend.spacing.x = unit(10, 'pt'), 
    legend.background = element_rect(colour = NA), 
    legend.key = element_rect(colour = NA, fill = NA)
  )+
  labs(colour = "Division")+
  ggtitle("Big Ten Conference") +
  annotate("text", x = -87, y = 41, label = "United States", size = 6, colour = "black", alpha = 0.3) 

map
















