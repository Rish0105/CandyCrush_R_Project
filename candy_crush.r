# Setting up the size for plots for better viewing
options(repr.plot.width = 5, repr.plot.height = 4)

# Packages to be loaded
library(readr)
library(dplyr)
library(ggplot2)

# Reading Data
data <- read_csv("candy_crush.csv")

# Checking and printing few upper rows
head(data)

print("Number of players:")
length(unique(data$player_id))

print("Period for which we have data:")
range(data$dt)

# Calculating level difficulty
difficulty <- data %>%
  group_by(level)%>%
  summarise(attempts = sum(num_attempts), success=sum(num_success))%>%
  mutate(p_win = success/attempts)
# Printing the level difficulty
difficulty

# Plotting the level difficulty
library(scales)
ggplot(difficulty, aes(x=level, y=p_win))+
  geom_line()+
  scale_x_continuous(breaks=1:15)+
  scale_y_continuous(labels = percent)

# Adding points to the plot for spotting hard levels
ggplot(difficulty, aes(x=level, y=p_win))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=1:15)+
  scale_y_continuous(labels = percent)+
  geom_hline(yintercept = 0.1)

# Calculating the standard error of difficulty for each level
difficulty <- difficulty %>%
  mutate(error = sqrt(p_win * (1 - p_win) / attempts))
difficulty

# Adding standard error bars
ggplot(difficulty, aes(x=level, y=p_win))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks=1:15)+
  scale_y_continuous(labels = percent)+
  geom_errorbar(aes(ymin = p_win - error, ymax = p_win + error))
geom_hline(yintercept = 0.1)

# The probability of completing the episode without losing a single time
p <- prod(difficulty$p_win)
# Printing it out
p

