# west african population logit function for temperature to proportion male
# dr. ana caldras patricio

# set working directory
setwd("~/Projects/iliketurtles/code")

# load libraries
library(ggplot2)

# load in data
logit <- read.csv("C:/Users/vique/Box Sync/Quennessen_Thesis/PhD Thesis/Dr Ana Caldas Patricio/logit_points.csv")

# pivotal temperature
diff <- abs(logit$p_males - 0.50)
pivot_temp <- logit$Temp[which(diff == min(diff))]

# plot points
ggplot(data = logit, aes(x = Temp, y = p_males)) +
  geom_vline(xintercept = pivot_temp, size = 1, col = 'gray40', lty = 2) +
  geom_path(col = 'darkblue', size = 2) +
  ylab("Proportion Male") +
  xlab("Temperature (\u00B0C)") +
  theme_grey() +
  theme(text = element_text(size = 20), 
        axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
        axis.title.x = element_text(margin = margin(t = 15, b = 10))) +
  annotate("text", x = 30, y = 0.94, label = "29.2 \u00B0C", size = 6)

