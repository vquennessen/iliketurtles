# make figures representing output

# set working directory
setwd('~/Projects/iliketurtles')

# load libraries
library(ggplot2)
library(viridis)

# load in first output file
load('~/Projects/iliketurtles/output/SSP1-1.9/beta10/10000_abundance.Rda')

# if year1 is 2022, calculate year indices for 2040, 2060, and 2100
start_year <- 2023
end_year <- 2100
years <- start_year:end_year

index1 <- which(years == 2040)
index2 <- which(years == 2060)
index3 <- which(years == 2100)

# pull out number of simulations where population survived to 2040, 2060, 2100
DF <- data.frame(Scenario = NULL, 
                 Beta = NULL, 
                 Survive_to = NULL, 
                 Probability = NULL)

# Scenarios
Scenarios <- c('SSP1-1.9', 'SSP1-2.6', 'SSP2-4.5', 'SSP3-7.0', 'SSP5-8.5')

# Betas
Betas <- c(1, 2, 3, 5, 10, 20, 50, 100)

for (s in 1:length(Scenarios)) {
  
  for (b in 1:length(Betas)) {
    
    # load in appropriate output file
    load(paste('~/Projects/iliketurtles/output/', Scenarios[s], '/beta', 
               Betas[b], '/10000_abundance.Rda', sep = ''))
    
    DF2 <- data.frame(Scenario = rep(Scenarios[s], 3),
                      Beta = rep(Betas[b], 3), 
                      Survive_to = c(2040, 2060, 2100), 
                      Probability = c(sum(sims_abundance[index1, ] > 0) / 10000, 
                                      sum(sims_abundance[index2, ] > 0) / 10000,
                                      sum(sims_abundance[index3, ] > 0) / 10000))
    
    DF <- rbind(DF, DF2)
    
  }
  
}

# heatmap for survival to all three years - horizontal
fig <- ggplot(data = DF, aes(x = as.factor(Beta), y = Scenario, fill = Probability)) +
  geom_tile(color = "white",
            lwd = 1.5,
            linetype = 1) +
  # coord_fixed() +
  facet_wrap(vars(Survive_to)) +
  scale_fill_gradientn(colors = hcl.colors(5, "viridis")) +
  guides(fill = guide_colourbar(title = "Probability \n of population \n persistence")) +
  xlab('Beta Value') +
  ylab('Climate Scenario')
# +
#   theme(text = element_text(size = 15), 
#         axis.title.y = element_text(margin = margin(r = 15, l = 10)), 
#         axis.title.x = element_text(margin = margin(t = 15, b = 10)))

# # heatmap for survival to all three years - 2x2
# ggplot(data = DF, aes(x = as.factor(Beta), y = Scenario, fill = Probability)) +
#   geom_tile(color = "white",
#             lwd = 1.5,
#             linetype = 1) +
#   coord_fixed() +
#   facet_wrap(vars(Survive_to), ncol = 2) +
#   scale_fill_gradientn(colors = hcl.colors(5, "viridis")) +
#   guides(fill = guide_colourbar(title = "Probability \n of survival")) +
#   xlab('Beta') +
#   theme(text = element_text(size = 15)) +
#   theme(legend.position = c(0.75, 0.2))
# 
# # heatmap for only survival to 2060
# DF %>%
#   filter(Survive_to == 2060) %>%
#   ggplot(aes(x = as.factor(Beta), y = Scenario, fill = Probability)) +
#   geom_tile(color = "white",
#             lwd = 1.5,
#             linetype = 1) +
#   coord_fixed() +
#   scale_fill_gradientn(colors = hcl.colors(5, "viridis")) +
#   guides(fill = guide_colourbar(title = "Probability \n of survival \n to 2060")) +
#   xlab('Beta') +
#   theme(text = element_text(size = 15))
# 
# 
# # heatmap for only survival to 2100
# DF %>%
#   filter(Survive_to == 2100) %>%
#   ggplot(aes(x = as.factor(Beta), y = Scenario, fill = Probability)) +
#   geom_tile(color = "white",
#             lwd = 1.5,
#             linetype = 1) +
#   coord_fixed() +
#   scale_fill_gradientn(colors = hcl.colors(5, "viridis")) +
#   guides(fill = guide_colourbar(title = "Probability \n of survival \n to 2100")) +
#   xlab('Beta') +
#   theme(text = element_text(size = 15))

# save to file
ggsave(plot = fig, 
       filename = 'persistence_heatmap.png',
       path = '~/Projects/iliketurtles/figures/',
       width = 8, height = 3)
