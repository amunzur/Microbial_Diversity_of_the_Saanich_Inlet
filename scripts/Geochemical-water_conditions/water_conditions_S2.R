library(tidyverse)
library(wesanderson)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=12),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=20,  angle = 45),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=20),
                      axis.title = element_text(size=20,face="bold"), 
                      legend.title = element_text(color = "black", size = 12),
                      legend.text = element_text(color = "black", size = 12),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=20, face="bold"),
                      strip.text.x = element_text(size=10, color="black",
                                                  face="bold"))

Saanich_Data <- read_csv("~/Desktop/MICB_405/MICB405_proj/Saanich_Data.csv")

######################################################
# 3 DEPTHS ONLY
######################################################

df <- Saanich_Data %>% filter(Cruise == 72, Depth == 0.100 | Depth == 0.120 | Depth == 0.200)

# subset data 
idx <- c(5, 27, 28, 29)
df <- df[, idx]
df$Depth <- c("100 m", "120 m", "200 m")

p1 <- ggplot(data = df, aes(x = Depth, y = Temperature, group = "Depth")) + 
  geom_line(linetype="solid", color="#f1bb7b", size=3) +
  geom_point(color="#f1bb7b", size=7) + 
  theme_classic() + 
  xlab("Depth") + 
  ylab("Temperature (°C)") + 
  ggtitle("Temperature across depths") +
  theme_asli

p2 <- ggplot(data = df, aes(x = Depth, y = Salinity, group = "Depth")) + 
  geom_line(linetype="solid", color="#fd6468", size=3) +
  geom_point(color="#fd6468", size=7) + 
  theme_classic() + 
  xlab("Depth") + 
  ylab("Salinity (ppt)") + 
  ggtitle("Salinity across depths") +
  theme_asli

p3 <- ggplot(data = df, aes(x = Depth, y = Density, group = "Depth")) + 
  geom_line(linetype="solid", color="#5b1a18", size=3) +
  geom_point(color="#5b1a18", size=7) + 
  theme_classic() + 
  ylim(23.9, 24.2) + 
  xlab("Depth") + 
  ylab("Density (1kg/m3)") + 
  ggtitle("Density across depths") +
  theme_asli


gridExtra::grid.arrange(p1, p2, p3, nrow = 1)

######################################################
# ALL DEPTHS
######################################################

df <- Saanich_Data %>% filter(Cruise == 72)

# subset data 
idx <- c(5, 27, 28, 29)
df <- df[, idx]
df$Depth <- 1000*df$Depth # conver to meters

p1 <- ggplot(data = df, aes(x = Depth, y = Temperature, group = "Depth")) + 
  geom_line(linetype="solid", color="#f1bb7b", size=1) +
  geom_point(color="#f1bb7b", size=4) + 
  scale_x_continuous(breaks = c(0, 20, 40, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab("Temperature (°C)") + 
  ggtitle("Temperature across depths") +
  theme_asli

p2 <- ggplot(data = df, aes(x = Depth, y = Salinity, group = "Depth")) + 
  geom_line(linetype="solid", color="#fd6468", size=1) +
  geom_point(color="#fd6468", size=4) + 
  scale_x_continuous(breaks = c(0, 20, 40, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab("Salinity (ppt)") + 
  ggtitle("Salinity across depths") +
  theme_asli

p3 <- ggplot(data = df, aes(x = Depth, y = Density, group = "Depth")) + 
  geom_line(linetype="solid", color="#5b1a18", size=1) +
  geom_point(color="#5b1a18", size=4) + 
  scale_x_continuous(breaks = c(0, 20, 40, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab("Density (1kg/m3)") + 
  ggtitle("Density across depths") +
  theme_asli

figure_path <- paste("../FINAL_PROJ/figures/SUPP/temperature_across_depths.pdf", sep = "/")
p1
ggsave(figure_path, width = 19, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/salinity_across_depths.pdf", sep = "/")
p2
ggsave(figure_path, width = 19, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/density_across_depths.pdf", sep = "/")
p3
ggsave(figure_path, width = 19, height = 15, units = "cm")







