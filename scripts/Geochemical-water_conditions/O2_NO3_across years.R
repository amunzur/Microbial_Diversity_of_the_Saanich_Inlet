library(tidyverse)
library(wesanderson)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=12),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=15,  angle = 45),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=15),
                      axis.title = element_text(size=15),
                      legend.title = element_text(color = "black", size = 12),
                      legend.text = element_text(color = "black", size = 12),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=14, face="bold"),
                      strip.text.x = element_text(size=10, color="black",
                                                  face="bold"))

Saanich_Data <- read_csv("~/Desktop/MICB_405/MICB405_proj/Saanich_Data.csv")

########################################################
# O2 concentration across years, measured in august 
########################################################
df <- Saanich_Data
df <- df[, 3:7] # subset to cols we need 

# get idx of NA 
df <- df[-c(1:275), ]
df$Depth <- 1000*df$Depth # convert to m
names(df) <- c("cruise", "date", "depth", "O2", "NO3")

# get idx of august (08)
idx <- which(lapply(as.list(df$date), function(some_date) strsplit(as.character(some_date), split = "-")[[1]][[2]]) == "08")
df <- df[idx, ]

# remove something 
idx <- which(df$date == "2012-08-28")
df <- df[-c(idx), ]
df$date <- as.factor(df$date)

p8 <- ggplot(data = df, aes(x = depth, y = O2, group = date)) + 
  geom_line(aes(color = date)) +
  geom_point(aes(color = date)) + 
  scale_color_brewer(palette = "Reds") + 
  scale_x_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("O2 concentration ", "(", mu, "M", ")"))) +
  ggtitle("O2 concentration across years") +
  theme_asli

p9 <- ggplot(data = df, aes(x = depth, y = NO3, group = date)) + 
  geom_line(aes(color = date)) +
  geom_point(aes(color = date)) + 
  scale_color_brewer(palette = "Reds") + 
  scale_x_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("NO3 concentration ", "(", mu, "M", ")"))) +
  ggtitle("NO3 concentration across years") +
  theme_asli


figure_path <- paste("../FINAL_PROJ/figures/O2_amount_years.pdf", sep = "/")
p8
ggsave(figure_path, width = 19, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/NO3_amount_years.pdf", sep = "/")
p9
ggsave(figure_path, width = 19, height = 15, units = "cm")



