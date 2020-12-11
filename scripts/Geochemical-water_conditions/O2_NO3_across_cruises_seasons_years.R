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
# Cruise 72 - O2 and NO3 concentration
########################################################
df <- Saanich_Data %>% filter(Cruise == 72)
idx <- c(5, 6, 9)

df <- df[, idx] # subset to cols we need 
df$Depth <- 1000*df$Depth # convert to m
names(df) <- c("depth", "O2", "NO3")

# modify the df for plotting 
depth <- rep(df$depth, 2)
amount <- c(df$O2, df$NO3)
molecule <- c(rep("O2", 16), rep("NO3", 16))

df_new <- as.data.frame(cbind(as.numeric(depth), as.numeric(amount), molecule))

p2 <- ggplot(data = df, aes(x = depth, y = O2)) + 
  geom_line(linetype="solid", color="#e0c41c", size=2) +
  geom_point(color="#e0c41c", size=5) + 
  scale_x_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("O2 concentration ", "(", mu, "M", ")"))) +
  ggtitle("O2 concentration across depths - Cruise 72") +
  theme_asli

p3 <- ggplot(data = df, aes(x = depth, y = NO3)) + 
  geom_line(linetype="solid", color="#c7772f", size=2) +
  geom_point(color="#c7772f", size=5) + 
  scale_x_continuous(breaks=c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("NO3 concentration ", "(", mu, "M", ")"))) +
  ggtitle("NO3 concentration across depths - Cruise 72") +
  theme_asli

combined <- ggplot(data = df_new, aes(x = depth, y = amount, group = molecule)) + 
  geom_line(aes(color = molecule), size = 1.5) +
  geom_point(aes(color = molecule), size = 2) + 
  scale_color_manual(values=c("#e0c41c", "#c7772f")) +
  scale_x_continuous(breaks=c(0, 20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("molecule concentration ", "(", mu, "M", ")"))) +
  ggtitle("NO3 and O2 concentration across depths - Cruise 72") +
  theme_asli

figure_path <- paste("../FINAL_PROJ/figures/O2_amount.pdf", sep = "/")
p2
ggsave(figure_path, width = 19, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/NO3_amount.pdf", sep = "/")
p3
ggsave(figure_path, width = 19, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/O2_NO3_combined.pdf", sep = "/")
combined
ggsave(figure_path, width = 19, height = 15, units = "cm")


########################################################
# Cruise 67, 68, 69, 70, 71, 72 - O2 and NO3 concentration
########################################################
df <- Saanich_Data %>% filter(Cruise == 67 | Cruise == 68 | Cruise == 69 | Cruise == 70 | Cruise == 71 | Cruise == 72 )
idx <- c(3, 5, 6, 9)
df <- df[, idx] # subset to cols we need 
df$Depth <- 1000*df$Depth # convert to m
names(df) <- c("cruise", "depth", "O2", "NO3")
df$cruise <- as.factor(df$cruise)


p4 <- ggplot(data = df, aes(x = depth, y = O2, group = cruise)) + 
  geom_line(aes(color = cruise), size = 1.5) +
  geom_point(aes(color = cruise), size = 2) + 
  scale_color_manual(values=c("#ddba33", "#e4072b", "#098a75", "#e47721", "#5fabc9", "#28203e")) +
  scale_x_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("O2 concentration ", "(", mu, "M", ")"))) +
  ggtitle("O2 concentration") +
  theme_asli

p5 <- ggplot(data = df, aes(x = depth, y = NO3, group = cruise)) + 
  geom_line(aes(color = cruise), size = 1.5) +
  geom_point(aes(color = cruise), size = 2) + 
  scale_color_manual(values=c("#ddba33", "#e4072b", "#098a75", "#e47721", "#5fabc9", "#28203e")) + 
  scale_x_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("NO3 concentration ", "(", mu, "M", ")"))) +
  ggtitle("NO3 concentration") +
  theme_asli

figure_path <- paste("../FINAL_PROJ/figures/O2_amount_cruises.pdf", sep = "/")
p4
ggsave(figure_path, width = 19, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/NO3_amount_cruises.pdf", sep = "/")
p5
ggsave(figure_path, width = 19, height = 15, units = "cm")



########################################################
# across seasons, cruise 66, 69, 72, 77
########################################################
df <- Saanich_Data %>% filter(Cruise == 66 | Cruise == 69 | Cruise == 72 | Cruise == 77 )
idx <- c(3, 5, 6, 9)
df <- df[, idx] # subset to cols we need 
df$Depth <- 1000*df$Depth # convert to m
names(df) <- c("cruise", "depth", "O2", "NO3")
df$cruise <- as.factor(df$cruise)

# rename some of the depths 
idx <- which(df$cruise == 66)
a <- rep("66 - February", 16)

idx <- which(df$cruise == 69)
b <- rep("69 - May", 16)

idx <- which(df$cruise == 72)
c <- rep("72 - August", 16)

idx <- which(df$cruise == 77)
d <- rep("77 - November", 16)

df$cruise <- c(a, b, c, d)


p6 <- ggplot(data = df, aes(x = depth, y = O2, group = cruise)) + 
  geom_line(aes(color = cruise), size = 1.5) +
  geom_point(aes(color = cruise), size = 2) + 
  scale_colour_brewer(palette = "Reds") +
  scale_x_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("O2 concentration ", "(", mu, "M", ")"))) +
  ggtitle("O2 concentration across seasons in 2012") +
  theme_asli

p7 <- ggplot(data = df, aes(x = depth, y = NO3, group = cruise)) + 
  geom_line(aes(color = cruise), size = 1.5) +
  geom_point(aes(color = cruise), size = 2) + 
  scale_colour_brewer(palette = "Blues") +
  scale_x_continuous(breaks=c(20, 40, 60, 80, 100, 120, 140, 160, 180, 200)) + 
  theme_classic() + 
  xlab("Depth (m)") + 
  ylab(expression(paste("NO3 concentration ", "(", mu, "M", ")"))) +
  ggtitle("NO3 concentration across seasons in 2012") +
  theme_asli

figure_path <- paste("../FINAL_PROJ/figures/O2_amount_seasons.pdf", sep = "/")
p6
ggsave(figure_path, width = 19, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/NO3_amount_seasons.pdf", sep = "/")
p7
ggsave(figure_path, width = 19, height = 15, units = "cm")

########################################################
# across YEARS, cruise 66, 69, 72, 77
########################################################
















