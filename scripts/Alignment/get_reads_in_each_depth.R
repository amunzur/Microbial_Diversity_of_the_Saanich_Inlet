library(tidyverse)
library(wesanderson)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=10),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=10),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=10),
                      axis.title = element_text(size=10, face="bold"),
                      legend.title = element_text(color = "black", size = 12),
                      legend.text = element_text(color = "black", size = 12),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=14, face="bold"),
                      strip.text.x = element_text(size=10, color="black", face="bold"))

df <- as.data.frame(read_csv("metagenomics_data/misc/read_numbers_track.csv"))
df$Depth <- as.factor(df$Depth)
df[6, 3] <- 14024

a <- lapply(as.list(df$Mag), function(some_string) strsplit(some_string, "__")[[1]][[2]])
a <- lapply(a, function(some_string) strsplit(some_string, "-")[[1]][[1]])
df$Mag <- unlist(a)
df$Mag[c(17, 41, 65)] <- c("27_sub_1_1_2_2", "27_sub_1_1_2_2", "27_sub_1_1_2_2")

Mag_list
# 
# df$Mag <- factor(c("106_1", "106_2", "109", "11_sub", "136_2", "136_3", "137", "142_1", "142_2", "165_1", "183", "187", 
#                  "21", "23_1", "23_2", "27_sub_1_1_2_1", "27_sub_1_1_2_2", "3_sub", "41", "48", "80", "99_1", "99_2", "99_3"), 
#                  levels = c("80", "3_sub", "23_1", "21", "99_1", "99_2", "27_sub_1_1_2_2", "23_2", "137", "136_3", "136_2", "11_sub", 
#                             "99_3", "48", "27_sub_1_1_2_1", "187", "183", "165_1", "142_2", "142_1", "109", "106_2", "106_1"))

pl <- ggplot(data = df, aes(x = Mag, y = reads, fill = Depth)) + 
  geom_bar(stat = "identity") + 
  scale_fill_brewer(palette="Reds") +
  theme_classic() + 
  coord_flip()+
  xlab("MAGS") + 
  ylab("number of reads") + 
  ggtitle("Number of reads in MAGs from each depth") +
  theme_asli + 
  theme(axis.text.y = element_text(colour = c('black', 'black', 'black', 
                                              'chartreuse3', 'chartreuse3', 'chartreuse3', 'chartreuse3', 
                                              'black', 'black', 'black', 'black', 'black',
                                              'blue', 'blue', 
                                              'chartreuse3', 
                                              'black', 
                                              'chartreuse3', 
                                              'blue', 
                                              'chartreuse3', 
                                              'black', 
                                              'blue', 
                                              'chartreuse3', 
                                              'chartreuse3', 
                                              'black'))) 
