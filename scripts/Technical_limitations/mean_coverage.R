library(tidyverse)
library(wesanderson)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=13),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=13),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=13),
                      axis.title = element_text(size=13, face="bold"),
                      legend.title = element_text(color = "black", size = 12),
                      legend.text = element_text(color = "black", size = 12),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=14, face="bold"),
                      strip.text.x = element_text(size=13, color="black", face="bold"))


# get paths of all coverage files for all bins
paths <- list.files("metagenomics_data/06_Ready_For_Refining/SI072_MERGED_SUMMARY/dastool_out/bin_by_bin", 
                    recursive = T, 
                    full.names = T,
                    pattern = "-mean_coverage.txt")

# get bin names
names <- lapply(as.list(paths), function(some_string) strsplit(some_string, "/")[[1]][[6]])
names <- unlist(lapply(names, function(some_string) strsplit(some_string, "__")[[1]][[2]]))

df_list <- list()
for (path in paths){
  
  dat <- as.data.frame(read.delim(path))
  df_list <- append(df_list, list(dat))
  
}

# combine all dfs
combined <- do.call(rbind, df_list)
combined$bin <- names
combined$bin[3] <- "11"
combined$bin[12] <- "27"
combined$bin[13] <- "3"

names(combined) <- c("bin", "100 m", "120 m", "200 m")

# some levelling
combined$bin <- factor(c("106", "109", "11", "136", "137", "142", "165", "183", "187", "21", "23", "27",  "3", "41", "48", "80", "99"),
                       levels = c("80", "3", "21", "23", "106", "109", "11", "136", "137", "142", "165", "183", "187", "27", "41", "48", "99"))

# gather the table in the "LONG" format for plotting
combined <- combined %>% 
  gather(key = "depth", value = "coverage", -bin)

# round the coverage col 
combined$coverage <- round(combined$coverage, 2)

# PLOTTING! 
pl <- ggplot(data = combined, aes(x = bin, y = coverage, fill = depth)) + 
  geom_bar(stat = "identity", position = position_dodge(width=0.5)) + 
  scale_fill_manual(values = wes_palette(9, name = "Cavalcanti1", type = "continuous"), name = "") + 
  theme_classic() + 
  xlab("Medium and high quality MAGs") + 
  ylab("coverage") + 
  ylim(0, 100) +
  theme_asli
pl

figure_path <- paste("../FINAL_PROJ/figures/coverage_depth.pdf", sep = "/")
pl
ggsave(figure_path, width = 20, height = 10, units = "cm")





