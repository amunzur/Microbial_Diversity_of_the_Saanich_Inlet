Saanich_Data <- read_csv("~/Desktop/MICB_405/MICB405_proj/Saanich_Data.csv")

df <- Saanich_Data %>% filter(Cruise == 72, Depth == 0.100 | Depth == 0.120 | Depth == 0.200)

# subset data 
idx <- c(5, 6, 7, 9, 10, 12, 14, 23, 25)
df <- df[, idx]
element_list <- c("O2", "PO4", "NO3", "NH4", "NO2", "H2S", "N20", "CH4")
element_list <- lapply(element_list, function(some_name) rep(some_name, 3))

depth <- rep(c("100 m", "120 m", "200 m"), length(elemen_list)) # repeat depth info for new df
# element_list <- as.list(names(df))
# element_list[[1]] <- NULL
# element_list <- lapply(element_list, function(some_name) rep(some_name, 3))

# collapse all values into one 
values <- unlist(df, use.names = FALSE)
values <- values[-c(1, 2, 3)]

df <- data.frame(values = values, 
                 depth = as.factor(depth), 
                 element_list = unlist(element_list))

df[22, 1] <- df[22, 1]/1000
df[23, 1] <- df[23, 1]/1000
df[24, 1] <- df[24, 1]/1000

p1 <- ggplot(data = df, aes(x = element_list, y = values, fill = depth)) + 
  geom_bar(stat = "identity",  position="dodge") + 
  scale_fill_manual(values = wes_palette(9, name = "Cavalcanti1", type = "continuous"), name = "") + 
  geom_text(aes(label=round(values, 0)), vjust=-.5, color="black",
            position = position_dodge(width=1), size=4)+
  theme_classic() + 
  xlab("molecule name") + 
  ylab("amount") + 
  ggtitle(expression(paste("Amount of molecules in water ", "(", mu, "M", ")"))) +
  theme(panel.border = element_blank(), 
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

figure_path <- paste("../FINAL_PROJ/figures/molecule_amount.pdf", sep = "/")
p1
ggsave(figure_path, width = 19, height = 15, units = "cm")

  
  
 