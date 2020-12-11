library(tidyverse)
library(wesanderson)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=10),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=10, face="bold"),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=10, face="bold"),
                      axis.title = element_text(size=10),
                      legend.title = element_text(color = "black", size = 12),
                      legend.text = element_text(color = "black", size = 12),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=14, face="bold"),
                      strip.text.x = element_text(size=10, color="black",
                                                  face="bold"))

main_df <- as.data.frame(read_csv("metagenomics_data/TreeSAPP/class_abundance.csv"))
# main_df <- main_df[-c(7), ] # drop one row
names(main_df) <- c("gene", "class", "rank")

make_class_plot <- function(gene_name, main_df){
  
  abundance_path <- paste("metagenomics_data/13_TreeSAPP_Output/Med_High_Quality_Bins/SI072_100m_Med_Hi_MAGs/iTOL_output", 
                          gene_name, 
                          paste0(gene_name, "_abundance_simplebar.txt"), 
                          sep = "/")
  
  labels_path <- paste("metagenomics_data/13_TreeSAPP_Output/Med_High_Quality_Bins/SI072_100m_Med_Hi_MAGs/iTOL_output", 
                       gene_name, 
                       paste0(gene_name, "_labels.txt"), 
                       sep = "/")
  
  abundance_df <- read_delim(abundance_path, delim = "|")
  labels <- read_delim(labels_path, delim = "|")
  
  # ABUNDANCE
  # modify these dfs 
  abundance <- as.list(abundance_df$DATASET_SIMPLEBAR)
  abundance <- as.numeric(unlist(lapply(abundance, function(some_value) strsplit(some_value, ",")[[1]][2]))) 
  abundance <- abundance[-c(1:4)]
  
  # species numbers here
  gene <- as.list(abundance_df$DATASET_SIMPLEBAR)
  gene <- unlist(lapply(gene, function(some_value) strsplit(some_value, ",")[[1]][1]))
  gene <- gene[-c(1:4)]
  
  abundance_df_new <- as.data.frame(cbind(gene, abundance))
  abundance_df_new <- abundance_df_new[order(-abundance),] 
  top3 <- as.vector(head(abundance_df_new$gene, 3)) # get the name of top 3 genes 
  
  # LABELS 
  # modify these dfs 
  gene <- as.list(labels$LABELS)
  gene <- unlist(lapply(gene, function(some_value) strsplit(some_value, ",")[[1]][1]))
  gene <- gene[-c(1:3)]
  
  # species numbers here
  species <- as.list(labels$LABELS)
  species <- unlist(lapply(species, function(some_value) strsplit(some_value, ",")[[1]][2]))
  species <- species[-c(1:3)]
  
  labels_df_new <- as.data.frame(cbind(gene, species))
  
  # reorder based on info from abundance df 
  idx <- match(abundance_df_new$gene, labels_df_new$gene)
  labels_df_new <- labels_df_new[idx, ]
  
  # combine the two df and pick top 3 genes, make a new df
  labels_df_new$abundance <- abundance_df_new$abundance
  df <- labels_df_new[1:5, ]
  df$abundance <- round(as.numeric(as.character(df$abundance)), 0)
  
  # find the related gene in the main_df
  idx <- which(main_df$gene == gene_name)
  class <- main_df$class[idx]
  df$class <- class
  
  df$class <- factor(df$class,levels = c(as.character(df$class[[1]]), 
                                             as.character(df$class[[2]]), 
                                             as.character(df$class[[3]]), 
                                             as.character(df$class[[4]]), 
                                             as.character(df$class[[5]])))
  
  
  # and now make a plot 
  pl <- ggplot(data = df, aes(x = class, y = abundance, fill = class)) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = wes_palette(5, name = "Royal2", type = "continuous"), name = "") +
    geom_text(aes(label=round(abundance, 0)), vjust=0.3, hjust = 1, color="black",
              position = position_dodge(width=1), size=3)+
    theme_classic() + 
    coord_flip()+
    xlab(" ") + 
    ylab(" ") + 
    ggtitle(paste(gene_name)) +
    theme_asli + 
    theme(legend.position = "none") 
  
  return(pl)
  
}

NapA_plot <- make_class_plot("NapA", main_df)
NifD_plot <- make_class_plot("NifD", main_df)
NirK_plot <- make_class_plot("NirK", main_df)
NirS_plot <- make_class_plot("NirS", main_df)
NorB_plot <- make_class_plot("NorB", main_df)
NorC_plot <- make_class_plot("NorC", main_df)
NosZ_plot <- make_class_plot("NosZ", main_df)
NxrA_plot <- make_class_plot("NxrA", main_df)
NxrB_plot <- make_class_plot("NxrB", main_df)

fig_height <- 15
fig_width <- 6

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NapA_plot.pdf", sep = "/")
NapA_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NifD_plot.pdf", sep = "/")
NifD_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NirK_plot.pdf", sep = "/")
NirK_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NirS_plot.pdf", sep = "/")
NirS_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NorB_plot.pdf", sep = "/")
NorB_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NorC_plot.pdf", sep = "/")
NorC_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NosZ_plot.pdf", sep = "/")
NosZ_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NxrA_plot.pdf", sep = "/")
NxrA_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/class/NxrB_plot.pdf", sep = "/")
NxrB_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")
 








gridExtra::grid.arrange(NapA_plot, 
                        NifD_plot, 
                        NirK_plot, 
                        NirS_plot, 
                        NorB_plot, 
                        NorC_plot, 
                        NosZ_plot, 
                        NxrA_plot, 
                        NxrB_plot, nrow = 3)

gridExtra::grid.arrange(NapA_plot, 
                        NirK_plot, 
                        NorB_plot, 
                        NosZ_plot, nrow = 2)




