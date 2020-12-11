library(tidyverse)
library(wesanderson)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=20, face="bold"),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=20, face="bold"),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=20, face="bold"),
                      axis.title = element_text(size=20, face="bold"),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=25, face="bold"),
                      strip.text.x = element_text(size=15, color="black",
                                                  face="bold"))

get_data <- function(gene_name){
  
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
  # top_genes <- as.vector(head(abundance_df_new$gene, top)) # get the name of top 5 genes 
  
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
  
  df$species <- factor(df$species,levels = c(as.character(df$species[[1]]), 
                                             as.character(df$species[[2]]), 
                                             as.character(df$species[[3]]), 
                                             as.character(df$species[[4]]), 
                                             as.character(df$species[[5]])))
  
  # get the class information and add to the current df
  class_df <- as.data.frame(read_csv("metagenomics_data/TreeSAPP/class_abundance.csv"))
  idx <- which(class_df$Gene == gene_name)
  class_df <- class_df[idx, ]
  df$class <- class_df$`class name`
  
  # some modifications to convert the axes to numeric, otherwise ggplot complains
  df$numeric_label <- c(1, 2, 3, 4, 5)
  
  # and now make a plot 
  pl <- ggplot(data = df, aes(x = numeric_label, y = abundance, fill = species)) + 
    geom_bar(stat = "identity") + 
    scale_fill_manual(values = wes_palette(5, name = "Royal2", type = "continuous"), name = "") +
    geom_text(aes(label=round(abundance, 0)), vjust=0.3, hjust = 1, color="black",
              position = position_dodge(width=1), size=6)+
    scale_x_continuous(breaks = c(1, 2, 3, 4, 5), labels = df$species, 
                       sec.axis = dup_axis(labels = df$class)) + 
    theme_classic() + 
    coord_flip()+
    xlab(" ") + 
    ylab(" ") + 
    ggtitle(paste(gene_name)) +
    theme_asli + 
    theme(legend.position = "none") 
  
  return(pl)
  
}


NapA_plot <- get_data("NapA")
NifD_plot <- get_data("NifD")
NirK_plot <- get_data("NirK")
NirS_plot <- get_data("NirS")
NorB_plot <- get_data("NorB")
NorC_plot <- get_data("NorC")
NosZ_plot <- get_data("NosZ")
NxrA_plot <- get_data("NxrA")
NxrB_plot <- get_data("NxrB")

fig_height <- 45
fig_width <- 16

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NapA_plot.pdf", sep = "/")
NapA_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NifD_plot.pdf", sep = "/")
NifD_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NirK_plot.pdf", sep = "/")
NirK_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NirS_plot.pdf", sep = "/")
NirS_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NorB_plot.pdf", sep = "/")
NorB_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NorC_plot.pdf", sep = "/")
NorC_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NosZ_plot.pdf", sep = "/")
NosZ_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NxrA_plot.pdf", sep = "/")
NxrA_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/combined/NxrB_plot.pdf", sep = "/")
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



