library(tidyverse)
library(wesanderson)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=14, face="bold"),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=14, face="bold"),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=14, face="bold"),
                      axis.title = element_text(size=14, face="bold"),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=25, face="bold"),
                      strip.text.x = element_text(size=15, color="black", face="bold"))

get_data <- function(gene_name, path, depth_name){
  
  abundance_path <- paste(path, 
                          gene_name, 
                          paste0(gene_name, "_abundance_simplebar.txt"), 
                          sep = "/")
  
  labels_path <- paste(path, 
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
  
  # df$species <- factor(df$species,levels = c(as.character(df$species[[1]]),
  #                                             as.character(df$species[[2]]),
  #                                             as.character(df$species[[3]]),
  #                                             as.character(df$species[[4]]),
  #                                             as.character(df$species[[5]])))
  
  # get the class information and add to the current df
  class_df <- as.data.frame(read_csv("metagenomics_data/TreeSAPP/class_abundance.csv"))
  idx <- which(class_df$Gene == gene_name)
  class_df <- class_df[idx, ]
  df$class <- class_df$`class name`
  df$depth <- c(rep(depth_name, dim(df)[1]))
  
  df$class <- factor(df$class,levels = c(as.character(df$class[[1]]),
                                         as.character(df$class[[2]]),
                                         as.character(df$class[[3]]),
                                         as.character(df$class[[4]]),
                                         as.character(df$class[[5]])))
  
  
  # df$class <- unlist(lapply(as.list(df$class), function(some_string) str_split(some_string, "_")[[1]][[1]]))
  
  return(df)
  
}

main_func <- function(gene_name){
  
  m100 <- get_data(gene_name, 
                   "metagenomics_data/13_TreeSAPP_Output/Med_High_Quality_Bins/SI072_100m_Med_Hi_MAGs/iTOL_output", 
                   "100 m")
  
  m120 <- get_data(gene_name, 
                   "metagenomics_data/13_TreeSAPP_Output/Med_High_Quality_Bins/SI072_120m_Med_Hi_MAGs/iTOL_output", 
                   "120 m")
  
  m200 <- get_data(gene_name, 
                   "metagenomics_data/13_TreeSAPP_Output/Med_High_Quality_Bins/SI072_200m_Med_Hi_MAGs/iTOL_output", 
                   "200 m")
  
  df <- rbind(m100, m120, m200)
  df$gene <- gene_name
  df$species_nums <- rep(c(1, 2, 3, 4, 5), 3)
  
  return(df)
}

make_plot <- function(df){
  
  pl <- ggplot(data = df, aes(x = depth, y = class, color = depth)) + 
    geom_point(aes(size = abundance)) +
    scale_color_manual(values = wes_palette(9, name = "Cavalcanti1", type = "continuous"), name = "") +
    theme_classic() + 
    scale_size(range = c(1, 7)) +
    xlab(" ") + 
    ylab(" ") + 
    ggtitle(paste(unique(df$gene))) +
    theme_asli 
  
  return(pl)
}



NapA_df <- main_func("NapA")
NirK_df <- main_func("NirK")
NorB_df <- main_func("NorB")
NosZ_df <- main_func("NosZ")

NapA_plot <- make_plot(NapA_df)
NorB_plot <- make_plot(NorB_df)
NosZ_plot <- make_plot(NosZ_df)
NirK_plot <- make_plot(NirK_df)

fig_height <- 22
fig_width <- 13

figure_path <- paste("../FINAL_PROJ/figures/SUPP/bubble/NapA_plot_class.pdf", sep = "/")
NapA_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/bubble/NorB_plot_class.pdf", sep = "/")
NorB_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/bubble/NosZ_plot_class.pdf", sep = "/")
NosZ_plot
ggsave(figure_path, width = fig_height, height = fig_width, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/bubble/NirK_plot_class.pdf", sep = "/")
NirK_plot
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



