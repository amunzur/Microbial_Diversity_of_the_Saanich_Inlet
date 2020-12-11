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
# load treesapp results for each depth 

df <- as.data.frame(read_tsv("metagenomics_data/13_TreeSAPP_Output/Med_High_Quality_Bins/SI072_100m_Med_Hi_MAGs/marker_contig_map.tsv"))

# get depth information 
depth <- as.numeric(lapply(as.list(m100$Query), function(some_string) strsplit(some_string, "_")[[1]][[2]]))
df$depth <- depth

# filter the df based on depth
m100 <- filter(df, depth == 100)
m120 <- filter(df, depth == 120)
m200 <- filter(df, depth == 200)

nit_genes <- c("NapA", "NifD", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB", "NrfA", "NasA", "NarG")

gene_nums100 <- lapply(nit_genes, function(gene) length(grep(paste(gene), m100$Marker, ignore.case = TRUE)))
gene_nums120 <- lapply(nit_genes, function(gene) length(grep(paste(gene), m120$Marker, ignore.case = TRUE)))
gene_nums200 <- lapply(nit_genes, function(gene) length(grep(paste(gene), m200$Marker, ignore.case = TRUE)))

# make a new df based on these 
df100 <- data.frame(gene = nit_genes, number_of_variant = unlist(gene_nums100), depth = rep("100 m", 12))
df120 <- data.frame(gene = nit_genes, number_of_variant = unlist(gene_nums120), depth = rep("120 m", 12))
df200 <- data.frame(gene = nit_genes, number_of_variant = unlist(gene_nums200), depth = rep("200 m", 12))

combined <- rbind(df100, df120, df200)

# make a plot 
pl <- ggplot(data = combined, aes(x = gene, y = number_of_variant, fill = factor(depth))) + 
  geom_bar(stat = "identity",  position="dodge") + 
  scale_fill_manual(values = wes_palette(9, name = "Cavalcanti1", type = "continuous"), name = "") +
  geom_text(aes(label=round(number_of_variant, 0)), vjust=-.5, color="black",
            position = position_dodge(width=1), size=3)+
  theme_classic() + 
  xlab("genes") + 
  ylab("number of variants") + 
  ggtitle("Nitrogen cycle related genes across depths - TreeSAPP") +
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
        plot.title = element_text(color="black", size=14),
        strip.text.x = element_text(size=10, color="black",
                                    face="bold"))

figure_path <- paste("../FINAL_PROJ/figures/treesapp_genes.pdf", sep = "/")
pl
ggsave(figure_path, width = 19, height = 15, units = "cm")








