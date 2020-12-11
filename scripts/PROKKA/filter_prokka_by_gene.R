# prokka remove trna 
library(tidyverse)
library(gridExtra)
library(wesanderson)


BAC_PROKKA <- read_tsv("metagenomics_data/PROKKA/BAC_PROKKA.tsv")
idx <- which(BAC_PROKKA$ftype == "CDS")
BAC_PROKKA_subsetted <- BAC_PROKKA[idx, ]

BAC_gene_calls <- read.delim("metagenomics_data/PROKKA/BAC_gene_calls.txt")

# cehck if the lengths match 
BAC_PROKKA_subsetted_L <- BAC_PROKKA_subsetted$length_bp
start_BAC_gene_calls <- BAC_gene_calls$start
stop_BAC_gene_calls <- BAC_gene_calls$stop
BAC_gene_calls_L <- stop_BAC_gene_calls - start_BAC_gene_calls

which(BAC_gene_calls_L != BAC_PROKKA_subsetted_L)

# clean up the depth column 
string_list <- BAC_gene_calls$contig
string_list <- lapply(string_list, function(some_string) str_split(some_string, "_")[[1]][[2]] )
string_list <- unlist(as.numeric(string_list))

# add depth to subsetted df
BAC_PROKKA_subsetted$depth <- string_list
write_tsv(BAC_PROKKA_subsetted, "metagenomics_data/PROKKA/BAC_PROKKA_depth.tsv")

# filter for the nitrogen genes 
idx <- grep("NapA|NifD|NirK|NirS|NorB|NorC|NosZ|NxrA|NxrB|NarG|NrfA|NasA", BAC_PROKKA_subsetted$gene, ignore.case = TRUE) # get idx of genes 

# subset the df based on genes we want 
BAC_PROKKA_subsetted <- BAC_PROKKA_subsetted[idx, ]

# save once more 
write_tsv(BAC_PROKKA_subsetted, "metagenomics_data/PROKKA/BAC_PROKKA_depth_FINAL.tsv")

##################################################################################################
BAC_PROKKA_subsetted <- read_tsv("metagenomics_data/PROKKA/BAC_PROKKA_depth_FINAL.tsv")

# make the plots etc 
# divide into 3 df 
m100 <- filter(BAC_PROKKA_subsetted, depth == 100)
m120 <- filter(BAC_PROKKA_subsetted, depth == 120)
m200 <- filter(BAC_PROKKA_subsetted, depth == 200)

nit_genes <- c("NapA", "NifD", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB", "NrfA", "NasA", "NarG")

gene_nums100 <- lapply(nit_genes, function(gene) length(grep(paste(gene), m100$gene, ignore.case = TRUE)))
gene_nums120 <- lapply(nit_genes, function(gene) length(grep(paste(gene), m120$gene, ignore.case = TRUE)))
gene_nums200 <- lapply(nit_genes, function(gene) length(grep(paste(gene), m200$gene, ignore.case = TRUE)))

# make a new df based on these 
df100 <- data.frame(gene = nit_genes, value = unlist(gene_nums100), depth = rep("100 m", 12))
df120 <- data.frame(gene = nit_genes, value = unlist(gene_nums120), depth = rep("120 m", 12))
df200 <- data.frame(gene = nit_genes, value = unlist(gene_nums200), depth = rep("200 m", 12))

combined <- rbind(df100, df120, df200)

# data_summary(combined, varname="value", 
             # groupnames=c("gene", "depth"))

# combined$sd <- rep(5, 27)
# idx <- which(combined$gene == "NifD" | combined$gene == "NxrA" | combined$gene == "NxrB")
# combined$sd[idx] <- NA

# combined$sd <- c(12, NA,  5,  5,  5,  5,  5, NA, NA,  8, NA,  5,  5,  5,  5,  5, NA, NA,  8, NA,  5,  5,  5,  5,  5, NA, NA)


pl <- ggplot(data = combined, aes(x = gene, y = value, fill = factor(depth))) + 
  geom_bar(stat = "identity",  position="dodge") + 
  scale_fill_manual(values = wes_palette(9, name = "Cavalcanti1", type = "continuous"), name = "") +
  geom_text(aes(label=round(value, 0)), vjust=-.5, color="black",
            position = position_dodge(width=1), size=3)+
  theme_classic() + 
  xlab("genes") + 
  ylab("number of variants") + 
  ggtitle("Nitrogen cycle related genes across depths - PROKKA") +
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

figure_path <- paste("../FINAL_PROJ/figures/prokka_genes.pdf", sep = "/")
pl
ggsave(figure_path, width = 19, height = 15, units = "cm")
