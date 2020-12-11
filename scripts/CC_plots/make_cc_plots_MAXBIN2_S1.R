library(tidyverse)
library(ggrepel)
library(RColorBrewer)

theme_asli <-   theme(panel.border = element_blank(), 
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(), 
                      axis.line = element_line(colour = "black", size = 1), 
                      axis.ticks = element_line(colour = "black", size = 2),
                      axis.text = element_text(size=15),
                      axis.text.x = element_text(vjust=0.5, colour = "black", size=15),
                      axis.text.y = element_text(vjust=0.5, colour = "black", size=15),
                      axis.title = element_text(size=15),
                      legend.title = element_text(color = "black", size = 12),
                      legend.text = element_text(color = "black", size = 12),
                      axis.ticks.length=unit(0.15, "cm"), 
                      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 20)), 
                      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
                      plot.title = element_text(color="black", size=20, face="bold"),
                      strip.text.x = element_text(size=15, color="black", face="bold"))


# load the main cc data
cc_main <- read_tsv("metagenomics_data/08_Refined_From_Anvio/SI072_Bins_Final_Bins/SI072_Bins_Ref_5_CheckM.tsv")
names(cc_main)[1] <- "bin"

# load the taxa information 
cc_taxa_arc <- read_tsv("metagenomics_data/10_SI072_Bins_GTDB_r95/gtdbtk.ar122.summary.tsv") %>% 
  select(user_genome, classification)
names(cc_taxa_arc) <- c("bin", "taxa")

cc_taxa_bac <- read_tsv("metagenomics_data/10_SI072_Bins_GTDB_r95/gtdbtk.bac120.summary.tsv") %>% 
  select(user_genome, classification)
names(cc_taxa_bac) <- c("bin", "taxa")

# filter both to keep metabat only 
idx <- grep("MAXBIN", cc_main$bin) # find bins with the metabat 
cc_metabat <- cc_main[idx, ] # subset 

idx <- grep("MAXBIN", cc_taxa_arc$bin)
cc_taxa_arc <- cc_taxa_arc[idx, ]

idx <- grep("MAXBIN", cc_taxa_bac$bin)
cc_taxa_bac <- cc_taxa_bac[idx, ]

# separate out the taxa information
ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
arc_bac_combined <- rbind(cc_taxa_bac, cc_taxa_arc) %>% 
  separate(taxa, into = ranks, sep = ";")

# now an inner join between cc data and taxa data, to keep the common bins 
mags_combined <- inner_join(arc_bac_combined, cc)

# clean up the names of phyla and order
p_names <- as.list(mags_combined$Phylum)
p_names <- unlist(lapply(p_names, function(some_string) strsplit(some_string, "__")[[1]][[2]]))
mags_combined$Phylum <- p_names

o_names <- as.list(mags_combined$Order)
o_names <- unlist(lapply(o_names, function(some_string) strsplit(some_string, "__")[[1]][[2]]))
mags_combined$Order <- o_names

# make dfs here to add the rectangles
d <- data.frame(x = c(90, 100), 
                y = c(0, 5))

d_high_quality <- data.frame(xmin=90, xmax=Inf, ymin=-Inf, ymax=5)
d_med_quality1 <- data.frame(xmin=50, xmax=90, ymin=-Inf, ymax=10)
d_med_quality2 <- data.frame(xmin=90, xmax=Inf, ymin=5, ymax=10) 
d_low_quality1 <- data.frame(xmin=-Inf, xmax=50, ymin=-Inf, ymax=Inf) 
d_low_quality2 <- data.frame(xmin=50, xmax=Inf, ymin=10, ymax=Inf) 


p1 <- ggplot(data = mags_combined, aes(x = Completeness, y = Contamination, color = Phylum)) + 
  geom_rect(data=d_high_quality, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) + 
  geom_rect(data=d_med_quality1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow", alpha=0.1, inherit.aes = FALSE) + 
  geom_rect(data=d_med_quality2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow", alpha=0.1, inherit.aes = FALSE) + 
  geom_rect(data=d_low_quality1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) + 
  geom_rect(data=d_low_quality2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
  geom_point(size = 4) + 
  geom_point(shape = 1,size = 4,colour = "black") + 
  geom_text_repel(label = p_names, color = "black") +
  scale_color_brewer(palette = "Set2") +
  geom_vline(xintercept=c(50,90), linetype="dotted") + 
  geom_hline(yintercept=c(5, 10), linetype="dotted") + 
  xlab("Completeness (%)") + 
  ylab("Contamination (%)") + 
  theme_minimal() + 
  theme_asli +
  ggtitle("MAXBIN2 completion - contamination plot showing phylum") +
  xlim(0, 100) + 
  ylim(0, 10) 

p2 <- ggplot(data = mags_combined, aes(x = Completeness, y = Contamination, color = Order)) + 
  geom_rect(data=d_high_quality, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="green", alpha=0.2, inherit.aes = FALSE) + 
  geom_rect(data=d_med_quality1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow", alpha=0.1, inherit.aes = FALSE) + 
  geom_rect(data=d_med_quality2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow", alpha=0.1, inherit.aes = FALSE) + 
  geom_rect(data=d_low_quality1, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) + 
  geom_rect(data=d_low_quality2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.1, inherit.aes = FALSE) +
  geom_point(size = 4) + 
  geom_point(shape = 1,size = 4,colour = "black") + 
  geom_text_repel(label = o_names, color = "black") +
  scale_color_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(length(unique(mags_combined$Order)))) +
  geom_vline(xintercept=c(50,90), linetype="dotted") + 
  geom_hline(yintercept=c(5, 10), linetype="dotted") + 
  xlab("Completeness (%)") + 
  ylab("Contamination (%)") + 
  theme_minimal() + 
  theme_asli +
  ggtitle("MAXBIN2 completion - contamination plot showing order") +
  xlim(0, 100) + 
  ylim(0, 10)

figure_path <- paste("../FINAL_PROJ/figures/SUPP/MAXBIN2_cc_phylum.pdf", sep = "/")
p1
ggsave(figure_path, width = 25, height = 15, units = "cm")

figure_path <- paste("../FINAL_PROJ/figures/SUPP/MAXBIN2_cc_order.pdf", sep = "/")
p2
ggsave(figure_path, width = 25, height = 15, units = "cm")





