# CAPSTONE 1 
library(tidyverse)

arc_path <- "metagenomics_data/CAMI_HIGH_BINS/gtdbtk.ar122.summary.tsv"
bac_path <- "metagenomics_data/CAMI_HIGH_BINS/gtdbtk.bac120.summary.tsv"
genome_taxa_path <- "metagenomics_data/CAMI_HIGH_BINS/genome_taxa_info.tsv"
metawrap_path <- "metagenomics_data/CAMI_HIGH_BINS/metawrap_70_10_bins.stats.tsv"

genome_taxa <- read_tsv(genome_taxa_path)

# read data, only pick the cols we need
metawrap <- read_tsv(metawrap_path) %>% 
  select(bin, completeness, contamination)
names(metawrap) <- c("mag", "completeness", "contamination")

arc <- read_tsv(arc_path) %>% 
  select(user_genome, classification)

bac <- read_tsv(bac_path) %>% 
  select(user_genome, classification)

# some renaming for the cols
names(arc) <- c("mag", "taxonomy")
names(bac) <- c("mag", "taxonomy")

# separate the cols to make our life easier 
ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
arc_bac_combined <- rbind(arc, bac) %>% 
  separate(taxonomy, into = ranks, sep = ";") %>% #separate into cols
  select(mag:Family) # select cols until family 

# now an inner join between contamination data and arc_bac data 
mags_combined <- inner_join(arc_bac_combined, metawrap)

# now make the plot 
p_family <- ggplot(data = mags_combined, aes(x = completeness, y = contamination, color = Family)) + 
  geom_point() + 
  ggtitle("Family") + 
  theme_minimal()

p_phylum <- ggplot(mags_combined, aes(x = completeness, y = contamination, color = Phylum)) + 
  geom_point() + 
  ggtitle("Phylum") + 
  theme_minimal()

# plots for question 3 
# phylum 
pl_phylum <- ggplot(mags_combined, aes(x = Phylum, color = Phylum, fill = Phylum)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("GTDB output phylum counts")

pl_family <- ggplot(mags_combined, aes(x = Family, color = Family, fill = Family)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("GTDB output Family counts")













# tutorial 
library(tidyverse)

tutorial_markers <- read_tsv("marker_contig_map.tsv") %>% 
  select(Sample, Query, Taxonomy, EvoDist) %>% 
  distinct(Taxonomy, .keep_all = TRUE)

# check if all taxa are unique 
dim(tutorial_markers)
length(unique(tutorial_markers$Taxonomy))




  
