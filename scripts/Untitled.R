# CAPSTONE 1 
library(tidyverse)

arc_path <- "data/gtdbtk.ar122.classification_pplacer.tsv"
bac_path <- "data/gtdbtk.bac120.classification_pplacer.tsv"


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


# plots for question 3 
# phylum 
pl_phylum <- ggplot(arc_bac_combined, aes(x = Phylum, color = Phylum, fill = Phylum)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("CAMI output phylum counts")

pl_family <- ggplot(arc_bac_combined, aes(x = Family, color = Family, fill = Family)) + 
  geom_histogram(stat = "count") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90)) + 
  ggtitle("CAMI output Family counts")













# tutorial 
library(tidyverse)

tutorial_markers <- read_tsv("marker_contig_map.tsv") %>% 
  select(Sample, Query, Taxonomy, EvoDist) %>% 
  distinct(Taxonomy, .keep_all = TRUE)

# check if all taxa are unique 
dim(tutorial_markers)
length(unique(tutorial_markers$Taxonomy))





