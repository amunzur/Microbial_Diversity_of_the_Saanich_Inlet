library(tidyverse)

cc_main <- read_tsv("metagenomics_data/08_Refined_From_Anvio/SI072_Bins_Final_Bins/SI072_Bins_Ref_5_CheckM.tsv") # CC info 
taxa_info_ARC_main <- read_tsv("metagenomics_data/10_SI072_Bins_GTDB_r95/gtdbtk.ar122.summary.tsv")
taxa_info_BAC_main <- read_tsv("metagenomics_data/10_SI072_Bins_GTDB_r95/gtdbtk.bac120.summary.tsv")

# some renaming
names(cc_main)[1] <- "bin"
names(taxa_info_ARC_main)[1] <- "bin"
names(taxa_info_BAC_main)[1] <- "bin"

# select the cols we need 
cc <- cc_main %>% 
  select(bin, Completeness, Contamination)

BAC <- taxa_info_BAC_main %>% 
  select(bin, classification)

ARC <- taxa_info_ARC_main %>% 
  select(bin, classification)

# separate the taxa
ranks <- c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
arc_bac_combined <- rbind(ARC, BAC) %>% 
  separate(classification, into = ranks, sep = ";")

# now an inner join between cc data and taxa data, to keep the common bins 
mags_combined <- inner_join(arc_bac_combined, cc)

# keep only high to medium metabat binning data 
# load the info with the names of bins we will keep 
bins_keep_DF <- read_csv("metagenomics_data/09_Bins_Sorted_By_Quality/Med_HQ_Bins/Med_and_High_Quality_Bins.csv", col_names = FALSE)
bins_keep <- as.vector(bins_keep_DF$X1)

# get the metabat bins only 
idx <- grep("METABAT", bins_keep)
bins_keep <- bins_keep[idx] # subset 
bins_keep <- unlist(lapply(bins_keep, function(some_string) str_split(some_string, "[.]")[[1]][[1]])) # remove the ".fa" part for grep later on 

idx <- which(mags_combined$bin %in%  bins_keep) 
mags_combined <- mags_combined[idx, ] # only retain our chosen bins 

pl_Phylum <- ggplot(data = mags_combined, aes(x = Completeness, y = Contamination, color = Phylum)) + 
  geom_point() + 
  theme_minimal() + 
  xlim(0, 100) + 
  ylim(0, 100)

pl_Family <- ggplot(data = mags_combined, aes(x = Completeness, y = Contamination, color = Family)) + 
  geom_point() + 
  theme_minimal() + 
  xlim(0, 100) + 
  ylim(0, 100)


