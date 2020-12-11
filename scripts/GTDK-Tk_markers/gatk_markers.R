library(tidyverse)

arc_markers <- read_tsv("metagenomics_data/10_SI072_Bins_GTDB_r95/gtdbtk.ar122.markers_summary.tsv")
bac_markers <- read_tsv("metagenomics_data/10_SI072_Bins_GTDB_r95/gtdbtk.bac120.markers_summary.tsv")

idx <- grep("METABAT", bac_markers$name, ignore.case = TRUE) # grep for metabat bins

# subset
arc_markers <- arc_markers[idx, ]
bac_markers <- bac_markers[idx, ]

# rename 
arc_markers <- arc_markers %>% dplyr::rename_all(paste0, "ARC") %>% 
  select(1:5)
bac_markers <- bac_markers %>% dplyr::rename_all(paste0, "BAC") %>% 
  select(2:5)

# combine
combined <- cbind(arc_markers, bac_markers)
