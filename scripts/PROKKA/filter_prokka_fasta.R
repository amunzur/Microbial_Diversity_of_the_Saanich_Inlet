library(tidyverse)
library(seqinr)

# load the df where we will pull the locus tags from 
BAC_PROKKA <- read_tsv("metagenomics_data/PROKKA/BAC_PROKKA.tsv")

# grep the gene names we are interested in
nit_genes <- c("NapA", "NifD", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB")

idx <- grep("NapA|NifD|NirK|NirS|NorB|NorC|NosZ|NxrA|NxrB", BAC_PROKKA$gene, ignore.case = TRUE) # get idx of genes 

# get the locus tags of the idx
tags <- BAC_PROKKA$locus_tag[idx]

# read the prokka fasta file 
fa <- read.fasta("metagenomics_data/PROKKA/BAC_PROKKA.faa")
fa_subset <- fa[tags] # subset to gene of interest 

# save the fasta 
write.fasta(sequences = fa_subset, 
            names = names(fa_subset), 
            file.out = "metagenomics_data/PROKKA/BAC_PROKKA_subsetted.faa")

