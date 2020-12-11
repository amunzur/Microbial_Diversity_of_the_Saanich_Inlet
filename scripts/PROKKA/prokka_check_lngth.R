# working with PROKKA
library(tidyverse)

BAC_gene_annot <- read.delim("metagenomics_data/PROKKA/BAC_gene_annot.txt")
BAC_gene_calls <- read.delim("metagenomics_data/PROKKA/BAC_gene_calls.txt")
start_bp <- BAC_gene_calls$start
stop_bp <- BAC_gene_calls$stop
length_BACgenecalls <- stop_bp - start_bp

BAC_PROKKA <- read_tsv("metagenomics_data/PROKKA/BAC_PROKKA.tsv")
length_BACPROKKA <- BAC_PROKKA$length_bp

length_BACgenecalls == length_BACPROKKA



BAC_genes <- (BAC_PROKKA$gene)



# only keep the genes we are interested in 
nit_genes <- c("NapA", "NifD", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB")
idx <- grep("NapA|NifD|NirK|NirS|NorB|NorC|NosZ|NxrA|NxrB", BAC_PROKKA$gene, ignore.case = TRUE) # get idx of genes 
BAC_PROKKA <- BAC_PROKKA[idx, ] # subset the df to these genes only 
unique_lengths <- unique(BAC_PROKKA$length_bp) # get the unique lengths 
idx <- match(unique_lengths, BAC_PROKKA$length_bp) # get the idx of unique lengths 

# subset the df based on unique lengths 
subsetted <- BAC_PROKKA[idx, ]
L <- subsetted$length_bp # extract lenghts

# match this with the contig ids
which(L == length_BACgenecalls)
L %in% length_BACgenecalls

idx <- which(L == length_BACgenecalls)
