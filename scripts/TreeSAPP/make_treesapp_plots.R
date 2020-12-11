# working with PROKKA
library(tidyverse)
library(gridExtra)

BAC_gene_annot <- read.delim("metagenomics_data/PROKKA/BAC_gene_annot.txt")
BAC_gene_calls <- read.delim("metagenomics_data/PROKKA/BAC_gene_calls.txt")
ts_100 <- read_tsv("metagenomics_data/13_TreeSAPP_Output/Med_High_Quality_Bins/SI072_100m_Med_Hi_MAGs/marker_contig_map.tsv")




# get how many times they appeared in treesap outputs 
nit_genes <- list("NapA", "NifD", "NifH", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB")

# NifH complexes with NifD
gene_numbers_treesapp <- lapply(nit_genes, function(some_gene) length(grep(some_gene, ts_100$Marker)))
gene_numbers_prokka <- lapply(nit_genes, function(some_gene) length(grep(some_gene, BAC_gene_annot$accession, ignore.case = TRUE)))

# make dfs with these, then plot
data_treesapp <- data.frame(
  name=c("NapA", "NifD", "NifH", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB") ,  
  value=c(unlist(gene_numbers_treesapp)))

data_prokka <- data.frame(
  name=c("NapA", "NifD", "NifH", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB") ,  
  value=c(unlist(gene_numbers_prokka)))

pl_prokka <- ggplot(data_prokka, aes(x = name, y = value)) + 
  geom_bar(stat = "identity") + 
  ylim(0, 500) + 
  ggtitle("prokka") 

pl_treesapp <- ggplot(data_treesapp, aes(x = name, y = value)) + 
  geom_bar(stat = "identity") + 
  ylim(0, 500) + 
  ggtitle("treesapp")

gridExtra::grid.arrange(pl_treesapp, pl_prokka, nrow = 1)

# now plot how many genes treesapp found a taxonomy for 
# order by gene name 
ts_100 <- ts_100[order(ts_100$Marker),]
idx_list <- lapply(nit_genes, function(some_gene) which(ts_100$Marker == some_gene))
dflist <- lapply(idx_list, function(idx) ts_100[idx, ]) # list of DFs that have genes separated 

some_function <- function(dflist){
  
  idx_list <- list()
  for (df in dflist) {
    
    cats <- unique(df$Taxonomy)
    idx <- lapply(cats, function(cat) length(which(df$Taxonomy == cat)))
    names(idx) <- cats
    idx <- list(idx) # name each index with taxa info
    # name(idx) <- unique(df$Marker) # name with the taxa info 
    
    idx_list <- append(idx, idx_list)
    
  }
  
    return(idx_list)
}


idx_list <- some_function(dflist)

a <- names(idx_list[[1]])
b <- names(idx_list[[2]])
c <- names(idx_list[[3]])
d <- names(idx_list[[4]])
e <- names(idx_list[[5]])
f <- names(idx_list[[6]])
g <- names(idx_list[[7]])
h <- names(idx_list[[8]])
i <- names(idx_list[[9]])
j <- names(idx_list[[10]])

numbers <- c(28, 1, 3, 1, 1, 1, 1, 1, 1, 4, 2, 2, 6, 3, 1, 2, 3, 2, 5, 1, 34, 3, 2, 8, 3, 9, 48, 2, 2, 2, 1)
taxa <- c(a, b, c, d, e, f, g, h, i, j)

# get how many times each gene appeared 
to_repeat <- c(4, 5, 3, 2, 6, 3, 1, 2, 5)

x <- lapply(dflist, function(df) dim(df)[2])

nit_genes <- c("NapA", "NifD", "NirK", "NirS", "NorB", "NorC", "NosZ", "NxrA", "NxrB")
genes <- mapply(rep, nit_genes, to_repeat)

df <- as.data.frame(cbind(unlist(genes), numbers, taxa))

pl <- ggplot(df, aes(x = taxa, y = numbers, color = unlist(genes))) +
  geom_bar()



