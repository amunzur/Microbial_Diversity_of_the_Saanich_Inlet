# Genome-Resolved Metagenomics of Nitrogen Cycling in Saanich Inlet

This repository contains the code to replicate the findings in our publication titled:  
**"Genome-resolved metagenomics of nitrogen cycling processes in Saanich Inlet."**

Access the peer-reviewed publication here: https://ojs.library.ubc.ca/index.php/UJEMI/issue/view/183015

## Summary

Denitrification genes were found to be more abundant than nitrification genes in the anoxic environment of Saanich Inlet. Using metagenomic analysis, we quantitatively assessed the abundance of nitrogen cycling genes and discovered a much higher presence of nitrogen-reducing genes. The most abundant were **napA** and **narG**, predominantly found at suboxic depths between 100m and 200m. Taxonomic analysis showed these genes are primarily encoded by members of the **Gammaproteobacteria** class.

Together, these findings support a global trend where microbial communities shift from oxygen to nitrogen as the terminal electron acceptor, likely in response to climate change. This shift may further exacerbate climate change through the production of nitrous oxide, a potent greenhouse gas. Reporting on these microbial shifts is essential for understanding how expanding oxygen minimum zones will affect coastal marine ecosystems.

## Repository Overview

This repository includes all R scripts used in our manuscript.

### Folder Structure & Script Descriptions

#### `CC_plots/`
- **MaxBin2 and MetaBAT MAG Plots (Figure S1)**  
  Contains scripts to generate completion-contamination plots for MAGs:
  - `completion_contamination_plot_maxbin.R`
  - `completion_contamination_plot_metabat.R`

#### `Geochemical-water_conditions/`
- **Geochemical Figures (Figure 3, Figure 4A, Figure S2, Figure S4)**  
  Scripts to visualize geochemical data across depth and seasons.

#### `PROKKA/`
- **Gene Annotation Checks and Filtering (Figure 4B)**  
  - `prokka_check_length.R`: Confirms gene lengths match between PROKKA outputs.  
  - `filter_prokka_by_gene.R`: Filters output to retain only genes of interest and generate Figure 4B.

#### `TreeSAPP/`
- **Taxonomic Visualization of Nitrogen Cycling Genes (Figures 4C, 5B, S3)**  
  - `bubble_plot_class.R`: Generates Figure 5B.  
  - `bubble_plot.R`: Creates comprehensive bubble plots (Figure S3).  
  - `visualize_treesapp_genes.R`: Produces bar plots (Figure 4C) to compare PROKKA vs. TreeSAPP.

  *Additional scripts not used in the manuscript but provided as supplementary material:*
  - `class_bargraphs.R`: Bar plots of most abundant phyla based on TreeSAPP.  
  - `species_bargraphs.R`: Species-level bar plots.  
  - `combined_bargraphs.R`: Combined class and species bar plots per gene.

#### `Technical limitations/`
- **Coverage Visualization (Figure 6)**  
  - `mean_coverage.R`: Visualizes coverage information computed by DASTool.

---

## Contact

For further clarification or access to additional code or data, please contact the authors by email.
