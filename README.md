# Saanich inlet diversity :dna: :microscope:
This repo contains the R scripts that were used to generate the figures presented in the metagenomics project paper for the MICB 405 course. You can recreate all the figures in the paper by following the instructions below. 

The repository is organized in such a way to allow users who wish to reproduce our analysis to do so in a simple way. The scripts folder is divided into further subdirectories highlighting the main steps we took in our analysis. 

**CC_plots** folder contains two scripts that makes completion - contamination plots for MAGs generated with MaxBin2 and MetaBAT (Figure S1).  

**Geochemical-water_conditions** folder contains scripts for all figures showing geochemical data (Figure 3, Figure 4A, Figure S2 and S4) across depths and seasons.  

**PROKKA** folder contains 2 scripts:  
* prokka_check_length.R confirms that the gene lengths match in two separate PROKKA outputs.  
* filter_prokka_by_gene.R filters the PROKKA output to only retain the genes of interest and plot the figure 4B.  

**TreeSAPP** folder contains the following scripts:  
* bubble_plot_class.R is used to create figure 5B.  
* bubble_plot.R is used for making comprehensive bubble plots highlighting species and class information for genes of interest in figure S3.  
* visualize_treesapp_genes.R generates the bar plot in figure 4C and allow us to compare the performance of PROKKA to TreeSAPP in terms of nitrogen cycle gene identification.  

  
The following scripts are included in this folder, but we decided not to include the figures they generate in the final manuscript. They are provided here as extra material.  
* class_bargraphs.R script makes bargraphs showing the most abundant phyla based on TreeSAPP annotations.  
* species_bargraphs.R script similarly makes bargraphs showing species information.  
* combined_bargraphs.R script combined information from the two scripts mentioned above makes bargraphs for each gene of interest showing both species and class abundance. 

**Technical limitations** folder contains one script named mean_coverage.R to visualize the coverage information computed by DASTool. This script also makes figure 6. 

Further clarification and code can be provided by the authors upon request by email. 
