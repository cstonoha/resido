# Introduction to resido

<!-- badges: start -->

```         
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15604830.svg)](https://doi.org/10.5281/zenodo.15604830)
```

<!-- badges: end -->

resido was designed to explore amino acid composition of peptide/protein sequences in proteomes or a smaller subset of sequences. This package facilitates the characterization and discovery of proteins based on amino acid content. resido accepts a FASTA file of one or more amino acid sequences and can perform various analyses. The default output is a dataframe that reports the total number or percentage of amino acids of interest per sequence, but the user can also choose the output to be a csv file. There are visual output options that illustrate amino acid frequency of peptide sequences as well as multi-protein heat map comparisons of amino acid content.

resido is available on GitHub and Zenodo.

## Installation

You can install the development version of resido from [GitHub] (<https://github.com/>):

``` r
# install.packages("devtools")
devtools::install_github("cstonoha/resido")
```

## Usage

For the vast majority of functions in resido, the standard input is a FASTA file, which will be read into a dataframe. This is an example using protein sequences from chromosome six of *Medicago truncatula*.

``` r
library(resido)
## This takes the FASTA input and calculates the percent sulfur-containing amino acids for each sequence in the file. It then sorts the resulting dataframe by percent sulfur amino acids. 

medicago_protein_dataframe <- resido_amino_acid_group_percent("~/resido/sample_data/M_truncatula_chr6.fasta", "sulfur", sort_by = "percent_amino_acid_group", decreasing = TRUE)
```

This takes the sorted dataframe from the previous example and shows the ten sequences with the highest percent of sulfur amino acids.

``` r
## This builds on the list from the previous example

medicago_dataframe_top_ten <- medicago_protein_dataframe[1:10, , drop = FALSE]
```
