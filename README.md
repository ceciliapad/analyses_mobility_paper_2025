# Code for Padilla-Iglesias et al. 2025: "Sexual division of labour shapes hunter-gatherer spatial ranges"

This code reproduces the results of the paper "Sexual division of labour shapes hunter-gatherer spatial ranges" by Padilla-Iglesias et al. 2025.

All data required to reproduce the results from the paper is contained in the `data` folder, with the exception of the GPS coordinates of 
the study sites, which are not included for privacy reasons. Nonetheless, distances travelled for each displacement are provided.

The code is organized into sections that correspond to the main figures and tables in the paper, and scripts can be inspected in any order.
All scripts can be run in R, and all the required packages are listed in the section below.

All results reported in the manuscript and supplementary material are in the `results` folder.

Running most Bayesian models used in the scripts can take 5-10 minutes (each) in a standard laptop.


## R session information 

R version 4.4.2 (2024-10-31)
Platform: aarch64-apple-darwin20
Running under: macOS Sequoia 15.4

Matrix products: default
BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib 
LAPACK: /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.0

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

time zone: Europe/Zurich
tzcode source: internal

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] lubridate_1.9.4    forcats_1.0.0      stringr_1.5.1      purrr_1.0.2        readr_2.1.5       
 [6] tibble_3.2.1       tidyverse_2.0.0    measurements_1.5.1 ggstatsplot_0.13.0 brms_2.22.0       
[11] Rcpp_1.0.14        MetBrewer_0.2.0    vtable_1.4.8       kableExtra_1.4.0   ggplot2_3.5.1     
[16] dplyr_1.1.4        tidyr_1.3.1 

