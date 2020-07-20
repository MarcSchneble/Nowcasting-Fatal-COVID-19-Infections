This repository contains the R-Code related to the submitted paper to Biometrical Journal: 
	Nowcasting Deadly COVID-19 Infections on a Regional Level in Germany

Authors of the manuscript: Marc Schneble, Giacomo de Nicola, Göran Kauermann, Ursula Berger

Responsible for the R-code: Marc Schneble and Giacomo de Nicola

E-mail contact for questions and comments regarding the R-code: marc.schneble@stat.uni-muenchen.de

R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] matrixStats_0.56.0 ggsn_0.5.0         survminer_0.4.6    ggpubr_0.3.0      
 [5] survival_3.1-12    gridExtra_2.3      checkmate_2.0.0    rgdal_1.4-8       
 [9] sp_1.4-1           readxl_1.3.1       mgcv_1.8-31        nlme_3.1-144      
[13] lubridate_1.7.4    magrittr_1.5       forcats_0.5.0      stringr_1.4.0     
[17] dplyr_0.8.3        purrr_0.3.3        readr_1.3.1        tidyr_1.0.3       
[21] tibble_3.0.1       ggplot2_3.3.0      tidyverse_1.3.0    MASS_7.3-51.5     

loaded via a namespace (and not attached):
 [1] bitops_1.0-6        fs_1.4.1            sf_0.9-3            httr_1.4.1         
 [5] tools_3.6.3         backports_1.1.7     R6_2.4.1            KernSmooth_2.23-16 
 [9] DBI_1.1.0           colorspace_1.4-1    withr_2.2.0         tidyselect_0.2.5   
[13] curl_4.3            compiler_3.6.3      cli_2.0.2           rvest_0.3.5        
[17] xml2_1.3.2          labeling_0.3        scales_1.0.0        classInt_0.4-3     
[21] survMisc_0.5.5      digest_0.6.25       foreign_0.8-75      rio_0.5.16         
[25] jpeg_0.1-8.1        pkgconfig_2.0.3     dbplyr_1.4.3        rlang_0.4.6        
[29] rstudioapi_0.11     generics_0.0.2      zoo_1.8-8           jsonlite_1.6.1     
[33] zip_2.0.4           car_3.0-7           Matrix_1.2-18       Rcpp_1.0.2         
[37] munsell_0.5.0       fansi_0.4.1         abind_1.4-5         lifecycle_0.2.0    
[41] stringi_1.4.3       carData_3.0-3       plyr_1.8.6          maptools_1.0-1     
[45] crayon_1.3.4        lattice_0.20-38     haven_2.2.0         splines_3.6.3      
[49] hms_0.5.3           knitr_1.28          pillar_1.4.4        rjson_0.2.20       
[53] ggsignif_0.6.0      reprex_0.3.0        glue_1.4.1          data.table_1.12.8  
[57] modelr_0.1.7        vctrs_0.3.0         png_0.1-7           RgoogleMaps_1.4.5.3
[61] cellranger_1.1.0    gtable_0.3.0        km.ci_0.5-2         assertthat_0.2.1   
[65] xfun_0.13           openxlsx_4.1.5      xtable_1.8-4        broom_0.5.6        
[69] e1071_1.7-3         rstatix_0.5.0       class_7.3-15        KMsurv_0.1-5       
[73] units_0.6-6         ggmap_3.0.0         ellipsis_0.3.0 

In order to reproduce our analyses the following folder 

Link: https://syncandshare.lrz.de/getlink/fi7u5WzqBV4GN5YKu1KKKvdZ/, 

hosted at the Leibniz-Rechenzentrum (LRZ), needs to be downloaded as it is and saved locally. 
It contains the data required to fit the models and consists of the following subfolders:

1) Data (itself grouped into the following subfolders)

	RKI: Raw RKI data daily downloaded from 
		Link: https://www.arcgis.com/home/item.html?id=f10774f1c63e40168479a1feb6c7ca74
	Formatted: Preprocessed and formatted RKI data since March 27, 2020
	Maps: Boundaries of districts and states of Germany
	Demographic: Population statistics and coordinates of districts

2) Output: All R model objects are stored here by default. The model objects of the quasi-Poisson model
	are already stored here but can be reproduced by sourcing the main file.

3) Plots: All plots produced are stored here by default. 


The R-Code is broken down into the following sub-files:

MainCovid19Fatalities.R: This is the main file. Running this file produces all the results presented in the paper. 
	The only thing that needs to be adjusted by every user is the variable ``path.LRZ``. 
	Here, every user needs to specify his/her specific absolute path to the LRZ folder described above.

Preprocessing.R: The first two functions are only used when a new data set from the RKI needs to be read in
	(read.RKI()) and formatted (format.RKI()). The last function (preprocess.districts()) is
	used inside other functions and provides information on the gender/age specific population sizes
	in each district and coordinates of the centroids of each district.

Descriptives.R: Produces the descriptive analysis in Section 2.

Fitting.R: This file contains all the modeling. The first two functions (nowcasting() and predict.R(), 
	the latter only used inside the former) provide the nowcasting results. The function fit.death.model()
	produce the results from the models in Sections 3 and Sections 6.2. The function get.data.long() 
	is only used inside the function fit.death.model(). The function nowcasting.districts() calculates the 
	fitted deaths for a specified time frame.

Plotting.R: This file contains all the functions that are used for plotting the results. The first three
	functions produce the plots for the nowcasting results. The function plot.map() is used inside
	plot.fitted() and plot.effects.deaths() and produces the maps in the paper. 
	The function plot.nowcasted.deaths.ref() produces Figure 6.