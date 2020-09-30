This repository contains the R-Code related to the paper following paper submitted to Biometrical Journal: 
	Nowcasting Deadly COVID-19 Infections on a Regional Level in Germany

Authors of the manuscript: Marc Schneble, Giacomo de Nicola, Göran Kauermann, Ursula Berger

Responsible for the R-code: Marc Schneble

E-mail contact for questions and comments regarding the R-code: marc.schneble@stat.uni-muenchen.de

Configuration on which the software has been tested:

R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252    LC_MONETARY=English_United States.1252
[4] LC_NUMERIC=C                           LC_TIME=English_United States.1252    

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] matrixStats_0.56.0 ggsn_0.5.0         survminer_0.4.8    ggpubr_0.3.0       survival_3.2-3     gridExtra_2.3      checkmate_2.0.0   
 [8] rgdal_1.4-8        sp_1.4-1           readxl_1.3.1       mgcv_1.8-33        nlme_3.1-144       lubridate_1.7.9    magrittr_1.5      
[15] forcats_0.5.0      stringr_1.4.0      dplyr_1.0.2        purrr_0.3.3        readr_1.3.1        tidyr_1.1.2        tibble_3.0.3      
[22] ggplot2_3.3.2      tidyverse_1.3.0    MASS_7.3-51.5     

loaded via a namespace (and not attached):
 [1] bitops_1.0-6        fs_1.4.1            sf_0.9-3            httr_1.4.1          tools_3.6.3         backports_1.1.7     R6_2.4.1           
 [8] KernSmooth_2.23-16  DBI_1.1.0           colorspace_1.4-1    withr_2.2.0         tidyselect_1.1.0    curl_4.3            compiler_3.6.3     
[15] cli_2.0.2           rvest_0.3.5         xml2_1.3.2          scales_1.0.0        classInt_0.4-3      survMisc_0.5.5      foreign_0.8-75     
[22] rio_0.5.16          jpeg_0.1-8.1        pkgconfig_2.0.3     dbplyr_1.4.3        rlang_0.4.7         rstudioapi_0.11     generics_0.0.2     
[29] zoo_1.8-8           jsonlite_1.6.1      zip_2.0.4           car_3.0-7           Matrix_1.2-18       Rcpp_1.0.2          munsell_0.5.0      
[36] fansi_0.4.1         abind_1.4-5         lifecycle_0.2.0     stringi_1.4.3       carData_3.0-3       plyr_1.8.6          maptools_1.0-1     
[43] crayon_1.3.4        lattice_0.20-38     haven_2.2.0         splines_3.6.3       hms_0.5.3           knitr_1.29          pillar_1.4.4       
[50] rjson_0.2.20        ggsignif_0.6.0      reprex_0.3.0        glue_1.4.1          data.table_1.12.8   modelr_0.1.7        vctrs_0.3.3        
[57] png_0.1-7           RgoogleMaps_1.4.5.3 cellranger_1.1.0    gtable_0.3.0        km.ci_0.5-2         assertthat_0.2.1    xfun_0.16          
[64] openxlsx_4.1.5      xtable_1.8-4        broom_0.5.6         e1071_1.7-3         rstatix_0.5.0       class_7.3-15        KMsurv_0.1-5       
[71] units_0.6-6         ggmap_3.0.0.902     ellipsis_0.3.0  

In order to reproduce all the results of the paper, you just need to source the file MainCovid19Fatalities.R after downloading the
GitHub folder. This folder contains the following subfolders:

1) Data (itself grouped into the following subfolders)

	RKI: Raw RKI data daily downloaded from 
		Link: https://www.arcgis.com/home/item.html?id=f10774f1c63e40168479a1feb6c7ca74
	     This is the original data. Due to reasons of memory we have not included the data sets
	     of every day. If you are interested in receiving the original files, please contact
		marc.schneble@stat.uni-muenchen.de.		
	Formatted: Preprocessed and formatted RKI data from March 27, 2020 - July 31, 2020
	Maps: Boundaries of districts and states of Germany
	Demographic: Population statistics and coordinates of districts

2) Output: The mortality model objects are stored here by default. 

3) Plots: All plots are stored here by default. 

4) Functions:

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