This repository contains the R-Code related to the submitted paper to Biometrical Journal: 
	Nowcasting Deadly COVID-19 Infections on a Regional Level in Germany

Authors of the manuscript: Marc Schneble, Giacomo de Nicola, Göran Kauermann, Ursula Berger

Responsible for the R-code: Marc Schneble and Giacomo de Nicola

E-mail contact for questions and comments regarding the R-code: marc.schneble@stat.uni-muenchen.de

Configuration of R used for our analyses: 
R version 3.6.3 (2020-02-29)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 17763)

Matrix products: default

locale:
[1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
[3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
[5] LC_TIME=English_United States.1252    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] checkmate_1.9.4 rgdal_1.4-7     sp_1.4-1        readxl_1.3.1    mgcv_1.8-31     nlme_3.1-144   
 [7] lubridate_1.7.4 magrittr_1.5    forcats_0.4.0   stringr_1.4.0   dplyr_0.8.3     purrr_0.3.3    
[13] readr_1.3.1     tidyr_1.0.0     tibble_2.1.3    ggplot2_3.2.1   tidyverse_1.2.1

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.2       cellranger_1.1.0 pillar_1.4.2     compiler_3.6.3   tools_3.6.3     
 [6] zeallot_0.1.0    jsonlite_1.6     lifecycle_0.1.0  gtable_0.3.0     lattice_0.20-38 
[11] pkgconfig_2.0.3  rlang_0.4.1      Matrix_1.2-18    cli_1.1.0        rstudioapi_0.10 
[16] haven_2.2.0      withr_2.1.2      xml2_1.2.2       httr_1.4.1       generics_0.0.2  
[21] vctrs_0.2.0      hms_0.5.2        grid_3.6.3       tidyselect_0.2.5 glue_1.3.1      
[26] R6_2.4.0         modelr_0.1.5     splines_3.6.3    backports_1.1.5  scales_1.0.0    
[31] rvest_0.3.5      assertthat_0.2.1 colorspace_1.4-1 stringi_1.4.3    lazyeval_0.2.2  
[36] munsell_0.5.0    broom_0.5.2      crayon_1.3.4   

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

Fitting.R: This file contains all the modeling. The first two functions (nowcasting() and predict.C(), 
	the latter only used inside the former) provide the nowcasting results. The function fit.death.model()
	produce the results from the models in Sections 4 and Sections 6.2. The function get.data.long() 
	is only used inside the function fit.death.model(). The function get.fitted() calculates the 
	fitted deaths for a specified time frame.

Plotting.R: This file contains all the functions that are used for plotting the results. The first four 
	functions produce the plots for the nowcasting results. The function plot.map() is used inside
	plot.fitted() and plot.effects.deaths() and produces the maps in the paper. 
	The function plot.nowcasted.death.rate() produces Figure 1.