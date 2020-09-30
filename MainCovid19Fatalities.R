# set current path directing to this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# clear the global environment
rm(list = ls())

# sets local system language to English
Sys.setlocale("LC_ALL","English")

# load functions and packages ----
source("Functions/Preprocessing.R")
source("Functions/Descriptives.R")
source("Functions/Fitting.R")
source("Functions/Plotting.R")

library(MASS)
library(tidyverse)
library(magrittr)
library(lubridate)
library(mgcv)
library(readxl)
library(rgdal)
library(checkmate)
library(ggplot2)
library(gridExtra)
library(survival)
library(survminer)
library(ggsn)
library(matrixStats)


# settings for fitting ----
T.0 <- as.POSIXlt("2020-03-26", tz = "GMT")
doa <- as.POSIXlt("2020-05-14", tz = "GMT")
d.max <- 40


# preprocess the data ----

# only run lines 46 and 47 if you add new data to the repository
# all data up to July 31 is stored in the repository as preprocessed
# .Rds file.
# if you would like to get the original RKI files please contact
# marc.schneble@lmu.de (the original files are just too large for the repository)

#read.RKI()
#format.RKI()


# descriptive analysis (Figures 1 and 2) ----
duration.time.KM(doa, d.max)


# fitting the models ----

# the first three models need around 20-30 minutes to fit
# the last model needs around 90 minutes to fit

# fit the mortality model including the nowcast estimate in the offset
fit.death.model(doa, T.0, d.max, re = "joint", nowcast = "estimate", 
                print.effects = TRUE)

# fit the mortality model including the 0.025 (0.975) quantile of the nowcast in the offset
fit.death.model(doa, T.0, d.max, re = "joint", nowcast = "lower", return.model = FALSE) 
fit.death.model(doa, T.0, d.max, re = "joint", nowcast = "upper", return.model = FALSE) 

# fit the model from Section 6.2
fit.death.model(doa, T.0, d.max, re = "sep", nowcast = "estimate", return.model = FALSE) 



# creating the model related plots ----

# fit the nowcast model from Section 4 and create Figures 3-5
nowcasting(doa, T.0, d.max, create.plots = TRUE, print.effects = TRUE)

# create Figure 6
plot.nowcasted.deaths.ref(doa)

# create Figure 7
nowcasting.districts(period = 14, doa = doa, T.0 = T.0, d.max = d.max)

# create Figures 8 and 9
plot.effects.deaths(doa, re = "joint", nowcast = "estimate")

# create Figure 10
plot.effects.deaths(doa, re = "sep", nowcast = "estimate")

# create Figure 11
plot.ACF(doa)
