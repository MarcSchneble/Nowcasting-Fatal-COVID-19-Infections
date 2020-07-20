# set current path directing to this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# clear the global environment
rm(list = ls())

# sets local system language to English
Sys.setlocale("LC_ALL","English")

# path to the folder where all the data are stored and
# model objects and plots are saved to
# insert your local absolute path here!!!
path.LRZ <<- "C:/Users/ru58paj/LRZ Sync+Share/Nowcasting Fatal COVID-19 Infections/"                     


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

read.RKI()
format.RKI()


# descriptive analysis ----

# produces Figures 1 and 2
duration.time.KM(doa, d.max)


# fitting the models ----

# the first three models need around 20-30 minutes to fit
# the last model needs around 90 minutes to fit
# the model outputs are already in the LRZ folder that can be downloaded
# so running these models is for reproducing only

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