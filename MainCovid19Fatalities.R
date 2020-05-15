# set current path to this file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# clear the global environment
rm(list = ls())

# sets local system language to English
Sys.setlocale("LC_ALL","English")

# path to the folder where all the data are stored and
# model objects and plots are saved to
# replace your local absolute path here!!!
path.LRZ <<- "C:/Users/ru58paj/LRZ Sync+Share/Nowcasting Fatal COVID-19 Infections/"                     



# load functions and packages ----
source("Functions/Preprocessing.R")
source("Functions/Fitting.R")
source("Functions/Plotting.R")

library(tidyverse)
library(magrittr)
library(lubridate)
library(mgcv)
library(readxl)
library(rgdal)
library(checkmate)
library(ggplot2)



# settings for fitting ----
doa <- as.POSIXlt("2020-05-14", tz = "GMT")
T.0 <- as.POSIXlt("2020-03-26", tz = "GMT")
d.max <- 30



# preprocess the data ----

read.RKI()
format.RKI()



# fitting the models ----
# the first three models need around 20-30 minutes to fit
# the last model needs around 90 minutes to fit
# the model outputs are already in the LRZ folder that can be downloaded
# so running these models is for reproducing only

# fit the model from Section 4 including the nowcast estimate in the offset
fit.death.model(doa, T.0, d.max, re = "joint", nowcast = "estimate", 
                return.model = FALSE, print.summary = TRUE)

# fit the model from Section 4 including the 0.025 (0.975) quantile of the nowcast in the offset
fit.death.model(doa, T.0, d.max, re = "joint", nowcast = "lower", return.model = FALSE)
fit.death.model(doa, T.0, d.max, re = "joint", nowcast = "upper", return.model = FALSE)

# fit the model from Section 6.2
fit.death.model(doa, T.0, d.max, re = "sep", nowcast = "estimate", return.model = FALSE)



# creating the plots ----

# create Figure 1
plot.nowcasted.death.rate(doa)

# create Figure 2
get.fitted(days.from = 3, period = 7, doa = doa, T.0 = T.0, d.max = d.max)

# fit the nowcast model from Section 5 and create Figures 3-5 and 9
nowcasting(doa, T.0, d.max, create.plots = TRUE, print.summary = TRUE)

# create Figures 6 and 7
plot.effects.deaths(doa, re = "joint", nowcast = "estimate")

# createFigure 8
plot.effects.deaths(doa, re = "sep", nowcast = "estimate")