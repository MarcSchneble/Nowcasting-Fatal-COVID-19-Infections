# This function performs the nowcasting described in Section 5 of the paper

# Input: 
# - doa: day of analysis
# - T.0: first registration date to be considered in the nowcast, corresponds to t = 0
# - d.max: maximum duration time that is assumed, see Section 5
# - create.plots: if TRUE, the plots which are depicted in Figures 3-5 and Figure 9
  # are depcicted, if FALSE (default), no plots are produced
# - print.summary: prints the model summary if TRUE (default to FALSE)

# Output: a data frame that contains the estimated distribution function
  # F_t(T-t) as well as the corresponding 2.5% and 97.5% quantiles

nowcasting <- function(doa, T.0, d.max, 
                       create.plots = FALSE, print.summary = FALSE){
  
  # number of considered registration (and also reporting) dates
  T.max <- interval(T.0, doa) %/% days(1)
  # all preprocessed RKI data sets
  files <- list.files(path= paste(path.LRZ, "Data/Formatted", sep = ""))
  # reporting date of each data set
  reporting.dates <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files)), 
                                hour = 0, tz = "GMT")
  
  # only consider data from the last T.max days before the day of analysis
  files <- tail(files[which(doa >= reporting.dates)], T.max)
  reporting.dates <- tail(reporting.dates[which(doa >= reporting.dates)], T.max)
  
  # registration dates
  registration.dates <- reporting.dates - days(1)
  
  # weekday of registration dates
  day.infos <- tibble(registration.date = registration.dates, t = 0:(T.max-1), 
                      weekday = weekdays.POSIXt(registration.dates, abbreviate = TRUE))
  
  # initialize matrix C
  C <- matrix(NA, T.max, d.max)
  rownames(C) <- as.character(registration.dates)
  colnames(C) <- 1:d.max
  
  # initialize lists filled with RKI data
  data <- data.by.date <- vector("list", length(files))
  names(data) <- names(data.by.date) <- reporting.dates
  
  for (i in 1:length(files)) {
    # get cumulative death tolls for every reporting date 
    data[[i]] <- read_rds(paste0(path.LRZ, "Data/Formatted/", files[i]))
    data.by.date[[i]] <- data[[i]] %>% filter(deaths > 0, date >= min(rownames(C))) %>% 
               group_by(date) %>% summarise(deaths = sum(deaths))
      
    # find row indices in C
    ind.row <- tail(match(as.character(data.by.date[[i]]$date), rownames(C)), d.max)
    ind.row <- intersect(ind.row, which(registration.dates >= reporting.dates[i] - days(d.max)))
    
    # find col incices in C
    ind.col <- tail(as.numeric(reporting.dates[i] - data.by.date[[i]]$date), d.max)
    ind.col <- ind.col[which(ind.col <= ncol(C))]
    
    # fill matrix C 
    C[cbind(ind.row, ind.col)] <- tail(data.by.date[[i]]$deaths, length(ind.col))
  }
  
  # replace NAs with zero counts for entries above the NA diagonal
  ind.NAs <- which(is.na(C), arr.ind = TRUE)
  for (i in 1:nrow(ind.NAs)) {
    if (sum(ind.NAs[i, ]) <= T.max+1) {
      C[ind.NAs[i, 1], ind.NAs[i, 2]] <- 0  
    }
  }
  
  # report from April 5 is not complete (only half of the dataset was reported)
  if (is.element("2020-04-05", as.character(reporting.dates))) {
    ind <- which(rownames(C) == "2020-04-04")
    # remove 0-count for April 4
    C[ind, 1] <- NA
    if (ind > 1){
      # other counts are interpolated through geometric mean of 
      # counts from previous and subsequent report
      C[tail(cbind(1:(ind-1), ind:2), d.max-1)] <- 
        tail(round(sqrt(data.by.date$`2020-04-04`$deaths*
                          data.by.date$`2020-04-06`$deaths
                        [1:(nrow(data.by.date$`2020-04-06`)-2)])),d.max-1)
    }
  }
  
  # create matrix N with newly reported cases from the cumulative cases
  N <- C - cbind(0, C[, -ncol(C)])
  # throw a warning if negative N occur (replaced by zeros)
  # this can be due to false reported deaths from the previous days
  if (length(which(N < 0)) > 0){
    message(paste0("Negative N occured! Replaced ", 
                   length(which(N < 0)), " entries with zeros."))
    N[which(N < 0, arr.ind = TRUE)] <- 0
  }
  
  # indeces used for creating the data table
  ind <- which(!is.na(N), arr.ind= TRUE)
  # date table sorted by time index
  data.nowcast <- tibble(t = ind[, 1]-1, d = ind[, 2], C = C[ind], N = N[ind], 
                 weekday = day.infos$weekday[match(t, day.infos$t)], pi = NA) %>% 
    mutate(weekday = factor(weekday, 
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
    arrange(t, d)
  
  # fit the nowcast model and add fitted values to the data frame
  model <- gam(cbind(N, C - N) ~  s(d, k = 5, bs = "ps") + s(t, k = 8, bs = "ps") + 
                 weekday, family = quasibinomial, 
               data = data.nowcast, subset = which(d > 1))
  # print summary if required
  if (print.summary){
    print(summary(model))
  }
  data.nowcast$pi[which(data.nowcast$d > 1)] <- predict.gam(model, type = "response")
  
  # NA indeces in the bottom right corner of C sorted by time index
  ind.NAs <- which(is.na(N), arr.ind = TRUE)
  ind <- ind.NAs[which(rowSums(ind.NAs) > T.max+1), ]
  ind <- ind[order(ind[, 1]), ]
  
  # data to be predicted
  newdata <- tibble(t = ind[, 1]-1, d = ind[, 2], C = NA, N = NA, 
                    weekday = day.infos$weekday[match(t, day.infos$t)], 
                    pi = NA)
  
  # predict probabilites
  # since the s(t) function has very wide confidence bands at the end
  # we take for these days the value of day 5 prior to the day of analysis
  newdata$pi <- predict.gam(model, type = "response", newdata = newdata %>% 
                              mutate(t = pmin(t, T.max-5))) 
  
  # predict C
  data.nowcast <- predict.C(model = model, data = data.nowcast, 
                            newdata = newdata, T.max = T.max)
  
  # observed C
  observed <- rep(0, T.max)
  observed[match(data.by.date[[paste(tail(reporting.dates, 1))]]$date, registration.dates)] <- 
    data.by.date[[paste(tail(reporting.dates, 1))]]$deaths
  
  # save offset in date frame and replace NAs with 1
  # NAs occur if no nowcast is available for some time point t
  # this happens when the last observed C for a registration date is zero
  F.t <- tibble(
    estimate = pmin(observed/data.nowcast$C[which(data.nowcast$d == d.max)], 1),
    lower = pmin(observed/data.nowcast$C.lower[which(data.nowcast$d == d.max)], 1),
    upper = pmin(observed/data.nowcast$C.upper[which(data.nowcast$d == d.max)], 1)) %>%
    replace_na(list(estimate = 1, lower = 1, upper = 1))
  
  if (create.plots){
    plot.effects.nowcast(model, doa, day.infos)
    plot.residuals.nowcast(model, doa)
    plot.nowcast(data.nowcast, doa, T.max, d.max, observed, day.infos, registration.dates)
    plot.F(c(19, T.max-1), data.nowcast, registration.dates)
  } else {
    return(F.t)
  }
}

# This function performs the actual nowcast (i.e. calculates the nowcasted fatalities) 
# and calculates the required quantiles via a bootstrap.
# It is only called inside the function nowcasting(...)

# Input: 
# - model: the fitted nowcast model
# - data: a data frame that contains all the data fitted in the nowcasting model
  # a column that contains the fitted probabilities pi() is also included
# - newdata: a data frame that contains the data to be predicted 
  # (i.e. the NAs in the matrix stated in Section 5)
  # a column that contains the predicted probabilities pi() is also included
# - T.max: the number of considered registration dates
# - n: number of bootstrap samples (default to n = 10 000)
# - alpha: alpha/2 and 1-alpha/2 quantiles of the nowcast will be calculated
  # (default to alpha = 0.05)

# Output: a data frame that binds the rows of data and newdata including new 
# columns for the nowcast estimate as well as the 
# alpha/2 and 1-alpha/2 quantiles

predict.C = function(model, data, newdata, T.max, n = 10000, alpha = 0.05){
  
  # prediction matrix
  modelmatrix = predict.gam(model, type = "lpmatrix", newdata = newdata %>%
                              mutate(t = pmin(t, T.max-5)))
  
  # mean and covariance matrix of estimated model paramters
  theta = model$coefficients
  V = vcov.gam(model)
  
  # simulate from model parameters and determine pi
  X = rmvn(n, theta, V)
  lp = X%*%t(modelmatrix)
  pi = exp(lp)/(1+exp(lp))
  
  # matrix of predicted F and C
  F.sim <- C.sim <- matrix(0, n, ncol(pi))
  
  # add prediction bounds to the datasets
  data$C.lower <- newdata$C.lower <- NA
  data$C.upper <- newdata$C.upper <- NA
  
  # saves indices for reporting dates for which we do not have any observation
  ind.del <- NULL
  
  for (tt in unique(newdata$t)) {
    # get indices in newdata which belong to this time point
    ind.t = which(newdata$t == tt)
    if (length(which(data$t == tt)) > 0){
      # if there is an observation for this reporting date
      C.t = data$C[which(data$t == tt & data$d == max(data$d[which(data$t == tt)]))]
      
      # compute predicted C
      newdata$C[ind.t] = C.t/cumprod(1 - newdata$pi[ind.t])
      
      # compute simulated C
      for (j in 1:n) {
        C.sim[j, ind.t] <- C.t/cumprod(1 - pi[j, ind.t])
      }
    } else {
      # if there is no observation for this reporting date
      message(paste0("No record for reporting time point t = ", t, " available."))
      ind.del <- c(ind.del, which(newdata$t == tt)) 
    }
  }
  
  # if one t has no observation do not compute quantiles 
  if (length(ind.del) > 0){
    ind <- setdiff(1:ncol(pi), ind.del)
  } else {
    ind <- 1:ncol(pi)
  }
  
  # compute alpha/2 and 1-alpha/2 quantiles for every C_{t, d_max}
  for (k in ind) {
    newdata$C.lower[k] <- quantile(C.sim[, k], alpha/2)
    newdata$C.upper[k] <- quantile(C.sim[, k], 1-alpha/2)
  }
  
  return(rbind(data, newdata))
}

# This function fits the quasi-Poisson model stated in Section 4
# (or the extension of the model stated in Section 6.2)
# and saves it into the Output folder

# Input:
# - doa: day of analysis
# - T.0: first registration date to be considered in the nowcast, corresponds to t = 0
# - d.max: maximum duration time that is assumed, see Section 5
# - re: which random effects should be used?
  # re = "joint" performs the analysis of Section 4
  # re = "sep" performs the analysis of Section 6.2
# - nowcast: which nowcasting results should be used for the offset?
  # nowcast = "estimate" uses the estimate
  # nowcast = "lower" uses the alpha/2 quantile
  # nowcast = "upper" uses the 1-alpha/2 quantile
# - return.model: returns the model object if TRUE (default to FALSE)
# - print.summary: prints the model summary if TRUE (default to FALSE)

# Output: model object (if required)

fit.death.model <- function(doa, T.0, d.max, re, nowcast, 
                            return.model = FALSE, print.summary = FALSE){
  
  # number of considered registration (and also reporting) dates
  T.max <- interval(T.0, doa) %/% days(1)
  
  # preprocess district data
  districts <- preprocess.districts()
  
  # gender-age combinations
  age.groups <- c("A15-A34", "A35-A59", "A60-A79", "A80+")
  genders <- c("M", "W")
  
  # get the data for the reporting date
  files <- list.files(path= paste0(path.LRZ, "Data/Formatted"))
  file <- files[grep(as.character(doa), files)]
  data <- read_rds(paste0(path.LRZ, "Data/Formatted/", file)) %>% 
    mutate(day = interval(T.0, date) %/% days(1), 
           weekday = weekdays.POSIXt(date, abbreviate = TRUE))  
  
  # all considered registration dates
  registration.dates <- T.0 %m+% days(x = seq.int(from = 0, to = T.max-1, by = 1))
  weekdays <- weekdays.POSIXt(registration.dates, abbreviate = TRUE)
  
  # get data.long table
  data.long <- get.data.long(data, T.max, age.groups, weekdays)
  
  # nowcast
  F.t <- pull(nowcasting(doa, T.0, d.max), nowcast)
  F.t <- rep(F.t, each = nrow(districts)*length(age.groups)*length(genders)) 

  # fit model
  if (re == "joint"){
    model <- bam(deaths ~ s(day, bs = "ps", k = 8) + s(lon, lat) + 
                   s(day.recent, districtId, bs = "re") + 
                   age + gender + weekday + offset(log(data.long$pop*F.t)), 
                 data = data.long %>% mutate(districtId = factor(districtId), 
                                             day.recent = factor(day.recent)), 
                 family = quasipoisson)
  } else {
    model <- bam(deaths ~ s(day, bs = "ps", k = 8) + s(lon, lat) + 
                   s(day.recent, age.80, districtId, bs = "re") + 
                   age + gender + weekday + offset(log(data.long$pop*F.t)), 
                 data = data.long %>% mutate(districtId = factor(districtId), 
                                             day.recent = factor(day.recent), 
                                             age.80 = factor(age.80)), 
                 family = quasipoisson)
  }

  # add registration dates and data to bam object 
  model$registration.dates <- registration.dates
  model$data <- data.long
  model$F.t <- F.t
    
  # save model in the LRZ folder
  saveRDS(model, file = paste0(path.LRZ, "Output/", nowcast, "_", re, "_", doa, ".Rds"))
  
  # print model summary if required
  if (print.summary){
    print(summary(model))
  }
    
  # return model if required
  if (return.model) {
    return(model)
  } 
}

# This function is used inside fit.death.modeo(...) to produce the data frame
# used for the quasi-Poisson model

# Input: 
# - data: formatted RKI data of the day of analysis
# - T.max: the number of considered registration dates
# - age.groups: a character vector contraining the age groups 
  # which are considered
# - weekdays: character vector of length T.max containing the (abbreviated) 
  # weekday corresponding to the registration dates

# Output: a data frame containing the death counts for all combinations of covariates

get.data.long <- function(data, T.max, age.groups, weekdays){
  
  districts <- preprocess.districts()
  genders <- c("M", "W")
  
  # only extract cells with deaths since date.t0
  data.long = data %>% filter(deaths > 0, day >= 0, 
                              age != "unbekannt", gender != "unbekannt", 
                              age != "A00-A04", age != "A05-A14") %>% 
    group_by_at(vars(day, districtId, age, gender)) %>% 
    summarise(deaths = sum(deaths)) %>% ungroup() 
  
  # if not all districts have an observation within the last T.max days
  if (length(unique(data.long$districtId)) != nrow(districts)){
    missing.Ids <- setdiff(districts$districtId, as.numeric(unique(data.long$districtId)))
    data.long <- bind_rows(data.long, 
                           tibble(districtId = missing.Ids, day = 0, 
                                  age = "A15-A34", gender = "M", deaths = 0))
  }
  
  # if not all days have an observation
  if (length(unique(data.long$day)) != T.max){
    missing.days <- setdiff(0:(T.max-1), unique(data.long$day))
    data.long <- bind_rows(data.long, tibble(districtId = 1001, 
                                             day = missing.days, age = "A15-A34", 
                                             gender = "M", deaths = 0)) 
  }
  
  # complete data.long table
  data.long <- complete(data.long, day, districtId, age, gender, fill = list(deaths = 0)) %>%
    mutate(day.recent = 1*(T.max - day <= 14),
           age.80 = 1*(age == "A80+"),
           gender = factor(gender, levels = genders),
           age = factor(age, levels = c("A35-A59", "A15-A34", "A60-A79", "A80+")),
           weekday = factor(rep(weekdays, 
                                each = nrow(districts)*length(age.groups)*length(genders)), 
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           pop = rep(as.vector(t(as.matrix(select(districts, 9:16)))), T.max),
           lat = rep(rep(districts$lat, each = length(age.groups)*length(genders)), T.max),
           lon = rep(rep(districts$lon, each = length(age.groups)*length(genders)), T.max))
  
  return(data.long)
}

get.fitted <- function(days.from, period = 7, doa, T.0, d.max, return.data = FALSE) {
  
  districts <- preprocess.districts()
  
  # check whether fitted model exists, otherwise fit it
  if (file.exists(paste0(path.LRZ, "Output/estimate_joint_", 
                         as.character(doa), ".Rds"))){
    model <- read_rds(paste0(path.LRZ, "Output/estimate_joint_", 
                             as.character(doa), ".Rds")) 
  } else {
    message("Object does not exist. Fitting the model now. This is taking some time.")
    model <- fit.death.model(doa, T.0, d.max, re = "joint")
  }
  
  # add fitted deaths to the data set
  # here we fit the model without the additional offset F.t
  # since it is included in the model fit we divide by F.t in the following
  data <- model$data %>% 
    mutate(fitted.deaths = predict.bam(model, type = "response")/model$F.t)
  
  # first and last day (since t0)
  day.max <- max(data$day) - days.from
  day.min <- day.max - period + 1
  
  # calculate fitted deaths (per 100000 inhabitants) per district within the period
  deaths.by.district <- data %>% filter(day <= day.max, day >= day.min) %>% 
    group_by(districtId) %>% 
    summarise(fitted.deaths = sum(fitted.deaths), observed.deaths = sum(deaths)) %>% 
    ungroup() %>%
    mutate(fitted.deaths.per100k = fitted.deaths/districts$pop*100000) 
  
  # plot fitted deaths per 100k and ratios
  plot.fitted(deaths.by.district, doa)
  
  if (return.data){
    return(deaths.by.district)
  }
}
