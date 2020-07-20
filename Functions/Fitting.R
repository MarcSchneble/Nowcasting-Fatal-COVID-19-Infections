# This function performs the nowcasting described in Section 4 of the paper

# Input: 
# - doa: day of analysis
# - T.0: first registration date to be considered in the nowcast, corresponds to t = 0
# - d.max: maximum duration time that is assumed, see Section 5
# - create.plots: if TRUE, the plots which are depicted in Figures 3-5 
# are produced, if FALSE (default), no plots are produced
# - print.effects: if TRUE (default to FALSE) prints the numbers shown in Table 3 

# Output: a data frame that contains the estimated distribution function
# F_t(T-t) as well as the corresponding 2.5% and 97.5% quantiles

nowcasting <- function(doa, T.0, d.max, 
                           create.plots = FALSE, print.effects = FALSE){
  
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
  
  data <- data.by.date <- vector("list", length(files))
  names(data) <- reporting.dates
  
  data.nowcast <- NULL
  
  for (tt in 0:(T.max-1)) {
    data[[tt+1]] <- read_rds(paste0(path.LRZ, "Data/Formatted/", files[tt+1])) 
    
    data.t <- data[[tt+1]] %>% mutate(age.80 = 1*(age == "A80+")) %>%
      filter(date >= max(registration.dates[tt+1] - days(d.max-1), registration.dates[1]), 
                                  age != "unbekannt", gender != "unbekannt") %>% 
      group_by_at(vars(date, age.80)) %>% summarise(C.t.d = pmax(sum(deaths), 0)) %>% ungroup() 
    
    num.days <- min(d.max, tt)
    time.points <- seq(max(tt-d.max+1, 0), tt, 1)
    
    data.t <- complete(data.t, date, age.80, fill = list(C.t.d = 0)) %>%
      mutate(t = rep(time.points, each = 2),
             d = interval(date, reporting.dates[tt+1]) %/% days(1))
    
    data.t.d <- NULL
    
    for (dd in 1:min(d.max, tt+1)){
      data.t.d <- bind_rows(data.t.d, 
                            filter(data.t, d == dd) %>% 
                              mutate(N.t.d = switch(as.character(dd),
                                                    "1" = filter(data.t, t == tt, d == dd)$C.t.d,
                                                    filter(data.t, t == tt-dd+1, d == dd)$C.t.d - 
                                                      filter(data.nowcast, t == tt-dd+1, d == dd-1)$C.t.d)))
    }
    
    data.nowcast <- bind_rows(data.nowcast, data.t.d) 
  }
  
  data.nowcast <- data.nowcast %>% arrange(t, d) %>%
    mutate(weekday = day.infos$weekday[match(t, day.infos$t)], pi = NA)
  
  # interpolate data for report of April 5 (only half of the cases were reported)
  data.nowcast[which(data.nowcast$t + data.nowcast$d == 10 & data.nowcast$d > 1), ]$C.t.d <-
    round(sqrt((filter(data.nowcast, t+d == 9) %>% pull(C.t.d))*
    head((filter(data.nowcast, t+d == 11) %>% pull(C.t.d)), 18)), 0)
  data.nowcast[which(data.nowcast$t + data.nowcast$d == 10 & data.nowcast$d > 1), ]$N.t.d <-
    head((filter(data.nowcast, t+d == 10) %>% pull(C.t.d)), 18) - (filter(data.nowcast, t+d == 9) %>% pull(C.t.d))
  data.nowcast[which(data.nowcast$t + data.nowcast$d == 11 & data.nowcast$d > 1), ]$N.t.d <-
    head((filter(data.nowcast, t+d == 11) %>% pull(C.t.d)), 20) - (filter(data.nowcast, t+d == 10) %>% pull(C.t.d))
  
  data.nowcast <- data.nowcast %>% 
    mutate(N.t.d = pmax(N.t.d, 0),
           weekday = factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))
  
  # fit the nowcast model and add fitted values to the data frame
  model <- gam(cbind(N.t.d, C.t.d - N.t.d) ~ s(d, k = 5, bs = "ps", by = age.80) +
                 s(t, k = 8, bs = "ps") + 
                 weekday, family = quasibinomial, 
               data = data.nowcast %>% mutate(age.80 = factor(age.80)), subset = which(d > 1))
  
  data.nowcast$pi[which(data.nowcast$d > 1)] <- predict.gam(model, type = "response")
  
  # predict probabilites
  # since the s(t) function has very wide confidence bands at the end
  # we take for these days the value of day 5 prior to the day of analysis
  newdata <- data.nowcast %>% complete(t, d, age.80, fill = list(C.t.d = NA, N.t.d = NA, pi = NA)) %>%
    mutate(weekday = factor(day.infos$weekday[match(t, day.infos$t)], levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>%
    filter(t+d > T.max) 
  
  newdata <- mutate(newdata, pi = predict.gam(model, type = "response", newdata = newdata %>% 
                              mutate(t = pmin(t, T.max-5))))
  
  data.nowcast <- predict.F(model = model, data = data.nowcast, 
                            newdata = newdata, T.max = T.max)
  
  F.t <- data.nowcast %>% filter(d == d.max) %>%
    mutate(estimate = F.estimate, lower = F.lower, upper = F.upper) %>% 
    dplyr::select(t, age.80, estimate, lower, upper)
  
  if (create.plots){
    plot.effects.nowcast(model, doa, day.infos)
    plot.nowcast(data.nowcast, doa, T.max, d.max, day.infos, registration.dates)
    plot.F(c(18, T.max-1), data.nowcast, registration.dates)
  } else {
    return(F.t)
  }
  
  if (print.effects){
    ind <- 1:7
    beta.hat <- model$coefficients[ind]
    table.effects <- tibble(name = names(beta.hat), 
                            beta.hat = round(beta.hat, 2), 
                            se = round(sqrt(diag(vcov(model)))[ind], 3),
                            rr = round(exp(model$coefficients[ind]), 2))
    table.effects <- bind_cols(table.effects, rr.intervals(model, ind = ind))
    print(table.effects)
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

predict.F <- function(model, data, newdata, T.max, n = 10000, alpha = 0.05){
  
  # prediction matrix
  modelmatrix = predict.gam(model, type = "lpmatrix", newdata = newdata %>%
                              mutate(t = pmin(t, T.max-5)))
  
  # mean and covariance matrix of estimated model paramters
  theta = model$coefficients
  V = vcov.gam(model)
  
  # simulate from model parameters and determine pi
  set.seed(1)
  X = rmvn(n, theta, V)
  lp = X%*%t(modelmatrix)
  pi = exp(lp)/(1+exp(lp))
  
  # matrix of predicted F and C
  F.sim <- C.sim <- matrix(0, n, ncol(pi))
  
  # add prediction bounds to the datasets
  data$C.estimate <- newdata$C.estimate <- NA
  data$C.lower <- newdata$C.lower <- NA
  data$C.upper <- newdata$C.upper <- NA
  
  # add F to the datasets
  data$F.estimate <- newdata$F.estimate <- 1
  data$F.lower <- newdata$F.lower <- 1
  data$F.upper <- newdata$F.upper <- 1
  
  for (tt in unique(newdata$t)) {
    ind.t.a80 = which(newdata$t == tt & newdata$age.80 == 1)
    ind.t.u80 = which(newdata$t == tt & newdata$age.80 == 0)
    
    C.t.d.a80 <- data$C.t.d[which(data$t == tt & data$d == max(data$d[which(data$t == tt)]) & data$age.80 == 1)]
    C.t.d.u80 <- data$C.t.d[which(data$t == tt & data$d == max(data$d[which(data$t == tt)]) & data$age.80 == 0)]
    
    newdata$F.estimate[ind.t.u80] <- cumprod(1 - newdata$pi[ind.t.u80]) 
    newdata$F.estimate[ind.t.a80] <- cumprod(1 - newdata$pi[ind.t.a80]) 
    
    # compute predicted C
    newdata$C.estimate[ind.t.u80] <- C.t.d.u80/newdata$F.estimate[ind.t.u80]
    newdata$C.estimate[ind.t.a80] <- C.t.d.a80/newdata$F.estimate[ind.t.a80]
    
    # compute simulated C
    for (j in 1:n) {
      F.sim[j, ind.t.u80] <- cumprod(1 - pi[j, ind.t.u80])
      F.sim[j, ind.t.a80] <- cumprod(1 - pi[j, ind.t.a80])
      C.sim[j, ind.t.u80] <- C.t.d.u80/F.sim[j, ind.t.u80]
      C.sim[j, ind.t.a80] <- C.t.d.a80/F.sim[j, ind.t.a80]
    }
  }
  
  # compute alpha/2 and 1-alpha/2 quantiles for every C_{t, d_max}
  for (k in 1:ncol(pi)) {
    newdata$F.lower[k] <- quantile(F.sim[, k], 1-alpha/2)
    newdata$F.upper[k] <- quantile(F.sim[, k], alpha/2)
    newdata$C.lower[k] <- quantile(C.sim[, k], alpha/2)
    newdata$C.upper[k] <- quantile(C.sim[, k], 1-alpha/2)
  }
  
  return(bind_rows(data, newdata))
}



# This function fits the quasi-Poisson model stated in Section 4
# (or the extension of the model stated in Section 6.2)
# and saves it into the Output folder

# Input:
# - doa: day of analysis
# - T.0: first registration date to be considered in the nowcast, corresponds to t = 0
# - d.max: maximum duration time that is assumed, see Section 4
# - re: which random effects should be used?
  # re = "joint" performs the analysis of Section 5
  # re = "sep" performs the analysis of Section 6.2
# - nowcast: which nowcasting results should be used for the offset?
  # nowcast = "estimate" uses the estimate
  # nowcast = "lower" uses the alpha/2 quantile
  # nowcast = "upper" uses the 1-alpha/2 quantile
# - return.model: returns the model object if TRUE (default to FALSE)
# - print.effects: if TRUE (default to FALSE) prints the numbers
# - which are shown in Table 2

# Output: model object (if required)

fit.death.model <- function(doa, T.0, d.max, re, nowcast, 
                            return.model = FALSE, print.effects = FALSE){
  
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
  data.long <- get.data.long(data, T.max, age.groups, weekdays) %>% mutate(F.t = 1)
  
  # nowcast
  F.t <- nowcasting(doa, T.0, d.max) %>% dplyr::select(t, age.80, nowcast)
  data.long[which(data.long$age.80 == 0), ]$F.t <- rep(F.t %>% filter(age.80 == 0) %>% pull(nowcast), each = nrow(districts)*2*3)  
  data.long[which(data.long$age.80 == 1), ]$F.t <- rep(F.t %>% filter(age.80 == 1) %>% pull(nowcast), each = nrow(districts)*2) 

  # fit model
  if (re == "joint"){
    model <- bam(deaths ~ s(day, bs = "ps", k = 8) + s(lon, lat) + 
                   s(day.recent, districtId, bs = "re") + 
                   age*gender + weekday + offset(log(pop*F.t)), 
                 data = data.long %>% mutate(districtId = factor(districtId), 
                                             day.recent = factor(day.recent)), 
                 family = nb, nthreads = 20)
  } else {
    model <- bam(deaths ~ s(day, bs = "ps", k = 8) + s(lon, lat) + 
                   s(day.recent, age.80, districtId, bs = "re") +
                   age*gender + weekday + offset(log(pop*F.t)), 
                 data = data.long %>% mutate(districtId = factor(districtId), 
                                             day.recent = factor(day.recent), 
                                             age.80 = factor(age.80)), 
                 family = nb, nthreads = 20)
  }

  # add registration dates and data to bam object 
  model$registration.dates <- registration.dates
  model$data <- data.long
  model$F.t <- F.t
    
  # save model in the LRZ folder
  saveRDS(model, file = paste0(path.LRZ, "Output/", nowcast, "_", re, "_", doa, ".Rds"))
  
  # print model summary if required
  if (print.effects){
    ind <- 1:14
    beta.hat <- model$coefficients[ind]
    table.effects <- tibble(name = names(beta.hat), 
                            beta.hat = round(beta.hat, 2), 
                            se = round(sqrt(diag(vcov(model)))[ind], 3),
                            rr = round(exp(model$coefficients[ind]), 2))
    table.effects <- bind_cols(table.effects, rr.intervals(model, ind = ind))
    print(table.effects)
  }
    
  # return model if required
  if (return.model) {
    return(model)
  } 
}

# This function is used inside fit.death.model(...) to produce the data frame
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
  data.long = data %>% filter(day >= 0, 
                              age != "unbekannt", gender != "unbekannt", 
                              age != "A00-A04", age != "A05-A14") %>% 
    group_by_at(vars(day, districtId, age, gender)) %>% 
    summarise(deaths = pmax(sum(deaths), 0), cases = pmax(sum(cases), 0)) %>% ungroup() 
  
  # if not all districts have an observation within the last T.max days
  if (length(unique(data.long$districtId)) != nrow(districts)){
    missing.Ids <- setdiff(districts$districtId, as.numeric(unique(data.long$districtId)))
    data.long <- bind_rows(data.long, 
                           tibble(districtId = missing.Ids, day = 0, 
                                  age = "A15-A34", gender = "M", deaths = 0, cases = 0))
  }
  
  # if not all days have an observation
  if (length(unique(data.long$day)) != T.max){
    missing.days <- setdiff(0:(T.max-1), unique(data.long$day))
    data.long <- bind_rows(data.long, tibble(districtId = 1001, 
                                             day = missing.days, age = "A15-A34", 
                                             gender = "M", deaths = 0, cases = 0)) 
  }
  
  # complete data.long table
  data.long <- complete(data.long, day, districtId, age, gender, fill = list(deaths = 0, cases = 0)) %>%
    mutate(day.recent = 1*(T.max - day <= 14),
           age.80 = 1*(age == "A80+"),
           gender = factor(gender, levels = genders),
           age = factor(age, levels = c("A35-A59", "A15-A34", "A60-A79", "A80+")),
           weekday = factor(rep(weekdays, 
                                each = nrow(districts)*length(age.groups)*length(genders)), 
                            levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
           pop = rep(as.vector(t(as.matrix(dplyr::select(districts, 9:16)))), T.max),
           lat = rep(rep(districts$lat, each = length(age.groups)*length(genders)), T.max),
           lon = rep(rep(districts$lon, each = length(age.groups)*length(genders)), T.max))
  
  return(data.long)
}

# This function calculates the district-wise nowcasts that arise from
# the mortality model in Section 3 with the nowcast from Section 4 included
# in the offset

# Input:
# - period: the number of days before the date of analysis that should
#   be nowcasted
# - doa: day of analysis
# - T.0: first registration date to be considered in the nowcast, corresponds to t = 0
# - d.max: maximum duration time that is assumed, see Section 5
# - return.nowcast: if required, returns the table with the nowcasted values

nowcasting.districts <- function(period, doa, T.0, d.max, return.nowcast = FALSE) {
  
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
    mutate(fitted.deaths = predict.bam(model, type = "response")/model$data$F.t)
  
  # first and last day (since t0)
  day.max <- max(data$day)
  day.min <- day.max - period + 1
  
  # calculate fitted deaths (per 100000 inhabitants) per district within the period
  deaths.by.district <- data %>% filter(day <= day.max, day >= day.min) %>% 
    group_by(districtId) %>% 
    summarise(fitted.deaths = sum(fitted.deaths), observed.deaths = sum(deaths)) %>% 
    ungroup() %>%
    mutate(fitted.deaths.per100k = fitted.deaths/districts$pop*100000) 
  
  # plot fitted deaths per 100k and ratios
  plot.fitted(deaths.by.district, doa, model, day.min, day.max)
  
  if (return.nowcast){
    return(deaths.by.district)
  }
}

# computes 1-alpha confidence intervals of the relative risk (rr) by simulation

# Input:
# - model: fitted bam model
# - n: number of bootstrap samples (default to n = 100 000)
# - alpha: alpha/2 and 1-alpha/2 quantiles of the nowcast will be calculated
# (default to alpha = 0.05)
# - ind: determines for which of the model parameters the rr are being computed 

rr.intervals <- function(model, n = 10000, alpha = 0.05, ind = 1:length(model$coefficients)){
  
  # mean and covariance matrix of estimated model paramters
  set.seed(1)
  theta = model$coefficients
  V = vcov.gam(model)
  
  # simulate from model parameters 
  X = exp(rmvn(n, theta, V))
  Q = colQuantiles(X, probs = c(alpha/2, 1-alpha/2))
  rr.intervals = round(tibble(rr.lower = Q[ind, 1], rr.upper = Q[ind, 2]), 2)
  
  return(rr.intervals)
}