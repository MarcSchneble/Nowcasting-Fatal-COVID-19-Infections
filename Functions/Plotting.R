# This function creates plots of the smooth effects in the nowcast model
# and saves them in the LRZ folder

# Input: 
# - model: the nowcast model object
# - doa: day of analysis
# - day.infos: a data frame consisting of the columns named by 
# t (values 0,...,T.max-1)
# registration date (corresponding to t)

# Output: none

plot.effects.nowcast <- function(model, doa, day.infos){
  
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  # time plot
  pd <- plot.gam(model, select = 0)[[3]]
  s.t <- data.frame(x = pd$x, y = pd$fit, lower = pd$fit - pd$se, upper = pd$fit + pd$se)          
  
  pdf(file = paste0("Plots/Nowcasting/TimeEffect/", doa, ".pdf"), width = 6, height = 4) 
  g <- ggplot(s.t, aes(x = x)) + geom_line(aes(y = y), size = 1.2) + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) + 
    labs(x = "Registration Date", y = expression(widehat(s)[1](t))) + 
    scale_x_continuous(breaks = seq(0, max(s.t$x), 2), 
                       labels = as.character(substring(day.infos$registration.date, 6))
                       [seq(0, max(s.t$x)+1, 2)]) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  print(g)
  dev.off()
  
  # delay plots
  pd.u80 <- plot.gam(model, select = 0)[[1]]
  pd.a80 <- plot.gam(model, select = 0)[[2]]
  s.d <- data.frame(x = c(pd.u80$x, pd.a80$x), y = c(pd.u80$fit, pd.a80$fit), 
                    lower = c(pd.u80$fit - pd.u80$se, pd.a80$fit - pd.a80$se), 
                    upper = c(pd.u80$fit + pd.u80$se, pd.a80$fit + pd.a80$se),
                    kind = c(rep("80-", length(pd.u80$x)), rep("80+", length(pd.a80$x))))
  
  pdf(file = paste0("Plots/Nowcasting/DurationEffect/", doa, ".pdf"), 
      width = 6, height = 4) 
  g <- ggplot(s.d, aes(x = x)) + geom_line(aes(y = y, color = kind, linetype = kind), size = 1.2) + 
    geom_ribbon(data = s.d %>% filter(kind == "80-"), aes(ymin = lower, ymax = upper), alpha = 0.3) + 
    geom_ribbon(data = s.d %>% filter(kind == "80+"), aes(ymin = lower, ymax = upper), alpha = 0.2) + 
    labs(x = "d", y = expression(paste(widehat(s)[2](d), "  and  ", widehat(s)[2](d)+widehat(s)[3](d))), 
         color = "Age Group", linetype = "Age Group") + 
    scale_x_continuous(breaks = seq(0, d.max, 5), limits = c(0, d.max)) + 
    theme(legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.99), legend.text.align = 0) + 
    scale_color_manual(values = c("seagreen", "sienna"), 
                       labels = c(expression(paste("80-  [", widehat(s)[2](d), "]")), 
                                  expression(paste("80+  [", widehat(s)[2](d)+ widehat(s)[3](d), "]")))) + 
    scale_linetype_manual(values = c(1, 2), 
                          labels = c(expression(paste("80-  [", widehat(s)[2](d), "]")), 
                                     expression(paste("80+  [", widehat(s)[2](d)+ widehat(s)[3](d), "]"))))
  print(g)
  dev.off()
}


# This function creates the plot of the nowcasting results 
# and saves it in the LRZ folder

# Input: 
# - model: the nowcast model object
# - doa: day of analysis
# - T.max: the number of considered registration dates
# - d.max: maximum duration time that is assumed, see Section 5
# - observed: death counts observed on the day of analysis for 
# registration dates t = 0,...,T.max-1
# - day.infos: a data frame consisting of the columns named by 
# t (values 0,...,T.max-1)
# registration date (corresponding to t)
# - regisration.dates: dates corresponding to t = 0,...,T.max-1

#Output: none

plot.nowcast <- function(data, doa, T.max, d.max, 
                         day.infos, registration.dates){
  
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  # get the observed values at the day of analysis
  observed <- read_rds(paste0("Data/Formatted/cases_GermanTemporal_", doa, ".rds")) %>%
    filter(date >= min(registration.dates), date <= max(registration.dates),
           age != "A00-A04", age != "A05-A14", age != "unbekannt", gender != "unbekannt") %>%
    group_by(date) %>%
    summarize(deaths = sum(deaths)) %>%
    ungroup() %>%
    pull(deaths)
  
  # extract the nowcasts
  data <- data %>% filter(d == d.max) %>% group_by(t) %>% 
    summarize(C.estimate = sum(C.estimate), C.lower = sum(C.lower), C.upper = sum(C.upper)) %>% 
    ungroup() %>% 
    mutate(C.observed = observed,
           C.observed.finally = C.observed,
           C.estimate = coalesce(C.estimate, C.observed),
           C.lower = coalesce(C.lower, C.observed),
           C.upper = coalesce(C.upper, C.observed)) 
  
  # get the observed values after d.max days
  for (t in (T.max-d.max+1):(T.max-1)) {
    data.d.max <- read_rds(paste0("Data/Formatted/cases_GermanTemporal_", T.0 + days(t + d.max), ".rds"))
    data$C.observed.finally[t+1] <- 
      data.d.max %>% filter(date == registration.dates[t+1], 
                            age != "A00-A04", age != "A05-A14", 
                            age != "unbekannt", gender != "unbekannt") %>%
      group_by(date) %>%
      summarize(deaths = sum(deaths)) %>%
      ungroup() %>%
      pull(deaths)
  }
  
  # build data frame for plotting
  df <- data.frame(t = rep(data$t, 3), y = c(data$C.observed, data$C.estimate, data$C.observed.finally), 
                   kind = c(rep("Observed", T.max), 
                            rep("Nowcasted", T.max),
                            rep("Observed.finally", T.max)))
  df$kind <- factor(df$kind, levels = c("Observed", 
                                        "Nowcasted",
                                        "Observed.finally"))
  df.bounds <- data.frame(t = data$t, lower = data$C.lower, upper = data$C.upper)
  
  # plot
  labels.legend <- c(paste("Observed on ", doa),
                     paste("Nowcasted on ", doa),
                     expression(paste("Finally realized ", d[max], " Days after the Registration Date")))
  shape.legend <- c(16, 17, 15)
  linetype.legend <- c(1, 3, 4)
  
  
  pdf(file = paste0("Plots/Nowcasting/Nowcast/", 
                    max(registration.dates)+days(1), ".pdf"), width = 10, height = 6) 
  g <- ggplot(df, aes(x = t)) + geom_line(aes(y = y, color = kind, linetype = kind)) + 
    geom_point(aes(y = y, color = kind, shape = kind)) +
    labs(x = "Registration Date", color = "", linetype = "", shape = "",
         y = "Count of fatal COVID-19 infections",
         title = paste0("Counts of fatal COVID-19 infections by date of registration \nbased on the RKI data reported on ", doa)) + 
    geom_ribbon(data = df.bounds, aes(ymin = lower, ymax = upper), alpha = 0.3) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1), 
          legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.995)) + 
    scale_x_continuous(breaks = seq(0, max(df$t), 2), 
                       labels = as.character(substring(registration.dates, 6))[seq(1, max(df$t)+1, 2)]) + 
    scale_y_continuous(breaks = seq(0, max(df$y), 50), 
                       sec.axis = dup_axis(name = "")) + 
    scale_color_hue(labels = labels.legend) +
    guides(colour = guide_legend(override.aes = list(shape = shape.legend, linetype = linetype.legend))) + 
    scale_shape(guide = FALSE) +
    scale_linetype(guide = FALSE)
  # add vertical lines on sundays
  for (s in which(day.infos$weekday == "Sun")) {
    g <- g + geom_vline(xintercept = s-1, linetype = "dashed")
  }
  print(g)
  dev.off()
}

# This function creates the plot of the cumulative distribution function F_t
# for several t and saves the plot in the LRZ folder

# Input:
# - time: time points t for which F_t should be plotted
# - data: a data frame which is the output of the function predict.F(...)
# - regisration.dates: dates corresponding to t = 0,...,T.max-1

# Output: none

plot.F <- function(time, data, registration.dates){
  
  # compute F_t(d) for d = 1,...d_max and given t
  d <- unique(data$d)
  F.t <- matrix(1, length(d), 2)
  
  data.t1 <- filter(data, t == time[1], age.80 == 1)
  data.t2 <- filter(data, t == time[2], age.80 == 1)
  for (i in 2:length(d)) {
    F.t[i-1, 1] <- prod(1-data.t1$pi[i:nrow(data.t1)])
    F.t[i-1, 2] <- prod(1-data.t2$pi[i:nrow(data.t2)])
  }
  
  
  # data frame for plotting
  df <- data.frame(d = rep(c(0, d), 2), 
                   F.t = c(0, F.t[, 1], 0, F.t[, 2]),
                   kind = c(rep(as.character(registration.dates[time[1]+1]), d.max+1), 
                            rep(as.character(registration.dates[time[2]+1]), d.max+1)))
  
  # plot 
  pdf(file = paste0("Plots/Nowcasting/F/", 
                    max(registration.dates)+days(1), ".pdf"), width = 6, height = 4)
  g <- ggplot(df, aes(x = d, y = F.t)) + 
    geom_step(aes(color = kind, linetype = kind), direction = "hv") + 
    geom_point(aes(color = kind), size = 0.8) +
    theme(legend.justification = c(0.01, 0.99), legend.position = c(0.01, 0.99)) + 
    scale_x_continuous(breaks = seq(0, 40, 5)) + 
    labs(y = expression(F[t](d)), color = "Registration Date t", linetype = "Registration Date t") + 
    geom_hline(yintercept = 0.5, alpha = 0.3, linetype = "dashed") 
  print(g)
  dev.off()
} 

# This function is used inside other functions and contains the main 
# commands for plotting the maps

# Input:
# - data: a data frame which contains all the information on the district borders
# as well as the  spatial effects for each district
# - type: a character vector of length one containing one of the following values
# "m_2", "u_r", "u_rt", "fitted.deaths.per100k", "u_r0", "u_r1", 
# "u_r0_u80", "u_r1_u80", "u_r0_a80", "u_r1_a80"
# - limits: a vector of length two specifying the limits of the legend
# - state_borders: a data frame which contains the state borders of Germany
# - plot_title: the title printed at the top of the map
# - legend_title: the title printed at the top of the legend
# - caption: printed below the plot title

# Output: a ggplot object

plot.map <- function(data, type, limits, date, state_borders,
                     plot_title, legend_title, caption) {
  
  # Checking of inputs:
  assert_choice(x = type, choices = c("m_2", "u_r", "u_rt", "fitted.deaths.per100k",
                                      "u_r0", "u_r1", 
                                      "u_r0_u80", "u_r1_u80", "u_r0_a80", "u_r1_a80"))
  
  # ggplot theme:
  theme <- theme_classic() +
    theme(text = element_text(size = 40), 
          legend.text = element_text(size = 40),
          plot.title = element_text(hjust = 0.5, size =40, face = "bold"),
          strip.text.y = element_text(size = 40), legend.text.align = 0,
          strip.placement = "outside", strip.background = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.key.size = unit(2.5, "cm"))
  
  fill <- sym(type)
  
  plot <- ggplot() +
    ggtitle(plot_title) +
    labs(caption = caption) +
    geom_polygon(data = data,
                 mapping = aes(x = long, y = lat, group = group,
                               fill = !!fill),
                 col = "azure1") +
    geom_polygon(data = data[data$type %in% c("Kreisfreie Stadt",
                                              "Stadtkreis"), ],
                 mapping = aes(x = long, y = lat, group = group, fill = !!fill),
                 col = "white") +
    geom_polygon(data = state_borders,
                 mapping = aes(x = long, y = lat, group = group),
                 col = "grey20", size = 0.75, fill = NA) + 
    geom_polygon(data = state_borders[state_borders$state %in% c("Berlin",
                                                                 "Bremen"), ],
                 mapping = aes(x = long, y = lat, group = group),
                 col = "grey20", fill = NA) +
    scale_fill_gradient2(legend_title, low = "steelblue", high = "firebrick2", limits = limits) +
    north(data, location = "topleft", scale = 0.15) +
    scalebar(data, transform = TRUE, dist_unit = "km", dist = 100, model = "WGS84",
             location = "topright") +
    theme 
  return(plot)
}

# This function creates the plot of the fitted deaths per district 
# and saves the result to the LRZ folder

# Input:
# - deaths.by.district: a data frame with 412 rows containing the columns 
# districtId and fitted.deaths.per100k
# - doa: day of analysis

# Output: none

plot.fitted <- function(deaths.by.district, doa, model, day.min, day.max){
  
  # district and state boundaries for plotting
  district_borders = read_rds("Data/Maps/district_borders.rds")
  state_borders = read_rds("Data/Maps/state_borders.rds")
  
  # Matching of both datasets:
  plot_data_effects <- full_join(district_borders, deaths.by.district)
  
  png(file = paste0("Plots/Fitted/", doa, ".png"), 
      width = 1200, height = 1650, units = "px") 
  print(plot.map(plot_data_effects, type = "fitted.deaths.per100k", 
                 limits = range(deaths.by.district$fitted.deaths.per100k), 
                 date = doa, state_borders = state_borders, 
                 plot_title = paste0("Nowcasted fatal infections per 100 000 inhabitants \nwith registration dates from ",
                                     model$registration.dates[day.min+1], " until ",
                                     model$registration.dates[day.max+1]),
                 legend_title = expression(widehat(lambda)[r]),
                 caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                  min(model$registration.dates), " until ", max(model$registration.dates), ".")))
  dev.off()
}

# This function creates the plots of the two spatial effects 
# included in the negative binomial model and saves the plots in the LRZ folder

# Input: 
# - doa: day of analysis
# - re: which random effects should be used?
# re = "joint" (default) performs the analysis of Section 4
# re = "sep" performs the analysis of Section 6.2
# - nowcast: which nowcasting results should be used for the offset?
# nowcast = "estimate" (default) uses the estimate
# nowcast = "lower" uses the alpha/2 quantile
# nowcast = "upper" uses the 1-alpha/2 quantile

# Output: none

plot.effects.deaths <- function(doa, re = "joint", nowcast = "estimate"){
  
  # district and state boundaries for plotting
  district_borders = read_rds("Data/Maps/district_borders.rds")
  state_borders = read_rds("Data/Maps/state_borders.rds")
  
  # get desired models
  files <- list.files(path = "Output")
  file <- files[intersect(grep(paste(doa), files), intersect(grep(paste(re), files), 
                                                             grep(paste(nowcast), files)))]
  model = read_rds(paste0("Output/", file))  
  
  # initilaize table with district effects
  districts <- preprocess.districts()
  district.effects <- tibble(district = districts$name, 
                             districtId = districts$districtId,                            
                             m_2 = 0) 
  
  # random effects
  u <- as.vector(plot.gam(model, select = 0)[[3]]$fit)
  district.effects <- switch(re,
                             "joint" = mutate(district.effects, 
                                              u_r0 = u[seq(1, 2*nrow(districts), 2)],
                                              u_r1 = u[seq(2, 2*nrow(districts), 2)]),
                             "sep" = mutate(district.effects,
                                            u_r0_u80 = u[seq(1, 4*nrow(districts), 4)],
                                            u_r1_u80 = u[seq(2, 4*nrow(districts), 4)],
                                            u_r0_a80 = u[seq(3, 4*nrow(districts), 4)],
                                            u_r1_a80 = u[seq(4, 4*nrow(districts), 4)]))
  
  # spatial effect
  pd <- plot.gam(model, select = 0)[[2]]
  fitted.matrix <- matrix(pd$fit, nrow = length(pd$y), byrow = TRUE)[length(pd$x):1, ]
  
  for (j in 1:nrow(district.effects)) {
    distance = matrix(0, length(pd$y), length(pd$x))
    for (lat in 1:nrow(distance)) {
      for (lon in 1:ncol(distance)) {
        distance[lat, lon] = sum(sqrt((districts$lat[j] - pd$y[length(pd$y) - lat + 1])^2 + 
                                        (districts$lon[j] - pd$x[lon])^2))
      }
    }
    ind = which(min(distance[which(!is.na(fitted.matrix)) ]) == distance, arr.ind = TRUE)
    district.effects$m_2[j] = fitted.matrix[ind]
  }
  
  # Matching of effects with dataset that contains the district borders
  data <- full_join(district_borders, district.effects)
  
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  # plot spatial effect
  directory <- paste0("Plots/SpatialEffect/", re)
  dir.create(directory, showWarnings = FALSE)
  
  png(file = paste0(directory, "/", doa, ".png"), 
      width = 1200, height = 1550, units = "px")
  print(plot.map(data, type = "m_2", limits = range(district.effects$m_2), 
                 date = doa, state_borders = state_borders, 
                 plot_title = "Estimates of Smooth Spatial Effect",
                 legend_title = expression(widehat(m)[2](s[r])),
                 caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                  min(model$registration.dates), " until ", max(model$registration.dates), ".")))
  dev.off()
  
  # plot random intercept
  if (re == "joint"){
    # plot random intercept u_r0
    directory <- "Plots/RandomIntercept/u_r0/joint"
    png(file = paste0(directory, "/", doa, ".png"), 
        width = 1200, height = 1550, units = "px")
    print(plot.map(data, type = "u_r0", limits = range(u), 
                   date = doa, state_borders = state_borders, 
                   plot_title = "Estimates of Long-Term Random Intercept",
                   legend_title = expression(widehat(u)[r0]),
                   caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                    min(model$registration.dates), " until ", max(model$registration.dates), ".")))
    dev.off()
    
    # plot random intercept u_r1
    directory <- "Plots/RandomIntercept/u_r1/joint"
    png(file = paste0(directory, "/", doa, ".png"), 
        width = 1200, height = 1550, units = "px")
    print(plot.map(data, type = "u_r1", limits = range(u), 
                   date = doa, state_borders = state_borders, 
                   plot_title = "Estimates of Short-Term Random Intercept",
                   legend_title = expression(widehat(u)[r1]),
                   caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                    min(model$registration.dates), " until ", max(model$registration.dates), ".")))    
    dev.off()
    
    
  } else {
    directory <- "Plots/RandomIntercept/u_r0/u80"
    png(file = paste0(directory, "/", doa, ".png"), 
        width = 1200, height = 1550, units = "px")
    print(plot.map(data, type = "u_r0_u80", limits = range(u), 
                   date = doa, state_borders = state_borders, 
                   plot_title = "Estimates of Long-Term Random Intercept (80-)",
                   legend_title = expression(widehat(u)[r0]),
                   caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                    min(model$registration.dates), " until ", max(model$registration.dates), ".")))
    dev.off() 
    
    directory <- "Plots/RandomIntercept/u_r1/u80"
    png(file = paste0(directory, "/", doa, ".png"), 
        width = 1200, height = 1550, units = "px")
    print(plot.map(data, type = "u_r1_u80", limits = range(u), 
                   date = doa, state_borders = state_borders, 
                   plot_title = "Estimates of Short-Term Random Intercept (80-)",
                   legend_title = expression(widehat(u)[r1]),
                   caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                    min(model$registration.dates), " until ", max(model$registration.dates), ".")))
    dev.off()
    
    directory <- "Plots/RandomIntercept/u_r0/a80"
    png(file = paste0(directory, "/", doa, ".png"), 
        width = 1200, height = 1550, units = "px")
    print(plot.map(data, type = "u_r0_a80", limits = range(u), 
                   date = doa, state_borders = state_borders, 
                   plot_title = "Estimates of Long-Term Random Intercept (80+)",
                   legend_title = expression(widehat(u)[r0]),
                   caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                    min(model$registration.dates), " until ", max(model$registration.dates), ".")))
    dev.off()
    
    directory <- "Plots/RandomIntercept/u_r1/a80"
    png(file = paste0(directory, "/", doa, ".png"), 
        width = 1200, height = 1550, units = "px")
    print(plot.map(data, type = "u_r1_a80", limits = range(u), 
                   date = doa, state_borders = state_borders, 
                   plot_title = "Estimates of Short-Term Random Intercept (80+)",
                   legend_title = expression(widehat(u)[r1]),
                   caption = paste0("Based on data reported up to ", doa, ".\nModel includes registration dates from\n",
                                    min(model$registration.dates), " until ", max(model$registration.dates), ".")))
    dev.off()
    
  }
}

# This function creates the time plot of the expected deaths in the reference group
# and saves the plot in the LRZ folder

# Input:
# - doa: day of analysis

# Output: none

plot.nowcasted.deaths.ref <- function(doa){
  
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  # get the required model objects
  files <- list.files(path = "Output")
  files <- files[intersect(grep(as.character(doa), files), grep("joint", files))]
  model.estimate <- read_rds(paste0("Output/estimate_joint_", 
                                    as.character(doa), ".rds"))
  model.lower <- read_rds(paste0("Output/lower_joint_", 
                                 as.character(doa), ".rds"))
  model.upper <- read_rds(paste0("Output/upper_joint_", 
                                 as.character(doa), ".rds"))
  
  # extract the intercepts for joining the curves at the beginning
  intercept.estimate <- as.numeric(model.estimate$coefficients[1])
  intercept.lower <- as.numeric(model.lower$coefficients[1])
  intercept.upper <- as.numeric(model.upper$coefficients[1]) 
  
  # data frame for plotting
  pd.estimate <- plot.gam(model.estimate, select = 0)[[1]]
  pd.lower <- plot.gam(model.lower, select = 0)[[1]]
  pd.upper <- plot.gam(model.upper, select = 0)[[1]]
  df <- data.frame(x = rep(pd.estimate$x, 3), 
                   y = c(exp(pd.estimate$fit + intercept.estimate)*100000, 
                         exp(pd.lower$fit + intercept.lower)*100000, 
                         exp(pd.upper$fit + intercept.upper)*100000),
                   kind = c(rep("estimate", length(pd.estimate$x)), 
                            rep("2.5% quantile", length(pd.lower$x)),
                            rep("97.5% quantile", length(pd.upper$x))))  
  df$kind <- factor(df$kind, levels = unique(df$kind))
  
  rate <- df$y[which(df$kind == "estimate")]
  boundaries <- data.frame(x = pd.estimate$x,
                           lower = rate - 2*sqrt(rate^2*(pd.estimate$se/pd.estimate$se.mult)^2),
                           upper = rate + 2*sqrt(rate^2*(pd.estimate$se/pd.estimate$se.mult)^2))
  
  # plot
  pdf(file = paste0("Plots/DeathsTime/", doa, ".pdf"), 
      width = 6, height = 4) 
  g <- ggplot(df, aes(x = x)) + geom_line(aes(y = y, color = kind, linetype = kind)) + 
    geom_ribbon(data = boundaries, aes(ymin = lower, ymax = upper), alpha = 0.3) +
    theme(legend.justification = c(0.99, 0.99), legend.position = c(0.99, 0.99)) + 
    scale_color_manual(values = c("black", "green", "red")) + 
    scale_x_continuous(breaks = seq(0, max(df$x), 2), 
                       labels = as.character(substring(model.estimate$registration.dates, 6))
                       [1+seq(0, max(df$x), 2)]) + 
    theme(panel.grid = element_blank(), plot.title = element_text(hjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_y_continuous(limits = c(0, max(df$y, boundaries$upper)), 
                       breaks = seq(0,  max(df$y, boundaries$upper), 0.005)) + 
    labs(x = "Registration Date", y = "Expected Deaths per 100 000 Inhabitants \nin the Reference Group", 
         color = "Nowcast", linetype = "Nowcast") 
  for (h in seq(0,  max(df$y, boundaries$upper), 0.005)) {
    g <- g + geom_hline(yintercept = h, linetype = "dashed", alpha = 0.3)
  }
  print(g)
  dev.off()
}

# This function creates the ACF plot of the residuals in the mortality model
# and saves it in the LRZ folder

# Input:
# - doa: Day of Analysis

# Output: none

plot.ACF <- function(doa){
  
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  # get the required model objects
  files <- list.files(path = "Output")
  files <- files[intersect(grep(as.character(doa), files), grep("joint", files))]
  model <- read_rds(paste0("Output/estimate_joint_", 
                           as.character(doa), ".rds"))
  
  T.max <- length(unique(model$data$day))
  
  data <- matrix(residuals.gam(model, type = "pearson"), nrow = 412*2*4, byrow = TRUE)
  ACF <- tibble(lag = 0:(T.max-1), acf = 1)
  for (k in 1:(T.max-1)) {
    ind.1 <- seq(1, T.max-k, 1)
    ind.2 <- seq(k+1, T.max, 1)
    resid.1 <- NULL
    for (i in ind.1) {
      resid.1 <- c(resid.1, data[ind.1, ])
    }
    resid.2 <- NULL
    for (i in ind.2) {
      resid.2 <- c(resid.2, data[ind.2, ])
    }
    ACF$acf[k+1] <- cor(resid.1, resid.2)
  }
  
  g <- ggplot(data = ACF, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) + 
    scale_y_continuous(limits = c(-0.25, 1), breaks = seq(-0.2, 1, 0.2)) + 
    labs(x = "Lag", y = "ACF")
  
  pdf(file = paste0("Plots/ACF/", doa, ".pdf"), 
      width = 6, height = 4) 
  print(g)
  dev.off()
} 