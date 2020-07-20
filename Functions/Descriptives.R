# This function calculates for every fatal case which is observed until the 
# date of analysis the duration between registration and reported death.
# This is stratified by age group (80-, 80+) and gender.
# The results are used to produce Figures 1 and 2 in the paper.

# Input:
# - doa: day of analysis
# - d.max: maximum duration time that is assumed, see Section 4
# Output: none

duration.time.KM = function(doa, d.max){ 
  
  # read all preprocessed data and extract the reporting date from the file name
  files <- list.files(path= paste(path.LRZ, "Data/Formatted", sep = ""))
  reporting.dates <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files)), hour = 0, tz = "GMT")
  
  # only use data up to the date of analysis
  ind <- which(reporting.dates <= doa)
  files <- files[ind]
  reporting.dates <- reporting.dates[ind]
  
  # initialize vectors where the counts of duration times are stored
  D.w.a80 <- D.m.a80 <- D.w.u80 <- D.m.u80 <- rep(0, d.max)
  
  # go through all data sets
  for (i in 1:length(files)) {
    data.RKI = read_rds(paste0(path.LRZ, "Data/Formatted/", files[i]))
    # "new_fatality" and "new_death" are two names for the same column
    if (is.element("new_fatality", names(data.RKI))){
      data.RKI <- mutate(data.RKI, newdeath = new_fatality)
    }
    # calculate the duration time for every newly reported fatal case
    data.RKI <- filter(data.RKI, newdeath == 1) %>% mutate(d = as.numeric(reporting.dates[i] - date))
    # store numbers for durations <d.max
    for (dur in 1:(d.max-1)) {
      D.w.a80[dur] <- D.w.a80[dur] + sum(data.RKI %>% filter(gender == "W", age == "A80+", d == dur) %>% pull(deaths))
      D.m.a80[dur] <- D.m.a80[dur] + sum(data.RKI %>% filter(gender == "M", age == "A80+", d == dur) %>% pull(deaths))
      D.w.u80[dur] <- D.w.u80[dur] + sum(data.RKI %>% filter(gender == "W", age != "A80+", d == dur) %>% pull(deaths))
      D.m.u80[dur] <- D.m.u80[dur] + sum(data.RKI %>% filter(gender == "M", age != "A80+", d == dur) %>% pull(deaths))
    }
    # store numbers for duration >= d.max
    D.w.a80[d.max] <- D.w.a80[d.max] + sum(data.RKI %>% filter(gender == "W", age == "A80+", d >= d.max) %>% pull(deaths))
    D.m.a80[d.max] <- D.m.a80[d.max] + sum(data.RKI %>% filter(gender == "M", age == "A80+", d >= d.max) %>% pull(deaths))
    D.w.u80[d.max] <- D.w.u80[d.max] + sum(data.RKI %>% filter(gender == "W", age != "A80+", d >= d.max) %>% pull(deaths))
    D.m.u80[d.max] <- D.m.u80[d.max] + sum(data.RKI %>% filter(gender == "M", age != "A80+", d >= d.max) %>% pull(deaths))
  } 
  
  # set theme for plotting
  theme_set(theme_bw() + theme(panel.grid = element_blank(), 
                               plot.title = element_text(hjust = 0.5)))
  
  # add vectors to a data frame for both age groups
  df.u80 <- tibble(d = as.factor(rep(1:d.max, 2)),
                   deaths = c(D.w.u80, D.m.u80),
                   kind = c(rep("Females", d.max), rep("Males", d.max)))
  df.a80 <- tibble(d = as.factor(rep(1:d.max, 2)),
                   deaths = c(D.w.a80, D.m.a80),
                   kind = c(rep("Females", d.max), rep("Males", d.max)))
  
  # get upper limit of y-axis
  lim <- max(df.a80 %>% group_by(d) %>% summarize(deaths = sum(deaths)) %>% pull(deaths))
  
  # plot the gender related counts of observed duration times by age group
  g.u80 <- ggplot(df.u80, aes(x = d, y = deaths)) + geom_bar(stat = "identity", position = "stack", aes(fill = kind)) + 
    theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99)) + 
    scale_y_continuous(limits = c(0, lim)) + 
    scale_x_discrete(breaks = c(1, seq(5, d.max, 5)), labels = as.character(c(1, seq(5, d.max-5, 5), paste0(d.max, "+")))) +  
    labs(x = "Days between registered infection and reported death", y = "Counts", fill = "Gender",
         title = "Age group 80-")
  
  g.a80 <- ggplot(df.a80, aes(x = d, y = deaths)) + geom_bar(stat = "identity", position = "stack", aes(fill = kind)) + 
    theme(legend.justification=c(0.99,0.99), legend.position=c(0.99,0.99)) + 
    scale_y_continuous(limits = c(0, lim)) + 
    scale_x_discrete(breaks = c(1, seq(5, d.max, 5)), labels = as.character(c(1, seq(5, d.max-5, 5), paste0(d.max, "+")))) +  
    labs(x = "Days between registered infection and reported death", y = "Counts", fill = "Gender",
         title = "Age group 80+")
  
  pdf(file = paste0(path.LRZ, "Plots/Descriptives/DurationTime.pdf"), height = 6, width = 12)
  g <- grid.arrange(g.u80, g.a80, nrow = 1)
  dev.off()
  
  # rearrange data on duration times to show the Kaplan-Meier estimators
  surv.df.a80 <- surv.df.u80 <- NULL
  for (d in 1:(d.max-1)) {
    surv.df.u80 <- bind_rows(surv.df.u80, tibble(d = rep(d, D.w.u80[d]), event = 1, gender = "Female"))
    surv.df.u80 <- bind_rows(surv.df.u80, tibble(d = rep(d, D.m.u80[d]), event = 1, gender = "Male"))
    surv.df.a80 <- bind_rows(surv.df.a80, tibble(d = rep(d, D.w.a80[d]), event = 1, gender = "Female"))
    surv.df.a80 <- bind_rows(surv.df.a80, tibble(d = rep(d, D.m.a80[d]), event = 1, gender = "Male"))
  }
  surv.df.u80 <- bind_rows(surv.df.u80, tibble(d = rep(d.max-1, D.w.u80[d.max]), event = 0, gender = "Female"))
  surv.df.u80 <- bind_rows(surv.df.u80, tibble(d = rep(d.max-1, D.m.u80[d.max]), event = 0, gender = "Male"))
  surv.df.a80 <- bind_rows(surv.df.a80, tibble(d = rep(d.max-1, D.w.a80[d.max]), event = 0, gender = "Female"))
  surv.df.a80 <- bind_rows(surv.df.a80, tibble(d = rep(d.max-1, D.m.a80[d.max]), event = 0, gender = "Male"))
  
  # fit Kaplan-Meier models for each age group and plot the results
  km.u80 <- survfit(formula = Surv(time = d, event = event) ~ gender,
                    type = "kaplan-meier", data = surv.df.u80)
  g.u80 <- ggsurvplot(fit = km.u80, data = surv.df.u80, surv.median.line = "hv",
                      conf.int = TRUE,
                      xlab = "Days between registered infection and reported death", 
                      ylab = "Kaplan Meier Estimator", legend = c(0.90, 0.91),
                      title = "Age Group 80-",
                      legend.title = "Gender",
                      legend.labs = c("Females", "Males"),
                      break.x.by = 5,
                      ggtheme=theme_bw() + theme(panel.grid = element_blank(), 
                                                 plot.title = element_text(hjust = 0.5)))
  
  km.a80 <- survfit(formula = Surv(time = d, event = event) ~ gender,
                    type = "kaplan-meier", data = surv.df.a80)
  g.a80 <- ggsurvplot(fit = km.a80, data = surv.df.u80, surv.median.line = "hv",
                      conf.int = TRUE,
                      xlab = "Days between registered infection and reported death", 
                      ylab = "Kaplan Meier Estimator", legend = c(0.90, 0.91),
                      title = "Age Group 80+",
                      legend.title = "Gender",
                      legend.labs = c("Females", "Males"),
                      break.x.by = 5,
                      ggtheme=theme_bw() + theme(panel.grid = element_blank(), 
                                                 plot.title = element_text(hjust = 0.5)))
  
  
  g <- arrange_ggsurvplots(x = list(g.u80, g.a80), nrow = 1, print = FALSE)
  
  pdf(file = paste0(path.LRZ, "Plots/Descriptives/KaplanMeier.pdf"), height = 6, width = 12)
  print(g)
  dev.off()
} 