# This function reads the raw RKI data, gives suitable column names
# and saves the table as tibble
# Caution: The style of the RKI datasets has changed over time and probably
# will change again, i.e. new columns are added or columns are rearranged

# Input:
# - all: if TRUE, all raw data available are being read and saved,
# if FALSE (default), only datasets which have not been read yet
# will be read. Setting "all" to TRUE should be avoided.

# Output: none

read.RKI <- function(all = FALSE){
  
  # get all filenames and the respective dates from the original RKI datasets
  files.RKI <- list.files(path = "Data/RKI")
  dates.RKI <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files.RKI)), 
                          hour = 0, tz = "GMT")
  
  # get all filenames and the respective dates from the data that already has been preprocessed
  files.read <- list.files(path = "Data/Formatted")
  dates.read <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files.read)), 
                           hour = 0, tz = "GMT")
  
  if (!all){
    # only preprocess if data has not been preprocessed yet
    files.RKI <- files.RKI[which(!is.element(dates.RKI, dates.read))]
  }
  
  for (file in files.RKI) {
    # read RKI dataset and get reporting data
    data.RKI <- read_csv(file = paste0("Data/RKI/", file))
    reporting.date <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , file)), 
                                 hour = 0, tz = "GMT")
    
    # number of columns in the RKI dataset
    cols <- as.character(ncol(data.RKI))
    
    # label columns names depending on cols
    names(data.RKI) <- switch(cols,
                              "18" = c("id", "landId","land","district","age","gender",
                                       "cases","deaths","date","districtId",
                                       "updated","newcase","newdeath","date.desease",
                                       "cured","newcure", "is.beginning.desease", "age2"),
                              "17" =  c("landId","land","district","age","gender",
                                        "cases","deaths","id","date","districtId",
                                        "updated","newcase","newdeath","date.desease",
                                        "cured","newcure", "is.beginning.desease"),
                              "14" = c("landId","land","district","age","gender",
                                       "cases","deaths","id","date","districtId",
                                       "updated","newcase","newdeath","date.desease"))
    
    # save prepocessed dataset
    saveRDS(data.RKI, file = paste0("Data/Formatted/cases_GermanTemporal_", 
                                    as.character(reporting.date), ".rds"))
  }
}

# This function formats the RKI data read in by read.RKI() to have the data in a
# common style

# Input: none

# Output: none

format.RKI <- function(){
  
  files.preprocessed <- list.files(path = "Data/Formatted")
  dates.preprocessed <- as.POSIXct(sub("\\..*" , "", sub("^[^2]*", "" , files.preprocessed)), 
                                   hour = 0, tz = "GMT")
  
  for (file in files.preprocessed) {
    data <- as_tibble(read_rds(paste0("Data/Formatted/", file))) %>% 
      mutate(gender = as.character(gender))
    
    # change data format of districtId from character to numeric
    if (is.character(data$districtId)){
      data$districtId <- as.numeric(data$districtId)
    }
    
    # 3152 is not a districts any more
    if (length(which(data$districtId == 3152)) > 0) {
      data.RKI <- data.RKI[-which(data.RKI$districtId == 3152), ]
    }
    
    # if date is not in desired format
    if (!is.element("POSIXct", class(data$date))){
      data <- data %>% mutate(date = as.POSIXct(x = date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d"),
                                                tz = "UTC")) %>% 
        mutate(date = as_datetime(x = date))
    }
    
    # rename age variable of necessary
    if (is.element("age_group", names(data))){
      data <- rename(data, age = age_group) %>% mutate(age = as.character(age))
    }
    
    saveRDS(data, file = paste0("Data/Formatted/", file))
  }
}

# This function creates a data frame that with one row for each district. 
# The columns contain 
# - the district names and Ids, 
# - the gender/age group specific population sizes
# - the coordinates of the centroids of the districts
# - the population density of the districts (not used in the analyses)

# Input: none

# Output: the data frame described above

preprocess.districts <- function(){
  
  # read population and coordinates of districts
  coordinates <- read_excel("Data/Demographics/coordinates.xlsx")
  population <- read_excel("Data/Demographics/population.xlsx")
  pop.density <- read_excel("Data/Demographics/population_total.xlsx")
  
  districts <- tibble(districtId = as.numeric(population$districtId[seq(1, nrow(population), 2)]),
                      pop = round(population$gesamt[seq(1, nrow(population), 2)]), 
                      pop.m = round(population$gesamt[seq(2, nrow(population), 2)]), 
                      pop.f = pop - pop.m,
                      pop.m.0.4 = round(rowSums(population[seq(2, nrow(population), 2), 5:9])),
                      pop.w.0.4 = round(rowSums(population[seq(1, nrow(population), 2), 5:9])) -
                        round(rowSums(population[seq(2, nrow(population), 2), 5:9])),
                      pop.m.5.14 = round(rowSums(population[seq(2, nrow(population), 2), 10:19])),
                      pop.w.5.14 = round(rowSums(population[seq(1, nrow(population), 2), 10:19])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 10:19])),
                      pop.m.15.34 = round(rowSums(population[seq(2, nrow(population), 2), 20:39])),
                      pop.w.15.34 = round(rowSums(population[seq(1, nrow(population), 2), 20:39])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 20:39])),
                      pop.m.35.59 = round(rowSums(population[seq(2, nrow(population), 2), 40:64])),
                      pop.w.35.59 = round(rowSums(population[seq(1, nrow(population), 2), 40:64])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 40:64])),
                      pop.m.60.79 = round(rowSums(population[seq(2, nrow(population), 2), 65:84])),
                      pop.w.60.79 = round(rowSums(population[seq(1, nrow(population), 2), 65:84])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 65:84])),
                      pop.m.80 = round(rowSums(population[seq(2, nrow(population), 2), 85:95])),
                      pop.w.80 = round(rowSums(population[seq(1, nrow(population), 2), 85:95])) - 
                        round(rowSums(population[seq(2, nrow(population), 2), 85:95]))) 
  
  # add coordinate information
  districts <- districts[order(districts$districtId), ] %>% 
    mutate(name = coordinates$name, 
           lon = as.numeric(coordinates$longitude), 
           lat = as.numeric(coordinates$latitude), 
           density = pop.density$perkm2)
  
  return(districts)
}