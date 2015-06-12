setwd("D:/R_workspace")

columnClasses <- c(
  id="numeric",
  party="character",
  county="character",
  name="character",
  tookoffice="numeric", #date
  leftoffice="numeric", #date
  link="character",
  birthdate="numeric", #date
  location="character",
  hassentence="character",
  militaryservice="character",
  conflictingduty="character",
  othergo="character",
  othercitizenship="character",
  guiltyforsomething="character",
  birthplace="character",
  nationality="character",
  languages="character",
  mainworkplace="character",
  communityactivities="character",
  maritalstatus="character",
  spousename="character",
  spouselastname="character",
  spouseworkplace="character",
  children="character",
  childrencount="numeric",
  otherelections="character",
  otherparties="character",
  turtas="numeric",
  vertybiniai="numeric",
  pinigai="numeric",
  suteiktospaskolos="numeric",
  gautospaskolos="numeric",
  pajamos="numeric",
  mokesciai="numeric",
  runsformayor="character",
  status="character")

#Excel prepared so that only mayors are imported
library("xlsx")
mayors_data <- read.xlsx2("mayors_excel.xls", 1, colClasses=columnClasses)

head(mayors_data)
sapply(mayors_data, class)
summary(mayors_data)

mayors_data[["tookoffice"]] <- 
  as.POSIXct(mayors_data[["tookoffice"]] * (60*60*24)
             , origin="1899-12-30")
mayors_data[["leftoffice"]] <- 
  as.POSIXct(mayors_data[["leftoffice"]] * (60*60*24)
             , origin="1899-12-30")
mayors_data[["birthdate"]] <- 
  as.POSIXct(mayors_data[["birthdate"]] * (60*60*24)
             , origin="1899-12-30", tz = "Europe/Vilnius")

save(mayors_data, file="mayors_data.RData")