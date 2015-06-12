setwd("D:/Documents/R/muni")

library("dplyr")
library("xlsx")
library("ggplot2")

party_elect <- read.xlsx2("Savivalda2015_1.xlsx", 1)
head(party_elect)

#Set up appropriate column classes for each import column. otherwise figures become incorrect.
mayor_elect_colClasses <- c(
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character",
  "character", #actually a date - fixed later
  "character",
  "character",
  "character",
  "character",
  "character",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric",
  "numeric")

mayor_elect <- read.xlsx2("Savivalda2015_2.xlsx", 1, stringsAsFactors = TRUE, colClasses=mayor_elect_colClasses)

#Change to Date
mayor_elect$Gimimo.diena <- as.Date(mayor_elect$Gimimo.diena)

#Create Name Surname for joining datafranes
mayor_elect$vardas.pavarde <- paste(mayor_elect$VARDAS, mayor_elect$PAVARDE, sep=' ') 

#Import procedure takes last row, which is empty. The following excludes last row.
mayor_elect <- filter(mayor_elect, BALSU_SK>=0)

head(mayor_elect)
tail(mayor_elect)

save(mayor_elect, file="mayor_elect.RData")
load(file = "mayor_elect.RData")

#Check import status
sapply(mayor_elect, class)
summary(mayor_elect)

#Total proper votes by round
mayor_elect %>%
  group_by(Turas) %>%
  summarise(votes = sum(BALSU_SK)) %>%
  print %>%

#Votes by round and candidate
votes_by_round <-
  mayor_elect %>%
  group_by(Turas, APYGARDA, vardas.pavarde) %>%
  summarise(votes = sum(BALSU_SK)) %>%
  filter(votes > 0, APYGARDA=="Vilniaus miesto") %>%
  arrange(desc(votes)) %>%
  print



#Filter only candidates running for mayor
mc_mayors <- filter(mc, runsformayor==1)
#Count number of unique names and surnames (427 - no problem)
summarise(mayors_data, n_distinct(name))



#Merge voting data and data about candidate
mayor_add <- merge(mayor_elect, mayors_data, by.x=c("APYGARDA", "vardas.pavarde"), by.y=c("county", "name"))

sapply(mayor_add, class)
summary(mayor_add)

mayor_add$BALSU_SK <- as.numeric(mayor_add$BALSU_SK)


votes_by_county_name <-
  mayor_add %>%
    group_by(APYGARDA, vardas.pavarde) %>%
    summarise(votes = sum(BALSU_SK)) %>%
    filter(votes > 0) %>%
    arrange(desc(votes))

qplot(APYGARDA, votes, data=votes_by_county_name)

checkVar(votes_by_county_name)
sum(votes_by_county_name$votes)
summary(mayor_elect)
str(mayor_elect)

summarise(mayor_add, n_distinct(name))

#add mayor_win parameter to mayors_data
mayor_win <- distinct(select(mayor_elect, 24, 11))
distinct(select(mayors_data, name))
mayors_data_1 <- merge(mayors_data, mayor_win, by.x=c("name"), by.y=c("vardas.pavarde"))
mayors_data_1[,38] <- as.numeric(mayors_data_1[,38])

names(mayors_data_1)
checkVar(mayors_data_1[,38])

plot(select(mayors_data_1, turtas, pajamos))
plot(select(mayors_data_1, turtas, vertybiniai, pinigai, suteiktospaskolos, gautospaskolos, pajamos, mokesciai))

mayor_reg <- lm(Ar.išrinktas.i.merus. ~ turtas + vertybiniai + pinigai + suteiktospaskolos + gautospaskolos + pajamos + mokesciai, data=mayors_data_1)
summary(mayor_reg) # show results

mayor_reg <- lm(Ar.išrinktas.i.merus. ~ turtas + pajamos, data=mayors_data_1)
summary(mayor_reg) # show results
# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(mayor_reg)



mayor_add <- merge(mayor_elect, mayors_data, by.x=c("APYGARDA", "vardas.pavarde"), by.y=c("county", "name"))


rm(mayor_add)
rm(mc)
rm(mc_mayors)
rm(temp)
rm(tempNA)
rm(v_pav)
rm(votes_by_person)
