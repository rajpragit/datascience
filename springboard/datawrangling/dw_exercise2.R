library(dplyr)
library(readxl)
library(xlsx)

#titanic <-read_excel("C:/DataScience/Code/titanic3.xls")
titanic <-read_excel("C:/DataScience/Code/titanic.xls")
tdf<-tbl_df(titanic)
View(titanic)
fix_embark_column <- function(tdf) {
  tdf$embarked[is.na(tdf$embarked)] <- 'S'
  return(tdf)
}

tdf<-fix_embark_column(tdf)

fix_age_boat_column <- function(tdf, age) {
  tdf$age[is.na(tdf$age)] <- age
  tdf$boat[is.na(tdf$boat)] <- "None"
  return(tdf)
}

mean_age <- mean(tdf$age, na.rm=T)

tdf<-fix_age_boat_column(tdf, mean_age)

fix_missing_cabin <- function(cabin) {
  if (is.na(cabin)) {
    return(0)
  } else {
    return(1)
  }
}

tdf<-mutate(tdf, has_cabin_number=mapply(fix_missing_cabin, tdf$cabin))

View(tdf)
write.csv(tdf, file="C:/DataScience/Code/titanic_clean.csv", row.names = FALSE)

rm(tdf)