library(readxl)
library(dplyr)
library(ggplot2)
library(ggmap)
library(caTools)
library(ROCR)

#rm(bankdata)
bankdata <- read_excel("C:/DataScience/code/bank.xls")

get_bal_category <- function(bal)
{
  if (bal <= 0)
  {
    return(1)
  }
  else if (bal > 0 & bal <= 10000)
  {
    return(2)
  }
  else if (bal > 10000 & bal <= 20000)
  {
    return(3)
  }
  else if (bal > 20000 & bal <= 30000)
  {
    return(4)
  }
  else if (bal > 30000 & bal <= 40000)
  {
    return(5)
  }
  else if (bal > 40000 & bal <= 50000)
  {
    return(6)
  }
  else if (bal > 50000 & bal <= 60000)
  {
    return(7)
  }
  else if (bal > 60000 & bal <= 70000)
  {
    return(8)
  }
  else if (bal > 70000)
  {
    return(9)
  }
  
}


get_indicator <- function(ind)
{
  #if (ind == "yes")
  #{
  #  return(1)
  #}else
  #{
  #  return(0)
  #}
  
  switch(ind,
         yes = 1,
         no = 0
  )
  
  
}


get_minutes <- function(sec)
{
  return(round((sec/60),0))
}

get_age_group <- function(age)
{
  if (age <= 20)
  {
    return(1)
  }
  else if (age > 20 & age <= 30)
  {
    return(2)
  }
  else if (age > 30 & age <= 40)
  {
    return(3)
  }
  else if (age > 40 & age <= 50)
  {
    return(4)
  }  
  else if (age > 50 & age <= 60)
  {
    return(5)
  }
  else if (age > 60 & age <= 70)
  {
    return(6)
  }
  else if (age > 70 & age <= 80)
  {
    return(7)
  } 
  else if (age > 80)
  {
    return(8)
  }
}


tdf<-tbl_df(bankdata)
bankdata <- mutate(bankdata, bal_group=mapply(get_bal_category , bankdata$balance))
bankdata <- mutate(bankdata, age_group=mapply(get_age_group, bankdata$age))
bankdata <- mutate(bankdata, signup=mapply(get_indicator , bankdata$y))
bankdata <- mutate(bankdata, housing_loan=mapply(get_indicator , bankdata$housing))
bankdata <- mutate(bankdata, personal_loan=mapply(get_indicator , bankdata$loan))
bankdata <- mutate(bankdata, duration_minutes=mapply(get_minutes , bankdata$duration))
#View(bankdata)
#str(bankdata)

write.csv(bankdata, file="C:/DataScience/Projects/datacamp/bankdata_clean.csv", row.names = FALSE)

View(bankdata)



#---------------------------------------------------------------------------------------
#Sign up based on Marital Status

signup_only <- bankdata[bankdata$signup == 1, ]

signup_dist <- signup_only %>% group_by(marital) %>%
  summarise(no_rows = length(marital))


df1 <- signup_dist %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(marital = factor(marital, levels = c("single", "married", "divorced")),
         cumulative = cumsum(no_rows),
         midpoint = cumulative - no_rows / 2,
         label = paste0(marital, " ", round(no_rows / sum(no_rows) * 100, 1), "%"))


ggplot(df1, aes(x = 1, weight = no_rows, fill = marital)) +
  geom_bar(width = 1, position = "stack") +
  labs(title = "Sign up distribution based on Marital Status") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.2, y = midpoint, label = label)) +
  #theme_classic()
  #theme_minimal()
  #theme_nothing() 
  theme_void()


#--------------------------------------------------------------------------------------
job_dist <- signup_only %>% group_by(job) %>%
  summarise(no_rows = length(job))
View(job_dist)
#View(signup_dist)

df2 <- job_dist %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(job = factor(job, levels = c("unknown","unemployed","technician","student","services","self-employed","retired","management","housemaid","entrepreneur","blue-collar","admin.")),
         cumulative = cumsum(no_rows),
         midpoint = cumulative - no_rows / 2,
         label = paste0("", " ", round(no_rows / sum(no_rows) * 100, 1), "%"))

View(df2)

ggplot(df2, aes(x = 1, weight = no_rows, fill = job)) +
  geom_bar(width = 1, position = "stack") +
  labs(title = "Sign up distribution based on Job Type") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.2, y = midpoint, label = label)) +
  #theme_classic()
  #theme_minimal()
  #theme_nothing() 
  theme_void()


#--------------------------------------------------------------------------------------
edu_dist <- signup_only %>% group_by(education) %>%
  summarise(no_rows = length(education))
View(edu_dist)
#View(signup_dist)

df3 <- edu_dist %>%
  # factor levels need to be the opposite order of the cumulative sum of the values
  mutate(education = factor(education, levels = c("unknown","tertiary","secondary","primary")),
  #       mutate(education = factor(education, levels = c("primary","secondary","tertiary","unknown")),
         cumulative = cumsum(no_rows),
         midpoint = cumulative - no_rows / 2,
         label = paste0("", " ", round(no_rows / sum(no_rows) * 100, 1), "%","(",no_rows,")")
)

View(df3)
ggplot(df3, aes(x = 1, weight = no_rows, fill = education)) +
  geom_bar(width = 1, position = "stack") +
  labs(title = "Sign up distribution based on Education") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.2, y = midpoint, label = label)) +
  #theme_classic()
  #theme_minimal()
  #theme_nothing() 
  theme_void()

#---------------------------------------------------------------------------------------
#Distribution comparison based on Marital Status

ggplot(bankdata, aes(x =factor(y), fill = marital)) +
  geom_bar(position = "dodge") +
labs(title = "Signup Distribution based on Marital status") +
  scale_x_discrete(name ="Signup Yes/No")

ggplot(bankdata, aes(x =duration_minutes, fill = factor(signup))) +
  geom_bar(position = "dodge") +
  labs(title = "Signup Distribution based on Marital status") +
  scale_x_discrete(name ="Duration Minutes ")



#---------------------------------------------------------------------------------------

balance_30000 <- bankdata[bankdata$balance < 30000, ]

ggplot(balance_30000, aes(y=balance, x=duration_minutes,col=factor(signup))) +
  geom_point() +
  geom_jitter() 

  
#---------------------------------------------------------------------------------------
# Logical regression
#

t_signup <-  table(bankdata$signup)
t_signup[1]/(t_signup[1]+t_signup[2])

set.seed(22)
split=sample.split(bankdata$signup, SplitRatio = 0.12)
split
bankdataTrain=subset(bankdata, split==TRUE)
bankdataTest=subset(bankdata, split==FALSE)
View(bankdataTrain)
View(bankdataTest)
nrow(bankdataTrain) 

bankdataLog = glm(signup ~ age + balance , data=bankdataTrain, family=binomial)
summary(bankdataLog)
predictTrain=predict(bankdataLog, type="response")
summary(predictTrain)
tapply(predictTrain, bankdataTrain$signup, mean)

table(bankdataTrain$signup, predictTrain > 0.5)

ROCRpred = prediction(predictTrain,bankdataTrain$signup)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
plot(ROCRperf, colorize=TRUE)
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

predictTest=predict(bankdataLog, type="response", newdata=bankdataTest)

table(bankdataTest$signup, predictTest > 0.5)
