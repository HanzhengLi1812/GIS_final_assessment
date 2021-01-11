library(tidyverse)
COVID_filedata<- read_csv("./data/COVID3.csv")#read the data
view(COVID_data)#view the data
COVID_data1 <- COVID_filedata %>%
  select(Total_Case, Economic, POPU, Bachelor, time,GEOID)#


COVID_data1%>%summary()#summary data
datalot(COVID_data1)+
  geom_histogram(mapping = aes(x = Total_Case)) #analyze the distribution of the data

COVID_data2 <- COVID_filedata%>%
  mutate(logTotal_Case = log(Total_Case+1))%>%
  mutate(logEconomic = log(Economic+1))%>%
  mutate(logPOPU = log(POPU+1))%>%
  mutate(logBachelor = log(Bachelor+1))%>%
  mutate(logtime = time)%>%
  select(-Total_Case, -Economic, -POPU, -Bachelor, -time)#make a logarithmic operation here in order to make data process easier, because the the value of data is large

model_1 <- lm(logTotal_Case~  logEconomic+logPOPU+logBachelor+logtime, data = COVID_data2)#alalyze model
summary(model_1)#summary model


par(mfrow=c(2,2))#Analyze model accuracy
plot(model_1)

hist(model_1$residuals)
library(car)

