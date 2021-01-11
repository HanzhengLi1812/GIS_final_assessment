library(tidyverse)
mtcars <- read_csv("./data/COVID6.csv")
view(mtcars)


library(sp)
library(rgdal)
American_polygons <- readOGR("./data/COVID.json")
class(American_polygons)
plot(American_polygons)
American_polygons@data

sd_houses <- American_polygons@data



American_cts <- inner_join(mtcars, sd_houses, by = "GEOID")

American_cts

view(American_cts)

American_cts%>%
  summary()

ggplot(data = American_cts, mapping = aes(x = Total_Case, y = Economic))+
  geom_point()+
  geom_smooth(method = "lm", se = F)

plot(American_cts$Total_Case, American_cts$Economic)

abline(lm(Economic~Total_Case, data =American_cts))

ggplot(data = American_cts)+
  geom_boxplot( mapping = aes(x = "", y = Total_Case))

boxplot(American_cts$Total_Case)
ggplot(American_cts)+
  geom_histogram(mapping = aes(x = Total_Case))

hist(American_cts$Total_Case)
view(American_cts)
#American_cts<-American_cts%>%
#  select(-GEOID)%>%
view(American_cts)  
  map(hist)


sd_trans <- American_cts%>%
  mutate(logTotal_Case = log(Total_Case+1))%>%
  mutate(logEconomic = log(Economic+1))%>%
  mutate(logPOPU = log(POPU+1))%>%
  mutate(logBachelor = log(Bachelor+1))%>%
  mutate(logtime = time)%>%
  select(-Total_Case, -Economic, -POPU, -Bachelor, -time)
view(sd_trans)

cor_mat <- cor(sd_trans%>%select(-GEOID))
view(cor_mat)
corrplot::corrplot(cor_mat,cl.pos = "b",tl.pos = "d")

# Scatter Plot
library(car)
scatterplotMatrix(~logTotal_Case+logEconomic, data = sd_trans,main="Scatter Plot")



final_model <- lm(logTotal_Case ~ logEconomic+logPOPU+logBachelor+logtime, data = sd_trans)
final_model
summary(final_model)



par(mfrow=c(2,2))
plot(final_model)

#view(sd_trans)
#view(final_model$residuals)

vif(final_model)
#Moran’s I for  residuals
final_model_dataframe <- cbind(sd_trans, residuals=final_model$residuals)
final_model_dataframe

view(American_polygons@data)
view(final_model_dataframe)

#names(American_polygons)
#names(final_model_dataframe)

#American_spdf <- merge(American_polygons, final_model_dataframe, by.x = "GEOID", by.y = 'GEOID',all.x = FALSE)
American_spdf <- merge(American_polygons, final_model_dataframe, by.x="GEOID",all.x = FALSE)

view(American_spdf)

library(latticeExtra)
spplot(American_spdf, "residuals")

library(RColorBrewer)
display.brewer.all()
col_palette <- brewer.pal(n = 7, name = "BrBG")
col_palette
spplot(American_spdf, "residuals",
       col.regions = col_palette, # The colour palette you just created
       cuts = 6, # Cuts is the number of colours - 1
       col = "transparent") # This sets the border colour, try changing to "black"


library(spdep)
American_nb <- poly2nb(American_spdf)

par(mai=c(0,0,0,0))
plot(American_spdf)
plot(American_nb, coordinates(American_spdf), col='red', lwd=2, add=TRUE)

American_listw <- nb2listw(American_nb)

print("View the neighbours for polygon 20")
American_listw$neighbours[[20]]
print("View the weights for polygon 20")
American_listw$weights[[20]]

moran.test(American_spdf$residuals, American_listw)
moran.mc(American_spdf$residuals, American_listw, 999)

lag_model = lagsarlm(logTotal_Case ~ logEconomic+logPOPU+logBachelor+logtime,
                     data = American_spdf,
                     listw = American_listw)
summary(lag_model)
American_spdf$lagResids <- residuals(lag_model)
moran.mc(American_spdf$lagResids, American_listw, 999)

