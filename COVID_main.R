library(sf)
library(tmap)
library(spdep)
library(tidyverse) 
library(viridis) 
library(corrplot) 
library(Hmisc) 
library(PerformanceAnalytics) 
library(spgwr) 
library(car) 
library(tidyverse)


## -----------------------------------------------------------------------------
usa <- st_read('./USA_adm1_51/USA_adm1_51.shp')
data <- read_csv('./data/COVID3.csv')
shp <- merge(usa,data,by.x='NAME_1',by.y='name')
shp.point <- st_centroid(shp)
ggplot(shp,) +
  geom_sf(aes(fill=Total_Case))


## -----------------------------------------------------------------------------
df <- shp %>% select(Total_Case, Economic, POPU, Bachelor, time) %>% st_drop_geometry()
chart.Correlation(df, histogram=TRUE, pch=19, method = 'pearson') #see the correlation



## -----------------------------------------------------------------------------
model <- lm(Total_Case ~ Economic + POPU + Bachelor + time, data = df) #OLS model
summary(model)


## -----------------------------------------------------------------------------
durbinWatsonTest(model) #Autocorrelation
shp$ols_residuals <- residuals(model)
shp.point$ols_residuals <- residuals(model)
ggplot(data=shp) +
  geom_sf(aes(fill=ols_residuals))


## -----------------------------------------------------------------------------
set.ZeroPolicyOption(TRUE)
coords <- coordinates(as(shp.point,'Spatial'))
knn_wards <- knearneigh(shp.point, k=5)#nearest 
LWard_knn <- knn2nb(knn_wards)
plot(LWard_knn, coords, col="red")


## -----------------------------------------------------------------------------
Lward.knn_5_weight <- nb2listw(LWard_knn, style="C")
moran.test(shp.point$ols_residuals, Lward.knn_5_weight)


## ----Getis-Ord Gi* (z scores)-------------------------------------------------
lg1 <- localG(shp.point$Total_Case, listw=Lward.knn_5_weight, zero.policy=T)
shp$lg1 <- lg1[]
ggplot(shp) +
  geom_sf(aes(fill=lg1))



## -----------------------------------------------------------------------------
shp.sp <- as(shp.point, 'Spatial')
GWRbandwidth <- gwr.sel(Total_Case ~ Economic + POPU + Bachelor + time, data=shp.sp, adapt=T, method="cv") #calculate kernel bandwidth
GWRModel <- gwr(Total_Case ~ Economic + POPU + Bachelor + time, data=shp.sp, adapt=GWRbandwidth,
                hatmatrix = TRUE,se.fit = TRUE) #run the gwr model
GWRModel #print the results of the model


## -----------------------------------------------------------------------------
results<-as.data.frame(GWRModel$SDF)
names(results)
shp$pred <- results$pred  #GWR prediction result
ggplot(shp) + 
  geom_sf(aes(fill=pred))

## -----------------------------------------------------------------------------
shp$gwr.error <- results$gwr.e  #GWR prediction error
ggplot(shp) + 
  geom_sf(aes(fill=gwr.error))

