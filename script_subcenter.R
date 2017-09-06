library(GWmodel)
library(sf)
library(gstat)
library(tidyverse)
library(ggplot2)
library(sp)
library(spgwr)
library(McSpatial)
library(boot)
library(RANN)


setwd("C:/Users/fred/Dropbox/000_subcenters/OD2007_Dados")
zonas_OD <- read_sf("zonas_OD_dados.shp")
CBD <- read_sf("CBD_1.shp")

zonas_OD$pop_density <- zonas_OD$Pop/zonas_OD$Area_ha
zonas_OD$job_density <- zonas_OD$Empregos/zonas_OD$Area_ha
zonas_OD$job_perctg <- zonas_OD$Empregos/sum(zonas_OD$Empregos)*100


# scatterplot #

plot (zonas_OD$job_perctg, zonas_OD$job_density, main = "Scatter plot of the employment density and the proportion of employment in relation to the total of the SPMA in each OD zone", xlab = "Percentage of jobs in the ZOD in relation to SPMR", ylab = "job density")

#ploting some maps #

plot (zonas_OD["pop_density"])
plot (zonas_OD["job_density"])

# creating a dataset without zeros and the log of job density#

zonas_OD_temp <- zonas_OD %>% select(Empregos,Area_ha,job_density, pop_density) %>% filter(job_density>0) %>% mutate(job_density_log=log(job_density))

plot(zonas_OD_temp)

summary(zonas_OD_temp$job_density)
summary(zonas_OD_temp$job_density_log)

# converting to centroids #
zonas_OD_P <- st_centroid(zonas_OD_temp)
plot (zonas_OD_P)


#calculating distance to each centroid and the CBD #

zonas_OD_P$distances <- st_distance(zonas_OD_P, CBD)

# transformig into a spatial dataframe #

zonas_OD_P_sp <- as(zonas_OD_P,"Spatial")
plot(zonas_OD_P_sp)
zonas_oD_poly_df <-as(zonas_OD,"Spatial")
hist(zonas_OD_P_sp$distances)
linmod<-lm(zonas_OD_P_sp$job_density_log~zonas_OD_P_sp$distances)
plot(zonas_OD_P_sp$job_density_log~zonas_OD_P_sp$distances)
abline(linmod)

#calculating variogram for job_density #

variog <- variogram(job_density_log~1,data=zonas_OD_P_sp, cutoff = 80000)
plot(variog)

print(zonas_OD_P_sp)






# a partir daqui tenho que começar a pensar melhor no que fazer pois#

plot(zonas_OD_P_sp)
zonas_OD_P_sp$job_density_log

lwr_test<- lwr(zonas_OD_P_sp$job_density_log~zonas_OD_P_sp$distances+zonas_OD_P_sp$pop_density, bandwidth=80,kern="gauss",distance="Euclid",
    target="alldata",data=zonas_OD_P_sp)

plot(lwr_test)
?lwr
print(lwr_test)
names(lwr_test)
summary(lwr_test$yhat)
summary(lwr_test$yhat.se)

lwr_test$yhat

hist(lwr_test$ytarget.se)

hist((zonas_OD_P_sp$job_density_log - lwr_test$ytarget)/lwr_test$ytarget.se)
hist(lwr_test$ytarget.se)

?gwr

gwr_test <- gwr(zonas_OD_P_sp$job_density_log~zonas_OD_P_sp$distances+zonas_OD_P_sp$pop_density,data=zonas_OD_P_sp,bandwidth=6000,se.fit=TRUE,se.fit.CCT=TRUE)
plot(gwr_test$SDF)
gwr_test_map <- gwr_test$SDF
plot(gwr_test_map$pred)
summary(gwr_test_map$pred)

hist((zonas_OD_P_sp$job_density_log - gwr_test_map$pred)/gwr_test_map$pred.se)

hist((zonas_OD_P_sp$job_density_log - lwr_test$yhat)/lwr_test$yhat.se)

?lwr
# calculating gwr #


st_bbox(zonas_OD)# retrieving the bounding box of the polygons
grade <- SpatialGrid(GridTopology(c(275286.7,7337262.5), c(1000,1000),c(154,99)))#creating a grid 

plot(grade)
plot(zonas_oD_poly_df,add=TRUE, col=adjustcolor('navyblue', alpha.f = 0.5))
plot(zonas_OD_P_sp, add=TRUE, pch=16,col=adjustcolor('red'))

DM <- gw.dist(dp.locat = coordinates(zonas_OD_P_sp), rp.locat = coordinates(grade))# calculating the distance matrix of observations and grid points

names(zonas_OD_P_sp)

gwr_res <- gwr.basic(job_density_log~distances+pop_density, data=zonas_OD_P_sp, regression.points=grade, bw=60000, dMat=DM,kernel='gaussian')
gwr_res

names(gwr_res)
names(gwr_res$SDF)


grade_points<- coordinates(grade)
names(grade_sp)


print(grade_points)



grade_point$ID <-count(grade_points)
transform(grade_points, new.col = new.data)

grade_sp<-SpatialPointsDataFrame(grade_sp, CRS("+proj=utm +zone=23 +south +ellps=intl +units=m +no_defs"))
plot(grade_sp)
plot(zonas_OD_P_sp, add=TRUE, pch=16,col=adjustcolor('red'))

grade_sp_df <- as(grade_sp,"SpatialPointDataFrame")











gwr_pred <- gwr.predict(job_density_log~distances+pop_density, data = zonas_OD_P_sp,predictdata = grade1, bw, kernel="gaussian",adaptive=FALSE, p=2,
                                    theta=0, longlat=F,dMat1, dMat2)


?gwr.predict



