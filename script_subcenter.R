install.packages(c("sf", "tidyverse", "devtools"))
install.packages("spgwr")
library(spgwr)

# Importing OD database #

setwd("C:/Users/fred/Dropbox/000_subcenters/OD2007_Dados")
zonas_OD <- read_sf("zonas_OD_dados.shp")
str(zonas_OD)
zonas_OD = subset(zonas_OD, select = -c(OBJECTID, Id, Zona, NomeZona, Área_ha))
plot(zonas_OD)


# calculate density #

zonas_OD$pop_density <- zonas_OD$Pop/zonas_OD$Area_ha
zonas_OD$job_density <- zonas_OD$Empregos/zonas_OD$Area_ha

# calculate the percentage of jobs in each OD zone #

zonas_OD$job_perctg <- zonas_OD$Empregos/sum(zonas_OD$Empregos)*100

# scatterplot #

plot (zonas_OD$job_perctg, zonas_OD$job_density, main = "Scatter plot of the employment density and the proportion of employment in relation to the total of the SPMA in each OD zone", xlab = "Percentage of jobs in the ZOD in relation to SPMR", ylab = "job density")

#ploting some maps #

plot (zonas_OD["pop_density"])
plot (zonas_OD["job_density"])
