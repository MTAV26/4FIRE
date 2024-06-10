rm(list = ls())
graphics.off()
gc()

library(ncdf4)
library(fields)
library(maps)
library(maptools)

where = 'onfire'

anni = 2000:2020

## fix parameters
data(wrld_simpl)
num_ens = 25
nome_variable = 'tp'

if (where == 'mac') {
  dir_oss = '/Users/marco/Documents/dati/4DROP/'
  dir_s5 = '/Users/marco/Documents/dati/SEAS5/'
  dir_out = '/Users/marco/Documents/dati/obs/DROP/'
  load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
  load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
} else if (where == 'climax') {
  dir_oss = '/home/marco/4drop/'
  dir_out = '/home/marco/4drop/'
  dir_s5 = '/Users/marco/Documents/dati/SEAS5/'
  load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
  load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
} else if (where == 'onfire') {
  dir_oss = '/diskonfire/DROP/'
  dir_out = '/diskonfire/4FIRE/'
  dir_s5 = '/diskonfire/4FIRE/'
  load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
  load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))
}


# 
# dir_oss='C:/Users/Usuario/Dropbox/4FIRE/data/DROP/'
# dir_s5='C:/Users/Usuario/Dropbox/4FIRE/data/ECMWF/'
# dir_out ='C:/Users/Usuario/Dropbox/4FIRE/data/SEAS5/'


load(file.path(dir_oss, "lon_GPCP_1981_2017.RData"))
load(file.path(dir_oss, "lat_GPCP_1981_2017.RData"))


dates = seq(1, 4)
mesi = rep(1:12, length(anni))
start_date = 2

s5 = array(NA, dim = c(length(lon), length(lat), length(dates), length(anni), num_ens))
for (iyear in 1:length(anni)) {
  cat('Processing ', anni[iyear], 'of', length(anni), 'anni', '\n')

  for (iens in 1:num_ens) {
    # print(iens)
    # 1981/SEASONAL5.TP.19810701.0_deacc.nc
    fname <-
      paste0(
        dir_s5,
        'ECMWF-S5-prlr-',
        anni[iyear],'-',
        sprintf('%02d', start_date),'-',
        sprintf('%02d', iens - 1), '.nc'
        
      )
    s5.nc <- nc_open(fname)
    obs <- ncvar_get(s5.nc, nome_variable)

    s5[, , , iyear, iens] = obs[,,1:4]
  }

}
save(s5,
     file = paste0(
       dir_out,
       'SEASONAL5.TP.',
       sprintf('%02d', start_date),
       '_all_members.Rdata'
     ))

#dim(s5)
#image(lon, lat, s5[,,1,1,1])
#plot(wrld_simpl, add = TRUE)
