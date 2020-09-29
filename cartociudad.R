library(rgdal)
library(caRtociudad)
library(ggmap)
# Más o menos, Madrid dentro de la M-30
so.lat <- 40.3967656
so.lon <- -3.7300284
ne.lat <- 40.4798895
ne.lon <- -3.6705881
centro <- c(so.lat + (ne.lat - so.lat)/2,
            so.lon + (ne.lon - so.lon)/2)
mapa <- cartociudad_get_map(centro, 280)
ggmap(mapa) 

casa <- cartociudad_geocode("calle Valle del Tiétar 19, Villanueva de la Cañada, madrid")
#casa <- cartociudad_geocode("calle pinar del rio 17, Barcelona")
#casa <- cartociudad_geocode("calle Sancho Garcia,Sepulveda, Segovia")
#casa <- cartociudad_geocode("Avenida Senenca 2, madrid")
geocode <- cartociudad_geocode("calle valle del tiétar 19, Villanueva de la Cañada, Madrid")
geocode <- cartociudad_geocode("calle Sancho García 17, Sepúlveda, Segovia")
geocode <- cartociudad_geocode("Calle Segovia 16,Madrona, Segovia")
cartociudad_get_location_info(geocode$lat,geocode$lng,year=2010,info.source=c("census","cadastre","reverse"))
# Finca Madrona
geocode$lat <- 40.893845
geocode$lng <- -4.174925
# Finca Alejo Madrona
geocode$lat <- 40.893758
geocode$lng <- -4.173561
mapa_casa <- cartociudad_get_map(c(geocode$lat,geocode$lng),
                                 0.2,
                                 add.postcode.area = T,
                                 add.cadastral.layer = F,
                                 add.censal.section = F)
ggmap(mapa_casa)
cartociudad_get_location_info(41.422278,2.182607,year=2015,info.source = "reverse")
cartociudad_reverse_geocode(41.422278,2.182607)
#+ geom_point(aes(x = lon, y = lat), data = localizaciones)


# Generamos una nube de puntos entre esos límites
#bbox<- c(left=ne.lat,right=so.lat,top=so.lon,bottom=ne.lon)
#ggmap(get_stamenmap(bbox, zoom = 13))
set.seed(1234)
n <- 250
localizaciones <- data.frame(lat = runif(n, so.lat, ne.lat),
                             lon = runif(n, so.lon, ne.lon))

cartociudad_geocode("calle alameda 11, madrid")
cartociudad_geocode(province = "valencia", municipality = "alzira",
                    road_type = "calle",
                    road_name = "verge de la murta",
                    road_number = 11, zip = "46600")
cartociudad_get_location_info(40.473219, -3.7227241,
                              year = 2015,
                              info.source = "census")
cartociudad_get_location_info(40.473219, -3.7227241,
                              year = 2015,
                              info.source = "cadastre")
centro <- c(so.lat + (ne.lat - so.lat)/2,
            so.lon + (ne.lon - so.lon)/2)
mapa <- cartociudad_get_map(centro, 9)
ggmap(mapa) + geom_point(aes(x = lon, y = lat), data = localizaciones)

