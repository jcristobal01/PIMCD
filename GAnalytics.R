library(googleAnalyticsR)
library(ggplot2)
#ga_auth()
#my_accounts <- ga_account_list()

#View(my_accounts)
my_id <- "115311684"
id.cv <- "19-107910"
id.moodle <- "118059"
start_date <- "2019-09-09"
end_date <- "2019-12-20"

df1 <- dim_filter("pagePath",operator = "REGEXP",expressions = "^/moodle/course/view.php")
df2 <- dim_filter("pagePath",operator = "REGEXP",expressions = "[&]",not=T)
df3 <- dim_filter("pagePath",operator = "REGEXP",expressions = paste(".*=",id.moodle,sep=""))


# Now, put that filter object into a filter clause. If you had multiple filters, there
# are two aspects of this that wouldn't seem so weird. With one... they do seem weird:
# 1. The "filters" argument takes a list. So, you have to put your filter object in a
#    list. It's a one-element list.
# 2. The "operator" argument is moot -- it can be AND or OR...but you have to have
#    it be something, even though it doesn't do anything.
my_dim_filter_clause <- filter_clause_ga4(list(df1,df2,df3),operator = "AND")

# Pull the data. See ?google_analytics_4() for additional parameters. Depending on what
# you're expecting back, you probably would want to use an "order" argument to get the
# results in descending order. But, we're keeping this example simple.
ga_data <- google_analytics(viewId = my_id,
                            date_range = c(start_date, end_date),
                            metrics = c("uniqueDimensionCombinations",
                                        "users",
                                        "uniquePageViewas",
                                        "sessions",
                                        "bounces"),
                            dimensions = c("dateHourMinute",
                                           "Country",
                                           "Region",
                                           "City",
                                           "latitude",
                                           "longitude"),
#                                           "OperatingSystem","landingPagePath","exitPagePath","previousPagePath",
#                                           "socialInteractionNetworkAction","socialEngagementType"),
                            dim_filters = my_dim_filter_clause,
                            rows_per_call = 100000,
                            samplingLevel="LARGE",
                            slow_fetch=T,
                            anti_sample=T,
                            anti_sample_batches=10)
#                            max=-1)

save(ga_data,file=paste("./Data/GA_",id.cv,".Rdata",sep=""))
ga_data$fecha <- strptime(ga_data$dateHourMinute,format="%Y%m%d%H%M")
ga_data$dia <- as.Date(ga_data$Fecha,format="%Y-%m-%d")
accFecha <- aggregate(sessions ~ dia, data=ga_data,sum)
ga_data$pagePath<-NULL
#ga_data$Users <- NULL
library(RgoogleMaps)
#library(ggmap)
#library(maps)
ga_data$Fecha <- strptime(ga_data$dateHourMinute,format="%Y%m%d%H%M")
ga_data$dateHourMinute <- NULL
counts <- as.data.frame(table(ga_data$Country,ga_data$Region,ga_data$City))
counts <- counts[counts$Visitas > 0,]
colnames(counts) <- c("Pais","Comunidad","Ciudad","Visitas")

ga_data$dia <- as.Date(ga_data$Fecha,format="%Y-%m-%d")

accFecha <- as.data.frame(table(ga_data$Dia))
sum(accFecha$Freq)
accFecha <- google_analytics(my_id, date_range = c(start_date, end_date), 
                             dim_filters = my_dim_filter_clause, 
                             metrics = c("sessions","uniquePageviews","entrances"),
                             dimensions = "date",
                             rows_per_call = 100000,
                             samplingLevel="LARGE",
                             slow_fetch=T,
                             anti_sample=T,
                             anti_sample_batches=1)
#counts <- as.data.frame(table(round(as.numeric(ga_data$latitude),2),round(as.numeric(ga_data$longitude),2)))
#counts$lat <- as.numeric(as.character(counts$Var1))
#counts$lon <- as.numeric(as.character(counts$Var2))
#counts$Var1 <- NULL
#counts$Var2 <- NULL
ga_data$lat <- round(as.numeric(ga_data$latitude),2)
ga_data$lon <- round(as.numeric(ga_data$longitude),2)
ucm_lat <- 40.448049 
ucm_lon <- -3.727710

ga_data <- ga_data[ga_data$lat > 0,]
ucm.map <- MapBackground(ga_data$lat,ga_data$lon,size = c(2048, 2048))
#legend("topleft" )
#ucm.map <- plotmap(ucm_lat,ucm_lon,zoom=4,maptype="satellite",alpha=1)
sum.ga <- table(ga_data$lat,ga_data$lon)
PlotOnStaticMap(ucm.map,as.numeric(ga_data$latitude),as.numeric(ga_data$longitude),pch=19,cex=ga_data$Users)








PlotOnStaticMap(ucm.map,ga_data$lat,ga_data$lon,pch=21,cex=ga_data$Users)
#for (i in 1:NROW(ga_data)) {
#  PlotOnStaticMap(ucm.map,lat=ucm_lat,lon=ucm_lon,add=T,lwd=1.5,col="red",
#                  points(x=ga_data$latitude[i],y=ga_data$longitude[i]))
#  print(paste(ga_data$latitude[i],ga_data$longitude[i],sep="-"))
#}
ucm.map <- GetMap(center=c(ucm_lat,ucm_lon),zoom=12,
#                  destfile = "imagenes/mapa.jpg",
#                  format="jpeg",
                  maptype = "roadmap"
                  )
PlotOnStaticMap(ucm.map)
PlotOnStaticMap(ucm.map, add, lat0=ucm_lat, lon0=ucm_lon, col = 'red')




ucm  <- geocode('Avenida Complutense, Madrid, España', 
                  source = "google")
madrid <- get_googlemap("Madrid, Spain", zoom = 8, maptype = "terrain")
ggmap (Madrid)
