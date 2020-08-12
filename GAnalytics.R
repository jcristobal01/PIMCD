library(googleAnalyticsR)   # Conexisón con Google Analytics
library(ggplot2)            # Libreria de Gráficos
library(scales)             # Libreria para cambiar escaas en los gráficos
library(dplyr)              
#ga_auth()
#my_accounts <- ga_account_list()

#View(my_accounts)
my_id <- "115311684"
id.cv <- "19-107910"
id.moodle <- "118059"
start_date <- "2019-09-06"
end_date <- "2019-09-15"
## Calculamos las sesiones por día de todos los cursos
cargar_sesiones = function (fecha_ini,fecha_fin) {
  fecha_ini <- as.Date(fecha_ini)
  vecha_fin <- as.Date(fecha_fin)
  class(fecha_ini)
  class(fecha_fin)
  print(fecha_ini)
  print(fecha_fin)
  df1 <- dim_filter("pagePath",operator = "REGEXP",expressions = "^/moodle/course/view.php")
  df2 <- dim_filter("pagePath",operator = "REGEXP",expressions = "[&]",not=T)
  my_dim_filter_clause <- filter_clause_ga4(list(df1,df2),operator = "AND")
  df <- google_analytics(viewId = my_id,
                               date_range = c(fecha_ini, fecha_fin),
                               metrics = c("sessions"),
                               dimensions = c("pagePath","date"),
                               dim_filters = my_dim_filter_clause,
                               rows_per_call = 100000,
                               samplingLevel="LARGE",
                               slow_fetch=T,
                               anti_sample=T,
                               anti_sample_batches=1)
  #                               max=-1)
  df$curso <- data.frame(do.call('rbind', strsplit(as.character(df$pagePath),'=',fixed=TRUE)))[2]
  df <- df[df$sessions > 0,]
  df$pagePath <-NULL
  colnames(df) <- c("Fecha","Sesiones","Curso")
  df <- df[df$Curso != "null",]
  df <- df[df$Curso != "/moodle/course/view.php",]
  return (df)
}
if (file.exists("./Data/GA_Sesiones.Rdata")) { 
  load("./Data/GA_Sesiones.Rdata",verbose=F)
  if (start_date < min(ga_total$Fecha)) {
    df_temp <- cargar_sesiones(start_date,min(ga_total$Fecha)-1)
    ga_total <- ga_total %>% add_row(df_temp)
  }
  if (end_date > max(ga_total$Fecha)) {
    df_temp <- cargar_sesiones(max(ga_total$Fecha)+1,end_date)
    ga_total <- ga_total %>% add_row(df_temp)
  }
} else { 
  ga_total <- cargar_sesiones(start_date,end_date)
}
accmean <- aggregate(ga_total$Sesiones, by=list(ga_total$Fecha),FUN=mean)
colnames(accmean) <- c("Fecha", "Sesiones")
graph <- ggplot(data=accmean, aes(x=Fecha)) +
  geom_line(aes(y=Sesiones),color="red",linetype="dashed") +
  geom_line(aes(y=ga_total$Sesiones[ga_total$Curso == id.moodle]),color ="black") +
  ggtitle("Sesiones durante el curso (por Fechas)") +
  scale_x_date(breaks = date_breaks("weeks"),labels = date_format("%d/%m/%Y")) +
  theme(axis.text.x = element_text(angle=45))
print (graph)
save(ga_total,file="./Data/GA_Sesiones.Rdata")







# Change the line type
graph <- ggplot(data=df, aes(x=dose, y=len, group=1))  + 
  geom_line(linetype = "dashed") +
  geom_point()
# Change the color
ggplot(data=df, aes(x=dose, y=len, group=1)) +
  geom_line(color="red")+
  geom_point()


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
                                        "uniquePageViews",
                                        "sessions",
                                        "bounces"),
                            dimensions = c("dateHourMinute","pagePath",
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
                            anti_sample_batches=1)
#                            max=-1)
save(ga_data,file=paste("./Data/GA_",id.cv,".Rdata",sep=""))
ga_data$curso <- data.frame(do.call('rbind', strsplit(as.character(ga_data$pagePath),'=',fixed=TRUE)))[2]
ga_data$fecha <- strptime(ga_data$dateHourMinute,format="%Y%m%d%H%M")
ga_data$dia <- as.Date(ga_data$fecha,format="%Y-%m-%d")
accesos <-table(ga_data$pagePath,ga_data$dia)
accFecha <- aggregate(sessions ~ dia, data=ga_data,sum)
## He realizado cruces con Google Maps y no cuadran los datos porque GoogleMaps considera sesiones a 
## cualquier usuario que entre a Moodle aunque no acceda a ningún curso. Y considera la misma sesion aunque
## acceda a distintos cursos.
## Nosotros consideramos sesiones a cada usuario que pasa por la página de inicio del curso, pero no podemos
## distinguir que pertenezca al mismo usuario o a otro.-
## Google Maps no conserva datyos personales del usuario (id o IP)
## Ejemplo para el curso 118059 para el día 2020-09-09
##   Sesiones segun Moodle: 123
##   Sesiones segun GAnalytics: 91
##   Sesiones segin GA filtradas: 13
ga_data <- ga_data[ga_data$sessions > 0,]
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
