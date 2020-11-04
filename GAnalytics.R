library(googleAnalyticsR)   # Conexisón con Google Analytics
library(gridExtra)
library(cowplot)
library(ggplot2)            # Libreria de Gráficos
library(scales)             # Libreria para cambiar escaas en los gráficos
library(dplyr)              

#ga_auth()
#my_accounts <- ga_account_list()

#View(my_accounts)
my_id <- "115311684" #backup
#my_id <- "83251795"
id.cv <- "19-107910"
df_log <- read.csv ("../Data/19-107910 Registros.csv",header=T,sep=";")
df_log <- df_log[df_log$origen == "web",]
df_log$idCV <- sapply(strsplit(as.character(df_log$descripcion),split="The user with id '",fixed=T),"[",2)
df_log$idCV <- sapply(strsplit(as.character(df_log$idCV),split="'",fixed=T),"[",1)


id.moodle <- "118059"
start_date <- "2019-09-05"
end_date <- "2019-12-20"
## Calculamos las sesiones por día de todos los cursos
cargar_sesiones_global = function (fecha_ini,fecha_fin) {
  df1 <- dim_filter("pagePath",operator = "REGEXP",expressions = "^/moodle/course/view.php")
  df2 <- dim_filter("pagePath",operator = "REGEXP",expressions = "[&]",not=T)
  my_dim_filter_clause <- filter_clause_ga4(list(df1,df2),operator = "AND")
  df <- google_analytics(viewId = my_id,
                               date_range = c(as.Date(fecha_ini), as.Date(fecha_fin)),
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
cargar_sesiones_global = function (fecha_ini,fecha_fin) {
  df1 <- dim_filter("pagePath",operator = "REGEXP",expressions = "^/moodle/course/view.php")
  df2 <- dim_filter("pagePath",operator = "REGEXP",expressions = "[&]",not=T)
  my_dim_filter_clause <- filter_clause_ga4(list(df1,df2),operator = "AND")
  df <- google_analytics(viewId = my_id,
                         date_range = c(as.Date(fecha_ini), as.Date(fecha_fin)),
                         metrics = c("sessions"),
                         dimensions = c("pagePath","date"),
                         dim_filters = my_dim_filter_clause,
                         rows_per_call = 100000,
                         samplingLevel="LARGE",
                         slow_fetch=T,
                         anti_sample=T,
                         anti_sample_batches=1)
  #                               max=-1)
  df <- df[grep("^/moodle/course/view.php\\?id\\=",df$pagePath),]
  df$curso <- data.frame(do.call('rbind', strsplit(as.character(df$pagePath),'=',fixed=TRUE)))[2]
  df <- df[df$sessions > 0,]
  df$pagePath <-NULL
  colnames(df) <- c("Fecha","Sesiones","Curso")
  df <- df[df$Curso != "null",]
  df <- df[df$Curso != "/moodle/course/view.php?id",]
  return (df)
}
fich <- "../Data/GA_Sesiones_Global.Rdata"
if (file.exists(fich)) { 
  load(fich,verbose=F)
  if (start_date < min(ga_total$Fecha)) {
    df_temp <- cargar_sesiones_global(start_date,min(ga_total$Fecha)-1)
    ga_total <- ga_total %>% add_row(df_temp)
  }
  if (end_date > max(ga_total$Fecha)) {
    df_temp <- cargar_sesiones_global(max(ga_total$Fecha)+1,end_date)
    ga_total <- ga_total %>% add_row(df_temp)
  }
} else { 
  ga_total <- cargar_sesiones_global(start_date,end_date)
}  
accmean <- aggregate(ga_total$Sesiones, by=list(ga_total$Fecha),FUN=mean)
colnames(accmean) <- c("Fecha", "Sesiones")
accmean$data <- "CVirtual"
accmean <- accmean[accmean$Fecha >= start_date & accmean$Fecha <= end_date,]
#thePlot <- ggplot(data=accmean, aes(x=Fecha)) +
#  geom_line(aes(y=CVirtual),color="red",linetype="dashed") +
##  geom_line(aes(y=ga_total$Sesiones[ga_total$Curso == id.moodle]),color ="black") +
#  ggtitle("Media de Sesiones por espacio durante el curso") +
#  scale_x_date(breaks = date_breaks("2 weeks"),labels = date_format("%d/%m/%Y")) +
#  theme(axis.text.x = element_text(angle=45))
#print(thePlot)
save(ga_total,file=fich)

#######################################
## Consulta de accesos para el curso ##
#######################################
cargar_sesiones_curso = function (curso,fecha_ini,fecha_fin) {
  fecha_ini <- "2019-09-09"
  fecha_fin <- "2019-12-20"
  df1 <- dim_filter("pagePath",operator = "REGEXP",expressions = "^/moodle/course/view.php")
  df2 <- dim_filter("pagePath",operator = "REGEXP",expressions = "[&]",not=T)
  df3 <- dim_filter("pagePath",operator = "REGEXP",expressions = paste(".*=",curso,sep=""))
  my_dim_filter_clause <- filter_clause_ga4(list(df1,df2,df3),operator = "AND")
  df <- google_analytics(viewId = my_id,
                         date_range = c(as.Date(fecha_ini), as.Date(fecha_fin)),
                         metrics = c("sessions"),
#                                     "uniqueDimensionCombinations",
#                                     "users",
#                                     "uniquePageViews",
#                                     "bounces"),
                         dimensions = c("dateHourMinute",
                                        "pagePath",
                                        "Country",
                                        "Region",
                                        "City",
                                        "latitude",
                                        "longitude"),
                         dim_filters = my_dim_filter_clause,
                         rows_per_call = 100000,
                         samplingLevel="LARGE",
                         slow_fetch=T,
                         anti_sample=T,
                         anti_sample_batches=1)
  df <- df[df$sessions > 0,]
  df$curso <- data.frame(do.call('rbind', strsplit(as.character(df$pagePath),'=',fixed=TRUE)))[2]
  df$fecha <- strptime(df$dateHourMinute,format="%Y%m%d%H%M")
  df$dia <- as.Date(df$fecha,format="%Y-%m-%d")
  df$dateHourMinute <- NULL
  df$pagePath <- NULL
##  colnames(df) <- c("Fecha","Sesiones","Curso")
##  df <- df[df$Curso != "null",]
##  df <- df[df$Curso != "/moodle/course/view.php",]
  return (df)
}
curso_file <- paste("../Data/GA_Sesiones_",id.cv,".RData",sep="")
if (file.exists(curso_file)) { 
  load(curso_file,verbose=F)
  if (start_date < min(ga_data$fecha)) {
    df_temp <- cargar_sesiones_curso(id.moodle,start_date,min(ga_data$fecha)-1)
    ga_data <- ga_data %>% add_row(df_temp)
  }
  if (end_date > max(ga_total$fecha)) {
    df_temp <- cargar_sesiones_curso(id.moodle,max(ga_data$fecha)+1,end_date)
    ga_data <- ga_data %>% add_row(df_temp)
  }
} else { 
  ga_data <- cargar_sesiones_curso(id.moodle,start_date,end_date)
} 

accFecha <- aggregate(sessions ~ dia, data=ga_data,sum)
accFecha <- accFecha[accFecha$dia >= start_date & accFecha$dia <= end_date,]
colnames(accFecha) <- c("Fecha","Sesiones")
accFecha$data <- id.cv
accTotal <- rbind.data.frame(accmean,accFecha)
#accTotal <- merge (accmean,accFecha, by="Fecha")
thePlot <- ggplot(data=accTotal, aes(x=Fecha,y=Sesiones)) +
  geom_line(aes(colour=data),size=2) +
  geom_point(colour="black",size=3) +
#  geom_line(aes(y=Sesiones),color="red",linetype="dashed") +
#  geom_line(aes(y=Curso),color="blue",linetype="solid") + 
  ggtitle("Media de Sesiones por espacio durante el curso") +
  scale_x_date(breaks = date_breaks("2 weeks"),labels = date_format("%d/%m/%Y")) +
  theme(axis.text.x = element_text(angle=45))
print(thePlot)
save(ga_data,file=curso_file)

library(caRtociudad)
library(ggmap)
#casa <- cartociudad_geocode("calle Valle del Tiétar, Villanueva de la Cañada, madrid")
#casa <- cartociudad_geocode("calle Sancho Garcia,Sepulveda, Segovia")
#casa <- cartociudad_geocode("Avenida Senenca 2, madrid")
casa <- cartociudad_geocode("Puerta del Sol, Madrid")
#casa <- cartociudad_geocode("Carrion de los condes, Madrid")
mapa_casa <- cartociudad_get_map(c(casa$lat,casa$lng),
                                 1200,   # 100-Madrid, 1200-España, 5000-Europa
                                 add.postcode.area = T,
                                 add.cadastral.layer = F,
                                 add.censal.section = F)
#ggmap(mapa_casa) + geom_point(aes(x = as.numeric(longitude), y = as.numeric(latitude)), size=5, data = ga_data)
#accMuni <- aggregate(sessions ~ Country + Region + City, data=ga_data,FUN=sum)
accMuni <- aggregate(sessions ~ latitude + longitude + Country + Region + City, data=ga_data,FUN=sum)
accMuni <- accMuni[order(-accMuni$sessions),]
#library(data.table)
#setDT(accMuni)[setDT(accMuni), (City) := mget(Region, on = "(not set)"]
#accMuni$City <- accMuni[accMuni$City == "(not set)","Region"]
accMuni2 <- accMuni[-1,]
thePlot <- ggmap(mapa_casa) + geom_point(aes(x = as.numeric(longitude), y = as.numeric(latitude)), 
                                         size=2, data = accMuni2)
h <- grid.arrange(ggdraw(thePlot),tableGrob(accMuni[1:20,-(1:4)],rows=NULL),nrow=1) 


library(RgoogleMaps)
#library(ggmap)
#library(maps)
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

