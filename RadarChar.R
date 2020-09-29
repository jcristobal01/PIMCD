library(dplyr)
library(ggplot2)
library(scales)
library(reshape2)
library(tibble)
library(data.table)
librdf_log <- read.csv("../Data/18-107910 Registros.csv")
if (ncol(df_log == 1)) {
  df_log <- read.csv("../Data/18-107910 Registros.csv",sep=";")
}
colnames(df_log) <- c("hora","estudiante","usuario","evento","componente","nombreEvento","descripcion",
                      "origen","ip")
# Elimino los elementos que no se registran por web
df_log <- df_log[df_log$origen == "web",]
df_log$idCV <- sapply(strsplit(as.character(df_log$descripcion),split="The user with id '",fixed=T),"[",2)
df_log$idCV <- sapply(strsplit(as.character(df_log$idCV),split="'",fixed=T),"[",1)ary(cowplot)
data(mtcars)
df = mtcars %>%
  rownames_to_column( var = "car" ) %>% 
  mutate_each(funs(rescale), -car) %>% 
  melt(id.vars=c('car'), measure.vars=colnames(mtcars)) %>% 
  arrange(car)

line_plot = df %>%
  filter(variable=='mpg') %>%
  ggplot(aes(x=car, y=value, group=1)) + 
  geom_line(color = 'purple')
print(line_plot + coord_polar())

polygon_plot = df %>% 
  filter(variable=='mpg') %>%
  ggplot(aes(x=car, y=value, group=1)) + 
  geom_polygon(color = 'purple', fill=NA)
print(polygon_plot + coord_polar())
i=4
jpeg(paste("imagenes/",df_cursos$id[i],"/006.1-",df_cursos$id[i],".jpg",sep=""),width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)
df_chart2 <- df_chart %>% rownames_to_column( var = "grupo" ) %>% 
  #        mutate_each(funs(rescale), -grupo) %>% 
  melt(id.vars=c('grupo'), measure.vars=colnames(df_chart)) %>% 
  arrange(grupo)
polygono_plot = df_chart2 %>% 
  ggplot(aes(x=grupo, y=value, group=1)) + 
  geom_polygon(color = 'purple',fill="red") +
        coord_polar()
#  annotation_custom(tableGrob(df_chart), xmin=35, xmax=50, ymin=-2.5, ymax=-1)
print(polygono_plot)
data <- structure(list(Farm = c("Best", "Worst", "User"), 
               CO2Fert = c(2.187635996, 4.240789564, 1.189743632), 
               CO2Manure = c(0.670289261, 0.503797793,2.852713125), 
               CO2Feed = c(0.773605214, 1.884548328, 0.419821994),
               CO2Housing = c(2.415801361, 5.114791701, 0.630429463), 
               CO2Fuel = c(1.180859203,3.411847194, 2.982960729), 
               CO2Other = c(2.876050311, 5.150085802, 1.489036959), 
               NO3Fert = c(2.19509301, 1.848317101, 1.643695528),
               NO3Manure = c(2.452857906, 3.153028268, 1.922113286),
               NO3Grazing = c(0.037698451, 4.452847769, 2.546101867), 
               NH4Spreading = c(2.880954824, 2.60492997, 3.186211336),
               NH4Storage = c(1.178815284, 4.893388111, 2.432823901), 
               NH4Grazing = c(0.509207305, 2.998872111, 4.444466334), 
               NH4Housing = c(2.523406518, 5.255666955, 1.287199958)),
          .Names = c("Farm", "CO2Fert", "CO2Manure", "CO2Feed", "CO2Housing", "CO2Fuel", "CO2Other", 
                     "NO3Fert", "NO3Manure", "NO3Grazing", "NH4Spreading", "NH4Storage", "NH4Grazing", 
                     "NH4Housing"), 
          row.names = c(NA, -3L), 
          class = c("data.table", "data.frame"))
dat2test <- melt.data.table(data, c("Farm"), c("CO2Fert", "CO2Manure", "CO2Feed", "CO2Fuel", "CO2Housing", "CO2Other"))
thePlot <-  ggplot(data = dat2test, aes(x=variable, y=value, group=Farm, color= Farm)) + ## Define data and colour column
  geom_polygon(fill = NA) + ## make the lines meet
  coord_polar() 
stable.p <- ggtexttable(data, rows = NULL, theme = ttheme("minimal"))
h <- ggdraw(thePlot)
h + draw_grob(ggplotGrob(stable.p), 0.6, -0.35)
dev.off()
