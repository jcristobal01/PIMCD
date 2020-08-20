df_log <- read.csv("../Data/18-107910 Registros.csv")
if (ncol(df_log == 1)) {
  df_log <- read.csv("../Data/18-107910 Registros.csv",sep=";")
}
colnames(df_log) <- c("hora","estudiante","usuario","evento","componente","nombreEvento","descripcion",
                      "origen","ip")
# Elimino los elementos que no se registran por web
df_log <- df_log[df_log$origen == "web",]
df_log$idCV <- sapply(strsplit(as.character(df_log$descripcion),split="The user with id '",fixed=T),"[",2)
df_log$idCV <- sapply(strsplit(as.character(df_log$idCV),split="'",fixed=T),"[",1)
resumen <- as.data.frame(table(df_log$idCV,df_log$componente,df_log$nombreEvento))
resumen <- resumen[!resumen$Freq == 0,]
colnames(resumen) <- c("idCV","componente","evento","num")
#alumn <- subset(resumen,resumen$idCV == "315116")
estudiantes <- data.frame(unique(df_log$idCV))
colnames(estudiantes) <- "idCV"
df_accion <- read.csv("./Data/acciones_control.csv",sep=";",header=T,stringsAsFactors = F) 
contar_accion <- function(df,componente,evento,id) sum(df[df$idCV == id & df$componente == componente & df$evento == evento,"num"])
for (i in 1:nrow(df_accion)) {
  estudiantes$V1 <- lapply(as.list(estudiantes[,"idCV"]),function(x) contar_accion(resumen,df_accion$componente[i],df_accion$evento[i],x))
  names(estudiantes)[names(estudiantes) == "V1"] <- df_accion$accion[i]
}
