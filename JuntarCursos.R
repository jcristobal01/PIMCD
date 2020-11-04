curso <- c("18-93062","19-93062")
length(curso) 
df_alumTotal <- data.frame()
df_caliTotal <- data.frame()
df_logTotal <- data.frame()
for (i in 1:length(curso)) {
  fichalum <- paste("../Data/", curso[i]," Alumnos.csv",sep="")
  df_alu <- read.csv(fichalum,fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)
  names(df_alu)<- c("Mat.","Conv.","Sexo","Apellidos","Nombre","DNI","email","Tlfn.")
  df_alumTotal <- rbind(df_alumTotal, df_alu)
  fichcali <-  paste("../Data/", curso[i]," Calificaciones.csv",sep="")
  df_cali <- read.csv(fichcali,fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)
  switch (curso[i],
          "18-93062" = { df_cali <- df_cali[,1:8] },
          "19-93062" = { df_cali <- df_cali[,c(1:7,15)] }
  ) 
  names(df_cali) <- c("Nombre","Apellido(s)","[[id]]","Id. UCM","DNI/NIF","Dirección de correo","Actas Junio","Actas Julio")
  df_caliTotal <- rbind(df_caliTotal, df_cali)
  fichlog <-  paste("../Data/", curso[i]," Registros.csv",sep="")
  df_log <- read.csv(fichlog,fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)
  names(df_log) <- c("Hora","Nombre.completo.del.usuario","Usuario.afectado","Contexto.del.evento","Componente",
                     "Nombre.evento","Descripción","Origen","Dirección.IP")
  df_logTotal <- rbind(df_logTotal, df_log)
}
write.csv (df_alumTotal,file="../Data/XX-Teoria Alumnos.csv")
write.csv (df_caliTotal,file="../Data/XX-Teoria Calificaciones.csv")
write.csv (df_logTotal,file="../Data/XX-Teoria Registros.csv")

curso <- c("seminario-invest-5914-19","seminario3262-3","17-190652")
length(curso) 
df_alumTotal <- data.frame()
df_caliTotal <- data.frame()
df_logTotal <- data.frame()
for (i in 1:length(curso)) {
  fichalum <- paste("../Data/", curso[i]," Alumnos.csv",sep="")
  df_alu <- read.csv(fichalum,fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)
  names(df_alu)<- c("Mat.","Conv.","Sexo","Apellidos","Nombre","DNI","email","Tlfn.")
  df_alumTotal <- rbind(df_alumTotal, df_alu)
  fichcali <-  paste("../Data/", curso[i]," Calificaciones.csv",sep="")
  df_cali <- read.csv(fichcali,fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)
  switch (curso[i],
          "seminario-invest-5914-21" = { df_cali <- df_cali[,c(1:6,13)] },
          "seminario-invest-5914-19" = { df_cali <- df_cali[,1:7] },
          "seminario3262-3"          = { df_cali <- df_cali[,c(1:6,9)] },          
          "17-190652" = { df_cali <- df_cali[,c(1:6,14)] },
  )
  names(df_cali) <- c("Nombre","Apellido(s)","[[id]]","Id. UCM","DNI/NIF","Dirección de correo","NOTA FINAL DE PRÁCTICAS (Real)")
  df_caliTotal <- rbind(df_caliTotal, df_cali)
  fichlog <-  paste("../Data/", curso[i]," Registros.csv",sep="")
  df_log <- read.csv(fichlog,fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)
  names(df_log) <- c("Hora","Nombre.completo.del.usuario","Usuario.afectado","Contexto.del.evento","Componente",
                     "Nombre.evento","Descripción","Origen","Dirección.IP")
  df_logTotal <- rbind(df_logTotal, df_log)
}
write.csv (df_alumTotal,file="../Data/XX-Practicas Alumnos.csv")
write.csv (df_caliTotal,file="../Data/XX-Practicas Calificaciones.csv")
write.csv (df_logTotal,file="../Data/XX-Practicas Registros.csv")  
