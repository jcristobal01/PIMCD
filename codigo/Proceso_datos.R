for (i in 1:NumCursos) {
  #  i = 1
  ### Archivo maestro de Alumnos (GEA)
  df_grade <- df_grades[df_grades$Curso==i,] 
  #      read.csv("../Data/18-473738 Alumnos.csv")
  if (file.exists(as.vector(df_cursos$resourcesfile[i]))) {
    df_recurso     <- read.csv(as.vector(df_cursos$resourcesfile[i]),fileEncoding="utf-8",check.names=FALSE,header=T,sep=";",stringsAsFactors=FALSE)
    #    colnames(df_alum) <- c("Matrícula","Convocatoria","Sexo","Apellidos","Nombre","NIF","Correo","Teléfono")
  } else {df_recurso <- data.frame()}
  #      read.csv(as.vector(df_cursos$studentsfile[2]))
  if (file.exists(as.vector(df_cursos$studentsfile[i]))) {
    df_alum     <- read.csv(as.vector(df_cursos$studentsfile[i]),fileEncoding="utf-8",check.names=FALSE,header=T,sep=";",stringsAsFactors=FALSE)
    if (ncol(df_alum) == 1) {df_alum <-  read.csv(as.vector(df_cursos$studentsfile[i]),fileEncoding="utf-8",
                                                  check.names=FALSE,header=T,sep=",",stringsAsFactors=FALSE)} 
    if (!"PROCEDENCIA"%in%colnames(df_alum)){
      df_alum$PROCEDENCIA <-""
      df_alum <- df_alum[,c(1,2,9,3:8)] # reordenamos columnas
    }
    colnames(df_alum) <- c("Matrícula","Convocatoria","Procedencia","Sexo","Apellidos","Nombre","NIF","Correo","Teléfono")
    df_alum$DNI <- as.numeric(substring(df_alum$NIF,1,8))      # Eliminamos ceros por la izquiera y Letra NIF
    df_alum$DNI[is.na(df_alum$DNI)] <- df_alum$NIF[is.na(df_alum$DNI)]
    df_alum$Teléfono <- as.character(df_alum$Teléfono)
  }
  ### Archivo de Calificaciones (Moodle)
  if (file.exists(as.vector(df_cursos$studentsfile[i]))) {  
    df_nota    <- read.csv(as.vector(df_cursos$gradesfile[i]),fileEncoding="utf-8",check.names=FALSE,header=T,sep=";",stringsAsFactors=FALSE)      # Calificaciones de los alumnos
    if (ncol(df_nota) < 6) {df_nota <- read.csv(as.vector(df_cursos$gradesfile[i]),fileEncoding="utf-8",check.names=FALSE,header=T,sep=",",stringsAsFactors=FALSE)} 
    colnames(df_nota)[1:6] <- c("Nombre","Apellido(s)","[[id]]","Id. UsuarioUCM.","NIF","Dirección de correo")
    df_nota$NombreEntero <- paste(df_nota$Nombre,df_nota$`Apellido(s)`,sep=" ")
    #quitamos la letra del NIF
    df_nota$DNI <- as.numeric(substring(df_nota$NIF,1,8))      # Eliminamos ceros por la izquiera y Letra NIF
    df_nota$DNI[is.na(df_nota$DNI)] <- df_nota$NIF[is.na(df_nota$DNI)]
    #        for (j in 1:NROW(df_nota)) {
    #          letra <- substr(df_nota[j,"NIF"],str_length(df_nota$NIF[j]),str_length(df_nota$NIF[j]))
    #          if (letra >= 0 & letra <=9) {
    #            df_nota[j,"DNI"] <- df_nota[j,"NIF"]
    #          } else {
    #            df_nota[j,"DNI"] <- substr(df_nota[j,"NIF"],1,str_length(df_nota$NIF[j])-1)
    #          }
    #          }
    df_nota1   <- df_nota[,c(grep("DNI",colnames(df_nota)),which(names(df_nota)%in%unlist(df_grade$Nombre)))]
    df_nota    <- cbind(df_nota$NombreEntero,df_nota$"[[id]]",df_nota$"Dirección de correo",df_nota1)
    #    colnames(df_nota)[1:2] <- c("IdCV","email")
    df_nota1   <- df_nota[,c(1:4)]
    df_cursos$NumNotas[i] <- NROW(df_grade$Nombre)
    # Las dos primeras calificaciones son las notas principales
    for (j in 1:df_cursos$NumNotas[i]) {
      if (nchar(as.vector(df_grade$Nombre[j])) > 0) {
        df_notax  <- as.data.frame(df_nota[,which(names(df_nota)%in%df_grade$Nombre[j])]) 
      } else {df_notax <- as.data.frame("-")}
      df_notax <- as.data.frame(df_notax[as.numeric(as.character(df_notax[,1])) >= 0 | df_notax[,1] == 'NP',])
      #      df_nota1  <- cbind(df_nota1,as.numeric(as.character(df_notax[,1])))
      df_nota1  <- cbind(df_nota1,as.character(df_notax[,1]))
    }
    df_nota    <- cbind(df_nota1,data.frame(matrix(ncol=(MAX_GRADES_COL-df_cursos$NumNotas[i]))))
    colnames(df_nota) <- c("NombreEntero","IdCV","email","DNI","Main1","Main2","Nota1","Nota2","Nota3","Nota4","Nota5","Nota6","Nota7","Nota8","Nota9","Nota10",
                           "Nota11","Nota12","Nota13","Nota14","Nota15","Nota16","Nota17","Nota18","Nota19","Nota20",
                           "Nota21","Nota22","Nota23","Nota24","Nota25","Nota26","Nota27","Nota28")
    df_nota$NombreEntero <- gsub("^ ","",toupper(df_nota$NombreEntero))
  }  
  df_alum <- merge(x = df_alum, y = df_nota, by = "DNI", all.x  = TRUE)
  ### Calculamos la Nota Final del Curso --------------------------------------
  df_tmp <- df_alum[,c("DNI","Main1","Main2")]
  df_tmp$Main1 <- as.numeric(as.character(df_tmp$Main1))
  df_tmp$Main2 <- as.numeric(as.character(df_tmp$Main2))
  df_tmp$Final_T <- pmax(df_tmp$Main1,df_tmp$Main2,na.rm=T)
  df_alum$Final_T <- df_tmp$Final_T
  #  df_alum$Final_T <- pmax(df_alum$Main1,df_alum$Main2,na.rm=TRUE)
  df_alum$Final_T[df_alum$Main1 == "NP" & df_alum$Main2 == "NP"]<-0
  df_alum$Final_T <- as.numeric(as.character(df_alum$Final_T))
  if (file.exists(as.vector(df_cursos$logfile[i])) ) {
    df_log <- read.csv(as.vector(df_cursos$logfile[i]),fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)                         # Log de accesos al Campus Virtual - Histórico
    if (ncol(df_log) < 6) {df_log <- read.csv(as.vector(df_cursos$logfile[i]),fileEncoding="utf-8",check.names=FALSE,header= T,sep = ",",stringsAsFactors=FALSE)}
    switch (as.character(df_cursos$logvers[i]),
            "2.6" = {
              df_log       <- df_log[as.character(df_log$idEspacioCV) == as.character(df_cursos$id[i]),]  
              df_log$Fecha <- as.Date(format(as.POSIXct(df_log$time, origin="1970-01-01 00:00"),"%Y-%m-%d %H:%M:%S"))
              #            df_log$Hora  <- format(as.POSIXct(df_log$timecreated, origin="1970-01-01"),"%T)},
              df_log <- df_log[as.character(df_log$idEspacioCV) == as.character(df_cursos$id[i]),]
              df_log <- df_log[df_log$rol != "Profesor",]
              df_log <- subset(df_log, select=-idEspacioCV)
              df_log <- df_log[,c("userid","Fecha","module","action","url","ip")]
            },
            "2.9" = {
              df_log       <- df_log[as.character(df_log$idEspacioCV) == as.character(df_cursos$id[i]),]
              df_log$Fecha <- as.Date(format(as.POSIXct(df_log$time, origin="1970-01-01 00:00"),"%Y-%m-%d %H:%M:%S"))
              df_log <- df_log[as.character(df_log$idEspacioCV) == as.character(df_cursos$id[i]),]
              df_log <- df_log[df_log$rol != "Profesor",]
              df_log <- subset(df_log, select=-idEspacioCV)
              df_log <- df_log[,c("idUsuario","Fecha","component","action","other","ip")]
            },
            #            df_log$Hora  <- format(as.POSIXct(df_log$time, origin="1970-01-01"),"%H:%M:%S")},
            "2.9.I" = {
              df_log$Fecha <- as.POSIXct(strptime(df_log$Hora,"%d/%m/%Y %H:%M"))
              df_log$Id <- str_split_fixed(df_log$Descripción,"'",3)[,2]
              colnames(df_log) <- c("Hora","NombreEntero","Usuario","Contexto","Módulo","Acción","Descripción","Origen","IP","Fecha","Id")              
              df_log <- df_log[df_log$Origen == "web",]
              df_log <- df_log[(df_log$Fecha >= as.POSIXct(strptime(df_cursos$fecini[i],"%Y-%m-%d")) & 
                                  df_log$Fecha <= as.POSIXct(strptime(df_cursos$fecfin[i],"%Y-%m-%d"))),]
              Contabiliza_Acciones(df_log,as.character(df_cursos$id[i]))
              df_view <- subset(df_log,  Módulo == "Sistema" & Acción == "Curso visto")
              df_view <- df_view[order(df_view$NombreEntero),]
              df_view <- df_view[!duplicated(df_view$NombreEntero),]
              df_view$NombreEntero <- gsub("^ ","",toupper(df_view$NombreEntero))
              df_nota1 <- merge(x=df_nota,y=df_view,by="NombreEntero",all.x=TRUE)
              df_nota1$IdCV[is.na(df_nota1$IdCV)] <- df_nota1$Id[is.na(df_nota1$IdCV)]
              
              df_nota <- df_nota1[,c("NombreEntero","IdCV","email","DNI","Main1","Main2","Nota1","Nota2","Nota3","Nota4","Nota5","Nota6","Nota7",
                                     "Nota8","Nota9","Nota10","Nota11","Nota12","Nota13","Nota14","Nota15","Nota16","Nota17","Nota18","Nota19",
                                     "Nota20","Nota21","Nota22","Nota23","Nota24","Nota25","Nota26","Nota27","Nota28"),]
              df_view <- df_nota[,c("NombreEntero","IdCV","DNI")]
              df_alum <- merge(df_alum,df_view,by="NombreEntero")
              df_alum$IdCV.x[is.na(df_alum$IdCV.x)] <- df_alum$IdCV.y[is.na(df_alum$IdCV.x)]
              df_alum$IdCV.y <- NULL
              #                    df_alum$NombreEntero <- NULL
              colnames(df_alum)[colnames(df_alum)=="IdCV.x"] <- "IdCV"
              df_log <- df_log[,c("Id","NombreEntero","Hora","Módulo","Acción","Descripción","IP")]
              df_log$NombreEntero <- gsub("^ ","",toupper(df_log$NombreEntero))
            },
            {}
    )
  }
  colnames(df_log) <- c("IdCV","NombreEntero","Fecha","Módulo","Acción","Otros","Ip")
  #  df_log <-df_log[df_log$Fecha >= df_cursos$fecini[i] & df_log$Fecha <= df_cursos$fecfin[i],]
  
  ####################################
  ## Cuentas de acceso por usuarios ##
  ####################################
  ## Visitas
  df_accFecha       <- data.frame(Fecha=seq(as.Date(df_cursos$fecini[i]),as.Date(df_cursos$fecfin[i]),1))
  for (Modulo in List_Modulos_Esp) {
    # Comentamos porque, al haber dos entornos moodle diferentes, los idCV de cada usuario son diferentes, 
    # por eso se contabilizan por nombreEntero y no por idCV
    #          df_tmp <- Contar(i,Modulo,"IdCV")
    #          if (nrow(df_tmp) > 0 ) {
    #            df_tmp$IdCV <- as.integer(df_tmp$IdCV)
    #            df_alum <- merge(df_alum,df_tmp,by="IdCV",all.x=TRUE)
    #            if (NROW(df_tmp) == 0) {df_alum <- cbind(df_alum,0)}
    #            df_alum$Contador <- as.integer(df_alum$Contador)
    #            df_alum$Contador[is.na(df_alum$Contador)]<-0
    #          } else {
    #            df_alum$Contador <- 0
    #          }
    #          if (sum(df_alum$Contador) == 0) {
    #            df_alum$Contador <- NULL
    df_tmp <- Contar(i,Modulo,"NombreEntero")
    if (nrow(df_tmp) > 0 ) {
      df_alum <- merge(df_alum,df_tmp,by="NombreEntero",all.x=TRUE)
      if (NROW(df_tmp) == 0) {df_alum <- cbind(df_alum,0)}
      df_alum$Contador <- as.integer(df_alum$Contador)
      df_alum$Contador[is.na(df_alum$Contador)]<-0
    } else {
      df_alum$Contador <- 0
    }
    #          }
    
    colnames(df_alum) [ncol(df_alum)] <- Modulo
    
    df_tmp <-Contar(i,Modulo,"Fechas")
    if (nrow(df_tmp) > 0 ) {
      df_tmp$Fecha <- as.Date(df_tmp$Fecha,"%Y-%m-%d")
      df_accFecha <- merge (df_accFecha,df_tmp,by="Fecha",all.x=TRUE)
      df_accFecha$Contador[is.na(df_accFecha$Contador)] <- 0
    } else {
      df_accFecha$Contador <- 0
    } 
    colnames(df_accFecha) [ncol(df_accFecha)] <- Modulo
  }  
  df_alumaux <- as.data.frame(df_alum[,"NombreEntero"])
  colnames(df_alumaux) <- "NombreEntero"
  df_log <- merge(x=df_alumaux,y=df_log,by="NombreEntero",all.x=TRUE) # Solo alumnos
  df_log$Curso      <- i
  #  colname(df_log) <- c("IdCV","DNI","Rol","Fecha","Módulo","Acción","Otros","Ip")
  df_logs           <- rbind(df_logs,df_log)
  #  RangoFecha$Curso  <- i
  #  RangoFechas       <- RangoFecha
  df_alum$Curso     <- i
  
  df_alums          <- rbind(df_alums,df_alum,stringAsFactor=TRUE ,deparse.level=2)
  df_alums          <- df_alums[df_alums$Sexo!="TRUE",]
  if (NROW(df_recurso) > 0) {
    df_recurso$Curso  <- i
    df_recursos      <- rbind(df_recursos,df_recurso,stringAsFactor=TRUE)
  }
  df_accFecha$Curso <- i
  df_accFechas      <- rbind(df_accFechas,df_accFecha)
  
  rm(df_log,df_nota,df_nota1,df_notax,df_alum,df_recurso,df_accFecha)
}
df_logs <- df_logs[!is.na(df_logs$Fecha),]
df_accFechas <- df_accFechas[!is.na(df_accFechas$Fecha),]
#########################################################  
## Accesos a MYSQL para conocer accesos por Titulación ##
#########################################################
rDataFile <- paste(myXML,".Rdata",sep="")
if (file.exists(rDataFile)) {load(rDataFile)
} else {
  storiesDb <- dbConnect(RMariaDB::MariaDB(), user='us_pimcd', password='us_pimcd_BD_2020!', dbname='CV', host='127.0.0.1')
  #  dbListTables(storiesDb)
  df_logCentro<-data.frame()
  ContEspaciosEstudios <- matrix(ncol=3)
  if (!unique(as.character(df_cursos$degree)) == "") {
    for (CodplanEstudios in unique(as.character(df_cursos$degree))) {
      if (CodplanEstudios != "XXXX") {
        #  CodplanEstudios   <- as.character(i)
        for (cursoAcademico in unique(substr(df_cursos$anoaca[df_cursos$degree == CodplanEstudios],3,7))) {
          #    CursoAcademico <- substr(j,3,7)
          anoaca <- paste(substr(cursoAcademico,1,2),substr(cursoAcademico,4,5),sep="")
          if (anoaca == "2021") {anoaca="1920"}
          curso <- cursoAcademico
          if (cursoAcademico == "20/21") {curso ="19/20"}
          MySQLTable1 <- paste("LAPIMCD201820_",anoaca,"_",DesPlanEstudios(CodplanEstudios),sep="")
          MySQLTable2 <- paste("mdl_logstore_LAPIMCD201820_",anoaca,"_",DesPlanEstudios(CodplanEstudios),sep="")
          query<-paste("Select * from ",MySQLTable1," WHERE codigoPlan = '",CodplanEstudios,"' AND cursoAcademico = '",curso,"';",sep="")
          rsSelect <- dbSendQuery(storiesDb, query)
          planEstudios <- dbFetch(rsSelect)
          #  df_planEstudios <- df_planEstudios[!duplicated(df_planEstudios$idEspacioCV),]
          dbClearResult(rsSelect)
          cont = 0
          NumEspaciosCentro = 0
          for (espacio in unique(planEstudios$url)) {
            query<-paste("Select * from ",MySQLTable2," WHERE courseid = '",espacio,"';",sep="")
            rsSelect <- dbSendQuery(storiesDb, query)
            df_tmp <- as.data.frame(dbFetch(rsSelect))
            df_tmp <- df_tmp[df_tmp$origin =="web",]
            df_tmp <- df_tmp[, c(14,18,3,4,17,20)]
            df_tmp <- unique(df_tmp)
            if (NROW(df_tmp) > 0) {
              df_tmp$CodPlanEstudios <- CodplanEstudios
              df_tmp$cursoAcademico <- cursoAcademico
              colnames(df_tmp)<- c("idCV","Fecha","Módulo","Acción","Otros","Ip","CodPlanEst","cursoAcademico")
              cont = cont + 1
              if (NROW(df_tmp) > 100) {
                NumEspaciosCentro <- NumEspaciosCentro + 1
                df_logCentro <- rbind(df_logCentro,df_tmp)
              }
            }
            print (paste(cont,CodplanEstudios,cursoAcademico,espacio,NROW(df_tmp),sep=" "))
            dbClearResult(rsSelect)
          }
          ContEspaciosEstudios <- rbind(ContEspaciosEstudios,c(CodplanEstudios,cursoAcademico,NumEspaciosCentro))
        }
      }
      df_ContEspaciosEstudios <- as.data.frame(ContEspaciosEstudios)
      colnames(df_ContEspaciosEstudios) <- c("estudios","anoaca", "numEspacios")
      df_ContEspaciosEstudios <- df_ContEspaciosEstudios[!is.na(df_ContEspaciosEstudios$estudios),]
      dbDisconnect(storiesDb)  
      #df_logCentro$Curso = 1
      df_logCentro <- df_logCentro[!is.na(df_logCentro$Fecha),]
      df_logCentro$Fecha <- as.POSIXct(as.integer(df_logCentro$Fecha), origin="1970-01-01 00:00")
      save(df_ContEspaciosEstudios,df_logCentro,file=rDataFile)
    }
  } else {
    df_ContEspaciosEstudios <- data.frame(estudios=0,anoaca=0,numEspacios=0)
    df_ContEspaciosEstudios <- df_ContEspaciosEstudios[!(df_ContEspaciosEstudios$estudios == 0),]
  }
}
