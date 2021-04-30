        ###################################################
        # Ubicacion de archivos
        #   Home:     C:\
        #   Index :   C:\R\PIMCD\git\Data
        #   Fuentes:  C:\R\PIMCD\git\Organografía6.R
        #   Datos:    C:\R\PIMCD\Data
        #   Imagenes: C:\R\PIMCD\Datos\imagennes
        #   RData:    C:\R\PIMCD\git\Data
        # Cosas a hacer:
        #   - Asignatura de Yolanda. Ok
        #   - Estadisticas por estudios. Ok
        #   - Actividades activas/pasivas
        #   - Cluster de usuarios. Ok
        #   - Desglose de recursos por tipo (formato)
        ###################################################
        library(rpart)            ## Librería para realizar árboles de decisión
        library(rpart.plot)       ##     "         representar   "
        library(party)            ## Libreria para realizar árboles de regresión
        library(stringr)          ## Libreria para fraccionar cadenas de caracteres 
        library ("RColorBrewer")  ## Libreria para tratar paletas de colores
        library(ggplot2)          ## Librería de gráficos
        library (lattice)         ## Librería de Gráficos 
        library (RMariaDB)
        library (gridExtra)       ## Libreria Complementaria para ggplot
        library(dplyr)
        library(reshape2)
        library(data.table)
        library(cowplot)
        library(scales)           ## Libreria para reescalado de datos
        require(XML)              ## Librería para tratar archivos XML
        #colors<-c("#3F748F","#6ABADD","#F39C1D","#8A5B9D","#A7BA17","#AAAAAA","#2A3A4A")
        colors<-c("Red","Blue","Green","Brown","#A7BA17","#AAAAAA","#2A3A4A")
        
        # Estadísticas de accesos al curso 16/17 del Campus Virtual
        
        List_Modulos_Esp <- c("Visitas","Recursos","AEvaluaciones","URLs","Foros","Tareas","Glosarios","Wikis")
        List_Modulos_Eng <- c("Views","Resources","Quizzes","URLs","Forums","Assigments","Glossaries","Wikis")
        Tools<- matrix(
          c(
            #      "E","assignsubmission_comments","created",
            "E","assignsubmission_file","created",
            "E","assignsubmission_file","updated",
            "E","assignsubmission_file","uploaded",
            #      "E","gradereport_grader","viewed",
            #      "E","gradereport_overview","viewed",
            "I","gradereport_user","viewed",
            "E","mod_assign","graded",
            "E","mod_assign","submitted",
            "I","mod_assign","viewed",
            "E","mod_forum","created",
            "E","mod_forum","deleted",
            "E","mod_forum","restored",
            "E","mod_forum","updated",
            "E","mod_forum","uploaded",
            "I","mod_forum","viewed",
            "E","mod_glossary","created",
            "E","mod_glossary","updated",
            "A","mod_glossary","viewed",
            "I","mod_page","viewed",
            "E","mod_quiz","started",
            "E","mod_quiz","submitted",
            "A","mod_quiz","reviewed",    
            "A","mod_quiz","viewed",
            "A","mod_resource","viewed",
            "I","mod_url","viewed",
            "E","mod_wiki","created",
            "E","mod_wiki","deleted",
            "E","mod_wiki","restored",
            "E","mod_wiki","updated",
            "A","mod_wiki","viewed"),
          nrow=28,  # number of rows 
          ncol=3,   # number of columns 
          byrow = TRUE) # fill matrix by rows 
        colnames(Tools)<-c("Tipo","Component","Action")
        MAX_GRADES_COL <- 30      # Numero máximo de Columnas de notas
        Sys.setlocale("LC_TIME","es_ES.UTF-8")
        CursosCV <- 11100         # Cursos Virtualizados en 2017/18
        GraToFich <- "S"          # Destino de los Gráficos
        GraOpt <- ",width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100"
    #    myXML <- "./Data/index_PilarXX.xml"
        myXML <- "./Data/index_Teresa1920.xml"
        lang <- "Esp"
        ###     #########     #
        ###   ##         ##   #
        ###  ##   E T L   ##  #
        ###   ##         ##   #
        ###     #########     #
        # setwd("~/R/PIMCD/Datos")
        #setwd("/R/Organografía/Datos")
        #dir <- readline("Indique directorio directorio de trabajo: ")
        #setwd(dir);
        ### Archivo General XML
        ###
        #myXML <- readline(prompt="Indica archivo XML: ")
        #while (GraToFich != "N" && GraToFich != "S") {
        #  GraToFich <- toupper(readline(prompt = "Gráficos a fichero (S/n> "))
        #}
        #if (GraToFich == "N" || GraToFich == "NO") {GraToFich <- "N"} else {GraToFich <- "S"} 
        #tmp <- GET(doc)
        #doc <- content(doc, as="text", encoding="UTF-8")
        #doc <- substr(doc, 2, nchar(doc)) # skip encoding bits at the beginning
        
        #doc_x <- xmlParse(doc, encoding="UTF-8")
        data <- xmlParse(myXML,useDotNames=FALSE,useInternalNodes=TRUE)
        df_cursos <- xmlToDataFrame(collectNames=TRUE,nodes = getNodeSet(data, "//cursos/curso"))
        NumCursos <- NROW (df_cursos)
        df_parciales<-xmlToDataFrame(nodes = getNodeSet(data, "//cursos/curso/parciales"),homogeneous = T)
        if (NCOL(df_parciales > 0)) {
          for (i in 1:NCOL(df_parciales)){ 
            df_parciales[,i] <-as.Date(df_parciales[,i],format="%Y-%m-%d")
          }
        }
        df_pruebas<-xmlToDataFrame(nodes = getNodeSet(data, "//cursos/curso/pruebas"),homogeneous = T)
        if (NCOL(df_pruebas > 0)) {
          for (i in 1:NCOL(df_pruebas)){ 
            df_pruebas[,i] <-as.Date(df_pruebas[,i],format="%Y-%m-%d")
          }
        }
        df_evaluaciones<-xmlToDataFrame(nodes = getNodeSet(data, "//cursos/curso/autoevaluaciones"),homogeneous = T)
        if (NCOL(df_evaluaciones > 0)) {
          for (i in 1:NCOL(df_evaluaciones)){ 
            df_evaluaciones[,i] <-as.Date(df_evaluaciones[,i],format="%Y-%m-%d")
          }
        }
        df_foros<-xmlToDataFrame(nodes = getNodeSet(data, "//cursos/curso/foros"),homogeneous = T)
        if (NCOL(df_foros > 0)) {
          for (i in 1:NCOL(df_foros)){ 
            df_foros[,i] <-as.Date(df_foros[,i],format="%Y-%m-%d")
          }
        }
        df_tareas<-xmlToDataFrame(nodes = getNodeSet(data, "//cursos/curso/tareas"),homogeneous = T)
        if (NCOL(df_tareas > 0)) {
          for (i in 1:NCOL(df_tareas)){ 
            df_tareas[,i] <-as.Date(df_tareas[,i],format="%Y-%m-%d")
          }
        }
        list_grades <- c("//cursos/curso/grades/main_grade",
                         "//cursos/curso/grades/main_grade2",
                         "//cursos/curso/grades/grade")
        df_grades<-data.frame(stringsAsFactors = FALSE)
        for (i in 1:NumCursos) {
          df_grade <- xmlToDataFrame(xmlRoot(data)[[i]][["grades"]],colClasses=c("character","character"))
        #  df_grade <- xmlToDataFrame(xmlRoot(data)[[i]][["grades"]])
          df_grade$passgrade <- as.numeric(as.character(df_grade$passgrade))
          df_grade$Curso <- i
        #  df_grade <- df_grade[!is.na(df_grade[2]),]
          df_grades <- rbind(df_grades,df_grade,stringAsFactor=FALSE)
        }
        colnames(df_grades) <- c("Nombre","Passgrade","Curso")
        df_grades<- df_grades[!is.na(df_grades$Nombre),]
        df_grades<- df_grades[!(df_grades$Nombre == "FALSE"),]
        #df_grades<-xmlToDataFrame(nodes = getNodeSet(data,list_grades),homogeneous = T)
        list_groups <- c("//cursos/curso/groups/group",
                         "//cursos/curso/groups/group/grade",
                         "//cursos/curso/groups/group/splits/split")
        df_ingrades <- data.frame(stringsAsFactors = FALSE)
        #df_ingrades <-data.frame(stringsAsFactors = FALSE)
        #df_groups <- xmlToDataFrame(nodes = getNodeSet(data,list_groups))
        #df_groups <- xmlToDataFrame(xmlRoot(data)[[i]][["groups"]],colClasses=c("character","character"))
        #df_groups<-data.frame(stringsAsFactors = FALSE)
        for (i in 1:NumCursos) {
          df_ingrade <- xmlToDataFrame(xmlRoot(data)[[i]][["groups"]])
          df_ingrade$Curso <- i
          df_ingrades <- rbind(df_ingrades,df_ingrade,stringAsFactor=FALSE)
        }
        df_ingrades <- df_ingrades[df_ingrades$Curso != 0,]
        if (file.exists("./Data/AccDiaCV.csv")) {
          accDia_CV <- read.csv("./Data/AccDiaCV.csv",fileEncoding="utf-8",check.names=FALSE,header=T,sep=";",stringsAsFactors=FALSE)
          colnames(accDia_CV) <- c("Fecha","VisitasCV")
          accDia_CV$Fecha <- as.Date(as.character(accDia_CV$Fecha),format="%Y%m%d")
          accDia_CV$DiaSem <- as.POSIXlt(accDia_CV$Fecha)$wday # Dia de la semana. De 0 a 6 (Domingo, lunes ... Sábado)
        }
        if (file.exists("./Data/AccHoraCV.csv")) {
          accHora_CV <- read.csv("./Data/AccHoraCV.csv",fileEncoding="utf-8",check.names=FALSE,header=T,sep=";",stringsAsFactors=FALSE)
          colnames(accHora_CV) <- c("Hora","Visitas")
        }
        free(data)
        NumCursos <- NROW (df_cursos)
        ################################
        ### DECLARACION DE FUNCIONES ###
        ################################
        Contar <- function(curso,que,por) {
          if (curso > 0) {
            version=df_cursos$logvers[curso]
          } else {version = "2.9"}
          switch (que,
                  Visitas = {
                    switch (as.character(version),
                            "2.6" = {df_view <- subset(df_log, Módulo == "course" & Acción == "view")},
                            "2.9" = {df_view <- subset(df_log, Módulo == "core" & Acción == "viewed")},
        #                    "2.9" = {df_view <- subset(df_log, Módulo == "core" & Acción == "viewed" & Otros == "a:1:{s:19:\"coursesectionnumber\";i:-1;}")},
                            "2.9.I" = {df_view <- subset(df_log, Módulo == "Sistema" & Acción == "Curso visto")},
                            {}
                            )
                    },
                  Recursos ={
                    switch (as.character(version),
                          "2.6" = {df_view <- subset(df_log, Módulo == "resource"     & Acción == "view")},
                          "2.9" = {df_view <- subset(df_log, Módulo == "mod_resource" & Acción=="viewed")},
                          "2.9.I" = {df_view<-subset(df_log, Módulo == "Recurso"      & Acción == "Módulo de curso visto")},
                          {}
                          )
                    },
                  AEvaluaciones ={
                    switch (as.character(version),
                          "2.6" = {df_view <- subset(df_log, Módulo == "quiz"         & Acción == "close attempt")}, 
                          #                                       & cmid != "1811831" & cmid != "1811832" & cmid != "1811933")},
                          "2.9" = {df_view <- subset(df_log, Módulo == "mod_quiz"     & Acción == "submitted")},
                          "2.9.I" = {df_view<-subset(df_log, Módulo == "Cuestionario" & Acción == "Intento enviado")},
                          {}
                          )
                    },
                  Foros ={
                    switch (as.character(version),
                          "2.6" = {df_view <- subset(df_log, Módulo == "forum"     & Acción == "add post")},
                          "2.9" = {df_view <- subset(df_log, Módulo == "mod_forum" & Acción == "uploaded")},
                          "2.9.I" = {df_view<-subset(df_log, Módulo == "Foro"      & Acción == "Tema visto")},
                          {}
                          )
                    },
                  Tareas ={
                    switch (as.character(version),
                          "2.6" = {df_view<-subset(df_log, Módulo == "assign"     & Acción == "upload")},
                          "2.9" = {df_view<-subset(df_log, Módulo == "mod_assign" & Acción == "submitted")},
                          "2.9.I" = {df_view <-subset(df_log, Módulo == "Tarea"   & Acción == "Se ha enviado una entrega")},
                          {}
                          )
                    },
                  URLs ={
                    switch (as.character(version),
                          "2.6" = {df_view<-subset(df_log, Módulo == "assign"     & Acción == "upload")},
                          "2.9" = {df_view<-subset(df_log, Módulo == "mod_assign" & Acción == "submitted")},
                          "2.9.I" = {df_view <-subset(df_log, Módulo == "URL"   & Acción == "Módulo de curso visto")},
                          {}
                          )
                    },
                  Glosarios ={
                    switch (as.character(version),
                          "2.6" = {df_view<-subset(df_log, Módulo == "assign"     & Acción == "upload")},
                          "2.9" = {df_view<-subset(df_log, Módulo == "mod_assign" & Acción == "submitted")},
                          "2.9.I" = {df_view <-subset(df_log, Módulo == "Glosario"   & Acción == "La entrada ha sido creada")},
                          {}
                          )
                    },
                  Wikis ={
                    switch (as.character(version),
                          "2.6" = {df_view<-subset(df_log, Módulo == "assign"     & Acción == "upload")},
                          "2.9" = {df_view<-subset(df_log, Módulo == "mod_assign" & Acción == "submitted")},
                          "2.9.I" = {df_view <-subset(df_log, Módulo == "Wiki"   & (Acción == "Página wiki creada" || Acción == "Página de la wiki actualizada"))},
                          {}
                          )
                  },
                  {}
                  )
          switch(por,
                 IdCV = {
                   df_view <- df_view[order(df_view$IdCV),]
                   cc <- by(df_view,df_view["IdCV"], 
                            function (y) list(Contador = as.numeric(length(y$IdCV))),simplify=FALSE)
                   dd <- cbind(unique(df_view["IdCV"]), do.call(rbind,cc ))
                   },
                 NombreEntero = {
                   df_view <- df_view[order(df_view$NombreEntero),]
                   cc <- by(df_view,df_view["NombreEntero"], 
                            function (y) list(Contador = as.numeric(length(y$NombreEntero))),simplify=FALSE)
                   dd <- cbind(unique(df_view["NombreEntero"]), do.call(rbind,cc ))
                 },
                 Fechas = {
                   if (curso != 0) {
                     df_view$Fecha <- as.Date(substr(df_view$Fecha,1,10),"%d/%m/%Y")
                   } else {
                       df_view$Fecha <- as.Date(format(df_view$Fecha,"%Y-%m-%d"))
                       }
                   df_view <- df_view[order(df_view$Fecha),]
                   cc <- by(df_view,df_view["Fecha"], 
                            function (y) list(Contador = as.numeric(length(y$Fecha))),simplify=FALSE)
                   dd <- cbind(unique(df_view["Fecha"]), do.call(rbind,cc))
                 },
                 {}
                 )
          return(dd)
          }
        
        ### Resultados por Parciales ------------------------------------------------
        res_par <- function(curso) {
          df_alum  <- df_alums[df_alums$Curso == curso,]
          df_grade <- df_grades[df_grades$Curso == curso,]
          numnotas <- df_cursos$NumNotas[curso]
          ini = grep("Main1",colnames(df_alum))[1]
          x <- matrix(ncol=numnotas,nrow=3)
          for (j in 1:numnotas) {
            k = ini+j-1
            x[1,j] <- NROW(subset(df_alum,as.numeric(as.character(df_alum[,k])) >= df_grade$Passgrade[j] & !is.na(df_alum[,k])))
            x[2,j] <- NROW(subset(df_alum,as.numeric(as.character(df_alum[,k])) < df_grade$Passgrade[j] & !is.na(df_alum[,k])))
            x[3,j] <- NROW(subset(df_alum,df_alum[,k] == "NP"))
        #    x[3,j] <- NROW(df_alum[is.na(df_alum[,k]),])
          }
          return(x)
          }
        
        ### Contabiliza las acciones de cada curso ------------------------------------------------
        Contabiliza_Acciones <- function(df_log,curso) {
          if (!file.exists("./Data/Modulos_Acciones.csv")) {
            df_acciones <- data.frame(-1,0)
            colnames(df_acciones) <- c("Modulo","Accion")
          } else {
            df_acciones <- read.csv("./Data/Modulos_Acciones.csv",fileEncoding="utf-8",check.names=FALSE,header=T,sep=",",stringsAsFactors=FALSE)
            if (colnames(df_acciones)[1]== "") {df_acciones[,1] <- NULL}
          }
        # Comprobamos si ha sido contabilizada esta asignatura
          pos=grep(curso,colnames(df_acciones))[1]
          if (is.na(pos)) { # Curso previamente contabilizado
            df_acciones$New <- 0
            colnames(df_acciones)[colnames(df_acciones)=="New"] <- curso
            pos=1
          } else {
            df_acciones[[curso]] <- 0
          }
          for (comp in unique(df_log$Módulo)) {
            ##  lista_Eventos <- unique(df_log[df_log$Componente==i,]$`Nombre evento`)unique(df_log$Componente)
            for (even in unique(df_log[df_log$Módulo==comp,]$Acción)) {
              cont <- NROW(df_log[df_log$Módulo == comp & df_log$Acción==even,])
              if (NROW(df_acciones[df_acciones$Modulo == comp & df_acciones$Accion == even,]) == 1) {  # Ya esiste este Módulo + Acción
                df_acciones[df_acciones$Modulo ==comp & df_acciones$Accion == even,curso] <- cont
              } else {
                df_tmp<- df_acciones[1,]
                df_tmp$Modulo <- comp
                df_tmp$Accion <- even
                df_tmp[1,curso] <- cont
                df_acciones <- rbind(df_acciones,df_tmp)
              }
            }
          }
          df_acciones <- df_acciones[df_acciones$Modulo != "-1",]
          write.csv(df_acciones,"Modulos_Acciones.csv")
        }
      # Descripción de la titulación
        DesPlanEstudios <- function(i) {
          Estud <- "Sin datos de la titulación"
          switch (as.character(i),
                  "0805" = {Estud <- "Medicina"},
                  "0843" = {Estud <- "Fisioterapia"},
                  "0844" = {Estud <- "Podologia"}
          )
          return(Estud)
        }
        
        df_alums    <- data.frame()
        df_notas    <- data.frame()
        df_logs     <- data.frame()
        df_recursos <- data.frame()
        df_accFechas<- data.frame (Curso=numeric(),FecIni=as.Date(character()),FecFin=as.Date(character()),stringsAsFactors=FALSE)
        #############################################
        ### PROCESO DE TRATAMIENTO DE LOS n CURSOS ##
        #############################################
        source ("./Proceso_datos.R")
        ###    #############################
        ###  ##                             ##
        ### ##  R E P R E S E N T A C I Ó N  ##
        ###  ##                             ##
        ###    #############################
        ### Imprimimos por pantalla o gráfico las tablas de estado de cada asignatura
        if (lang == "Eng") {
          Sys.setlocale("LC_TIME","C")
          } else {
            Sys.setlocale("LC_TIME","es_ES.UTF-8")
          }
        # Creamos los directrorios de imágenes si no existieran
        for (i in 1:NumCursos) {
          subDir <- paste("imagenes",df_cursos$id[i],sep="/")
          if (!file.exists(subDir)) dir.create(file.path(".", subDir))
        }
        ###---------------------###
        ## 000 - CUADRO DE MANDO ##
        ###---------------------###
        for (i in 1:NumCursos) {
          df_alum <- df_alums[df_alums$Curso == i,]
          df_alum$nombreEntero <- paste(df_alum$Nombre,df_alum$Apellidos)
          df_log <- read.csv(as.vector(df_cursos$logfile[i]),fileEncoding="utf-8",check.names=FALSE,header= T,sep = ";",stringsAsFactors=FALSE)                         # Log de accesos al Campus Virtual - Histórico
          if (ncol(df_log) < 6) {df_log <- read.csv(as.vector(df_cursos$logfile[i]),fileEncoding="utf-8",check.names=FALSE,header= T,sep = ",",stringsAsFactors=FALSE)}
          df_log$fecha <- as.POSIXct(strptime(df_log$Hora,"%d/%m/%Y %H:%M"))
          df_log$idCV <- sapply(strsplit(as.character(df_log$Descripción),split="The user with id '",fixed=T),"[",2)
          df_log$idCV <- sapply(strsplit(as.character(df_log$idCV),split="'",fixed=T),"[",1)
          colnames(df_log) <- c("hora","nombreEntero","usuario","contexto","modulo","accion","descripcion","origen","ip","fecha","idCV")              
          df_log <- df_log[df_log$origen == "web",]
          df_log <- df_log[(df_log$fecha >= as.POSIXct(strptime(df_cursos$fecini[i],"%Y-%m-%d")) & 
                              df_log$fecha <= as.POSIXct(strptime(df_cursos$fecfin[i],"%Y-%m-%d"))),]
          df_log1 <- subset(df_log, (df_log$nombreEntero %in% df_alum$nombreEntero))
          df_log2 <- subset(df_log, (df_log$idCV %in% df_alum$IdCV))
          if (NROW(df_log1 ) > NROW(df_log2)) {df_log <- df_log1} else {df_log <- df_log2}
          resumen <- as.data.frame(table(df_log$modulo,df_log$accion))
          resumen <- resumen[!resumen$Freq == 0,]
          resumen1 <- as.data.frame(table(df_log$modulo,df_log$accion,df_log$idCV))
          resumen1 <- resumen1[!resumen1$Freq == 0,]
          resumen1 <- as.data.frame(table(resumen1$Var1,resumen1$Var2))
          resumen1 <- resumen1[!resumen1$Freq == 0,]
          resumen <- cbind(resumen,resumen1$Freq)
          if (NROW(resumen) > 0) {
            colnames(resumen) <- c("Componente","Acción","Veces","Usuarios")
            resumen$Acción <- paste(resumen$Componente, resumen$Acción, sep = " > ")
            resumen$Componente <- NULL
            resumen <- resumen[order(resumen$Veces,decreasing=T),]
          }
          if (GraToFich == "S") {jpeg(paste(paste("imagenes/",df_cursos$id[i],"/000.jpg",sep="")),
                                      width = 1240, height = 780, units = 'px', 
                                      pointsize = 12,quality = 100)}
          tt3 <- ttheme_default(core=list(fg_params=list(hjust=0, x=0.1)),
                                rowhead=list(fg_params=list(hjust=0, x=0)))
          titulo=textGrob(paste("Resumen de acciones para el curso: ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep="")
                          ,gp=gpar(fontsize=20,font=3))
          top <- nrow(resumen)
          if (top <= 35) {
            grid.arrange(tableGrob(resumen[1:top,1:3],rows=NULL,theme=tt3),
                         nrow=1,top=titulo)
            } else {
              if (top <= 70) {
                grid.arrange(tableGrob(resumen[1:35,1:3],rows=NULL,theme=tt3),
                             tableGrob(resumen[36:top,1:3],rows=NULL,theme=tt3),
                             nrow=1,top=titulo)
                } else {
                  if (top <= 105) {
                    grid.arrange(tableGrob(resumen[1:35,1:3],rows=NULL,theme=tt3),
                                 tableGrob(resumen[36:70,1:3],rows=NULL,theme=tt3),
                                 tableGrob(resumen[71:top,1:3],rows=NULL,theme=tt3),
                                 nrow=1,top=titulo)
                    } else {
                      grid.arrange(tableGrob(resumen[1:35,1:3],rows=NULL,theme=tt3),
                                   tableGrob(resumen[36:70,1:3],rows=NULL,theme=tt3),
                                   tableGrob(resumen[71:105,1:3],rows=NULL,theme=tt3),
                                   nrow=1,top=titulo)
                    }
                }
            }
          
    #      p <- ggplot(resumen, geom="blank") + theme_bw() + theme(line=element_blank()) + 
    #        ggtitle("Acciones Principales") + annotation_custom(tableGrob(resumen[1:20,])) 
    #      print (p)
          if (GraToFich == "S") {dev.off()}
          }
    #    library(gridExtra)
    #    df_tmp <- df_cursos[,c("id","title"),]
    #    if (GraToFich == "S") {jpeg(paste(paste("imagenes/",df_cursos$id[i],"/000.jpg",sep="")),
    #                                width = 1240, height = 780, units = 'px', 
    #                                pointsize = 12,quality = 100)}
    #    for (i in 1:NumCursos) {
    #      df_tmp[i,3]<-  NROW(df_alums[df_alums$Curso == i,])
    #      for (j in List_Modulos_Esp) {
    #        l = which (List_Modulos_Esp %in% j)
    #    #    if (j %in% colnames(df_tmp))
    #    #    {
    #          df_tmp[i,l+3] <- round(NROW(df_alums[df_alums$Curso == i & df_alums[j] > 0,])*100 / NROW(df_alums[df_alums$Curso == i,]),digits=2)
    #    #    } else {
    #    #      df_tmp[i,l+2] <- round(NROW (df_alums[df_alums$Curso == i & df_alums[j] > 0,])*100 / NROW(df_alums[df_alums$Curso == i,]),digits=2)
    #    #    }
    #      }
    #    }
    #    if (lang == "Eng") {
    #      names(df_tmp) <- c("id","name","Num",List_Modulos_Eng)
    #      } else {
    #        names(df_tmp) <- c("id","Nombre","Núm",List_Modulos_Esp)
    #      }
    #    #colnames(df_tmp) <- c("Cód.","Asignatura","Acceden","AutoEvaluaciones","Tareas","Recursos","Foros")
    #    p <- ggplot(df_tmp, geom="blank") + theme_bw() + theme(line=element_blank()) + 
    #      ggtitle("Marcadores Principales") + annotation_custom(tableGrob(df_tmp)) + ggtitle("Marcadores Principales") 
    #    # + geom_bar()
    #    print (p)
    #    #qplot(1:10, 1:10, geom = "blank") + theme_test() + theme(line = element_blank(), text = element_blank()) +
    #    #  # Then I add my table :
    #    #  annotation_custom(grob = tableGrob(df_tmp))
    #    if (GraToFich == "S") {dev.off()}
        
        ###------------------------------###
        ## 001 - PARES POR CALIFICACIONES ##
        ###------------------------------###
        for (i in 1:NumCursos) {
          df_grade <- df_grades[df_grades$Curso==i,] 
    
          if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/001-",df_cursos$id[i],".jpg",sep=""),
                                      width = 1240, height = 780, units = 'px', 
                                      pointsize = 12,quality = 100)}
          if (df_cursos$NumNotas[i] > 3) {
            #    plot.new()  
            df_alum <- df_alums[df_alums$Curso==i,]
            ini=grep("Main1",colnames(df_alum))[1]
            fin=ini+df_cursos$NumNotas[i]-1
            colsex=grep("Sexo",colnames(df_alum))
            df_alum <- df_alum[,c(colsex,ini:fin)]
            for (j in 2:ncol(df_alum)) {
              if (sum(!is.na(df_alum[,j])) > 0) {
                colnames(df_alum)[j] = as.character(df_grade$Nombre[j-1])
              } else {
                colnames(df_alum)[j] <- "NULL"
              }
            }
            df_alum <- df_alum[,!names(df_alum) == "NULL"]
            df_alum[,-1] <- mutate_if(df_alum[,-1], is.character, as.numeric)
            pairs(df_alum[,c(2:ncol(df_alum))],main=paste(df_cursos$title[i]," (",df_cursos$id[i],")",sep=""),
                  col = ifelse(df_alum$Sexo == "V", "blue","red"), 
                  pch =15,cex.labels=2.5,cex.main=2.5,cex.axis=2)
            #    legend("topleft", border="black", fill = c("blue","gold"), legend = c("V","M"))
            if (GraToFich == "S") {dev.off()}
          }
        }
        
        ###-------------------###
        ##  002                ##
        ###-------------------###
        ### Relación entre notas parciales por Sexo ----------------------------------------
        for (i in 1:NumCursos) {
          df_grade <- df_grades[df_grades$Curso==i,] 
          df_alum <- df_alums[df_alums$Curso==i,]
          if (sum(!is.na(df_alum$Main1)) > 0  & sum(!is.na(df_alum$Main2)) > 0) {
            if (GraToFich == "S") {
              jpeg(paste("imagenes/",df_cursos$id[i],"/002-",df_cursos$id[i],".jpg",sep=""))
            }
            aaa="No HOlA"
            ini=grep("Main1",colnames(df_alum))[1]
            fin=ini+1
            colnames(df_alum)[ini] <- as.character(df_grade$Nombre[1])
            colnames(df_alum)[fin] <- as.character(df_grade$Nombre[2])
            df_alum[ini] <- as.numeric(df_alum[[ini]])
            df_alum[fin] <- as.numeric(df_alum[[fin]])          
            pairs(main=paste(df_cursos$title[i]," (",df_cursos$id[i],")",sep=""),
                  df_alum[,c(ini:fin)], na.action=na.omit,col = ifelse(df_alum$Sexo == "V", "blue","red"), 
                  pch =15)
            #    legend("topleft", border="black", fill = c("blue","gold"), legend = c("V","M"))
            if (GraToFich == "S") {dev.off()}
          } 
        }
        ###----------------------------###
        ##  003 - MATRICULADOS POR SEXO ##
        ###----------------------------###
        #if (GraToFich == "S") {jpeg('imagenes/003.jpg')}
        par(cex=2.5)
        tab = matrix(1,ncol=2)
        if (lang == "Eng") {
          colnames(tab) <- c("Female","Male")
        } else {
          colnames(tab) <- c("Mujeres","Varones")
        }
        for (i in 1:NumCursos) {
          #  tab = matrix(nrow=1,ncol=3)
          df_alum <- df_alums[df_alums$Curso==i,] 
          tab[1,] <- c(prop.table(table(as.factor(df_alum$Sexo)))*100)
          #           prop.table(table(as.factor(df_alums[df_alums$Curso == i,]$Sexo)))*100)
          if (lang == "Eng") {
            title=paste("Enrolled (%)",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep="")
          } else {
            title=paste("Matriculados (%) ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep="")
          }
          if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/003-",df_cursos$id[i],".jpg",sep=""),
                                      width = 360, height = 360, units = 'px', 
                                      pointsize = 12,quality = 100)}
        #  title=paste("Students enrolled (%)",df_cursos$title[i],sep=" - ")
          legend= c(paste(colnames(tab)[1],round(tab[1,1],digits=2),"%"),paste(colnames(tab)[2],round(tab[1,2],digits=2),"%"))
          barplot(tab[1,], main=title,col=c("red","blue"),ylim=c(0, max(tab[1,]) + 10),legend.text=legend)
          #  rm(tab)
          if (GraToFich == "S") {dev.off()}
        }
        
        
        ###---------------------------###
        ##  003.1 - APROBADOS POR SEXO ##
        ###---------------------------###
        
        #if (GraToFich == "S") {jpeg('imagenes/003-1.jpg')}
        par(cex=1.5)
        tab = matrix(nrow=1,ncol=2)
        if (lang == "Eng") {
          colnames(tab) <- c("Female","Male")
        } else {
          colnames(tab) <- c("Mujeres","Varones")
        }
        for (i in 1:NumCursos) {
          #  tab = matrix(nrow=1,ncol=3)
          df_alum <- df_alums[df_alums$Curso==i,]       
          tab[1,] <- prop.table(table(as.factor(df_alum[as.numeric(as.character(df_alum$Final_T)) >= 5,]$Sexo)))*100
          if (lang == "Eng") {
            title=paste("Students passed (%) ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep="")
          } else {
            title=paste("Aprobados (%) ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep="")
          }#  rm(tab)
    
          if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/003.1-",df_cursos$id[i],".jpg",sep=""),
                                      width = 720, height = 360, units = 'px', 
                                      pointsize = 12,quality = 100)}
          legend= c(paste(colnames(tab)[1],round(tab[1,1],digits=2),"%"),paste(colnames(tab)[2],round(tab[1,2],digits=2),"%"))
          barplot(tab[1,], main=title,col=c("red","blue"),ylim=c(0, max(tab[1,]) + 10),legend.text=legend)
          #  rm(tab)
          if (GraToFich == "S") {dev.off()}
        }
        
        
        ###-------------------------------###
        ##  004 - RESULTADOS POR PARCIALES ##
        ###-------------------------------###
        # Core wrapping function
        wrap.it <- function(x, len)
        { 
          sapply(x, function(y) paste(strwrap(y, len), 
                                      collapse = "\n"), 
                 USE.NAMES = FALSE)
        }
        
        
        # Call this function with a list or vector
        wrap.labels <- function(x, len)
        {
          if (is.list(x))
          {
            lapply(x, wrap.it, len)
          } else {
            wrap.it(x, len)
          }
        }
  #      if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/004.jpg",sep=""),
  #                                  width = 1240, height = 780, units = 'px', 
  #                                  pointsize = 12,quality = 100)}
      #  par(mfrow=c(1,NumCursos),cex=1.5)
        for (i in 1:NumCursos) {
          if (df_cursos$NumNotas[i] > 0) {
            NumNotas <- df_cursos$NumNotas[i]
            df_alum  <- df_alums[df_alums$Curso==i,]
            df_grade <- df_grades[df_grades$Curso==i,]
            ini = grep("Main1",colnames(df_alum))[1]
            fin = ini+df_cursos$NumNotas[i]-1
            tab_par <- res_par(i)
        #    tab_par <- res_par(as.data.frame(df_alum[,c(ini:fin)]),NumNotas)
            if (lang == "Eng") {
              rownames(tab_par) = c("Passed","Failed","Absented")
            } else {
              rownames(tab_par) = c("Aprobados","Suspensos","No Presentados")
            }    
            #  colnames(tab_par)=c("1º","2º","3º","4º","5º")
            # colnames(tab_par)=c(1:NumNotas)
        #    for (j in 1:NumNotas) { names(tab_par)[j] <- paste("Nota",j,sep="") }
        #    colnames(tab_par) <- names(tab_par)[1:NumNotas]
            colnames(tab_par) <- wrap.labels(df_grade$Nombre[1:NumNotas],10)
            # colnames(tab_par)=c(1:paste("Nota",i,sep=""))
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/004-",df_cursos$id[i],".jpg",sep=""),
                                        width = 1240, height = 780, units = 'px', 
                                        pointsize = 12,quality = 100)}
            par(cex=2)
            barplot(tab_par,col=colors,beside=FALSE,ylim=c(0,NROW(df_alum)),
                    main=paste(df_cursos$title[i],"\n(",df_cursos$id[i],")",sep=""),names.arg=colnames(tab_par),
                    cex.names=0.8,las=2)
        #    ggplot(as.data.frame(tab_par),aes(x=colnames(tab_par),y=rownames(tab_par))) + 
        #      geom_bar () + 
        #      ggtitle(df_cursos$id[1])  
        #      geom_text(aes(label = round(tab_par, 2)), size = 6,position = position_dodge(0.85), vjust = -0.5)
            legend("bottomleft", legend=rownames(tab_par),fill=colors,bg='transparent')
        #    mtext("Notas Parciales",side=4,font=2)
        #    mtext("Midterm Grades ",side=4,font=2)
        #    text(tab_par[i,],cex=1)
            if (GraToFich == "S") {dev.off()}
          }
        }
        
        
        ###---------------------------### 
        ##  005 - DATAGRAMA POR MÓDULO ##
        ###---------------------------###
        df_alums_original <- df_alums
        ini <- which (colnames(df_alums) %in% "Main1")
        top <- which (colnames(df_alums) %in% "Nota28")
        # Ojo, Convertimos los NA y los NP a nulos con lo que no aparecerán en la estadística  
        for (k in (ini:top)) { df_alums[,k] <- as.numeric(as.character(df_alums[,k])) }
        #
        #  if (GraToFich == "S") {jpeg(paste("imagenes/005-",Modulo,".jpg",sep=""))}
        #    {jpeg('imagenes/005.jpg')}
        #  par(mfrow=c(NumCursos,1))
    #    cols <- c("Evaluaciones"="grey","Tareas"="brown","Parciales"="blue","Foros"="green")
        for (i in 1:NumCursos) {
          df_accFecha <- df_accFechas[df_accFechas$Curso == i,]
          for (Modulo in List_Modulos_Esp) {
            if (NROW(df_accFecha[df_accFecha[[Modulo]] > 0,]) > 0) {
              if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/005-",df_cursos$id[i],"-",Modulo,".jpg",sep=""),width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
        ##      par(cex=5.5)
    #          df_accFecha <- df_accFechas[df_accFechas$Curso == i,]
    #          list_tareas <- as.list(df_tareas[i,])
    #          list_parciales <- as.list(df_parciales[i,])
              p <- ggplot(df_accFecha,aes(x=Fecha,y=as.numeric(.data[[Modulo]])),show.legend=F) + 
                geom_line(linetype=1,color="black") +  
    
                labs(title=paste(df_cursos$title[i]," (",df_cursos$id[i],")",sep=""),
                     x=" ",y=Modulo)
              if (NCOL(df_evaluaciones[i,!is.na(df_evaluaciones[i,])]) > 0) {
                for (j in 1:NCOL(df_evaluaciones[i,!is.na(df_evaluaciones[i,])])) {
                  p <- p + geom_vline(aes_(xintercept = df_evaluaciones[i,j],color = "Eva"),linetype="dashed") 
                }
                }
              if (NCOL(df_tareas[i,!is.na(df_tareas[i,])]) > 0) {
                for (j in 1:NCOL(df_tareas[i,!is.na(df_tareas[i,])])) {
    ###              print(paste("Tareas",df_cursos$id[i],Modulo,i,j,tarea,sep="|"))
                  p <- p + geom_vline(aes_(xintercept = df_tareas[i,j],color = "Tar"),linetype="dashed") 
                }
              }
              if (NCOL(df_parciales[i,!is.na(df_parciales[i,])]) > 0) {
                for (j in 1:NCOL(df_parciales[i,!is.na(df_parciales[i,])])) {
                  p <- p + geom_vline(aes_(xintercept = df_parciales[i,j],color = "Par"),linetype="dashed") 
                }
              }
              if (NCOL(df_pruebas[i,!is.na(df_pruebas[i,])]) > 0) {
                for (j in 1:NCOL(df_pruebas[i,!is.na(df_pruebas[i,])])) {
                  p <- p + geom_vline(aes_(xintercept = df_pruebas[i,j],color = "Pru"),linetype="dashed") 
                }
              }
              if (NCOL(df_foros[i,!is.na(df_foros[i,])])) {
                for (j in 1:NCOL(df_foros[i,!is.na(df_foros[i,])])) {
                  p <- p + geom_vline(aes_(xintercept = df_foros[i,j],color = "For"),linetype="dashed") 
                }
                }
              p <- p + scale_color_manual(name="HITOS",values = colors,
                                          breaks = c("Eva", "Tar", "Par","Pru","For"),
                                          labels = c("Autoevaluaciones", "Trabajos", "Exámenes","Pruebas","Foros")) +
                  scale_linetype_manual(values = c("dashed"))
              print (p)
              
              if (GraToFich == "S") {dev.off()}
            }
          }
        }
    
        ###---------------------------------###
        ##  006.1 - RADAR POR CALIFICACIONES ##
        ###---------------------------------###
        library (fmsb)
        library(ggiraph)
        library(ggiraphExtra)
        library(ggpubr)
        for (i in 1:NumCursos) {
          df_grade <- df_grades[df_grades$Curso==i,] 
          df_alum <- df_alums[df_alums$Curso == i & !is.na(df_alums$Final_T),]
          if (nrow(df_alum) > 0 & nrow(df_grade) > 0) {
            if (lang == "Eng") {
              df_chart <- data.frame(row.names=c("Max.","Min","Final mark=[0,5)","Final mark=[5,8)", "Final mark=[8,10]"))
              } else {
                df_chart <- data.frame(row.names=c("Max.","Min","Nota Final=[0,5)","Nota Final=[5,8)", "Nota Final=[8,10]"))
                }
            df_chart[3,1] <-NROW(df_alum[as.numeric(as.character(df_alum$Final_T)) < 5,])
            df_chart[4,1] <-NROW(df_alum[as.numeric(as.character(df_alum$Final_T)) >= 5 & as.numeric(as.character(df_alum$Final_T)) < 8,])
            df_chart[5,1] <-NROW(df_alum[as.numeric(as.character(df_alum$Final_T)) >= 8,])
            df_chart[1,1] <- round(max(df_chart[,1],na.rm=T),digits=0)
            df_chart[2,1] <- 0
            for (j in List_Modulos_Esp) {
              k = which (List_Modulos_Esp %in% j)+1
              df_chart[3,k] <- round(sum(df_alum[as.numeric(as.character(df_alum$Final_T)) < 5,][[j]])/df_chart[3,1],digits=2)
              df_chart[4,k] <- round(sum(df_alum[as.numeric(as.character(df_alum$Final_T)) >= 5 & as.numeric(as.character(df_alum$Final_T)) < 8,][[j]])/df_chart[4,1],digits=2)
              df_chart[5,k] <- round(sum(df_alum[as.numeric(as.character(df_alum$Final_T)) >= 8,][[j]])/df_chart[5,1],digits=2)
              df_chart[1,k] <- round(max(df_chart[,k],na.rm=T),digits=0)
              df_chart[2,k] <- 0
            }
            if (lang == "Eng") {colnames(df_chart)<-c("Num",List_Modulos_Eng)
            } else {
              colnames(df_chart)<-c("Num",List_Modulos_Esp) 
              }
          #  Limpiamos los módulos que no tienen valores
           df_chart <-df_chart[,df_chart[1,] > 0]  
    #      colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
    #      colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
           if (lang == "Eng") {
             tit = paste("Clusters according to Final Grades - ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep="")
             } else {
               tit = paste("Cluster por grupos de Notas - ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep="")
               }
           df_chart <- df_chart[-c(1:2),]
           df_chart$Rango <- rownames(df_chart)
           lista <- colnames(df_chart)
           lista <- lista[2:(length(lista)-1)]
           df_chart <- df_chart %>% select(Rango,Num,lista)
           df_chart2 <- df_chart
           df_chart2[is.na(df_chart2)] <- 0
           lista.z <- c(NULL)
           for (col in lista) {
              name <- paste("z",col,sep=".")
              df_chart2[name] <- df_chart2[,col]/max(df_chart2[,col])
  #            df_chart2[name] <- rescale(df_chart2[,col],to=c(0,1))
              lista.z <- c(lista.z,name)
            }
            setDT(df_chart2)  # Convertimos df_chart en table
            dat2test <- melt.data.table(df_chart2[,-2],"Rango",lista.z) # Eliminamios la columna Num
    
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/006.1-",df_cursos$id[i],".jpg",sep=""),width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
            thePlot <-  ggplot(data = dat2test, aes(x=variable, y=value, group=Rango, color=Rango)) + ## Define data and colour column
              geom_polygon(fill = NA) + ## make the lines meet
              theme_bw() +
              theme(text = element_text(size=20),axis.title.x=element_blank(),
                    axis.text.y=element_blank(),axis.title.y=element_blank()) +
              coord_polar() +
              ggtitle(tit)
            stable.p <- ggtexttable(df_chart, rows = NULL, theme = ttheme("mBlue"))
    #        h <- ggdraw(thePlot) +
    #          annotation_custom(grob=rectGrob(gp=gpar(fill="red")),
    #                            xmin=2.5,
    #                            xmax=Inf,
    #                            ymin=-Inf,
    #                            ymax=5)
            h <- grid.arrange(ggdraw(thePlot),tableGrob(df_chart[,-1]),nrow=1) 
    #        h <- ggdraw(thePlot)
    #        h + cowplot::draw_grob(ggplotGrob(stable.p), vjust=0.4,hjust=-0.25)
    #        print(h)
            if (GraToFich == "S") dev.off()
            }
          }
        
        
          ###------------------------###
          ## 006-I - ANALISIS INVERSO ##
          ###------------------------###
    ### Tabla con grafico en https://stackoverflow.com/questions/12318120/adding-table-within-the-plotting-region-of-a-ggplot-in-r
        library(ggplotify)
        for (i in 1:NumCursos) {
          df_ingrade <- df_ingrades[df_ingrades$Curso == i,]
          df_grade <- df_grades[df_grades$Curso == i,]
        #  df_alum <- df_alums[df_alums$Curso == i & !is.na(df_alums$Final_T),]
          df_alum <- df_alums[df_alums$Curso == i,]
          df_ingrade1 <- data.frame(
            ingrade = c("Final_T","Final_T","Final_T","Final_T"),
            cod_split = c("Entre 0 y 5","Entre 5 y 6,94","Entre 6,95 y 8,54","Más de 8,54"),
            min_split = c("0","5","6.95","8.55"),
            max_split = c("5","6.95","8.55","1000"),
            Curso = c(1,1,1,1))
          df_ingrade <- rbind(df_ingrade,df_ingrade1,stringAsFactor=FALSE)
          df_ingrade <- df_ingrade[df_ingrade$Curso != 0,]
          #  Grupos <- unique(df_ingrade$ingrade)
          for (grupo in unique(df_ingrade$ingrade)) { # Calificación
            df_ingrade1 <- df_ingrade[df_ingrade$ingrade == grupo,]
            df_chart <- data.frame() # Media
        #    df_chart <- data.frame(row.names=c("Max.","Min.",as.vector(unique(df_ingrade1$cod_split))))
        #    df_chart2 <- data.frame(row.names=c("Max.","Min.",as.vector(unique(df_ingrade1$cod_split))))
            df_ingrade1$min_split <- as.numeric(as.character(df_ingrade1$min_split))
            df_ingrade1$max_split <- as.numeric(as.character(df_ingrade1$max_split))
            if (grupo  == "Final_T") {
              pos <- which(as.vector(colnames(df_alum)) %in% grupo)
            } else {
              pos <- which(as.vector(df_grade$Nombre) %in% grupo)
              ini <- which(as.vector(colnames(df_alum)) %in% "Main1")
              pos <- ini+pos-1
            }
            for (j in 1:NROW(df_ingrade1)) { # split
              df_chart[2+j,1] <-NROW(df_alum[df_alum[pos] >= df_ingrade1$min_split[j] & 
                                            df_alum[pos] < df_ingrade1$max_split[j]  & !is.na(df_alum[pos]),])
              for (k in List_Modulos_Esp) { # Módulo
                l = which (List_Modulos_Esp %in% k)
                df_chart[2+j,l+1] <- round(mean(df_alum[df_alum[pos] >= df_ingrade1$min_split[j] & 
                                                          df_alum[pos] < df_ingrade1$max_split[j],k],na.rm=T),2)
              }
              }
            if (lang == "Eng") {
              colnames(df_chart)<-c("Num",List_Modulos_Eng)
            } else {
              colnames(df_chart)<-c("Num",List_Modulos_Esp)
            }
            for (j in 1:ncol(df_chart)) {
              df_chart[1,j] <- trunc(max(df_chart[,j],na.rm=T),digits=0)
            }
             #  Limpiamos los módulos que no tienen valores
  
            df_chart <-df_chart[,df_chart[1,] > 0]
            row.names(df_chart)=c("Max.","Min.",as.vector(unique(df_ingrade1$cod_split)))
            df_chart <- df_chart[-c(1:2),]
            df_chart$Rango <- rownames(df_chart)
            lista <- colnames(df_chart)
            lista <- lista[2:(length(lista)-1)]
            df_chart <- df_chart %>% select(Rango,Num,lista)
            df_chart2 <- df_chart
            df_chart2[is.na(df_chart2)] <- 0
            lista.z <- c(NULL)
            for (col in lista) {
              name <- paste("z",col,sep=".")
              df_chart2[name] <- df_chart2[,col]/max(df_chart2[,col])
              #            df_chart2[name] <- rescale(df_chart2[,col],to=c(0,1))
              lista.z <- c(lista.z,name)
            }
            if (ncol(df_chart) > 0) {
  ##            colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9),"DarkGreen")
  ##            colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4),rgb(0.5,0.7,0.5,0.4) )
              if (lang == "Eng") {
                tit = paste("Cluster according to ",grupo," (",df_cursos$title[i],")\n(",df_cursos$id[i],")",sep="")
                } else {
                  tit = paste("Cluster por grupos de ",grupo," (",df_cursos$title[i],")\n(",df_cursos$id[i],")",sep="")
                }
    #          if (ncol(df_chart)> 3) {
    #            radarchart(df_chart[,-1], axistype=2, maxmin=T, pcol=colors_border, pfcol=colors_in, plwd=4 , plty=1,
    #                 #            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1000,10), cglwd=0.8, seg=max(df_chart)/10, title=paste("Cluster por grupos de Notas (",df_cursos$title[i],")",sep="")
    #                 cglcol="black", cglty=1, axislabcol="black", cglwd=0.8, seg=10,title=tit)
    #            legend(x=1, y=1.3, legend = rownames(df_chart[-c(1,2),]), bty = "n", pch=20, col=colors_border, text.col = "black", cex=1.2, pt.cex=3)
    #            if (GraToFich == "S") {dev.off()}
    #          }
    #          if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/006-I-",df_cursos$id[i],"-",grupo,"(tabla).jpg",sep=""),width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
    #          grid.newpage() 
    #          pushViewport(viewport(layout=grid.layout(2, 1, widths=unit(c(5,4), "inches"))))
    #          pushViewport(viewport(layout.pos.col=1, layout.pos.row=2)) 
    #    #      print(df_chart, newpage=FALSE) 
    #          popViewport(1)
    #          pushViewport(viewport(x=0.8, y=0.8, clip="off",gp=gpar(cex=0.9,alpha=0.9)))
    #    #     grid.arrange(top="Iris dataset", tableGrob(t(df_chart[-c(1:2),])))
    #    #
    #          grid.draw(tableGrob(t(df_chart[-c(1:2),])))
    #          pushViewport(viewport(x=0.5, y=0.1, clip="off",gp=gpar(cex=0.9,alpha=0.9)))
    #          grid.draw(tableGrob(t(df_chart2[-c(1:2),])))
    #          popViewport()
              if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/006-I-",df_cursos$id[i],"-",grupo,".jpg",sep=""),
                                          width = 1240, height = 780, units = 'px',
                                          pointsize = 12,quality = 100)}
  ## Antiguo gráficos
  ##            if (ncol(df_chart) > 3) {
  ##              thePlot <- as.ggplot(~radarchart(df_chart[,-1], axistype=2, maxmin=T, pcol=colors_border, pfcol=colors_in, 
  ##                         plwd=4 , plty=1,cglcol="black", cglty=1, axislabcol="black", cglwd=0.8, seg=10,title=tit)) 
  ##  #            thePlot <- thePlot + as.ggplot(legend(x=1, y=1.3, legend = rownames(df_chart[-c(1,2),]), bty = "n", pch=20, 
  ##  #                                                  col=colors_border, text.col = "black", cex=1.2, pt.cex=3))
  ##              h <- thePlot +
  ##                annotation_custom(
  ##                  grob = tableGrob(df_chart[-c(1:2),]),
  ##                  xmin = 0.2,
  ##                  xmax = 0.9,
  ##                  ymin = 0.2,
  ##                  ymax = 0
  ##                )
              setDT(df_chart2)  # Convertimos df_chart en table
              dat2test <- melt.data.table(df_chart2[,-2],"Rango",lista.z) # Eliminamios la columna Num
              thePlot <-  ggplot(data = dat2test, aes(x=variable, y=value, group=Rango, color=Rango)) + ## Define data and colour column
                geom_polygon(fill = NA) + ## make the lines meet
                theme_bw() +
                theme(text = element_text(size=20),axis.title.x=element_blank(),
                      axis.text.y=element_blank(),axis.title.y=element_blank()) +
                coord_polar() +
                ggtitle(tit)
              stable.p <- ggtexttable(df_chart, rows = NULL, theme = ttheme("mBlue"))
              #        h <- ggdraw(thePlot) +
              #          annotation_custom(grob=rectGrob(gp=gpar(fill="red")),
              #                            xmin=2.5,
              #                            xmax=Inf,
              #                            ymin=-Inf,
              #                            ymax=5)
              h <- grid.arrange(ggdraw(thePlot),tableGrob(df_chart[,-1]),nrow=1)             
    #            print (h)
    #            grid.arrange(thePlot,grid.raster(as.matrix(df_chart[-c(1:2),]),vp=viewport(angle=90)),nrow=1)
    #            grid.arrange(thePlot,ggtexttable(df_chart[-c(1:2),]),nrow=1)
              }
              if (GraToFich == "S") {dev.off()}
          }
        }
        
        ###------------------------------###
        ##  007 - APROBADOS POR MATRICULA ##
        ###------------------------------###
        for (i in 1:NumCursos) {
          df_alum <- df_alums[df_alums$Curso == i,]
          if (NROW(unique(df_alum$Matrícula)) > 1 ) {
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/007-",df_cursos$id[i],".jpg",sep=""),width = 1240, 
                                        height = 780, units = 'px', pointsize = 12,quality = 100)}
            p <- ggplot(df_alum, aes(x = factor(Matrícula), y = Final_T)) + 
              geom_boxplot() + 
              geom_jitter(aes(colour=Matrícula),show.legend=F) +
              theme(text = element_text(size=20)) +
              ggtitle(paste("Calificaciones Finales según Nº Matrícula\n",df_cursos$id[i])) +
              xlab("") + ylab("Nota Final")
            print(p)
            if (GraToFich == "S") {dev.off()}
          }
        }
        ###---------------------------------###
        ##  008 - APROBADOS POR CONVOCATORIA ##
        ###---------------------------------###
        for (i in 1:NumCursos) {
          df_alum <- df_alums[df_alums$Curso == i,]
          if (NROW(unique(df_alum$Convocatoria)) > 1 ) {
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/008-",df_cursos$id[i],".jpg",sep=""),width = 1240, 
                                        height = 780, units = 'px', pointsize = 12,quality = 100)}
            p <- ggplot(df_alum, aes(x = factor(Convocatoria), y = Final_T)) + 
              geom_boxplot() + 
              geom_jitter(aes(colour=Convocatoria),show.legend=F) +
              theme(text = element_text(size=20)) +
              ggtitle(paste("Calificaciones Finales según Nº Convocatoria\n",df_cursos$id[i])) +
              xlab("") + ylab("Nota Final")
            print(p)
            if (GraToFich == "S") {dev.off()}
          }
        }
        ###--------------------------------###
        ##  009 - APROBADOS POR PROCEDENCIA ##
        ###--------------------------------###
        for (i in 1:NumCursos) {
          df_alum <- df_alums[df_alums$Curso == i,]
          if (NROW(unique(df_alum$Procedencia)) > 1 ) {
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/009-",df_cursos$id[i],".jpg",sep=""),width = 1240, 
                                        height = 780, units = 'px', pointsize = 12,quality = 100)}
            p <- ggplot(df_alum, aes(x = Procedencia, y = Final_T)) + 
              geom_boxplot(outlier.shape = NA,show.legend=F,aes(color=factor(df_alum$Procedencia))) + 
    #          geom_text(size=3,position=position_stack(vjust=0.5)) +
              geom_jitter(aes(colour=Procedencia),show.legend=F,size=5) +
              theme(text = element_text(size=20)) +
              ggtitle(paste("Calificaciones Finales según Procedencia\n",df_cursos$id[i])) +
              xlab("") + ylab("Nota Final")
            print(p)
            if (GraToFich == "S") {dev.off()}
          }
        }
        ###------------------------###
        ##  014 - DIAGRAMA DE CAJAS ##
        ###------------------------###
        ##Distribución de Calificaciones
          for (i in 1:NumCursos) {
            df_grade <- df_grades[df_grades$Curso==i,]
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/014-",df_cursos$id[i],".jpg",sep=""),width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
          #  if (GraToFich == "S") {jpeg(paste("imagenes/014-",i,".jpg",sep=""))}
            df_alum <- df_alums[df_alums$Curso == i,]
            ini=grep("Main1",colnames(df_alum))[1]
            fin=ini+df_cursos$NumNotas[i]-1
            df_alum <- df_alum[,c(ini:fin)]
            if (lang == "Eng") {
              tit <- paste("Grades - ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep=" ")
              lab <- "Calificacion"
            } else {
              tit <- paste("Nota Exámenes - ",df_cursos$title[i],"\n(",df_cursos$id[i],")",sep=" ")
              lab <- "Grade"
            }
            boxplot(df_alum,main=tit,xlab='Prueba',las=2,ylab=lab,names=df_grade[,1])
            legend.text=""
            for (j in 1:NCOL(df_alum)) {
              legend.text <- paste(legend.text,colnames(df_alum)[j]," --> ",df_grade$Nombre[j],"\n",sep="") 
            }
          #  mtext(legend.text,side=1,adj=1,outer=F)
            if (GraToFich == "S") {dev.off()}
          }
          
          ###----------------------------###
          ##  015 - DATAGRAMAS POR MODULO ##
          ###----------------------------###
          #df_log <- df_logCentro
          #accDia_Centro <- Contar(0,"Visitas","Fechas")
          #accDia_Centro$Contador <- as.integer(round(as.integer(accDia_Centro$Contador)/2,digits=5))
          #accDia_Centro$Contador <- as.integer(round(as.integer(accDia_Centro$Contador) / (NumEspaciosCentro*2),digits=0))
          for (i in 1:NumCursos) {
              if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/015-",df_cursos$id[i],".jpg",sep=""),
                                        width = 1240, height = 780, units = 'px', 
                                        pointsize = 12,quality = 100)}
            NumEspaciosCentro <- as.numeric(as.character(df_ContEspaciosEstudios$numEspacios[df_ContEspaciosEstudios$estudios == df_cursos$degree[i] &
                                                                                             df_ContEspaciosEstudios$anoaca == substr(df_cursos$anoaca[i],3,7)]))
            df_log <- df_logCentro[df_logCentro$CodPlanEst == as.character(df_cursos$degree[i]) & 
                                     df_logCentro$cursoAcademico == substr(df_cursos$anoaca[i],3,7),]
            if  (NROW(df_ContEspaciosEstudios) > 0) {
              accDia_Centro <- Contar(0,"Visitas","Fechas")
          #  accDia_Centro$Curso <- i
              accDia_Centro$Contador <- as.integer(round(as.integer(accDia_Centro$Contador) / (NumEspaciosCentro*2),digits=0))
            }
            df_accFecha <- df_accFechas[df_accFechas$Curso == i,]
            df_accFecha <- merge(df_accFecha,accDia_CV,by="Fecha",all.x=TRUE)
            if  (NROW(df_ContEspaciosEstudios) > 0) {
              df_accFecha <- merge(df_accFecha,accDia_Centro,by="Fecha",all.x=TRUE)
            }
            if (lang == "Eng") {
              tit <- "Course site views"
            } else {
              tit <- "Accesos durante el curso"
            }
            plot.new()
            p <- ggplot(df_accFecha, aes(x=df_accFecha$Fecha))
            p <- p + labs(title=tit, subtitle=paste(df_cursos$title[i]," (",df_cursos$id[i],")",sep=""),
                          caption="Data Source: Google Analytics") 
            #  p <- p + ggtitle(expression(atop(paste("Accesos durante el curso"),
            #                                   atop(paste(df_cursos$title[i]," (",df_cursos$title[i],")",sep="")),""))) 
            p <- p + theme_update(plot.title = element_text(hjust = 0.5))
            p <- p + geom_line(aes(y=as.numeric(df_accFecha$Visitas)), colour=colors[1])
            p <- p + geom_line(aes(y=as.numeric(df_accFecha$VisitasCV)/CursosCV), colour=colors[2])
            if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) { 
              p <- p + geom_line(aes(y=df_accFecha$Contador), colour=colors[3])
            } 
            p <- p + xlab("") + ylab("")
            NumParciales <- NCOL(df_parciales[i,!is.na(df_parciales[i,])])
            if (NumParciales > 0) {
              for (j in 1:NumParciales) {
                p <- p + geom_vline(xintercept=df_parciales[i,j],color="red", size=0.5)}}
            NumEvaluaciones <- NCOL(df_evaluaciones[i,!is.na(df_evaluaciones[i,])])
            if (NumEvaluaciones > 0) {
              for (j in 1:NumEvaluaciones) {
                p <- p + geom_vline(xintercept=df_evaluaciones[i,j],color="dark grey", size=0.5)}}
            print (p);
            if  (exists("df_ContEspaciosEstudios") && is.data.frame(get("df_ContEspaciosEstudios"))) {
              if (lang == "Eng") {
                if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) { 
                  leyenda <- c(df_cursos$id[i],DesPlanEstudios(df_cursos$degree[i]),"CV-UCM mean")
                } else {
                  leyenda <- c(df_cursos$id[i],"CV-UCM mean")
                }
              } else {
                if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) { 
                  leyenda <- c(df_cursos$id[i],DesPlanEstudios(df_cursos$degree[i]),"Media CV-UCM")
                } else {
                  leyenda <- c(df_cursos$id[i],"Media CV-UCM")
                }
              }
            } else {
              if (lang == "Eng") {
                leyenda <- c("Site views","CV-UCM mean")
              } else {
                leyenda <- c("Visitas","Media CV-UCM")
              }
            }
            legend("topright",inset=.05,cex=1,leyenda,horiz=FALSE,lty=c(1,1),lwd=c(2,2),col=colors[1:5])
            if (GraToFich == "S") {dev.off()}
          }
          
          ###----------------------------------###
          ## 016 - ACCESOS POR DIA DE LA SEMANA ##
          ###----------------------------------###
          dayweek_esp <- c("dom","lun","mar","mié","jue","vie","sáb")
          dayweek_eng <- c("Sun","Mon","Tue","Wed","Thu","Fri","Sat")
          acc_diasem = matrix(nrow=3,ncol=7)
          acc_hora = matrix(nrow=3,ncol=24)
          if (lang == "Esp") {dayweek <- dayweek_esp} else {dayweek <- dayweek_eng}
          colnames(acc_diasem) = dayweek
          colnames(acc_hora) = c("00:01","01:02","02:03","03:04","04:05","05:06","06:07","07:08","08:09","09:10","10:11","11:12",
                                 "12:13","13:14","14:15","15:16","16:17","17:18","18:19","19:20","20:21","21:22","22:23","23:24")
          for  (i in c(1:NumCursos)) {
            df_log <- df_logs[df_logs$Curso == i & df_logs$Módulo == "Sistema" & df_logs$Acción == "Curso visto",]
            num_weeks = as.numeric(round(difftime(as.Date(df_cursos$fecfin[i]),as.Date(df_cursos$fecini[i]),units="weeks"),digits=0))
            num_dias = as.numeric(round(difftime(as.Date(df_cursos$fecfin[i]),as.Date(df_cursos$fecini[i]),units="days"),digits=0))
            df_accFecha <- df_accFechas[df_accFechas$Curso == i & df_accFechas$Fecha >= as.Date(df_cursos$fecini[i]) & 
                                          df_accFechas$Fecha <= as.Date(df_cursos$fecfin[i]),]
            accDia_CV_aux <- accDia_CV[accDia_CV$Fecha >= as.Date(df_cursos$fecini[i]) & accDia_CV$Fecha <= as.Date(df_cursos$fecfin[i]),]
            df_log$HoraDia <- as.numeric(strftime(as.POSIXct(df_log$Fecha,format="%d/%m/%Y %H:%M"),format="%H"))
            df_log$DiaSem <- as.POSIXlt(df_log$Fecha)$wday # Dia de la semana. De 0 a 6 (Domingo, lunes ... Sábado)
            df_accFecha$DiaSem <- as.POSIXlt(df_accFecha$Fecha)$wday # Dia de la semana. De 0 a 6 (Domingo, lunes ... Sábado)
    #        df_log$DiaSem  <- strftime(as.POSIXct(df_log$Fecha,format="%d/%m/%Y %H:%M"),format="%A")
            anoaca <- substr(df_cursos$anoaca[i],3,7)
            if (anoaca == "20/21") {anoaca <- "19/20"}
            for (j in c(1:7)) {
              acc_diasem[1,j] <- round(NROW(df_log[df_log$DiaSem == j-1,]) / num_weeks,digits=2)
          #    acc_diasem[2,j] <- round(sum(accDia_Centro_aux$Contador[format(accDia_Centro$Fecha,format="%a") == dayweek[j]],na.rm=T) / num_weeks,digits=2)
              if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {
                NumEspaciosCentro <- as.numeric(as.character(df_ContEspaciosEstudios$numEspacios[df_ContEspaciosEstudios$estudios == df_cursos$degree[i] &
                                                                                                   df_ContEspaciosEstudios$anoaca == anoaca]))
                acc_diasem[2,j] <- round(sum(as.numeric(df_accFecha$Visitas[as.POSIXlt(df_accFecha$Fecha)$wday == j-1])) /num_weeks / NumEspaciosCentro,digits=2)
    #            acc_diasem[2,j] <- round(sum(as.integer(df_accFecha$Visitas[format(df_accFecha$Fecha,format="%a") == substr(dayweek[j],1,3)]),na.rm=T) / num_weeks / NumEspaciosCentro,digits=2)
              } else {
                NumEspaciosCentro = 0
                acc_diasem[2,j] = 0
              }
              acc_diasem[3,j] <- round(sum(as.numeric(accDia_CV_aux$VisitasCV[as.POSIXlt(accDia_CV_aux$Fecha)$wday == j-1])) / num_weeks / CursosCV,digits=2)
    #          acc_diasem[3,j] <- round(sum(accDia_CV_aux$VisitasCV[format(accDia_CV_aux$Fecha,format="%a") == substr(dayweek[j],1,3)]) / num_weeks / CursosCV,digits=2)
            }
          #  accDia_Centro <- df_logCentro %>%
          #    select(count(*) as Visitas,df_logCentro[format(df_logCentro$Fecha,format="%H")]) %>%
          #    group_by(df_logCentro[format(df_logCentro$Fecha,format="%H")]) %>%
          #    summarise( = mean(mpg), hp = mean(hp), qsec = mean(qsec))
          #  accHora_Centro <- sum(df_logCentro[format(df_logCentro$Fecha,format="%H")] == j-1)
          ##  df_log <- df_logCentro2
            df_logHora_Centro <- df_logCentro[df_logCentro$CodPlanEst == df_cursos$degree[i] & df_logCentro$cursoAcademico == substr(df_cursos$anoaca[i],3,7) &
                                                df_logCentro$Módulo == "core" & df_logCentro$Acción == "viewed",]
            df_logHora_Centro <- unique(df_logHora_Centro)
            df_logHora_Centro$DiaSem <- as.numeric(strftime(as.POSIXct(df_logHora_Centro$Fecha,format="%d/%m/%Y %H:%M"),format="%H"))
    #        Tot_AccHoras <- table(strftime(as.POSIXct(df_log$Fecha,format="%d/%m/%Y %H:%M"),format="%H"))
            for (j in c(1:24)) {
              acc_hora[1,j] <- round(NROW(df_log[df_log$HoraDia == j-1,]) / num_dias , digits=2)
    #          acc_hora[1,j] <- round(Tot_AccHoras[[j]],digits=2)
              if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {  
                acc_hora[2,j] <- round(NROW(df_logHora_Centro[df_logHora_Centro$DiaSem == j-1,]) / num_dias / NumEspaciosCentro, digits=2)
              }
              acc_hora[3,j] <- round((as.numeric(accHora_CV[j,2])/24)/CursosCV,digits=2)
          
            }
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/016-",df_cursos$id[i],".jpg",sep=""),width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
            if (lang == "Eng") {
              dayweek <- dayweek_eng
              cabecera <- "Site views per weekday (mean)"
              if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {
                leyenda <- c(as.character(df_cursos$id[i]),DesPlanEstudios(df_cursos$degree[i]),"CV-UCM Median")
              } else {
                leyenda <- c(as.character(df_cursos$id[i]),"CV-UCM Median")
              }
            } else {
              dayweek <- dayweek_esp
              cabecera <- "Accesos por día de la semana (media)"
              if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {
                leyenda <- c(as.character(df_cursos$id[i]),DesPlanEstudios(df_cursos$degree[i]),"Media CV-UCM")
              } else {
                leyenda <- c(as.character(df_cursos$id[i]),"Media CV-UCM")
              }
            }
          #  acc_diasem <- acc_diasem[-dim(acc_diasem),]
            if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {
              barplot(acc_diasem,beside=TRUE,axes=TRUE,las=1,col=colors[c(1:dim(acc_diasem)[1])],names.arg=dayweek,
                      main=cabecera,legend.text=leyenda)
            } else {
              barplot(acc_diasem[c(1,3),],beside=TRUE,axes=TRUE,las=1,col=colors[c(1:dim(acc_diasem)[1]-1)],names.arg=dayweek,
                      main=cabecera,legend.text=leyenda)
            }
            #        main="Site views per days of the week (mean)",legend.text=c(as.vector(df_cursos$id),"CV-UCM"))
            if (GraToFich == "S") {dev.off()}
          ###--------------------------------###
          ## 017 - ACCESOS POR FRANJA HORARIA ##
          ###--------------------------------###
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/017-",df_cursos$id[i],".jpg",sep=""),width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
          #if (GraToFich == "S") {jpeg('imagenes/017.jpg')}
            if (lang == "Eng") {
              cabecera <- "Site views per hours of the day (mean)" 
          #  legend <- c(as.vector(df_cursos$id),DesPlanEstudios,"CV-UCM"
              if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {
                leyenda <- c(as.character(df_cursos$id[i]),"Medicine degree","CV-UCM")
              } else {
                leyenda <- c(as.character(df_cursos$id[i]),"CV-UCM")
              }
            } else {
              cabecera <- "Acceso por franja horaria (media)"
          #  legend <- c(as.vector(df_cursos$id),DesPlanEstudios,"CV-UCM")
              if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {
                leyenda <- c(as.character(df_cursos$id[i]),DesPlanEstudios(df_cursos$degree[i]),"CV-UCM")
              } else {
                leyenda <- c(as.character(df_cursos$id[i]),"CV-UCM")
              }
            }
          #  acc_hora <- acc_hora[-dim(acc_hora),]
            if  (df_cursos$degree[i] %in% df_ContEspaciosEstudios$estudios) {
              barplot(acc_hora,beside=TRUE,axes=TRUE,las=1,width=1,col=colors[c(1:dim(acc_hora)[1])],names.arg=colnames(acc_hora),
                      main=cabecera,las=2)
              } else {
                barplot(acc_hora[c(1,3),],beside=TRUE,axes=TRUE,las=1,width=1,col=colors[c(1:dim(acc_hora)[1]-1)],names.arg=colnames(acc_hora),
                        main=cabecera,las=2)
              }
  #          }
            legend("topleft",inset=.02,legend=leyenda,box.col="black",box.lty=1,cex=0.9,ncol=1,fill,col=colors[1:dim(acc_hora)[1]],lty=1,lwd=8)
          #          ,args.legend = list(x = "topleft", bty="n", cex = 0.9,ncol = 1))
            if (GraToFich == "S") {dev.off()}
          }
          
          ###---------------------------------------###
          ## 018 - MAPA DE CALOR ACTIVIDAD DEL CURSO ##
          ###---------------------------------------###
          for (i in 1:NumCursos) {
            if (GraToFich == "S") {jpeg(paste("imagenes/",df_cursos$id[i],"/018-",df_cursos$id[i],".jpg",sep=""),
                                        width = 1240, height = 780, units = 'px', 
                                        pointsize = 12,quality = 100)}
            df_log <- df_logs[df_logs$Curso == i,]
            if (lang == "Eng") {
              weekday_order <- c("Saturday","Friday","Thursday","Wednesday","Tuesday","Monday","Sunday")
              cabecera <- "Activity Map"
            } else {
              weekday_order <- c("domingo","sábado","viernes","jueves","miércoles","martes","lunes")
              cabecera <- "Mapa de calor"
            }
            dayweek <- c("domingo","sábado","viernes","jueves","miércoles","martes","lunes")
            if (df_cursos$id[i] == "19-93062") { 
              lista <- as.Date(as.list(as.data.frame(t(df_evaluaciones[i,])))[[1]])
              df_log <- df_log[!(as.Date(df_log$Fecha,format="%d/%m/%Y") %in% lista),]
            }
            date.df<-table(data.frame(data.frame(Hora=format(as.POSIXct(strptime(df_log$Fecha,"%d/%m/%Y %H:%M",tz="")),format="%H"),
                                                 Dia=factor(weekdays(as.Date(df_log$Fecha,"%d/%m/%Y")),levels=weekday_order))))
            print (levelplot(date.df,col.regions=brewer.pal(9,"Oranges"),cuts=8,xlab="",ylab="",main=cabecera,
                      sub=paste(df_cursos$title[i],"\n (",df_cursos$id[i],")",sep="")))
            if (GraToFich == "S") {dev.off()}
          }
          
          ###-------------------------###
          ## 019 - ÁRBOLES DE DECISIÓN ##
          ###-------------------------###
          ## Visitas
          for  (i in c(1:NumCursos)){
            df_grade <- df_grades[df_grades$Curso==i,] 
            df_alum <- df_alums[df_alums$Curso == i,]
  #          df_tmp <- Contar(i,"Visitas","IdCV")
  #          df_tmp <- Contar(i,"Visitas","NombreEntero")
  #          df_tmp$IdCV <- as.integer(df_tmp$IdCV)
            Col_Final <- grep("Final_T",colnames(df_alum))[1]
            rpart_arg <- paste(colnames(df_alum)[Col_Final]," ~ ",sep="")
            fich <- paste("imagenes/",df_cursos$id[i],"/019-",df_cursos$id[i],"-Final_T.jpg",sep="")
            if (GraToFich == "S") {jpeg(fich,width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
          #  if (GraToFich == "S") {jpeg(fich)}
            par(mfrow=c(2,1))
            for (Modulo in List_Modulos_Esp) {
          #    if (Modulo != "Tareas") {
              rpart_arg <- paste(rpart_arg,Modulo, " + ",sep="")}
          #  }
            rpart_arg = paste(rpart_arg,"Matrícula + Convocatoria + Procedencia")
    #        rpart_arg <- substr(rpart_arg,1,nchar(rpart_arg)-3) 
          #  rpart_arg <- "Final_T~Visitas+Recursos+Tareas+Foros+AEvaluaciones"
    #        prp(rpart_arg,type=2,extra=104,nn=T,fallen.leaves = T,faclen = T,varlen = 8,shadow.col = "gray")
    
            arbol=rpart(rpart_arg,data=df_alum, method= "anova",control = rpart.control(cp=0.01),maxdepth=3)
            arbol
            rpart.plot(arbol,
                       shadow.col="gray",type=4, extra=1,under=TRUE, fallen.leaves=TRUE,digits=3, 
                       cex=NULL, tweak=1,main=paste(colnames(df_alum)[Col_Final],df_cursos$title[i],sep=" "))
            
            sbs <- subset(df_alum,!is.na(Final_T))
            sbs$Matrícula    <- factor(sbs$Matrícula)
            sbs$Convocatoria <- factor(sbs$Convocatoria)
            sbs$Procedencia  <- factor(sbs$Procedencia)
            rpart_arg <- as.formula(rpart_arg)
            arbol <- ctree(rpart_arg, data = sbs,controls = ctree_control(minsplit = 50,mincriterion = 0.001))
            arbol
            print(arbol)
            plot(arbol,type="simple",main=paste(df_cursos$title[i],"Nota Final",sep =" - "),           # no terminal plots
                 inner_panel=node_inner(arbol,
                                        abbreviate = FALSE,          # short variable names
                                        pval = TRUE,                # p-values
                                        id = FALSE),
                 terminal_panel=node_terminal(arbol, 
                                              abbreviate = FALSE,
                                              digits = 2,                   # few digits on numbers
                                              fill = c("white"),            # make box white not grey
                                              id = FALSE))
            dev.off()
            ###-----###
            ##  020  ##
            ###-----###
            ini=grep("Main1",colnames(df_alum))[1]
            fin=ini+df_cursos$NumNotas[i]-1
            k=0
            for (j in ini:fin) {
              k=k+1
              fich <- paste("imagenes/",df_cursos$id[i],"/020-",df_cursos$id[i],"-",j,".jpg",sep="")
              if (GraToFich == "S") {jpeg(fich,width = 1240, height = 780, units = 'px', pointsize = 12,quality = 100)}
          #    if (GraToFich == "S") {jpeg(fich)}
              par(mfrow=c(2,1))
              if (sum(!is.na(df_alum[,j])) > 1 ) {
                rpart_arg <- paste(colnames(df_alum)[j]," ~ ",sep="")
                for (Modulo in List_Modulos_Esp) {
          #       if (Modulo != "Tareas") {
                    rpart_arg <- paste(rpart_arg,Modulo, " + ",sep="")
          #          }
                }
                rpart_arg = paste(rpart_arg,"Matrícula + Convocatoria + Procedencia")
    #            rpart_arg <- substr(rpart_arg,1,nchar(rpart_arg)-3) 
                arbol=rpart(rpart_arg,data=df_alum, method= "anova",control = rpart.control(cp=0.01),maxdepth=3)
                arbol
                rpart.plot(arbol,
                           shadow.col="gray",type=4, extra=1,under=TRUE, fallen.leaves=TRUE,digits=3, 
                           cex=NULL, tweak=1,main=paste(colnames(df_alum)[j],df_cursos$title[i],sep=" "))
                sbs <- subset(df_alum,!is.na(df_alum[,j]))
                sbs$Matrícula    <- factor(sbs$Matrícula)
                sbs$Convocatoria <- factor(sbs$Convocatoria)
                sbs$Procedencia <- factor(sbs$Procedencia)
                rpart_arg <- as.formula(rpart_arg)
                arbol <- ctree(rpart_arg, data = sbs,controls = ctree_control(minsplit = 0.1*NROW(df_alum),mincriterion = 0.001))
                arbol <- ctree(rpart_arg, data = sbs,controls = ctree_control(minsplit = 50,mincriterion = 0.001))
                arbol
                print(arbol)
                plot(arbol,type="simple",main=paste(df_cursos$title[i]," (",df_cursos$id[i],") - ",df_grade$Nombre[k],sep =""),           # no terminal plots
                     inner_panel=node_inner(arbol,
                                            abbreviate = FALSE,          # short variable names
                                            pval = TRUE,                # p-values
                                            id = FALSE),
                     terminal_panel=node_terminal(arbol, 
                                                  abbreviate = FALSE,
                                                  digits = 2,                   # few digits on numbers
                                                  fill = c("white"),            #  make box white not grey
                                                  id = FALSE))
              }
              dev.off()
            }
            #text(arbol, use.n=TRUE)  
            # printcp(arbol)
          }
          ####################################
          ## Acceso a recursos              ##
          ####################################
          ## 
          for  (i in c(1:NumCursos)) {
    #        df_recurso <- df_recursos[df_recursos$Curso==i,] 
    #        df_tmp <- subset(df_logs,df_logs$Curso == i & df_logs$Módulo == "Recurso" & df_logs$Acción == "Módulo de curso visto")
    #        df_rescurso$Sum <- sapply(df_tmp$Rec)
    #        df_tmp$Recurso <-sapply(df_tmp$Otros,function (x) {unlist(strsplit(x,"'"))[6]})
    #        
    #        sumrecursos <- table (df_tmp$IdCV,df_tmp$Recurso)
            
            df_accFecha <- df_accFechas[df_accFechas$Curso == i,]
            df_alum <- df_alums_original[df_alums_original$Curso == i,]
            df_log <- df_logs[df_logs$Curso == i,]
            write.csv(df_alum,
                      paste("imagenes/",df_cursos$id[i],"/df_alums_",df_cursos$id[i],".csv",sep=""))
            write.csv(df_log,
                      paste("imagenes/",df_cursos$id[i],"/df_logs_",df_cursos$id[i],".csv",sep=""))
            write.csv(data.frame(df_accFecha[1],apply(df_accFecha[2:NCOL(df_accFecha)],2,as.character)),
                      paste("imagenes/",df_cursos$id[i],"/df_accFechas_",df_cursos$id[i],".csv",sep=""))
            
          }
    #      fich <- paste("imagenes/,",df_cursos$id[i],"/df_alums_",df_cursos$id[i],".csv",sep="")
        
          #######################################
          ## Archivo de intercambio con Carmen ##
          #######################################
          arrange.vars <- function(data, vars){
            ##stop if not a data.frame (but should work for matrices as well)
            stopifnot(is.data.frame(data))
            
            ##sort out inputs
            data.nms <- names(data)
            var.nr <- length(data.nms)
            var.nms <- names(vars)
            var.pos <- vars
            ##sanity checks
            stopifnot( !any(duplicated(var.nms)), 
                       !any(duplicated(var.pos)) )
            stopifnot( is.character(var.nms), 
                       is.numeric(var.pos) )
            stopifnot( all(var.nms %in% data.nms) )
            stopifnot( all(var.pos > 0), 
                       all(var.pos <= var.nr) )
            
            ##prepare output
            out.vec <- character(var.nr)
            out.vec[var.pos] <- var.nms
            out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
            stopifnot( length(out.vec)==var.nr )
            
            ##re-arrange vars by position
            data <- data[ , out.vec]
            return(data)
          }
          df_carmen <- df_alums
          df_carmen$DNI <- NULL
          df_carmen$Apellidos <- NULL
          df_carmen$Nombre <- NULL
          df_carmen$NIF <- NULL
          df_carmen$Correo <- NULL
          df_carmen$Teléfono <- NULL
          df_carmen$email <- NULL
          for (i in 1:NumCursos) {
            df_carmen[df_carmen$Curso == i,"Profesor"] <- as.character(df_cursos[i,"author"])
            df_carmen[df_carmen$Curso == i,"NomAsignatura"] <- as.character(df_cursos[i,"title"])
            df_carmen[df_carmen$Curso == i,"codAsignatura"] <- as.character(df_cursos[i,"id"])
            df_carmen[df_carmen$Curso == i,"CursoAcademico"] <- as.character(df_cursos[i,"anoaca"])
          }
          df_carmen <- arrange.vars(df_carmen, c("codAsignatura"=1, "NomAsignatura"=2, "Profesor"=3,
                                                 "CursoAcademico"=4,"IdCV"=5,"Matrícula"=6,"Convocatoria"=7,
                                                 "Procedencia"=8,"Sexo"=9,"Final_T"=10,"Visitas"=11,"Recursos"=12,
                                                 "AEvaluaciones"=13,"URLs"=14,"Foros"=15,"Tareas"=16,"Glosarios"=17,
                                                 "Wikis"=18,"Curso"=19)
    )
    #      df_carmen <- df_carmen[,c("codAsignatura","NomAsignatura","Profesor","CursoAcademico",2:45)]
          write.csv (df_carmen,paste("imagenes/Carmen.csv"))
          df_carmen1 <- data.frame(matrix(ncol = 4, nrow = 0))
          df_carmen1[] <- lapply(df_carmen1, as.character)
          
          colnames(df_carmen1) <- c("Curso","Columna","Nombre","Valor_Maximo")
    #      df_carmen1 <- c(0,".",",",0)
          df_carmen1 <- as.data.frame(df_carmen1, stringsAsFactors = FALSE)
          for (i in 1:NumCursos) {
            df_grade <- df_grades[df_grades$Curso==i,]
            df_grade[] <- lapply(df_grade, as.character)
            df_carmen1 <- rbind(df_carmen1,c(as.character(df_cursos$id[i]),"Main1",as.character(df_grade$Nombre[1]),as.character(as.numeric(df_grade$Passgrade[1])*2)))
            df_carmen1[] <- lapply(df_carmen1, as.character)
            df_carmen1 <- rbind(df_carmen1,c(as.character(df_cursos$id[i]),"Main2",as.character(df_grade$Nombre[2]),as.character(as.numeric(df_grade$Passgrade[2])*2)))
            df_carmen1[] <- lapply(df_carmen1, as.character)
            for (j in 3:NROW(df_grade)) {
              colum <- paste("Nota",j-2,sep="")
              df_carmen1 <- rbind(df_carmen1,c(as.character(df_cursos$id[i]),colum,as.character(df_grade$Nombre[j]),as.character(as.numeric(df_grade$Passgrade[j])*2)))
              df_carmen1[] <- lapply(df_carmen1, as.character)
            }
          }
          colnames(df_carmen1) <- c("Curso","Columna","Nombre","Valor_Maximo")
          write.csv (df_carmen1,paste("imagenes/Carmen_index.csv"))
  
        
      
      
            library(caret)   
      library(randomForest) 
      set.seed (2020)
      df_alum <- df_alums[df_alums$Curso == i,]
      df_temp <- df_alum[!is.na(df_alum$Final_T),42:50]
      apply(df_temp$Final_T,1,function(x) if (x < 5) {df_temp$Final_T = 0} else {df_temp$Final_T = 1})
      training.ids <- createDataPartition(df_temp$Final_T,p=0.7,list=F)
      mod <- randomForest(x=df_temp[training.ids,-1],
                          y=df_temp[training.ids,1],
                          ntree=500,keep.forest=T)
      pred <- predict(mod,df_temp[-training.ids,],type="class")
      table(df_temp[-training.ids,"Final_T"],pred,dnn=c("Actual","Predicho"))
      
  # Leccion 63  
      library(ROCR)
      probs <- predict(mod, df_temp[-training.ids,],type="prob")
      pred <- prediction(probs[,2],df_temp[-training.ids,"Final_T"])
      perf <- performance(pred,"trpo","fpr")
      plot (perf)










  Tools<-cbind(Tools, Contador=0) 
attach(df_log_new)
  for (i in 1:nrow(Tools)) {
  Tools[i,"Contador"]<-NROW(df_log_new[component == Tools[i,"Component"] & action == Tools[i,"Action"]  & rol != "Profesor",])
}
df_alum_new<-cbind(df_alum_new, Tot_E=0, Tot_A=0,Tot_I=0)
for (i in 1:nrow(Tools)) {
  for (j in 1:nrow(df_alum_new)) {
    switch (Tools[i,"Tipo"],
            E = {
              df_alum_new[j,"Tot_E"]<-df_alum_new[j,"Tot_E"]+NROW(df_log_new[component == Tools[i,"Component"] & action == Tools[i,"Action"]  & df_log_new$DNI == df_alum_new[j,"DNI"],])
            }, 
            A = {
              df_alum_new[j,"Tot_A"]<-df_alum_new[j,"Tot_A"]+NROW(df_log_new[component == Tools[i,"Component"] & action == Tools[i,"Action"]  & df_log_new$DNI == df_alum_new[j,"DNI"],])
            },
            I = {
              df_alum_new[j,"Tot_I"]<-df_alum_new[j,"Tot_I"]+NROW(df_log_new[component == Tools[i,"Component"] & action == Tools[i,"Action"]  & df_log_new$DNI == df_alum_new[j,"DNI"],])
            }
    )
  } 
}
arbol=rpart(Final_T~Tot_A + Tot_E + Tot_I, data=df_alum_new, method="anova",na.action = na.omit)
arbol
printcp(arbol)
rsq.rpart(arbol);
if (GraToFich == "S") {jpeg('imagenes/",df_cursos$id[i],"/020-10.jpg')}
par(mfrow=c(1,1))
rpart.plot(arbol,
           shadow.col="gray",
           type=4, extra=1,
           under=TRUE, fallen.leaves=TRUE,
           digits=3, 
           cex=NULL, tweak=1,
           ##           snip=T,
           ##           box.palette=0, shadow.col=0,
           main=paste("Nota FInal.",df_cursos$anoaca[2],sep=" "))
dev.off()
# Capamos el arbol  
df_dos=subset(df_alum_new,Tot_I > 100)
arbol=rpart(Final_T~Tot_A + Tot_E + Tot_I, data=df_dos, method="anova",na.action = na.omit)
arbol
printcp(arbol)
if (GraToFich == "S") {jpeg('imagenes/017-10-1.jpg')}
par(mfrow=c(1,1))
rpart.plot(arbol,
           shadow.col="gray",
           type=4, extra=1,
           under=TRUE, fallen.leaves=TRUE,
           digits=3, 
           cex=NULL, tweak=1,
           ##           snip=T,
           ##           box.palette=0, shadow.col=0,
           main=paste("Nota FInal.",df_cursos$anoaca[2],sep=" "))
dev.off()
###             ###
##    CTREE      ##
###             ###
if (GraToFich == "S") {jpeg('imagenes/017-11.jpg')}
sbs <- subset(df_alum_new,!is.na(Final_T))
arbol <- ctree(Final_T~Tot_A+Tot_E+Tot_I, data = sbs,controls = ctree_control(minsplit = 50,
                                                                              mincriterion = 0.001))
##arbol=ctree(Julio~Visitas+Tareas+Foros+Recursos+AEvaluaciones,data=sbs,controls = ctree_control(maxsurrogate = 3))
arbol
print(arbol)
plot(arbol,type="simple",           # no terminal plots
     inner_panel=node_inner(arbol,
                            abbreviate = FALSE,          # short variable names
                            pval = TRUE,                # p-values
                            id = FALSE),
     terminal_panel=node_terminal(arbol, 
                                  abbreviate = FALSE,
                                  digits = 2,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE))

dev.off()
if (GraToFich == "S") {jpeg('imagenes/017-11-1.jpg')}
sbs <- subset(df_alum_new,!is.na(Final_T) & Tot_A > 148)
arbol <- ctree(Final_T~Tot_A+Tot_E+Tot_I, data = sbs,controls = ctree_control(minsplit = 50,
                                                                              mincriterion = 0.001))
##arbol=ctree(Julio~Visitas+Tareas+Foros+Recursos+AEvaluaciones,data=sbs,controls = ctree_control(maxsurrogate = 3))
arbol
plot(arbol,type="simple",           # no terminal plots
     inner_panel=node_inner(arbol,
                            abbreviate = FALSE,          # short variable names
                            pval = TRUE,                # p-values
                            id = FALSE),
     terminal_panel=node_terminal(arbol, 
                                  abbreviate = FALSE,
                                  digits = 2,                   # few digits on numbers
                                  fill = c("white"),            # make box white not grey
                                  id = FALSE))

dev.off()
write.csv(Tools,"Tools.csv")
write.csv(df_alum_new,"df_alum_new.csv")
arbol=rpart(Julio~Visitas + Tareas + Foros+ AEvaluaciones,data=df_alum_new, method= "anova",control = rpart.control(cp=0.01))
arbol
par(mfrow=c(1,1))
rpart.plot(arbol,
           shadow.col="gray",
           type=4, extra=1,
           under=TRUE, fallen.leaves=TRUE,
           digits=3, 
           cex=NULL, tweak=1,
           ##           snip=T,
           ##           box.palette=0, shadow.col=0,
           main=paste ("Examen Julio.",df_cursos$anoaca[2], sep=" "))
#text(arbol, use.n=TRUE)
dev.off()
printcp(arbol)

write.csv(df_alum_old, file = "Alum15-16.csv")
write.csv(df_alum_new, file = "Alum16-17.csv")


## N?mero de alumnos que aprueban por parciales
tab_Parciales = matrix(nrow=7,ncol=5)
rownames(tab_Parciales) = c("Aprob. Examen","Suspensos","Aprob. Parcial","Aprob. con +1","Aprob. con +2","Alum.+1","Alum.+2")
#rownames(tab_Parciales) = c("Passed","Failed","Passed Exam","Passed +1","Passed +2","Stud.+1","Stud.+2","")
colnames(tab_Parciales) = c(1:5)
#tab_Parciales <- data.frame(1:5)

tab_Parciales[1,1] <- NROW(df_alum_old[df_alum_old$P1 >= 5 & !is.na(df_alum_old$P1),])
tab_Parciales[2,1] <- NROW(df_alum_old[df_alum_old$P1 < 5 | is.na(df_alum_old$P1),])
tab_Parciales[3,1] <- NROW(df_alum_old[df_alum_old$FP1 >= 5 & !is.na(df_alum_old$FP1),])
tab_Parciales[4,1] <- NROW(df_alum_old[round(df_alum_old$FP1 - df_alum_old$P1,digits=2) <= 1 & round(df_alum_old$FP1 - df_alum_old$P1,digits=2) > 0 &
                                         df_alum_old$P1 < 5 & df_alum_old$FP1 >= 5 & !is.na(df_alum_old$P1) & !is.na(df_alum_old$FP1),])
tab_Parciales[5,1] <- NROW(df_alum_old[round(df_alum_old$FP1 - df_alum_old$P1,digits=2) > 1 &
                                         df_alum_old$P1 < 5 & df_alum_old$FP1 >= 5 & !is.na(df_alum_old$P1) & !is.na(df_alum_old$FP1),])
tab_Parciales[6,1] <- NROW(df_alum_old[round(df_alum_old$FP1 - df_alum_old$P1,digits=2) <= 1 & round(df_alum_old$FP1 - df_alum_old$P1,digits=2) > 0 &
                                         !is.na(df_alum_old$P1) & !is.na(df_alum_old$FP1),])
tab_Parciales[7,1] <- NROW(df_alum_old[round(df_alum_old$FP1 - df_alum_old$P1,digits=2) > 1 & !is.na(df_alum_old$P1) & !is.na(df_alum_old$FP1),])

tab_Parciales[1,2] <- NROW(df_alum_old[df_alum_old$P2 >= 5 & !is.na(df_alum_old$P2),])
tab_Parciales[2,2] <- NROW(df_alum_old[df_alum_old$P2 < 5 | is.na(df_alum_old$P2),])
tab_Parciales[3,2] <- NROW(df_alum_old[df_alum_old$FP2 >= 5 & !is.na(df_alum_old$FP2),])
tab_Parciales[4,2] <- NROW(df_alum_old[round(df_alum_old$FP2 - df_alum_old$P2,digits=2) <= 1 & round(df_alum_old$FP2 - df_alum_old$P2,digits=2) > 0 &
                                         df_alum_old$P2 < 5 & df_alum_old$FP2 >= 5 & !is.na(df_alum_old$P2) & !is.na(df_alum_old$FP2),])
tab_Parciales[5,2] <- NROW(df_alum_old[round(df_alum_old$FP2 - df_alum_old$P2,digits=2) > 1 &
                                         df_alum_old$P2 < 5 & df_alum_old$FP2 >= 5 & !is.na(df_alum_old$P2) & !is.na(df_alum_old$FP2),])
tab_Parciales[6,2] <- NROW(df_alum_old[round(df_alum_old$FP2 - df_alum_old$P2,digits=2) <= 1 & round(df_alum_old$FP2 - df_alum_old$P2,digits=2) > 0 &
                                         !is.na(df_alum_old$P2) & !is.na(df_alum_old$FP2),])
tab_Parciales[7,2] <- NROW(df_alum_old[round(df_alum_old$FP2 - df_alum_old$P2,digits=2) > 1 & !is.na(df_alum_old$P2) & !is.na(df_alum_old$FP2),])

tab_Parciales[1,3] <- NROW(df_alum_old[df_alum_old$P3 >= 5 & !is.na(df_alum_old$P3),])
tab_Parciales[2,3] <- NROW(df_alum_old[df_alum_old$P3 < 5 | is.na(df_alum_old$P3),])
tab_Parciales[3,3] <- NROW(df_alum_old[df_alum_old$FP3 >= 5 & !is.na(df_alum_old$FP3),])
tab_Parciales[4,3] <- NROW(df_alum_old[round(df_alum_old$FP3 - df_alum_old$P3,digits=2) <= 1 & round(df_alum_old$FP3 - df_alum_old$P3,digits=2) > 0 &
                                         df_alum_old$P3 < 5 & df_alum_old$FP3 >= 5 & !is.na(df_alum_old$P3) & !is.na(df_alum_old$FP3),])
tab_Parciales[5,3] <- NROW(df_alum_old[round(df_alum_old$FP3 - df_alum_old$P3,digits=2) > 1 &
                                         df_alum_old$P3 < 5 & df_alum_old$FP3 >= 5 & !is.na(df_alum_old$P3) & !is.na(df_alum_old$FP3),])
tab_Parciales[6,3] <- NROW(df_alum_old[round(df_alum_old$FP3 - df_alum_old$P3,digits=2) <= 1 & round(df_alum_old$FP3 - df_alum_old$P3,digits=2) > 0 &
                                         !is.na(df_alum_old$P3) & !is.na(df_alum_old$FP3),])
tab_Parciales[7,3] <- NROW(df_alum_old[round(df_alum_old$FP3 - df_alum_old$P3,digits=2) > 1 & !is.na(df_alum_old$P3) & !is.na(df_alum_old$FP3),])

tab_Parciales[1,4] <- NROW(df_alum_old[df_alum_old$P4 >= 5 & !is.na(df_alum_old$P4),])
tab_Parciales[2,4] <- NROW(df_alum_old[df_alum_old$P4 < 5 | is.na(df_alum_old$P4),])
tab_Parciales[3,4] <- NROW(df_alum_old[df_alum_old$FP4 >= 5 & !is.na(df_alum_old$FP4),])
tab_Parciales[4,4] <- NROW(df_alum_old[round(df_alum_old$FP4 - df_alum_old$P4,digits=2) <= 1 & round(df_alum_old$FP4 - df_alum_old$P4,digits=2) > 0 &
                                         df_alum_old$P4 < 5 & df_alum_old$FP4 >= 5 & !is.na(df_alum_old$P4) & !is.na(df_alum_old$FP4),])
tab_Parciales[5,4] <- NROW(df_alum_old[round(df_alum_old$FP4 - df_alum_old$P4,digits=2) > 1 &
                                         df_alum_old$P4 < 5 & df_alum_old$FP4 >= 5 & !is.na(df_alum_old$P4) & !is.na(df_alum_old$FP4),])
tab_Parciales[6,4] <- NROW(df_alum_old[round(df_alum_old$FP4 - df_alum_old$P4,digits=2) <= 1 & round(df_alum_old$FP4 - df_alum_old$P4,digits=2) > 0 &
                                         !is.na(df_alum_old$P4) & !is.na(df_alum_old$FP4),])
tab_Parciales[7,4] <- NROW(df_alum_old[round(df_alum_old$FP4 - df_alum_old$P4,digits=2) > 1 & !is.na(df_alum_old$P4) & !is.na(df_alum_old$FP4),])

tab_Parciales[1,5] <- NROW(df_alum_old[df_alum_old$P5 >= 5 & !is.na(df_alum_old$P5),])
tab_Parciales[2,5] <- NROW(df_alum_old[df_alum_old$P5 < 5 | is.na(df_alum_old$P5),])
tab_Parciales[3,5] <- NROW(df_alum_old[df_alum_old$FP5 >= 5 & !is.na(df_alum_old$FP5),])
tab_Parciales[4,5] <- NROW(df_alum_old[round(df_alum_old$FP5 - df_alum_old$P5,digits=2) <= 1 & round(df_alum_old$FP5 - df_alum_old$P5,digits=2) > 0 &
                                         df_alum_old$P5 < 5 & df_alum_old$FP5 >= 5 & !is.na(df_alum_old$P5) & !is.na(df_alum_old$FP5),])
tab_Parciales[5,5] <- NROW(df_alum_old[round(df_alum_old$FP5 - df_alum_old$P5,digits=2) > 1 &
                                         df_alum_old$P5 < 5 & df_alum_old$FP5 >= 5 & !is.na(df_alum_old$P5) & !is.na(df_alum_old$FP5),])
tab_Parciales[6,5] <- NROW(df_alum_old[round(df_alum_old$FP5 - df_alum_old$P5,digits=2) <= 1 & round(df_alum_old$FP5 - df_alum_old$P5,digits=2) > 0 &
                                         !is.na(df_alum_old$P5) & !is.na(df_alum_old$FP5),])
tab_Parciales[7,5] <- NROW(df_alum_old[round(df_alum_old$FP5 - df_alum_old$P5,digits=2) > 1 & !is.na(df_alum_old$P5) & !is.na(df_alum_old$FP5),])
if (GraToFich == "S") {jpeg('imagenes/018.jpg')}
par(mfrow=c(1,1))
barplot(tab_Parciales[c(1,3,6,7),],beside=TRUE,las=1,names.arg=colnames(tab_Parciales),
        main=paste("Resultados por Parciales",df_cursos$anoaca[1],sep=" "),legend.text=rownames(tab_Parciales)[c(1,3,6,7)],
        col=colors[c(1,2,3,4)],ylim=c(0,NROW(df_alum_old)*0.8))
dev.off()
write.csv(tab_Parciales,file="Parciales2015-16")

tab_Parciales[1,1] <- NROW(df_alum_new[df_alum_new$P1 >= 5 & !is.na(df_alum_new$P1),])
tab_Parciales[2,1] <- NROW(df_alum_new[df_alum_new$P1 < 5 | is.na(df_alum_new$P1),])
tab_Parciales[3,1] <- NROW(df_alum_new[df_alum_new$FP1 >= 5 & !is.na(df_alum_new$FP1),])
tab_Parciales[4,1] <- NROW(df_alum_new[round(df_alum_new$FP1 - df_alum_new$P1,digits=2) <= 1 & round(df_alum_new$FP1 - df_alum_new$P1,digits=2) > 0 &
                                         df_alum_new$P1 < 5 & df_alum_new$FP1 >= 5 & !is.na(df_alum_new$P1) & !is.na(df_alum_new$FP1),])
tab_Parciales[5,1] <- NROW(df_alum_new[round(df_alum_new$FP1 - df_alum_new$P1,digits=2) > 1 &
                                         df_alum_new$P1 < 5 & df_alum_new$FP1 >= 5 & !is.na(df_alum_new$P1) & !is.na(df_alum_new$FP1),])
tab_Parciales[6,1] <- NROW(df_alum_new[round(df_alum_new$FP1 - df_alum_new$P1,digits=2) <= 1 & round(df_alum_new$FP1 - df_alum_new$P1,digits=2) > 0 &
                                         !is.na(df_alum_new$P1) & !is.na(df_alum_new$FP1),])
tab_Parciales[7,1] <- NROW(df_alum_new[round(df_alum_new$FP1 - df_alum_new$P1,digits=2) > 1 & !is.na(df_alum_new$P1) & !is.na(df_alum_new$FP1),])

tab_Parciales[1,2] <- NROW(df_alum_new[df_alum_new$P2 >= 5 & !is.na(df_alum_new$P2),])
tab_Parciales[2,2] <- NROW(df_alum_new[df_alum_new$P2 < 5 | is.na(df_alum_new$P2),])
tab_Parciales[3,2] <- NROW(df_alum_new[df_alum_new$FP2 >= 5 & !is.na(df_alum_new$FP2),])
tab_Parciales[4,2] <- NROW(df_alum_new[round(df_alum_new$FP2 - df_alum_new$P2,digits=2) <= 1 & round(df_alum_new$FP2 - df_alum_new$P2,digits=2) > 0 &
                                         df_alum_new$P2 < 5 & df_alum_new$FP2 >= 5 & !is.na(df_alum_new$P2) & !is.na(df_alum_new$FP2),])
tab_Parciales[5,2] <- NROW(df_alum_new[round(df_alum_new$FP2 - df_alum_new$P2,digits=2) > 1 &
                                         df_alum_new$P2 < 5 & df_alum_new$FP2 >= 5 & !is.na(df_alum_new$P2) & !is.na(df_alum_new$FP2),])
tab_Parciales[6,2] <- NROW(df_alum_new[round(df_alum_new$FP2 - df_alum_new$P2,digits=2) <= 1 & round(df_alum_new$FP2 - df_alum_new$P2,digits=2) > 0 &
                                         !is.na(df_alum_new$P2) & !is.na(df_alum_new$FP2),])
tab_Parciales[7,2] <- NROW(df_alum_new[round(df_alum_new$FP2 - df_alum_new$P2,digits=2) > 1 & !is.na(df_alum_new$P2) & !is.na(df_alum_new$FP2),])

tab_Parciales[1,3] <- NROW(df_alum_new[df_alum_new$P3 >= 5 & !is.na(df_alum_new$P3),])
tab_Parciales[2,3] <- NROW(df_alum_new[df_alum_new$P3 < 5 | is.na(df_alum_new$P3),])
tab_Parciales[3,3] <- NROW(df_alum_new[df_alum_new$FP3 >= 5 & !is.na(df_alum_new$FP3),])
tab_Parciales[4,3] <- NROW(df_alum_new[round(df_alum_new$FP3 - df_alum_new$P3,digits=2) <= 1 & round(df_alum_new$FP3 - df_alum_new$P3,digits=2) > 0 &
                                         df_alum_new$P3 < 5 & df_alum_new$FP3 >= 5 & !is.na(df_alum_new$P3) & !is.na(df_alum_new$FP3),])
tab_Parciales[5,3] <- NROW(df_alum_new[round(df_alum_new$FP3 - df_alum_new$P3,digits=2) > 1 &
                                         df_alum_new$P3 < 5 & df_alum_new$FP3 >= 5 & !is.na(df_alum_new$P3) & !is.na(df_alum_new$FP3),])
tab_Parciales[6,3] <- NROW(df_alum_new[round(df_alum_new$FP3 - df_alum_new$P3,digits=2) <= 1 & round(df_alum_new$FP3 - df_alum_new$P3,digits=2) > 0 &
                                         !is.na(df_alum_new$P3) & !is.na(df_alum_new$FP3),])
tab_Parciales[7,3] <- NROW(df_alum_new[round(df_alum_new$FP3 - df_alum_new$P3,digits=2) > 1 & !is.na(df_alum_new$P3) & !is.na(df_alum_new$FP3),])

tab_Parciales[1,4] <- NROW(df_alum_new[df_alum_new$P4 >= 5 & !is.na(df_alum_new$P4),])
tab_Parciales[2,4] <- NROW(df_alum_new[df_alum_new$P4 < 5 | is.na(df_alum_new$P4),])
tab_Parciales[3,4] <- NROW(df_alum_new[df_alum_new$FP4 >= 5 & !is.na(df_alum_new$FP4),])
tab_Parciales[4,4] <- NROW(df_alum_new[round(df_alum_new$FP4 - df_alum_new$P4,digits=2) <= 1 & round(df_alum_new$FP4 - df_alum_new$P4,digits=2) > 0 &
                                         df_alum_new$P4 < 5 & df_alum_new$FP4 >= 5 & !is.na(df_alum_new$P4) & !is.na(df_alum_new$FP4),])
tab_Parciales[5,4] <- NROW(df_alum_new[round(df_alum_new$FP4 - df_alum_new$P4,digits=2) > 1 &
                                         df_alum_new$P4 < 5 & df_alum_new$FP4 >= 5 & !is.na(df_alum_new$P4) & !is.na(df_alum_new$FP4),])
tab_Parciales[6,4] <- NROW(df_alum_new[round(df_alum_new$FP4 - df_alum_new$P4,digits=2) <= 1 & round(df_alum_new$FP4 - df_alum_new$P4,digits=2) > 0 &
                                         !is.na(df_alum_new$P4) & !is.na(df_alum_new$FP4),])
tab_Parciales[7,4] <- NROW(df_alum_new[round(df_alum_new$FP4 - df_alum_new$P4,digits=2) > 1 & !is.na(df_alum_new$P4) & !is.na(df_alum_new$FP4),])

tab_Parciales[1,5] <- NROW(df_alum_new[df_alum_new$P5 >= 5 & !is.na(df_alum_new$P5),])
tab_Parciales[2,5] <- NROW(df_alum_new[df_alum_new$P5 < 5 | is.na(df_alum_new$P5),])
tab_Parciales[3,5] <- NROW(df_alum_new[df_alum_new$FP5 >= 5 & !is.na(df_alum_new$FP5),])
tab_Parciales[4,5] <- NROW(df_alum_new[round(df_alum_new$FP5 - df_alum_new$P5,digits=2) <= 1 & round(df_alum_new$FP5 - df_alum_new$P5,digits=2) > 0 &
                                         df_alum_new$P5 < 5 & df_alum_new$FP5 >= 5 & !is.na(df_alum_new$P5) & !is.na(df_alum_new$FP5),])
tab_Parciales[5,5] <- NROW(df_alum_new[round(df_alum_new$FP5 - df_alum_new$P5,digits=2) > 1 &
                                         df_alum_new$P5 < 5 & df_alum_new$FP5 >= 5 & !is.na(df_alum_new$P5) & !is.na(df_alum_new$FP5),])
tab_Parciales[6,5] <- NROW(df_alum_new[round(df_alum_new$FP5 - df_alum_new$P5,digits=2) <= 1 & round(df_alum_new$FP5 - df_alum_new$P5,digits=2) > 0 &
                                         !is.na(df_alum_new$P5) & !is.na(df_alum_new$FP5),])
tab_Parciales[7,5] <- NROW(df_alum_new[round(df_alum_new$FP5 - df_alum_new$P5,digits=2) > 1 & !is.na(df_alum_new$P5) & !is.na(df_alum_new$FP5),])
if (GraToFich == "S") {jpeg('imagenes/019.jpg')}
par(mfrow=c(1,1))
barplot(tab_Parciales[c(1,3,6,7),],beside=TRUE,las=1,names.arg=colnames(tab_Parciales),
        main=paste("Resultados por Parciales",df_cursos$anoaca[2],sep=" "),legend.text=rownames(tab_Parciales)[c(1,3,6,7)],
        #        args.legend=list(x = "topright", bty = "y", inset=c(-0.015,0.03)),
        col=colors[c(1,2,3,4)],ylim=c(0,NROW(df_alum_new)*0.8))
#barplot(tab_Parciales[c(1,3,4,5),],add=TRUE,beside=FALSE,space=4,col=colors[c(4,5,6,7)],
#        legend.text=rownames(tab_Parciales)[c(2,6,7,1,3,4,5)],args.legend = list(x = "bottomright", bty = "y", inset=c(-0.015, 0.03)),
#        axes=FALSE,names.arg=c("","","","",""))
dev.off()
write.csv(tab_Parciales,file="Parciales2016-17")
#------------------------------------------------------------------------

Aprob_Parciales <- NROW(df_alum_new[df_alum_old$Parciales >= 5 & !is.na(df_alum_old$Parciales),])
## Accesos desde la UCM o desde fuera
ptn = "^147.96.*?" # direcciones de la UCM
ndx=grep(ptn,df_log_new$ip,perl=T)
selected_rows = df_log_new[ndx,]
tab_components <- array(dim=c(nrow=length(unique(df_log_new$component)),2))
dimnames(tab_components) = list( 
  unique(df_log_new$component),   # row names 
  c("Total", "UCM"))              # column names 
for (i in c(1:NROW(tab_components))) {
  tab_components[i,"Total"] <- NROW(df_log_new[df_log_new$component == rownames(tab_components)[i],])
  tab_components[i,"UCM"]   <- NROW(selected_rows[selected_rows$component == rownames(tab_components)[i],])
}
barplot(tab_components,beside=T,horiz=F,col=rainbow(dim(tab_components)[1]),las=2)

########################################################################################################
########################################################################################################
########################################################################################################
# Recta de regresi?n entre la nota final y el sexo
reg0<-lm(df_alum_old$Final_T~df_alum_old$Sexo)
summary(reg0)
par(mfrow=c(2,2))
plot(reg0)
# Recta de regresi?n entre las notas del parcial y la nota final
reg1<-lm(df_alum_old$P1~df_alum_old$FP1-df_alum_old$P1)
summary(reg1)


par(mfrow=c(2,2))
plot(reg1)
par(oma=c(1,1,1,1),new=T,font=2,cex=0.5)
mtext(outer=T,"Gráficos básicos Regresión simple Primer Parcial",side=3)
predict(reg1,se.fit = TRUE)
#obtiene valores de predicci?on y error estandar
temp1<- predict(reg1,interval="prediction")
temp2<- predict(reg1,interval="confidence")

par(mfrow=c(1,1))
km <- kmeans(df_alum_old$Posts,center=3)
plot(df_alum_old$Posts,col=km$cluster, main="Clusterizaci?n de la nota Final")
points(km$center,col=1:2,pch=8,cex=1)

km <- kmeans(df_alum_old$Visitas,center=5,iter.max = 10)
plot(df_alum_old$Visitas,col=km$cluster, main="Clusterizaci?n del N? de Visitas")
points(km$center,col=1:2,pch=8,cex=1)

## Valoraci?n por R cuadrado ##################################
##   Para saber si es significativo el coeficente de r debe ser
##   Entre 0   y 0,4 --> Pobre
##     "   0.4 y 0.7 --> Regular
##     "   0.7 y 0.9 --> Alta
##     "   0.9 y 1.0 --> Muy Alta
summary(lm(df_alum_old$Visitas~df_alum_old$Parciales))$r.squared
summary(lm(df_alum_old$Visitas~df_alum_old$Parciales))$adj.r.squared

par(mfrow=c(1,2))
summary(lm(df_alum_old$P1~df_alum_old$AE1))$r.squared
summary(lm(df_alum_old$P1~df_alum_old$AE1))$adj.r.squared
plot(df_alum_old$P1[!is.na(df_alum_old$P1)],df_alum_old$AE1[!is.na(df_alum_old$P1)],xlab = "Parcial",ylab = "Autoevaluación",main="PARCIAL 1")  
summary(lm(df_alum_old$P2~df_alum_old$AE2))$r.squared
summary(lm(df_alum_old$P2~df_alum_old$AE2))$adj.r.squared
plot(df_alum_old$P2[!is.na(df_alum_old$P2)],df_alum_old$AE2[!is.na(df_alum_old$P2)],xlab = "Parcial",ylab = "Autoevaluación",main="PARCIAL 2") 
summary(lm(df_alum_old$P3~df_alum_old$AE3))$r.squared
summary(lm(df_alum_old$P3~df_alum_old$AE3))$adj.r.squared
plot(df_alum_old$P3[!is.na(df_alum_old$P3)],df_alum_old$AE3[!is.na(df_alum_old$P3)],xlab = "Parcial",ylab = "Autoevaluación",main="PARCIAL 3") 
summary(lm(df_alum_old$P4~df_alum_old$AE4))$r.squared
summary(lm(df_alum_old$P4~df_alum_old$AE4))$adj.r.squared
plot(df_alum_old$P4[!is.na(df_alum_old$P4)],df_alum_old$AE4[!is.na(df_alum_old$P4)],xlab = "Parcial",ylab = "Autoevaluación",main="PARCIAL 4") 
summary(lm(df_alum_old$P5~df_alum_old$AE5))$r.squared
summary(lm(df_alum_old$P5~df_alum_old$AE5))$adj.r.squared
plot(df_alum_old$P5[!is.na(df_alum_old$P5)],df_alum_old$AE5[!is.na(df_alum_old$P5)],xlab = "Parcial",ylab = "Autoevaluación",main="PARCIAL 5") 
plot(df_alum_old$Parciales[!is.na(df_alum_old$Parciales)],df_alum_old$Visitas[!is.na(df_alum_old$Parciales)],xlab = "Notas Parciales",ylab = "Visitas",main="Visitas x Parciales")  
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$AE1[!is.na(df_alum_old$AE1)],df_alum_old$Visitas[!is.na(df_alum_old$AE1)],xlab = "Autoevaluación 1",ylab = "Visitas",main="Visitas x Autoevaluación 1")
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$AE2[!is.na(df_alum_old$AE2)],df_alum_old$Visitas[!is.na(df_alum_old$AE2)],xlab = "Autoevaluación 2",ylab = "Visitas",main="Visitas x Autoevaluación 2")
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$AE3[!is.na(df_alum_old$AE3)],df_alum_old$Visitas[!is.na(df_alum_old$AE3)],xlab = "Autoevaluación 3",ylab = "Visitas",main="Visitas x Autoevaluación 3")
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$AE4[!is.na(df_alum_old$AE4)],df_alum_old$Visitas[!is.na(df_alum_old$AE4)],xlab = "Autoevaluación 4",ylab = "Visitas",main="Visitas x Autoevaluación 4")
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$AE5[!is.na(df_alum_old$AE5)],df_alum_old$Visitas[!is.na(df_alum_old$AE5)],xlab = "Autoevaluación 5",ylab = "Visitas",main="Visitas x Autoevaluación 5")
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$Junio[!is.na(df_alum_old$Junio)],         df_alum_old$Visitas[!is.na(df_alum_old$Junio)],xlab = "Nota Junio",ylab = "Visitas",main="Visitas x N.Junio")  
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$Julio[!is.na(df_alum_old$Julio)] ,        df_alum_old$Visitas[!is.na(df_alum_old$Julio)],xlab = "Nota Julio",ylab = "Visitas",main="Visitas x N.Julio")  
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")
plot(df_alum_old$N_Final[!is.na(df_alum_old$N_Final)],     df_alum_old$Visitas[!is.na(df_alum_old$N_Final)],xlab = "Nota Final",ylab = "Visitas",main="Visitas x N.Final")
abline(h=mean(as.numeric(df_alum_old$Visitas)), col="red")

## Valoración por árbol de decisión
arbol=rpart(Final_T~Visitas + Tareas + Foros+ AEvaluaciones,data=df_alum_old)
arbol
rpart.plot(arbol); text(arbol, use.n=TRUE)
printcp(arbol)

arbol=rpart(df_alum_old$Parciales~df_alum_old$Visitas,data=df_alum_old)
arbol
rpart.plot(arbol); text(arbol, use.n=TRUE)
printcp(arbol)

arbol=rpart(df_alum_old$P1~df_alum_old$AE1,data=df_alum_old)
arbol
rpart.plot(arbol,type=4,extra=1); text(arbol, use.n=TRUE)
printcp(arbol)

arbol=rpart(df_alum_old$P5~df_alum_old$AE5,data=df_alum_old)
arbol
rpart.plot(arbol,type=4,extra=1); text(arbol, use.n=TRUE)
printcp(arbol)


rsq <- function(x, y) summary (lm(as.numeric(y)~as.numeric(x))$r.squared
                               rsq (df_alum_old$Visitas,df_alum_old$N_Final)
                               
                               hist(as.numeric(df_alum_old$Visitas),df_alum_old$Sexo)
                               plot(df_alum_old$Sexo,df_alum_old$N_Final)
                               t.test (as.numeric(df_alum_old$Visitas),as.numeric(df_alum_old$N_Final))
                               
                               dnis <- row.names(unique(df_log_old[c("DNI")]))
                               df1 <- df_log_old[dnis,]                         # Obtengo los distintos DNI's de usuarios que acceden al curso
                               df_alum_old$IdMoodle <- df_log_old$idUsuario[match(df_alum_old$DNI,df_log_old$DNI)]
                               
                               table1$val2 <- table2$val2[match(table1$pid, table2$pid)]
                               df2 <- merge(x = df_alum_old, y = df1, by = "DNI", all.x  = TRUE)
                               
                               
                               df1levels(df$Nombre.evento)  # Muestra los nombres de los eventos
                               # Comprobamos la fecha de los registros y 
                               df<- df[order(df$Hora),]
                               fecha <- as.Date(df$Hora[[1]],"%d/%m/%Y")
                               if (fecha < as.Date("01/10/2017","%d/%m/%Y")) {log<- "Heredado"} else {log<-"Actual"}
                               ##if (log="Heredado") {
                               df_Sis <- subset (df, Componente=="Sistema")
                               levels(df_Sis$Nombre.evento)
                               df_Sis_Sect <- subset (df_Sis, Nombre.evento=="course_view section")  
                               dat1 <- data.frame(t(matrix(
                                 unlist(strsplit(as.vector(df_Sis_Sect$Descripción), split = " ")), 
                                 ncol = length(df_Sis_Sect$Descripción), nrow = 4)))    # Divido el campo Descripción en cuatro columnas.
                               df_Sis_Sect["UserId"]   <-dat1[2]
                               df_Sis_Sect["SeccionId"]<-dat1[4]
                               head(df_Sis_Sect)
                               
                               str(df_Sis_Sect)      # Comprusummary(df)  # Veo resumen de datos
                               
                               counts <- table(df_Sis_Sect$SeccionId)
                               barplot(counts)
                               ##  hist(as.numeric(df_Sis_Sect$SeccionId)) #accesos por Sección
                               ##
                               ## Resumen de recursos leidos
                               ##
                               df_Rec <- subset (df, Componente=="Recurso")
                               levels(df_Rec$Nombre.evento)
                               df_Rec_View <- subset (df_Rec, Nombre.evento=="resource_view")  
                               dat1 <- data.frame(t(matrix(
                                 unlist(strsplit(as.vector(df_Rec_View$Descripción), split = " ")), 
                                 ncol = length(df_Rec_View$Descripción), nrow = 3)))    # Divido el campo Descripción en cuatro columnas.
                               ##  df_Sis_Sect["UserId"]   <-dat1[2]
                               df_Rec_View["SeccionId"]<-dat1[3]
                               head(df_Rec_View)
                               
                               str(df_Rec_View)      # Comprusummary(df)  # Veo resumen de datos
                               
                               counts <- table(df_Rec_View$Contexto.del.evento)
                               barplot(counts)
                               
                               } else {
                                 
                               }


# Log heredados
# df <- subset (df, Nombre.evento=="Curso visto" & grepl("viewed the section number+", df$Descripción))
dat1 <- data.frame(t(matrix(
  unlist(strsplit(as.vector(df2$Descripción), split = " ")), 
  ncol = length(df$Descripción), nrow = 4)))    # Divido el campo Descripción en cuatro columnas.
# df["UserId"] <-dat1[2]
df["SeccionId"]<-dat1[4]
head(df)

str(df)      # Comprusummary(df)  # Veo resumen de datos

counts <- table(df$SeccionId)
barplot(counts)
hist(as.numeric(df$SeccionId)) #accesos por Sección
hist(df$SeccionId) #accesos por Sección
df$SeccionId[df$SeccionId=="-1"]<-"0"
df[SeccionId == "-1"]
M<-max(as.numeric(levels(df$SeccionId)))
m<-min(as.numeric(levels(df$SeccionId)))
levels(df$SeccionId)<-ordered(levels(df$SeccionId))
bymedian <- with(df$SeccionId, reorder(c(m,M)))
