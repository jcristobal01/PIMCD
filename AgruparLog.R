df_1 <- read.csv("../Data/20-473738 Registros_1.csv",header=T,sep=";",stringsAsFactors = F)
df_2 <- read.csv("../Data/20-473738 Registros_2.csv",header=T,sep=",",stringsAsFactors = F)
df_1 <- rbind(df_1,df_2)
write.csv(df_1,"../Data/20-473738 Registros.csv",row.names = F)
df_1 <- read.csv("../Data/20-473738 Registros.csv")
