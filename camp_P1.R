#############################################################
######### Campeonatos TC con sistema de puntaje P1. #########
#############################################################

# en la primera parte se descargan los archivos .xsl de actc.org.ar y se convierten a .csv
# (por alguna razon no funciono abrir los .xls)

# luego se realiza el conteo de puntos y graficado

# Al final se encuentra la funcion CampeonatoP1, para volver a realizar los graficos sin
# volver a descargar los archivos.

## CORRER EL SCRIP Y SEGUIR INSTRUCCIONES DE CONSOLA  

rm(list = ls())
library(readxl)
library(ggplot2)

ruta = getwd()

### DESCARGA Y CONVERSION DE ARCHIVOS DE ACTC.ORG.AR ###

print(paste("Los archivos seran guardados en: ", paste(getwd(),"/files_actc", sep = "")))
campeonato = readline("Campeonato: ")
while(campeonato != 2017 & campeonato != 2018 & campeonato != 2019 & campeonato != 2020){
  campeonato = readline("Ingresar campeonato entre 2017-2020: ")
}

#crea carpeta donde se guardaran los archicos
system(command = "mkdir files_actc") 

#numeros de las fechas en la pagina de actc
# aca habria que modificar si se queire incluir mas años, el problema es que se deben revisar
# "a mano", ya que en los casos de las fechas especiales puede cambiar el patron
# ademas se debe saber que fechas no tuvieron clasificacion o series.

cam_disp = c(2017,2018,2019,2020)
cam_fechas = list()
cam_fechas[[2017]] = seq(568,582)
cam_fechas[[2018]] = seq(669,683)
cam_fechas[[2019]] = c(829:832, 847:857)
cam_fechas[[2020]] = c(967,968,970,1062,971,972,1068,973,974,975,976)

#fechas especiales sin series ni clasificacio (1000K y carrera de las estrellas)
cam_fechas_esp = list(); cam_fechas_esp[[2017]] = c(575,576)
cam_fechas_esp[[2018]] = c(675,677); cam_fechas_esp[[2019]] = c(852,0)
cam_fechas_esp[[2020]] = c(0,0) # ninguna en 2020


# listado de piloto y numero correspondiente al campeonato seleccionado
URL = paste("https://www.actc.org.ar/upload/htmls/ppcev/Campeonato%20",campeonato,"%20TC.xls", sep = "")
destfile = paste(ruta,"/files_actc/cham_", campeonato,".xls", sep = "")
download.file(URL, destfile)

campeonato = as.numeric(campeonato)

# Descarga
for(i in cam_fechas[[campeonato]]){
  
  if(campeonato == 2020){ #caso especial, nombres de archivos no siguen el patron en las fechas dobles
    
    URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=8", sep = "")
    destfile = paste(ruta,"/files_actc/race_",i,".xls", sep = "")
    download.file(URL, destfile)
    
    if(i == 967 & i == 968){ # unicas dos fechas "normales"
      
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=2", sep = "")
      destfile = paste(ruta,"/files_actc/q1_",i,".xls", sep = "")
      download.file(URL, destfile)
      
      # general
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=4", sep = "")
      destfile = paste(ruta,"/files_actc/q2_",i,".xls", sep = "")
      download.file(URL, destfile)
      
    } else if(i == 970 | i == 1068){ 
      
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=239", sep = "")
      destfile = paste(ruta,"/files_actc/q1_",i,".xls", sep = "")
      download.file(URL, destfile)
      
    } else if(i == 1062){
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=970&idround=240", sep = "")
      destfile = paste(ruta,"/files_actc/q1_1062",".xls", sep = "")
      download.file(URL, destfile)
    } else if(i == 973){
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=1068&idround=240", sep = "")
      destfile = paste(ruta,"/files_actc/q1_1068",".xls", sep = "")
      download.file(URL, destfile)
    } else if(i == 971){
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=2", sep = "")
      destfile = paste(ruta,"/files_actc/q1_",i,".xls", sep = "")
      download.file(URL, destfile)
    } else if(i == 973 | i == 974 | i == 976){
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=243", sep = "")
      destfile = paste(ruta,"/files_actc/q1_",i,".xls", sep = "")
      download.file(URL, destfile)
    }
    
    for(s in 5:7){
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=", i, "&idround=",s, sep = "")
      destfile = paste(ruta,"/files_actc/serie",s-4,"_", i, ".xls", sep = "")
      download.file(URL, destfile)
      
    }
    
  } else {
    
    # Carreras: 
    URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=8", sep = "")
    destfile = paste(ruta,"/files_actc/race_",i,".xls", sep = "")
    download.file(URL, destfile)
    
    if(i != cam_fechas_esp[[campeonato]][1] & i != cam_fechas_esp[[campeonato]][2]){
      
      # Clasificaciones:
      # 1 
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=2", sep = "")
      destfile = paste(ruta,"/files_actc/q1_",i,".xls", sep = "")
      download.file(URL, destfile)
      
      # general
      URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=",i,"&idround=4", sep = "")
      destfile = paste(ruta,"/files_actc/q2_",i,".xls", sep = "")
      download.file(URL, destfile)
      
      # Series:
      for(s in 5:7){
        URL = paste("https://www.actc.org.ar/modules/ppcev/downloadexcel.php?idrace=", i, "&idround=",s, sep = "")
        destfile = paste(ruta,"/files_actc/serie",s-4,"_", i, ".xls", sep = "")
        download.file(URL, destfile)
        
      }
    }
  }  

}


# Conversion de xls a csv (usando LINUX) y eliminacion de los xls
setwd(paste(ruta, "/files_actc", sep = ""))
system(command = "for i in *.xls; do soffice --headless --convert-to csv $i; done",intern = T)
system(command = "rm *.xls")


###### CAMPEONATO ######

# factor por el que se multiplicara el puntaje de las carreras especiales (1000k y c. de las estrellas) corridas en el campeonato
mult = as.numeric(readline("1000K y carrera de las estrellas, puntaje doble (2), puntaje y medio (1.5) o simple (1): "))

pilotos = as.data.frame(read.csv(paste("cham_", campeonato,".csv", sep = ""), header = F, skip = 2))[,2:3]

# correccion de tildes en los nombres de los pilotos
wierd_char = c("\xed", "\xe9", "\xf3", "\xe1")
correc_char = c("i", "e", "o", "a")

for(i in 1:length(wierd_char)){
  pilotos[,2] = gsub(wierd_char[i], correc_char[i], pilotos[,2])
}

# solo apellidos
pilotos[,2] = gsub("^(.*?),.*", "\\1", as.factor(pilotos[,2]))

camP1 = cbind(pilotos, 0)
camP1[,1] = as.numeric(camP1[,1])
camP1[,2] = as.character(camP1[,2])
ganadores = vector()

# asignacion de puntaje
for(f in cam_fechas[[campeonato]]){
  
  # PUNTAJE P1.
  puntaje = list()
  puntaje[[1]] = c(25,23,22,21,20,14,13,12,11,10,5,5,5,5,5,1,1,1,1,1) # puntaje de carrera
  puntaje[[2]] = c(3,2,1) # puntaje series
  puntaje[[3]] = 1 # puntaje clasificacion
  if(campeonato == 2020 & f != 967 & f != 968){ # para el 2020, solo las fechas 1 y 2 tuvieron dos clasificaciones
    puntaje[[3]] = 2 # puntaje para una sola clasificacion
  }
  
  
  if(f == cam_fechas[[campeonato]][1] | f == cam_fechas[[campeonato]][length(cam_fechas[[campeonato]])]){
    puntaje = Map("*", puntaje, 2) # PRIMERA Y ULTIMA CON PUNTAJE DOBLE
  }
  
  if(f == cam_fechas_esp[[campeonato]][1] | f == cam_fechas_esp[[campeonato]][2]){
    puntaje = Map("*", puntaje, mult) # fechas especiales dependiendo de lo elegido antes
  }
  
  
  print(puntaje[[1]][1])
  evento = Sys.glob(paste("*", f, ".csv", sep = "")) # seleccion de todos los eventos de una fecha (clasificaciones, series y final)
  # clasifica
  
  for(e in 1:length(evento)){
    
    if(grepl("q", evento[e])){ # CLASIFICACIONES
      
      x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[1,2]))
      
      i = 1
      while(is.na(x)){
        x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[i,2]))
        i = i + 1
      }
      
      camP1[which(camP1 == x),3] = camP1[which(camP1 == x),3] + puntaje[[3]]
      
      rm(x)
      
      camP1 = camP1[1:48,,]
      
    }  
    if(grepl("serie", evento[e])){ # SERES
      
      x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[1:3,2]))
      i = 1
      while(is.na(x)){
        x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[(1+i):(3+i),2]))
        i = i + 1
      }
      for(p in 1:3){
        camP1[which(camP1 == x[p]),3] = camP1[which(camP1 == x[p]),3] + puntaje[[2]][p]
      }
      rm(x)
    }
    camP1 = camP1[1:48,,]
    
    if(grepl("race", evento[e])){ # CARRERAS
      
      x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[1:20,2]))
      
      i = 1
      while(is.na(x[1])){
        x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[(1+i):(20+i),2]))
        i = i + 1
      }
      
      ganadores[f] = x[1] # se guardan todos los ganadores, incluso si son repetidos
      
      for(p in 1:20){
        camP1[which(camP1 == x[p]),3] = camP1[which(camP1 == x[p]),3] + puntaje[[1]][p]
      }
      rm(x)
      camP1 = camP1[1:48,,]
    }
    
  }
}


# se eliminan las victorias repetidas y a cada ganador se le asignan + 100 puntos (por unica vez)
# 
for(i in 1:length(as.numeric(levels(as.factor(ganadores))))){
  camP1[which(camP1 == as.numeric(levels(as.factor(ganadores)))[i]),3] =  camP1[which(camP1 == as.numeric(levels(as.factor(ganadores)))[i]),3] + 100
}


camP1 = camP1[1:48,,] # por las dudas, aveces falla y agrega mas filas. ¿!?


# ordenamiento de lista de pilotos segun el puntaje
camP1 = camP1[order(camP1[,3], decreasing = T),]

camP1 = cbind(camP1, seq(1,48, by = 1))
colnames(camP1) = c("Nº", "Piloto", "Puntos", "Pos")



# graficado
puntoslim = max(camP1[,3])
while(puntoslim != 300 & puntoslim != 350 & puntoslim != 400 & puntoslim != 450 & puntoslim != 500 & puntoslim != 550){
  puntoslim = puntoslim + 1
}
p<-ggplot(data = camP1[0:20,], aes(x=rev(Pos), y=Puntos, fill = Piloto, colour = Piloto)) +
  theme_minimal()+
  geom_bar(stat = "identity", width=1, show.legend = F) +
  geom_text(aes(label=Piloto), vjust=0.2, color="black",
            size=5.4, hjust = 1) +
  scale_x_continuous(limits = c(0, 21),breaks =  seq(1,20, by = 1), labels = as.character(rev(seq(1,20))), name = "")+
  scale_y_continuous(limits = c(0,puntoslim),breaks = seq(0,puntoslim,by = 50), name = "Puntos")+
  
  
  ggtitle(paste("Campeonato ", campeonato, " TC - Sistema P1 (carr_espX", mult, ")", sep = ""))+
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 3),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5))
p = p + coord_flip()

setwd(ruta)
system(command = "mkdir CampeonatoP1") # carpeta dodne se guardarabn los graficos
ggsave(filename = paste(ruta,"/CampeonatoP1/CamP1_TC-", campeonato, "car_espX", mult, ".jpg", sep = ""), plot = p, width = 8, height = 6)
print(paste("Imagen guardad en ", getwd(), "/CampeonatoP1", sep = ""))



#######################################################
#----- SI YA SE TIENEN LOS ARCHIVOS descargados: -----#
#######################################################

CampeonatoP1 = function(){
  
#setwd(paste(getwd(), "/files_actc", sep = ""))
ruta = getwd()

print(paste("Los archivos seran guardados en: ", getwd()))
campeonato = readline("Campeonato: ")
while(campeonato != 2017 & campeonato != 2018 & campeonato != 2019 & campeonato != 2020){
  campeonato = readline("Ingresar campeonato entre 2017-2020: ")
}

cam_disp = c(2017,2018,2019,2020)
cam_fechas = list(); cam_fechas[[2017]] = seq(568,582); cam_fechas[[2018]] = seq(669,683)
cam_fechas[[2019]] = c(829:832, 847:857)
cam_fechas[[2020]] = c(967,968,970,1062,971,972,1068,973,974,975,976)

#fechas especiales sin series ni clasificacio (1000K y carrera de las estrellas)
cam_fechas_esp = list(); cam_fechas_esp[[2017]] = c(575,576)
cam_fechas_esp[[2018]] = c(675,677); cam_fechas_esp[[2019]] = c(852,0); cam_fechas_esp[[2020]] = c(0,0)

campeonato = as.numeric(campeonato)

mult = as.numeric(readline("1000K y carrera de las estrellas, puntaje doble (2), puntaje y medio (1.5) o simple (1): "))

pilotos = as.data.frame(read.csv(paste(ruta,"/files_actc/cham_", campeonato,".csv", sep = ""), header = F, skip = 2))[,2:3]

# correccion nombres
wierd_char = c("\xed", "\xe9", "\xf3", "\xe1")
correc_char = c("i", "e", "o", "a")

for(i in 1:length(wierd_char)){
  pilotos[,2] = gsub(wierd_char[i], correc_char[i], pilotos[,2])
}
pilotos[,2] = gsub("^(.*?),.*", "\\1", as.factor(pilotos[,2]))

camP1 = cbind(pilotos, 0)
camP1[,1] = as.numeric(camP1[,1])
camP1[,2] = as.character(camP1[,2])
ganadores = vector()

setwd(paste(ruta, "/files_actc", sep = ""))
fecha = 1
for(f in cam_fechas[[campeonato]]){
  
  puntaje = list()
  puntaje[[1]] = c(25,23,22,21,20,14,13,12,11,10,5,5,5,5,5,1,1,1,1,1) # puntaje de carrera
  puntaje[[2]] = c(3,2,1) # puntaje series
  puntaje[[3]] = 1 # puntaje clasificacion
  if(campeonato == 2020 & f != 967 & f != 968){
    puntaje[[3]] = 2 # puntaje clasificacion
  }
  
  
  
  if(f == cam_fechas[[campeonato]][1] | f == cam_fechas[[campeonato]][length(cam_fechas[[campeonato]])]){
    puntaje = Map("*", puntaje, 2)
  }
  
  if(f == cam_fechas_esp[[campeonato]][1] | f == cam_fechas_esp[[campeonato]][2]){
    puntaje = Map("*", puntaje, mult)
  }
  
  
  evento = Sys.glob(paste("*", f, ".csv", sep = ""))
  # clasifica
  
  for(e in 1:length(evento)){
    #Clasificaciones
    if(grepl("q", evento[e])){
      
      x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[1,2]))
      
      i = 1
      while(is.na(x)){
        x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[i,2]))
        i = i + 1
      }
      
      camP1[which(camP1 == x),3] = camP1[which(camP1 == x),3] + puntaje[[3]]
      
      rm(x)
      
      camP1 = camP1[1:48,,]
      
    }  #Series
    if(grepl("serie", evento[e])){
      
      x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[1:3,2]))
      i = 1
      while(is.na(x)){
        x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[(1+i):(3+i),2]))
        i = i + 1
      }
      for(p in 1:3){
        camP1[which(camP1 == x[p]),3] = camP1[which(camP1 == x[p]),3] + puntaje[[2]][p]
      }
      rm(x)
    }
    camP1 = camP1[1:48,,]
    #Carreras
    if(grepl("race", evento[e])){
      
      x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[1:20,2]))
      
      i = 1
      while(is.na(x[1])){
        x = as.numeric(as.character(as.data.frame(read.csv(evento[e], skip = 4, header = F))[(1+i):(20+i),2]))
        i = i + 1
      }
      
      ganadores[f] = x[1]
      
      for(p in 1:20){
        camP1[which(camP1 == x[p]),3] = camP1[which(camP1 == x[p]),3] + puntaje[[1]][p]
      }
      rm(x)
      camP1 = camP1[1:48,,]
    }
    
  }
  print(paste("Fecha", fecha))
        fecha = fecha + 1
        

}

as.numeric(levels(as.factor(ganadores)))

for(i in 1:length(as.numeric(levels(as.factor(ganadores))))){
  camP1[which(camP1 == as.numeric(levels(as.factor(ganadores)))[i]),3] =  camP1[which(camP1 == as.numeric(levels(as.factor(ganadores)))[i]),3] + 100
}


camP1 = camP1[1:48,,]

#camP12 = camP1

camP1 = camP1[order(camP1[,3], decreasing = T),]
#camP1[,2] = as.factor(camP1[,2])
camP1 = cbind(camP1, seq(1,48, by = 1))
colnames(camP1) = c("Nº", "Piloto", "Puntos", "Pos")

puntoslim = max(camP1[,3])
while(puntoslim != 300 & puntoslim != 350 & puntoslim != 400 & puntoslim != 450 & puntoslim != 500 & puntoslim != 550){
  if(mult == 1.5){
    puntoslim = puntoslim + 0.5
  }
  puntoslim = puntoslim + 1
}


library(ggplot2)
p<-ggplot(data = camP1[0:20,], aes(x=rev(Pos), y=Puntos, fill = Piloto, colour = Piloto)) +
  theme_minimal()+
  geom_bar(stat = "identity", width=1, show.legend = F) +
  geom_text(aes(label=Piloto), vjust=0.2, color="black",
            size=5.4, hjust = 1) +
  scale_x_continuous(limits = c(0, 21),breaks =  seq(1,20, by = 1), labels = as.character(rev(seq(1,20))), name = "")+
  scale_y_continuous(limits = c(0,puntoslim),breaks = seq(0,puntoslim,by = 50), name = "Puntos")+
  
  
  ggtitle(paste("Campeonato ", campeonato, " TC - Sistema P1 (carr_espX", mult, ")", sep = ""))+
  theme(axis.text.y   = element_text(size = 14), axis.text.x   = element_text(size = 14), axis.title.y  = element_text(size = 14),
        axis.title.x  = element_text(size = 14),
        panel.border = element_rect(colour = "black", fill = NA, size = 3),
        panel.ontop = F,
        plot.title = element_text(hjust = 0.5))
p = p + coord_flip()

setwd(ruta)
system(command = "mkdir CampeonatoP1") # carpeta dodne se guardarabn los graficos
ggsave(filename = paste(ruta,"/CampeonatoP1/CamP1_TC-", campeonato, "car_espX", mult, ".jpg", sep = ""), plot = p, width = 8, height = 6)
print(paste("Imagen guardad en ", getwd(), "/CampeonatoP1", sep = ""))

return(p)

}
