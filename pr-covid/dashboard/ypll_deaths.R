library(tidyverse)
library(pacman)
library(readxl)
library(dplyr)
library(data.table)
library(lubridate)
library(scales)
library(epiR)
library(gtsummary)
library(openxlsx)
library(splines)
library(knitr)


#Leemos los archivos de resumen de muerte de casos por Covid, desde la pagina del departamento de salud
json_file <- "https://bioportal.salud.pr.gov/api/administration/reports/deaths/summary"


ypll <- jsonlite::fromJSON(json_file) %>%
  mutate(age_start = as.numeric(str_extract(ageRange, "^\\d+")),
         age_end = as.numeric(str_extract(ageRange, "\\d+$")))

#ypll <- jsonlite::fromJSON(json_file) 

#El formato de fecha aparece como YYYY:MM:DD:HH:MM, y lo cambiamos para solo YYYY:MM:HH
ypll$deathDate<- strftime(ypll$deathDate, format="%Y-%m-%d")

#Los municipios estan encasillados en una de 6 categorias: Metro, Mayaguez, Ponce, Caguas, Fajardo, Bayamon
#Usamos estas funciones para sacarlos de la categoria y aparezcan todos los municipios
#con sus datos respectivos.

sanJuan <- ypll %>%
  mutate(region = recode(region, Metro = 'San Juan'))
  sanJuan <- subset(sanJuan, region!= "Mayaguez" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")

carolina <- ypll %>%
  mutate(region = recode(region, Metro = 'Carolina'))
  carolina <- subset(carolina, region!= "Mayaguez" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")

guaynabo <- ypll %>%
  mutate(region = recode(region, Metro = 'Guaynabo'))
  guaynabo <- subset(guaynabo, region!= "Mayaguez" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
  
loiza <- ypll %>%
  mutate(region = recode(region, Metro = 'Loiza'))
  loiza <- subset(loiza, region!= "Mayaguez" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")

trujilloAlto <- ypll %>%
  mutate(region = recode(region, Metro = 'Trujillo Alto'))
  trujilloAlto <- subset(trujilloAlto, region!= "Mayaguez" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
  
canovanas <- ypll %>%
  mutate(region = recode(region, Metro = 'Canovanas'))
  canovanas <- subset(canovanas, region!= "Mayaguez" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
  
#----------------------------------------------------------------
  
isabela <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Isabela"))
    isabela <- subset(isabela, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
aguadilla <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Aguadilla"))
    aguadilla <- subset(aguadilla, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
moca <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Moca"))
    moca <- subset(moca, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")

sanSebastian <- ypll %>%
    mutate(region = recode(region, Mayaguez = "San Sebastian"))
    sanSebastian <- subset(sanSebastian, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
aguada <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Aguada"))
    aguada <- subset(aguada, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
rincon <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Rincon"))
    rincon <- subset(rincon, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
anasco <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Anasco"))
    anasco <- subset(anasco, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
lasMarias <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Las Marias"))
    lasMarias <- subset(lasMarias, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
maricao <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Maricao"))
    maricao <- subset(maricao, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
hormigueros <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Hormigueros"))
    hormigueros <- subset(hormigueros, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
sanGerman <- ypll %>%
    mutate(region = recode(region, Mayaguez = "San German"))
    sanGerman <- subset(sanGerman, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
    
sabanaGrande <- ypll %>%
    mutate(region = recode(region, Mayaguez = "Sabana Grande"))
    sabanaGrande <- subset(sabanaGrande, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
caboRojo <- ypll %>%
      mutate(region = recode(region, Mayaguez = "Cabo Rojo"))
      caboRojo <- subset(caboRojo, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
        
lajas <- ypll %>%
      mutate(region = recode(region, Mayaguez = "Lajas"))
      lajas <- subset(lajas, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Ponce" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      

#----------------------------------------------------------------
      
guanica <- ypll %>%
      mutate(region = recode(region, Ponce = "Guanica"))
      guanica <- subset(guanica, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
yauco <- ypll %>%
      mutate(region = recode(region, Ponce = "Yauco"))
      yauco <- subset(yauco, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
guayanilla <- ypll %>%
      mutate(region = recode(region, Ponce = "Guayanilla"))
      guayanilla <- subset(guayanilla, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
penuelas <- ypll %>%
      mutate(region = recode(region, Ponce = "Penuelas"))
      penuelas <- subset(penuelas, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
adjuntas <- ypll %>%
      mutate(region = recode(region, Ponce = "Adjuntas"))
      adjuntas <- subset(adjuntas, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
jayuya <- ypll %>%
      mutate(region = recode(region, Ponce = "Jayuya"))
      jayuya <- subset(jayuya, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
juanaDiaz <- ypll %>%
      mutate(region = recode(region, Ponce = "Juana Diaz"))
      juanaDiaz <- subset(juanaDiaz, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
villalba <- ypll %>%
      mutate(region = recode(region, Ponce = "Villalba"))
      villalba <- subset(villalba, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
coamo <- ypll %>%
      mutate(region = recode(region, Ponce = "Coamo"))
      coamo <- subset(coamo, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
santaIsabel <- ypll %>%
      mutate(region = recode(region, Ponce = "Santa Isabel"))
      santaIsabel <- subset(santaIsabel, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
salinas <- ypll %>%
      mutate(region = recode(region, Ponce = "Salinas"))
      salinas <- subset(salinas, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
guayama <- ypll %>%
      mutate(region = recode(region, Ponce = "Guayama"))
      guayama <- subset(guayama, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
arroyo <- ypll %>%
      mutate(region = recode(region, Ponce = "Arroyo"))
      arroyo <- subset(arroyo, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")
      
patillas <- ypll %>%
      mutate(region = recode(region, Ponce = "Patillas"))
      patillas <- subset(patillas, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Caguas" & region != "Arecibo"
                  & region != "N/A")

#----------------------------------------------------------------  

aibonito <- ypll %>%
      mutate(region = recode(region, Caguas = "Aibonito"))
      aibonito <- subset(aibonito, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
cayey <- ypll %>%
      mutate(region = recode(region, Caguas = "Cayey"))
      cayey <- subset(cayey, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
cidra <- ypll %>%
      mutate(region = recode(region, Caguas = "Cidra"))
      cidra <- subset(cidra, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
aguasBuenas <- ypll %>%
      mutate(region = recode(region, Caguas = "Aguas Buenas"))
      aguasBuenas <- subset(aguasBuenas, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
gurabo <- ypll %>%
      mutate(region = recode(region, Caguas = "Gurabo"))
      gurabo <- subset(gurabo, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
sanLorenzo <- ypll %>%
      mutate(region = recode(region, Caguas = "San Lorenzo"))
      sanLorenzo <- subset(sanLorenzo, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
yabucoa <- ypll %>%
      mutate(region = recode(region, Caguas = "Yabucoa"))
      yabucoa <- subset(yabucoa, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
maunabo <- ypll %>%
      mutate(region = recode(region, Caguas = "Maunabo"))
      maunabo <- subset(maunabo, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
juncos <- ypll %>%
      mutate(region = recode(region, Caguas = "Juncos"))
      juncos <- subset(juncos, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
lasPiedras <- ypll %>%
      mutate(region = recode(region, Caguas = "Las Piedras"))
      lasPiedras <- subset(lasPiedras, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
humacao <- ypll %>%
      mutate(region = recode(region, Caguas = "Humacao"))
      humacao <- subset(humacao, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
naguabo <- ypll %>%
      mutate(region = recode(region, Caguas = "Naguabo"))
      naguabo <- subset(naguabo, region!= "Metro" & region != "Fajardo" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
#----------------------------------------------------------------  

rioGrande <- ypll %>%
      mutate(region = recode(region, Fajardo = "Rio Grande"))
      rioGrande <- subset(rioGrande, region!= "Metro" & region != "Caguas" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
luquillo <- ypll %>%
      mutate(region = recode(region, Fajardo = "Luquillo"))
      luquillo <- subset(luquillo, region!= "Metro" & region != "Caguas" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A") 
      
ceiba <- ypll %>%
      mutate(region = recode(region, Fajardo = "Ceiba"))
      ceiba <- subset(ceiba, region!= "Metro" & region != "Caguas" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A") 
      
vieques <- ypll %>%
      mutate(region = recode(region, Fajardo = "Vieques"))
      vieques <- subset(vieques, region!= "Metro" & region != "Caguas" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A") 
      
culebra <- ypll %>%
      mutate(region = recode(region, Fajardo = "Culebra"))
      culebra <- subset(culebra, region!= "Metro" & region != "Caguas" & region != "Bayamon" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
      
#----------------------------------------------------------------  
      
orocovis <- ypll %>%
      mutate(region = recode(region, Bayamon = "Orocovis"))
      orocovis <- subset(orocovis, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
barranquitas <- ypll %>%
      mutate(region = recode(region, Bayamon = "Barranquitas"))
      barranquitas <- subset(barranquitas, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
comerio <- ypll %>%
      mutate(region = recode(region, Bayamon = "Comerio"))
      comerio <- subset(comerio, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
corozal <- ypll %>%
      mutate(region = recode(region, Bayamon = "Corozal"))
      corozal <- subset(corozal, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
naranjito <- ypll %>%
      mutate(region = recode(region, Bayamon = "Naranjito"))
      naranjito <- subset(naranjito, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
toaAlta <- ypll %>%
      mutate(region = recode(region, Bayamon = "Toa Alta"))
      toaAlta <- subset(toaAlta, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
vegaAlta <- ypll %>%
      mutate(region = recode(region, Bayamon = "Vega Alta"))
      vegaAlta <- subset(vegaAlta, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
dorado <- ypll %>%
      mutate(region = recode(region, Bayamon = "Dorado"))
      dorado <- subset(dorado, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
toaBaja <- ypll %>%
      mutate(region = recode(region, Bayamon = "Toa Baja"))
      toaBaja <- subset(toaBaja, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
catano <- ypll %>%
      mutate(region = recode(region, Bayamon = "Catano"))
      catano <- subset(catano, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Arecibo"
                  & region != "N/A")
      
#----------------------------------------------------------------
     
quebradillas <- ypll %>%
      mutate(region = recode(region, Arecibo = "Quebradillas"))
      quebradillas <- subset(quebradillas, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A") 
      
lares <- ypll %>%
      mutate(region = recode(region, Arecibo = "Lares"))
      lares <- subset(lares, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A") 
      
camuy <- ypll %>%
      mutate(region = recode(region, Arecibo = "Camuy"))
      camuy <- subset(camuy, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
hatillo <- ypll %>%
      mutate(region = recode(region, Arecibo = "Hatillo"))
      hatillo <- subset(hatillo, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
utuado <- ypll %>%
      mutate(region = recode(region, Arecibo = "Utuado"))
      utuado <- subset(utuado, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
florida <- ypll %>%
      mutate(region = recode(region, Arecibo = "Florida"))
      florida <- subset(florida, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
barceloneta <- ypll %>%
      mutate(region = recode(region, Arecibo = "Barceloneta"))
      barceloneta <- subset(barceloneta, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
ciales <- ypll %>%
      mutate(region = recode(region, Arecibo = "Ciales"))
      ciales <- subset(ciales, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
manati <- ypll %>%
      mutate(region = recode(region, Arecibo = "Manati"))
      manati <- subset(manati, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
morovis <- ypll %>%
      mutate(region = recode(region, Arecibo = "Morovis"))
      morovis <- subset(morovis, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
quebradillas <- ypll %>%
      mutate(region = recode(region, Arecibo = "Quebradillas"))
      quebradillas <- subset(quebradillas, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")
      
vegaBaja <- ypll %>%
      mutate(region = recode(region, Arecibo = "Vega Baja"))
      vegaBaja <- subset(vegaBaja, region!= "Metro" & region != "Caguas" & region != "Fajardo" 
                  & region != "Mayaguez" & region != "Ponce" & region != "Bayamon"
                  & region != "N/A")

#----------------------------------------------------------------

#Eliminamos la categoria de Metro, dado que no hay municipios llamado Metro
ypllA <- subset(ypll, region!= "Metro")

#Se agrupan los datos de todos los municipios en una tabla
ypllA <- rbind(ypllA, carolina, sanJuan, guaynabo, loiza, trujilloAlto, canovanas,
               isabela, aguadilla, moca, sanSebastian, aguada, rincon, anasco, lasMarias, maricao, hormigueros,
               sanGerman, sabanaGrande, caboRojo, lajas,
               guanica, yauco, guayanilla, penuelas, adjuntas, jayuya, juanaDiaz, villalba, coamo,
               santaIsabel, salinas, guayama, arroyo, patillas,
               aibonito, cayey, cidra, aguasBuenas, gurabo, sanLorenzo, yabucoa, maunabo, juncos,
               lasPiedras, humacao, naguabo,
               rioGrande, luquillo, ceiba, vieques, culebra,
               orocovis, barranquitas, comerio, corozal, naranjito, toaAlta, vegaAlta, dorado,
               toaBaja, catano,
               quebradillas, lares, camuy, hatillo, utuado, florida, barceloneta, ciales, manati,
               morovis, vegaBaja )

#Se corrigen caracteres que no se puedan reconocer por UTF-8
ypllA$region <-gsub("Loiza", "Loíza",as.character(ypllA$region))
ypllA$region <-gsub("Canovanas", "Canóvanas",as.character(ypllA$region))
ypllA$region <-gsub("Mayaguez", "Mayagüez",as.character(ypllA$region))
ypllA$region <-gsub("Bayamon", "Bayamón",as.character(ypllA$region))

ypllA$region <-gsub("San Sebastian", "San Sebastián",as.character(ypllA$region))
ypllA$region <-gsub("Rincon", "Rincón",as.character(ypllA$region))
ypllA$region <-gsub("Anasco", "Añasco",as.character(ypllA$region))
ypllA$region <-gsub("Las Marias", "Las Marías",as.character(ypllA$region))
ypllA$region <-gsub("Mayaguez", "Mayagüez",as.character(ypllA$region))
ypllA$region <-gsub("San German", "San Germán",as.character(ypllA$region))

ypllA$region <-gsub("Guanica", "Guánica",as.character(ypllA$region))
ypllA$region <-gsub("Penuelas", "Peñuelas",as.character(ypllA$region))
ypllA$region <-gsub("Juana Diaz", "Juana Díaz",as.character(ypllA$region))

ypllA$region <-gsub("Rio Grande", "Río Grande",as.character(ypllA$region))

ypllA$region <-gsub("Comerio", "Comerío",as.character(ypllA$region))
ypllA$region <-gsub("Bayamon", "Bayamón",as.character(ypllA$region))
ypllA$region <-gsub("Catano", "Cataño",as.character(ypllA$region))

ypllA$region <-gsub("Manati", "Manatí",as.character(ypllA$region))

#----------------------------------------------------------------
#Esta funcion guarda el conteo de muertes correspondiente para algun municipio y su fecha
death_counts <- table(sex = ypllA$sex, deathDate = ypllA$deathDate, region = ypllA$region, ageRange = ypllA$ageRange)
death_counts = as.data.frame(death_counts)

death_counts$deathDate <- as.Date(death_counts$deathDate, format = "%Y-%m-%d")

#----------------------------------------------------------------


#CSV Plantilla con el formato correcto que deseamos utilizar para generar una grafica.
strata <- read_csv('strata.csv')


#Renombramos las columnas
colnames(strata) <- c('sex', 'deathDate', 'region', 'ageRange', 'Freq', 'tests')

strata$Freq[strata$Freq > 0] <- 0


#Corregimos los nombres de los municipios
strata$region <-gsub("Loiza", "Loíza",as.character(strata$region))
strata$region <-gsub("Canovanas", "Canóvanas",as.character(strata$region))
strata$region <-gsub("Mayaguez", "Mayagüez",as.character(strata$region))
strata$region <-gsub("Bayamon", "Bayamón",as.character(strata$region))

strata$region <-gsub("San Sebastian", "San Sebastián",as.character(strata$region))
strata$region <-gsub("Rincon", "Rincón",as.character(strata$region))
strata$region <-gsub("Anasco", "Añasco",as.character(strata$region))
strata$region <-gsub("Las Marias", "Las Marías",as.character(strata$region))
strata$region <-gsub("Mayaguez", "Mayagüez",as.character(strata$region))
strata$region <-gsub("San German", "San Germán",as.character(strata$region))

strata$region <-gsub("Guanica", "Guánica",as.character(strata$region))
strata$region <-gsub("Penuelas", "Peñuelas",as.character(strata$region))
strata$region <-gsub("Juana Diaz", "Juana Díaz",as.character(strata$region))

strata$region <-gsub("Rio Grande", "Río Grande",as.character(strata$region))

strata$region <-gsub("Comerio", "Comerío",as.character(strata$region))
strata$region <-gsub("Bayamon", "Bayamón",as.character(strata$region))
strata$region <-gsub("Catano", "Cataño",as.character(strata$region))

strata$region <-gsub("Manati", "Manatí",as.character(strata$region))

#Cambiamos las categorias de estudio a las que deseamos utilizar
male1 <- strata %>%
  mutate(sex = recode(sex, Molecular = "Male"))
  male1 <- subset(male1, sex!= "Antigens" & sex != "Molecular" )

male2 <- strata %>%
  mutate(sex = recode(sex, Antigens = "Male"))
  male2 <- subset(male2, sex!= "Antigens" & sex != "Molecular" )
  
female1 <- strata %>%
  mutate(sex = recode(sex, Molecular = "Female"))
  female1 <- subset(male1, sex!= "Antigens" & sex != "Molecular" )
  
female2 <- strata %>%
  mutate(sex = recode(sex, Antigens = "Female"))
  female2 <- subset(female2, sex!= "Antigens" & sex != "Molecular" )

#Unimos las tablas que generamos con su identificacion de estudio correspondiente

strataC <- dplyr::full_join(male1, male2, female1, by = c('sex', 'deathDate', 'region', 'ageRange', 'Freq', 'tests'))

strataC <- dplyr::full_join(strataC, female2, by = c('sex', 'deathDate', 'region', 'ageRange', 'Freq', 'tests'))

#----------------------------------------------------------------
#Agregamos los conteos de muerte correspondientes a cada persona por region, fecha y edad

data <- dplyr::full_join(death_counts, strataC, by = c('sex','deathDate', 'region', 'ageRange','Freq'))

max_deaths <- max(death_counts$'Freq')

data[c('tests')] <- max_deaths

max_deaths

#----------------------------------------------------------------
#Asignamos las cantidad de muertes correspondientes a cada caso

colnames(data) <- c('testType','date','patientCity','ageRange','positives','tests')

#data$deathDate <- as.Date(death_counts$deathDate, format = "%Y-%m-%d")

data$patientCity <- as.character(data$patientCity)

data$ageRange <- as.character(data$ageRange)

data$positives <- as.numeric(data$positives)

data <- data %>% mutate(tests = ifelse( positives == 3 &  tests == 5, 3, tests))

data <- data %>% mutate(tests = ifelse( positives == 2 &  tests == 5, 2, tests))

data <- data %>% mutate(tests = ifelse( positives == 1 &  tests == 5, 1, tests))

data <- data %>% mutate(tests = ifelse( positives == 0 &  tests == 5, 1, tests))

tests_by_strata <- data

lapply(data, class)

lapply(tests_by_strata, class)

colnames(tests_by_strata) <- c('testType','date','patientCity','ageRange','positives','tests')

#----------------------------------------------------------------

 
colnames(tests_by_strata) <- c('testType','date','patientCity','ageRange','positives','tests')

#save(tests_by_strata, file = file.path( "tests_by_strata.rda"))

ypll_deaths = tests_by_strata

colnames(ypll_deaths) <- c('testType','date','patientCity','ageRange','positives','tests')

save(ypll_deaths, file = file.path( "ypll_deaths.rda"))

lapply(data, class)

lapply(tests_by_strata, class)
