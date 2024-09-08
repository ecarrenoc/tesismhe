
library(tidyverse)
library(imputeTS)
theme_set(theme_bw(base_size = 25))
options(scipen = 999)






# DATA POBLACION ============================
pob0 <- readxl::read_excel("poblacion provincias 1885-1952.xlsx")

pob0$region <- case_when(
  pob0$provincia %in% c("Aconcagua", "Valparaíso") ~ "Valparaíso",
  pob0$provincia %in% c("Ohiggins", "Colchagua") ~ "Ohiggins",
  pob0$provincia %in% c("Curicó", "Talca", "Maule", "Linares") ~ "Maule",
  pob0$provincia %in% c("Ñuble", "Concepción", "Arauco", "Biobio") ~ "Biobio",
  pob0$provincia %in% c("Malleco", "Cautín") ~ "Araucanía",
  pob0$provincia %in% c("Valdivia", "Osorno", "Llanquihue","Chiloé") ~ "Los Lagos",
  pob0$provincia == "Antártica" ~ "Magallanes",
  T~pob0$provincia
)

pob0 <- pob0 %>% 
  group_by(region, anio) %>% 
  summarise(pobla = sum(pobla))


pob1 <- readxl::read_excel("población regiones 1960-1992.xlsx") %>% 
  gather("anio", "pobla", 2:ncol(.)) %>% 
  mutate(pobla = pobla * 1000)

pob1$region <- case_when(
  pob1$region == "De Tarapacá" ~ "Tarapacá",
  pob1$region == "De Antofagasta" ~ "Antofagasta",
  pob1$region == "De Atacama" ~ "Atacama",
  pob1$region == "De Coquimbo"~ "Coquimbo",
  pob1$region == "De Valparaíso" ~ "Valparaíso",
  pob1$region == "Región Metropolitana de Santiago" ~ "RM",
  pob1$region == "Del Libertador General Bernardo O'Higgins" ~ "Ohiggins",
  pob1$region == "Del Maule"  ~ "Maule",
  pob1$region == "Del Biobío" ~ "Biobio",
  pob1$region == "De La Araucanía" ~ "Araucanía",
  pob1$region == "De Los Lagos" ~ "Los Lagos",
  pob1$region == "Aisén del General Carlos Ibáñez del Campo" ~ "Aysén",
  pob1$region == "De Magallanes y de la Antártica Chilena" ~ "Magallanes",
  T~pob1$region
)

pob1$anio <- as.numeric(pob1$anio)

pob2 <- readxl::read_excel("problacion regiones 2002-2023.xlsx") %>% 
  gather("anio", "pobla", 2:ncol(.)) %>% 
  mutate(region = str_trim(region))

pob2$region <- case_when(
  # pob2$region == "Región de Arica y Parinacota" ~ "Arica y Parinacota",
  pob2$region %in% c("Región de Tarapacá", "Región de Arica y Parinacota") ~ "Tarapacá",
  pob2$region == "Región de Antofagasta" ~ "Antofagasta",
  pob2$region == "Región de Atacama" ~ "Atacama",
  pob2$region == "Región de Coquimbo"~ "Coquimbo",
  pob2$region == "Región de Valparaíso"  ~ "Valparaíso",
  pob2$region == "Región Metropolitana" ~ "RM",
  pob2$region == "Región del Libertador General Bernardo O'Higgins"  ~ "Ohiggins",
  pob2$region %in% c("Del Maule","Región del Maule")  ~ "Maule",
  pob2$region %in% c("Del Biobío", "Región del Biobío") ~ "Biobio",
  pob2$region == c("De La Araucanía", "Región de La Araucanía") ~ "Araucanía",
  pob2$region %in% c("De Los Lagos","Región de Los Ríos","Región de Los Lagos") ~ "Los Lagos",
  pob2$region %in% c("Aisén del General Carlos Ibáñez del Campo",
                     "Región de Aysén del General Carlos Ibáñez del Campo") ~ "Aysén",
  pob2$region %in% c("De Magallanes y de la Antártica Chilena",
                     "Región de Magallanes y la Antártica Chilena") ~ "Magallanes",
  pob2$region == "Región de Ñuble" ~ "Biobio",
  T~pob2$region
)

pob2$anio <- as.numeric(pob2$anio)

pob2 <- pob2 %>% 
  group_by(region, anio) %>% 
  summarise(pobla = sum(pobla))

pob0$pobla[pob0$pobla == 0] <- NA

consopob <- bind_rows(pob0 %>% 
                        select(region, anio, pobla),
                      pob1, pob2) %>% 
  as.data.frame() %>% 
  complete(region, anio = full_seq(anio, 1)) %>% 
  # mutate(pobla = ifelse(pobla == 0, NA, pobla)) %>% 
  group_by(region) %>% 
  mutate(pob_hat = round(na_kalman(pobla, type = "level")))

consopob$region <- factor(consopob$region,
                          unique(consopob$region)[c(12, 1, 3, 6, 13, 11, 10, 9, 5, 2, 7, 4, 8)])

rm(pob0, pob1, pob2)


unique(consopob$region)


write.csv2(consopob, "consolidado pobla regional.csv", row.names = F)

# Data Badia-Miro =======================

pibbm <- readxl::read_excel("pib_regional_data.xlsx") %>% 
  gather("anio", "pib", 3:ncol(.)) %>% 
  filter(!(region %in% c("Total", "Resto"))) %>% 
  mutate(anno = as.numeric(anio),
         region =  case_when(
           region %in% c("Aconcagua", "Valparaíso") ~ "Valparaíso",
           region %in% c("Ohiggins", "Colchagua","O'Higgins") ~ "Ohiggins",
           region %in% c("Curicó", "Talca", "Maule", "Linares") ~ "Maule",
           region %in% c("Ñuble", "Concepción", "Arauco", "Bío-Bío") ~ "Biobio",
           region %in% c("Malleco", "Cautín") ~ "Araucanía",
           region %in% c("Valdivia", "Osorno", "Llanquihue","Chiloé") ~ "Los Lagos",
           region == "Antártica" ~ "Magallanes",
           region == "Santiago" ~ "RM",
           region == "Tarapaca" ~ "Tarapacá",
           T~region
         )) %>% 
  group_by(region, anio, sector) %>% 
  summarise(pib = sum(pib)) %>% 
  mutate(origen = "Badia-Miró") 

pibbm$anio <- as.numeric(pibbm$anio)

write.csv2(pibbm, "pib Badia-Miro.csv", row.names = F)

# Data MIDEPLAN =========================

lapply(1:52, function(i){
  
  dat <- readxl::read_excel("empalme-seriepibreg.1960-2001final-anexo-13-64.xlsx", 
                            sheet = i, range = "A1:L12")
  
  
  if(!is.na(dat[11, 2])){
    dat <- readxl::read_excel("empalme-seriepibreg.1960-2001final-anexo-13-64.xlsx", 
                              sheet = i, range = "A2:L13")
  }
  
  dat$`Rama de Actividad Económica`[11] <- dat$`Rama de Actividad Económica`[11] %>% 
    str_remove("Producto Interno Bruto por Clase de Actividad Eco \\(Variación Porcentual\\)")
  
  temp <- dat$`Rama de Actividad Económica`[11]
  dat <- dat[-10,]
  
  dat <- dat %>% 
    gather("anio", "pib", 2:ncol(.)) %>% 
    filter(!is.na(pib)) %>% 
    mutate(region = temp)
  
  dat
  
}) %>% 
  do.call("rbind", .) %>% 
  as.data.frame() -> pibmp

colnames(pibmp)[1] <- "sector"

pibmp$sector <- tolower(pibmp$sector)

pibmp$region <- case_when(
  pibmp$region %in% c("I Región de Tarapacá\r\n","I Región de Tarapacá") ~ "Tarapacá",
  pibmp$region == "II Región de Antofagasta" ~ "Antofagasta",
  pibmp$region == "III Región de Atacama" ~ "Atacama",
  pibmp$region == "IV Región de Coquimbo" ~ "Coquimbo",
  pibmp$region == "V Región de Valparaíso" ~ "Valparaíso",
  pibmp$region == "Región Metropolitana De Santiago" ~ "RM",
  pibmp$region == "VI Región de O'Higgins" ~ "Ohiggins",
  pibmp$region == "VII Región del Maule" ~ "Maule",
  pibmp$region == "VIII Región del Bío-Bío" ~ "Biobio",
  pibmp$region == "IX Región de La Araucanía" ~ "Araucanía",
  pibmp$region == "X Región de Los Lagos" ~ "Los Lagos",
  pibmp$region == "XI Región de Aysén" ~ "Aysén",
  pibmp$region == "XII Región de Magallanes" ~ "Magallanes",
  T~pibmp$region
)

pibmp$origen <- "Mideplan"
pibmp$anio <- as.numeric(pibmp$anio)


write.csv2(pibmp, "consolidado mideplan.csv", row.names = F,
           fileEncoding = "latin1")


# DATA BC ============================
#  
# Cohorte 1985 a 1995

df1 <- readxl::read_excel("PIB PC 1985-1995.xlsx") %>% 
  gather("sector", "pib", 3:16) %>% 
  filter(sector != "Producto Interno Bruto",
         sector != "Menos: Imputaciones Bancarias") %>% 
  mutate(sector = tolower(sector))


# Cohorte 1996 - 2002
df2 <- readxl::read_excel("PIB BC 1996-2002.xlsx") %>% 
  gather("sector", "pib", 3:ncol(.)) %>% 
  filter(sector != "Producto interno bruto",
         sector != "Menos: imputaciones bancarias" ) %>% 
  mutate(sector = tolower(sector))


# Cohorte 2003 - 2007
df3 <- readxl::read_excel("PIB BC 2003-2007.xlsx") %>% 
  gather("sector", "pib", 3:ncol(.)) %>% 
  filter(sector != "Producto interno bruto",
         sector != "Menos: imputaciones bancarias" ) %>% 
  mutate(sector = tolower(sector))


# Cohorte 2008 - 2019
df4 <- readxl::read_excel("PIB BC 2008-2019.xlsx") %>% 
  gather("sector", "pib", 3:ncol(.)) %>% 
  filter(sector != "Producto interno bruto",
         anio < 2013) %>% 
  mutate(sector = tolower(sector))


# Cohorte 2013 - 2023
df5 <- readxl::read_excel("PIB BC 2013-2023.xlsx") %>% 
  gather("anio", "pib", 3:ncol(.)) %>% 
  filter(sector != "Producto interno bruto") %>% 
  mutate(sector = tolower(sector),
         anio = as.numeric(anio))

unique(df5$sector)

# Consolidando

bind_rows(df1, df2, df3, df4, df5) -> pibbc

rm(df1, df2, df3, df4, df5)

pibbc$sector <- str_trim(pibbc$sector)

pibbc$sector <- case_when(
  pibbc$sector == "servicios financieros y empresariales" ~ "servicios financieros",
  pibbc$sector == "transporte, información y comunicaciones" ~ "transportes y comunicaciones",
  pibbc$sector == "propiedad de vivienda" ~ "servicios de vivienda e inmobiliarios",
  pibbc$sector %in% c("electricidad, gas, agua y gestión de desechos", 
                 "electricidad, gas y agua" ) ~ "electricidad, gas, agua",
  T~pibbc$sector
)

pibbc$region <- case_when(
  pibbc$region == "Arica y Parinacota" ~ "Tarapacá",
  pibbc$region == "Los Ríos" ~ "Los Lagos",
  pibbc$region == "Ñuble" ~ "Biobio",
  pibbc$region == "Aysen" ~ "Aysén",
  pibbc$region == "Arauncanía" ~ "Araucanía",
  T~pibbc$region
)

pibbc$origen <- "Banco Central"



write.csv2(pibbc, "consolidado BC.csv", row.names = F, 
           fileEncoding = "latin1")



# CONSOLIDANDO ============================

consopib <- bind_rows(pibbm, pibmp, pibbc)
rm(pibbm, pibmp, pibbc)


consopib <- consopib %>% 
  mutate(sector2 = case_when(
    sector %in% c("Agrario", "pesca", "agropecuario-silvícola") ~ "Agrario",
    sector %in% c("Industria", "industria manufacturera", 
                  "construcción", "electricidad, gas, agua") ~ "Industria",
    sector %in% c("Minería", "minería") ~ "Minería",
    sector %in% c("Servicios", "transporte y comunicaciones",
                  "comercio",
                  "restaurantes y hoteles",
                  "comercio, restaurantes y hoteles", "servicios financieros", 
                  "servicios de vivienda e inmobiliarios",
                  "servicios personales", "servicios personales", 
                  "administración pública", "transportes y comunicaciones") ~ "Servicios",
    sector == "otros" ~ "Otros"
  ),
  sector3 = ifelse(sector2 == "Otros", "Servicios", sector2))


consolidado <- consopib %>% 
  left_join(consopob, by = c("region", "anio")) 





write.csv2(consolidado, "pibs y pobla final.csv", row.names = F,
           fileEncoding = "latin1")


rm(consopib, consolidado, consopob)


# PRUEBAS ==================





