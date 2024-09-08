
library(tidyverse)
library(imputeTS)
theme_set(theme_bw(base_size = 25))
options(scipen = 999)


# DATOS DEL CNED ==========

cnedraw1 <- readxl::read_excel("data raw/01_BaseINDICES_Pregrado.xlsx") %>% 
  group_by(Año, `Nombre Region`) %>% 
  summarise(matricula = sum(`Matrícula Total`))

cnedraw2 <- readxl::read_excel("data raw/02_BaseINDICES_Posgrado.xlsx") %>% 
  group_by(Año, `Nombre Region`) %>% 
  summarise(matricula = sum(`Matrícula Total`))

cnedraw <- bind_rows(cnedraw1, cnedraw2) %>% 
  group_by(Año, `Nombre Region`) %>% 
  summarise(matricula = sum(matricula))
  
colnames(cnedraw)[2] <- "region"
rm(cnedraw1, cnedraw2)

cnedraw$region <- case_when(
  str_detect(cnedraw$region, "Tarapacá|Arica") ~ "Tarapacá",
  str_detect(cnedraw$region, "Antofagasta") ~ "Antofagasta",
  str_detect(cnedraw$region, "Atacama") ~ "Atacama",
  str_detect(cnedraw$region, "Coquimbo") ~ "Coquimbo",
  str_detect(cnedraw$region, "Valparaíso") ~ "Valparaíso",
  str_detect(cnedraw$region, "Santiago|Metropolitana") ~ "RM",
  str_detect(cnedraw$region, "Bernardo|Higgins") ~ "Ohiggins",
  str_detect(cnedraw$region, "Maule") ~ "Maule",
  str_detect(cnedraw$region, "Ñuble|Biobío|Bío-Bío") ~ "Biobio",
  str_detect(cnedraw$region, "Araucanía") ~ "Araucanía",
  str_detect(cnedraw$region, "Ríos|Lagos") ~ "Los Lagos",
  str_detect(cnedraw$region, "Aysén") ~ "Aysén",
  str_detect(cnedraw$region, "Magallanes") ~ "Magallanes"
)

cnedraw <- cnedraw %>% 
  group_by(region, Año) %>% 
  summarise(matricula = sum(matricula))

colnames(cnedraw)[2] <- "anio"


# DATOS DEL SIIT =============
rawed <- read.csv("data raw/matricula educación superior.csv") %>% 
  select(-Variable) %>% 
  gather("anio", "matricula", 2:ncol(.)) %>% 
  mutate(anio = str_extract(anio, "\\d+") %>% as.numeric(),
         matricula = str_extract(matricula, "\\d+") %>% as.numeric()) %>% 
  mutate(matricula = ifelse(is.na(matricula), 0, matricula)) %>% 
  mutate(Unidad.territorial = case_when(
    str_detect(Unidad.territorial, "Tarapacá|Arica") ~ "Tarapacá",
    str_detect(Unidad.territorial, "Antofagasta") ~ "Antofagasta",
    str_detect(Unidad.territorial, "Atacama") ~ "Atacama",
    str_detect(Unidad.territorial, "Coquimbo") ~ "Coquimbo",
    str_detect(Unidad.territorial, "Valparaíso") ~ "Valparaíso",
    str_detect(Unidad.territorial, "Santiago") ~ "RM",
    str_detect(Unidad.territorial, "Bernardo") ~ "Ohiggins",
    str_detect(Unidad.territorial, "Maule") ~ "Maule",
    str_detect(Unidad.territorial, "Ñuble|Biobío") ~ "Biobio",
    str_detect(Unidad.territorial, "Araucanía") ~ "Araucanía",
    str_detect(Unidad.territorial, "Ríos|Lagos") ~ "Los Lagos",
    str_detect(Unidad.territorial, "Aysén ") ~ "Aysén",
    str_detect(Unidad.territorial, "Magallanes") ~ "Magallanes"
  )) %>% 
  group_by(Unidad.territorial, anio) %>% 
  summarise(matricula = sum(matricula)) %>% 
  group_by(anio) %>% 
  mutate(totanual = sum(matricula)) %>% 
  mutate(panual = matricula/totanual)

colnames(rawed)[1] <- "region"

rawed$region <- factor(rawed$region,
                      unique(rawed$region)[c(12, 1, 3, 6, 13, 11, 10, 9, 5, 2, 7, 4, 8)])

# LIMPIANDO ===============

# graficando inconsistencia de datos
rawed %>% 
  ggplot() +
  aes(anio, panual) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1984, 2022, 3),
                     limits = c(1984, 2022),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~region) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 20),
        axis.text.y = element_text(size = 20),
        legend.position = c(.8, .1),
        legend.title = element_blank()) -> plotanexo2

ggsave(plot = plotanexo2, "anexoFig2.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)

# explorando problemas con Ohiggins
rawed %>% 
  filter(region == "Ohiggins",
         anio %in% c(2006, 2007)) 

cnedraw %>% 
  filter(region == "Ohiggins",
         anio %in% c(2006, 2007)) 


# HACIENDO CORRECCIONES
rawedcorrected <- rawed %>% 
  select(anio, region, matricula)

# magallanes y santiago
mag <- cnedraw %>% 
  filter(anio == 2007, 
         region == "Magallanes") %>% 
  pull(matricula)

st <- rawedcorrected$matricula[rawedcorrected$anio == 2007 & 
                              rawedcorrected$region == "Magallanes"] 

rawedcorrected$matricula[rawedcorrected$anio == 2007 & rawedcorrected$region == "Magallanes"] <- mag
rawedcorrected$matricula[rawedcorrected$anio == 2007 & rawedcorrected$region == "RM"] <- st


# desviaciones importantes
bind_rows(rawed %>%
            select(anio, region, matricula) %>% 
            mutate(origen = "SIIT"),
          cnedraw %>% 
            mutate(origen = "CNED")) %>% 
  filter(region %in% c("Maule", "Araucanía", "Ohiggins")) %>% 
  ggplot() +
  aes(x = anio) +
  geom_line(aes(y = matricula, color = origen), size = 1.2) +
  scale_x_continuous(breaks = seq(1984,2022, 3),
                     limits = c(1984, 2022),
                     expand = c(.01,.01)) +
  facet_wrap(.~region) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(color = "Origen")

# -> plotanexo3

ggsave(plot = plotanexo3, "anexoFig3.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)



# problemas ohiggins y araucania
tempcned1 <- cnedraw %>% 
  filter(anio < 2007,
         anio >= 2005,
         region == "Ohiggins") %>% 
  pull(matricula)

tempcned2 <- cnedraw %>% 
  filter(anio < 2007,
         anio >= 2005,
         region == "Araucanía") %>% 
  pull(matricula)

rawedcorrected$matricula[rawedcorrected$region == "Ohiggins" &
                        (rawedcorrected$anio < 2007 & rawedcorrected$anio >= 2005)] <- tempcned1

rawedcorrected$matricula[rawedcorrected$region == "Araucanía" &
                        (rawedcorrected$anio < 2007 & rawedcorrected$anio >= 2005)] <- tempcned2

rawedcorrected$matricula[rawedcorrected$region == "Maule" &
                        (rawedcorrected$anio < 2014 & rawedcorrected$anio > 2010)] <- NA

rawedcorrected$matricula[rawedcorrected$region == "Maule"] <- round(na_kalman(rawedcorrected$matricula[rawedcorrected$region == "Maule"]))



bind_rows(rawed %>% 
            select(anio, region, matricula) %>% 
            filter(region %in% c("Araucanía", "Ohiggins", "Maule")) %>% 
            mutate(serie = "Original"), 
          rawedcorrected %>% 
            filter(region %in% c("Araucanía", "Ohiggins", "Maule")) %>% 
            mutate(serie = "Corregida")) %>% 
  ggplot() +
  aes(anio, matricula, color = serie) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1984,2022, 3),
                     limits = c(1984, 2022),
                     expand = c(.01,.01)) +
  facet_wrap(.~region) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5, size = 20),
        axis.text.y = element_text(size = 20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(color = "Origen") 

# -> plotanexo4

ggsave(plot = plotanexo4, "anexoFig4.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)



# IMPUTANDO EL 82 Y 83
newdata <- expand.grid(region = c("Antofagasta", "Araucanía", "Atacama", 
                                               "Aysén", "Biobio", "Coquimbo", 
                                               "Los Lagos", "Magallanes", "Maule", 
                                               "Ohiggins", "RM", "Tarapacá", "Valparaíso"), 
                        anio = c(1982, 1983))

newdata$matricula <- NA

rawedimp <- bind_rows(rawedcorrected, newdata)

rawedimp <- rawedimp %>% 
  group_by(region) %>%
  arrange(region, anio) %>% 
  mutate(mat_hat = round(na_kalman(matricula)))

rawedimp %>%
  ggplot() +
  aes(x = anio) +
  geom_point(aes(y = mat_hat, color = "pred")) +
  geom_line(aes(y = matricula, color = "obs")) +
  facet_wrap(.~region,
             scales = "free_y") +
  theme(legend.position = c(.8, 0),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 12),
        axis.text.y = element_text(size = 12))

rawedimp$mat_hat[rawedimp$region == "Aysén" &
                 rawedimp$anio < 1984] <- 0


# EMPALMANDO 2023
cned23 <- cnedraw %>% 
  filter(anio == 2023)

cned23$mat_hat <- cned23$matricula

rawedimp2 <- bind_rows(rawedimp, cned23)



# exportando
write.csv2(rawedimp2, "data procesada/consolidado matricula final.csv",
           fileEncoding = "latin1", row.names = F)


