
library(tidyverse)
theme_set(theme_bw(base_size = 17))
options(scipen = 999)

softmax <- function(x){
  exp(x)/(1+exp(x))
}

# 
# anidraw <- read.csv2("data raw/BDH_HISTORICA ANID.csv") %>%
#   select(1, 3, 4, 5, 8:10, 13, 17, 19, 20) %>%
#   filter(SUBDIRECCION %in% c("PROYECTOS DE INVESTIGACION",
#                              "INVESTIGACION APLICADA"))
# 
# write.csv2(anidraw, "data procesada/prepython_aniddata.csv", row.names = F)

# AQUÍ EJECUTAR CÓDIGO DE PYTHON


# LEER ARCHIVO PROCESADO
aniddata <- read.csv2("data procesada/postpython_aniddata.csv")

aniddata$REGION_EJECUCION <- ifelse(aniddata$REGION_EJECUCION == "12. MAGALLANES Y ANTARTICA CHILENA",
                                "MAGALLANES",aniddata$REGION_EJECUCION) %>% 
  str_extract("[ a-zA-ZñÑ]+") %>% 
  str_trim() %>% 
  str_to_title()

# aniddata %>% 
#   filter(REGION_EJECUCION %in% c("Sin Informacion", "Multiregional")) %>% 
#   nrow()

aniddata <- aniddata %>% 
  mutate(regionstd = case_when(
    str_detect(REGION_EJECUCION, "Arica|Tarapaca") ~ "Tarapacá",
    str_detect(REGION_EJECUCION, "Ñuble|Biobio") ~ "Biobio",
    REGION_EJECUCION == "Valparaiso" ~ "Valparaíso",
    REGION_EJECUCION == "La Araucania" ~ "Araucanía",
    REGION_EJECUCION == "Aysen" ~ "Aysén",
    REGION_EJECUCION == "Metropolitana" ~ "RM",
    REGION_EJECUCION == "Los Rios" ~ "Los Lagos",
    T~REGION_EJECUCION
  )) %>% 
  filter(!(REGION_EJECUCION %in% c("Sin Informacion", "Multiregional")))


# ESTUDIAR DATOS PERDIDOS
# aniddata$DURACION_MESES %>% 
#   is.na() %>% 
#   sum()
# 
# aniddata$MONTO_ADJUDICADO %>% 
#   is.na() %>% 
#   sum()
# 
# aniddata %>% 
#   filter(is.na(DURACION_MESES) |
#          is.na(MONTO_ADJUDICADO)) %>% 
#   nrow()



# MESES DURACIÓN DATOS PERDIDOS
glm(as.numeric(is.na(aniddata$DURACION_MESES)) ~ 
      aniddata$MONTO_ADJUDICADO + 
      aniddata$AGNO_FALLO +
      aniddata$AREA_OCDE +
      aniddata$regionstd+
      as.numeric(aniddata$LENGUAJE == "Spanish"),
    family = binomial(link = "logit")) -> mesesbias

# MONTO DATOS PERDIDOS
glm(as.numeric(is.na(aniddata$MONTO_ADJUDICADO)) ~ 
      aniddata$DURACION_MESES +
      aniddata$AGNO_FALLO +
      aniddata$AREA_OCDE +
      aniddata$regionstd+
      as.numeric(aniddata$LENGUAJE == "Spanish"),
    family = binomial(link = "logit")) -> montobias

stargazer::stargazer(mesesbias, montobias, type = "text")


# analisis de missing en monto adjudicado
montomissing <- aniddata %>% 
  filter(is.na(MONTO_ADJUDICADO))

montomissing %>% 
  filter(INSTRUMENTO == "POSTDOCTORADO") %>% 
  group_by(AGNO_FALLO) %>% 
  count()

aniddata %>% 
  filter(INSTRUMENTO == "POSTDOCTORADO",
         AGNO_FALLO == 2014) -> postdocmissing

glm(as.numeric(is.na(postdocmissing$MONTO_ADJUDICADO)) ~ 
      postdocmissing$DURACION_MESES + 
      postdocmissing$AREA_OCDE +
      postdocmissing$regionstd+
      as.numeric(postdocmissing$LENGUAJE == "Spanish"),
    family = binomial(link = "logit")) %>% 
  summary()


# analisis de missing en meses de duración
mesesmissing <- aniddata %>% 
  filter(is.na(DURACION_MESES))

mesesmissing %>% 
  group_by(INSTRUMENTO) %>% 
  count()

aniddata %>% 
  filter(INSTRUMENTO %in% 
           c("INICIACION", "POSTDOCTORADO","REGULAR")) -> fondecytmissing

glm(as.numeric(is.na(fondecytmissing$DURACION_MESES)) ~ 
      fondecytmissing$MONTO_ADJUDICADO + 
      fondecytmissing$AREA_OCDE +
      fondecytmissing$AGNO_FALLO +
      fondecytmissing$regionstd+
      as.numeric(fondecytmissing$LENGUAJE == "Spanish"),
    family = binomial(link = "logit")) %>% 
  summary()

softmax(0.673603693)


# REMOCIÓN DE CASOS PERDIDOS
aniddata2 <- aniddata %>% 
  filter(!is.na(DURACION_MESES) & !is.na(MONTO_ADJUDICADO))


# AJUSTANDO POR INFLACIÓN
aniddata2$montomiles <- aniddata2$MONTO_ADJUDICAD * 1000

temp <- data.frame(cod = aniddata2$CODIGO_PROYECTO,
                  anio = aniddata2$AGNO_FALLO,
                  monto = aniddata2$montomiles,
                  años = ifelse(aniddata2$DURACION_MESES/12 <= 1,
                                1,
                                aniddata2$DURACION_MESES/12)) %>% 
  # filter(años > 1) %>% 
  mutate(anualmount = monto/años) %>% 
  slice(rep(1:n(), años)) %>% 
  group_by(cod) %>% 
  mutate(idyear = anio + row_number()-1) %>% 
  left_join(
    readxl::read_excel("data raw/inflacion ref2015.xlsx"),
    by = c("idyear" = "anio")
  ) %>% 
  filter(idyear < 2024) %>% 
  mutate(realammount = anualmount * (indice/100)) %>% 
  group_by(cod) %>% 
  summarise(montomilesreal = sum(realammount)) 


aniddata3 <- aniddata2 %>% 
  left_join(temp, by = c("CODIGO_PROYECTO"="cod"))



# RE IMPUTANDO INSTITUCIONES DE SANTIAGO
aniddata4 <- aniddata3 %>% 
  filter(!(INSTITUCION_PRINCIPAL %in% c(
    "HEIDELBERG UNIVERSITY", "UNIVERSITE MONTPELLIER 2",
    "NATIONAL RESEARCH COUNCIL OF CANADA", "UNIVERSITY OF CALIFORNIA, SANTA CRUZ",
    "UNIVERSITY OF ABERDEEN", "UNIVERSIDAD NACIONAL DE EDUCACION A DISTANCIA",
    "THE UNIVERSITY OF NOTTINGHAM-CHILE"
  )),
  MONTO_ADJUDICADO > 0) %>% 
  mutate(regionstd = case_when(
    INSTITUCION_PRINCIPAL %in% c(
      "UNIVERSIDAD DE CHILE", "PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE",
      "BIOS CHILE INGENIERIA GENETICA S.A", "UNIVERSIDAD DE SANTIAGO DE CHILE",
      "UNIVERSIDAD DIEGO PORTALES", "UNIVERSIDAD GABRIELA MISTRAL",
      "UNIVERSIDAD TECNOLOGICA METROPOLITANA", "UNIVERSIDAD CATOLICA CARDENAL RAUL SILVA HENRIQUEZ",
      "UNIVERSIDAD FINIS TERRAE", "FUNDACION CIENCIA PARA LA VIDA",
      "ANDES ELECTRONICS SPA", "INGENIERIA Y BIONEGOCIOS FDM",
      "BLOOM ALERT SPA", "BIOAERA AUSTRAL",
      "EL REFUGIO SPA.", "BIOTECNOLOGIA E INNOVACION SPA",
      "ANDESITE SPA", "ATECH APPLIED TECHNOLOGY SPA.",
      "DESARROLLO DE PRODUCTOS Y SERVICIOS BIOTECNOLOGICOS SPA",
      "PHINET SPA","LUYEF BIOTECHNOLOGIES SPA"
    ) ~ "RM",
    T~ regionstd
  )) 


write.csv2(aniddata4, "data procesada/dataanidfinal.csv", 
           row.names = F)


# dat <- read.csv2("dataanidfinal.csv")


# TYPOS QUE CORREGIR
# "CENTRO DE INNOVACION E INVENTIGACION APLICADA"
# "CENTRO DE INNOVACION E INVESTIGACION APLICADA" 









