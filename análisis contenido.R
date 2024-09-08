
library(tidyverse)
library(stm)
library(ggrepel)
library(patchwork)
options(scipen = 999)
theme_set(theme_bw(base_size = 21))

# ============= FUNCIONES ===========

regordinal <- function(x){
  case_when(
    x=="Tarapacá" ~ 1,
    x=="Antofagasta"~2,
    x=="Atacama"~3,
    x=="Coquimbo"~4,
    x=="Valparaíso"~5,
    x=="RM"~6,
    x=="Ohiggins"~7,
    x=="Maule"~8,
    x=="Biobio"~9,
    x=="Araucanía"~10,
    x=="Los Lagos"~11,
    x=="Aysén"~12,
    x=="Magallanes"~13
  )
}


# ============== INGESTA DATOS ================

dat <- read.csv2("data procesada/dataanidfinal.csv") %>% 
  mutate(regordinal = regordinal(regionstd))

pib <- read.csv2("data procesada/pibs y pobla final.csv", 
                 encoding = "latin1") %>% 
  filter(origen == "Banco Central") %>% 
  select(region, anio, sector3, pib, pob_hat) %>% 
  bind_rows(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(anio <= 1984 & anio >= 1982,
             origen == "Mideplan") %>% 
      select(region, anio, sector3, pib, pob_hat)
  ) %>% 
  group_by(anio, region, sector3) %>% 
  summarise(pib = sum(pib)) %>% 
  group_by(anio, region) %>% 
  mutate(ppibreg = pib/sum(pib)) %>% 
  select(anio, region, sector3, ppibreg) %>% 
  spread(sector3, ppibreg) %>% 
  group_by(anio) %>% 
  mutate(agrdesv = Agrario-mean(Agrario),
         indesv = Industria-mean(Industria),
         mindesv = Minería-mean(Minería),
         ssdesv = Servicios-mean(Servicios)) %>% 
  select(anio, region, agrdesv, indesv, mindesv, ssdesv)

en <- dat %>% 
  filter(LENGUAJE=="English")

sp <- dat %>% 
  filter(LENGUAJE == "Spanish")


# ============= PROCESAMIENTO Y SELECCIÓN DE MODELOS ============

# set ingles
enpib <- en %>% 
  left_join(
    pib,
    by = c("AGNO_FALLO"="anio",
           "regionstd"="region")
  )

txtp <- textProcessor(enpib$NOMBRE_PROYECTO, metadata = enpib)
out <- prepDocuments(txtp$documents, txtp$vocab, txtp$meta)

idealk <- searchK(out$documents, out$vocab, 
                  prevalence = ~s(regordinal) + s(AGNO_FALLO) +
                    as.factor(regionstd) +
                    agrdesv +indesv +mindesv+ssdesv, 
                  K = c(10, 20, 30, 40, 50),
                  data = out$meta)

topicselecten <- as.data.frame(idealk$results)

topicselecten <- apply(topicselecten, 2, as.numeric) %>% 
  as.data.frame()

write_csv2(topicselecten, "topicos optimos en.csv")

model <- selectModel(documents = out$documents,
                     vocab = out$vocab,
             prevalence = ~s(regordinal) + s(AGNO_FALLO) +
                          as.factor(regionstd) +
                          agrdesv +indesv +mindesv+ssdesv,
             K=40,
             runs = 20,
             data = out$meta,
             max.em.its = 500)

# beepr::beep(3)


nromodelo <- rep(1:length(model$runout), 
               each = length(unlist(model$semcoh)) / length(model$runout))

# distribución exclusividad y coherencia
data.frame(
  id = nromodelo,
  ex = unlist(model$exclusivity),
  sc = unlist(model$semcoh)
) %>% 
  ggplot() +
  aes(ex) +
  geom_density()

data.frame(
  id = nromodelo,
  ex = unlist(model$exclusivity),
  sc = unlist(model$semcoh)
) %>% 
  ggplot() +
  aes(sc) +
  geom_density()

# promedios
# data.frame(
#   id = nromodelo,
#   ex = unlist(model$exclusivity),
#   sc = unlist(model$semcoh)
# ) %>% 
#   group_by(id) %>% 
#   summarise(avgex = mean(ex),
#           avsc = mean(sc)) %>% 
#   ggplot() +
#   aes(avgex, avsc) +
#   geom_jitter() +
#   geom_text(aes(label = id))

# medianas
data.frame(
  id = nromodelo,
  ex = unlist(model$exclusivity),
  sc = unlist(model$semcoh)
) %>%
  group_by(id) %>%
  summarise(mex = median(ex),
            msc = median(sc)) %>%
  ggplot() +
  aes(mex, msc) +
  geom_jitter() +
  geom_text(aes(label = id)) +
  labs(x="Exclusividad",
       y="Coherencia semántica") -> g1

modeloelegido <- model$runout[[4]]

save(modeloelegido, file =  "modelostm.RData")
save(modeloelegido, file =  "modelostm.rda")


# set español
espib <- sp %>% 
  left_join(
    pib,
    by = c("AGNO_FALLO"="anio",
           "regionstd"="region")
  )

txtp_es <- textProcessor(espib$NOMBRE_PROYECTO, metadata = espib,
                         language = "spanish")
out_es <- prepDocuments(txtp_es$documents, txtp_es$vocab, txtp_es$meta)


idealk_es <- searchK(out_es$documents, out_es$vocab, 
                  prevalence = ~s(regordinal) + s(AGNO_FALLO) +
                    as.factor(regionstd) +
                    agrdesv +indesv +mindesv+ssdesv, 
                  K = c(10, 20, 30, 40, 50),
                  data = out_es$meta)

# beepr::beep(3)

topicselectes <- as.data.frame(idealk_es$results)

topicselectes <- apply(topicselectes, 2, as.numeric) %>% 
  as.data.frame()

write_csv2(topicselectes, "topicos optimos es.csv")


model_es <- selectModel(documents = out_es$documents,
                       vocab = out_es$vocab,
                       prevalence = ~s(regordinal) + s(AGNO_FALLO) +
                         as.factor(regionstd) +
                         agrdesv +indesv +mindesv+ssdesv,
                       K=40,
                       runs = 20,
                       data = out_es$meta,
                       max.em.its = 500)

# beepr::beep(3)

nromodelo2 <- rep(1:length(model_es$runout), 
                 each = length(unlist(model_es$semcoh)) / length(model_es$runout))

# distribución exclusividad y coherencia
data.frame(
  id = nromodelo2,
  ex = unlist(model_es$exclusivity),
  sc = unlist(model_es$semcoh)
) %>% 
  ggplot() +
  aes(ex) +
  geom_density()

data.frame(
  id = nromodelo2,
  ex = unlist(model_es$exclusivity),
  sc = unlist(model_es$semcoh)
) %>% 
  ggplot() +
  aes(sc) +
  geom_density()

# promedios
data.frame(
  id = nromodelo2,
  ex = unlist(model_es$exclusivity),
  sc = unlist(model_es$semcoh)
) %>%
  group_by(id) %>%
  summarise(avgex = mean(ex),
            avsc = mean(sc)) %>%
  ggplot() +
  aes(avgex, avsc) +
  geom_jitter() +
  geom_text(aes(label = id))

# medianas
data.frame(
  id = nromodelo2,
  ex = unlist(model_es$exclusivity),
  sc = unlist(model_es$semcoh)
)  %>% 
  group_by(id) %>% 
  summarise(mex = median(ex),
            msc = median(sc)) %>% 
  ggplot() +
  aes(mex, msc) +
  geom_jitter() +
  geom_text(aes(label = id)) +
  theme(axis.title.y = element_blank()) +
  labs(x="Exclusividad") -> g2

modeloelegidoes <- model_es$runout[[1]]

save(modeloelegidoes, file =  "modelostmes.RData")
save(modeloelegidoes, file =  "modelostmes.rda")


ggsave(plot = g1+g2, "anexoFig5.png", 
       height = unit(6, "cm"), width = unit(15, "cm"),
       dpi = 1000, limitsize = F)

rm(model, model_es, nromodelo, nromodelo2, txtp_es, txtp,
   topicselecten, topicselectes, idealk_es, idealk,
   en, es)

# =============== INICIO ANALISIS  ==============

# cargar los modelos utilizados en la tesis
# debes tener cargado los objetos "out" para replicar
# txtp <- textProcessor(enpib$NOMBRE_PROYECTO, metadata = enpib)
# out <- prepDocuments(txtp$documents, txtp$vocab, txtp$meta)
# txtp_es <- textProcessor(espib$NOMBRE_PROYECTO, metadata = espib, language = "spanish")
# out_es <- prepDocuments(txtp_es$documents, txtp_es$vocab, txtp_es$meta)

load("modelostm.rda")
load("modelostmes.rda")

par(mfrow = c(1, 2),
    mar = c(2, .5, 2, .5), oma = c(0, 0, 0, 0),
    cex = .9)
plot(modeloelegido, n = 5, main = "Inglés",
     xlab= "Proporción media")
plot(modeloelegidoes, n = 5, main = "Español",
     xlab= "Proporción media")

# ======  EXPLORACIÓN DE LOS TÓPICOS =======

# contenido topicos
ej_es10 <- findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
                       n = 6,
                       topics = 10)$docs[[1]]

unique(ej_es10)

ej_es13 <- findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
                        n = 5,
                        topics = 13)$docs[[1]]

unique(ej_es13)


ej_en38 <- findThoughts(modeloelegido, texts = out$meta$NOMBRE_PROYECTO, 
                        n = 6,
                        topics = 38)$docs[[1]]

unique(ej_en38)


# Exploración componente territorial

# ingles
top20en <- labelTopics(modeloelegido, n = 20)$prob

c("tarapacá", "tarapaca") %in% top20en
"antofagasta" %in% top20en
"atacama" %in% top20en ## si está
"coquimbo" %in% top20en
c("valparaíso", "valparaiso") %in% top20en
c("rm", "santiago") %in% top20en # si está
c("ohiggins", "o'higgins") %in% top20en
"maule" %in% top20en
c("biobio", "bio bio", "bío bío", "bio-bio", "bío-bío") %in% top20en
c("araucanía", "araucania") %in% top20en
c("los lagos") %in% top20en
c("aysén", "aysen") %in% top20en
c("magallanes", "antarctica") %in% top20en


which(apply(top20en, 1, function(row) any(row == "atacama")))

which(apply(top20en, 1, function(row) any(row == "santiago")))


findThoughts(modeloelegido, texts = out$meta$NOMBRE_PROYECTO, 
             n = 10,
             topics = 29)$docs[[1]] %>% unique()


topicQuality(modeloelegido, 
             documents = out$documents)


# español
top20es <- labelTopics(modeloelegidoes, n = 20)$prob

c("tarapacá", "tarapaca") %in% top20es
"antofagasta" %in% top20es
"atacama" %in% top20es ## si esta
"coquimbo" %in% top20es
c("valparaíso", "valparaiso") %in% top20es # si está
c("rm", "santiago") %in% top20es # si esta
c("ohiggins", "o'higgins") %in% top20es
"maule" %in% top20es
c("biobio", "bio bio", "bío bío", "bio-bio", "bío-bío") %in% top20es
c("araucanía", "araucania") %in% top20es # si está
c("los lagos") %in% top20es
c("aysén", "aysen") %in% top20es
c("magallanes", "antartica", "antártica") %in% top20es

which(apply(top20es, 1, function(row) any(row == "atacama")))
which(apply(top20es, 1, function(row) any(row == "santiago")))
which(apply(top20es, 1, function(row) any(row == "araucania")))


findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
             n = 10,
             topics = 6)$docs[[1]] %>% unique()

findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
             n = 10,
             topics = 15)$docs[[1]] %>% unique()

topicQuality(modeloelegidoes, 
             documents = out$documents)



# ================ ESTIMACIÓN DE EFECTO ====================

# INGLÉS EFFECTO
resulten <- estimateEffect(1:40 ~s(regordinal) + s(AGNO_FALLO) + 
                             as.factor(regionstd) + 
                             agrdesv +indesv +mindesv+ssdesv,
                           modeloelegido,
                           meta = out$meta, uncertainty = "Global")

resultable_en <- summary(resulten)$tables


lapply(1:40, function(i){
  
  flag <- any(which(resultable_en[[i]][,"Pr(>|t|)"] < .1))
  # print(i)
  if(flag){
    data.frame(
      variables = names(which(resultable_en[[i]][, "Pr(>|t|)"] < .1)),
      coef = resultable_en[[i]][which(resultable_en[[i]][, "Pr(>|t|)"] < .1), 
                                "Estimate"],
      error = resultable_en[[i]][which(resultable_en[[i]][, "Pr(>|t|)"] < .1), 
                                 "Std. Error"],
      pvalores = resultable_en[[i]][which(resultable_en[[i]][, "Pr(>|t|)"] < .1), 
                                    "Pr(>|t|)"],
      topicos = i
    )
  }
  
}) %>% 
  do.call("bind_rows", .) %>% 
  as.data.frame() -> variablessignificativas_en


write.csv2(variablessignificativas_en,
           "efectos_en.csv",
           row.names = F)



# ESPAÑOL EFFECTO
resultes <- estimateEffect(1:40 ~s(regordinal) + s(AGNO_FALLO) + 
                             as.factor(regionstd) + 
                             agrdesv +indesv +mindesv+ssdesv, 
                           modeloelegidoes,
                           meta = out_es$meta, 
                           uncertainty = "Global")

resultable_es <- summary(resultes)$tables


lapply(1:40, function(i){
  
  flag <- any(which(resultable_es[[i]][,"Pr(>|t|)"] < .1))
  # print(i)
  if(flag){
    data.frame(
      variables = names(which(resultable_es[[i]][, "Pr(>|t|)"] < .1)),
      coef = resultable_es[[i]][which(resultable_es[[i]][, "Pr(>|t|)"] < .1), 
                                "Estimate"],
      error = resultable_es[[i]][which(resultable_es[[i]][, "Pr(>|t|)"] < .1), 
                                 "Std. Error"],
      pvalores = resultable_es[[i]][which(resultable_es[[i]][, "Pr(>|t|)"] < .1), 
                                    "Pr(>|t|)"],
      topicos = i
    )
  }
  
}) %>% 
  do.call("bind_rows", .) %>% 
  as.data.frame() -> variablessignificativas_es


write.csv2(variablessignificativas_es,
           "efectos_es.csv",
           row.names = F)


# ===== RELACIÓN DE TOPICOS Y SECTORES ================
# inglés mineria
findThoughts(modeloelegido, texts = out$meta$NOMBRE_PROYECTO, 
             n = 14,
             topics = 14)$docs[[1]] %>% unique()

# findThoughts(modeloelegido, texts = out$meta$NOMBRE_PROYECTO, 
#              n = 14,
#              topics = 29)$docs[[1]] %>% unique()

data.frame(
  orden = out$meta$regordinal,
  anio = out$meta$AGNO_FALLO,
  region = out$meta$regionstd,
  pmin = out$meta$mindesv,
  theta = modeloelegido$theta[, 14]
) %>% 
group_by(anio, region, orden) %>% 
  summarise(pmin = mean(pmin),
            avgmin = mean(theta)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=pmin, color = "% PIB minería sobre la media"), size = 1.2) +
  geom_line(aes(y=avgmin, color = "% Investigación minería"), size = 1.2) +
  facet_wrap(.~reorder(region, orden)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 10)) +
  # guides(color = "none") +
  theme(legend.position = c(.65, .05),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        strip.text.x = element_text(size = 12)) +
  labs(subtitle = "A)") -> g1


# español minería
findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
             n = 20,
             topics = 39)$docs[[1]] %>% unique()

data.frame(
  orden = out_es$meta$regordinal,
  anio = out_es$meta$AGNO_FALLO,
  region = out_es$meta$regionstd,
  pmin = out_es$meta$mindesv,
  theta = modeloelegidoes$theta[, 39]
) %>% 
  group_by(anio, region, orden) %>% 
  summarise(pmin = mean(pmin),
            avgmin = mean(theta)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=pmin, color = "% PIB minería sobre la media"), size = 1.2) +
  geom_line(aes(y=avgmin, color = "% Investigación minería"), size = 1.2) +
  facet_wrap(.~reorder(region, orden)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 10)) +
  guides(color = "none") +
  theme(legend.position = c(.65, .05),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 12)) +
  labs(subtitle = "B)") -> g2

g1+g2

ggsave(plot = g1+g2, "fig7.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 1000, limitsize = F)

# EXPLORANDO COQUIMBO
data.frame(
  orden = out_es$meta$regordinal,
  anio = out_es$meta$AGNO_FALLO,
  region = out_es$meta$regionstd,
  pmin = out_es$meta$mindesv,
  theta = modeloelegidoes$theta[, 39]
) %>% 
  filter(region == "Coquimbo") %>% 
  group_by(anio, region, orden) %>% 
  summarise(pmin = mean(pmin),
            avgmin = mean(theta)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=pmin, color = "% PIB minería sobre la media"), size = 1.2) +
  geom_line(aes(y=avgmin, color = "% Investigación minería"), size = 1.2) +
  # facet_wrap(.~reorder(region, orden)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 1)) +
  guides(color = "none") +
  theme(legend.position = c(.65, .05),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 12)) 


# OTROS SECTORES
# inglés agro
findThoughts(modeloelegido, texts = out$meta$NOMBRE_PROYECTO, 
             n = 14,
             topics = 35)$docs[[1]] %>% unique()

data.frame(
  orden = out$meta$regordinal,
  anio = out$meta$AGNO_FALLO,
  region = out$meta$regionstd,
  pagro = out$meta$agrdesv,
  theta = modeloelegido$theta[, 35]
) %>% 
  group_by(anio, region, orden) %>% 
  summarise(pagro = mean(pagro),
            avgagro = mean(theta)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=pagro, color = "% PIB agro sobre la media"), size = 1.2) +
  geom_line(aes(y=avgagro, color = "% Investigación agro"), size = 1.2) +
  facet_wrap(.~reorder(region, orden)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-.2, .3, .1),
                     limits = c(-.2, .3)) +
  scale_x_continuous(breaks = seq(1982, 2023, 10)) +
  # guides(color = "none") +
  theme(legend.position = c(.65, .05),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 12)) +
  labs(subtitle = "A)") -> g1

# español agro
findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
             n = 14,
             topics = 38)$docs[[1]] %>% unique()

data.frame(
  orden = out_es$meta$regordinal,
  anio = out_es$meta$AGNO_FALLO,
  region = out_es$meta$regionstd,
  pagro = out_es$meta$agrdesv,
  theta = modeloelegidoes$theta[, 38]
) %>% 
  group_by(anio, region, orden) %>% 
  summarise(pagro = mean(pagro),
            avgagro = mean(theta)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=pagro, color = "% PIB agro sobre la media"), size = 1.2) +
  geom_line(aes(y=avgagro, color = "% Investigación agro"), size = 1.2) +
  facet_wrap(.~reorder(region, orden)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(-.2, .3, .1),
                     limits = c(-.2, .3)) +
  scale_x_continuous(breaks = seq(1982, 2023, 10)) +
  guides(color = "none") +
  theme(legend.position = c(.65, .05),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 12)) +
  labs(subtitle = "B)") -> g2

g1+g2

ggsave(plot = g1+g2, "fig8.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 1000, limitsize = F)


# Explorar los lagos
data.frame(
  orden = out_es$meta$regordinal,
  anio = out_es$meta$AGNO_FALLO,
  region = out_es$meta$regionstd,
  pagro = out_es$meta$agrdesv,
  theta = modeloelegidoes$theta[, 38]
) %>% 
  filter(region == "Los Lagos") %>% 
  group_by(anio, region, orden) %>% 
  summarise(pagro = mean(pagro),
            avgagro = mean(theta)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=pagro, color = "% PIB agro sobre la media"), size = 1.2) +
  geom_line(aes(y=avgagro, color = "% Investigación agro"), size = 1.2) +
  # facet_wrap(.~reorder(region, orden)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 1)) +
  # guides(color = "none") +
  theme(legend.position = c(.8, .8),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 12))

# explorar los lagos
pib %>% 
  filter(region == "Los Lagos") %>% 
  gather(sector, especializacion, 3:6) %>% 
  ggplot() +
  aes(anio, especializacion, color = sector) +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 1)) +
  theme(legend.position = c(.8, .3),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),
        strip.text.x = element_text(size = 12))


# INVESTIGACIÓN REGIONAL
# INGLES
findThoughts(modeloelegido, texts = out$meta$NOMBRE_PROYECTO, 
             n = 20,
             topics = 12)$docs[[1]] %>% unique()

findThoughts(modeloelegido, texts = out$meta$NOMBRE_PROYECTO, 
             n = 20,
             topics = 38)$docs[[1]] %>% unique()

data.frame(
  anio = out$meta$AGNO_FALLO,
  theta1 = modeloelegido$theta[, 12],
  theta2 = modeloelegido$theta[, 38]
) %>% 
  group_by(anio) %>% 
  summarise(th1 = mean(theta1),
            th2 = mean(theta2)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=th1, color = "T. 12"), size = 1.2) +
  geom_line(aes(y=th2, color = "T. 38"), size = 1.2) +
  geom_vline(xintercept = 2008) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 1),
                     expand = c(0, 0)) +
  # guides(color = "none") +
  theme(legend.position = c(.8, .8),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank()
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),)
        ) +
  labs(subtitle = "A)") -> g1


# ESPAÑOL
findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
             n = 14,
             topics = 15)$docs[[1]] %>% unique()

findThoughts(modeloelegidoes, texts = out_es$meta$NOMBRE_PROYECTO, 
             n = 14,
             topics = 30)$docs[[1]] %>% unique()


data.frame(
  anio = out_es$meta$AGNO_FALLO,
  theta1 = modeloelegidoes$theta[, 15],
  theta2 = modeloelegidoes$theta[, 30]
) %>% 
  group_by(anio) %>% 
  summarise(th1 = mean(theta1),
            th2 = mean(theta2)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=th1, color = "T. 15"), size = 1.2) +
  geom_line(aes(y=th2, color = "T. 30"), size = 1.2) +
  geom_vline(xintercept = 2008) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 1),
                     expand = c(0, 0)) +
  # guides(color = "none") +
  theme(legend.position = c(.8, .8),
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank()
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),)
  ) +
  labs(subtitle = "B)") -> g2


ggsave(plot = g1/g2, "fig9.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 1000, limitsize = F)


# gráfica anterior aperturada por región
# inglés
data.frame(
  anio = out$meta$AGNO_FALLO,
  region = out$meta$regionstd,
  ord = out$meta$regordinal,
  theta1 = modeloelegido$theta[, 12],
  theta2 = modeloelegido$theta[, 38]
) %>% 
  group_by(anio, region, ord) %>% 
  summarise(th1 = mean(theta1),
            th2 = mean(theta2)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=th1, color = "T. 12"), size = 1.2) +
  geom_line(aes(y=th2, color = "T. 38"), size = 1.2) +
  geom_vline(xintercept = 2008) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 5),
                     expand = c(0, 0)) +
  facet_wrap(.~reorder(region, ord)) +
  # guides(color = "none") +
  theme(
        # legend.position = c(.5, .05),
        legend.position = "top",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
        axis.title.y = element_blank(),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.spacing = unit(.1, "cm")
        # axis.text.y = element_blank(),
        # axis.ticks.y = element_blank(),)
  ) 


# español
data.frame(
  anio = out_es$meta$AGNO_FALLO,
  region = out_es$meta$regionstd,
  ord = out_es$meta$regordinal,
  theta1 = modeloelegidoes$theta[, 15],
  theta2 = modeloelegidoes$theta[, 30]
) %>% 
  group_by(anio, region, ord) %>% 
  summarise(th1 = mean(theta1),
            th2 = mean(theta2)) %>% 
  ggplot() +
  aes(x=anio) +
  geom_line(aes(y=th1, color = "T. 15"), size = 1.2) +
  geom_line(aes(y=th2, color = "T. 30"), size = 1.2) +
  geom_vline(xintercept = 2008) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = seq(1982, 2023, 5),
                     expand = c(0, 0)) +
  facet_wrap(.~reorder(region, ord)) +
  # guides(color = "none") +
  theme(
    # legend.position = c(.5, .05),
    legend.position = "top",
    legend.title = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = 12, angle = 90, vjust = .5),
    axis.title.y = element_blank(),
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  
    legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
    legend.box.spacing = unit(.1, "cm"),
    legend.text = element_text(size = 21)
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),)
  ) -> g2


ggsave(plot = g2, "fig10.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 1000, limitsize = F)



