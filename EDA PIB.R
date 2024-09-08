
library(tidyverse)
library(imputeTS)
library(ineq)
library(patchwork)
theme_set(theme_classic(base_size = 25))
options(scipen = 999)


consolidado <- read.csv2("data procesada/pibs y pobla final.csv", encoding = "latin1")

consolidado$origen <- factor(consolidado$origen,
                             unique(consolidado$origen))

consolidado$region <- factor(consolidado$region,
                             unique(consolidado$region)[c(12, 1, 3, 6, 13, 11, 10, 9, 5, 2, 7, 4, 8)])


desvest <- function(x){
  
  sqrt((sum((x - mean(x))^2))/length(x))
  
}

# EXPLORANDO DESIGUALDADES REGIONALES ===================
consolidado %>% 
  group_by(origen, anio, region) %>% 
  # summarise(totpibpc = sum(pib)/mean(pob_hat)) %>% 
  summarise(totpib = sum(pib)) %>% 
  group_by(anio, origen) %>% 
  summarise(g = Gini(totpib)) %>% 
  ggplot() +
  aes(anio, g, linetype = origen) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1888, 2023, 5),
                     limits = c(1888, 2023),
                     expand = c(.01,.01)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.8, .2), 
        legend.title = element_blank()) + 
  labs(title = "A)",
       color = "Fuente") -> g1

# CV
# consolidado %>% 
#   group_by(origen, anio, region) %>% 
#   # summarise(totpibpc = sum(pib)/mean(pob_hat)) %>% 
#   summarise(totpib = sum(pib)) %>% 
#   group_by(anio, origen) %>% 
#   summarise(g = desvest(totpib)/mean(totpib)) %>% 
#   ggplot() +
#   aes(anio, g, linetype = origen) +
#   geom_line(linewidth = 1.2) +
#   scale_x_continuous(breaks = seq(1888, 2023, 5),
#                      limits = c(1888, 2023),
#                      expand = c(.01,.01)) +
#   theme(axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.text.x = element_text(angle = 90, vjust = .5),
#         legend.position = c(.8, .2), 
#         legend.title = element_blank()) 



# EXPLORANDO CONCENTRACIÓN SECTORIAL ===================
consolidado %>% 
  group_by(origen, region, anio, sector3) %>% 
  summarise(pibSEC3 = sum(pib)) %>% 
  group_by(origen, region, anio) %>% 
  mutate(totpibSEC3 = sum(pibSEC3)) %>% 
  mutate(p_regionpibSEC3 = pibSEC3/totpibSEC3) %>% 
  group_by(origen, region, anio) %>% 
  slice_max(order_by = p_regionpibSEC3) %>% 
  group_by(origen, anio) %>%
  summarise(IESec = mean(p_regionpibSEC3, na.rm = T), 
            IESecSD = sd(p_regionpibSEC3, na.rm = T)) %>% 
  ggplot() + 
  aes(anio, IESec, 
      ymin = IESec-IESecSD, ymax = IESec+IESecSD,
      linetype = origen) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(fill = origen),alpha = .3, color = "white") +
  guides(fill = "none", color = "none", linetype = "none") +
  scale_x_continuous(breaks = seq(1888, 2023, 5),
                     limits = c(1888, 2023),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.8, .8)) + 
  labs(title = "B)",
       color = "Fuente") -> g2

# FIGURA 1 ========================
g1/g2 -> fig1

ggsave(plot = fig1, "fig1.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)

# CONCENTRACIÓN POR REGIONES ====================
consolidado %>% 
  group_by(origen, anio, region) %>% 
  summarise(regpib = sum(pib),
            regpob = mean(pob_hat),
            regpibpc = sum(pib)/mean(pob_hat)) %>% 
  group_by(origen, anio) %>% 
  mutate(totpib = sum(regpib),
         totpob = sum(regpob),
         totpibpc = sum(regpibpc)) %>% 
  mutate(p_pib = regpib/totpib,
         p_pob = regpob/totpob,
         p_pibpc = regpibpc/totpibpc) %>% 
  as.data.frame() -> regs

regs %>% 
  ggplot() +
  aes(anio, p_pib, linetype = origen) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1888, 2022, 5),
                     limits = c(1888, 2022),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~region) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = c(.8, .1),
        legend.title = element_blank()) 

regs %>% 
  ggplot() +
  aes(anio, p_pib, linetype = origen) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1970, 2023, 5),
                     limits = c(1970, 2023),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~region) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = c(.8, .1),
        legend.title = element_blank()) 



regs %>% 
  # filter(region == "RM") %>% 
  ggplot() +
  aes(anio, p_pibpc, linetype = origen) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1888, 2022, 5),
                     limits = c(1888, 2022),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~region) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = c(.8, .1),
        legend.title = element_blank()) 

regs %>% 
  ggplot() +
  aes(anio, p_pob, color = region) +
  guides(color = "none") +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1888, 2022, 5),
                     limits = c(1888, 2022),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~region) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        axis.text.y = element_text(size = 9),
        legend.position = c(.2, .8),
        legend.title = element_blank()) +
  labs(title = "% población nacional")


regs %>%
  filter(anio >= 1960,
         anio <= 1970,
         origen == "Badia-Miró",
         region == "RM") %>%
  as.data.frame() %>%
  select(p_pibpc) %>%
  unlist() %>%
  mean()

regs %>%
  filter(anio >= 1960,
         anio <= 1970,
         origen == "Mideplan",
         region == "RM") %>%
  as.data.frame() %>%
  select(p_pibpc) %>%
  unlist() %>%
  mean()


regs %>%
  filter(anio >= 1960,
         anio <= 1970,
         region == "RM") %>%
  as.data.frame() %>%
  select(p_pob) %>%
  unlist() %>%
  mean()

regs %>%
  filter(anio >= 1990,
         anio <= 2000,
         origen == "Mideplan") %>%
  as.data.frame() %>%
  select(p_pib) %>%
  unlist() %>%
  mean()

regs %>%
  filter(anio >= 1990,
         anio <= 2000,
         origen == "Banco Central") %>%
  as.data.frame() %>%
  select(p_pib) %>%
  unlist() %>%
  mean()



# CRECIMIENTO DE LAS REGIONES
regs %>% 
  # filter(anio >= 1960) %>% 
  ggplot() +
  aes(anio, p_pibpc, linetype = origen) +
  geom_line(linewidth = 1.2) +
  facet_wrap(.~region) +
  scale_x_continuous(breaks = seq(1888, 2023, 5),
                     limits = c(1888, 2023),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.8, 0),
        legend.title = element_blank())




# SECTOR MÁS INTENSO POR REGIÓN
consolidado %>% 
  group_by(origen, region, anio, sector3) %>% 
  summarise(pibSEC3 = sum(pib)) %>% 
  group_by(origen, region, anio) %>% 
  mutate(totpibSEC3 = sum(pibSEC3)) %>% 
  mutate(p_regionpibSEC3 = pibSEC3/totpibSEC3) %>% 
  ggplot() +
  aes(x=region, y =p_regionpibSEC3, color = sector3) +
  guides(color = "none") +
  geom_boxplot() +
  facet_wrap(.~sector3) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .4, hjust = 1)) -> fig2

ggsave(plot = fig2, "fig2.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)






# =================== GRAFICAS ANEXAS ================

# SECTORES EN EL LARGO PLAZO 
consolidado %>% 
  group_by(origen, anio, sector3) %>% 
  summarise(totpibSEC3 = sum(pib)) %>% 
  group_by(origen, anio) %>% 
  mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
  ggplot() +
  aes(anio, p_SEC3, color = sector3, linetype = origen) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1888, 2022, 5),
                     limits = c(1888, 2022),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5)) +
  labs(color = "Sector",
       linetype = "Fuente") -> anexo1

ggsave(plot = anexo1, "anexoFig1.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)

# DISTRIBUCIÓN SECTORES
consolidado %>% 
  group_by(origen, anio, sector3) %>% 
  summarise(totpibSEC3 = sum(pib)) %>% 
  group_by(origen, anio) %>% 
  mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
  ggplot() +
  aes(anio, p_SEC3, color = sector3, linetype = origen) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = seq(1888, 2022, 5),
                     limits = c(1888, 2022),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5)) +
  labs(color = "Sector",
       linetype = "Fuente")



# SECTORES CON OTROS
consolidado %>% 
  filter(sector2 != "Otros") %>% 
  group_by(origen, region, anio, sector2) %>% 
  summarise(pibSEC2 = sum(pib)) %>% 
  group_by(origen, region, anio) %>% 
  mutate(totpibSEC2 = sum(pibSEC2)) %>% 
  mutate(p_regionpibSEC2 = pibSEC2/totpibSEC2) %>% 
  group_by(origen, region, anio) %>% 
  slice_max(order_by = p_regionpibSEC2) %>% 
  group_by(origen, anio) %>%
  summarise(IESec = mean(p_regionpibSEC2, na.rm = T), 
            IESecSD = sd(p_regionpibSEC2, na.rm = T)) %>% 
  ggplot() + 
  aes(anio, IESec, 
      ymin = IESec-IESecSD, ymax = IESec+IESecSD,
      linetype = origen) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(fill = origen),alpha = .3, color = "white") +
  # guides(fill = "none", color = "none", linetype = "none") +
  scale_x_continuous(breaks = seq(1888, 2023, 5),
                     limits = c(1888, 2023),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.position = c(.8, .8),
        legend.title = element_blank()) -> anexo1

ggsave(plot = anexo1, "anexoFig1.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)



# ESTUDIANDO ESPECIFICAMENTE EL SECTOR SERVICIOS
consolidado %>% 
  filter(origen == "Banco Central",
         sector3 == "Servicios") %>% 
  mutate(sector = case_when(
    sector %in% c("comercio", 
                  "restaurantes y hoteles") ~ "comercio, restaurantes y hoteles",
    T~sector
  )) %>% 
  group_by(anio) %>% 
  mutate(totpib = sum(pib)) %>% 
  as.data.frame() %>% 
  group_by(anio, sector) %>% 
  summarise(p_pibsec = sum(pib)/mean(totpib)) -> servicios

servicios %>% 
  ggplot() +
  aes(anio, p_pibsec, color = sector) +
  guides(color = "none") +
  geom_line(linewidth = 1.2) +
  facet_wrap(.~str_wrap(sector, 30)) +
  scale_x_continuous(breaks = seq(1985, 2023, 5),
                     limits = c(1985, 2023),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, 1, .05)) +
  geom_hline(yintercept = .1) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        axis.text.y = element_text(size = 9),
        legend.position = c(.8, 0),
        legend.title = element_blank(),
        strip.text.x = element_text(size = 15))


  # sectores %>% 
  #   filter(anio == 1985) %>% 
  #   as.data.frame() %>% 
  #   select(p_pibsec) %>% 
  #   unlist() %>% sum()
  














