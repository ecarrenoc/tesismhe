
library(tidyverse)
library(patchwork)
library(sandwich)
library(fixest)
library(margins)
options(scipen = 999)
theme_set(theme_bw(base_size = 21))


# ===== FUNCIONES PERSONALIZADAS =======

desvest <- function(x){
  x <- na.omit(x)
  sqrt(sum((x - mean(x))^2) / length(x))
}

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

generate_regression_table <- function(models, se_type = "HC4") {
  # Load necessary packages
  library(sandwich)
  library(lmtest)
  
  # Initialize lists to store results
  results_list <- list()
  
  # Function to add asterisks based on p-values
  add_asterisks <- function(coefficient, p_value) {
    if (p_value < 0.01) {
      return(paste0(round(coefficient, 4), "***"))
    } else if (p_value < 0.05) {
      return(paste0(round(coefficient, 4), "**"))
    } else if (p_value < 0.1) {
      return(paste0(round(coefficient, 4), "*"))
    } else {
      return(round(coefficient, 4))
    }
  }
  
  # Loop over each model
  for (i in seq_along(models)) {
    model <- models[[i]]
    
    # Extract model statistics
    coefficients <- coef(model)
    std_errors <- sqrt(diag(vcovHC(model, type = se_type)))
    t_values <- coefficients / std_errors
    p_values <- 2 * pt(-abs(t_values), df = df.residual(model))
    
    # Add asterisks to coefficients based on p-values
    coefficients_with_stars <- mapply(add_asterisks, coefficients, p_values)
    
    # Extract additional model statistics
    n_obs <- length(model$residuals)
    r_squared <- summary(model)$r.squared
    adj_r_squared <- summary(model)$adj.r.squared
    residual_std_error <- summary(model)$sigma
    f_statistic <- summary(model)$fstatistic[1]
    f_p_value <- pf(f_statistic, summary(model)$fstatistic[2], summary(model)$fstatistic[3], lower.tail = FALSE)
    f_statistic_with_stars <- add_asterisks(f_statistic, f_p_value)
    
    # Create a data frame for the coefficients and standard errors
    model_df <- data.frame(
      Term = names(coefficients),
      Coefficient = paste0(coefficients_with_stars, " (", round(std_errors, 4), ")")
    )
    
    # Add other statistics to the model data frame
    model_df <- rbind(
      model_df,
      data.frame(Term = "N", Coefficient = n_obs),
      data.frame(Term = "R^2", Coefficient = round(r_squared, 4)),
      data.frame(Term = "Adj. R^2", Coefficient = round(adj_r_squared, 4)),
      data.frame(Term = "Residual Std. Error", Coefficient = round(residual_std_error, 4)),
      data.frame(Term = "F Statistic", Coefficient = f_statistic_with_stars)
    )
    
    # Store the model results in the list
    results_list[[paste("Model", i)]] <- model_df$Coefficient
  }
  
  # Combine all models' results into a single data frame
  final_results <- data.frame(
    Term = model_df$Term,
    do.call(cbind, results_list)
  )
  
  # Return the final results as a data frame
  return(final_results)
}


# =========  INGESTA DE DATOS ======
dat <- read.csv2("data procesada/dataanidfinal.csv")
matricula <- read.csv2("data procesada/consolidado matricula final.csv",
                       fileEncoding = "latin1") 

dat$montomensual <- dat$montomilesreal/dat$DURACION_MESES

totmonto <- dat %>% 
  group_by(AGNO_FALLO, regionstd) %>% 
  summarise(qpreoyectos= n(),
            montotal = sum(montomilesreal),
            montotalmensual = sum(montomensual)) %>% 
  as.data.frame() %>% 
  complete(regionstd, AGNO_FALLO = 1982:2023,
           fill = list(qpreoyectos = 0,
                       montotal = 0,
                       montotalmensual = 0))

consoconvergencia <- totmonto %>% 
  left_join(matricula, by = c("AGNO_FALLO"="anio",
                              "regionstd"="region"))

consoconvergencia <- consoconvergencia %>% 
  mutate(reg_ordinal = regordinal(regionstd),
         q_pc = qpreoyectos/mat_hat,
         montopc = montotal/mat_hat,
         montotalmensualpc = montotalmensual/mat_hat)


# ============ PROCESO DE ANÁLISIS =============

# sigma convergencia
consoconvergencia %>% 
  group_by(AGNO_FALLO) %>% 
  summarise(cv_q = desvest(q_pc)/mean(q_pc, na.rm = T),
            cv_total = desvest(montopc)/mean(montopc, na.rm = T),
            cv_tmensual = desvest(montotalmensualpc)/mean(montotalmensualpc, na.rm = T)) %>% 
  ggplot() +
  aes(x=AGNO_FALLO) +
  geom_line(aes(y=cv_q, colour = "Cantidad"), size = 1.2) +
  geom_line(aes(y=cv_total, color = "Monto"), size = 1.2) +
  geom_line(aes(y=cv_tmensual, color = "Monto mensual"), size = 1.2) +
  scale_x_continuous(breaks = seq(1982, 2023, 1),
                     limits = c(1982, 2023),
                     expand = c(.01,.01)) +
  geom_vline(xintercept = c(1990, 1992, 2008, 2014, 2018)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.title = element_blank(),
        legend.position = "top",
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),  
        legend.box.margin = margin(t = 0, r = 0, b = 0, l = 0),
        legend.box.spacing = unit(.1, "cm"),
        legend.text = element_text(size = 21)) + 
  labs(subtitle = "A)") -> g1



# concentración santiago
stgocon <- consoconvergencia %>%
  group_by(AGNO_FALLO) %>% 
  summarise(qpreoyectos = sum(qpreoyectos),
            montotal = sum(montotal),
            montotalmensual = sum(montotalmensual),
            mat_hat = sum(mat_hat)) %>% 
  left_join(
    consoconvergencia %>%
      filter(regionstd=="RM") %>% 
      group_by(AGNO_FALLO) %>% 
      summarise(rm_qpreoyectos = sum(qpreoyectos),
                rm_montotal = sum(montotal),
                rm_montotalmensual = sum(montotalmensual),
                rm_mat_hat = sum(mat_hat))
  )

stgocon <- stgocon %>% 
  mutate(
    pproyectos = rm_qpreoyectos/qpreoyectos,
    pmontotal = rm_montotal/montotal,
    pmontotalmensual = rm_montotalmensual/montotalmensual,
    pproyectospc = (rm_qpreoyectos/rm_mat_hat)/(qpreoyectos/mat_hat),
    pmontotalpc = (rm_montotal/rm_mat_hat)/(montotal/mat_hat),
    pmontotalmensualpc = (rm_montotalmensual/rm_mat_hat)/(montotalmensual/mat_hat)
  )


stgocon %>% 
  ggplot() +
  aes(x=AGNO_FALLO) +
  geom_line(aes(y=pproyectos, color ="Cantidad"), size = 1.2) +
  geom_line(aes(y=pmontotal, color ="Total"), size = 1.2) +
  geom_line(aes(y=pmontotalmensual, color ="Total mensual"), size = 1.2) +
  guides(color = "none") +
  scale_x_continuous(breaks = seq(1982, 2023, 1),
                     limits = c(1982, 2023),
                     expand = c(.01,.01)) +
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = c(1990, 1992, 2008, 2014, 2018)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5),
        legend.title = element_blank(),
        legend.position = c(.8, .8)) + 
  labs(subtitle = "B)") -> g2


fig3 <- g1/g2

ggsave(plot = fig3, "fig3.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 600, limitsize = F)

# distribuciones regionales 
consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO <=1990) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(desc(p)) %>% 
  mutate(cump = cumsum(p)) %>% 
  select(p) %>%
  unlist() -> p1990

consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO <=1990) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p1990 = totproy/sum(totproy)) %>% 
  arrange(desc(p1990)) %>% 
  mutate(cump = cumsum(p1990)) %>% 
  as.data.frame() %>% 
  select(regionstd, p1990) -> t1


consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=1991 & AGNO_FALLO <= 2008) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(desc(p)) %>% 
  mutate(cump = cumsum(p)) %>% 
  select(p) %>%
  unlist() -> p2008

consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2000 & AGNO_FALLO <= 2008) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p2008 = totproy/sum(totproy)) %>% 
  arrange(desc(p2008)) %>% 
  mutate(cump = cumsum(p2008)) %>% 
  as.data.frame() %>% 
  select(regionstd, p2008) -> t2


t1 %>% 
  left_join(t2) %>% 
  mutate(delta = p2008-p1990) %>% 
  arrange(desc(delta))


consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2009 & AGNO_FALLO <= 2018) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(desc(p)) %>% 
  mutate(cump = cumsum(p)) %>% 
  select(p) %>% 
  unlist() -> p2018


consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2009 & AGNO_FALLO <= 2013) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(desc(p)) %>% 
  mutate(cump = cumsum(p))

consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2014 & AGNO_FALLO <= 2016) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(desc(p)) %>% 
  mutate(cump = cumsum(p))

consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2014 & AGNO_FALLO <= 2018) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc, na.rm = T)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(desc(p)) %>% 
  mutate(cump = cumsum(p))

# curvas de Lorenz
bind_rows(data.frame(ciclo = "1982-1990", p = p1990),
          data.frame(ciclo = "1991-2008", p = p2008),
          data.frame(ciclo = "2009-2018", p = p2018)) %>% 
  group_by(ciclo) %>% 
  arrange(ciclo, p) %>% 
  mutate(cump = cumsum(p),
         cumpreg = row_number()) %>% 
  ggplot() +
  aes(cumpreg, cump, color = ciclo) +
  geom_line(size = 1.2) +
  scale_y_continuous(breaks = seq(0, 1, .05),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 13, 1),
                     expand = c(0,0)) +
  theme(legend.position = c(.8, .3),
        legend.title = element_blank()) +
  labs(x="Cantidad acumulada de regiones",
       y="Proporción acumulada de proyectos") -> fig4

ggsave(plot = fig4, "fig4.png", 
       height = unit(10, "cm"), width = unit(15, "cm"),
       dpi = 1000, limitsize = F)


# variaciones en cantidad de proyectos a distintos cohortes
consoconvergencia %>% 
  group_by(AGNO_FALLO) %>% 
  summarise(q = sum(qpreoyectos)) %>% 
  filter(AGNO_FALLO >= 2010)

consoconvergencia %>% 
  group_by(AGNO_FALLO) %>% 
  summarise(q = sum(qpreoyectos)) %>% 
  ggplot() +
  aes(AGNO_FALLO, q) +
  geom_line()

consoconvergencia %>% 
  filter(AGNO_FALLO == 2018) %>% 
  group_by(regionstd) %>% 
  summarise(q1 = sum(qpreoyectos)) %>% 
  left_join(
    consoconvergencia %>% 
      filter(AGNO_FALLO == 2021) %>% 
      group_by(regionstd) %>% 
      summarise(q2 = sum(qpreoyectos)),
    by = c("regionstd")
  ) %>% 
  mutate(diff = q2-q1,
         delta = (q2-q1)/q1) %>% 
  arrange(desc(delta))


consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2022) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(desc(p)) %>% 
  mutate(cump = cumsum(p)) %>% 
  select(p) %>% 
  unlist() -> p2023

# densidades
ggplot() +
  geom_density(aes(x=p1990, color = "<1990"), size = 1.2) +
  geom_density(aes(x=p2008, color = "2000-2008"), size = 1.2) +
  geom_density(aes(x=p2018, color = "2009-2018"), size = 1.2) +
  geom_density(aes(x=p2023, color = ">2022"), size = 1.2) 


bind_rows(data.frame(ciclo = "1982-1990", p = p1990),
          data.frame(ciclo = "1991-2008", p = p2008),
          data.frame(ciclo = "2009-2018", p = p2018),
          data.frame(ciclo = "2019-2023", p = p2023)) %>% 
  group_by(ciclo) %>% 
  arrange(ciclo, p) %>% 
  mutate(cump = cumsum(p),
         cumpreg = row_number()) %>% 
  ggplot() +
  aes(cumpreg, cump, color = ciclo) +
  geom_line(size = 1.2) +
  scale_y_continuous(breaks = seq(0, 1, .05),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 13, 1),
                     expand = c(0,0)) +
  theme(legend.position = c(.8, .3),
        legend.title = element_blank()) +
  labs(x="Cantidad acumulada de regiones",
       y="Proporción acumulada de proyectos") 

consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2019) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(q_pc)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(p) %>% 
  mutate(cump = cumsum(p))

consoconvergencia %>% 
  as.data.frame() %>% 
  filter(AGNO_FALLO >=2019) %>% 
  group_by(regionstd) %>% 
  summarise(totproy = sum(montotalmensualpc/mat_hat)) %>% 
  mutate(p = totproy/sum(totproy)) %>% 
  arrange(p) %>% 
  mutate(cump = cumsum(p))


# ===== VELOCIDADES DE CONVERGENCIA =====
# q proyectos
cortes <- consoconvergencia %>% 
  select(AGNO_FALLO, regionstd, q_pc) %>% 
  filter(AGNO_FALLO %in% c(1982, 1985, 1990, 1991, 2008, 2009, 2018, 2023)) %>% 
  mutate(AGNO_FALLO = paste0("y", AGNO_FALLO)) %>% 
  spread(AGNO_FALLO, q_pc) %>% 
  mutate(total = log((y2023+1)/(y1982+1)),
         c1 = log((y1990+1)/(y1982+1)),
         c2 = log((y2008+1)/(y1991+1)),
         c3 = log((y2018+1)/(y2009+1))
         ) 

mtotal <- lm(total~log(y1982+1), data = cortes)
mc1 <- lm(c1~log(y1982+1), data = cortes)
mc2 <- lm(c2~log(y1991+1), data = cortes)
mc3 <- lm(c3~log(y2009+1), data = cortes)

-mtotal$coefficients[2]/(2023-1982)*100
-mc1$coefficients[2]/(1990-1982)*100
-mc2$coefficients[2]/(2008-1991)*100
-mc3$coefficients[2]/(2018-2009)*100

cortes %>% 
  ggplot() +
  aes(log(y1982+1), total) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = "y~log(x+1)")


# monto total
cortes2 <- consoconvergencia %>% 
  select(AGNO_FALLO, regionstd, montopc) %>% 
  filter(AGNO_FALLO %in% c(1982, 1985, 1990, 1991, 2008, 2009, 2018, 2023)) %>% 
  mutate(AGNO_FALLO = paste0("y", AGNO_FALLO)) %>% 
  spread(AGNO_FALLO, montopc) %>% 
  mutate(total = log((y2023+1)/(y1982+1)),
         c1 = log((y1990+1)/(y1982+1)),
         c2 = log((y2008+1)/(y1991+1)),
         c3 = log((y2018+1)/(y2009+1))
  ) 

mtotal <- lm(total~log(y1982+1), data = cortes2)
mc1 <- lm(c1~log(y1982+1), data = cortes2)
mc2 <- lm(c2~log(y1991+1), data = cortes2)
mc3 <- lm(c3~log(y2009+1), data = cortes2)

-mtotal$coefficients[2]/(2023-1982)*100
-mc1$coefficients[2]/(1990-1982)*100
-mc2$coefficients[2]/(2008-1991)*100
-mc3$coefficients[2]/(2018-2009)*100

cortes2 %>% 
  ggplot() +
  aes(log(y1982+1), total) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = "y~log(x+1)")


# monto mensual
cortes3 <- consoconvergencia %>% 
  select(AGNO_FALLO, regionstd, montotalmensualpc) %>% 
  filter(AGNO_FALLO %in% c(1982, 1985, 1990, 1991, 2008, 2009, 2018, 2023)) %>% 
  mutate(AGNO_FALLO = paste0("y", AGNO_FALLO)) %>% 
  spread(AGNO_FALLO, montotalmensualpc) %>% 
  mutate(total = log((y2023+1)/(y1982+1)),
         c1 = log((y1990+1)/(y1982+1)),
         c2 = log((y2008+1)/(y1991+1)),
         c3 = log((y2018+1)/(y2009+1))
  ) 

mtotal <- lm(total~log(y1982+1), data = cortes3)
mc1 <- lm(c1~log(y1982+1), data = cortes3)
mc2 <- lm(c2~log(y1991+1), data = cortes3)
mc3 <- lm(c3~log(y2009+1), data = cortes3)

-mtotal$coefficients[2]/(2023-1982)*100
-mc1$coefficients[2]/(1990-1982)*100
-mc2$coefficients[2]/(2008-1991)*100
-mc3$coefficients[2]/(2018-2009)*100

cortes3 %>% 
  ggplot() +
  aes(log(y1982+1), total) +
  geom_jitter() +
  geom_smooth(method = "lm", formula = "y~log(x+1)")


# ========= COMPONENTES ESTRUCTURALES EN CONVERGENCIA ============

set1 <- cortes %>% 
  select(regionstd, y1985, y2018) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2018+1)/(y1985+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1985, 2018)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2018-Agrario1985,
             deltamin = Minería2018-Minería1985,
             deltaind = Industria2018-Industria1985,
             deltass = Servicios2018-Servicios1985) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1985, 2018)) %>% 
      group_by(anio, region) %>% 
      summarise(pibregional = sum(pib),
                pobregional = sum(pob_hat)) %>% 
      mutate(pibpcregional = pibregional/pobregional) %>% 
      group_by(anio) %>% 
      mutate(ppib = pibpcregional/sum(pibpcregional)) %>% 
      mutate(anio = paste0("yp", anio)) %>% 
      select(region, anio, ppib) %>% 
      as.data.frame() %>% 
      spread(anio, ppib) %>% 
      mutate(deltapibpc = yp2018-yp1985) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  )


mod1 <- lm(log(deltaproy) ~ 
                log(y1985+1) +
                regionord +
                # deltapibpc +
                deltagro+
                deltamin+ 
                deltaind+
                deltass,
              data = set1)


set2 <- cortes2 %>% 
  select(regionstd, y1985, y2018) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2018+1)/(y1985+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1985, 2018)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2018-Agrario1985,
             deltamin = Minería2018-Minería1985,
             deltaind = Industria2018-Industria1985,
             deltass = Servicios2018-Servicios1985) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1985, 2018)) %>% 
      group_by(anio, region) %>% 
      summarise(pibregional = sum(pib),
                pobregional = sum(pob_hat)) %>% 
      mutate(pibpcregional = pibregional/pobregional) %>% 
      group_by(anio) %>% 
      mutate(ppib = pibpcregional/sum(pibpcregional)) %>% 
      mutate(anio = paste0("yp", anio)) %>% 
      select(region, anio, ppib) %>% 
      as.data.frame() %>% 
      spread(anio, ppib) %>% 
      mutate(deltapibpc = yp2018-yp1985) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  )


mod2 <- lm(log(deltaproy) ~ 
             log(y1985+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
              data = set2)


set3 <- cortes3 %>% 
  select(regionstd, y1985, y2018) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2018+1)/(y1985+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1985, 2018)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2018-Agrario1985,
             deltamin = Minería2018-Minería1985,
             deltaind = Industria2018-Industria1985,
             deltass = Servicios2018-Servicios1985) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1985, 2018)) %>% 
      group_by(anio, region) %>% 
      summarise(pibregional = sum(pib),
                pobregional = sum(pob_hat)) %>% 
      mutate(pibpcregional = pibregional/pobregional) %>% 
      group_by(anio) %>% 
      mutate(ppib = pibpcregional/sum(pibpcregional)) %>% 
      mutate(anio = paste0("yp", anio)) %>% 
      select(region, anio, ppib) %>% 
      as.data.frame() %>% 
      spread(anio, ppib) %>% 
      mutate(deltapibpc = yp2018-yp1985) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  )


mod3 <- lm(log(deltaproy) ~ 
             log(y1985+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
              data = set3)


tablaout <- generate_regression_table(list(mod1, mod2, mod3), 
                                      se_type = "HC4")

write.csv2(tablaout, "resultadosreg1.csv", 
           row.names = F)

# efecto de región ordinal
dat <- cplot(mod1, "regionord",
             vcov = vcovHC(mod1, type = "HC4"), 
             draw = F) %>% 
  filter(xvals %in% 1:13) %>% 
  left_join(
    set1 %>% 
      select(regionord,regionstd, deltaproy),
    by = c("xvals" = "regionord")
  )

dat2 <- cplot(mod2, "regionord",
             vcov = vcovHC(mod1, type = "HC4"), 
             draw = F) %>% 
  filter(xvals %in% 1:13) %>% 
  left_join(
    set2 %>% 
      select(regionord,regionstd, deltaproy),
    by = c("xvals" = "regionord")
  )

dat3 <- cplot(mod3, "regionord",
              vcov = vcovHC(mod1, type = "HC4"), 
              draw = F) %>% 
  filter(xvals %in% 1:13) %>% 
  left_join(
    set3 %>% 
      select(regionord,regionstd, deltaproy),
    by = c("xvals" = "regionord")
  )


dat %>% 
  ggplot() +
  aes(xvals, yvals) +
  geom_line() +
  geom_line(aes(y = upper), linetype = "dashed") +
  geom_line(aes(y = lower), linetype = "dashed") +
  geom_text(aes(y= log(deltaproy),label = regionstd)) +
  scale_x_continuous(breaks = 1:13) +
  theme(axis.title.x = element_blank()) + 
  labs(y = "Convergencia predicha",
       subtitle = "A)") -> g1

dat2 %>% 
  ggplot() +
  aes(xvals, yvals) +
  geom_line() +
  geom_line(aes(y = upper), linetype = "dashed") +
  geom_line(aes(y = lower), linetype = "dashed") +
  geom_text(aes(y= log(deltaproy),label = regionstd)) +
  scale_x_continuous(breaks = 1:13) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

dat3 %>% 
  ggplot() +
  aes(xvals, yvals) +
  geom_line() +
  geom_line(aes(y = upper), linetype = "dashed") +
  geom_line(aes(y = lower), linetype = "dashed") +
  geom_text(aes(y= log(deltaproy),label = regionstd)) +
  scale_x_continuous(breaks = 1:13) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(subtitle = "B)")-> g3


ggsave(plot = g1+g3 , "fig5.png", 
       height = unit(6, "cm"), width = unit(15, "cm"),
       dpi = 1000, limitsize = F)




