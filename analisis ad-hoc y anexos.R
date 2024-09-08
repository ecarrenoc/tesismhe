
library(tidyverse)
library(patchwork)
library(sandwich)
library(fixest)
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


# ================= ANALISIS AD-HOC =============

# exploración tendencias por región
consoconvergencia %>% 
  ggplot() +
  aes(AGNO_FALLO, qpreoyectos, color = regionstd) +
  guides(color = "none") +
  geom_line() +
  facet_wrap(.~reorder(regionstd, reg_ordinal), scales = "free_y") +
  scale_x_continuous(breaks = seq(1984, 2023, 2),
                     limits = c(1984, 2023),
                     expand = c(.01,.01)) +
  geom_vline(xintercept = 2014) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(subtitle = "qproyectos")

consoconvergencia %>% 
  # filter(AGNO_FALLO >=2008) %>%
  ggplot() +
  aes(AGNO_FALLO, q_pc, color = regionstd) +
  guides(color = "none") +
  geom_line() +
  facet_wrap(.~reorder(regionstd, reg_ordinal)) +
  scale_x_continuous(breaks = seq(1984, 2023, 2),
                     # limits = c(1984, 2023),
                     expand = c(.01,.01)) +
  geom_vline(xintercept = 2014) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(subtitle = "qproyectospc")


consoconvergencia %>% 
  filter(AGNO_FALLO >=2014) %>%
  ggplot() +
  aes(AGNO_FALLO, montopc, color = regionstd) +
  guides(color = "none") +
  geom_line() +
  facet_wrap(.~reorder(regionstd, reg_ordinal)) +
  scale_x_continuous(breaks = seq(1984, 2023, 2),
                     # limits = c(1984, 2023),
                     expand = c(.01,.01)) +
  geom_vline(xintercept = 2014) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(subtitle = "monto pc")


consoconvergencia %>% 
  group_by(regionstd) %>% 
  summarise(cv = desvest(qpreoyectos)/mean(qpreoyectos)) %>% 
  arrange(desc(cv))

consoconvergencia %>% 
  ggplot() +
  aes(AGNO_FALLO, mat_hat, color = regionstd) +
  guides(color = "none") +
  geom_line() +
  facet_wrap(.~reorder(regionstd, reg_ordinal), scales = "free_y") +
  scale_x_continuous(breaks = seq(1984, 2023, 2),
                     limits = c(1984, 2023),
                     expand = c(.01,.01)) +
  geom_vline(xintercept = 2014) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        legend.title = element_blank(),
        legend.position = c(.8, .8)) 


# explorar año de formación de Universidades
temp <- readxl::read_excel("data raw/01_BaseINDICES_Pregrado.xlsx")

ues <- temp %>% 
  filter(`Tipo Institución` == "Univ.") %>% 
  group_by(`Nombre Region`, `Nombre Institución`) %>% 
  summarise(anioinicio = min(`Año Inicio Actividades`))


# Recuento de instrumentos en el tiempo
dat2 <- read.csv2("data raw/BDH_HISTORICA ANID.csv")

dat2 %>% 
  group_by(AGNO_FALLO) %>% 
  summarise(n = length(unique(INSTRUMENTO))) %>% 
  ggplot() +
  aes(AGNO_FALLO, n) +
  geom_line() +
  scale_x_continuous(breaks = seq(1984, 2023, 2),
                     limits = c(1984, 2023),
                     expand = c(.01,.01)) +
  geom_vline(xintercept = 2014) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(subtitle = "qproyectos")


# explorando coquimbo
consoconvergencia %>% 
  filter(regionstd == "Coquimbo") %>% 
  ggplot() +
  aes(AGNO_FALLO, q_pc, color = regionstd) +
  guides(color = "none") +
  geom_line() +
  facet_wrap(.~reorder(regionstd, reg_ordinal), scales = "free_y") +
  scale_x_continuous(breaks = seq(1984, 2023, 2),
                     limits = c(1984, 2023),
                     expand = c(.01,.01)) +
  geom_vline(xintercept = 2014) +
  theme(axis.title.x = element_blank(),
        # axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = .5, size = 9),
        legend.title = element_blank(),
        legend.position = c(.8, .8)) +
  labs(subtitle = "qproyectos")


consoconvergencia %>% 
  filter(regionstd == "Coquimbo",
         AGNO_FALLO >= 1993 & AGNO_FALLO <= 2009) %>% 
  select(qpreoyectos) %>% 
  unlist() %>% 
  mean()

consoconvergencia %>% 
  filter(AGNO_FALLO >= 1993 & AGNO_FALLO <= 2009) %>% 
  select(qpreoyectos) %>% 
  unlist() %>% 
  median()



# ============ CONVERGENCIA POR COMPONENTES ESTRUCTURALES ==========


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



# ==== 1991 - 2008

set1 <- cortes %>% 
  select(regionstd, y1991, y2008) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2008+1)/(y1991+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1991, 2008)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2008-Agrario1991,
             deltamin = Minería2008-Minería1991,
             deltaind = Industria2008-Industria1991,
             deltass = Servicios2008-Servicios1991) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1991, 2008)) %>% 
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
      mutate(deltapibpc = yp2008-yp1991) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  )


mod1 <- lm(log(deltaproy) ~ 
             log(y1991+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
           data = set1)



set2 <- cortes2 %>% 
  select(regionstd, y1991, y2008) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2008+1)/(y1991+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1991, 2008)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2008-Agrario1991,
             deltamin = Minería2008-Minería1991,
             deltaind = Industria2008-Industria1991,
             deltass = Servicios2008-Servicios1991) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1991, 2008)) %>% 
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
      mutate(deltapibpc = yp2008-yp1991) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  )


mod2 <- lm(log(deltaproy) ~ 
             log(y1991+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
           data = set2)


set3 <- cortes3 %>% 
  select(regionstd, y1991, y2008) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2008+1)/(y1991+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1991, 2008)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2008-Agrario1991,
             deltamin = Minería2008-Minería1991,
             deltaind = Industria2008-Industria1991,
             deltass = Servicios2008-Servicios1991) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(1991, 2008)) %>% 
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
      mutate(deltapibpc = yp2008-yp1991) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  )


mod3 <- lm(log(deltaproy) ~ 
             log(y1991+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
           data = set3)


tablaout <- generate_regression_table(list(mod1, mod2, mod3), 
                                      se_type = "HC4")

write.csv2(tablaout, "resultadosreganexo1.csv", 
           row.names = F)


# ========= 2009 - 2018

set1 <- cortes %>% 
  select(regionstd, y2009, y2018) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2018+1)/(y2009+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(2009, 2018)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2018-Agrario2009,
             deltamin = Minería2018-Minería2009,
             deltaind = Industria2018-Industria2009,
             deltass = Servicios2018-Servicios2009) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(2009, 2018)) %>% 
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
      mutate(deltapibpc = yp2018-yp2009) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  ) 


mod1 <- lm(log(deltaproy) ~ 
             log(y2009+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
           data = set1)


set2 <- cortes2 %>% 
  select(regionstd, y2009, y2018) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2018+1)/(y2009+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(2009, 2018)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2018-Agrario2009,
             deltamin = Minería2018-Minería2009,
             deltaind = Industria2018-Industria2009,
             deltass = Servicios2018-Servicios2009) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(2009, 2018)) %>% 
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
      mutate(deltapibpc = yp2018-yp2009) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  ) 


mod2 <- lm(log(deltaproy) ~ 
             log(y2009+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
           data = set2)


set3 <- cortes3 %>% 
  select(regionstd, y2009, y2018) %>% 
  mutate(regionord = regordinal(regionstd),
         deltaproy = (y2018+1)/(y2009+1)) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(2009, 2018)) %>% 
      group_by(anio, region, sector3) %>% 
      summarise(totpibSEC3 = sum(pib)) %>% 
      group_by(anio, sector3) %>% 
      mutate(p_SEC3 = totpibSEC3/sum(totpibSEC3)) %>% 
      as.data.frame() %>% 
      mutate(secanio = paste0(sector3, anio)) %>% 
      select(region, secanio, p_SEC3) %>% 
      spread(secanio, p_SEC3) %>% 
      mutate(deltagro = Agrario2018-Agrario2009,
             deltamin = Minería2018-Minería2009,
             deltaind = Industria2018-Industria2009,
             deltass = Servicios2018-Servicios2009) %>% 
      select(region, deltagro, deltamin, deltaind, deltass),
    by = c("regionstd"="region")
  ) %>% 
  left_join(
    read.csv2("data procesada/pibs y pobla final.csv", 
              encoding = "latin1") %>% 
      filter(origen == "Banco Central",
             anio %in% c(2009, 2018)) %>% 
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
      mutate(deltapibpc = yp2018-yp2009) %>% 
      select(region, deltapibpc),
    by = c("regionstd"="region")
  ) 



mod3 <- lm(log(deltaproy) ~ 
             log(y2009+1) +
             regionord +
             # deltapibpc +
             deltagro+
             deltamin+ 
             deltaind+
             deltass,
           data = set3)


tablaout <- generate_regression_table(list(mod1, mod2, mod3), 
                                      se_type = "HC4")



write.csv2(tablaout, "resultadosreganexo2.csv", 
           row.names = F)







