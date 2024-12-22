##%######################################################%##
#                                                          #
####          Gráficos Capítulo PGD 2025-2027           ####
#                                                          #
##%######################################################%##

# Librerías  ----
library(tidyverse)
library(ggrepel)
library(scales)

# Figura 3 ----

Fig3 <- UnalData::Aspirantes %>% 
        mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
        summarise(Total = n(), .by = c(Periodo)) %>% 
        mutate(Grupo = "General",
               Totaf = ifelse(Periodo %in% c("2008-1", "2012-1", "2019-1", "2022-2", "2024-1", "2024-2"),  
                              format(Total, big.mark = ".", decimal.mark = ","), 
                              NA)) %>% 
        ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
        geom_point(size = 1)+
        geom_line()+
        geom_label_repel(aes(label = Totaf), 
                        box.padding = 0.5,
                        segment.linetype = 3,
                        size = 2.8)+
  labs(title = "Evolución total de aspirantes a la UNAL por peridos académicos",
       x = "\nPeriodo",
       y = "Total de aspirantes\n")+
  scale_y_continuous(limits = c(0,100000))+
  theme(axis.text.x = element_text(angle = 90))

Fig3

# Figura 4 ----

Fig4 <- UnalData::Aspirantes %>% filter(YEAR == 2024, SEMESTRE == 2) %>%
  summarise(Total = n(), .by = c(INS_SEDE_NOMBRE)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         INS_SEDE_NOMBRE = fct_rev(fct_reorder(INS_SEDE_NOMBRE,Total,sum))) %>% 
  rename(Sede = INS_SEDE_NOMBRE) %>% 
  ggplot(aes(x = Sede, y = Total))+
    geom_bar(position = "identity",
             stat = "identity", 
             fill = "gray45")+
  geom_text(aes(label = Porcentaje), vjust = -0.5, size = 3)+
  scale_y_continuous(limits = c(0,30000))+
  labs(title = "Participación total de aspirantes a la UNAL por sedes",
       subtitle = "Periodo 2024-2",
       x = "\nSede de la Universidad",
       y = "Total de aspirantes\n")
Fig4

# Figura 5 ----

Fig5 <- UnalData::Aspirantes %>% 
  filter(ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2008-1", "2013-2", "2018-1", "2023-2", "2024-1", "2024-2"),  
                        format(Total, big.mark = ".", decimal.mark = ","), 
                        NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 0.5,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de admitidos a la UNAL por peridos académicos",
       x = "\nPeriodo",
       y = "Total de admitidos\n")+
  scale_y_continuous(limits = c(0,12000))+
  theme(axis.text.x = element_text(angle = 90))

Fig5

# Figura 6 ----

Fig6 <- UnalData::Aspirantes %>% 
  filter(ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, MOD_INS)) %>% 
  group_by(Periodo) %>% 
  mutate(Participación = Total/sum(Total),
         Porcentaje = scales::percent(Participación, accuracy = 0.1)) %>% 
  mutate(Porcentaje = ifelse(Periodo %in% c("2008-1", "2017-1", "2024-1"),  
                 format(Porcentaje, big.mark = ".", decimal.mark = ","), 
                 NA)) %>% 
  ggplot(aes(x = Periodo, y = Participación, group = MOD_INS))+
  geom_line(aes(linetype = MOD_INS))+
  geom_text_repel(aes(label = Porcentaje),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  labs(title = "Evolución porcentaje de admitidos a la UNAL por modalidad de admisión",
       x = "\nPeriodo",
       y = "Porcentaje",
       linetype = "Modalidad de admisión")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
Fig6

# Figura 7 ----

Fig7 <- UnalData::Matriculados %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Periodo = ifelse(Periodo == "2020-1", "2020-1**", Periodo),
         Totaf = ifelse(Periodo %in% c("2009-1", "2020-1**", "2021-1", "2024-1"),  
                        format(Total, big.mark = ".", decimal.mark = ","), 
                        NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 1,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de estudiantes matriculados en la UNAL por peridos académicos",
       x = "\nPeriodo",
       y = "Total de matriculados\n",
       caption = "(**): Por anormalidad académica, no incluye los matriculados\nregulares de la Sede Medellín")+
  scale_y_continuous(limits = c(0,70000))+
  theme(axis.text.x = element_text(angle = 90))

Fig7

# Figura 8 ----

Fig8 <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1) %>%
  summarise(Total = n(), .by = c(SEDE_NOMBRE_MAT)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         SEDE_NOMBRE_MAT = fct_rev(fct_reorder(SEDE_NOMBRE_MAT, Total, sum))) %>% 
  rename(Sede = SEDE_NOMBRE_MAT) %>% 
  ggplot(aes(x = Sede, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45")+
  geom_text(aes(label = Porcentaje), vjust = -0.5, size = 3)+
  scale_y_continuous(limits = c(0,35000))+
  labs(title = "Participación total de estudiantes matriculados en la UNAL por sedes",
       subtitle = "Periodo 2024-1",
       x = "\nSede de la Universidad",
       y = "Total matriculados\n")
Fig8

# Figura 9 ----

Fig9 <- UnalData::Matriculados %>% 
  filter(TIPO_NIVEL == "Pregrado") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
    summarise(Total = n(), .by = c(Periodo, ESTRATO_ORIG)) %>%
  mutate(Periodo = ifelse(Periodo == "2020-1", "2020-1**", Periodo),
         Total = ifelse(Periodo == "2011-2", NA, Total),
         Etiqueta = ifelse(Periodo %in% c("2009-1", "2024-1") & 
                           ESTRATO_ORIG %in% c("Estrato 1", "Estrato 2", "Estrato 3", "Estrato 4"),
                           Total, NA)) %>% 
  rename(Estrato = ESTRATO_ORIG) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Estrato))+
  geom_line(aes(linetype = Estrato))+
  geom_text_repel(aes(label = Etiqueta),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  geom_point(aes(shape = Estrato),
             size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución Total estudiantes matriculados en la UNAL por estrato",
       x = "\nPeriodo",
       y = "Total matriculados",
       linetype = "Estrato",
       caption = "(**): Por anormalidad académica, no incluye los matriculados\nregulares de la Sede Medellín")+
  theme(axis.text.x = element_text(angle = 90))

Fig9
