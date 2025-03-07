##%######################################################%##
#                                                          #
####          Gráficos Capítulo PGD 2025-2027           ####
#                                                          #
##%######################################################%##

# Librerías  ----

library(tidyverse)
library(UnalData)
library(ggrepel)
library(scales)
library(googledrive)
library(readxl)


# Importar bases ----

# Info de Rankigs
drive_download("Rankings/Rankings.xlsx", overwrite = TRUE)
Rankings <- read_excel("Rankings.xlsx")
unlink(c("Rankings.xlsx"))

# Info de programas
Programas <- read_excel("Fuentes/Programas.xlsx")


# Info Investigación
Grupos <- read_excel("Fuentes/Investigación.xlsx", sheet = "Grupos")
Investigadores <- read_excel("Fuentes/Investigación.xlsx", sheet = "Investigadores")   
Extensión <- read_excel("Fuentes/Investigación.xlsx", sheet = "Extensión")
   
# Info Deserción
Desersión <- read_excel("Fuentes/Desersión.xlsx", sheet = "Pregrado")


# Info Bienestar 
Bienestar <- read_excel("Fuentes/BienestarG.xlsx", sheet = "Hoja2") # General
Económica <- read_excel("Fuentes/Bienestar.xlsx", sheet = "GYFSE") # GYFS

# Gráfico 5 ----

Gra5 <- Programas %>% 
        mutate(Grupo = "General",
         Totaf = ifelse(Year %in% c(1994, 2000, 2010, 2020, 2024), Total, NA),
         Year = as.character(Year)) %>% 
  ggplot(aes(x = Year, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 0.5,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de programas académicos en la UNAL",
       subtitle = "Periodo 1994-2024",
       x = "\nAño",
       y = "Total de programas\n")+
  scale_y_continuous(limits = c(0,500))+
  theme(axis.text.x = element_text(angle = 90))

Gra5

ggsave("Exportar/SVG/Grafico5.svg")

# Gráfico 6 ----

Gra6 <- Programas %>% 
        pivot_longer(cols = c(Pregrado, Posgrado),
                     names_to = "Nivel",
                     values_to = "Global") %>% 
        mutate(Totaf = ifelse(Year %in% c(1994, 2000, 2010, 2020, 2024), Global, NA),
          Year = as.character(Year)) %>% 
  ggplot(aes(x = Year, y = Global, group = Nivel))+
  geom_line(aes(linetype = Nivel))+
  geom_text_repel(aes(label = Totaf),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  labs(title = "Evolución total de programas académicos en la UNAL por nivel de formación",
       subtitle = "Periodo 1994-2024", 
       x = "\nAño",
       y = "Total de programas\n",
       linetype = "Nivel de formación")+
  scale_y_continuous(limits = c(0, 400))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra6

ggsave("Exportar/SVG/Grafico6.svg")

# Gráfico 7 ----

Gra7 <- UnalData::Aspirantes %>% 
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
  labs(title = "Evolución total de aspirantes a la UNAL por periodos académicos",
       x = "\nPeriodo",
       y = "Total de aspirantes\n")+
  scale_y_continuous(limits = c(0,100000))+
  theme(axis.text.x = element_text(angle = 90))

Gra7
ggsave("Exportar/SVG/Grafico7.svg")

# Gráfico 8 ----

Gra8 <- UnalData::Aspirantes %>% filter(YEAR == 2024, SEMESTRE == 2) %>%
  summarise(Total = n(), .by = c(INS_SEDE_NOMBRE)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         INS_SEDE_NOMBRE = fct_rev(fct_reorder(INS_SEDE_NOMBRE,Total,sum))) %>% 
  rename(Sede = INS_SEDE_NOMBRE) %>% 
  ggplot(aes(x = Sede, y = Total))+
    geom_bar(position = "identity",
             stat = "identity", 
             fill = "gray45",
             width = 0.7)+
  geom_text(aes(label = Porcentaje), vjust = -0.5, size = 3)+
  scale_y_continuous(limits = c(0,30000))+
  labs(title = "Participación total de aspirantes a la UNAL por sedes",
       subtitle = "Periodo 2024-2",
       x = "\nSede de la Universidad",
       y = "Total de aspirantes\n")
Gra8
ggsave("Exportar/SVG/Grafico8.svg")

# Gráfico 9 ----

Gra9 <- UnalData::Aspirantes %>% 
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
  labs(title = "Evolución total de admitidos a la UNAL por periodos académicos",
       x = "\nPeriodo",
       y = "Total de admitidos\n")+
  scale_y_continuous(limits = c(0,12000))+
  theme(axis.text.x = element_text(angle = 90))

Gra9
ggsave("Exportar/SVG/Grafico9.svg")

# Gráfico 10 ----

Gra10 <- UnalData::Aspirantes %>% 
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

Gra10
ggsave("Exportar/SVG/Grafico10.svg")

# Gráfico 11 ----

Gra11 <- UnalData::Matriculados %>% 
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
  labs(title = "Evolución total de estudiantes matriculados en la UNAL por periodos académicos",
       x = "\nPeriodo",
       y = "Total de matriculados\n",
       caption = "(**): Por anormalidad académica, no incluye los matriculados\nregulares de la Sede Medellín")+
  scale_y_continuous(limits = c(0,70000))+
  theme(axis.text.x = element_text(angle = 90))

Gra11
ggsave("Exportar/SVG/Grafico11.svg")

# Gráfico 12 ----

Gra12 <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1) %>%
  summarise(Total = n(), .by = c(SEDE_NOMBRE_MAT)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         SEDE_NOMBRE_MAT = fct_rev(fct_reorder(SEDE_NOMBRE_MAT, Total, sum))) %>% 
  rename(Sede = SEDE_NOMBRE_MAT) %>% 
  ggplot(aes(x = Sede, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45",
           width = 0.7)+
  geom_text(aes(label = Porcentaje), vjust = -0.5, size = 3)+
  scale_y_continuous(limits = c(0,35000))+
  labs(title = "Participación total de estudiantes matriculados en la UNAL por sedes",
       subtitle = "Periodo 2024-1",
       x = "\nSede de la Universidad",
       y = "Total matriculados\n")

Gra12
ggsave("Exportar/SVG/Grafico12.svg")

# Gráfico 13 ----

Gra13 <- UnalData::Matriculados %>% 
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
  labs(title = "Evolución Total estudiantes matriculados en pregrado en la UNAL por estrato",
       x = "\nPeriodo",
       y = "Total matriculados",
       linetype = "Estrato",
       caption = "(**): Por anormalidad académica, no incluye los matriculados\nregulares de la Sede Medellín")+
  theme(axis.text.x = element_text(angle = 90))

Gra13
ggsave("Exportar/SVG/Grafico13.svg")

# Gráfico 14 ----

Gra14 <- UnalData::Graduados %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2009-1", "2019-1", "2019-2", "2021-2", "2024-1"),  
                        format(Total, big.mark = ".", decimal.mark = ","), 
                        NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 1,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de estudiantes graduados en la UNAL por periodos académicos",
       x = "\nPeriodo",
       y = "Total graduados\n")+
  scale_y_continuous(limits = c(0,8000))+
  theme(axis.text.x = element_text(angle = 90))

Gra14
ggsave("Exportar/SVG/Grafico14.svg")

# Gráfico 15 ----

Gra15 <- UnalData::Graduados %>% filter(YEAR == 2023 | (YEAR == 2024 & SEMESTRE == 1)) %>%
  summarise(Total = n(), .by = c(SEDE_NOMBRE_ADM)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         SEDE_NOMBRE_ADM = fct_rev(fct_reorder(SEDE_NOMBRE_ADM,Total,sum))) %>% 
  rename(Sede = SEDE_NOMBRE_ADM) %>% 
  ggplot(aes(x = Sede, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45",
           width = 0.7)+
  geom_text(aes(label = Porcentaje), vjust = -0.5, size = 3)+
  scale_y_continuous(limits = c(0,9000))+
  labs(title = "Participación total de graduados en la UNAL por sedes",
       subtitle = "Año 2023 y periodo 2024-1",
       x = "\nSede de la Universidad",
       y = "Total de graduados\n")

Gra15
ggsave("Exportar/SVG/Grafico15.svg")


# Gráfico 16 ----

Gra16 <- UnalData::Graduados %>% filter(YEAR == 2023 | (YEAR == 2024 & SEMESTRE == 1), TIPO_NIVEL == "Pregrado") %>%
  summarise(Total = n(), .by = c(ESTRATO_ORIG)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1)) %>% 
  rename(Estrato = ESTRATO_ORIG) %>% 
  ggplot(aes(x = Estrato, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45",
           width = 0.7)+
  geom_text(aes(label = Porcentaje), vjust = -0.5, size = 3)+
  scale_y_continuous(limits = c(0, 4000))+
  labs(title = "Participación total de graduados en pregrado en la UNAL por estratos",
       subtitle = "Año 2023 y periodo 2024-1",
       x = "\nEstrato",
       y = "Total de graduados\n")

Gra16
ggsave("Exportar/SVG/Grafico16.svg")


# Gráfico 17 ----

Gra17 <- UnalData::Docentes %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2008-2", "2019-1", "2024-2"),  
                        format(Total, big.mark = ".", decimal.mark = ","), 
                        NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 1,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de docentes de carrera en la UNAL por periodos académicos",
       x = "\nPeriodo",
       y = "Total docentes\n")+
  scale_y_continuous(limits = c(0, 4000))+
  theme(axis.text.x = element_text(angle = 90))

Gra17
ggsave("Exportar/SVG/Grafico17.svg")

# Gráfico 18 ----

Gra18 <- UnalData::Docentes %>% filter(YEAR == 2024, SEMESTRE == 2) %>%
  summarise(Total = n(), .by = c(FORMACION)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         FORMACION = fct_rev(fct_reorder(FORMACION,Total,sum))) %>% 
  rename(`Máxima Formación` = FORMACION) %>% 
  ggplot(aes(x = `Máxima Formación`, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45",
           width = 0.7)+
  geom_text(aes(label = Porcentaje), vjust = -0.5, size = 3)+
  scale_y_continuous(limits = c(0, 2000))+
  labs(title = "Participación máximo nivel de formación de los docentes de carrera de la UNAL",
       subtitle = "Periodo 2024-2",
       x = "\nMáxima Formación",
       y = "Total de docentes\n")

Gra18
ggsave("Exportar/SVG/Grafico18.svg")


# Gráfico 19 ----

Gra19 <- UnalData::Administrativos %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2008-2", "2019-1", "2024-2"),  
                        format(Total, big.mark = ".", decimal.mark = ","), 
                        NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 1,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de funcionarios de carrera en la UNAL por periodos académicos",
       x = "\nPeriodo",
       y = "Total funcionarios\n")+
  scale_y_continuous(limits = c(0, 4000))+
  theme(axis.text.x = element_text(angle = 90))

Gra19
ggsave("Exportar/SVG/Grafico19.svg")

# Gráfico 20 ----

Gra20 <- UnalData::Administrativos %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, SEXO)) %>% 
  group_by(Periodo) %>% 
  mutate(Participación = Total/sum(Total),
         Porcentaje = scales::percent(Participación, accuracy = 0.1)) %>% 
  mutate(Porcentaje = ifelse(Periodo %in% c("2008-2", "2024-2"),  
                             format(Porcentaje, big.mark = ".", decimal.mark = ","), 
                             NA)) %>% 
  ggplot(aes(x = Periodo, y = Participación, group = SEXO))+
  geom_line(aes(linetype = SEXO))+
  geom_text_repel(aes(label = Porcentaje),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
    labs(title = "Evolución porcentaje de administrativos de carrera en la UNAL por sexo biológico",
       x = "\nPeriodo",
       y = "Porcentaje",
       linetype = "Sexo")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra20
ggsave("Exportar/SVG/Grafico20.svg")


# Gráfico 21 ----


Gra21 <- UnalData::SaberPro %>% 
         select(starts_with("PUNT")) %>% 
         select(-PUNTAJE_GLOBAL) %>% 
         rename(`Competencias Ciudadanas` = PUNT_COMP_CIUD,
                `Comunicación Escrita` = PUNT_COMU_ESCR,
                `Inglés` = PUNT_INGLES,
                `Lectura Crítica` = PUNT_LECT_CRIT,
                `Razonamiento Cuantitativo` = PUNT_RAZO_CUANT) %>% 
         pivot_longer(cols = c(`Competencias Ciudadanas`:`Razonamiento Cuantitativo`),
                      names_to = "Prueba",
                      values_to = "Puntaje") %>% 
         na.omit() %>% 
         ggplot(aes(x = reorder(Prueba, Puntaje, FUN = mean), y = Puntaje))+
         geom_boxplot(outlier.colour = "gray70",
                      outlier.shape = 21,
                      width = 0.6)+
       scale_y_continuous(limits = c(0, 300), breaks = seq(0,300,50))+
        coord_flip()+
  labs(title = "Distribución resultados de los estudiantes de la UNAL en las competencias de la\nPrueba Saber Pro",
       subtitle = "Periodo 2016-2022",
       x = "Competencia evaluada\n",
       y = "\nPuntaje en la prueba")
 
Gra21
ggsave("Exportar/SVG/Grafico21.svg")


# Gráfico 22 ----

Gra22 <- UnalData::SaberPro %>% 
  select(starts_with("PUNT"), SEDE_NOMBRE_ADM) %>% 
  select(-PUNTAJE_GLOBAL) %>% 
  rename(`Competencias Ciudadanas` = PUNT_COMP_CIUD,
         `Comunicación Escrita` = PUNT_COMU_ESCR,
         `Inglés` = PUNT_INGLES,
         `Lectura Crítica` = PUNT_LECT_CRIT,
         `Razonamiento Cuantitativo` = PUNT_RAZO_CUANT,
         `Sede de admisión` = SEDE_NOMBRE_ADM) %>% 
  pivot_longer(cols = c(`Competencias Ciudadanas`:`Razonamiento Cuantitativo`),
               names_to = "Prueba",
               values_to = "Puntaje") %>% 
  na.omit() %>% 
  ggplot(aes(x = reorder(Prueba, Puntaje,  FUN = mean), y = Puntaje, fill = `Sede de admisión`))+
  geom_boxplot(outliers = FALSE, width = 0.6)+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0,300,50))+
  scale_fill_grey(start = 0.1, end = 1)+
  coord_flip()+
  labs(title = "Distribución resultados de los estudiantes de la UNAL en las competencias de la\nPrueba Saber Pro por sedes de admisión",
       subtitle = "Periodo 2016-2022",
       x = "Competencia evaluada\n",
       y = "\nPuntaje en la prueba")

Gra22
ggsave("Exportar/SVG/Grafico22.svg")


# Gráfico 23 ----

Gra23 <- UnalData::SaberPro %>% 
  select(starts_with("PUNT"), TIPO_ADM) %>% 
  select(-PUNTAJE_GLOBAL) %>% 
  rename(`Competencias Ciudadanas` = PUNT_COMP_CIUD,
         `Comunicación Escrita` = PUNT_COMU_ESCR,
         `Inglés` = PUNT_INGLES,
         `Lectura Crítica` = PUNT_LECT_CRIT,
         `Razonamiento Cuantitativo` = PUNT_RAZO_CUANT,
         `Modalidad de admisión` = TIPO_ADM) %>% 
  pivot_longer(cols = c(`Competencias Ciudadanas`:`Razonamiento Cuantitativo`),
               names_to = "Prueba",
               values_to = "Puntaje") %>%
  na.omit() %>%
  ggplot(aes(x = reorder(Prueba, Puntaje,  FUN = median), y = Puntaje, fill = `Modalidad de admisión`))+
  geom_boxplot(outliers = FALSE, width = 0.6)+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0,300,50))+
  scale_fill_grey(start = 0.3, end = 0.9)+
  coord_flip()+
  labs(title = "Distribución resultados de los estudiantes de la UNAL en las competencias de la\nPrueba Saber Pro según modalidades de admisión",
       subtitle = "Periodo 2016-2022",
       x = "Competencia evaluada\n",
       y = "\nPuntaje en la prueba")

Gra23
ggsave("Exportar/SVG/Grafico23.svg")

# Gráfico 24 ----

Gra24 <- Grupos %>%
         mutate(Sede = fct_relevel(Sede, "Bogotá", "Medellín", "Manizales", "Palmira", "La Paz", "Amazonía", 
                                   "Caribe", "Orinoquía", "Tumaco")) %>% 
         ggplot(aes(x = Sede, y = Total, fill = Categoria))+
         geom_bar(position = "dodge",
                  stat = "identity")+
         geom_text(aes(label = Total),
                   position = position_dodge(width = 0.9),
                   vjust = -0.5,
                   size = 3)+
  scale_y_continuous(limits = c(0, 180))+
  scale_fill_manual(values = c("#252525", "#525252", "#737373", "#969696","#D9D9D9"))+
   labs(title = "Grupos de investigación de la UNAL categorizados en Minciencias por sedes",
       subtitle = "Año 2023",
       x = "\nSede",
       y = "Total grupos\n",
       fill = "Categoría del grupo")+
    theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra24  
ggsave("Exportar/SVG/Grafico24.svg")

# Gráfico 25 ----

Gra25 <- Investigadores %>%
  mutate(Sede = fct_relevel(Sede, "Bogotá", "Medellín", "Manizales", "Palmira", "La Paz", "Amazonía", 
                            "Caribe", "Orinoquía", "Tumaco")) %>% 
  ggplot(aes(x = Sede, y = Total, fill = Tipo))+
  geom_bar(position = "dodge",
           stat = "identity")+
  geom_text(aes(label = Total),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 3)+
  scale_y_continuous(limits = c(0, 1500))+
  scale_fill_manual(values = c("#636363", "#CCCCCC"))+
  labs(title = "Total investigadores de la UNAL por sedes",
       subtitle = "Año 2023",
       x = "\nSede",
       y = "Total investigadores\n",
       fill = "Tipo docente")+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra25  
ggsave("Exportar/SVG/Grafico25.svg")

# Gráfico 26 ----

Gra26 <- Extensión %>%
  mutate(Modalidad = fct_rev(fct_relevel(Modalidad, "Educación continua y permanente",
                                    "Servicios académicos",
                                      "Extensión solidaria")),
         Año = as.character(Año),
         ) %>% 
  ggplot(aes(x = Año, y = Total, fill = Modalidad))+
  geom_bar(position = "fill",
           stat = "identity")+
  geom_text(aes(label = Total),
            position = position_fill(vjust = 0.5),
            size = 3)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_grey(start = 0.3, end = 0.7)+
  labs(title = "Evolución de las actividades, proyectos, programas y planes de extensión en la UNAL",
       subtitle = "Periodo 2014-2023",
       x = "\nAño",
       y = "Porcentaje\n",
       fill = "Modalidad")+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra26 
ggsave("Exportar/SVG/Grafico26.svg")


# Gráfico 27 ----

Gra27 <- Rankings %>% filter(RANKING == "QSMundo") %>% 
  mutate(YEAR = as.character(YEAR),
         Totalf = ifelse(YEAR %in% c("2011", "2019", "2023"),  
                 format(Total, big.mark = ".", decimal.mark = ","), 
                 NA)) %>% 
  ggplot(aes(x = YEAR, y = Total, group = Clase))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totalf), 
                   box.padding = 0.5,
                   segment.linetype = 3,
                   size = 2.8) +
  scale_y_continuous(limits = c(500, 0),  trans = "reverse")+
  labs(title = "Evolución posiciones de la UNAL en QS World University Rankings",
       subtitle = "Periodo 2011-2023", 
       x = "\nAño",
       y = "Puesto Mundo\n")

Gra27
ggsave("Exportar/SVG/Grafico27.svg")

# Gráfico 28 ----

Gra28 <- Rankings %>% filter(RANKING == "THEMundo") %>% 
  mutate(YEAR = as.character(YEAR),
         Totalf = ifelse(YEAR %in% c("2017", "2020", "2023"),  
                         format(Total, big.mark = ".", decimal.mark = ","), 
                         NA)) %>% 
  ggplot(aes(x = YEAR, y = Total, group = Clase))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totalf), 
                   box.padding = 0.5,
                   segment.linetype = 3,
                   size = 2.8) +
  scale_y_continuous(limits = c(1300, 0),  trans = "reverse")+
  labs(title = "Evolución posiciones de la UNAL en Times Higher Education (THE)\nWorld University Rankings",
       subtitle = "Periodo 2017-2023", 
       x = "\nAño",
       y = "Puesto Mundo\n")

Gra28
ggsave("Exportar/SVG/Grafico28.svg")


# Gráfico 29 ----

Gra29 <- Rankings %>% filter(RANKING == "USapiens") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-"),
         Totalf = ifelse(Periodo %in% c("2011-1", "2017-2", "2023-2"),  
                         format(Total, big.mark = ".", decimal.mark = ","), 
                         NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Clase))+
  geom_line(aes(linetype = Clase))+
  geom_label_repel(aes(label = Totalf), 
                   box.padding = 0.5,
                   segment.linetype = 3,
                   size = 2.8) +
  geom_point(aes(shape = Clase),
             size = 1.3,
             alpha = 0.5)+
  scale_y_continuous(limits = c(60, 0),  trans = "reverse")+
  labs(title = "Evolución posiciones de las sedes de la UNAL en el Ranking U-Sapiens*",
       subtitle = "Periodo 2011-2023", 
       x = "\nPeriodo",
       y = "Puesto en Colombia\n",
       linetype = "Sede",
       shape = "Sede",
       caption = "(*). En el ranking U-Sapiens sólo cumplen criterios de inclusión\nlas sedes Bogotá, Medellín y Palmira") +
  theme(axis.text.x = element_text(angle = 90))

Gra29
ggsave("Exportar/SVG/Grafico29.svg")

# Gráfico 30 ----
# Salud

Gra30 <- Bienestar %>% 
  filter(Programa == "Salud") %>% 
  mutate(Usuariosf = ifelse(Year %in% c(2017, 2021, 2023),
                            format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>%
  ggplot(aes(x = Year, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                   box.padding = 1,
                   segment.linetype = 6,
                   size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total beneficiarios Área de Salud",
       subtitle = "Periodo 2017-2023",
       x = "\nAño",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 40000))+
  scale_x_continuous(breaks = c(2017:2023))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra30
ggsave("Exportar/SVG/Grafico30.svg")

# Gráfico 31 ----
# Deportes

Gra31 <- Bienestar %>% 
  filter(Programa == "Actividad Física y Deporte") %>% 
  mutate(Usuariosf = ifelse(Year %in% c(2017, 2020, 2021, 2023),
                            format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>%
  ggplot(aes(x = Year, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                   box.padding = 1,
                   segment.linetype = 6,
                   size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total beneficiarios Área Actividad Física y Deporte",
       subtitle = "Periodo 2017-2023",
       x = "\nAño",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 30000))+
  scale_x_continuous(breaks = c(2017:2023))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra31
ggsave("Exportar/SVG/Grafico31.svg")

# Gráfico 32 ----
# Cultura

Gra32 <- Bienestar %>% 
  filter(Programa == "Cultura") %>% 
  mutate(Usuariosf = ifelse(Year %in% c(2017, 2020, 2021, 2023),
                            format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>%
  ggplot(aes(x = Year, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                   box.padding = 1,
                   segment.linetype = 6,
                   size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total beneficiarios Área de Cultura",
       subtitle = "Periodo 2017-2023",
       x = "\nAño",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 25000))+
  scale_x_continuous(breaks = c(2017:2023))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra32
ggsave("Exportar/SVG/Grafico32.svg")

# Gráfico 33 ----
# Acompañamiento integral

Gra33 <- Bienestar %>% 
  filter(Programa == "Acompañamiento Integral") %>% 
  mutate(Usuariosf = ifelse(Year %in% c(2017, 2020, 2021, 2023),
                            format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>%
  ggplot(aes(x = Year, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                   box.padding = 1,
                   segment.linetype = 6,
                   size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total beneficiarios Área Acompañamiento Integral",
       subtitle = "Periodo 2017-2023",
       x = "\nAño",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 40000))+
  scale_x_continuous(breaks = c(2017:2023))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra33
ggsave("Exportar/SVG/Grafico33.svg")


# Gráfico 34 ----
# Apoyo alimentario

Gra34 <- Económica %>%  mutate(Periodo = paste0(str_sub(Periodof, 1, 5), 
                                                 str_sub(Periodof, 7, 7))) %>% 
         filter(Programa == "Apoyo alimentario estudiantil") %>% 
         mutate(Usuariosf = ifelse(Periodo %in% c("2017-1", "2019-2", "2020-1", "2022-1", "2023-2"),  
                 format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>% 
  ggplot(aes(x = Periodo, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total estudiantes beneficiarios del programa de Apoyo Alimentario Estudiantil",
       subtitle = "Periodo 2017-2023",
       x = "\nPeriodo",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 7000))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra34
ggsave("Exportar/SVG/Grafico34.svg")

# Gráfico 35 ----
# Apoyo económico

Gra35 <- Económica %>%  mutate(Periodo = paste0(str_sub(Periodof, 1, 5), 
                                                 str_sub(Periodof, 7, 7))) %>% 
  filter(Programa == "Apoyo económico estudiantil") %>% 
  mutate(Usuariosf = ifelse(Periodo %in% c("2017-1", "2019-2", "2020-1", "2022-1", "2023-2"),  
                            format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>% 
  ggplot(aes(x = Periodo, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                   box.padding = 1,
                   segment.linetype = 6,
                   size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total estudiantes beneficiarios del programa de Apoyo Económico Estudiantil",
       subtitle = "Periodo 2017-2023",
       x = "\nPeriodo",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 3000))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra35
ggsave("Exportar/SVG/Grafico35.svg")


# Gráfico 36 ----
# Alojamiento

Gra36 <- Económica %>%  mutate(Periodo = paste0(str_sub(Periodof, 1, 5), 
                                                 str_sub(Periodof, 7, 7))) %>% 
  filter(Programa == "Apoyo para el alojamiento estudiantil") %>% 
  mutate(Usuariosf = ifelse(Periodo %in% c("2017-1", "2019-2", "2020-2", "2022-1", "2023-2"),  
                            format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>% 
  ggplot(aes(x = Periodo, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                   box.padding = 1,
                   segment.linetype = 6,
                   size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total estudiantes beneficiarios del programa de Apoyo Para el Alojamiento Estudiantil",
       subtitle = "Periodo 2017-2023",
       x = "\nPeriodo",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 1500))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra36
ggsave("Exportar/SVG/Grafico36.svg")

# Gráfico 37 ----
# Alojamiento

Gra37 <- Económica %>%  mutate(Periodo = paste0(str_sub(Periodof, 1, 5), 
                                                 str_sub(Periodof, 7, 7))) %>% 
  filter(Programa == "Apoyo para el transporte estudiantil") %>% 
  mutate(Usuariosf = ifelse(Periodo %in% c("2017-1", "2019-2", "2020-2", "2022-1", "2023-2"),  
                            format(Usuarios, big.mark = ".", decimal.mark = ","), NA)) %>% 
  ggplot(aes(x = Periodo, y = Usuarios, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_label_repel(aes(label = Usuariosf),
                   box.padding = 1,
                   segment.linetype = 6,
                   size = 2.8)+
  geom_point(size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución total estudiantes beneficiarios del programa de Apoyo para el Transporte Estudiantil",
       subtitle = "Periodo 2017-2023",
       x = "\nPeriodo",
       y = "Total beneficiarios\n") +
  scale_y_continuous(limits = c(0, 6500))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position="none")

Gra37
ggsave("Exportar/SVG/Grafico37.svg")


# Gráfico 38 ----

Gra38 <- Desersión %>% mutate(Año = as.numeric(str_sub(APERTURA, 1, 4)),
                              Periodo = str_sub(APERTURA, 1, 6)) %>% 
  summarise(Total = sum(Total), 
            Deserción = sum(Deserción), 
            .by = c(Año, Periodo)) %>% 
  mutate(Participa = Deserción/Total,
         Porcentaje = ifelse(Periodo %in% c("2007-1", "2010-1", "2013-2", "2015-2"), 
                             scales::percent(Participa, accuracy = 0.1), NA),
         Grupo = "Global") %>% 
  filter(Año  <= 2015) %>% 
  ggplot(aes(x = Periodo, y = Participa, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Porcentaje), 
                   box.padding = 0.5,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Tasa de deserción en Pregrado en la UNAL por cohortes",
       subtitle = "Cohortes 2007-2015",
       x = "\nCohorte",
       y = "Tasa de deserción \n")+
  scale_y_continuous(labels = percent,
                     limits = c(0,1))+
  theme(axis.text.x = element_text(angle = 90))

Gra38
ggsave("Exportar/SVG/Grafico38.svg")

# Gráfico 39 ----

Gra39 <- Desersión %>% mutate(Año = as.numeric(str_sub(APERTURA, 1, 4)),
                              Periodo = str_sub(APERTURA, 1, 6)) %>% 
  summarise(Total = sum(Total), 
            Deserción = sum(Deserción), 
            .by = c(Año, Periodo, Sede)) %>% 
  mutate(Participa = Deserción/Total,
         Porcentaje = ifelse(Periodo %in% c("2007-1", "2011-2", "2015-2"), 
                             scales::percent(Participa, accuracy = 0.1), NA)) %>% 
  filter(Año  <= 2015) %>% 
  ggplot(aes(x = Periodo, y = Participa, group = Sede))+
  geom_line(aes(linetype = Sede))+
  geom_label_repel(aes(label = Porcentaje),
                   box.padding = 1,
                   segment.linetype = 3,
                   size = 2.8)+
  geom_point(aes(shape = Sede),
             size = 1.5,
             alpha = 0.7)+
  labs(title = "Tasa de deserción en Pregrado en la UNAL por cohortes y sedes",
       subtitle = "Cohortes 2007-2015",
       x = "\nCohorte",
       y = "Tasa de deserción \n")+
  scale_y_continuous(labels = percent,
                     limits = c(0,0.7))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra39
ggsave("Exportar/SVG/Grafico39.svg")


# Gráfico 40 ----

Gra40 <- UnalData::Aspirantes %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, SEXO)) %>% 
  group_by(Periodo) %>% 
  mutate(Participación = Total/sum(Total),
         Porcentaje = scales::percent(Participación, accuracy = 0.1)) %>% 
  mutate(Porcentaje = ifelse(Periodo %in% c("2008-1", "2024-2"),  
                             format(Porcentaje, big.mark = ".", decimal.mark = ","), 
                             NA)) %>% 
  ggplot(aes(x = Periodo, y = Participación, group = SEXO))+
  geom_line(aes(linetype = SEXO))+
  geom_text_repel(aes(label = Porcentaje),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  labs(title = "Evolución porcentaje de aspirantes a la UNAL por género",
       x = "\nPeriodo",
       y = "Porcentaje",
       linetype = "Género")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra40
ggsave("Exportar/SVG/Grafico40.svg")


# Gráfico 41 ----

Asp <- UnalData::Aspirantes %>% filter(YEAR == 2024, 
                                       SEMESTRE == 1,
                                       SEXO %in% c("Mujeres", "Hombres")) %>% 
       summarise(Total = n(), .by = c(SEXO)) %>% 
       mutate(Participa = Total/sum(Total),
              Porcent = scales::percent(Participa, accuracy = 0.1),
              Pobla = "Aspirantes")
Adm <- UnalData::Aspirantes %>% filter(YEAR == 2024, 
                                       SEMESTRE == 1,
                                       ADMITIDO == "Sí",
                                       SEXO %in% c("Mujeres", "Hombres")) %>% 
  summarise(Total = n(), .by = c(SEXO)) %>% 
  mutate(Participa = Total/sum(Total),
         Porcent = scales::percent(Participa, accuracy = 0.1),,
         Pobla = "Admitidos")
Mat <- UnalData::Matriculados %>% filter(YEAR == 2024, 
                                       SEMESTRE == 1,
                                       SEXO %in% c("Mujeres", "Hombres")) %>% 
  summarise(Total = n(), .by = c(SEXO)) %>% 
  mutate(Participa = Total/sum(Total),
         Porcent = scales::percent(Participa, accuracy = 0.1),
         Pobla = "Matriculados")
Gra <- UnalData::Graduados %>% filter(YEAR == 2024, 
                                         SEMESTRE == 1,
                                         SEXO %in% c("Mujeres", "Hombres")) %>% 
  summarise(Total = n(), .by = c(SEXO)) %>% 
  mutate(Participa = Total/sum(Total),
         Porcent = scales::percent(Participa, accuracy = 0.1),
         Pobla = "Graduados")
Doc <- UnalData::Docentes %>% filter(YEAR == 2024, 
                                      SEMESTRE == 1,
                                      SEXO %in% c("Mujeres", "Hombres")) %>% 
  summarise(Total = n(), .by = c(SEXO)) %>% 
  mutate(Participa = Total/sum(Total),
         Porcent = scales::percent(Participa, accuracy = 0.1),
         Pobla = "Docentes")
Fun <- UnalData::Administrativos %>% filter(YEAR == 2024, 
                                     SEMESTRE == 1,
                                     SEXO %in% c("Mujeres", "Hombres")) %>% 
  summarise(Total = n(), .by = c(SEXO)) %>% 
  mutate(Participa = Total/sum(Total),
         Porcent = scales::percent(Participa, accuracy = 0.1),
         Pobla = "Funcionarios")
Pob_Sexo <- bind_rows(Adm, Mat, Gra) %>% 
            mutate(Pobla = fct_relevel(Pobla, "Aspirantes", "Admitidos", "Matriculados", "Graduados", "Docentes", "Funcionarios"))

Gra41 <- Pob_Sexo %>% ggplot(aes(x = Pobla, y = Participa, fill = SEXO))+
             geom_bar(stat = "identity", position = "dodge", width = 0.8)+
             geom_text(aes(label = Porcent),
                       position = position_dodge(width = 0.9),
                       vjust = -0.7,
                       size = 3)+
              geom_hline(yintercept = 0.5, linetype = "dashed", linewidth = 0.4)+
  scale_y_continuous(labels = scales::percent,limits = c(0, 1))+ 
  scale_fill_manual(values = c("gray35", "gray60"))+
  labs(title = "Participación de hombres y mujeres en las poblaciones de admitidos,matriculados y\ngraduados",
       subtitle = "Periodo 2024-1",
       x = "\nPoblación",
       y = "Porcentaje\n",
       fill = "Sexo")+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
  
Gra41
ggsave("Exportar/SVG/Grafico41.svg")

# Gráfico 42 ----

Gra42 <- UnalData::Matriculados %>% 
  filter(TIPO_ADM %in% c("PAES", "PEAMA", "PAET")) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, SEXO)) %>% 
  group_by(Periodo) %>% 
  mutate(Participación = Total/sum(Total),
         Porcentaje = scales::percent(Participación, accuracy = 0.1)) %>% 
  mutate(Porcentaje = ifelse(Periodo %in% c("2009-1", "2024-1"),  
                             format(Porcentaje, big.mark = ".", decimal.mark = ","), 
                             NA)) %>% 
  ggplot(aes(x = Periodo, y = Participación, group = SEXO))+
  geom_line(aes(linetype = SEXO))+
  geom_text_repel(aes(label = Porcentaje),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  labs(title = "Evolución porcentaje de matriculados en pregrado programas PAES, PEAMA y PAET*\npor género",
       x = "\nPeriodo",
       y = "Porcentaje",
       linetype = "Género",
       caption = "(*). Los primeros matriculados admitidos a través del programa PAET\nse dieron en el periodo 2024-1")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra42
ggsave("Exportar/SVG/Grafico42.svg")

# Gráfico 43 ----

Gra43 <- UnalData::Docentes %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, SEXO)) %>% 
  group_by(Periodo) %>% 
  mutate(Participación = Total/sum(Total),
         Porcentaje = scales::percent(Participación, accuracy = 0.1)) %>% 
  mutate(Porcentaje = ifelse(Periodo %in% c("2008-2", "2024-2"),  
                             format(Porcentaje, big.mark = ".", decimal.mark = ","), 
                             NA)) %>% 
  ggplot(aes(x = Periodo, y = Participación, group = SEXO))+
  geom_line(aes(linetype = SEXO))+
  geom_text_repel(aes(label = Porcentaje),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  labs(title = "Evolución porcentaje de docentes de carrera en la UNAL por sexo biológico",
       x = "\nPeriodo",
       y = "Porcentaje",
       linetype = "Sexo")+
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")

Gra43
ggsave("Exportar/SVG/Grafico43.svg")

# Gráfico 44 ----

Gra44 <- UnalData::Aspirantes %>% 
  filter(DISCAPACIDAD == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2009-1", "2019-1", "2021-1", "2023-1", "2024-2"),  
                        format(Total, big.mark = ".", decimal.mark = ","), 
                        NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 1,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de aspirantes a la UNAL en situación de discapacidad\npor periodos académicos",
       x = "\nPeriodo",
       y = "Total aspirantes\n")+
  scale_y_continuous(limits = c(0, 700))+
  theme(axis.text.x = element_text(angle = 90))

Gra44
ggsave("Exportar/SVG/Grafico44.svg")

# Gráfico 45 ----

Gra45 <- UnalData::Aspirantes %>% 
  filter(DISCAPACIDAD == "Sí", ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2009-1", "2016-2", "2019-1", "2023-1", "2024-2"),  
                        format(Total, big.mark = ".", decimal.mark = ","), 
                        NA)) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Grupo ))+
  geom_point(size = 1)+
  geom_line()+
  geom_label_repel(aes(label = Totaf), 
                   box.padding = 1,
                   segment.linetype = 3,
                   size = 2.8)+
  labs(title = "Evolución total de admitidos a la UNAL en situación de discapacidad\npor periodos académicos",
       x = "\nPeriodo",
       y = "Total admitidos\n")+
  scale_y_continuous(limits = c(0, 80))+
  theme(axis.text.x = element_text(angle = 90))

Gra45
ggsave("Exportar/SVG/Grafico45.svg")

# Gráfico 46 ----

Gra46 <- UnalData::Aspirantes %>% 
  filter(TIPO_INS %in% c("PAES", "PEAMA", "PAET")) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, TIPO_INS)) %>%
  mutate(Etiqueta = ifelse(Periodo %in% c("2008-1", "2019-1", "2024-2") & 
                             TIPO_INS %in% c("PAES", "PEAMA", "PAET"),
                           Total, NA)) %>% 
  rename(Programa = TIPO_INS) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_text_repel(aes(label = Etiqueta),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  geom_point(aes(shape = Programa),
             size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución Total de aspirantes en la UNAL programas PAES, PEAMA y PAET",
       x = "\nPeriodo",
       y = "Total aspirantes",
       linetype = "Programa")+
  theme(axis.text.x = element_text(angle = 90))

Gra46
ggsave("Exportar/SVG/Grafico46.svg")

# Gráfico 47 ----

Gra47 <- UnalData::Matriculados %>% 
  filter(TIPO_ADM %in% c("PAES", "PEAMA", "PAET")) %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo, TIPO_ADM)) %>%
  mutate(Etiqueta = ifelse(Periodo %in% c("2009-1", "2024-1") & 
                             TIPO_ADM %in% c("PAES", "PEAMA", "PAET"),
                           Total, NA)) %>% 
  rename(Programa = TIPO_ADM) %>% 
  ggplot(aes(x = Periodo, y = Total, group = Programa))+
  geom_line(aes(linetype = Programa))+
  geom_text_repel(aes(label = Etiqueta),
                  box.padding = 1,
                  segment.linetype = 6,
                  size = 2.8)+
  geom_point(aes(shape = Programa),
             size = 1.3,
             alpha = 0.5)+
  labs(title = "Evolución Total de matriculados en la UNAL programas PAES, PEAMA y PAET",
       x = "\nPeriodo",
       y = "Total matriculados",
       linetype = "Programa")+
  theme(axis.text.x = element_text(angle = 90))

Gra47
ggsave("Exportar/SVG/Grafico47.svg")

# Gráfico 48 ----

Gra48 <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1,
                                           TIPO_ADM %in% c("PAES")) %>%
  summarise(Total = n(), .by = c(PAES)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         PAES = fct_reorder(PAES, Total, sum)) %>% 
  rename(Modalidades = PAES) %>% 
  ggplot(aes(x = Modalidades, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45",
           width = 0.5)+
  geom_text(aes(label = Porcentaje), hjust = -0.3, size = 3)+
  scale_y_continuous(limits = c(0,1500))+
  labs(title = "Participación total de matriculados en la UNAL por modalidades\ndel programa PAES",
       subtitle = "Periodo 2024-1",
       x = "Modalidades programa PAES\n",
       y = "\nTotal de matriculados")+
  coord_flip()

Gra48
ggsave("Exportar/SVG/Grafico48.svg")

# Gráfico 49 ----

Gra49 <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1,
                                           TIPO_ADM %in% c("PEAMA")) %>%
  summarise(Total = n(), .by = c(PEAMA)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         PEAMA = fct_reorder(PEAMA, Total, sum)) %>% 
  rename(Modalidades = PEAMA) %>% 
  ggplot(aes(x = Modalidades, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45",
           width = 0.5)+
  geom_text(aes(label = Porcentaje), hjust = -0.3, size = 3)+
  scale_y_continuous(limits = c(0,2000))+
  labs(title = "Participación total de matriculados en la UNAL por modalidades\ndel programa PEAMA",
       subtitle = "Periodo 2024-1",
       x = "Modalidades programa PEAMA\n",
       y = "\nTotal de matriculados")+
  coord_flip()

Gra49
ggsave("Exportar/SVG/Grafico49.svg")

# Gráfico 50 ----

Gra50 <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1,
                                           TIPO_ADM %in% c("PAET")) %>%
  summarise(Total = n(), .by = c(PAET)) %>% 
  mutate(Porcentaje = scales::percent(Total/sum(Total), accuracy = 0.1),
         PAET = fct_reorder(PAET, Total, sum)) %>% 
  rename(Modalidades = PAET) %>% 
  ggplot(aes(x = Modalidades, y = Total))+
  geom_bar(position = "identity",
           stat = "identity", 
           fill = "gray45",
           width = 0.5)+
  geom_text(aes(label = Porcentaje), hjust = -0.3, size = 3)+
  scale_y_continuous(limits = c(0, 110))+
  labs(title = "Participación total de matriculados en la UNAL por modalidades\ndel programa PAET",
       subtitle = "Periodo 2024-1",
       x = "Modalidades programa PAET\n",
       y = "\nTotal de matriculados")+
  coord_flip()

Gra50
ggsave("Exportar/SVG/Grafico50.svg")

