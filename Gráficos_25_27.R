##%######################################################%##
#                                                          #
####          Gráficos Capítulo PGD 2025-2027           ####
#                                                          #
##%######################################################%##

# Librerías  ----

library(tidyverse)
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



# Figura 1 ----

Fig1 <- Programas %>% 
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

Fig1

# Figura 2 ----

Fig2 <- Programas %>% 
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
Fig2

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
  labs(title = "Evolución total de aspirantes a la UNAL por periodos académicos",
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
             fill = "gray45",
             width = 0.7)+
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
  labs(title = "Evolución total de admitidos a la UNAL por periodos académicos",
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
  labs(title = "Evolución total de estudiantes matriculados en la UNAL por periodos académicos",
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
           fill = "gray45",
           width = 0.7)+
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

# Figura 10 ----

Fig10 <- UnalData::Graduados %>% 
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

Fig10

# Figura 11 ----

Fig11 <- UnalData::Graduados %>% filter(YEAR >= 2023) %>%
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
Fig11


# Figura 12 ----

Fig12 <- UnalData::Graduados %>% filter(YEAR >= 2023, TIPO_NIVEL == "Pregrado") %>%
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
Fig12


# Figura 13 ----

Fig13 <- UnalData::Docentes %>% 
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

Fig13

# Figura 14 ----

Fig14 <- UnalData::Docentes %>% filter(YEAR == 2024, SEMESTRE == 2) %>%
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
Fig14


# Figura 15 ----

Fig15 <- UnalData::Administrativos %>% 
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

Fig15

# Figura 16 ----

Fig16 <- UnalData::Administrativos %>% 
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
Fig16


# Figura 20 ----

Fig20 <- Grupos %>%
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
   labs(title = "Grupos de investigación de la UNAL categorizados en MinCiencias por sedes",
       subtitle = "Año 2023",
       x = "\nSede",
       y = "Total grupos\n",
       fill = "Categoría del grupo")+
    theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
Fig20  

# Figura 21 ----

Fig21 <- Investigadores %>%
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
Fig21  

# Figura 22 ----

Fig22 <- Extensión %>%
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
Fig22 



# Figura 23 ----

Fig23 <- Rankings %>% filter(RANKING == "QSMundo") %>% 
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

Fig23

# Figura 24----

Fig24 <- Rankings %>% filter(RANKING == "THEMundo") %>% 
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

Fig24


# Figura 25----

Fig25 <- Rankings %>% filter(RANKING == "USapiens") %>% 
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

Fig25


# Figura 26 ----

Fig26 <- UnalData::Aspirantes %>% 
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
Fig26


# Figura 27 ----

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
Pob_Sexo <- bind_rows(Asp, Adm, Mat, Gra, Doc, Fun) %>% 
            mutate(Pobla = fct_relevel(Pobla, "Aspirantes", "Admitidos", "Matriculados", "Graduados", "Docentes", "Funcionarios"))

Fig27 <- Pob_Sexo %>% ggplot(aes(x = Pobla, y = Participa, fill = SEXO))+
             geom_bar(stat = "identity", position = "dodge", width = 0.8)+
             geom_text(aes(label = Porcent),
                       position = position_dodge(width = 0.9),
                       vjust = -0.7,
                       size = 3)+
              geom_hline(yintercept = 0.5, linetype = "dashed", size = 0.4)+
  scale_y_continuous(labels = scales::percent,limits = c(0, 1))+ 
  scale_fill_manual(values = c("gray35", "gray60"))+
  labs(title = "Participación de hombres y mujeres en las poblaciones de aspirantes, admitidos,\nmatriculados, graduados, docentes y funcionarios",
       subtitle = "Periodo 2024-1",
       x = "\nPoblación",
       y = "Porcentaje\n",
       fill = "Sexo")+
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "bottom")
  
Fig27

# Figura 28 ----

Fig28 <- UnalData::Matriculados %>% 
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
Fig28

# Figura 29 ----

Fig29 <- UnalData::Docentes %>% 
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
Fig29

# Figura 30 ----

Fig30 <- UnalData::Aspirantes %>% 
  filter(DISCAPACIDAD == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2009-1", "2021-1", "2023-1", "2024-2"),  
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

Fig30

# Figura 31 ----

Fig31 <- UnalData::Aspirantes %>% 
  filter(DISCAPACIDAD == "Sí", ADMITIDO == "Sí") %>% 
  mutate(Periodo = paste(YEAR, SEMESTRE, sep = "-")) %>% 
  summarise(Total = n(), .by = c(Periodo)) %>% 
  mutate(Grupo = "General",
         Totaf = ifelse(Periodo %in% c("2009-1", "2016-2", "2023-1", "2024-2"),  
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

Fig31

# Figura 32 ----

Fig32 <- UnalData::Aspirantes %>% 
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

Fig32

# Figura 33 ----

Fig32 <- UnalData::Matriculados %>% 
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

Fig32

# Figura 34 ----

Fig34 <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1,
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

Fig34

# Figura 35 ----

Fig35 <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1,
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


Fig35

# Figura 35b ----

Fig35b <- UnalData::Matriculados %>% filter(YEAR == 2024, SEMESTRE == 1,
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

Fig35b

# Figura 36 ----

Fig36 <- Desersión %>% mutate(Año = as.numeric(str_sub(APERTURA, 1, 4)),
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

Fig36

# Figura 37 ----

Fig37 <- Desersión %>% mutate(Año = as.numeric(str_sub(APERTURA, 1, 4)),
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
  
Fig37


