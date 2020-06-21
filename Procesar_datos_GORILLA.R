#Instalo paquetes 
install.packages("tidyverse")
install.packages("xlsx")

# Setear el directorio de trabajo correspondiente
setwd("C:/Users/User/Desktop/Taller - Procesamiento de datos")

#Abro los archivos
data_v14 <- read.csv("data_exp_11854-v14_task-5ksx.csv")
data_v15 <- read.csv("data_exp_11854-v15_task-5ksx.csv")

#Junto las dos bases de datos, una abajo de la otra 
data_raw <- bind_rows(data_v14, data_v15)

#Uso rename para cambiar el nombre de algunas variables (columnas)
data_raw <- rename(data_raw,"id" = Participant.Public.ID, "group" = randomiser.qg34, "RT" = Reaction.Time, "Acc" = Correct, "relation" = primeType, "familiarity" = trialType, "block" = 	block_testingSR) 

#Uso filter para quedarme con la Accuracy y RT del target. Además me quedo con la tarea (no analizo el demo) 
data_raw <- filter (data_raw, Screen.Name == "target", display == "Tarea") 

#Uso select para quedarme sólo con las columnas que me interesan
data_raw <- select (data_raw, id, group, relation, familiarity, block, prime, target, Response, Acc, RT) 
  
#Uso arrange para ordenar las filas 
data_raw <- arrange (data_raw, group, id, familiarity, block, target)

#Todo junto!
data_raw <- bind_rows(data_v14, data_v15) %>% 
  rename("id" = Participant.Public.ID, "group" = randomiser.qg34, "RT" = Reaction.Time, "Acc" = Correct, "relation" = primeType, "familiarity" = trialType, "block" = 	block_testingSR) %>% 
  filter (Screen.Name == "target", display == "Tarea") %>% 
  select (id, group, relation, familiarity, block, prime, target, Response, Acc, RT) %>% 
  arrange (group, id, familiarity, block, target)

#Chequeo
trials_count <- data_raw %>% 
  group_by(id) %>% 
  count()

#Participantes outliers por accuracy
data_outliers <- data_raw %>%
  filter (familiarity == "familiar") %>% 
  group_by(id, group) %>% 
  summarise(meanAcc = mean(Acc)) %>% 
  filter(meanAcc < 0.9)

#Los elimino de mi base de datos
data <- data_raw %>% 
  filter(!(id %in% data_outliers$id) & !(RT < 200))  

#Análisis exploratorio
data_condition <- data %>% 
  group_by(group, familiarity, relation) %>% 
  summarise(meanAcc = mean(Acc), 
            meanRT = mean(RT, na.rm = TRUE),
            sd_RT = sd(RT))

#Para analizar RTs: uso mutate para generar una nueva columna que tenga sólo los RTs en los que la rta fue correcta. 
data <- data %>% 
  mutate(accRT = ifelse(Acc == 1, RT, NA))

#Cambio el tipo de base de datos (de larga a ancha) con pivot_wider 
data_wide <- data %>% 
  select(id, Acc, target, prime, relation) %>% 
  pivot_wider(names_from = id, values_from = Acc)

#Vuelvo a la forma anterior con pivot_longer
data_long <- data_wide %>% 
  pivot_longer(cols = c(-target, -prime, -relation), names_to = "id", values_to = "Acc")
