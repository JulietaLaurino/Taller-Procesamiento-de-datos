
# Parte 1 -----------------------------------------------------------------

## **Configuraciones iniciales**  
  
#### **Paquetes**
install.packages("tidyverse")
install.packages("xlsx")

#### **Directorio de trabajo**
setwd("C:/Users/User/Desktop/Taller - Procesamiento de datos")

## **Paso 1: Leo y almaceno la base de datos** 
library(tidyverse)
data_v14 <- read.csv("data_exp_11854-v14_task-5ksx.csv")
data_v15 <- read.csv("data_exp_11854-v15_task-5ksx.csv")

## **Paso 2: Unir bases de datos**
data_raw <- bind_rows(data_v14, data_v15)

## **Paso 3: Renombro columas**
data_raw <- rename (data_raw,"id" = Participant.Public.ID, "group" = randomiser.qg34, "RT" = Reaction.Time, "Acc" = Correct, "relation" = primeType, "familiarity" = trialType, "block" = 	block_testingSR) 

## **Paso 4: Filtro filas** 
data_raw <- filter (data_raw, Screen.Name == "target", display == "Tarea") 

## **Paso 5: Selecciono columnas**
data_raw <- select (data_raw, id, group, relation, familiarity, block, prime, target, Response, Acc, RT) 

## **Bonus track I: Ordeno filas**
data_raw <- arrange (data_raw, group, id, familiarity, block, target)

## **Bonus track II: Nuevas columnas** 
data_raw <- mutate (data_raw, accRT = ifelse(Acc == 1, RT, NA))

slice(data_raw, 1:10)

## **Resumen y conlusión**
### **Todo junto!**
data_raw <- bind_rows(data_v14, data_v15) %>% 
  rename("id" = Participant.Public.ID, "group" = randomiser.qg34, "RT" = Reaction.Time, "Acc" = Correct, "relation" = primeType, "familiarity" = trialType, "block" = 	block_testingSR) %>% 
  filter (Screen.Name == "target", display == "Tarea") %>% 
  select (id, group, relation, familiarity, block, prime, target, Response, Acc, RT) %>% 
  arrange (group, id, familiarity, block, target) %>% 
  mutate (accRT = ifelse(Acc == 1, RT, NA))

### **Chequeo**
trials_count <- data_raw %>% 
  group_by(id) %>% 
  count()

slice(trials_count, 1:10)



# Parte 2 -----------------------------------------------------------------

## **Criterio de inclusión por accuracy**
data_exclude <- data_raw %>%
  filter (familiarity == "familiar") %>% 
  group_by(id, group) %>% 
  summarise(meanAcc = mean(Acc)) %>% 
  filter(meanAcc < 0.95)

slice(data_exclude, 1:20)

data <- data_raw %>% 
  filter(!(id %in% data_exclude$id) & !(RT < 200))

## **Análisis exploratorio**
data_condition <- data %>% 
  group_by(group, familiarity, relation) %>% 
  summarise(meanAcc = mean(Acc), 
            meanRT = mean(RT, na.rm = TRUE),
            sd_RT = sd(RT))

slice(data_condition, 1:8)

## **Guardado**
library(xlsx)
write.xlsx(data, file= "PRIMINGdata.gorilla.xlsx", sheetName = "sheet1")
write.xlsx(data, file= "PRIMINGdata.gorilla.xlsx", append = T, sheetName = "sheet2")

## **Bonus track: cambio de tipo de base de datos**
# Pivot wider
data_wide <- data %>% 
  select(id, Acc, target, prime, relation) %>% 
  pivot_wider(names_from = id, values_from = Acc)

slice(data_wide, 1:10)

# Pivot longer
data_long <- data_wide %>% 
  pivot_longer(cols = c(-target, -prime, -relation), names_to = "id", values_to = "Acc")







# FIN :)
