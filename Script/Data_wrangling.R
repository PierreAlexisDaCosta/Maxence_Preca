# Load Package and Function ####
source("./Script/Package.R")

set.seed(123)

# Import and inspect the data ####
raw_data <-
  readxl::read_excel("./Raw_data/Enquête précarité 2023 - ANEMF (réponses).xlsx")

pre_processed_data <-
  raw_data %>%
  filter(`Donnez-vous votre consentement pour que nous analysions vos réponses ? Elles seront supprimées de notre base de données à la fin de leur traitement.` == "Oui") %>%
  select(- Horodateur) %>%
  select(- `Donnez-vous votre consentement pour que nous analysions vos réponses ? Elles seront supprimées de notre base de données à la fin de leur traitement.`) %>%
  filter((`Quelle est votre année d'étude ?` == "DFGSM2 (P2)" & 
           `Êtes-vous externe pour l'année 2022-2023` == "Non") |
           (`Quelle est votre année d'étude ?` == "DFGSM3 (D1)" & 
              `Êtes-vous externe pour l'année 2022-2023` == "Non") |
           (`Quelle est votre année d'étude ?` == "DFASM1 (D2)" & 
              `Êtes-vous externe pour l'année 2022-2023` == "Oui") |
           (`Quelle est votre année d'étude ?` == "DFASM2 (D3)" & 
              `Êtes-vous externe pour l'année 2022-2023` == "Oui") |
           (`Quelle est votre année d'étude ?` == "DFASM3 (D4)" & 
              `Êtes-vous externe pour l'année 2022-2023` == "Oui")) %>%
  select(- `Êtes-vous externe pour l'année 2022-2023`) %>%
  as_tibble()

pre_processed_data <- 
  pre_processed_data %>% 
  dplyr::rename(Genre = `Vous êtes :`) 

pre_processed_data$`Quel âge avez-vous ?` <-
  str_remove(string = pre_processed_data$`Quel âge avez-vous ?`,
           pattern = " ans")

pre_processed_data$`Quel âge avez-vous ?` <-
  factor(pre_processed_data$`Quel âge avez-vous ?`)

# Années d'études
pre_processed_data$`Quelle est votre année d'étude ?` <-
  factor(pre_processed_data$`Quelle est votre année d'étude ?`)

levels(pre_processed_data$`Quelle est votre année d'étude ?`) <-
  list(`DFGSM2 (P2)` ="1", `DFGSM3 (D1)` ="2", 
       `DFASM1 (D2)` ="3", `DFASM2 (D3)` ="4",
       `DFASM3 (D4)` ="5")

pre_processed_data <- 
  pre_processed_data %>%
  mutate(`Êtes-vous externe pour l'année 2022-2023` = 
           ifelse(`Quelle est votre année d'étude ?` == "DFASM1 (D2)" | 
                  `Quelle est votre année d'étude ?` == "DFASM2 (D3)"| 
                    `Quelle est votre année d'étude ?` == "DFASM3 (D4)",
                  yes = "Externe",
                  no = "1er_cycle")) 

# Genre
pre_processed_data$Genre <-
  factor(pre_processed_data$Genre)

levels(pre_processed_data$Genre) <-
  list(`Un homme` ="1", `Une femme` ="2", 
       `Autre` ="3")

# Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié de Revenu de stage ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                             "Revenu de stage") == T,
             yes = "Revenu de stage",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié de Revenu d'emploi (hors stage) ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "Revenu d'emploi") == T,
             yes = "Revenu d'emploi (hors stage)",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié d'Aides publiques ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "Aides publiques") == T,
             yes = "Aides publiques (bourse du CROUS, autres bourses, allocations diverses (APL, CAF...))",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié du CESP ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "CESP") == T,
             yes = "CESP",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié de Participation de la famille ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "Participation de la famille") == T,
             yes = "Participation de la famille",
             no = "Non"))


pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié de Participation du partenaire ou conjoint ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "Participation du partenaire ou conjoint") == T,
             yes = "Participation du partenaire ou conjoint",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié d'un Prêt étudiant (public ou privé) sur l'année 2022 ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "Prêt étudiant") == T,
             yes = "Prêt étudiant (public ou privé) sur l'année 2022",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié d'un Prêt de la famille (à rembourser) ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "Prêt de la famille") == T,
             yes = "Prêt de la famille (à rembourser)",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(`Au cours de ce semestre, avez-vous bénéficié de vos Économies, épargnes globalement disponibles sur l'année 2022 ?` = 
           ifelse(
             str_detect(pre_processed_data$`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`,
                        "Économies, épargnes globalement disponibles sur l'année 2022") == T,
             yes = "Économies, épargnes globalement disponibles sur l'année 2022",
             no = "Non"))


# Bourse CROUS
pre_processed_data$`Bénéficiez-vous d'une bourse sur critère sociaux (bourse du CROUS) sur l'année universitaire 2022-2023` <-
  factor(pre_processed_data$`Bénéficiez-vous d'une bourse sur critère sociaux (bourse du CROUS) sur l'année universitaire 2022-2023`)
  

pre_processed_data$`Quel est votre échelon de bourses ?` <-
  ifelse(pre_processed_data$`Bénéficiez-vous d'une bourse sur critère sociaux (bourse du CROUS) sur l'année universitaire 2022-2023` == "Non" &
         is.na(pre_processed_data$`Quel est votre échelon de bourses ?`) == T,
       yes = "Pas_boursier",
       no = pre_processed_data$`Quel est votre échelon de bourses ?`)
  
pre_processed_data$`Quel est votre échelon de bourses ?` <-
  factor(pre_processed_data$`Quel est votre échelon de bourses ?`)

# Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?	

pre_processed_data <-
  pre_processed_data %>%
  mutate(Obligation_prêt_étudiant = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Obligation d'un prêt étudiant") == T,
             yes = "Obligation d'un prêt étudiant",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Obligation_emprun_proche = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Obligation d'emprunt auprès d'un proche") == T,
             yes = "Obligation d'emprunt auprès d'un proche",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Découvert_bancaire = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Découvert bancaire") == T,
             yes = "Découvert bancaire",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Obligation_emprun_proche = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Réduction de l'épargne") == T,
             yes = "Réduction de l'épargne",
             no = "Non"))


pre_processed_data <-
  pre_processed_data %>%
  mutate(Difficulté_loyer = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Difficulté à payer son loyer") == T,
             yes = "Difficulté à payer son loyer",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Impossibilité_loyer = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Impossibilité à payer son loyer") == T,
             yes = "Impossibilité à payer son loyer",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Cessation_location_logement = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Cessation de location de logement") == T,
             yes = "Cessation de location de logement",
             no = "Non"))





# Processing ####
processed_data <-
  pre_processed_data %>% 
  select(-`Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`) %>%
  select(- `Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`)	%>%
  view



# Table summary ####
str(pre_processed_data)

processed_data %>% 
  select(- `Quel âge avez-vous ?`) %>%
  select(- `De quelle UFR êtes-vous ?`) %>%
  select(-`Au cours de ce semestre, avez-vous bénéficié de Revenu de stage ?` : `Au cours de ce semestre, avez-vous bénéficié de vos Économies, épargnes globalement disponibles sur l'année 2022 ?`) %>%
  tbl_summary() 

summary(raw_data)
