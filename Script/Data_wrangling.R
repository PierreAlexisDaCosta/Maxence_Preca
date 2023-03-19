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
  select(- `Combien de personnes avez-vous à votre charge ?`) %>%
  rename(`De quel type d'activité rémunérée s'agissait-il ?` = `De quel type d'activité rémunérée s'agissait-il ?...35`) %>%
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

pre_processed_data <-
  pre_processed_data %>%
  mutate(Saut_de_repas = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Saut de repas") == T,
             yes = "Saut de repas",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Diminution_hygiène = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Diminution de l'hygiène de vie") == T,
             yes = "Diminution de l'hygiène de vie",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Renonciation_soins = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Renonciation à des soins") == T,
             yes = "Renonciation à des soins",
             no = "Non"))


pre_processed_data <-
  pre_processed_data %>%
  mutate(Renonciation_loisirs = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Renonciation à des loisirs") == T,
             yes = "Renonciation à des loisirs",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Impossibilité_vacances = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Impossibilité de partir en vacances") == T,
             yes = "Impossibilité de partir en vacances",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Aucun_problème = 
           ifelse(
             str_detect(pre_processed_data$`Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`,
                        "Cela n'entraîne aucune de ces situations") == T,
             yes = "Cela n'entraîne aucune de ces situations",
             no = "Non"))

# En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?	
pre_processed_data <-
  pre_processed_data %>%
  mutate(Bourse_mérite = 
           ifelse(
             str_detect(pre_processed_data$`En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?`,
                        "Une bourse au mérite") == T,
             yes = "Une bourse au mérite",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Allocation_logement = 
           ifelse(
             str_detect(pre_processed_data$`En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?`,
                        "Une allocation logement") == T,
             yes = "Une allocation logement",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Allocation_familiale = 
           ifelse(
             str_detect(pre_processed_data$`En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?`,
                        "Une allocation familiale") == T,
             yes = "Une allocation familiale",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Allocation_chômage = 
           ifelse(
             str_detect(pre_processed_data$`En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?`,
                        "Une allocation chômage") == T,
             yes = "Une allocation chômage",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Aucune = 
           ifelse(
             str_detect(pre_processed_data$`En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?`,
                        "Aucune de ces aides") == T,
             yes = "Aucune de ces aides",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Autres_aides = 
           ifelse(
             str_detect(pre_processed_data$`En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?`,
                        "Aucune de ces aides|Une bourse au mérite|Une allocation logement|Une allocation familiale|Une allocation chômage") == F,
             yes = "Autre",
             no = "Non"))

# Les membres de votre famille vous aident-ils : [En vous donnant des provisions alimentaires]	
processed_data$`Les membres de votre famille vous aident-ils : [En vous donnant des provisions alimentaires]` <-
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous donnant des provisions alimentaires]`)

#Les membres de votre famille vous aident-ils : [En vous prêtant une voiture]	
processed_data$`Les membres de votre famille vous aident-ils : [En vous prêtant une voiture]` <-
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous prêtant une voiture]`)

#Les membres de votre famille vous aident-ils : [En vous faisant de "gros" cadeaux (hifi, TV, meubles, vacances...)]	

processed_data$`Les membres de votre famille vous aident-ils : [En vous faisant de "gros" cadeaux (hifi, TV, meubles, vacances...)]`<-
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous faisant de "gros" cadeaux (hifi, TV, meubles, vacances...)]`)

#Les membres de votre famille vous aident-ils : [En vous faisant de "petits" cadeaux (disques, livres, vêtements...)]	
processed_data$`Les membres de votre famille vous aident-ils : [En vous faisant de "petits" cadeaux (disques, livres, vêtements...)]`<-
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous faisant de "petits" cadeaux (disques, livres, vêtements...)]`)

#Les membres de votre famille vous aident-ils : [En vous finançant le loyer de votre logement]	

processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant le loyer de votre logement]` <-
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant le loyer de votre logement]`)

#Les membres de votre famille vous aident-ils : [En vous finançant vos frais de déplacements (abonnement de bus, etc)]	
processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant vos frais de déplacements (abonnement de bus, etc)]` <-
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant vos frais de déplacements (abonnement de bus, etc)]`)

#Les membres de votre famille vous aident-ils : [En vous finançant une partie ou l'entièreté de vos charge (électricité, eau)]	
processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant une partie ou l'entièreté de vos charge (électricité, eau)]`	<- 
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant une partie ou l'entièreté de vos charge (électricité, eau)]`)

#Les membres de votre famille vous aident-ils : [En vous finançant une partie ou l'entièreté de vos abonnements téléphoniques ou internet]
processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant une partie ou l'entièreté de vos abonnements téléphoniques ou internet]` <-
  factor(processed_data$`Les membres de votre famille vous aident-ils : [En vous finançant une partie ou l'entièreté de vos abonnements téléphoniques ou internet]`)

#Depuis le début de l'année 2022, vous est-il arrivé ...	
pre_processed_data <-
  pre_processed_data %>%
  mutate(Epargner = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "D’épargner") == T,
             yes = "Epargner",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Découvert_banque = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "D'avoir un découvert à la banque") == T,
             yes = "Découvert à la banque",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Emprunt_banque = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "D'emprunter à la banque") == T,
             yes = "D'emprunter à la banque",
             no = "Non"))


pre_processed_data <-
  pre_processed_data %>%
  mutate(Refus_prêt = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "D'avoir un refus de prêt") == T,
             yes = "Refus de prêt",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Demande_aide_famille = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "De demander une aide exceptionnelle à votre famille") == T,
             yes = "De demander une aide exceptionnelle à votre famille	",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Demande_aide_amis = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "De demander une aide exceptionnelle à vos amis") == T,
             yes = "De demander une aide exceptionnelle à vos amis",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Restriction_activités = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "De vous restreindre") == T,
             yes = "De vous restreindre dans vos activités sociales",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Piocher_économies = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "De piocher dans vos économies") == T,
             yes = "De piocher dans vos économies",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Demande_aide_sociale = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "De demander une aide sociale exceptionnelle") == T,
             yes = "De demander une aide sociale exceptionnelle",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Travailler = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "De vous mettre à travailler") == T,
             yes = "De vous mettre à travailler",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Renoncer_soins = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "Renoncer à des soins") == T,
             yes = "Renoncer à des soins",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Rien = 
           ifelse(
             str_detect(pre_processed_data$`Depuis le début de l'année 2022, vous est-il arrivé ...`,
                        "Rien de cela") == T,
             yes = "Rien de cela",
             no = "Non"))

#Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, 
#dans quelle mesure les propositions suivantes correspondent-elles
#à votre situation ? [Elle(s) m'est (me sont) indispensable(s) pour vivre]
pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) m'est (me sont) indispensable(s) pour vivre]` <-
  factor(pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) m'est (me sont) indispensable(s) pour vivre]`)

#Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) m'assure(nt) l'indépendance vis à vis de mes parents]	
pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) m'assure(nt) l'indépendance vis à vis de mes parents]` <-
  factor(pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) m'assure(nt) l'indépendance vis à vis de mes parents]`)
  
#Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) me permet(tent) d'acquérir une expérience professionnelle]	
pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) me permet(tent) d'acquérir une expérience professionnelle]` <-
  factor(pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) me permet(tent) d'acquérir une expérience professionnelle]`)

#Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) me permet(tent) d'occuper mon temps libre]	
pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) me permet(tent) d'occuper mon temps libre]` <-
  factor(pre_processed_data$`Concernant votre (vos) activité(s) rémunérée(s) durant l'année universitaire, dans quelle mesure les propositions suivantes correspondent-elles à votre situation ? [Elle(s) me permet(tent) d'occuper mon temps libre]`)

#De quel type d'activité rémunérée s'agissait-il ?

pre_processed_data <-
  pre_processed_data %>%
  mutate(Employé_hopîtal = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "hôpital") == T,
             yes = "Employé d'hôpital",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Babysitter_aide_à_la_personne = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "Baby-sitter") == T,
             yes = "Babysitter_aide_à_la_personne",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Employé_université = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "université") == T,
             yes = "Employé_université",
             no = "Non"))



pre_processed_data <-
  pre_processed_data %>%
  mutate(Cours_particuliers = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "particuliers") == T,
             yes = "Cours_particuliers",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Intérim = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "Intérim") == T,
             yes = "Intérim",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Employé_commerce_ou_restauration = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "commerce") == T,
             yes = "commerce",
             no = "Non"))


pre_processed_data <-
  pre_processed_data %>%
  mutate(Employé_administratif = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "administratif") == T,
             yes = "administratif",
             no = "Non"))


pre_processed_data <-
  pre_processed_data %>%
  mutate(Enseignant = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "Enseignant") == T,
             yes = "Enseignant",
             no = "Non"))


pre_processed_data <-
  pre_processed_data %>%
  mutate(Animateur = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "Animateur") == T,
             yes = "Animateur",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Agent_de_service = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "Agent de service") == T,
             yes = "Agent de service",
             no = "Non"))

pre_processed_data <-
  pre_processed_data %>%
  mutate(Autre_travail = 
           ifelse(
             str_detect(pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?`,
                        "Agent de service|Animateur|enseignant|administratif|commerce|Intérim|particuliers|université|Baby-sitter|hôpital") == FALSE |
               pre_processed_data$`De quel type d'activité rémunérée s'agissait-il ?` == "Autre",
             yes = "Autre_travail",
             no = "Non"))



#Concernant vos activités rémunérées, diriez-vous qu’elles......41	####

# Processing ####
processed_data <-
  pre_processed_data %>% 
  select(- `Au cours de ce semestre, de quelle(s) source(s) de financement bénéficiez-vous ?`) %>%
  select(- `Le fait de ne pas avoir de bourses pendant les 2 mois d’été engendre-t-il une de situations suivantes ?`)	%>%
  select(- `En dehors des bourses sur critères sociaux et des aides d’urgence, quelle(s) aide(s) recevez-vous pour cette année universitaire ?`) 

processed_data %>% view
# Table summary ####
str(pre_processed_data)

processed_data %>% 
  select(- `Quel âge avez-vous ?`) %>%
  select(- `De quelle UFR êtes-vous ?`) %>%
  select(- `Depuis le début de l'année 2022, vous est-il arrivé ...`) %>%
  select(- Epargner:Rien) %>%
  select(- Bourse_mérite:Autres_aides)%>%
  select(-`Au cours de ce semestre, avez-vous bénéficié de Revenu de stage ?` : Aucun_problème) %>%
  select(- Obligation_prêt_étudiant:Aucun_problème) %>% 
  tbl_summary() 

summary(raw_data)

