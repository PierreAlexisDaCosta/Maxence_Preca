pacman::p_load(ggupset)
pacman::p_load(UpSetR)
require(ggplot2); require(plyr); require(gridExtra); require(grid);

Financement <-
  pre_processed_data %>%
  select(`Au cours de ce semestre, avez-vous bénéficié de Revenu de stage ?` : `Au cours de ce semestre, avez-vous bénéficié de vos Économies, épargnes globalement disponibles sur l'année 2022 ?`)
# Financement %>% tbl_summary

# wit setupR ####
Financement_1 <- 
  Financement %>% 
  # convert the "yes" and "no" values into 1s and 0s
  mutate(`Au cours de ce semestre, avez-vous bénéficié de Revenu de stage ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié de Revenu de stage ?` == "Revenu de stage", 1, 0), 
         `Au cours de ce semestre, avez-vous bénéficié de Revenu d'emploi (hors stage) ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié de Revenu d'emploi (hors stage) ?` == "Revenu d'emploi (hors stage)", 1, 0),
         `Au cours de ce semestre, avez-vous bénéficié d'Aides publiques ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié d'Aides publiques ?` == "Aides publiques (bourse du CROUS, autres bourses, allocations diverses (APL, CAF...))", 
                  yes = 1, 
                  no = 0),
         `Au cours de ce semestre, avez-vous bénéficié du CESP ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié du CESP ?` == "CESP", 1, 0),
         `Au cours de ce semestre, avez-vous bénéficié de Participation de la famille ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié de Participation de la famille ?` == "Participation de la famille", 1, 0),
         `Au cours de ce semestre, avez-vous bénéficié de Participation du partenaire ou conjoint ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié de Participation du partenaire ou conjoint ?` == "Participation du partenaire ou conjoint", 1, 0),
         `Au cours de ce semestre, avez-vous bénéficié d'un Prêt étudiant (public ou privé) sur l'année 2022 ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié d'un Prêt étudiant (public ou privé) sur l'année 2022 ?` == "Prêt étudiant (public ou privé) sur l'année 2022", 1, 0),
         `Au cours de ce semestre, avez-vous bénéficié d'un Prêt de la famille (à rembourser) ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié d'un Prêt de la famille (à rembourser) ?` == "Prêt de la famille (à rembourser)", 1, 0),
         `Au cours de ce semestre, avez-vous bénéficié de vos Économies, épargnes globalement disponibles sur l'année 2022 ?` = 
           ifelse(`Au cours de ce semestre, avez-vous bénéficié de vos Économies, épargnes globalement disponibles sur l'année 2022 ?` == "Économies, épargnes globalement disponibles sur l'année 2022", 1, 0))

Financement_2 <-
  Financement_1 %>% 
  dplyr::rename(Revenu_de_stage = `Au cours de ce semestre, avez-vous bénéficié de Revenu de stage ?`) %>%
  dplyr::rename(Revenu_d_emploi = `Au cours de ce semestre, avez-vous bénéficié de Revenu d'emploi (hors stage) ?`) %>%
  dplyr::rename(Aides_publiques = `Au cours de ce semestre, avez-vous bénéficié d'Aides publiques ?`) %>%
  dplyr::rename(CESP = `Au cours de ce semestre, avez-vous bénéficié du CESP ?`) %>%
  dplyr::rename(Participation_famille = `Au cours de ce semestre, avez-vous bénéficié de Participation de la famille ?` ) %>%
  dplyr::rename(Participation_du_partenaire = `Au cours de ce semestre, avez-vous bénéficié de Participation du partenaire ou conjoint ?`) %>%
  dplyr::rename(Prêt_étudiant = `Au cours de ce semestre, avez-vous bénéficié d'un Prêt étudiant (public ou privé) sur l'année 2022 ?`) %>%
  dplyr::rename(Prêt_famille = `Au cours de ce semestre, avez-vous bénéficié d'un Prêt de la famille (à rembourser) ?`) %>%
  dplyr::rename(Epargne_personnelle = `Au cours de ce semestre, avez-vous bénéficié de vos Économies, épargnes globalement disponibles sur l'année 2022 ?`)

Financement_2 %>%
  as.data.frame() %>%
  UpSetR::upset(nset = 9,
                nintersects = 10,
                order.by = "freq",
                empty.intersections = "on",  
                number.angles = 0,
                point.size = 3.5,
                line.size = 2, 
                mainbar.y.label = "Financement Combinaison",
                sets.x.label = "Etudiant avec Financement")

