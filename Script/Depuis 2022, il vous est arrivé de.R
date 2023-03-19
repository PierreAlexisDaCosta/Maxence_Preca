pacman::p_load(ggupset)
pacman::p_load(UpSetR)


Depuis2022 <-
  pre_processed_data %>%
  select(Epargner:Rien)

Depuis2022 %>% 
  tbl_summary

Depuis2022_1 <- 
  Depuis2022 %>% 
  # convert the "yes" and "no" values into 1s and 0s
  mutate(Epargner = 
           ifelse(Epargner == "Epargner", 1, 0), 
         Découvert_banque	 = 
           ifelse(Découvert_banque == "Découvert à la banque", 1, 0),
         Emprunt_banque = 
           ifelse(Emprunt_banque == "D'emprunter à la banque", 
                  yes = 1, 
                  no = 0),
         Demande_aide_famille = 
           ifelse(Demande_aide_famille == "De demander une aide exceptionnelle à votre famille", 1, 0),
         Refus_prêt = 
           ifelse(Refus_prêt == "Refus de prêt", 1, 0),
         Demande_aide_amis = 
           ifelse(Demande_aide_amis == "De demander une aide exceptionnelle à vos amis", 1, 0),
         Restriction_activités	 = 
           ifelse(Restriction_activités	 == "De vous restreindre dans vos activités sociales", 1, 0),
         Piocher_économies = 
           ifelse(Piocher_économies == "De piocher dans vos économies", 1, 0),
         Demande_aide_sociale = 
           ifelse(Demande_aide_sociale == "De demander une aide sociale exceptionnelle", 1, 0),
         Travailler = 
           ifelse(Travailler == "De vous mettre à travailler", 1, 0),
         Renoncer_soins = 
           ifelse(Renoncer_soins == "Renoncer à des soins", 1, 0)#,
         # Rien = 
         #   ifelse(Rien == "Rien de cela", 1, 0)
  )


Depuis2022_1 %>%
  as.data.frame() %>%
  UpSetR::upset(nset = 11,
                nintersects = 15,
                order.by = "freq",
                empty.intersections = "on",  
                number.angles = 0,
                point.size = 3.5,
                line.size = 2, 
                mainbar.y.label = "Difficultés Combinaison",
                sets.x.label = "Etudiant avec difficultés")




