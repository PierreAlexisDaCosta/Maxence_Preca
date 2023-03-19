pacman::p_load(ggupset)
pacman::p_load(UpSetR)


Type_de_travail <-
  pre_processed_data %>%
  select(Employé_hopîtal:Autre_travail) %>%
  na.omit()

Type_de_travail %>% 
  tbl_summary

Type_de_travail_1 <- 
  Type_de_travail %>% 
  # convert the "yes" and "no" values into 1s and 0s
  mutate(Employé_hopîtal = 
           ifelse(Employé_hopîtal == "Employé d'hôpital", 1, 0), 
         Employé_commerce_ou_restauration	 = 
           ifelse(Employé_commerce_ou_restauration == "commerce", 1, 0),
         Employé_administratif = 
           ifelse(Employé_administratif == "administratif", 
                  yes = 1, 
                  no = 0),
         Enseignant = 
           ifelse(Enseignant == "Enseignant", 1, 0),
         Animateur = 
           ifelse(Animateur == "Animateur", 1, 0),
         Agent_de_service = 
           ifelse(Agent_de_service == "Agent de service", 1, 0),
         Babysitter_aide_à_la_personne	 = 
           ifelse(Babysitter_aide_à_la_personne	 == "Babysitter_aide_à_la_personne", 1, 0),
         Employé_université = 
           ifelse(Employé_université == "Employé_université", 1, 0),
         Cours_particuliers = 
           ifelse(Cours_particuliers == "Cours_particuliers", 1, 0),
         Intérim = 
           ifelse(Intérim == "Intérim", 1, 0),
         Autre_travail = 
           ifelse(Autre_travail == "Autre_travail", 1, 0)
  )


Type_de_travail_1 %>%
  as.data.frame() %>%
  UpSetR::upset(nset = 11,
                nintersects = 15,
                order.by = "freq",
                empty.intersections = "on",  
                number.angles = 0,
                point.size = 3.5,
                line.size = 2, 
                mainbar.y.label = "Travail Combinaison",
                sets.x.label = "Etudiant avec travail")




