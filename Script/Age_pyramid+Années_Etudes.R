# Pyramide ####
test <-
  pre_processed_data %>% filter(`Vous êtes :` != "Autre")

apyramid::age_pyramid(data = test,
                      age_group = `Quel âge avez-vous ?`,
                      split_by = `Vous êtes :`) +
  my_theme +
  theme(axis.line.y = element_line(color = "grey90"))

apyramid::age_pyramid(data = test,
                      age_group = `Quel âge avez-vous ?`,
                      split_by = `Quelle est votre année d'étude ?`) +
  my_theme +
  theme(axis.line.y = element_line(color = "grey90"))

# Années ####
pre_processed_data %>%
      dplyr::select(`Quelle est votre année d'étude ?`, 
                    `Êtes-vous externe pour l'année 2022-2023`) %>% 
  group_by(`Quelle est votre année d'étude ?`, 
           `Êtes-vous externe pour l'année 2022-2023`) %>%
  summarise(n = n()) %>% 
  ggplot(aes(x = n,
             y = `Êtes-vous externe pour l'année 2022-2023`,
             fill = `Quelle est votre année d'étude ?`, 
             label = n)) +
  coord_flip() +
  geom_col() + 
  geom_text(color = "white",
            position = position_stack(vjust = 0.5),
            fontface = "bold") +
  my_theme

# REGARDER POUR FAIRE UN TRUC SUR LES FACULTES
