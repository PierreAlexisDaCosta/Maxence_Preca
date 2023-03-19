# Theme ####
my_theme <- theme( 
  panel.background = element_rect(fill = "white"),
  axis.line = element_line(color = "grey90"),
  strip.background = element_rect(fill = "grey30"),
  strip.text = element_text(colour = "white"),
  text = element_text(face = "bold")
)

?theme
# Colour ####

histology_color <- 
  c("adenocarcinoma" = "#c2be4a","squamous_cell_carcinoma" = "#de9a33", "other_histological_types" = "#94160d" )
