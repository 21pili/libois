#Préambule
source("0_aux.R")

# Préparer les données en format long pour ggplot
df_long <- score %>%
  select(op, elec, phone, index,  scaste, scaste2, sgen, me2, me3, fe2, fe3, dms14, dms15, dms19, dmaq3, dmaq2, dmph4, dmph5, dmph5, dmph6, dmph7) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques en barres
ggplot(df_long, aes(x = factor(value))) +
  geom_bar(fill = "blue", alpha = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 4) + # Organiser en grille avec 3 colonnes
  labs(title = "Distribution of Discrete Variables", x = "", y = "Observations") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )
ggsave("OUTPUT/MEDIA/score_bar.png", plot = last_plot(), width = 8, height = 10, dpi = 300)

# Préparer les données en format long pour ggplot
df_long <- score %>%
  select(totpop, lon, lat, rain, lit, frsc, frobc, gpdistroad, density, meanscore, score, studentattendance, tepupr) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Créer les graphiques de densité
ggplot(df_long, aes(x = value)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol = 4) + # Organiser en une grille avec 3 colonnes
  labs(title = "Densities of Continuous Variables", x = "", y = "Density") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white")) +
  theme(
    strip.text = element_text(size = 10, face = "bold"), # Style des titres des facettes
    plot.title = element_text(hjust = 0.5, face = "bold") # Centrer le titre
  )
ggsave("OUTPUT/MEDIA/score_density.png", plot = last_plot(), width = 8, height = 10, dpi = 300)
