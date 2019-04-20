# Länderfinanzausgleich

library(tidyverse)

# Datenquelle: https://de.wikipedia.org/wiki/L%C3%A4nderfinanzausgleich
# In Excel bearbeitet (z. B. Dash für negative Zahlen ersetzt ...)

daten <- readxl::read_excel("daten/Länderfinanzausgleich.xlsx",
                   na = c("", "NA", "./."))

daten <- daten %>% 
  mutate(Volumen = str_remove_all(Volumen, "± ")) %>% 
  mutate_if(is.character, str_remove_all, "\\.") %>% 
  mutate_if(is.character, as.numeric)

daten <- daten %>% 
  gather(-Jahr, key = Bundesland, value = Finanzausgleich) %>% 
  arrange(Jahr, desc(Finanzausgleich)) %>% 
  mutate(Typ = ifelse(Finanzausgleich < 0, "Geberland", "Empfängerland")) %>% 
  mutate(Typ = fct_rev(Typ)) %>% 
  mutate(Finanzausgleich = -Finanzausgleich) %>% 
  filter(Bundesland != "Volumen")

# Factor levels in Reihenfolge des 2018er-Finanzausgleichs
reihenfolge <- daten %>%
  filter(Jahr == 2018) %>% 
  transmute(Bundesland = fct_inorder(Bundesland)) 
reihenfolge <- reihenfolge$Bundesland
daten$Bundesland <- fct_relevel(daten$Bundesland, as.character(reihenfolge))
rm(reihenfolge)

daten %>% 
  filter(Jahr == 2018) %>% 
  mutate(Bundesland = fct_inorder(Bundesland)) %>% 
  ggplot(aes(x = Bundesland, y = Finanzausgleich)) +
    geom_bar(stat = "identity", aes(fill = Typ), width = 0.6) +
    labs(y = "Finanzausgleich in Mio. Euro",
         title = "Länderfinanzausgleich 2018",
         caption = "Datenquelle: https://de.wikipedia.org/wiki/Länderfinanzausgleich#Finanzvolumen") +
    coord_flip() +
    scale_fill_manual(name = "", values = c("steelblue1", "peachpuff4")) +
    theme(legend.position = "top",
          text = element_text(size = 14))
  
library(gganimate)

anim <- daten %>% 
  ggplot(aes(x = Bundesland, y = Finanzausgleich)) +
  geom_bar(stat = "identity", aes(fill = Typ), width = 0.6) +
  labs(y = "Finanzausgleich in Mio. Euro",
       title = "Länderfinanzausgleich {closest_state}") +
  coord_flip() +
  scale_fill_manual(name = "", values = c("steelblue1", "peachpuff4")) +
  theme(legend.position = "top",
        text = element_text(size = 16)) +
  transition_states(Jahr, transition_length = 2, state_length = 3)

# Default: 100 frames; 2 frames needed per year
anim <- animate(anim, nframes = 2 * length(unique(daten$Jahr)) + 10, end_pause = 10, fps = 4,
                width = 500, height = 600)
save_animation(anim, file = "Länderfinanzausgleich.gif")

# Benutzerdefinierte Reihenfolge der Bundesländer:
# Nach Zahlen des jeweiligen Jahres sortiert?

# Inspiration:
# https://stackoverflow.com/questions/53162821/animated-sorted-bar-chart-with-bars-overtaking-each-other

anim2 <- daten %>% 
  na.omit() %>% 
  # Reihenfolge, Labels erstellen
  group_by(Jahr) %>% 
  mutate(Rang = rank(-Finanzausgleich, ties.method = "first")) %>% 
  group_by(Bundesland) %>% 
  # Kein Filter: Alle 16 Bundesländer sollen drauf bleiben
  ggplot(aes(x = -Rang, y = Finanzausgleich, fill = Typ)) +
    # geom_col(width = 0.8, position = "identity") +
    geom_tile(aes(y = Finanzausgleich / 2, height = Finanzausgleich, width = 0.9),
              alpha = 0.8, color = NA) +
    coord_flip(clip = "off", expand = FALSE) +
  geom_text(aes(-Rang, y = ifelse(Finanzausgleich > -3000, -5000, +500),
                label = paste0(Bundesland, ": ", Finanzausgleich), hjust = 0), size = 5) +
  # geom_text(aes(-Rang, y = Finanzausgleich, label = Finanzausgleich, hjust = 0)) +
  scale_fill_manual(name = "", values = c("steelblue1", "peachpuff4")) +
  labs(y = "Finanzausgleich in Mio. Euro",
       title = "Länderfinanzausgleich {closest_state}",
       subtitle = "in Mio. Euro",
       caption = paste("Created by WR at", Sys.time())) +
  theme_minimal() +
  theme(legend.position = "top",
        text = element_text(size = 16),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank()) +
  transition_states(Jahr, transition_length = 4, state_length = 1) +
  ease_aes('cubic-in-out')

anim2 <- animate(anim2, nframes = 2 * length(unique(daten$Jahr)) + 10, fps = 20, duration = 60,
        end_pause = 50, width = 600, height = 700)
save_animation(anim2, file = "Länderfinanzausgleich2.gif")


################################################################################
# Farben

farben <- tmaptools::get_brewer_pal("Dark2", n = 16)

animdaten <- daten %>% 
  filter(Jahr > 1980) %>% 
  na.omit() %>% 
  group_by(Jahr) %>% 
  mutate(Rang = rank(-Finanzausgleich, ties.method = "first")) %>% 
  group_by(Bundesland)

anim3 <- animdaten %>% 
  ggplot(aes(x = -Rang, y = Finanzausgleich, fill = Bundesland, color = Bundesland)) +
  geom_tile(aes(y = Finanzausgleich / 2, height = Finanzausgleich, width = 0.9),
            alpha = 0.8, color = NA) +
  annotate("text", x = -10, y = 1500,
           label = paste("Bayern wurde 1989 zum Geberland",
                         "(1992 nochmals Empfänger).\n\n",
                         "1995 wurden die ostdeutschen Bundesländer\n",
                         "Berlin, Brandenburg, Sachsen,\n",
                         "Sachsen-Anhalt und Thüringen in den\n",
                         "Länderfinanzausgleich aufgenommen.\n\n",
                         "Ab dieser Zeit stieg\n",
                         "das Transfervolumen enorm an."),
                          size = 4.5, hjust = 0) +
  coord_flip(clip = "off", expand = TRUE) +
  # coord_flip() +
  geom_text(aes(y = -5000,
                label = paste0(Bundesland, ": ", Finanzausgleich), hjust = 1),
            size = 5) +
  scale_fill_manual(values = farben) +
  scale_color_manual(values = farben) +
  scale_y_continuous(limits = c(-12000, 13000)) +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Länderfinanzausgleich {closest_state}",
       subtitle = "in Mio. Euro",
       caption = paste("Animation von 1981 bis 2018\n",
                       "Created by WR at", Sys.time(), "\n", 
                       "Datenquelle: https://de.wikipedia.org/wiki/Länderfinanzausgleich#Finanzvolumen")) +
  # theme_minimal() +
  theme(text = element_text(size = 16),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  transition_states(Jahr, transition_length = 1, state_length = 1, wrap = FALSE) +
  ease_aes('bounce-in-out')

anim3 <- animate(anim3, fps = 20, end_pause = 50, nframes = 600,
        rewind = FALSE, width = 700, height = 700)
anim3
save_animation(anim3, file = "Länderfinanzausgleich3.gif")

rm(farben)

#######################################################################
# extrafont package; font_import(); loadfonts(device = "win")

# Events definieren: Schnell bis 1990, dann langsamer?

# Test für annotate

daten %>% 
  filter(Jahr == 2018) %>% 
  mutate(Bundesland = fct_inorder(Bundesland)) %>% 
  ggplot(aes(x = Bundesland, y = Finanzausgleich)) +
  geom_bar(stat = "identity", aes(fill = Typ), width = 0.6) +
  labs(y = "Finanzausgleich in Mio. Euro",
       title = "Länderfinanzausgleich 2018") +
  coord_flip() +
  scale_fill_manual(name = "", values = c("steelblue1", "peachpuff4")) +
  annotate("label", x = 8, y = 4000,
           label = "Bayern wurde 1989 zum Geberland.
        1995 wurden die ostdeutschen Bundesländer
        Berlin, Brandenburg, Sachsen, Sachsen-Anhalt
        und Thüringen in den Länderfinanzausgleich aufgenommen.
        Ab dieser Zeit stieg das Transfervolumen enorm an.", size = 4, hjust = 0.5) +
  theme_minimal() +
  theme(legend.position = "top",
        text = element_text(size = 14))


############################################
# Duplicating 1995 to make the animation pause there
# Doesn't work with duplication: gganimate uses factor levels
# So progresses just as fast to 1996 ...
# Try transition_reveal
# Umlaute-Fehler
# Jahr müsste gerundet werden
# Keine Pause 1995!
# 2018er Bild ist leer, wenn keep_last = FALSE
# transition_reveal kann anscheinend nicht mit Umlauten umgehen
# Back to transition_states

farben <- tmaptools::get_brewer_pal("Dark2", n = 16)

y1995_1 <- daten %>% 
  filter(Jahr == 1995) %>% 
  mutate(Jahr = 1995.1,
        Rang = rank(-Finanzausgleich))

y1995_2 <- daten %>% 
  filter(Jahr == 1995) %>% 
  mutate(Jahr = 1995.2,
         Rang = rank(-Finanzausgleich))

y1995_3 <- daten %>% 
  filter(Jahr == 1995) %>% 
  mutate(Jahr = 1995.3,
         Rang = rank(-Finanzausgleich))

y1995_4 <- daten %>% 
  filter(Jahr == 1995) %>% 
  mutate(Jahr = 1995.4,
         Rang = rank(-Finanzausgleich))

animdaten <- daten %>% 
  filter(Jahr > 1987) %>% 
  na.omit() %>% 
  group_by(Jahr) %>% 
  mutate(Rang = rank(-Finanzausgleich, ties.method = "first")) %>% 
  ungroup() %>% 
  bind_rows(y1995_1, y1995_2, y1995_3, y1995_4) %>% 
  # mutate(Bundesland = fct_recode(Bundesland,
  #                           `Baden-Wuerttemberg` = "Baden-Württemberg",
  #                           `Thueringen` = "Thüringen")) %>%
  arrange(Jahr)

anim4 <- animdaten %>% 
  ggplot(aes(x = -Rang, y = Finanzausgleich, fill = Bundesland, color = Bundesland)) +
  geom_tile(aes(y = Finanzausgleich / 2, height = Finanzausgleich, width = 0.9),
            alpha = 0.8, color = NA) +
  annotate("text", x = -10, y = 2000,
           label = paste("Bayern wurde 1989 zum Geberland\n",
                        "(1992 nochmals Empfänger).\n\n",
                        "1995 wurden die ostdeutschen Bundesländer\n",
                        "Berlin, Brandenburg, Sachsen,\n",
                        "Sachsen-Anhalt und Thüringen in den\n",
                        "Länderfinanzausgleich aufgenommen.\n\n",
                        "Ab dieser Zeit stieg\n",
                        "das Transfervolumen deutlich an.\n\n",
                        "Auffällig die starken Bewegungen\n",
                        "Nordrhein-Westfalens zwischen\n",
                        "Rang 1 (1995) und Rang 15 (2017)."),
           size = 4.5, hjust = 0) +
  coord_flip(clip = "off", expand = TRUE) +
  geom_text(aes(y = -5000,
                label = paste0(Bundesland, ": ", Finanzausgleich), hjust = 1,
                fontface = ifelse(Bundesland == "Nordrhein-Westfalen" | 
                                    Bundesland == "Bayern", "bold", "plain")),
            size = 5) +
  scale_fill_manual(values = farben) +
  scale_color_manual(values = farben) +
  scale_y_continuous(limits = c(-12000, 13000)) +
  guides(color = FALSE, fill = FALSE) +
  labs(title = "Länderfinanzausgleich {round(as.numeric(as.character({closest_state})), 0)}",
       subtitle = "in Mio. Euro; positive Werte: Geber; negative Werte: Empfänger",
       caption = paste("Animation von 1988 bis 2018, mit Pause 1995\n",
                       "Erstellt mit R 3.5.3, gganimate 1.0.3, ggplot2 3.1.1,", Sys.Date(), "\n", 
                       "Datenquelle: https://de.wikipedia.org/wiki/Länderfinanzausgleich#Finanzvolumen")) +
  # theme_minimal() +
  theme(text = element_text(size = 16),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  # transition_reveal(along = Jahr, keep_last = TRUE) +
  transition_states(Jahr, transition_length = 1, state_length = 1, wrap = FALSE) +
  ease_aes('bounce-in-out')

anim4 <- animate(anim4, fps = 15, end_pause = 100, nframes = 1000,
                 rewind = FALSE, width = 700, height = 700)
anim4
save_animation(anim4, file = "Länderfinanzausgleich4.gif")

rm(farben, animdaten, y1995_1, y1995_2, y1995_3, y1995_4)
