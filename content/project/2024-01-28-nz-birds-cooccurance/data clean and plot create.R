library(tidyverse)
library(showtext)

font_add_google("Bitter", "bitter", regular.wt = 400, bold.wt = 700)
showtext_auto()
showtext_opts(dpi = 250)

# graph theme set ####
theme_set(theme_void() +
            theme(plot.background = element_rect(fill = "#F3F3EE", colour = "#F3F3EE"),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                  text = element_text(size = 13, family = "bitter"),
                  plot.title = element_text(family = "bitter", face = "bold", size = 19, margin = margin(b = 5)),
                  plot.title.position = "plot",
                  plot.subtitle = element_text(face = "bold", margin = margin(1,1,10,1)),
                  legend.position = "top",
                  legend.title = element_blank(),
                  legend.text = element_text(face = "bold", margin = margin(l = -0.8, r = 1, unit = "cm")),
                  legend.key.width = unit(2, "cm"),
                  legend.key.height = unit(0.3, "cm"),
                  axis.text.y.right = element_text(size = 0),
                  axis.text.y = element_text(size = 11, hjust = 1, margin = margin(r = 2)),
                  axis.text.x = element_text(size = 9, hjust = 0.5),
                  panel.spacing.y = unit(0.5, "cm"),
                  strip.text = element_text(size = 14, colour = "grey30", family = "bitter", face = "bold", margin = margin(b=3)),
                  strip.background = element_blank()))

DATA <- file.choose() %>%
  read_rds()

RAW <- DATA
DATA <- RAW %>%
  sf::st_drop_geometry() %>%
  filter(observation_year == "2020" & all_species_reported == TRUE) %>%
  select(checklist_id:observation_year) %>%
  mutate(duration_minutes = 60*(observation_count/observation_count_hour))

write.csv(DATA, "ebird_NZ_2020.csv", row.names = FALSE)


pDATA_1 <- DATA %>%
  filter(duration_minutes >= 15 & duration_minutes <= 120 & Status %in% c("Introduced", "Native/Endemic")) %>%
  select(checklist_id,common_name, reo_m, Status, IUCN_status, DOC_conservation_area) %>%
  mutate(DOC_conservation_area = !is.na(DOC_conservation_area))


CHECKLISTS <- pDATA_1 %>%
  group_by(checklist_id) %>%
  summarise(DOC_area = ifelse(sum(DOC_conservation_area) > 0, TRUE, FALSE), .groups = "drop") 

CHECKLISTS_n <- CHECKLISTS %>%
  group_by(DOC_area) %>%
  summarise(n_check = n())


pDATA_1 <- pDATA_1 %>%
  group_by(checklist_id, IUCN_status) %>%
  summarise(Observed = 1) %>%
  ungroup() %>%
  complete(checklist_id, IUCN_status, fill = list(Observed = 0)) %>%
  left_join(., CHECKLISTS, by = "checklist_id") %>%
  group_by(DOC_area, IUCN_status) %>%
  summarise(Prop = mean(Observed), .groups = "drop") %>%
  left_join(., CHECKLISTS_n, by = "DOC_area") %>%
  mutate(SE = sqrt((Prop * (1-Prop))/n_check),
            CI_L = Prop - (1.96*SE),
            CI_U = Prop + (1.96*SE)) %>%
  filter(IUCN_status != "Unknown") %>%
  ungroup()

pDATA_1 <- left_join(
  pDATA_1 %>% filter(DOC_area == TRUE) %>% select(-DOC_area),
  pDATA_1 %>% filter(DOC_area != TRUE) %>% select(-DOC_area),
  by = "IUCN_status") %>%
  mutate(signif = case_when(
    CI_U.x < CI_L.y | CI_L.x > CI_U.y ~ "**",
    TRUE ~ ""
  )) %>%
  select(IUCN_status, signif) %>%
  left_join(., pDATA_1, by = "IUCN_status")

pDATA_1 %>%
  mutate(IUCN_status = factor(IUCN_status, levels = c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", "Least Concern"))) %>%
  ggplot(aes(y = IUCN_status, colour = DOC_area)) +
  geom_segment(aes(yend = IUCN_status, x = CI_L, xend = CI_U), alpha = 0.35, linewidth = 5) +
  geom_point(aes(x = Prop), size = 8, shape = "|") +
  geom_text(data = pDATA_1 %>% filter(DOC_area == TRUE),
            aes(x = Prop, label = paste0(round(100*Prop, 1), "%")), size = 4, vjust = -2.5, hjust = 0, family = "bitter", show.legend = FALSE) +
  geom_text(data = pDATA_1 %>% filter(DOC_area != TRUE),
            aes(x = Prop, label = paste0(round(100*Prop, 1), "%")), size = 4, vjust = 3, hjust = 1, family = "bitter", show.legend = FALSE) +
  geom_text(aes(label = signif, x = 0), size = 8, colour = "grey30", vjust = 0.9, hjust = 1.3, family = "bitter", show.legend = FALSE) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_discrete(limits = rev) +
  scale_color_viridis_d(option = "mako", end = 0.7, begin = 0.3, direction = -1) +
  theme(panel.grid.major.y = element_line(colour = "grey80", linetype = "dashed"),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(face = "bold")) +
  labs(title = "Percent of checklists observing a bird species from each IUCN Status",
       subtitle = "You are significantly more likely to see Endangered and Vulnerable bird species in Conservation areas in Aotearoa.",
       colour = "Department of Conservation Area?",
       x = "Percent of checklists",
       y = NULL,
       caption = str_wrap("Shaded area = 95% confidence interval. ** = confidence intervals do not overlap, indicating likelihood of observing species is different between areas."))


ggsave(filename = "IUCN_status_plot.png", width = 3200, height = 2600, units = "px")

#### co-occurence plots

pDATA_2 <- DATA %>%
  filter(!is.na(DOC_conservation_area)) %>%
  filter(duration_minutes >= 15 & duration_minutes <= 120 & Status %in% c("Introduced", "Native/Endemic")) %>%
  mutate(bird_name = case_when(
    reo_m != common_name ~ paste0(reo_m, " | ", common_name),
    is.na(reo_m) ~ common_name,
    TRUE ~ reo_m
  ))

SPECIES <- pDATA_2 %>%
  select(checklist_id, bird_name)%>%
  mutate(observed = TRUE) %>%
  distinct() %>%
  group_by(bird_name) %>%
  summarise(n = sum(observed)) %>%
  slice_max(n = 15, order_by = n, with_ties = TRUE) %>%
  mutate(bird_name = str_to_title(bird_name) %>% str_wrap(., 20),
         bird_name = fct_reorder(bird_name, n))

OBS <- pDATA_2 %>%
  mutate(bird_name = str_to_title(bird_name) %>% str_wrap(., 20)) %>%
  select(checklist_id, bird_name)%>%
  filter(bird_name %in% SPECIES$bird_name) %>%
  mutate(observed = TRUE) %>%
  distinct() %>%
  complete(checklist_id, bird_name, fill = list(observed = FALSE)) %>%
  pivot_wider(names_from = bird_name, values_from = observed)


pDATA_2 <- OBS %>%
  pivot_longer(cols =-checklist_id) %>%
  left_join(., OBS, by = "checklist_id") %>%
  filter(value == TRUE) %>%
  group_by(name) %>%
  summarise(across(where(is.logical),
                   ~sum(.x, na.rm = TRUE)/n())) %>%
  pivot_longer(cols = c(-name, -value), names_to = "species_with", values_to = "proportion") %>%
  filter(name != species_with) %>%
  rename("checklist_saw" = "name") %>%
  mutate(checklist_saw = factor(checklist_saw, levels = levels(SPECIES$bird_name)),
         species_with = factor(species_with, levels = levels(SPECIES$bird_name)))

pDATA_2 %>%
  ggplot(aes(y = checklist_saw, x = species_with, fill = proportion)) +
  geom_tile() +
  geom_text(aes(label = paste0(100*round(proportion, 2), "%"), colour = proportion), size = 4, family = "bitter") +
  scale_colour_gradient2(high = "white", low = "#17261E", mid = "grey80", na.value = "grey40", midpoint = 0.45) +
  scale_fill_gradient(low = "white", high = "#2F4C3E", na.value = "grey40") +
  guides(colour = "none", fill = "none", alpha = "none") +
  annotate(geom = "text", y = Inf, x = -Inf, label = "Most observed species", family = "bitter", colour = "grey40", size = 3, hjust = -0.1, vjust = 1.5) +
  annotate(geom = "text", y = -Inf, x = Inf, label = "Least\nobserved species", family = "bitter", colour = "grey40", size = 3, hjust = 1, vjust = -0.5) +
  scale_x_discrete(position = "bottom", limits = rev, expand = expansion(add = c(0, 1.5))) +
  scale_y_discrete(position = "left", expand = expansion(add = c(0,1))) +
  labs(title = "Bird species co-occurence plot for most observed species",
       subtitle = "Percent of checklists who saw this species of manu...",
       caption = str_wrap("Includes all checklists from within DOC Conservation areas. Only results for the 10 most observed species are shown. Data: eBird observations from 2020"),
       x = "\n...who also saw this species of manu",
       y = NULL) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0, size = 9, margin = margin(t = 10) ),
        axis.text.y = element_text(face = "plain", size = 10, margin = margin(r=10)),
        axis.title.x = element_text(family = "bitter", face = "bold", size = 12, margin = margin(b = 15)),
        plot.subtitle = element_text(family = "bitter", face = "bold", size = 12),
        axis.line = element_line(arrow = arrow(length = unit(0.15, "in")), colour = "grey40"))

ggsave(filename = "featured.png", width = 3200, height = 2800, units = "px")
   