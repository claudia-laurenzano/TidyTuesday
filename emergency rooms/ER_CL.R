# SETUP -------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(here)
library(janitor)
library(blancocentR)
library(sf)
# library(geofacet)
# library(ggbeeswarm)
library(tigris)
library(tidycensus)
library(scales)
library(geofacet)
library(patchwork)
library(glue)
library(colorspace)
library(ggdist)

# tidy tuesday week 14, 2025-04-08

tuesdata <- tidytuesdayR::tt_load(2025, week = 14)
care_state <- tuesdata$care_state


# EXPLORE -----------------------------------------------------------------

care_state %>% glimpse
care_state %>% tabyl(condition) %>% arrange(-n)
care_state %>%
  filter(condition == "Healthcare Personnel Vaccination") %>%
  distinct(measure_id, measure_name)
care_state %>% pull(score) %>% summary
care_state %>% pull(start_date) %>% range
care_state %>% pull(end_date) %>% range
care_state %>% tabyl(end_date)
care_state %>% tabyl(start_date)
care_state %>% distinct(state)

vaccines = care_state %>%
  filter(condition == "Healthcare Personnel Vaccination") %>%
  mutate(
    vaccine = if_else(
      str_detect(measure_name, "COVID"),
      "COVID-19",
      "Influenza"
    )
  ) %>%
  summarize(.by = c(state, vaccine), percent = score)

vaccines %>%
  ggplot(aes(x = vaccine, y = 1, fill = percent)) +
  facet_geo(~state) +
  gggibbous::geom_moon(aes(ratio = 1), fill = "white", color = light_text) +
  gggibbous::geom_moon(aes(ratio = percent / 100), color = NA) +
  # gggibbous::geom_moon(data = vaccines %>% filter(vaccine == "COVID-19"),
  #                      aes(ratio = percent/100), color = NA) +
  scale_fill_viridis_c(option = "rocket") +
  ggnewscale::new_scale_fill() +
  # gggibbous::geom_moon(data = vaccines %>% filter(vaccine == "Influenza"),
  #                      aes(ratio = percent/100), color = NA) +
  scale_fill_viridis_c(option = "mako") +
  # geom_col(aes(y = 100), fill = light_gray) +
  # geom_col(alpha = 0.9)  +
  # theme_bc(void = TRUE) +
  theme(plot.background = element_rect(fill = "white"))

time = care_state %>%
  filter(str_detect(measure_id, "OP_18b")) %>%
  distinct(measure_id, measure_name, state, score) %>%
  mutate(
    hours = score %/% 60,
    minutes = score %% 60,
    label = case_when(
      str_detect(measure_id, "b_HIGH_MIN") ~ "high",
      str_detect(measure_id, "LOW") ~ "low",
      str_detect(measure_id, "MEDIUM") ~ "medium",
      str_detect(measure_id, "VERY_HIGH") ~ "very high",
      .default = "median"
    ),
    label = factor(
      label,
      levels = c("median", "low", "medium", "high", "very high")
    )
  )


# DATA WRANGLING ----------------------------------------------------------

## states ----------------------------------------------------------------

us_states = states() %>%
  filter(GEOID < "60") %>%
  shift_geometry() %>%
  mutate(center = st_centroid(geometry))

states_pop = get_decennial(
  geography = "state",
  variables = "P1_001N",
  year = 2020,
  geometry = TRUE
) %>%
  filter(GEOID < "60") %>%
  shift_geometry() %>%
  mutate(center = st_centroid(geometry))


## wait time -------------------------------------------------------------

time = care_state %>%
  filter(measure_id == "OP_18b") %>%
  mutate(
    hours = score %/% 60,
    minutes = score %% 60,
    hr_cut = cut(
      score,
      breaks = c(0, 60 * seq(2, 5), Inf),
      labels = c("0-2", "2-3", "3-4", "4-5", "5+")
    )
  ) %>%
  left_join(
    us_states %>%
      clean_names %>%
      as_tibble %>%
      select("state" = stusps, name)
  ) %>%
  left_join(
    states_pop %>%
      clean_names %>%
      select(name, "pop" = value, center)
  ) %>%
  filter(!is.na(pop))

wait_start = time %>%
  slice_min(start_date) %>%
  pull(start_date) %>%
  unique

wait_end = time %>%
  slice_min(end_date) %>%
  pull(end_date) %>%
  unique


## vaccines --------------------------------------------------------------

vaccines_all = care_state %>%
  filter(condition == "Healthcare Personnel Vaccination") %>%
  mutate(
    vaccine = if_else(
      str_detect(measure_name, "COVID"),
      "COVID-19",
      "Influenza"
    )
  )


vaccines = vaccines_all %>%
  summarize(.by = c(state, vaccine), percent = score) %>%
  mutate(level = if_else(vaccine == "COVID-19", 1, 2)) %>%
  expand_grid(label = c("yes", "no")) %>%
  mutate(
    .by = c(state, vaccine),
    percent = if_else(label == "no", 100 - percent[label == "yes"][1], percent)
  ) %>%
  mutate(
    text = if_else(
      label == "yes",
      glue(
        "{vaccine}
                               vaccinated: {number(percent, accuracy = 1L, suffix = '%')}"
      ),
      glue(
        "{vaccine}
                               not vaccinated: {number(percent, accuracy = 1L, suffix = '%')}"
      )
    ),
    ypos = if_else(
      label == "yes",
      0.5 * percent,
      (100 - percent + 0.5 * percent)
    ),
    color = case_when(
      label == "yes" & vaccine == "COVID-19" ~ "c",
      label == "yes" & vaccine == "Influenza" ~ "b",
      .default = "a"
    )
  )

vax_start = vaccines_all %>%
  slice_min(start_date) %>%
  pull(start_date) %>%
  unique

vax_end = vaccines_all %>%
  slice_min(end_date) %>%
  pull(end_date) %>%
  unique

vax_sum = vaccines %>%
  summarize(.by = c(vaccine, label), med = median(percent, na.rm = TRUE))


# PLOT PREP ---------------------------------------------------------------

sysfonts::font_add_google("Orbitron", family = "Orbitron")
sysfonts::font_add_google("Barlow", family = "Barlow")
showtext::showtext_auto()

text_sans = "Barlow"
text_serif = "Orbitron"

er_col = "#B52027"
title_col = col_lighter(er_col, 10)
bg_col = "#333333"

monochromeR::generate_palette(
  "white",
  blend_colour = bg_col,
  n_colours = 5,
  view_palette = TRUE
)

lt_gray = "#D6D6D6"
md_gray = "#ADADAD"
dk_gray = "#848484"
dkr_gray = "#5B5B5B"

theme_er = theme_bc(
  title_font = text_serif,
  base_font = text_sans,
  light_text = lt_gray,
  dark_text = "white",
  base_size = 10,
  void = TRUE
) +
  theme(
    plot.title = ggtext::element_textbox_simple(color = title_col),
    plot.background = element_rect(fill = bg_col, color = NA),
    legend.background = element_rect(fill = bg_col)
  )

theme_set(theme_er)


colors = c(
  "#F5F8FA", # Fluoro White
  "#A0C4CF", # Sterile Blue
  "#2C7A7B", # Trauma Teal
  "#5E6E72", # Pager Gray
  "#1C2A3A" # Night Shift Navy
)

charcoal_grays <- c(
  TrueCharcoal = "#2A2A2A",
  DeepCharcoal = "#1E1E1E",
  ClassicCharcoal = "#333333",
  BalancedCharcoal = "#262626",
  CoolCharcoal = "#202124"
)

# orbitron
# barlow

title = "Timely and Effective Care in U.S. States"

caption = glue(
  "Source: Centers for Medicare & Medicaid Servies (data.cms.gov) | ",
  "#TidyTuesday week 14, April 2025 | ",
  "Visualization by Claudia Laurenzano | ",
  "Created in R"
)


# WAIT TIME ---------------------------------------------------------------

## text ------------------------------------------------------------------

subtitle = glue(
  "The **average (median) time patients spent in the emergency department** ",
  "before leaving from the visit varies greatly by state. The map shows the ",
  "wait time (circle color) and the population size (circle size) for each state ",
  "between {format(wait_start, '%B %d, %Y')} and {format(wait_end, '%B %d, %Y')}.",
  "The lollipop chart shows the wait time for each state in ascending order. ",
  "In {time %>% slice_min(score) %>% pull(name)}, the wait time is the shortest ",
  "({time %>% slice_min(score) %>% pull(score)} min), while patients in the ",
  "{time %>% slice_max(score) %>% pull(name)} wait the longest ",
  "({time %>% slice_max(score) %>% pull(score)} min)."
)

waittime_text = ggplot() +
  geom_blank() +
  labs(title = title, subtitle = subtitle) +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(4, 0, 16, 0)
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      margin = margin(32, 0, 6, 0)
    )
  )


## map -------------------------------------------------------------------

breaks = seq(0, max(time$score, na.rm = TRUE), 60)

waittime_map = time %>%
  # filter(name == "District of Columbia") %>%
  ggplot() +
  geom_sf(aes(geometry = geometry), fill = NA, color = md_gray) +
  geom_sf(
    aes(geometry = center, size = pop, fill = score / 60),
    shape = 21,
    alpha = 0.8,
    color = "white",
    show.legend = c(size = TRUE, fill = TRUE)
  ) +
  scale_fill_gradientn(colors = colors, breaks = breaks / 60) +
  scale_size(
    range = c(3, 10),
    labels = label_number(big.mark = ",", scale_cut = cut_short_scale())
  ) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(size = 7))) +
  labs(fill = "Hours waited", size = "State population") +
  theme(legend.box = "vertical", legend.spacing.y = unit(0, "lines"))


## lollipop --------------------------------------------------------------

waittime_lollipop = time %>%
  ggplot(aes(y = reorder(name, -score), x = score)) +
  geom_vline(xintercept = breaks, color = md_gray, linetype = "dotted") +
  geom_linerange(aes(xmin = 0, xmax = score), color = dk_gray, lwd = 0.2) +
  geom_point(
    aes(fill = score),
    shape = 21,
    color = "white",
    size = 5,
    show.legend = FALSE
  ) +
  scale_fill_gradientn(colors = colors, breaks = breaks / 60) +
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.1)),
    breaks = breaks,
    labels = function(x) paste(x / 60, "hrs")
  ) +
  scale_y_discrete(expand = expansion(mult = c(0.02, 0.02))) +
  theme(axis.text = element_text())


## pw --------------------------------------------------------------------

pw_waittime = wrap_plots(
  ncol = 2,
  wrap_plots(
    nrow = 2,
    heights = c(0.3, 1),
    waittime_text,
    waittime_map
  ),
  free(waittime_lollipop),
  widths = c(1, 1.2)
) +
  plot_annotation(
    caption = caption,
    theme = theme_er
  )

ggsave(
  here("emergency rooms", "ER_wait_CL.svg"),
  plot = pw_waittime,
  bg = bg_col,
  height = 8.5,
  width = 11
)


# VACCINES ----------------------------------------------------------------

## text ------------------------------------------------------------------

subtitle = glue(
  "The **percentage of immunized healthcare staff varies greatly by state and vaccination** ",
  "between {format(vax_start, '%B %d, %Y')} and {format(vax_end, '%B %d, %Y')}.",
  "Most healthcare personnel received an influenza vaccine ",
  # "(median: {vax_sum %>% filter(vaccine == 'Influenza', label == 'yes') %>% pull(med)}%), ",
  ", while only a small portion are up to date with COVID-19 immunization ",
  # "(median: {vax_sum %>% filter(vaccine == 'COVID-19', label == 'yes') %>% pull(med)}%).",
  ". The map shows immunization rates for both vaccines by state: ",
  "the lightblue outer ring represents influenza, ",
  "the teal inner section shows COVID-19. See the closeup in the top-left for details. ",
  "The bottom-left shows the data distribution for each vaccine with median values."
)

vaccines_text = ggplot() +
  geom_blank() +
  labs(title = title, subtitle = subtitle) +
  theme(
    plot.title = ggtext::element_textbox_simple(
      margin = margin(4, 0, 16, 0)
    ),
    plot.subtitle = ggtext::element_textbox_simple(
      margin = margin(32, 0, 6, 0)
    )
  )


## map -------------------------------------------------------------------

vaccines_main = vaccines %>%
  ggplot(aes(x = vaccine, y = percent, fill = color)) +
  facet_geo(~state) +
  geom_col(
    width = 1,
    # color = "gray90",
    color = NA,
    size = 0.25,
    position = position_stack(),
    show.legend = FALSE
  ) +
  scale_fill_manual(
    values = list("c" = "#2C7A7B", "b" = "#A0C4CF", "a" = bg_col)
  ) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  coord_polar(theta = "y")


## inset -----------------------------------------------------------------

vaccines_inset = vaccines %>%
  filter(state == "LA") %>%
  ggplot(aes(x = vaccine, y = percent, fill = color)) +
  facet_wrap(~state, scales = "free") +
  geom_col(
    width = 1,
    # color = "gray90",
    color = NA,
    size = 0.25,
    position = position_stack(),
    show.legend = FALSE
  ) +
  geom_label(
    aes(y = ypos, label = text),
    fill = "white",
    family = text_sans,
    color = dkr_gray,
    size = 3,
    alpha = 0.7
  ) +
  scale_fill_manual(
    values = list("c" = "#2C7A7B", "b" = "#A0C4CF", "a" = bg_col)
  ) +
  scale_x_discrete(breaks = NULL) +
  scale_y_continuous(breaks = NULL) +
  coord_polar(theta = "y") +
  labs(
    # title = title,
    subtitle = "Louisiana healthcare staff vacination rates for influenza (outer ring) and COVID-19 (inner ring)"
  )


## rain clouds -----------------------------------------------------------

vaccines_clouds = vaccines %>%
  filter(label == "yes") %>%
  ggplot(aes(y = fct_rev(vaccine), x = percent)) +
  stat_halfeye(
    aes(color = vaccine, fill = after_scale(lighten(color, 0.5))),
    adjust = 0.5, # 0.5
    # width = 0.75,
    .width = 0,
    justification = -0.2, # -0.4
    point_color = NA
  ) +
  scale_color_manual(
    values = list(
      "COVID-19" = "#2C7A7B",
      "Influenza" = "#A0C4CF"
    )
  ) +
  stat_dots(side = "left", justification = 0.9, fill = "white") +
  stat_dots(
    aes(color = vaccine, fill = after_scale(lighten(color, 0.5))),
    side = "left",
    justification = 0.9
  ) +
  stat_summary(
    geom = "text",
    fun = "median",
    aes(
      label = paste0(round(after_stat(x), 2), "%"),
      color = stage(vaccine, after_scale = darken(color, .3, space = "HLS"))
    ),
    family = text_sans,
    fontface = "bold",
    size = 4.5,
    vjust = -1.5 # -5.5, -2.5
  ) +
  scale_x_continuous(
    expand = expansion(mult = c(0.1, 0.1)),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    subtitle = "Vaccination rate distribution. The median vaccination rate for Influenza is 82.5% compared to only 11.1% for COVID-19."
  ) +
  theme(axis.text = element_text(), legend.position = "none")


## pw --------------------------------------------------------------------

pw_vaccines = wrap_plots(
  ncol = 2,
  widths = c(1, 1.5),
  wrap_plots(
    nrow = 2,
    vaccines_inset,
    vaccines_clouds
  ),
  free(vaccines_main)
) +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = theme_er +
      theme(
        plot.title = ggtext::element_textbox_simple(
          margin = margin(4, 0, 16, 0)
        ),
        plot.subtitle = ggtext::element_textbox_simple(
          margin = margin(0, 0, 24, 0)
        )
      )
  )

ggsave(
  here("emergency rooms", "ER_vax_CL.svg"),
  plot = pw_vaccines,
  bg = bg_col,
  height = 8.5,
  width = 11
)
