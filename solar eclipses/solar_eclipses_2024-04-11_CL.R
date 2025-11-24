# Setup -------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(here)
library(janitor)

library(tigris)
library(sf)
library(ggnewscale)
library(glue)
library(gggibbous)
library(patchwork)

source(here("..", "..", "Central Data", "theme_BC_CL2.R"))
source(here("..", "..", "Central Data", "colors_BC_CL.R"))

tuesdata <- tidytuesdayR::tt_load('2024-04-09')

annular_2023 <- tuesdata$eclipse_annular_2023
total_2024 <- tuesdata$eclipse_total_2024
partial_2023 <- tuesdata$eclipse_partial_2023
partial_2024 <- tuesdata$eclipse_partial_2024
# week 15: Solar eclipses



# EXPLORATION -------------------------------------------------------------

# sure 
states(resolution = "20m") %>% 
    filter(!STUSPS %in% c("AK", "HI", "PR", 
                          "VI", "MP", "GU", "AS")) %>% 
    ggplot() + 
    geom_sf() +
    geom_sf(data = total_2024 %>% 
                st_as_sf(coords = c("lon", "lat"), crs = 4326)) +
    geom_sf(data = annular_2023 %>% 
                st_as_sf(coords = c("lon", "lat"), crs = 4326))

# meh
states(resolution = "20m") %>% 
    filter(!STUSPS %in% c("AK", "HI", "PR", 
                          "VI", "MP", "GU", "AS"))  %>% 
    ggplot() + 
    geom_sf() +
    geom_sf(data = partial_2024 %>%
                st_as_sf(coords = c("lon", "lat"), crs = 4326)) +
    geom_sf(data = partial_2023 %>% 
                st_as_sf(coords = c("lon", "lat"), crs = 4326))



# DATA WRANGLING ----------------------------------------------------------


## spatial ---------------------------------------------------------------

usa = states(resolution = "20m") %>% 
    filter(!STUSPS %in% c("AK", "HI", "PR", 
                          "VI", "MP", "GU", "AS"))

path_total = total_2024 %>% 
    mutate(year = 2024) %>% 
    full_join(annular_2023 %>% 
                  mutate(year = 2023)) %>% 
    mutate(dur_full_s = eclipse_4 - eclipse_3,
           dur_full_mins = as.numeric(dur_full_s)/60, 
           begin = as.POSIXct(eclipse_1), 
           begin_full = as.POSIXct(eclipse_3)) %>% 
    mutate(city = name, 
           name = str_c(city, state, sep = " ")) %>%
    relocate(city) %>% 
    left_join(
        maps::us.cities %>% select(name, pop, capital)
    ) %>% 
    mutate(lon1 = lon, lat1 = lat) %>% 
    st_as_sf(coords = c("lon1", "lat1"), crs = 4326)


path24 = path_total %>% filter(year == 2024)
path23 = path_total %>% filter(year == 2023)



## biggest cities --------------------------------------------------------

cities = path_total %>% 
    filter(.by = c(year, state), 
           pop == max(pop, na.rm = TRUE))

cities_long = cities %>% pivot_longer(
    cols = starts_with("eclipse_"), 
    names_to = "period",
    values_to = "time"
    ) %>% 
    select(name, lat, year, dur_full_mins, period, time) %>% 
    arrange(year, name) %>%
    mutate(.by = c(year, name),
           duration_s = time - lag(time),
           mid = time - 0.5 * (time - lag(time))) %>% 
    mutate(duration = seconds_to_period(duration_s))

moons = cities_long %>% 
    filter(period != "eclipse_3") %>% 
    mutate(time = if_else(period == "eclipse_4", mid, time),
           ratio = case_when(str_detect(period, "1") ~ 0.1, 
                             str_detect(period, "2") ~ 0.5,
                             str_detect(period, "3") ~ 1,
                             str_detect(period, "4") ~ 1,
                             str_detect(period, "5") ~ 0.5,
                             str_detect(period, "6") ~ 0.1),
           right = case_when(str_detect(period, "1") ~ "right", 
                             str_detect(period, "2") ~ "right",
                             str_detect(period, "3") ~ "right",
                             str_detect(period, "4") ~ "left",
                             str_detect(period, "5") ~ "left",
                             str_detect(period, "6") ~ "left"))

cities_long %>% 
    mutate(hour = format(round(time, units = "hours"), format = "%H:%M")) %>% glimpse

# THEME -------------------------------------------------------------------

fill_col = misc_pal$charcoal
text_light = misc_pal$sand
text_dark = misc_pal$mustard
line_col = lighten(light_text, 0.2)

theme_eclipse = theme(
    plot.background = element_rect(fill = fill_col, 
                                   color = NA),
    legend.background = element_rect(fill = fill_col),
    legend.title = element_text(
        color = text_dark
    ),
    text = element_text(color = text_light), 
    strip.text = element_text(color = text_dark)
    )

theme_set(theme_bc(void = TRUE) + theme_eclipse)
            


# TEXT --------------------------------------------------------------------

text1 = glue(
    "An **Annular** solar eclipse is different than **Totality** ",
    "in that it occurs when the Moon is closer to its maximum distance ",
    "from Earth in its orbit. If an eclipse happens during this situation, ",
    "the Moon will appear visually smaller than the Sun and its shadow cast ",
    "will not be long enough to reach Earth. ",
    "-- National Weather Service"
    )

title = glue(
    "**Annular and total eclipse events across U.S. cities in ",
    "2023 and 2024** ")

caption = glue(
    "Source: NASA's Scientific Visualization Studio ",
    "| #TidyTuesday week 15, {format(ymd('2024-04-09'), format = '%b %d, %y')} ",
    "Visualization by Claudia Laurenzano ", 
    "| Created in R"
)

# PLOT --------------------------------------------------------------------

plot_path_time = function(time_col, sub = subtitle){
    usa %>% 
        ggplot() + 
        geom_sf(fill = fill_col, 
                color = line_col) +
        geom_sf(data = path24, 
                aes(color = {{time_col}}),
                alpha = 0.4,
                size = 1) +
        scale_color_stepsn(
            name = "2024",
            colors = viridis_pal(direction = -1, option = "rocket")(8),
            trans = "time") +
        new_scale_color() +
        geom_sf(data = path23, 
                aes(color = {{time_col}}),
                alpha = 0.4, 
                size = 1) +
        scale_color_stepsn(
            name = "2023",
            colors = viridis_pal(direction = -1, option = "mako")(8),
            trans = "time") +
        geom_sf(data = cities,
                shape = 21, 
                color = "#FFFBF0", 
                fill = NA) +
        geom_sf_text(data = cities,
                     aes(label = name), 
                     color = "#FFFBF0", 
                     vjust = -0.2, 
                     family = text_sans) + 
        scale_alpha(range = c(0.1, 0.5)) +
        labs(
            subtitle = sub
        ) +
        theme(
            legend.key.width = unit(4, "lines"), 
            legend.key.height = unit(0.6, "lines"),
        )
}

plot_path_period = function(time_col, sub = subtitle){
    usa %>% 
        ggplot() + 
        geom_sf(fill = fill_col, 
                color = line_col) +
        geom_sf(data = path24, 
                aes(color = {{time_col}}),
                size = 1,
                alpha = 0.4) +
        scale_color_stepsn(
            name = "2024",
            colors = viridis_pal(direction = 1, option = "rocket")(8)) +
        new_scale_color() +
        geom_sf(data = path23, 
                aes(color = {{time_col}}),
                size = 1,
                alpha = 0.4) +
        scale_color_stepsn(
            name = "2023",
            colors = viridis_pal(direction = 1, option = "mako")(8)) +
        geom_sf(data = cities,
                shape = 21, 
                color = "#FFFBF0", 
                fill = NA) +
        geom_sf_text(data = cities,
                     aes(label = name), 
                     color = "#FFFBF0", 
                     vjust = -0.2, 
                     family = text_sans) + scale_alpha(range = c(0.1, 0.5)) +
        labs(
            subtitle = sub
        ) +
        theme(
            legend.key.width = unit(4, "lines"), 
            legend.key.height = unit(0.6, "lines"),
        )
}


## paths -----------------------------------------------------------------

subtitle = glue(
    "**US cities experiencing annular and total eclipse events in ",
    "2023 and 2024, respecively:** ",
    "Time of day at which the moon first contacts the sun.")

path_begin = plot_path_time(begin)

subtitle = glue(
    "**Time of day at which annularity (2023) and totality (2024) begin.**"
)

path_full = plot_path_time(begin_full)

subtitle = glue(
    "Duration of full annularity (2023) and totality (2024) in minutes."
)

path_duration = plot_path_period(dur_full_mins)


## duration 2024 ---------------------------------------------------------

vir_cols = viridis_pal(option = "rocket")(3)
w = 2
moon = 5
mn_y = -0.3
subtitle = glue(
    "**Total eclipse in 2024**: ",
    "Time stamps ", 
    "<span style='color:{vir_cols[3]}'>between the moon's first contact ",
    "with the sun and 50% eclipse</span>, ",
    "<span style='color:{lighten(vir_cols[2], 0.2)}'>between 50% eclipse and totality</span>, ",
    "<span style='color:{vir_cols[1]}'>totality</span>, ",
    "<span style='color:{lighten(vir_cols[2], 0.2)}'>between totality and 50% eclipse</span>, ",
    "and ",
    "<span style='color:{vir_cols[3]}'>between 50% eclipse ",
    "and the moon's last contact with the sun and 50% eclipse</span>, ",
    "for different major U.S. cities in 2024."
)

cities_timeline = cities %>% 
    mutate(across(starts_with("eclipse_"), 
                  hms)) %>% 
    filter(year == 2024) %>% 
    ggplot(aes(y = reorder(name, lat))) +
    geom_linerange(aes(xmin = eclipse_1, xmax = eclipse_2), 
                   color = vir_cols[3], 
                   size = w) +
    geom_linerange(aes(xmin = eclipse_2, xmax = eclipse_3), 
                   color = vir_cols[2], 
                   size = w) +
    geom_linerange(aes(xmin = eclipse_3, xmax = eclipse_4),
                   color = vir_cols[1], 
                   size = w) +
    geom_linerange(aes(xmin = eclipse_4, xmax = eclipse_5), 
                   color = vir_cols[2], 
                   size = w) +
    geom_linerange(aes(xmin = eclipse_5, xmax = eclipse_6), 
                   color = vir_cols[3], 
                   size = w) +
    
    geom_text(data = cities_long %>% 
                  filter(year == 2024, 
                         period %in% c("eclipse_1", "eclipse_6")), 
              aes(x = time, label = time), 
              vjust = 1.8, 
              color = text_dark, 
              family = text_serif) +
    geom_text(data = cities_long %>% 
                  filter(year == 2024), 
              aes(x = mid, label = duration %>% str_to_lower), 
              vjust = -0.8, 
              color = text_light, 
              family = text_sans) +
    geom_text(aes(x = eclipse_1, 
                  label = name), 
              hjust = 0.7,
              vjust = -0.8, 
              color = text_light, 
              family = text_sans) +
    
    geom_moon(data = moons %>% filter(year == 2024),
              aes(x = time),
              size = moon,
              position = position_nudge(y = mn_y),
              ratio = 1, fill = text_light) +
    geom_moon(data = moons %>% filter(year == 2024, right == "left"),
              aes(x = time, 
                  ratio = ratio),
              fill = vir_cols[1],
              right = FALSE,
              size = moon,
              position = position_nudge(y = mn_y)) +
    geom_moon(data = moons %>% filter(year == 2024, right == "right"),
              aes(x = time, 
                  ratio = ratio),
              fill = vir_cols[1],
              right = TRUE,
              size = moon,
              position = position_nudge(y = mn_y)) +
    
    labs(subtitle = subtitle) +

    theme(
        panel.grid.major.x = element_line(linetype = "dotted", 
                                          color = line_col), 
        plot.margin = margin(12, 24, 12, 24), 
        axis.text.x = element_text()
    )
    


## patchwork -------------------------------------------------------------

# wrap_plots(path_begin, path_full, path_duration, cities_timeline)

wrap_plots(path_full, cities_timeline, 
           nrow = 2, 
           heights = c(0.9, 1.1)) +
    plot_annotation(title = title, 
                    subtitle = text1, 
                    caption = caption, 
                    theme = theme(
                        plot.title = element_textbox_simple(
                            color = text_dark,
                            margin = margin(12, 0, 24, 0)
                        )
                    ))

ggsave(here("solar eclipses", "solar_eclipses_CL.svg"), plot = last_plot(),
       bg = fill_col, height = 18, width = 12)
