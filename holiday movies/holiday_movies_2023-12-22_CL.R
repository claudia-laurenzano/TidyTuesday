
# Setup -------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(here)
library(janitor)

library(tidytext)

library(colorspace)
library(scales)
library(glue)

library(ggnewscale)
library(ggstream)
library(ggforce)
library(patchwork)

source(here("..", "..", "Central Data", "theme_BC_CL2.R"))
source(here("..", "..", "Central Data", "colors_BC_CL.R"))

tuesdata <- tidytuesdayR::tt_load('2023-12-12')
holiday_movies <- tuesdata$holiday_movies
holiday_movie_genres <- tuesdata$holiday_movie_genres
# week 50: Christmas movies blog


# Data exploration --------------------------------------------------------


glimpse(holiday_movies)
glimpse(holiday_movie_genres)

holiday_movie_genres %>% 
    drop_na(genres) %>% 
    count(genres) %>% 
    ggplot(aes(y = reorder(genres, n), x = n)) +
    geom_col()

holiday_movies %>% count(holiday)
summary(holiday_movies)

holiday_movies %>% 
    filter(christmas == TRUE) %>%
    unnest_tokens(output = "genre", input = genres, to_lower = FALSE) %>% 
    drop_na(genre) %>% 
    tabyl(genre) %>% 
    arrange(desc(n)) %>% 
    head(10) %>% 
    mutate(percent_sum = sum(percent)) %>% 
    ggplot(aes(y = reorder(genre, n), x = n)) +
    geom_col()

holiday_movies %>% 
    filter(christmas == TRUE) %>% 
    pull(year) %>% 
    summary()



# Data wrangling ----------------------------------------------------------

christmas = holiday_movies %>% 
    filter(christmas == TRUE) %>%
    unnest_tokens(output = "genre", input = genres, to_lower = FALSE) %>% 
    drop_na(genre)

top5 = christmas %>% 
    tabyl(genre) %>% 
    arrange(desc(n)) %>% 
    head(5) %>% 
    mutate(sum = sum(percent))

christmas_filtered = christmas %>% 
    filter(genre %in% top5$genre)

christmas_filtered %>% nrow
christmas_filtered %>% glimpse


christmas_count = christmas_filtered %>% 
    summarize(.by = c(year, genre), 
              n = n())

christmas_rating = christmas_filtered %>% 
    summarize(.by = c(year, genre), 
              rating = mean(average_rating)) %>% 
    mutate(decade = year - year %% 10) %>% 
    # remove outliers
    group_by(genre) %>% 
    filter(!(abs(rating - median(rating)) > 2*sd(rating))) %>% 
    ungroup()

christmas_rating %>% 
    ggplot(aes(x = genre, y = rating, fill = genre)) +
    geom_boxplot() +
    geom_point(data = christmas_rating %>% 
                   summarize(.by = genre, rating = mean(rating, na.rm = T)), 
               size = 3) +
    theme_bc()

christmas_runtime = christmas_filtered %>% 
    summarize(.by = c(year, genre),
              minutes = mean(runtime_minutes)) %>% 
    # remove outliers
    group_by(genre) %>% 
    filter(!(abs(minutes - median(minutes, 
                                  na.rm = TRUE)) > 2*sd(minutes, 
                                                        na.rm = TRUE))) %>% 
    ungroup()

christmas_runtime %>% 
    ggplot(aes(x = genre, y = minutes, fill = genre)) +
    geom_boxplot() +
    geom_point(data = christmas_runtime %>% 
                   summarize(.by = genre, minutes = mean(minutes, na.rm = T)), 
               size = 3) +
    theme_bc()


get_q = function(df, var){
    df %>% 
        summarize(.by = genre, 
                  quants = quantile({{var}}, 
                                    probs = c(0.25, 0.5, 0.75), 
                                    na.rm = TRUE), 
                  mean = mean({{var}}, na.rm = TRUE)) %>% 
        mutate(q = rep(c(1, 2, 3), 5)) %>% 
        pivot_wider(names_from = q, values_from = quants, names_prefix = "q") %>% 
        left_join(df %>% 
                      summarize(.by = genre,
                                iqr = IQR({{var}}, na.rm = TRUE))) %>% 
        left_join(df %>% 
                      summarize(.by = genre,
                                min = min({{var}}, na.rm = TRUE), 
                                max = max({{var}}, na.rm = TRUE))
        ) %>% 
        mutate(range = max - min, 
               rad = 0.5*range)
}

rating_q = get_q(christmas_rating, rating)
runtime_q = get_q(christmas_runtime, minutes)

christmas_runtime %>% 
    summarize(.by = genre, 
              quants = quantile(minutes, 
                                probs = c(0.25, 0.5, 0.75), 
                                na.rm = TRUE), 
              mean = mean(minutes, na.rm = TRUE)) %>% 
    mutate(q = rep(c(1, 2, 3), 5)) %>% 
    pivot_wider(id_cols = mean, 
                names_from = q, 
                values_from = 
                    quants, names_prefix = "q") %>% 
    left_join(df %>% 
                  summarize(.by = genre,
                            iqr = IQR(minutes, na.rm = TRUE))) %>% 
    left_join(df %>% 
                  summarize(.by = genre,
                            min = min(minutes, na.rm = TRUE), 
                            max = max(minutes, na.rm = TRUE))
    ) %>% 
    mutate(range = max - min, 
           rad = 0.5*range)



# Theme -------------------------------------------------------------------

## Fonts -----------------------------------------------------------------

# sans serif
font_add_google("Josefin Sans", family = "Josefin Sans")
font_add_google("Montserrat", family = "Montserrat")

# serif
font_add_google("Yeseva One", family = "Yeseva One")

showtext_auto()


## Colors ----------------------------------------------------------------

xcols = c("#034f1b", "#e6dcb1", "#ceac5c", "#bd3634", "#7e121d")

monochromeR::generate_palette(
    "#034f1b", blend_colour = "black", n_colours = 5, view_palette = TRUE)

dark_text1 = "#011F0A" %>% lighten(0.2)
light_text1 = lighten("#011F0A", 0.4)

show_col(c(dark_text1, light_text1))


## Theme -----------------------------------------------------------------

theme_xmas = theme_bc(title_font = "Yeseva One", 
                   base_font = "Josefin Sans",
                   # base_font = "Montserrat", 
                   dark_text = dark_text1, 
                   light_text = light_text1) +
              theme(
                  plot.title = ggtext::element_textbox_simple(
                      color = "#bd3634"
                      ), 
                  axis.line = element_line(color = light_text1), 
                  axis.text.y = element_blank(), 
                  axis.ticks.y = element_blank(), 
                  axis.line.y = element_blank()
                  )

theme_set(theme_xmas)



# Plot --------------------------------------------------------------------

title = "The top 5 genres of Christmas movies over the years"

subtitle = glue(
    "Christmas movie production dramatically increased over time, especially ",
    "<span style='color:{xcols[5]}'>**Romance**</span> movies. ",
    "<span style='color:{xcols[2]}'>**Comedies**</span> are the most common ",
    "Christmas movie genre overall. ",
    "The ornaments show average rating and run time. The solid line ",
    "represents the median and the ribbon shows the ",
    "upper (75%) and lower (25%) quartiles, while ",
    "the ornament size reflects the range. ",
    "<span style='color:{xcols[1]}'>**Animation**</span> movies are rated ",
    "highest on average (6.6), and have the shortest run time (42 min). ",
    "Movies can have more than one genre.")

# with snow
subtitle1 = glue(
    "Christmas movie production dramatically increased over time, especially ",
    "<span style='color:{xcols[5]}'>**Romance**</span> movies. ",
    "<span style='color:{xcols[2]}'>**Comedies**</span> are the most common ",
    "Christmas movie genre overall. ",
    "The ornaments show average rating and run time. The solid line ",
    "represents the median and the ribbon shows the ",
    "upper (75%) and lower (25%) quartiles, while ",
    "the ornament size reflects the range. The snow shows the data. ",
    "<span style='color:{xcols[1]}'>**Animation**</span> movies are rated ",
    "highest on average (6.6), and have the shortest run time (42 min). ",
    "Movies can have more than one genre.")

caption = glue("Source: IMDb | ",
               "N = {christmas_filtered %>% nrow %>% number(big.mark = ',')} ",
               "| #TidyTuesday week 50, ",
               "{'2023-12-12' %>% month(label = T)} ",
               "{'2023-12-12' %>% day()}, ",
               "{'2023-12-12' %>% year()} ",
               "| Visualization by Claudia Laurenzano ", 
               "| Created in R")



## movie count -----------------------------------------------------------

movie_count = christmas_count %>% 
    ggplot(aes(year, n, color = genre, fill = genre)) +
    geom_stream(
        geom = "polygon", 
        bw = 0.1, 
        size = 0
    ) +
    geom_vline(
        data = tibble(x = seq(1940, 2020, by = 10)), 
        aes(xintercept = x), 
        inherit.aes = FALSE, 
        color = light_gray, 
        size = 0.5, 
        linetype = "dotted"
    ) +
    
    labs(x = NULL, 
         y = NULL, 
         subtitle = "Number of movies over time") +
    scale_fill_manual(values = xcols) +
    scale_color_manual(values = xcols) +
    scale_x_continuous(breaks = pretty_breaks(7)) +
    theme(legend.position = "none", 
          plot.subtitle = ggtext::element_textbox_simple(halign = 0.94))



## movie ratings ---------------------------------------------------------



movie_ratings = rating_q %>% ggplot() +
    geom_linerange(aes(ymin = 0.9*max, ymax = 9,
                       x = 0),
                   color = light_text1) +
    geom_circle(aes(x0 = 0, y0 = mean, r = rad,
                    color = genre, fill = genre)) +
    geom_rect(aes(xmin = -0.99*rad, xmax = 0.99*rad,
                  ymin = q1, ymax = q3),
              fill = "white", alpha = 0.5) +
    geom_linerange(aes(xmin = -rad, xmax = rad,
                       y = q2),
                   color = "white", size = 1) +
    geom_jitter(data = christmas_rating,
               aes(x = 0, y = rating),
               shape = 8, size = 1, color = "white", alpha = 0.5, width = 0.6) +
    scale_fill_manual(values = xcols) +
    scale_color_manual(values = xcols) +
    geom_text(aes(x = 0, y = q2, 
                  label = q2 %>% number(accuracy = 0.1)), 
              hjust = 0.5, 
              vjust = -0.1,
              color = dark_text,
              family = "Josefin Sans") +
    facet_wrap( ~ genre, nrow = 1) +
    coord_fixed() +
    labs(y = NULL, 
         x = NULL, 
         subtitle = "Average movie rating") + 
    theme(
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none"
    )


## movie run time --------------------------------------------------------

movie_runtime = runtime_q %>% ggplot() +
        geom_linerange(aes(ymin = 0.9*max, ymax = 140,
                           x = 0),
                       color = light_text1) +
        geom_circle(aes(x0 = 0, y0 = mean, r = rad,
                        color = genre, fill = genre)) +
        geom_rect(aes(xmin = -0.99*rad, xmax = 0.99*rad,
                      ymin = q1, ymax = q3),
            fill = "white", alpha = 0.5) +
        geom_linerange(aes(xmin = -rad, xmax = rad,
                           y = q2),
                       color = "white", size = 1) +
        geom_jitter(data = christmas_runtime,
                aes(x = 0, y = minutes),
                shape = 8, size = 1, color = "white", alpha = 0.5, width = 12) +
        geom_text(aes(x = 0, y = q2, 
                      label = paste(q2 %>% number(accuracy = 2L), "min")), 
                  hjust = 0.5, 
                  vjust = -0.1,
                  color = dark_text, 
                  family = "Josefin Sans") +
        scale_fill_manual(values = xcols) +
        scale_color_manual(values = xcols) +
        facet_wrap( ~ genre, nrow = 1) +
        coord_fixed() +
        labs(y = NULL, 
             x = NULL, 
             subtitle = "Average movie length") +
        theme(
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "none", 
            strip.background = element_blank(), 
            strip.text = element_blank()
        )



## patchwork -------------------------------------------------------------

movie_count + 
    inset_element(movie_ratings, 
                  0, 0.5, 0.75, 1) +
    inset_element(movie_runtime, 
                  0, 0, 0.75, 0.5) +
    plot_annotation(title = title, 
                    subtitle = subtitle, 
                    caption = caption, 
                    theme = theme_xmas + 
                        theme(
                            plot.title = ggtext::element_textbox_simple(
                                margin = margin(0, 0, 12, 0))))

ggsave(here("holiday movies", "holiday_movies_CL.svg"), last_plot(), 
       bg = lighten("#a7b59b", 0.25), height = 9, width = 9)

# with snow
movie_count + 
    inset_element(movie_ratings, 
                  0, 0.5, 0.75, 1) +
    inset_element(movie_runtime, 
                  0, 0, 0.75, 0.5) +
    plot_annotation(title = title, 
                    subtitle = subtitle1, 
                    caption = caption, 
                    theme = theme_xmas + 
                        theme(
                            plot.title = ggtext::element_textbox_simple(
                                margin = margin(0, 0, 12, 0))))

ggsave(here("holiday movies", "holiday_movies_snow_CL.svg"), last_plot(), 
       bg = lighten("#a7b59b", 0.25), height = 9, width = 9)
