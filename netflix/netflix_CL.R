
# SETUP -------------------------------------------------------------------

library(tidyverse)
library(here)
library(blancocentR)
library(scales)
library(colorspace)
library(glue)
library(patchwork)
library(cowplot)
library(ggtext)
library(ggimage)


movies_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/movies.csv')
shows_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-29/shows.csv')


logo_path = here(
    "netflix", "logos", "01_Netflix_Logo", "01_Netflix_Logo_RGB", "Netflix_logo_RGB.png"
)



# DATA WRANGLING ----------------------------------------------------------

movies = movies_raw %>% 
    filter(str_detect(source, "2025Jan-Jun")) %>% 
    mutate(runtime = period(runtime), 
           runtime_mins = hour(runtime) * 60 + minute(runtime),
           year = year(release_date))

df1 = movies %>%
    summarize(.by = year,
              hours_viewed = sum(hours_viewed, na.rm = TRUE),
              n = n(), 
              views = sum(views, na.rm = TRUE))



# EXPLORATION -------------------------------------------------------------

# season_delims = c(": Season", ": Säsong", ": Temporada")

shows = shows_raw %>% 
    mutate(title_raw = title, 
           title = title %>% 
               str_replace(": Season|: Säsong|: Temporada", ": Season"),
           runtime = period(runtime),
           runtime_mins = hour(runtime) * 60 + minute(runtime),
           year = year(release_date)) %>%
    separate_wider_delim(cols = title, 
                         delim = ": Season", 
                         names = c("title", "season"), 
                         too_few = "align_start", 
                         too_many = "merge") %>% 
    relocate(title_raw, .before = title) %>% 
    mutate(
        limited_series = if_else(
            str_detect(title, regex("limited series|miniserie", ignore_case = TRUE)),
            "Limited series",
            NA
        ),
        title = str_remove_all(title, regex("limited series", ignore_case = TRUE)), 
        title = str_remove_all(title, ":"))

shows %>% slice_max(order_by = views, n = 10)
shows %>% slice_max(order_by = hours_viewed, n = 10)

shows %>% 
    summarize(.by = year, n = n()) %>% 
    ggplot(aes(x = year, y = n)) + 
    geom_col()

movies %>% 
    slice_max(order_by = views, n = 10) %>% 
    ggplot(aes(y = reorder(title, hours_viewed), x = hours_viewed, fill = runtime_mins)) +
    geom_col() +
    scale_x_continuous(labels = label_number(scale_cut = cut_short_scale())) 

movies %>% 
    summarize(.by = year, n = n()) %>% 
    ggplot(aes(x = year, y = n)) + 
    geom_col()



# PLOT PREP ---------------------------------------------------------------

## colors ----------------------------------------------------------------

# netflix color palette 
# https://brand.netflix.com/en/assets/logos/

netflix_cols = list(
    "nred" = "#E50914",      # standard red: N, wordmark, vertical bar
    "nblackbg" = "#111111",  # background black
    "nblacktxt" = "#000000", # wordmark when in b/w (text)
    "nwhite" = "#ffffff",    # wordmark when in b/w (text, background)
    "ngray" = "#d9dadc",     # N (shading)
    "ndarkred" = "#B20710",  # N
    # "norange" = "#ff9b2d",   # vertical bar
    "ndarkgray" = "#505256"  # grid lines (darken(netflix_cols$ngray, 0.6))
)

sysfonts::font_add_google("Bebas Neue", family = "Bebas Neue")
sysfonts::font_add_google("Inter", family = "Inter")

showtext::showtext_auto()

# text font
text_logo = "Bebas Neue"
text_copy = "Inter"

base_size = 12

theme_netflix = theme_bc(
    title_font = text_copy,
    base_font = text_copy,
    light_text = "#C6C6C6", # darken(netflix_cols$nwhite, 0.2)
    dark_text = netflix_cols$nwhite,
    base_size = base_size
) +
    theme(
        plot.title = ggtext::element_textbox_simple(
            color = netflix_cols$nwhite, 
            size = base_size * 2, 
            face = "bold",
            margin = margin(4, 0, 8, 0)),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.background = element_rect(fill = "transparent", color = NA),
        # plot.background = element_rect(fill = netflix_cols$nblackbg, color = NA),
        # legend.background = element_rect(fill = netflix_cols$nblackbg, color = NA),
        # panel.border = element_blank(), 
        # panel.spacing = unit(0, "pt"),
        axis.line = element_line(color = "#C6C6C6"), 
        axis.ticks = element_line(color = "#C6C6C6"), 
        axis.text = element_text(color = "#C6C6C6", size = base_size * 0.84), 
        axis.title = element_text(color = "#C6C6C6", size = base_size * 0.917, family = text_copy)
    )

theme_set(theme_netflix)

update_geom_defaults(
    "text",
    list(size = 3.5, family = text_copy, color = "#C6C6C6")
)

create_title = function(title) {
    glue::glue("<span style='color:{netflix_cols$nred}'>\u007C</span> {title}")
}

set.seed(20250902)

caption = glue(
    "Source: Netflix ", 
    "N = {movies %>% filter(!is.na(year)) %>% nrow %>% number(big.mark = ',')} ", 
    "movies (excludes ",
    "{movies %>% filter(is.na(year)) %>% nrow %>% number(big.mark = ',')} ",
    "movies without release date)
    
    #TidyTuesday week 30, ",
    "{format(ymd('2025-07-29'), '%B %d, %Y')} | ",
    "Visualization by Claudia Laurenzano | ",
    "{format(today(), '%B %d, %Y')} | ",
    "Created in R"
)



# PLOTS -------------------------------------------------------------------


## p1 --------------------------------------------------------------------

top_movie = movies %>% 
    slice_max(hours_viewed, n = 1) %>% 
    mutate(hours_viewed_pretty = number(hours_viewed, scale_cut = cut_short_scale()), 
           text = glue(
               "<span style='font-size:12pt'>**{title}**</span><br>",
               "Released: {release_date %>% format('%b %d, %Y')}<br>",
               "Run time: {runtime_mins} min<br>", 
               "Viewed: {hours_viewed_pretty} hours"
           ))

# hours viewed by year, individual movies
p1 = movies %>% 
    ggplot(aes(x = year, y = hours_viewed)) +
    geom_point(data = movies %>% filter(!title %in% top_movie$title),
               color = netflix_cols$nwhite,
               fill = netflix_cols$ndarkred,
               alpha = 0.6,
               size = 3,
               shape = 21,
               position = position_jitter(width = 0.3, height = 0)) +
    geom_point(data = top_movie, 
               color = netflix_cols$nwhite,
               fill = netflix_cols$ndarkred,
               alpha = 1,
               size = 3,
               shape = 21,
               stroke = 1.2,
               position = position_jitter(width = 0.3, height = 0)) +
    ggtext::geom_textbox(
        data = top_movie, 
        position = position_jitter(width = 0.3, height = 0),
        fill = netflix_cols$nblackbg,
        size = 3.5, 
        family = text_copy, 
        color = "#C6C6C6",
        vjust = 0, 
        # width = 0.19,
        aes(y = hours_viewed*1.3,
            label = glue("{text}"))
    ) +
    scale_y_log10(labels = label_number(big.mark = ",",
                                        scale_cut = cut_short_scale()),
                  expand = expansion(mult = c(0.05, 0.2))) +
    scale_x_continuous(
        breaks = seq(2013, 2025, 1), 
        position = "top",
        expand = expansion(mult = c(0.1, 0.2))) +
    labs(subtitle = glue(
        "**Hours viewed by release year:** ",
        "compared to previous years, total view time of individual, viewed ",
        "movies sharply increased for 2024 releases and is highest for ",
        "brandnew movies (log-scaled)"), 
         x = NULL, 
         y = NULL) + 
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(), 
          # axis.text.x = element_blank(), 
          axis.title = element_blank(), 
          panel.grid.major.y = element_line(color = netflix_cols$ndarkgray, 
                                            linetype = "dotted"))


## p2 --------------------------------------------------------------------

# total hours viewed by year
p2 = df1 %>% 
    ggplot(aes(x = year, y = hours_viewed)) +
    geom_col(fill = netflix_cols$ngray, width = 0.61) +
    scale_y_continuous(
        labels = label_number(big.mark = ",", scale_cut = cut_short_scale()), 
        breaks = pretty_breaks(4)
        ) +
    scale_x_continuous(breaks = seq(2013, 2025, 1), 
                       position = "top", 
                       labels = function(x) str_replace(x, "20", "'")) +
    labs(subtitle = glue(
        "**Total hours viewed by release year:** viewers watched a total of ",
        "{df1 %>% drop_na(year) %>% slice_max(hours_viewed) %>% pull(hours_viewed) %>% number(scale_cut = cut_short_scale())} ",
        "hours of movies released in ", 
        "{df1 %>% drop_na(year) %>% slice_max(hours_viewed) %>% pull(year)} "
        )) + 
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(), 
          # axis.text.x = element_blank(), 
          axis.title = element_blank(), 
          panel.grid.major.y = element_line(color = netflix_cols$ndarkgray, 
                                            linetype = "dotted"))


## p3 --------------------------------------------------------------------

# count of movie views by year
p3 = df1 %>% 
    ggplot(aes(x = year, y = views)) +
    geom_col(fill = netflix_cols$ngray, width = 0.61) +
    scale_y_continuous(
        labels = label_number(big.mark = ",", scale_cut = cut_short_scale()), 
        breaks = pretty_breaks(4)
    ) +
    scale_x_continuous(breaks = seq(2013, 2025, 1)) +
    labs(subtitle = glue(
        "**Count of movie views by release year:** movies released in ",
        "{df1 %>% drop_na(year) %>% slice_max(views, n = 2) %>% pull(year) %>% paste(collapse = ' and ')} ", 
        "are most popular"
    )) + 
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(), 
          axis.text.x = element_blank(),
          axis.title = element_blank(), 
          panel.grid.major.y = element_line(color = netflix_cols$ndarkgray, 
                                            linetype = "dotted"))


## p4 --------------------------------------------------------------------

# count of individual movies by year
p4 = df1 %>% 
    ggplot(aes(x = year, y = n)) +
    geom_col(fill = netflix_cols$ngray, width = 0.61) +
    scale_y_continuous(
        labels = label_number(big.mark = ","), 
        breaks = pretty_breaks(4)
    ) +
    scale_x_continuous(breaks = seq(2013, 2025, 1)) +
    labs(subtitle = glue(
        "**Count of individual, viewed movies by release year:** ",
        "movies released during COVID-19 are still popular in 2025"
        )) + 
    theme(axis.line = element_blank(),
          axis.ticks = element_blank(), 
          axis.text.x = element_blank(),
          axis.title = element_blank(), 
          panel.grid.major.y = element_line(color = netflix_cols$ndarkgray, 
                                            linetype = "dotted"))




## header ----------------------------------------------------------------

logo_df = tibble(
    x = c(0.5, 1, 2, 3, 3.5), 
    y = 0.5, 
    type = c("point", "text", "image", "text", "point"), 
    label = c(NA, "What have we been watching on", NA, "in the first half of 2025?", NA), 
    image = c(NA, NA, logo_path, NA, NA)
)

header_title = logo_df %>% 
    ggplot(aes(x = x, y = y)) +
    geom_image(
        aes(image = image),
        size = 2,
        by = "width") +
    geom_textbox(
        data = logo_df %>% filter(type == "text"),
        fill = NA,
        color = netflix_cols$nwhite, 
        size = 10,
        box.color = NA,
        family = text_logo,
        width = unit(3, "inches"),
        halign = 0.5,
        aes(label = label)
    ) +
    geom_point(
        data = logo_df %>% filter(type == "point"),
        color = NA
    ) +
    # coord_cartesian(clip = "off") +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.title = element_blank(), 
          plot.margin = margin(-8, 16, -8, 16, unit = "pt"))

header_desc = tibble(x = 0, y = 0, label = glue(
    "In 2023, Netflix started to release regular engagement reports summarizing ", 
    "how much time viewers have spent watching show and movie content over the ", 
    "previous six months. **These visualizations focus on movie viewership by ", 
    "release year from January through June 2025.** Unsurprisingly, new releases ", 
    "show the highest numbers overall. However, the most movies viewers watched ", 
    "in the first half of this year were released during the height of the ",
    "COVID-19 pandemic.")
) %>% 
    ggplot(aes(x = x, y = y, label = label)) +
    geom_textbox(
        fill = NA,
        color = "#C6C6C6", 
        size = 6,
        box.color = NA,
        family = text_copy,
        width = unit(11, "inches"),
        # halign = 0.5
    ) +
    theme(axis.text = element_blank(), 
          axis.ticks = element_blank(), 
          axis.line = element_blank(), 
          axis.title = element_blank())
    


# PW ----------------------------------------------------------------------

pw = wrap_plots(ncol = 2, widths = c(2, 1), 
           p1, 
           wrap_plots(nrow = 3, heights = c(1, 1, 1), p2, p3, p4)) +
    plot_annotation(title = create_title("Movies: viewers spend the most time on recent movies"), 
                    caption = caption)

full = plot_grid(header_title, header_desc, pw, ncol = 1, rel_heights = c(1, 1, 4))



# EXPORT ------------------------------------------------------------------

fct = 1.3
# ggsave(here("netflix", "netflix_CL.svg"), plot = full, bg = netflix_cols$nblackbg,
#        height = 11 * fct, width = 9 * fct)
