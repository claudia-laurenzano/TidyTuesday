
# ABOUT -------------------------------------------------------------------

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-08-26/readme.md#billboard-hot-100-number-ones
# Billboard Hot 100 Number Ones

# This week we are exploring the Billboard Hot 100 Number Ones Database. This
# workbook contains substantial data about every song to ever top the Billboard
# Hot 100 between August 4, 1958 and January 11, 2025. It was compiled by Chris
# Dalla Riva as he wrote the book Uncharted Territory: What Numbers Tell Us
# about the Biggest Hit Songs and Ourselves. It also often powers his newsletter
# Can't Get Much Higher.


# SETUP -------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(tidytext)
library(ggstream)
library(ggroove) # color palettes inspired by Radiohead LP covers
library(scales)
library(glue)
library(ggtext)
library(patchwork)
library(cowplot)
library(gt)
library(ggimage)

billboard <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/billboard.csv')
topics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-26/topics.csv')

logo_path = here("billboard", "Billboard_Hot_100_logo.jpg")



# DATA WRANGLING ----------------------------------------------------------

# unnest genre strings and add time identifiers
# discogs_genre has fewer NA's than cdr_genre (16 vs 88)
genres = billboard %>% 
    unnest_tokens(output = "genre", 
                  input = discogs_genre, 
                  token = "regex",
                  pattern = ";", 
                  to_lower = FALSE) %>% 
    mutate(genre = if_else(genre == "Funk / Soul", "Funk/Soul", genre),
           date = ymd(date),
           year = year(date), 
           year_date = floor_date(date, unit = "year"),
           decade = floor(year/10) * 10)

decades = genres %>% filter(year == decade) %>% distinct(decade, year_date)

# total songs, total w/ genre, total w/o genre
n_songs = billboard %>% nrow
n_songs_genre = billboard %>% filter(!is.na(cdr_genre)) %>% nrow()
n_songs_na = billboard %>% filter(is.na(cdr_genre)) %>% nrow()

# top 5 genres
genres_ranked = genres %>% count(genre) %>% drop_na() %>% arrange(-n)
top_genres = genres %>% 
    count(genre) %>% 
    mutate(total = n_songs, percent = n/total) %>% 
    slice_max(n = 5, order_by = n)

# top genre by decade
top_genres_decade = genres %>% 
    count(genre, decade) %>% 
    drop_na(genre) %>% 
    filter(.by = decade, n == max(n)) %>% 
    arrange(decade)


# top artists by song (n >= 10)
top_artists = billboard %>% 
    tabyl(artist) %>% 
    filter(n >= 10) %>% 
    left_join(genres %>% select(artist, genre)) %>% 
    drop_na(genre) %>% 
    distinct() %>% 
    arrange(-n, artist, factor(genre, levels = genres_ranked$genre))  %>% 
    mutate(rank = dense_rank(-n))

# top 5 songs by weeks at no. 1
top_songs = billboard %>% 
    summarize(.by = song, total_weeks = sum(weeks_at_number_one)) %>% 
    filter(total_weeks >= 15) %>% 
    left_join(genres %>% select(song, artist, year, genre)) %>% 
    arrange(factor(genre, levels = genres_ranked$genre)) %>% 
    summarize(.by = c(song, artist, year, total_weeks), 
              genre = paste0(genre, collapse = "; ")) %>% 
    mutate(genre = if_else(genre == "NA", "Unknown", genre)) %>% 
    select(total_weeks, song, artist, year, genre) %>% 
    arrange(-total_weeks)



# PLOT PREP ---------------------------------------------------------------

# fonts
# title: Anton (Google)
# copy: IBM Plex Sans (Google)
systemfonts::require_font("Anton")
# systemfonts::require_font("IBM Plex Sans")
systemfonts::get_from_google_fonts("IBM Plex Sans")

# colors: 
# https://github.com/DanOvando/ggroove
# remotes::install_github("danovando/ggroove")
# https://sophiemeakin.github.io/technocolour/
# devtools::install_github("sophiemeakin/technocolour")

ggroove::album_pal(lp = "pablo_honey", direction = -1, n = 6) %>% show_col
# [1] "#2A0A43FF" "#396293FF" "#CE2117FF" "#7DBA63FF" "#F1DA3FFF" "#E2EAD2FF"

pal_df = genres %>% 
    count(genre) %>% 
    drop_na() %>% 
    arrange(-n) %>% 
    mutate(
        top5 = if_else(genre %in% top_genres$genre, "yes", "no"), 
        fill = c("#CE2117FF", "#2A0A43FF", "#7DBA63FF", "#F1DA3FFF", "#396293FF",
               # col_lighter("gray70", seq(0, 21, 3))
               col_mix("#E2EAD2FF", paste0("gray", seq(20, 90, 10)))
               ), 
        color = if_else(
            top5 == "yes",
            col_mix(fill, "gray90"),
            col_lighter(fill, 4)
            ))

pal_fill = pal_df %>% select(genre, fill) %>% deframe
pal_col = pal_df %>% select(genre, color) %>% deframe

pal_fill_dk = pal_df %>% 
    mutate(fill = if_else(genre == "Electronic", col_darker(fill, 15), fill)) %>% 
    select(genre, fill) %>% 
    deframe

theme_set(
    theme_void() +
        theme_void(
            base_family = "IBM Plex Sans", 
            paper = "white",
            ink = col_lighter("black", 20),
            # accent = "maroon"
            ) +
        theme_sub_plot(
            title = element_text(family = "Anton"), 
            title.position = "panel", 
            subtitle = ggtext::element_textbox_simple(),
            caption = ggtext::element_textbox_simple(),
            margin = margin_auto(10)) +
        theme_sub_axis_x(text = element_text())
)


cap = glue(
    "Source: Uncharted Territory, Chris Dalla Riva | ", 
    "N = {n_songs %>% number(big.mark = ',')} ", 
    "songs (excludes {n_songs_na} songs without specified genre) | ",
    "#TidyTuesday week 34, ",
    "{format(ymd('2025-08-26'), '%B %d, %Y')} | ",
    "Visualization by Claudia Laurenzano | ",
    "{format(today(), '%B %d, %Y')} | ",
    "Created in R"
)



# EXPLORE -----------------------------------------------------------------

billboard %>% glimpse

# total songs
billboard %>% nrow

# top genres by songs
genres %>% count(genre) %>% slice_max(n, n = 5)
genres %>% tabyl(genre) %>% slice_max(n, n = 5) %>% mutate(total = sum(percent))

# top songs by weeks
billboard %>% 
    summarize(.by = song, 
              weeks = sum(weeks_at_number_one)) %>% 
    slice_max(weeks, n = 10) %>% 
    left_join(genres %>% select(song, artist, year, genre))



# most songs are linked to top genres (95%)
(genres %>% filter(genre %in% top_genres$genre) %>% distinct(song) %>% nrow)/(billboard %>% nrow)

# genres by year
genres %>% summarize(.by = c(year, genre), n = n()) 

# genres by decade
genres %>% tabyl(genre, decade)

# genres by decade (long)
genres %>% 
    count(genre, decade) %>% 
    drop_na(genre) %>% 
    arrange(-decade, -n) %>% 

# avg weeks at top by decade
genres %>% 
    distinct(song, artist, decade, weeks_at_number_one) %>% 
    summarize(.by = c(decade), mean_weeks = mean(weeks_at_number_one))

# total number of songs by decade
genres %>% 
    distinct(song, artist, decade) %>% 
    summarize(.by = c(decade), n_songs = n())


genres %>% 
    summarize(.by = c(decade, year_date, genre), 
              n = n()) %>% 
    mutate(.by = year_date, total = sum(n)) %>% 
    
    ggplot(aes(x = year_date, y = n, fill = genre)) +
    geom_stream() +
    geom_vline(data = decades,
               aes(xintercept = year_date),
               linetype = "dotted",
               color = "white") +
    scale_x_date(breaks = decades %>% pull(year_date), labels = decades %>% pull(decade))
    
 

# PLOTS -------------------------------------------------------------------


## p1: stream ------------------------------------------------------------

sub1 = glue(
    "<span style='font-family:Anton'>Genre distribution over time</span>
    
    **<span style='color:{'#2A0A43FF'}'>Rock</span>** 
    music dominated the charts from the 1960's through the 1980's, until
    **<span style='color:{'#396293FF'}'>Hip Hop</span>** 
    ended the three decade-long reign and took over the lead in the 1990's and 
    early aughts. Having always been popular, 
    **<span style='color:{'#CE2117FF'}'>Pop</span>** 
    songs took over the top of the leaderboard in the 2010's. 
    "
    )

p1 = genres %>% 
    summarize(.by = c(decade, year_date, genre), n = n()) %>% 
    mutate(.by = year_date, total = sum(n)) %>% 
    mutate(
        genre = factor(
            genre, 
            levels = genres %>% count(genre) %>% drop_na() %>% arrange(-n) %>% pull(genre))
    ) %>% 
    ggplot(aes(
        x = year_date, 
        y = n, 
        color = genre,
        fill = genre
        )) +
    geom_stream() +
    geom_vline(data = decades,
               aes(xintercept = year_date),
               linetype = "dotted",
               color = "white") +
    scale_x_date(
        breaks = decades %>% pull(year_date), 
        labels = decades %>% pull(decade),
        expand = expansion(mult = c(0.01, 0.05))
        ) +
    scale_fill_manual(values = pal_fill) +
    scale_color_manual(values = pal_col) +
    guides(color = guide_legend(override.aes = list(color = NA))) +
    labs(subtitle = sub1,
         fill = NULL, 
         color = NULL) +
    theme_sub_legend(key.width = unit(20, "pt"), key.height = unit(10, "pt"))


## p2: top artists -------------------------------------------------------

sub2 = glue(
    "<span style='font-family:Anton'>Top artists with at least 10 #1 hits</span>
    
    Artists with number (left), date range (middle), and genres (right) of 
    chart toppers. All artists include elements of 
    **<span style='color:{'#CE2117FF'}'>Pop</span>** 
    in their hits, and hitting a diverse mix of several genres looks to be a 
    winning strategy among successful musicians. The Beatles are all-time winners 
    with 20 #1 hits, followed by Mariah Carey and Madonna. Will Taylor 
    Swift soon overtake all others?
    "
)

p2 = wrap_plots(
    nrow = 1, 
    widths = c(1, 1, 7),
    
    # artist labels, n
    top_artists %>%
        ggplot(aes(x = "N", y = reorder(artist, n))) +
        geom_text(aes(label = n), 
                  family = "Anton", 
                  size = 4) +
        theme(
            axis.text.y = element_text(),
            axis.text.x = element_blank()
        ),
    
    # date range
    genres %>% 
        filter(artist %in% top_artists$artist) %>% 
        distinct(artist, song, year) %>%
        mutate(.by = artist, n = n(), min_year = min(year), max_year = max(year), 
               range = paste0(min_year, "-", max_year)) %>% 
        distinct(artist, n, range) %>% 
        ggplot(aes(x = "N", y = reorder(artist, n))) +
        geom_text(aes(label = range)) +
        theme(
            axis.text.x = element_blank()
        ),
    
    # genres
    top_artists %>%
        ggplot(aes(
            x = factor(genre, levels = genres_ranked$genre),
            y = reorder(artist, n)
        )) +
        geom_point(aes(color = genre), size = 5, show.legend = FALSE) +
        scale_color_manual(values = pal_fill) +
        scale_x_discrete(
            position = "top",
            labels = function(x) str_wrap(x, 12),
            expand = expansion(mult = c(0.05, 0.1))
        )
    ) +
    plot_annotation(
        subtitle = sub2, 
        caption = cap
    )



## p3: top genres --------------------------------------------------------

sub3 = glue(
    "<span style='font-family:Anton'>Genre breakdown</span>
    
    Almost half of all songs that have reached #1 on the Billboard Hot 100
    feature elements of 
    **<span style='color:{'#CE2117FF'}'>Pop</span>** and/or 
    **<span style='color:{'#2A0A43FF'}'>Rock</span>**.
    Only 
    {percent((genres %>% filter(!genre %in% top_genres$genre) %>% distinct(song) %>% nrow)/n_songs)} 
    of these chart-topping songs fall outside of the five most common genres."
)

cap3 = glue(
    "**Note:** Songs can belong to multiple genres. Percentages reflect the 
    number of times a genre is assigned, divided by the total number of unique songs 
    (N = {n_songs %>% number(big.mark = ',')}), so totals exceed 100%."
)

p3 = top_genres %>% 
    ggplot(aes(y = reorder(genre, n), x = percent)) +
    scale_color_manual(values = pal_col) +
    geom_col(aes(fill = genre, color = genre), width = 0.4, show.legend = FALSE) +
    scale_fill_manual(values = pal_fill_dk) +
    geom_text(
        aes(x = 0, label = paste0(genre, ": ", percent(percent, accuracy = 1L))), 
        family = "Anton",
        color = "white", 
        hjust = 0, 
        nudge_x = 0.01, 
        size = 3) +
    labs(subtitle = sub3, 
         caption = cap3) +
    theme_sub_axis_x(
        text = element_blank()
    )


## p4: top songs ---------------------------------------------------------

sub4 = glue(
    "<span style='font-family:Anton'>Longest-running chart toppers</span>
    
    These six tracks held the #1 spot on the Billboard Hot 100 longer than 
    any others, between 15 and 19 weeks.  
    "
)

p4 = top_songs %>% 
    mutate(artist = str_wrap(artist, 25), 
           genre = str_wrap(genre, 35)) %>% 
    rename_with(str_to_sentence) %>% 
    gt()%>% 
    opt_table_font(font = "IBM Plex Sans") %>%
    opt_vertical_padding(scale = 0.7) %>% 
    tab_options(
        table.font.size = "12px"
    ) %>% 
    tab_style(
        style = cell_text(font = google_font("Anton")),
        locations = cells_column_labels()
    ) %>% 
    tab_style(
        style = cell_text(font = google_font("Anton")),
        locations = cells_body(columns = Total_weeks)
    ) %>% 
    cols_label(
        Total_weeks = "Weeks at #1"
    ) %>% 
    wrap_table() +
    plot_annotation(subtitle = sub4)



# header ------------------------------------------------------------------

## title -----------------------------------------------------------------

logo_df = tibble(
    y = 1, 
    x = c(0.5, 2, 3, 5), 
    type = c("point", "image", "text", "point"), 
    label = c(NA, NA, "Number Ones", NA), 
    image = c(NA, logo_path, NA, NA)
    )

header_title = logo_df %>% 
    ggplot(aes(x = x, y = y)) +
    geom_image(
        aes(image = image),
        size = 1,
        by = "width") +
    geom_textbox(
        data = logo_df %>% filter(type == "text"),
        fill = NA,
        size = 15,
        box.color = NA,
        family = "Anton",
        width = unit(4, "inches"),
        hjust = 0,
        halign = 0,
        aes(label = label)
    ) +
    geom_point(
        data = logo_df %>% filter(type == "point"),
        color = NA
    ) +
    theme_sub_axis_x(text = element_blank()) +
    theme_sub_plot(margin = margin(-12, 8, -16, 8, unit = "pt"))
    


## subtitle ---------------------------------------------------------------

header_desc = tibble(x = 0, y = 0, label = glue(
    "Most Billboard Hot 100 number one hits since 1958 are either 
    **<span style='color:{'#CE2117FF'}'>Pop</span>**, 
    **<span style='color:{'#2A0A43FF'}'>Rock</span>**, 
    **<span style='color:{'#7DBA63FF'}'>Funk/Soul</span>**, 
    **<span style='color:{col_darker('#F1DA3FFF', 15)}'>Electronic</span>**, 
    **<span style='color:{'#396293FF'}'>Hip Hop</span>**, 
    or any combination of these genres. Over time, taste in music has 
    changed as evident in the changing distribution patterns of genres 
    among the chart toppers across decades. More recently released songs 
    (2019, 2024) have had the longest-ever grip on the tip, and both use 
    elements of Hip Hop which has gained in popularity over the past decades, 
    but also Folk, World & Country. The all-time most successful artists with 
    the highest number one hits are The Beatles, followed by several other 
    artists from past decades. Will artists from the current time period soon
    take over the lead on total number one hits?"
    )
) %>% 
    ggplot(aes(x = x, y = y, label = label)) +
    geom_textbox(
        fill = NA,
        # color = "#C6C6C6", 
        size = 4.5,
        box.color = NA,
        family = "IBM Plex Sans",
        width = unit(11, "inches"),
        # halign = 0.5
    ) +
    theme_sub_axis_x(text = element_blank()) +
    theme_sub_plot(margin = margin(-12, 4, 4, 4))



# pw ----------------------------------------------------------------------

plot_grid(ncol = 1, 
          # rel_heights = c(1, 1, 3, 1.3, 1.3), 
          rel_heights = c(2, 1.5, 3.9, 3.1, 3.2), 
          
          header_title, 
          header_desc,
          
          p1, 
          plot_grid(ncol = 2, rel_widths = c(0.8, 1.2), p3, p4),
          p2)

fct = 1.3
# ggsave(here("billboard", "billboard_CL.png"), bg = "white", 
#        height = 11 * fct, width = 9 * fct)
