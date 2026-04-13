# ABOUT -------------------------------------------------------------------

# TidyTuesday week 15 (2026-04-14)
# Bird Sightings at Sea
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-04-14/readme.md

# This week we're exploring Bird Sightings at Sea! The data this week comes from
# Te Papa Tongarewa, The Museum of New Zealand. It consists of log book entries
# of bird sightings at sea near New Zealand, from 1969 to 1990.

# The data was recorded using guidelines for the Australasian Seabird Mapping
# Scheme and counts seabirds seen from a ship during a 10 minute period. The
# data includes geolocations of the sightings, bird species, numbers and
# behaviour, observer and ship name, and observation date and time.


# SETUP -------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(sf)
library(scales)
library(glue)
library(patchwork)
library(cowplot)

beaufort_scale <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/beaufort_scale.csv")
birds <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/birds.csv")
sea_states <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/sea_states.csv")
ships <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-04-14/ships.csv")



# FUNCTIONS ---------------------------------------------------------------

prep_data = function(df) {
    selected_records <- df %>% pull(record_id)
    
    # summarize bird data ----
    bird_df <- birds %>% 
        filter(record_id %in% selected_records, !is.na(count)) %>% 
        summarize(
            .by = record_id,
            n_birds = sum(count)
            ) %>% 
        mutate(
            sd = sd(n_birds, na.rm = TRUE),
            mn = mean(n_birds, na.rm = TRUE)
            ) %>%
        filter(between(n_birds, mn-2*sd, mn+2*sd))
    
    # join data ----
    df %>% 
        left_join(bird_df, by = "record_id") %>% 
        left_join(sea_states, by = "sea_state_class") %>% 
        left_join(beaufort_scale, by = "wind_speed_class") %>% 
        mutate(
            wind_speed = paste0(wind_speed_knots_min, "-", wind_speed_knots_max, " kn")
        )
}

plot_trips_map = function(alpha = 0.4, observer_var = NULL) {
    
    if(!is.null(observer_var)) {
        trips_df <- trips_df %>% filter(observer_last == observer_var)
        trips_sf <- trips_sf %>% filter(observer_last == observer_var)
    } else {
        trips_df <- trips_df
        trips_sf <- trips_sf
        }
    
    min_date <- min(trips_df$date) %>% format('%B %d, %Y')
    max_date <- max(trips_df$date) %>% format('%B %d, %Y')
    n_days <- trips_df %>% distinct(date) %>% nrow
    n_years <- number(n_days/365, accuracy = 0.1) 
    n_trips <- trips_df %>% summarize(n = n_distinct(trip_id)) %>% pull(n)
    n_observers <- trips_df %>% distinct(observer_last) %>% nrow

    sub_main <- glue(
        "From {min_date} to {max_date}, {n_observers} researchers ",
        " participated in {n_trips} bird counting trips* near New Zealand. ", 
        "Most trips occurred in the Tasman Sea but some ventured ",
        "further into the Indian Ocean. ",
        "Bird sighting records span a total of ",
        "{n_days %>% number(., big.mark = ',')} days ({n_years} years). ",
        "The top five observers are ranked by total number of records. Open circles ", 
        "mark voyage begin, closed circles mark voyage end."
    )
    
    sub_filtered <- glue(
        "**{observer_var}**: {min_date} to {max_date} – {n_trips} trips – {n_days} days",
    )
    
    if(!is.null(observer_var)) {
        sub <- sub_filtered
        cap <- NULL
        guide_list <- guides(
            color = "none", 
            fill = "none")
        annotations <- NULL
        ylower <- -47
    } else {
            sub <- sub_main
            cap <- glue(
                "*Trip IDs are derived from the first four digits of the record ",
                "ID, and the observer, respectively.")
            guide_list <- guides(
                color = guide_legend(title = "Observer"), 
                fill = guide_legend(title = "Observer"))
            annotations <- geom_text(
                    inherit.aes = FALSE,
                    data = tibble(
                        x = c(115.57, 164.43),
                        y = c(-39.67, -31.06),
                        label = c("Indian Ocean", "Tasman Sea")),
                    aes(x = x, y = y, label = label),
                    color = cols$gray,
                    fontface = "italic", 
                    size = 3 * 0.1 * base_size)
            ylower <- -60
            }
    
    necountries::countries() %>% 
        ggplot() + 
        geom_sf(color = NA, fill = cols$eggshell) +
        geom_sf(
            data = trips_sf, 
            aes(
                geometry = geometry, 
                color = reorder(obs_col, obs_rank),
            ), 
            alpha = alpha) +
        geom_point(
            data = trips_df %>% filter(start == TRUE), 
            shape = 21, 
            aes(x = longitude, y = latitude, color = reorder(obs_col, obs_rank)), 
            fill = cols$white, 
            alpha = alpha
        ) +
        geom_point(data = trips_df %>% filter(end == TRUE), 
                shape = 21, 
                aes(x = longitude, 
                    y = latitude, 
                    fill = reorder(obs_col, obs_rank), 
                    color = reorder(obs_col, obs_rank)), 
                alpha = alpha
        ) +
        annotations +
        scale_color_manual(values = pal_obs) +
        scale_fill_manual(values = pal_obs) +
        labs(
            subtitle = sub, 
            caption = cap, 
            x = NULL, 
            y = NULL
        ) +
        guide_list +
        scale_x_continuous(
            limits = c(100, 180), 
            expand = expansion(mult = 0)
        ) +
        scale_y_continuous(
            limits = c(ylower, -10), 
            expand = expansion(mult = 0)
        ) +
        theme(
            panel.grid.major = element_blank(), 
            axis.line.x = element_blank(), 
            axis.ticks.x = element_blank(), 
            axis.text.x = element_blank(),
            axis.line.y = element_blank(), 
            axis.ticks.y = element_blank(), 
            axis.text.y = element_blank(), 
            legend.position = "inside",
            legend.position.inside = c(-0.2, 0.5), 
            legend.justification = 0
        )
}

shorten_year_labels = function(x) {
    if_else(x == min(x, na.rm = TRUE), format(x, "%Y"), str_c("'", format(x, "%y")))
}

shortest_year_labels = function(x) {
    str_c("'", format(x, "%y"))
}

plot_record_counts = function(observer_var, intv = "1 year", alpha = 0.7) {
    
    df <- trips_df %>% 
        filter(observer_last == observer_var) %>% 
        summarize(
            .by = c(observer_last, obs_rank, n_records, obs_col, year, year_format), 
            n = n()
        )
    
    xmin <- min(df$year_format)
    xmax <- max(df$year_format)
    
    df %>% 
        ggplot(aes(x = year_format, y = n, color = observer_last, fill = observer_last)) + 
        geom_col(alpha = alpha, show.legend = FALSE) +
        scale_color_manual(values = pal_obs) +
        scale_fill_manual(values = pal_obs) +
        scale_x_date(
            expand = expansion(mult = c(0.05, 0.05)),
            breaks = seq(ymd(xmin), ymd(xmax), by = intv),
            labels = shorten_year_labels
        ) +
        scale_y_continuous(
            labels = label_number(big.mark = ','),
            expand = expansion(mult = 0.01)) +
        labs(
            x = NULL, 
            y = "Number of records",
            subtitle = observer_var
        )
}



# overview plots

plot_bird_sp = function(df, col) {
    dat <- birds %>% 
        # filter(record_id %in% df$record_id) %>%
        mutate(gen_sp = word(species_scientific_name, 1, 2)) %>% 
        summarize(.by = gen_sp, n_birds = sum(count, na.rm = TRUE), n = n())  %>% 
        slice_max(n = 5, order_by = n_birds) %>% 
        mutate(xpos = n_birds + 0.05 * max(n_birds))
            
    
    dat %>% 
        ggplot(aes(y = reorder(gen_sp, n_birds), x = n_birds)) +
        geom_linerange(aes(xmin = 0, xmax = n_birds), color = cols$lightgray) +
        geom_point(color = col, size = 2) +
        geom_text(
            aes(x = xpos, label = glue("{number(n_birds, scale_cut = cut_short_scale(), accuracy = 1L)}")),
            size =  3 * 0.1 * base_size,
            hjust = 0, 
            color = cols$gray) +
        scale_x_continuous(
            expand = expansion(mult = c(0.05, 0.2)), 
            labels = label_number(scale_cut = cut_short_scale(), big.mark = ",")
        ) +
        labs(
            x = NULL, 
            y = NULL, 
            subtitle = "Top 5 bird species") +
        theme(axis.text.y = element_text(face = "italic"), axis.text.x = element_blank())
}



plot_bird_counts = function(df, col) {
    df %>% 
        ggplot(aes(x = date, y = n_birds)) +
        geom_col(color = col) +
        labs(
            y = NULL, x = NULL, 
            subtitle = "Number of birds observed over time"
            )
}

plot_birds_time = function(df, col) {
    df %>% 
        summarize(.by = time, n_birds = median(n_birds, na.rm = TRUE), n = n()) %>% 
        ggplot(aes(x = time, y = n_birds)) +
        geom_col(color = col) +
        scale_x_time(
            date_breaks = "2 hours", 
            date_labels = "%H:%M") +
        labs(
            y = NULL, x = NULL, 
            subtitle = "Median number of birds observed by time of day"
            )
}

plot_birds_season = function(df, col) {
    df %>% 
        ggplot(aes(x = year_format, y = n_birds)) +
        facet_wrap(~ season, nrow = 1, labeller = as_labeller(str_to_sentence)) +
        geom_jitter(shape = 21, color = "white", fill = col, width = 0.2, alpha = 0.6) +
        stat_summary(
            fun = "median", 
            geom = "point",
            shape = 21, 
            color = "white", 
            fill = col_darker(col, 10), 
            size = 3) +
        scale_x_date(
            expand = expansion(mult = c(0.1, 0.1)),
            date_breaks = "2 years",
            labels = shortest_year_labels) +
        labs(
            y = NULL, 
            x = NULL, 
            subtitle = "Number of birds observed by season and year. Large points show medians, small points show individual counts."
            )
}

plot_birds_wind = function(df, col, alpha = 0.7) {
    dat <- df %>% 
        summarize(
            .by = c(wind_speed_class, wind_speed, wind_description), 
            n_birds = median(n_birds, na.rm = TRUE), 
            n = n()
        ) %>%
        arrange(wind_speed_class) %>% 
        mutate(
            wind_speed_class = factor(
                wind_speed_class, 
                levels = wind_speed_class, 
                labels = str_c(wind_speed, "\n", str_wrap(str_to_sentence(wind_description), 10)),
                ordered = TRUE
            )) %>% 
        filter(n > 10) 
    
    dat %>% ggplot(aes(x = factor(wind_speed_class), y = n_birds)) +
        geom_col(fill = col, color = col, alpha = alpha) +
        scale_x_discrete(
            sec.axis = dup_axis(
                name = "Sample size", 
                breaks = seq_len(length(unique(dat$n))),
                labels = paste0("n = ", dat$n)
            ) 
        ) +
        labs(
            y = NULL, x = NULL, 
            subtitle = "Median bird counts by wind speed class"
            )
}

plot_overviews = function(
        df,
        col = "default",
        subs = TRUE,
        bird_sp = TRUE,
        bird_counts = TRUE, 
        bird_time = TRUE, 
        bird_season = TRUE, 
        bird_wind = TRUE
        ) {
    
    if(col == "default") {col <- pal_obs %>% 
        enframe %>% 
        filter(name == unique(df$observer_last)) %>% 
        pull(value)
    } else {col <- col}
    
    overview <- list()
    
    if (bird_counts) {overview$bird_counts <- plot_bird_counts(df, col)}
    if (bird_time) {overview$bird_time <- plot_birds_time(df, col)}
    if (bird_season) {overview$bird_season <- plot_birds_season(df, col)}
    if (bird_wind) {overview$bird_wind <- plot_birds_wind(df, col)}
    
    if(bird_sp) {bird_sp <- plot_bird_sp(df, col)}
    
    plots <- list(
        bird_sp = bird_sp, 
        overview = overview
    )
    
    if (subs == FALSE) {plots <- map(plots, ~ .x + labs(subtitle = NULL))}
    
    return(plots)
    
}


create_header_and_footer = function(cap, obs_var) {
    header <- ggplot() + 
        geom_blank() +
        labs(
            title = "Bird Sightings at Sea",
            subtitle = glue("Log book entries of bird sightings at sea near New Zealand between 1969 and 1990: A closer look into {obs_var}'s observations")
        ) +
        theme(
            plot.subtitle = ggtext::element_textbox_simple(
                margin = margin(16, 2, 2, 2)), 
            plot.title = element_text(margin = margin(6, 2, 6, 2)))
    
    p_cap <- ggplot() + geom_blank() + labs(caption = cap)
    
    plot_elements <- list(
        header = header, 
        caption = p_cap
    )
    
    return(plot_elements)
}


create_full_viz = function(obs_var) {
    p1 <- plot_trips_map()
    
    p2 <- master %>% 
        filter(observer_last == obs_var) %>% 
        prep_data() %>% 
        plot_overviews(subs = TRUE)
    
    p3 <- wrap_plots(
        ncol = 2, widths = c(1.2, 1),
        plot_trips_map(observer_var = obs_var),
        p2$bird_sp
    )
    
    pw <- wrap_plots(
        ncol = 2, widths = c(1, 2),
        free(wrap_plots(nrow = 3, heights = c(30, 0.001, 10), p1, plot_spacer(), p3)), 
        wrap_plots(ncol = 2,  p2$overview, byrow = FALSE, widths = c(1, 1))
    )
    
    plot_elements <- create_header_and_footer(cap = cap, obs_var = obs_var)
    
    final <- plot_grid(
        rel_heights = c(1, 10, 0.5),  # 10
        ncol = 1, 
        # header, 
        plot_elements$header,
        pw, 
        # p_cap
        plot_elements$caption
    )
    
    final
}


# DATA WRANGLING ----------------------------------------------------------

# counting trips, selecting most prolific observers
master = ships %>% 
    filter(census_method == "full") %>% 
    mutate(date_time = ymd_hms(str_c(date, time, sep = " ")), 
           year = year(date), 
           year_format = floor_date(date, unit = "year"),
           season = case_when(
               month(date) %in% c(12, 1, 2) ~ "summer", 
               month(date) %in% c(3, 4, 5) ~ "fall",
               month(date) %in% c(6, 7, 8) ~ "winter",
               month(date) %in% c(9, 10, 11) ~ "spring"
           ),
           season = factor(season, levels = c("summer", "fall", "winter", "spring")),
           observer_last = str_extract(observer, "\\S+$") %>% str_squish, 
           id_pattern = str_extract(record_id, "\\d{4}"), 
           ) %>% 
    arrange(observer_last, date, time) %>% 
    mutate(
        .by = c(observer_last, id_pattern), 
        trip_id = paste(observer_last, cur_group_id())) %>% 
    add_count(observer_last, name = "n_records") %>% 
    mutate(
        obs_rank = dense_rank(desc(n_records)),
        top5_observer = obs_rank <= 5,
        obs_col = if_else(top5_observer == TRUE, observer_last, "Other")
        ) %>% 
    filter(!is.na(longitude) & !is.na(latitude))

observers = master %>% 
    filter(top5_observer == TRUE) %>% 
    pull(observer_last) %>% 
    unique

trips_df = master %>% 
    mutate(
        .by = c(trip_id, observer_last, obs_col),
        start = date_time == min(date_time), 
        end = date_time == max(date_time)
    ) %>% 
    mutate(observer_last = fct_reorder(.f = observer_last, .x = obs_rank, .fun = min))

trips_sf = trips_df %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
    summarize(
        .by = c(observer_last, obs_rank, obs_col, trip_id), 
        geometry = st_combine(geometry)
        ) %>% 
    st_cast("LINESTRING")

# top 10 birds for color inspiration
birds %>% 
    mutate(gen_sp = word(species_scientific_name, 1, 2)) %>% 
    count(gen_sp) %>% 
    slice_max(n = 10, order_by = n)



# PLOT PREP ---------------------------------------------------------------

## theming ---------------------------------------------------------------

base_size = 9

# Australian gannet (Morus serrator)
# https://ebird.org/species/ausgan1
cols = list( 
    "burntred" = "#924328",
    "orange" = "#AF5420", 
    "brightorange" = "#D07724", 
    "eggshell" = "#D5D2C3", 
    "white" = "#E9E9EA", 
    "lightgray" = "#C1CACB",
    "gray" = "#4B4C45", 
    "black" = "#1F231F", 
    "green" = "#45665B", 
    "oceanblue" = "#174A7C", 
    "skyblue" = "#6899B2"
)

theme_set(
    hrbrthemes::theme_ipsum(base_size = base_size) + 
        theme(
            text = element_text(color = cols$gray),
            line = element_line(color = cols$lightgray), 
            axis.text = element_text(color = cols$gray), 
            plot.subtitle = ggtext::element_textbox_simple(
                margin = margin(0, 0, 6, 0)), 
            panel.grid = element_line(linetype = "dotted"), 
            panel.background = element_blank(),
            axis.text.x.top = element_text(), 
            strip.text = element_text(color = cols$gray, size = base_size * 1.03)
            )
    )

pal_obs = c(
    "Baines" = cols$burntred,
    "Carter" = cols$orange,
    "Cheshire" = cols$oceanblue,
    "Cleaver" = cols$skyblue,
    "Jenkins" = cols$green,
    "Other" = cols$lightgray
)

cap = glue(
    "Source: Te Papa Tongarewa, The Museum of New Zealand | ", 
    "#TidyTuesday week 15 ({format(ymd('2026-04-14'), '%B %d, %Y')}) | ", 
    "Visualization by Claudia Laurenzano | ", 
    "{format(today(), '%B %d, %Y')} | ", 
    "Created in R")



# PLOTS -------------------------------------------------------------------

create_full_viz(obs_var = "Cleaver")
create_full_viz(obs_var = "Jenkins") # needs troubleshooting
create_full_viz(obs_var = "Carter")
create_full_viz(obs_var = "Cheshire")
create_full_viz(obs_var = "Baines") # needs troubleshooting



# EXPORT ------------------------------------------------------------------

ggsave(here("seabirds NZ", "CL_seabirds_Cleaver.png"), 
       plot = create_full_viz(obs_var = "Cleaver"), 
       bg = "white", # to avoid artifact lines from plot_grid()
       height = 9, width = 16)

ggsave(here("seabirds NZ", "CL_seabirds_Cheshire.png"), 
       plot = create_full_viz(obs_var = "Cheshire"), 
       bg = "white", # to avoid artifact lines from plot_grid()
       height = 9, width = 16)

ggsave(here("seabirds NZ", "CL_seabirds_Carter.png"), 
       plot = create_full_viz(obs_var = "Carter"), 
       bg = "white", # to avoid artifact lines from plot_grid()
       height = 9, width = 16)
