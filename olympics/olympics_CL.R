
# SETUP -------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(here)
library(janitor)
library(glue)
library(tinytable)
library(patchwork)

# tidy tuesday week 32, 2024-08-06

tuesdata <- tidytuesdayR::tt_load(2024, week = 32)
olympics <- tuesdata$olympics

source(here("..", "..", "Central Data", "theme_BC_CL2.R"))
source(here("..", "..", "Central Data", "colors_BC_CL2.R"))



# DATA WRANGLING ----------------------------------------------------------

## host countries --------------------------------------------------------

olympics_countries <- tibble(
    year = c(1896, 1900, 1904, 1908, 1912, 1916, 1920, 1924, 1928, 1932, 
             1936, 1940, 1944, 1948, 1952, 1956, 1960, 1964, 1968, 1972, 
             1976, 1980, 1984, 1988, 1992, 1996, 2000, 2004, 2008, 2012, 
             2016, 2020, 2024),
    city = c("Athens", "Paris", "St. Louis", "London", "Stockholm", NA, 
             "Antwerp", "Paris", "Amsterdam", "Los Angeles", "Berlin", 
             NA, NA, "London", "Helsinki", "Melbourne", "Rome", "Tokyo", 
             "Mexico City", "Munich", "Montreal", "Moscow", "Los Angeles", 
             "Seoul", "Barcelona", "Atlanta", "Sydney", "Athens", "Beijing", 
             "London", "Rio de Janeiro", "Tokyo", "Paris"),
    country = c("Greece", "France", "United States", "United Kingdom", 
                "Sweden", NA, "Belgium", "France", "Netherlands", "United States", 
                "Germany", NA, NA, "United Kingdom", "Finland", "Australia", 
                "Italy", "Japan", "Mexico", "West Germany", "Canada", "Soviet Union", 
                "United States", "South Korea", "Spain", "United States", 
                "Australia", "Greece", "China", "United Kingdom", "Brazil", "Japan", 
                "France"),
    noc = c("GRC", "FRA", "USA", "GBR", "SWE", NA, "BEL", "FRA", "NLD", "USA", 
            "DEU", NA, NA, "GBR", "FIN", "AUS", "ITA", "JPN", "MEX", "DEU", 
            "CAN", "RUS", "USA", "KOR", "ESP", "USA", "AUS", "GRC", "CHN", 
            "GBR", "BRA", "JPN", "FRA")
) %>%
    mutate(
        noc = case_when(noc == "DEU" ~ "GER", 
                        noc == "NLD" ~ "NED", 
                        noc == "RUS" ~ "URS",
                        .default = noc),
        country = case_when(country == "West Germany" ~ "Germany", 
                            .default = country)
    ) %>% filter(year <= 2016)



## top 5 countries -------------------------------------------------------

top5_nocs_medals = olympics %>% 
    filter(!is.na(medal),
           season == "Summer") %>% 
    summarize(.by = noc,
              medals = n()) %>% 
    arrange(-medals) %>% 
    slice(1:5)

top5_host = olympics_countries %>% filter(noc %in% top5_nocs_medals$noc)

top5_nocs = top5_nocs_medals %>% 
    left_join(top5_host %>% distinct(noc, country))


## country overview ------------------------------------------------------

total_country = olympics %>% 
    filter(!is.na(medal),
           season == "Summer", 
           noc %in% top5_nocs$noc) %>% 
    left_join(top5_nocs) %>% 
    mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze")),
           country = factor(country, levels = top5_nocs$country)) %>% 
    summarize(.by = c(year, country, medal, medals),
              n = n()) %>% 
    mutate(.by = c(year, country), 
           total = sum(n)) %>% 
    mutate(.by = year, 
           global_max = total == max(total)) %>% 
    mutate(.by = country, 
           local_max = total == max(total)) %>% 
    mutate(country_total = paste0(
        country, ": N = ", medals
    ))


total_overview = total_country %>% 
    # tabyl(country, medal)
    summarize(
        .by = c(country, medal),
        n = sum(n)) %>% 
    mutate(.by = country, 
           total = sum(n), 
           prop = n/total, 
           text = paste0(number(n, big.mark = ","), " (", percent(prop, accuracy = 1L), ")")) %>% 
    arrange(-total)


## top 10 sports ---------------------------------------------------------

top10_sports = map(.x = top5_nocs$noc, 
    .f = ~ {
        olympics %>% 
            filter(!is.na(medal),
                   season == "Summer", 
                   noc == .x) %>% 
            left_join(top5_nocs) %>% 
            summarize(.by = c(country, sport, sex), 
                      n = n()) %>% 
            mutate(.by = sport, 
                   total = sum(n)) %>% 
            pivot_wider(id_cols = c(country, sport, total),
                        names_from = sex, 
                        values_from = n) %>% 
            slice_max(order_by = total, n = 10) %>% 
            arrange(-total, sport)
    }) %>% 
    bind_rows
    


# PLOT --------------------------------------------------------------------

## plot prep -------------------------------------------------------------

theme_set(theme_bc())

# medal colors: https://www.schemecolor.com/olympic-medals-color-scheme.php
medal_cols_lt = list(
    "Gold" = "#FEE101",
    "Silver" = "#D7D7D7",
    "Bronze" = "#A77044"
)

medal_cols_dk = list(
    "Gold" = "#D6AF36", 
    "Silver" = "#A7A7AD", 
    "Bronze" = "#824A02"
)

# olympic colors: https://www.color-hex.com/color-palette/23070
olympic_cols = c(
    "#3e76ec",
    "#000000",
    "#ff0000",
    "#ffce01",
    "#179a13"
)


## overview --------------------------------------------------------------

# overview table on top 5 countries

dat = split(total_overview, total_overview$country)

tab = tibble(
    "Country" = names(dat),
    "Total" = "", 
    "Gold" = "",
    "Silver" = "", 
    "Bronze" = "", 
    "Medal distribution" = ""
)

f = function(d, ...){
    d %>% ggplot(aes(x = prop,
                     y = factor(country),
                     fill = medal)) +
        geom_col(width = 1, show.legend = FALSE) +
        scale_fill_manual(values = lighten(paste(medal_cols_dk), 0.3)) +
        scale_y_discrete(limits = rev) +
        scale_x_reverse() +
        theme_bc(void = TRUE)
}

overview_table = total_overview %>% 
    pivot_wider(id_cols = c(country, total), 
                names_from = medal, 
                values_from = text) %>% 
    relocate(Gold, .after = total) %>% 
    mutate(total = number(total, big.mark = ","),
           distribution = "") %>% 
    rename_all(~str_to_sentence(.)) %>% 
    tt() %>% 
    plot_tt(j = 6, 
            fun = f,
            data = dat
    ) %>% 
    style_tt(color = light_text)



## top5 countries --------------------------------------------------------

total_n = olympics %>% 
    filter(!is.na(medal),
           season == "Summer", 
           noc %in% top5_nocs$noc) %>% 
    nrow

p0 = top5_nocs %>% 
    ggplot(aes(y = reorder(country, medals), x = medals)) +
    geom_linerange(aes(xmin = 0, xmax = medals), 
                   color = light_text) +
    geom_point(size = 4, color = light_text) +
    geom_text(aes(label = number(medals, big.mark = ",")), 
              family = text_sans, 
              color = light_text, 
              hjust = -0.4) +
    labs(subtitle = glue(
        "Top 5 countries who have won the most medals in the Summer Olympics between ",
        "{min(olympics$year)} and {max(olympics$year)}"),
        caption = glue("N = {number(total_n, big.mark = ',')}"), 
        x = NULL,
        y = NULL) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    theme(axis.line = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text.x = element_blank())

p0

## total medals by country -----------------------------------------------

# total medal overview in top5 countries across time
# with host country indication
# local (country) medal max across years
# global (across top 5 countries) medal max by year

p1 = total_country %>% 
    ggplot(aes(x = year, y = n, color = medal, fill = medal)) +
    facet_wrap(~ factor(country), nrow = 1) +
    geom_vline(data = top5_host, 
               aes(xintercept = year),
               color = mid_gray, 
               linetype = "dashed") +
    geom_area(show.legend = FALSE) + 
    geom_hline(yintercept = seq(100, 400, 100),
               color = "white", 
               linetype = "dotted") +
    geom_point(
        data = total_country %>% 
            filter(local_max == TRUE) %>% 
            distinct(year, country, total, local_max),
        aes(y = total),
        shape = 21, size = 4, fill = NA, color = light_text
               ) +
    geom_point(
        data = total_country %>% 
            filter(global_max == TRUE) %>% 
            distinct(year, country, total, global_max),
        aes(y = total),
        shape = 21, size = 1, fill = light_text, color = light_text
    ) +
    geom_label(data = total_country %>% 
                  filter(local_max == TRUE) %>% 
                  distinct(year, country, total, global_max), 
              inherit.aes = FALSE,
              aes(x = year, 
                  y = total, 
                  label = paste0(year, ": ", total)),
              nudge_x = 25,
              family = text_sans, 
              color = light_text) +
    labs(subtitle = glue(
        "Countries win more medals when they host the games (dashed lines). ", 
        "Large circles show best year for each country, small circles show ", 
        "best country by year; e.g., in 1904 the U.S. won its most medals ",
        "between 1896 and 2016 (large circle), and won more medals than any ",
        "of the other countries in the top 5 (small circle)."), 
         x = NULL, 
         y = NULL
         ) +
    scale_color_manual(values = lighten(paste(medal_cols_dk), 0.3)) +
    # scale_fill_manual(values = medal_cols_lt) +
    scale_fill_manual(values = medal_cols_dk) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       breaks = scales::pretty_breaks(4)) +
    scale_x_continuous(breaks = scales::pretty_breaks(7)) +
    theme(axis.line.y = element_blank(), 
          axis.ticks.y = element_blank())

p1


## M/F by country ------------------------------------------------------

p2 = map(.x = top5_nocs$country, 
    .f = ~ {
        df_wide <- top10_sports %>% 
            filter(country == .x) %>% 
            arrange(-total) %>% 
            mutate(sports_rank = row_number())
        
        ylabs <- df_wide %>% 
            mutate(ylab = paste0(sports_rank, ". ", sport)) %>% 
            select(sport, ylab) %>% 
            as.list(pivot_wider(sport, ylab))
        
        df_long <- df_wide %>%
            filter(country == .x) %>% 
            pivot_longer(cols = -c(country, sport, sports_rank), 
                         names_to = "group", 
                         values_to = "n"
            ) %>% 
            arrange(sports_rank, sport, -n) %>% 
            mutate(.by = sport, rank = row_number()) %>% 
            mutate(hjust = case_when(rank == 1 ~ 0, 
                                       rank == 2 ~ 0.5, 
                                       rank == 3 ~ 1))
        
        p <- df_wide %>%
            filter(country == .x) %>% 
            ggplot(aes(y = reorder(sport, total), x = total)) +
            geom_vline(xintercept = 0, color = mid_gray, linetype = "dotted") +
            geom_linerange(aes(xmin = `F`, xmax = M), 
                           # lwd = 1, 
                           color = mid_gray) +
            geom_point(data = df_long, 
                       inherit.aes = FALSE, 
                       show.legend = FALSE,
                       aes(y = sport,
                           x = n, 
                           fill = group, 
                           size = group),
                       shape = 21, 
                       color = light_text,
                       alpha = 0.9) +
            scale_fill_manual(values = list("total" = mid_gray,
                                             "F" = light_text,
                                             "M" = "white")) +
            scale_size_manual(values = c(3, 3, 2)) +
            scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
            scale_y_discrete(expand = expansion(mult = c(0.05, 0.16)),
                             label = rev(ylabs$ylab)) +
            geom_text(aes(label = number(total, big.mark = ",")),
                      family = text_sans,
                      color = light_text,
                      nudge_y = 0.5,
                      size = 3) +
            # geom_text(aes(label = number(total, big.mark = ","), 
            #               nudge_y = if_else(str_detect(sport, "1"), 0, 0.5),
            #               nudge_x = if_else(str_detect(sport, "1"), 0, 20)), 
            #           family = text_sans, 
            #           color = light_text, 
            #           # nudge_y = 0.5, 
            #           size = 3) +
            labs(# subtitle = .x,
                 x = NULL, 
                 y = NULL) +
            theme(axis.line = element_blank(), 
                  axis.ticks = element_blank(), 
                  axis.text.x = element_blank())
        
        if(.x == "United States") {
            p +
               geom_text(data = df_long %>% 
                              filter(country == .x, 
                                     sports_rank == 1), 
                          inherit.aes = FALSE, 
                         aes(x = n, 
                             y = sport, 
                             label = case_when(group == "total" ~ "Total",
                                               group == "M" ~ "Men",
                                               group == "F" ~ "Women"),
                             hjust = case_when(group == "total" ~ 0.5,
                                               group == "M" ~ 0.8,
                                               group == "F" ~ 0.3)), 
                          family = text_sans, 
                          color = dark_text, 
                          nudge_y = 1.2, 
                          size = 3.5) +
                labs(subtitle = "Medals won in top 10 sports")
        } else {p}
    }
) %>% wrap_plots(nrow = 1) +
    plot_annotation(
        subtitle = "Number of medals won by gender in each country's top 10 sports"
        )

p2


## gender x age over time --------------------------------------------------

dat = olympics %>% 
    filter(!is.na(medal),
           season == "Summer", 
           noc %in% top5_nocs$noc) %>%
    left_join(top5_nocs) %>% 
    mutate(country = factor(country, levels = top5_nocs$country)) 
    
p3 = dat %>% filter(sex == "F") %>% 
    ggplot(aes(x = year, y = age, color = medal)) +
    facet_wrap(~ factor(country), nrow = 1)  +
    geom_point(data = dat %>% filter(sex == "M"), 
               # inherit.aes = FALSE,
               color = "white",
               # color = "gray98",
               # color = light_gray,
               size = 1, 
               alpha = 0.3) +
    geom_point(alpha = 0.6,
               show.legend = FALSE)  +
    scale_color_manual(values = medal_cols_dk) + 
    scale_x_continuous(breaks = scales::pretty_breaks(7)) +
    labs(subtitle = glue(
        "Medals won by women (colored) compared to men (white) by age over time: ", 
        "Female medal contribution increases with time"), 
        x = NULL, 
        y = NULL) +
    theme(
        strip.text = element_blank()
    )
  
p3

    
p4 = map(.x = c("F", "M"), .f = ~ {
    df <- olympics %>%
        filter(!is.na(medal),
               season == "Summer",
               noc %in% top5_nocs$noc,
               sex == .x)
    df_n <- nrow(df)
    age_max <- df %>% tabyl(age) %>% slice_max(order_by = n)
    
        
        # age freq by medal across all athletes in top5 countries
    df1 <- df %>% tabyl(age, medal) %>%
        pivot_longer(cols = -age,
                     names_to = "medal",
                     values_to = "n") %>% 
        mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze")))
    
    p <- df1 %>%
        
        ggplot(aes(x = age, y = n, fill = factor(medal))) +
        geom_col(show.legend = FALSE, width = 0.8) +
        geom_point(
            data = age_max,
            aes(y = n),
            shape = 21, size = 4, fill = NA, color = light_text
        ) +
        geom_label(data = age_max,
                  inherit.aes = FALSE, 
                  aes(x = age, y = n,
                      label = paste0("Peak age: ", age_max$age, " years")),
                  family = text_sans, 
                  color = light_text, 
                  nudge_x = 2, 
                  hjust = 0) +
        annotate("text", 
                 x = 11, 
                 y = age_max %>% pull(n), 
                 label = if_else(.x == "F", "Women", "Men"),
                 hjust = 0,
                 family = text_serif, 
                 color = dark_text) +
        labs(caption = glue::glue("N = {number(df_n, big.mark = ',')}"),
             x = NULL, 
             y = NULL) +
        # scale_fill_manual(values = medal_cols_lt) +
        scale_fill_manual(values = medal_cols_dk) +
        scale_x_continuous(expand = expansion(mult = c(0.01, 0.1)),
                           breaks = pretty_breaks(10), 
                           limits = c(10, 75)) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                           breaks = pretty_breaks(7), 
                           # limits = c(0, 900)
                           ) +
        theme(panel.grid.major.x = element_line(linetype = "dotted",
                                                color = "white"))
    
    if(.x == "F") {
        p + labs(
            subtitle = "Peak medal winning age differs slightly between women and men"
            )} else {p}
}) %>%
    wrap_plots(nrow = 2)

p4


## patchwork -------------------------------------------------------------

wrap_plots(
    ncol = 2,
    widths = c(3.8, 1),
    wrap_plots(nrow = 3, p1, p3, p2),
    wrap_plots(nrow = 2, p0, p4, heights = c(1, 2))
) + plot_annotation(
    title = "Olympic Medals: Summer Olympics 1896 to 2016",
    caption = glue(
        "Source: Kaggle | ",
        "#TidyTuesday week 32, August 6, 2024 | ",
        "Visualization by Claudia Laurenzano | ",
        "Created in R"
    ),
    theme = theme(
        plot.title = element_textbox_simple(
            color = darken(medal_cols_dk$Gold, 0.2),
            margin = margin(0, 0, 12, 0),
            halign = 0.5
        )
    )
)

# ggsave(here("olympics", "output", "olympics_pw_CL.svg"), plot = last_plot(), 
#        bg = "#FFEECD", height = 10.4, width = 19.9)
# 
# ggsave(here("olympics", "output", "olympics_pw_CL.svg"), plot = last_plot(), 
#        bg = "#FFEECD", height = 9.4, width = 19.5)
