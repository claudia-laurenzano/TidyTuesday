
# ABOUT -------------------------------------------------------------------

# ggclub Dec 2025 (12/12/2025)
# tidytuesday data from week 52 (2024-12-24)

# Sources:
#
# Lai S., Sorichetta A. and WorldPop (2020). Global Public and School Holidays
# 2010-2019. Mapping seasonal denominator dynamics in low- and middle-income
# settings, and Exploring the seasonality of COVID-19, funded by The Bill and
# Melinda Gates Foundation.
#
# Lai S., Sorichetta A. and WorldPop (2020). Monthly volume of airline
# passengers in 90 countries 2010-2018. Mapping seasonal denominator dynamics in
# low- and middle-income settings, and Exploring the seasonality of COVID-19,
# funded by The Bill and Melinda Gates Foundation.



# SETUP -------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(scales)
library(patchwork)
library(glue)
library(ggtext)
library(cowplot)

tuesdata <- tidytuesdayR::tt_load('2024-12-24') # 2024, week = 52
global_holidays <- tuesdata$global_holidays %>% clean_names()
monthly_passengers <- tuesdata$monthly_passengers %>% clean_names()



# DATA EXPLORATION --------------------------------------------------------

# glimpse data
global_holidays %>% glimpse
monthly_passengers %>% glimpse


# DEU: air traffic mostly driven by international flights
# USA: air traffic mostly driven by domestic flights
# more economical to drive/take train in small country (DEU), USA is much bigger
# most Germans I know have a passport and travel internationally, lots of Americans
# I know do not and have not
# (Nov), Dec: do people fly for (Thanksgiving and) Christmas? less than average;
# DEU: see above; USA: more than half of Americans live within an hour of family
# (https://www.pewresearch.org/short-reads/2022/05/18/more-than-half-of-americans-live-within-an-hour-of-extended-family/)
# short URL: https://pewrsr.ch/3yKn2ms
# https://www.nytimes.com/interactive/2015/12/24/upshot/24up-family.html#:~:text=According%20to%20an%20analysis%20of%20data%20from,for%20living%20near%20home%20is%20family%20ties.
# most activity in spring/summer



# DATA WRANGLING ----------------------------------------------------------

# filter for public holidays
annual_public_holidays = global_holidays %>% 
    filter(type == "Public holiday") %>% 
    mutate(year = year(date), 
           month_lab = month(date, label = TRUE)) %>% 
    summarize(.by = c(iso3, adm_name, year, month_lab, type), n = n()) %>% 
    mutate(.by = c(iso3, adm_name, year), total = sum(n)) %>% 
    # add 0 for missing months
    group_by(iso3, adm_name, year, type, total) %>% 
    complete(month_lab, fill = list(n = 0)) %>% 
    ungroup()


# passengers
passengers = monthly_passengers %>% 
    mutate(across(c(total:last_col()), ~ . * 1000)) %>% 
    left_join(annual_public_holidays %>% distinct(iso3, adm_name))



# create grid for flight data
grid = expand_grid(
    "type" = c("domestic", "international", "total"), 
    "iso" = c("DEU", "USA"))



# PLOT PREP ---------------------------------------------------------------

# colors from viridis::rocket(n = 6)
theme_set(
    theme_void(
        base_family = "IBM Plex Sans Condensed", 
        ink = "#4C1D4BFF",
        accent =  "#E83F3FFF"
    ) + 
        theme(
            axis.text = element_text(), 
            plot.title = element_textbox_simple(), 
            plot.subtitle = element_textbox_simple(margin = margin_auto(4)), 
            plot.caption = element_textbox_simple(),
            plot.margin = margin_auto(8)
            )
)

# caption
source = glue(
    "Source: ",
    "Lai, S., Sorichetta, A., Steele, J. et al. ",
    "Global holiday datasets for understanding seasonal human mobility ",
    "and population dynamics. ",
    "Sci Data 9, 17 (2022). "
    )

tt = glue(
    "#TidyTuesday week 52 ({format(ymd('2024-12-24'), '%D')})"
)

creator = glue(
    "Visualization by Claudia Laurenzano | ",
    "{format(today(), '%D')} | ",
    "Created in R"
)

cap = glue(
    "{source} | {tt} | {creator}"
)


# PLOTS -------------------------------------------------------------------

## p1 flights 2018 ----------------------------------------------------------
# passengers in 2018 by month: domestic, international, total

df1 = passengers %>% 
    filter(year == 2018, iso3 %in% unique(grid$iso)) %>% 
    summarize(.by = c(iso3, year), 
              across(c(total, domestic, international), sum)) %>% 
    pivot_longer(cols = c(domestic, international), 
                 names_to = "type", 
                 values_to = "passengers") %>% 
    mutate(
        prop = passengers/total, 
        prop_pretty = percent(prop, accuracy = 1L)) %>% 
    mutate(.by = iso3, max = prop == max(prop))
    

sub1 = glue(
    "**Air passengers in Germany (DEU) and the U.S. (USA) in 2018.** ",
    "In Germany, total air traffic is largely driven by international flights ",
    "({df1 %>% filter(iso3 == 'DEU', max == TRUE) %>% pull(prop_pretty)}), while ", 
    "in the U.S., most flights are domestic ",
    "({df1 %>% filter(iso3 == 'USA', max == TRUE) %>% pull(prop_pretty)}",
    "). Flight volume is above annual average from April through October in both ", 
    "countries--however, U.S. numbers dip below average in September when the ",
    "school year starts. ", 
    "Major family holidays such as Thanksgiving in the U.S. (end of November) and ", 
    "Christmas in both countries (end of December) do not increase flight traffic ", 
    "in either country. Germany's relatively small geographic size compared to the U.S. ", 
    "makes driving or rail travel more practical. Most Americans live within a few ", 
    "hours of family, reducing the need to fly for holiday family gatherings. "
)

p1 = map2(.x = grid$type,
     .y = grid$iso, 
     .f = ~ {
         df <- passengers %>% 
             filter(iso3 == .y, year == 2018) %>% 
             mutate(month_lab = month(month, label = TRUE)) %>% 
             pivot_longer(cols = c(domestic, international, total), 
                          names_to = "type", 
                          values_to = "passengers") %>% 
             mutate(.by = c(iso3, year, type), mean_passengers = mean(passengers)) %>% 
             mutate(comp = if_else(passengers >= mean_passengers, "above", "below")) %>% 
             filter(type == .x)
         
         unique_mean <- df %>% pull(mean_passengers) %>% unique
         annot_mean <- grid %>% 
             mutate(text = if_else(
                 iso == "DEU" & type == "domestic", 
                 "avg. ann. passengers", 
                 NA)) %>% 
             filter(iso == .y & type == .x) %>% 
             pull(text)
         
         df %>% 
             ggplot(aes(x = month_lab, y = passengers)) +
             annotate(
                 "text",
                 x = "Jan", 
                 y = Inf, 
                 label = glue("{.y}: {.x}"), 
                 fontface = if_else(.x == "total", "bold", "plain"),
                 vjust = 2, 
                 hjust = 0) +
             annotate(
                 "segment", 
                 x = "Jan", xend = "Dec", 
                 y = unique_mean, 
                 color = "#E83F3FFF",
                 linetype = "dotted"
             ) +
             annotate(
                 "text",
                 x = "Mar", 
                 y = unique_mean, 
                 label = annot_mean, 
                 color = "#E83F3FFF",
                 vjust = 1.5, 
                 hjust = 0, 
                 size = 3) +
             geom_ribbon( 
                 aes(ymin = mean_passengers, ymax = passengers), fill = NA) +
             geom_line(aes(group = year)) +
             scale_y_continuous(labels = label_number(scale_cut = cut_short_scale()))
     }) %>% 
    wrap_plots(nrow = 3, ncol = 2) +
    plot_annotation(
        subtitle = sub1, 
        theme = theme(
            plot.subtitle = ggtext::element_textbox_simple(margin = margin_auto(4)),
            plot.caption = ggtext::element_textbox_simple())
        )

p1


## p2 flights over time ------------------------------------------------------
# total passengers by month and year

sub2 = glue(
    "**Total monthly air traffic in Germany (DEU) and the U.S. (USA) between ",
    "2010 and 2018** (domestic and international passengers combined). ",
    "Passenger volume is highest in warmer months and increases over the years ", 
    "in both countries. ")

p2 = map(.x = c("DEU", "USA"), 
    .f = ~ monthly_passengers %>% 
        filter(iso3 == .x) %>%
        mutate(month_lab = month(month, label = TRUE)) %>%
        ggplot(aes(y = year, x = month_lab, fill = total)) +
        geom_tile() +
        scale_fill_viridis_c(
            option = "rocket",
            guide = guide_legend(reverse = TRUE), 
            labels = label_number(scale_cut = cut_short_scale()), 
            name = glue("{.x}: total")) + 
        scale_y_continuous(
            breaks = pretty_breaks(n = 10), 
            expand = expansion(mult = c(0, 0.01)), 
            position = if_else(.x == "DEU", "left", "right")
            ) +
        guides(fill = guide_legend(nrow = 1)) +
        coord_fixed() +
        theme(legend.position = "top", 
              legend.title = element_text(face = "bold"), 
              legend.title.position = "top", 
              axis.text = element_text(margin = margin_auto(4)))
) %>% wrap_plots +
    plot_annotation(
        subtitle = sub2
    )

p2


## p3 holidays ----------------------------------------------------------------
# total holidays in 2018

sub3 = glue(
    "**Number of public holidays in Germany (DEU) and the U.S. (USA) in 2018.** ",
    "Note that in Germany, public holidays are legally equivalent to Sundays-- ", 
    "most businesses must close and the vast majority of workers have the day off. ", 
    "In the U.S., federal holidays require closures and paid leave only for ", 
    "federal government employees; private employers are not legally required ", 
    "to close or provide time off, so many businesses remain open and many workers ", 
    "treat these days like regular work days. "
)

p3 = map(.x = c("DEU", "USA"),
    .f = ~ {
        df <- annual_public_holidays %>%
            filter(iso3 == .x, type == "Public holiday", year == 2018)

        df %>%
            ggplot(aes(y = "2018", x = month_lab, fill = n)) +
            geom_tile() +
            scale_fill_viridis_c(
                option = "rocket",
                guide = guide_legend(reverse = TRUE),
                name = "Holidays per month"
            ) +
            guides(fill = guide_legend(direction = "horizontal")) +
            scale_y_discrete(expand = expansion(mult = c(0, 0.2))) +
            labs(subtitle = glue("**{.x}**: total = ", unique(df$total))) +
            coord_fixed() +
            theme_sub_axis_x(text = element_text(margin = margin_auto(4))) +
            theme_sub_axis_y(
                title = element_blank(),
                text = element_blank(),
                ticks = element_blank()
            )
    }) %>%
    wrap_plots() +
    plot_layout(guides = "collect") +
    plot_annotation(
        subtitle = sub3,
        theme = theme(
            legend.position = "top"
    ))

p3


## headers ----------------------------------------------------------------

sub = glue(
    "Air travel patterns in both Germany and the United States of America are ", 
    "shaped primarily by seasonal vacation behavior rather than by public ", 
    "holidays. Public holidays--despite being culturally important--do not produce ", 
    "major increases in air travel in either country. "
    )

header = ggplot() +
    geom_blank() +
    labs(
        title = "Public Holidays and Air Travel in Germany and the U.S.", 
        subtitle = sub) +
    theme(
        plot.title = element_textbox_simple(
            size = 30, margin = margin(16, 8, 2, 8)),
        plot.subtitle = element_textbox_simple(size = 16, margin = margin_auto(8))
        )

footer = ggplot() +
    geom_blank() +
    labs(caption = cap)


## pw --------------------------------------------------------------------

pw = plot_grid(
    nrow = 2, 
    rel_heights = c(25, 1),
    plot_grid(
        ncol = 2, 
        plot_grid(
            header, p3, p2, 
            ncol = 1, 
            rel_heights = c(1.1, 1.2, 2.1), 
            rel_widths = c(1, 1, 1)
            ), 
        p1
        ), 
    footer)

pw


# EXPORT ------------------------------------------------------------------

ggsave(here("holiday travel", "CL_holiday_travel.png"), plot = pw, 
       bg = "white", # to avoid artifact lines from plot_grid()
       height = 9, width = 11*1.5)
