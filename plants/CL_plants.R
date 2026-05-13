# ABOUT -------------------------------------------------------------------

# TidyTuesday week 5 (2026-02-03)
# https://github.com/rfordatascience/tidytuesday/blob/main/data/2026/2026-02-03/readme.md#edible-plants-database

# Edible Plants Database
#
# This week we're exploring edible plants! The Edible Plant Database (EPD) is an
# outcome of the GROW Observatory, a European Citizen Science project on growing
# food, soil moisture sensing and land monitoring. It contains information on
# 146 edible plant species, including their ideal growing conditions and time to
# harvest and germination.
#
# The Edible Plant Database provides data based on geographical location and
# growing season to answer questions such as "What can I plant now" and "what
# can I plant that will yield a crop on some future date". Do plants that
# require more sunlight also require higher temperatures? What cultivation
# classes require the most water? Thank you to Nicola Rennie for curating this
# week's dataset.


# SETUP -------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(ggpomological)
library(scales)
library(patchwork)
library(glue)
library(gt)


plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-02-03/edible_plants.csv')



# DATA EXPLORATION --------------------------------------------------------

plants %>% glimpse
plants %>% count(soil)
plants %>% count(sunlight)
plants %>% count(water)
plants %>% count(temperature_class)








# FUNCTIONS ---------------------------------------------------------------

score_plants = function(df, n_winners = 3, ties = TRUE) {
    
    # temperature hardiness ----
    df1 <- df %>% 
        mutate(
            temperature_class = if_else(temperature_class == "Very hard", "Very hardy", temperature_class),
            temperature_fct = as_factor(temperature_class) %>% 
                fct_relevel("Very hardy", "Hardy", "Half hardy", "Tender", "Very tender"),
            score = case_when(
                temperature_fct == "Very hardy" ~ 5, 
                temperature_fct == "Hardy" ~ 4, 
                .default = 0)
        )
    
    # seasonality ----
    df2 <- df1 %>% 
        mutate(
            season = case_when(
                str_detect(season, regex("annual", ignore_case = TRUE)) ~ "annual", 
                str_detect(season, regex("biennial", ignore_case = TRUE)) ~ "biennial", 
                season == "Shrub" ~ "shrub", 
                .default = "perennial"
            ), 
            new_score = if_else(season %in% c("shrub", "perennial"), 5, 0), 
            score = score + new_score
        )
    
    # sensitivities ----
    df3 <- df2 %>% 
        mutate(
            sensitive = !is.na(sensitivities),
            new_score = if_else(sensitive, 0, 5), 
            score = score + new_score
        )
    
    # water ----
    df4 <- df3 %>% 
        mutate(
            water = str_to_lower(water) %>% as_factor() %>% fct_relevel(
                "very low", "low", "medium", "high", "very high"
            ), 
            new_score = case_when(
                water == "very low" ~ 5, 
                water == "low" ~ 4, 
                water == "medium" ~ 3, 
                .default = 0
            ), 
            score = score + new_score)
    
    # ph range ----
    df5 <- df4 %>% 
        mutate(
            ph_range = preferred_ph_upper - preferred_ph_lower, 
            new_score = case_when(
                ph_range >= 3 ~ 5, 
                ph_range == 2.5 ~ 4, 
                ph_range == 2 ~ 3, 
                .default = 0
            ), 
            score = score + new_score
        )
    
    # nutrients ----
    df6 <- df5 %>% 
        mutate(
            nutrients = if_else(
                str_detect(nutrients, regex("high", ignore_case = TRUE)), 
                "high", 
                str_to_lower(nutrients)
            ) %>% as_factor %>% fct_relevel("high", "medium", "low"), 
            new_score = case_when(
                nutrients == "low" ~ 5, 
                nutrients == "medium" ~ 3, 
                .default = 0
            ), 
            score = score + new_score
        )
    
    # min days to harvest ----
    df7 <- df6 %>% 
        separate_wider_delim(
            days_harvest, 
            delim = "-", 
            names = c("harvest_lower", "harvest_upper"), 
            too_few = "align_start"
        ) %>% 
        mutate(
            harvest_lower = as.numeric(harvest_lower),
            new_score = case_when(
                harvest_lower <= 30 ~ 5,
                harvest_lower <= 60 ~ 4,
                harvest_lower <= 90 ~ 3,
                .default = 0
            ),
            score = score + new_score
        )
    
    # winners ----
    top3 <- df7 %>% slice_max(score, n = n_winners, with_ties = ties)
    
    # output ----
    list(
        "top3" = top3,
        "temperature" = df1, 
        "season" = df2, 
        "sensitivities" = df3, 
        "water" = df4, 
        "ph" = df5, 
        "nutrients" = df6, 
        "harvest" = df7
    ) %>% 
        map(~ mutate(., sp_name = if_else(
            taxonomic_name %in% top3$taxonomic_name & common_name %in% top3$common_name, 
            taxonomic_name, 
            "other") %>% 
                as_factor %>% 
                fct_relevel(., "other", after = Inf)))
}

plot_jitter = function(df, yvar, sub, col_pal = pal, size_pal = size_sp) {
    df %>% 
        ggplot(aes(y = {{yvar}}, x = 1)) +
        geom_jitter(
            width = 0.1, 
            height = 0.1,
            aes(color = sp_name, size = sp_name)
        ) +
        scale_color_manual(values = col_pal, name = "Species") +
        scale_size_manual(values = size_pal, guide = "none") +
        scale_y_discrete(labels = str_to_sentence) +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(x = NULL, y = NULL, subtitle = sub) +
        theme_sub_axis_x(
            line = element_blank(), 
            text = element_blank()
        )
}
    
plot_beeswarm = function(df, yvar, sub, col_pal = pal, size_pal = size_sp) {
    df %>% 
        ggplot(aes(y = {{yvar}}, x = "a")) +
        ggbeeswarm::geom_beeswarm(
            # width = 0.1, 
            # height = 0.1,
            corral = "random", 
            corral.width = 0.9,
            cex = 2.5, 
            aes(color = sp_name, size = sp_name)
        ) +
        scale_color_manual(values = col_pal, name = "Species") +
        scale_size_manual(values = size_pal, guide = "none") +
        scale_y_discrete(labels = str_to_sentence) +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(x = NULL, y = NULL, subtitle = sub) +
        theme_sub_axis_x(
            line = element_blank(), 
            text = element_blank()
        )
}
    
plot_metrics = function(list) {
    
    # colors ----
    pal <- deframe(
        list$top3 %>% select(taxonomic_name) %>%
            mutate(
                col = rep_len(
                    c(
                        "#c03728",
                        "#919c4c",
                        "#fd8f24",
                        "#f5c04a",
                        "#e68c7c",
                        "#c3c377"
                    ),
                    n()
                )
            ) %>%
            add_row(taxonomic_name = "other", col = "#a89985")
    )
    
    # size ----
    size_sp <- deframe(
        list$top3 %>% select(taxonomic_name) %>%
            mutate(size = 4) %>% 
            add_row(taxonomic_name = "other", size = 2)
    )
    
    # temperature ----
    df <- list$temperature
    p1 <- df %>% plot_jitter(
        col_pal = pal,
        size_pal = size_sp,
        yvar = fct_rev(temperature_fct),
        sub = "Temperature"
    )
    
    # seasonality ----
    df <- list$season
    p2 <- df %>% plot_jitter(
        col_pal = pal,
        size_pal = size_sp,
        yvar = season,
        sub = "Seasonality"
    )
    
    # sensitivities ----
    df <- list$sensitivities
    p3 <- df %>% plot_jitter(
        col_pal = pal,
        size_pal = size_sp,
        yvar = sensitive,
        sub = "Sensitivity"
    ) +
        scale_y_discrete(labels = c("No", "Yes"))
    
    # water ----
    df <- list$water
    p4 <- df %>% plot_jitter(
        col_pal = pal,
        size_pal = size_sp, yvar = water, sub = "Water"
    )
    
    # ph range ----
    df <- list$ph %>% arrange(ph_range, preferred_ph_lower) %>% 
        mutate(taxonomic_name = factor(taxonomic_name, levels = unique(taxonomic_name)))
    p5 <- df %>% ggplot(aes(x = taxonomic_name)) +
        geom_linerange(
            aes(ymin = preferred_ph_lower, ymax = preferred_ph_upper, color = sp_name), 
            show.legend = FALSE) +
        geom_point(aes(y = preferred_ph_lower, color = sp_name, size = sp_name), show.legend = FALSE) +
        geom_point(aes(y = preferred_ph_upper, color = sp_name, size = sp_name)) +
        scale_color_manual(values = pal, name = "Species") +
        scale_size_manual(values = size_sp, guide = "none") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(x = NULL, y = NULL, subtitle = "pH range") +
        theme_sub_axis_x(
            line = element_blank(), 
            text = element_blank()
        )
    
    # nutrients ----
    df <- list$nutrients
    p6 <- df %>% plot_jitter(col_pal = pal, 
                             size_pal = size_sp, yvar = fct_rev(nutrients), sub = "Nutrients")
    
    
    # min days to harvest ----
    df <- list$harvest %>% 
        mutate(.by = harvest_lower, y = row_number()) %>% 
        drop_na(harvest_lower)
    p7 <- df %>% ggplot(aes(x = harvest_lower, y = "a")) +
        ggbeeswarm::geom_beeswarm(
            cex = 2.5, corral = "random", corral.width = 0.9, 
            aes(color = sp_name, size = sp_name)) +
        scale_color_manual(values = pal, name = "Species") +
        scale_size_manual(values = size_sp, guide = "none") +
        guides(color = guide_legend(override.aes = list(size = 4))) +
        labs(x = NULL, y = NULL, subtitle = "Min days to harvest") +
        theme_sub_axis_y(text = element_blank())
    
    
    # output ----
    list(
        "temperature" = p1, 
        "season" = p2, 
        "sensitivities" = p3, 
        "water" = p4, 
        "ph" = p5, 
        "nutrients" = p6, 
        "harvest" = p7
    )
    
}


# DATA WRANGLING ----------------------------------------------------------
dat = score_plants(plants, n_winners = 3, ties = TRUE)



# PLOT PREP ---------------------------------------------------------------

theme_set(
    theme_pomological_fancy(base_family = "Just Another Hand", base_size = 20)
    ) 

light_col = "#a89985" # pomological_base$medium_line
text_col = "#2b323f" # pomological_base$dark_blue
paper_col = "#fffeea" # pomological_base$paper

theme_pomological()

cap = glue(
    "Source: GROW Observatory | ", 
    "#TidyTuesday week 5 ({format(ymd('2026-02-03'), '%B %d, %Y')}) | ", 
    "Visualization by Claudia Laurenzano | ", 
    "{format(today(), '%B %d, %Y')} | ", 
    "Created in R")


# PLOTS -------------------------------------------------------------------

plots = dat %>% plot_metrics()
pw = wrap_plots(c(guide_area(), plots), nrow = 3, ncol = 3) +
    plot_layout(guides = "collect", design = "
                ABE
                FFG
                CDH") 

tab = dat$top3 %>%
    select(
        taxonomic_name, 
        common_name, 
        cultivation, 
        sunlight, 
        water, 
        ph_range, 
        nutrients, 
        soil, 
        "temperature" = temperature_fct, 
        "days to harvest" = harvest_lower
    ) %>% 
    mutate(across(c(common_name:last_col()), ~ str_to_sentence(.))) %>%
    mutate(across(everything(), ~ replace_na(., ""))) %>% 
    rename_with(str_to_sentence) %>% 
    rename_with(~ str_replace(., "_", " ")) %>% 
    gt() %>% 
    opt_table_font(font = "Just Another Hand", color = text_col) %>% 
    tab_options(table.background.color = paper_col) %>% 
    tab_style(
        style = cell_text(weight = "bold"),
        locations = cells_column_labels()
    ) %>% 
    wrap_table()

sub = glue(
    "Want to grow your own veggies but don't have a green thumb? Try these ",
    "six plants: ",
    "<span style='color:#c03728; font-weight: bold;'>garlic</span>, ",
    "<span style='color:#919c4c; font-weight: bold;'>spinach</span>, ",
    "<span style='color:#fd8f24; font-weight: bold;'>onions</span>, ",
    "<span style='color:#f5c04a; font-weight: bold;'>purslane</span>, ",
    "<span style='color:#e68c7c; font-weight: bold;'>spring onions</span>, and ",
    "<span style='color:#c3c377; font-weight: bold;'>Swiss chard</span> ", 
    "don't care about temperature, aren't fussy about water, grow in a decent ",
    "pH range, don't demand nutrients, don't have sensitivities, and you can ", 
    "harvest within about two months. The best thing: they're perennials so ",
    "you'll only have to plant them once and they'll be back next season!"
    )

p_sub = tibble(x = "a", y = "a", text = sub) %>% 
    ggplot(aes(x, y)) +
    # geom_blank() +
    ggtext::geom_textbox(
        aes(label = text), 
        box.color = NA, 
        fill = NA,
        width = unit(30, "lines"), 
        height = unit(20, "lines"),
        vjust = 0.75,
        hjust = 0.5,
        family = "Just Another Hand", 
        size = 6.5, 
        color = text_col) +
    coord_cartesian(expand = FALSE, clip = "off") + 
    theme(
        plot.subtitle = ggtext::element_textbox_simple(),
        panel.border = element_blank(), 
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(), 
        axis.title = element_blank()
    )

top_row = wrap_plots(nrow = 1, widths = c(0.9, 1.1), p_sub, tab)

final = wrap_plots(nrow = 2, heights = c(1, 4), top_row, pw) +
    plot_annotation(
        title = "Hardy Harvests", 
        caption = cap, 
        theme = theme(plot.title = element_text(
            color = "#919c4c", 
            size = 48, 
            hjust = 0.5
        ))
    )




# NOTES ----
# 1. add background color to table (or remove bg from everything and add to ggsave())
# 2. add large title
# 3. add subtitle description (top left)
# 4. move table to top right
# 5. fix species order in legend
# 6. add caption



# # EXPORT ------------------------------------------------------------------
# 
ggsave(here("plants", "CL_plants.png"), final, bg = paper_col,
       height = 9, width = 14)

ggsave(here("plants", "CL_plants.svg"), final, bg = paper_col,
       height = 9, width = 14)
