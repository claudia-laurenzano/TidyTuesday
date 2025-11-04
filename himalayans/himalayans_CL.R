# SETUP -------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(here)
library(janitor)
library(scales)
library(colorspace)
library(ggpomological)
library(nord)
library(glue)
library(ggtext)
library(patchwork)

# tidy tuesday week 3, 2025-01-21

tuesdata <- tidytuesdayR::tt_load(2025, week = 3)
exped_tidy <- tuesdata$exped_tidy %>% clean_names
peaks_tidy <- tuesdata$peaks_tidy %>% clean_names

# https://github.com/rfordatascience/tidytuesday/blob/main/data/2025/2025-01-21



# EXPLORE -----------------------------------------------------------------

exped_tidy %>% glimpse

# nation
# bcdate, smtdate, smtdays, totdays, termdate: time
# success*/route*
# mdeaths and hdeaths
# achievement: timeline

peaks_tidy %>% glimpse

# peakid, pkname, heightm, himal_factor, region_factor, open, pstatus_factor,
# phost_factor, pyear, pseason, psmtdate, pcountry, psummiters

# map would be great but don't have coords
# timeline?



# DATA WRANGLING ----------------------------------------------------------


## Peaks -----------------------------------------------------------------

peaks = peaks_tidy %>% 
    select(peakid, pkname, heightm, himal_factor, region_factor, open, pstatus_factor,
           phost_factor, pyear, pseason, psmtdate, pcountry, psummiters, psmtnote) %>% 
    separate_wider_delim(psmtdate, 
                         delim = " ", 
                         names = c("summit_month", "summit_day"),
                         too_few = "align_start", 
                         cols_remove = FALSE) %>% 
    mutate(summit_day = if_else(!is.na(summit_month) & is.na(summit_day),
                                "01",
                                summit_day),
           summit_date = ymd(paste(pyear, summit_month, summit_day)))

peak_labels = peaks %>% 
    filter(
        heightm == max(heightm) |
            summit_date == min(summit_date, na.rm = TRUE) |
            summit_date == max(summit_date, na.rm = TRUE)
        ) %>% 
    mutate(
        lab = paste0(
            format(summit_date, "%b %d, %Y"), ": ", pkname, " (", heightm, "m)"
            ),
        lab = case_when(
            peakid == "LNPO" ~ paste("First recorded successful summit\n", lab), 
            peakid == "EVER" ~ paste("Highest peak\n", lab),
            peakid == "PATR" ~ paste("Latest recorded successful summit\n", lab)
        ),
        ypos = case_when(
            peakid == "LNPO" ~ 8400, 
            peakid == "EVER" ~ 11000,
            peakid == "PATR" ~ 8500
        ),
        hjust = c(0.5, 0.05, 0.9))

arrows = peak_labels %>% 
    select(peakid, summit_date, heightm, ypos) %>% 
    mutate(x2 = case_when(peakid == "EVER" ~ summit_date + years(12),
                          peakid == "LNPO" ~ summit_date + years(7), 
                          peakid == "PATR" ~ summit_date - years(3)))
    

buildings = tribble(
    ~ name, ~ date, ~ heightm, ~ hjust,
    "Eiffel tower", ymd("1909-01-01"), 300, 0.2,
    "Empire State Building", ymd("1931-01-01"), 443, 0.1,
    "Burj Khalifa", ymd("2010-01-01"), 828, 0.5
)

sides = peak_labels %>% 
    filter(str_detect(lab, "recorded")) %>% 
    select(summit_date, heightm) %>% 
    mutate(date1 = if_else(summit_date == min(summit_date), 
                           summit_date - years(5),
                           summit_date), 
           date2 = if_else(summit_date == min(summit_date), 
                           summit_date,
                           summit_date + years(5)))

peaks_counts = peaks %>% 
    summarize(.by = c(open, pstatus_factor), 
              n = n()) %>% 
    mutate(.by = open,  
           total = sum(n),
           ypos = if_else(pstatus_factor == "Climbed", n, total)) %>% 
    mutate(open = factor(if_else(open == TRUE, "Open", "Closed"), 
                         levels = c("Open", "Closed")),
           pstatus_factor = factor(pstatus_factor, 
                                   levels = c("Climbed", "Unclimbed")),
           prop = n/total, 
           lab = if_else(open == "Open", 
                         paste(percent(prop, accuracy = 1L), pstatus_factor),
                         percent(prop, accuracy = 1L)), 
           hjust = if_else(open == "Open", 1, 0), 
           nudge_x = case_when(
               open == "Open" & pstatus_factor == "Climbed" ~ 3,
               open == "Open" & pstatus_factor == "Unclimbed" ~ 3, 
               open == "Closed" & pstatus_factor == "Climbed" ~ 3,
               open == "Closed" & pstatus_factor == "Unclimbed" ~ 3, ))



## Expeds ----------------------------------------------------------------

# 2020 to 2024

# nation
# bcdate, smtdate, smtdays, totdays, termdate: time
# success*/route*
# mdeaths and hdeaths
# achievement: timeline

# seasons:  1 - spring: 3, 4, 5
#           2 - summer: 6, 7, 8
#           3 - autumn: 9, 10, 11
#           4 - winter: 12, 1, 2

seasons = tibble(
    month = seq(1, 12), 
    season = case_when(month %in% c(3, 4, 5) ~ 1,
                       month %in% c(6, 7, 8) ~ 2,
                       month %in% c(9, 10, 11) ~ 3, 
                       month %in% c(12, 1, 2) ~ 4),
    season_factor = case_when(season == 1 ~ "Spring", 
                              season == 2 ~ "Summer", 
                              season == 3 ~ "Autumn", 
                              season == 4 ~ "Winter")
)

# all expeditions
exped = exped_tidy %>% 
    select(expid, peakid, year, season, season_factor, 
           contains("route"),
           contains("success"),
           bcdate,
           smtdate, 
           smtdays,
           totdays, 
           termdate, 
           totmembers,
           tothired,
           mdeaths, 
           hdeaths, 
           achievment) %>% 
    rowwise() %>% 
    mutate(success = if_else(if_any(contains("success"), ~ . == TRUE),
                             "success", "fail"),
           bc_smt_date = if_else(is.na(bcdate) & !is.na(smtdate), 
                             smtdate, 
                             bcdate),
           month = month(bc_smt_date), 
           season_factor = factor(season_factor,
                                  levels = c("Spring", 
                                             "Summer",
                                             "Autumn", 
                                             "Winter"))) %>% 
    ungroup()


# total successful summits
top3_peaks_smt = exped %>% 
    filter(if_any(contains("success"), ~. == TRUE)) %>% 
    summarize(.by = peakid, n = n()) %>% 
    slice_max(order_by = n, n = 3)


# everest: total by month, success y/n
expeds_season = seasons %>% 
    expand_grid(year = seq(2020, 2024, 1)) %>% 
    left_join(
        exped %>%
            filter(!is.na(bc_smt_date)) %>%
            summarize(
                .by = c(year, month, season, season_factor, success),
                n_expeds = n()
            ) %>% 
            pivot_wider(names_from = success, 
                        values_from = n_expeds)
    ) %>% 
    mutate(across(c(success, fail), ~ replace_na(., 0)),
           total = success + fail,
           success_rate = if_else(total > 0, success / total, 0),
           season_factor = factor(season_factor,
                                  levels = c("Spring", 
                                             "Summer",
                                             "Autumn", 
                                             "Winter")))

expeds_season_sum = expeds_season %>% 
    summarize(.by = c(year, season_factor),
           n_season_year = sum(total)) %>% 
    mutate(.by = year, 
           n_year = sum(n_season_year)) %>% 
    mutate(.by = season_factor, 
           n_season = sum(n_season_year))
    # arrange(year)


top_seasons = expeds_season_sum %>% 
        distinct(season_factor, n_season) %>% 
        slice_max(order_by = n_season, n = 2) %>%
        mutate(lab = paste0(season_factor, " (n = ", n_season, ")")) %>% 
        pull(lab) %>% 
        knitr::combine_words()
    
top_years = expeds_season_sum %>% 
        distinct(year, n_year) %>% 
        slice_max(order_by = n_year, n = 2) %>%
        mutate(lab = paste0(year, " (n = ", n_year, ")")) %>% 
        pull(lab) %>% 
        knitr::combine_words()
    
top_season_years = expeds_season_sum %>% 
        slice_max(order_by = n_season_year, n = 1) %>%
        mutate(lab = paste0(season_factor, " ", year, " (n = ", n_season_year, ")")) %>% 
        pull(lab) %>% 
        knitr::combine_words()
    
top_success_month = expeds_season %>% 
        slice_max(order_by = total, n = 5) %>% 
        mutate(max_success_rate = max(success_rate),
               month_year = paste(month(month, label = TRUE, abbr = FALSE), year))

exp_arrows = top_success_month %>% 
    filter(year == 2021 | max_success_rate == success_rate) %>% 
    add_row(expeds_season %>% 
                filter(year == 2022, month == 9)) %>% 
    mutate(lab = case_when(year == 2021 ~ "Top 5 month", 
                           year == 2022 ~ "Total expeds.", 
                           year == 2024 ~ paste0(percent(success_rate), " success rate")),
           )
    


# PLOT PREP ---------------------------------------------------------------

# sysfonts::font_add(textfont, regular = "HomemadeApple-Regular.ttf")
sysfonts::font_add_google("Homemade Apple", family = "Homemade Apple")
showtext::showtext_auto()

textfont = "Homemade Apple"
textsize = 12

caption = glue(
    "Source: The Himalayan Database | ", 
    "#TidyTuesday week 3, {format(ymd('2025-01-21'), '%B %d, %Y')} | ",
    "Visualization by Claudia Laurenzano | ", 
    "Created in R"
)

# nord_palettes

col1 = "#4B4B4B"
col2 = "#222B4C"
col_lines = lighten("#4B4B4B", 0.4)

season_cols = list("Spring" = "#919c4c", 
                   "Summer" = "#f5c04a",
                   "Autumn" = "#fd8f24", 
                   "Winter" = "#828585")
    


# PLOT --------------------------------------------------------------------


## Peaks -----------------------------------------------------------------

peaks_tl = peaks %>% 
    ggplot(aes(x = summit_date, y = heightm)) +
    # mountain range
    geom_line(color = col1) +
    # mountain fill color
    geom_area(fill = "#ECEFF4", alpha = 0.5) +
    # side fill color
    geom_rect(data = sides, 
              inherit.aes = FALSE, 
              aes(xmin = date1, xmax = date2, ymin = 0, ymax = heightm), 
              fill = "#ECEFF4", alpha = 0.5) +
    # peak highlights
    geom_point(data = peak_labels, 
               aes(y = heightm),
               size = 2, color = col1) +
    # buildings height
    geom_linerange(data = buildings, 
                   aes(x = date, ymin = 0, ymax = heightm),
                   color = col1, 
                   lwd = 1.5) +
    # buildings text
    geom_text(
        data = buildings,
        aes(
            x = date,
            y = heightm,
            hjust = hjust,
            label = paste0(name, "\n", heightm, "m")
        ),
        vjust = -0.2,
        color = col1,
        family = textfont
    ) +
    # peak highlights text
    geom_text(
        data = peak_labels,
        aes(y = ypos, hjust = hjust, label = lab),
        vjust = -0.2,
        color = col1,
        family = textfont
    ) +
    # peak highlights connectors
    geom_curve(
        data = arrows, 
        aes(x = x2, xend = summit_date, 
            y = ypos + 200, yend = heightm + 200),
        arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
        curvature = -0.4, 
        size = 0.4,
        color = col1, 
        linetype = "dashed"
    ) +
    # cosmetics
    labs(y = "Peak height",
         x = "Date of first summit", 
         subtitle = glue("Himalayan mountain peak first-time summits by date ",
                         "and height. Height and opening date* of iconic buildings ",
                         "included for comparison."),
         caption = glue("*The Eiffel Tower was opened in 1889, outside of the
                        chart range.")) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.4)),
                       labels = label_number(big.mark = ",", suffix = "m"), 
                       breaks = pretty_breaks()) +
    scale_x_date(expand = expansion(mult = c(0, 0)), 
                 breaks = pretty_breaks()) +
    theme_pomological_fancy(base_family = textfont,
                            text.color = col1, 
                            base_size = textsize) +
    theme(
        text = element_text(family = textfont),
        axis.title.y = element_text(
            family = textfont,
            angle = 0, 
            hjust = 1, 
            margin = margin(t = 0, r = 10, b = 0, l = 0)),
            # margin = margin(t = -20, r = -20, b = 50, l = 0)),
        axis.title.x = element_text(family = textfont, hjust = 0),
        plot.title = element_text(family = textfont), 
        plot.subtitle = element_textbox_simple(family = textfont),
        plot.caption = element_text(family = textfont))
    
peaks_tl


peaks_ov = peaks_counts %>% 
    ggplot(aes(x = rev(open), y = n, fill = reorder(pstatus_factor, n))) +
    geom_col(color = col1, width = 0.4) +
    # text open/closed
    annotate("text", x = 2, y = -6, label = "Open: N = 385", family = textfont, color = col1, hjust = 0.8) +
    annotate("text", x = 1, y = -24, label = "Closed: N = 95", family = textfont, color = col1, hjust = 0.3) +
    # text climbed/unclimbed
    annotate("text", x = 2.5, y = 100, label = "75% Climbed", family = textfont, color = col1) +
    annotate("text", x = 2.5, y = 335, label = "25% Unclimbed", family = textfont, color = col1) +
    annotate("text", x = 0.5, y = 78, label = "82%", family = textfont, color = col1) +
    annotate("text", x = 1, y = 145, label = "18%", family = textfont, color = col1) +
    scale_fill_manual(values = c("transparent", col1)) +
    guides(fill = guide_legend(override.aes = list(alpha = c(0, 0.8)))) +
    labs(subtitle = glue("Status of Himalayan mountain range peaks\n (total = {nrow(peaks)})"),
         fill = "Peak status", 
         x = NULL, 
         y = NULL) +
    coord_polar(theta = "y", start = 4.35, clip = "off") +
    theme_pomological_fancy(base_family = textfont,
                            text.color = col1, 
                            base_size = textsize) +
    theme(axis.line = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          legend.position = "none", 
          plot.subtitle = element_textbox_simple(),
          panel.border = element_blank())




## Expeds ----------------------------------------------------------------

subtitle = glue("Between 2020 and 2024, the most popular seasons for mountaineering expeditions in the ",
                "Himalayans were {top_seasons}, and the years during most ",
                "expeditions were attempted were {top_years}. During the busiest ",
                "months (",
                "{top_success_month %>% pull(total) %>% range %>% knitr::combine_words(and = ' to ')} ",
                "expeditions), ",
                "{top_success_month %>% filter(max_success_rate == success_rate) %>% pull(month_year)} ",
                "had the highest success rate (",
                "{top_success_month %>% filter(max_success_rate == success_rate) %>% pull(success_rate) %>% percent}",
                ")."
                )

expeds_plot_seasons = expeds_season %>% 
    ggplot(aes(x = factor(month), fill = season_factor)) +
    geom_col(aes(y = total), alpha = 0.4) +
    geom_col(data = top_success_month, 
             aes(y = total),
             fill = NA, color = col1) +
    geom_col(aes(y = success), alpha = 0.75, width = 0.7) +
    geom_text(data = top_success_month %>% filter(year == 2021),
              aes(y = 0, label = "Top 5 month"),
              color = col1,
              family = textfont,
              hjust = 1,
              nudge_y = -15) +
    geom_text(data = top_success_month %>% filter(year == 2024),
              aes(y = 0,
                  label = paste(percent(success_rate),
                                         "success")
                  ),
              color = col1,
              family = textfont,
              hjust = 1.1,
              nudge_y = -5) +
    scale_x_discrete(labels = month(1:12, label = TRUE, abbr = TRUE)) +
    scale_fill_manual(values = season_cols) +
    guides(fill = guide_legend(override.aes = list(shape = 18))) +
    labs(x = NULL, 
         y = NULL, 
         fill = "Season", 
         subtitle = subtitle
         ) +
    coord_radial(theta = "x", 
                 start = 0,
                 direction = 1,
                 clip = "off",
                 r.axis.inside = TRUE,
                 inner.radius = 0.6) +
    facet_wrap(~ year, nrow = 1) +
    theme_pomological_fancy(base_family = textfont,
                            text.color = col1, 
                            base_size = textsize) +
    theme(legend.position = "right", 
          plot.subtitle = element_textbox_simple(),
          panel.border = element_blank())



# VIEW --------------------------------------------------------------------

pw = wrap_plots(
    nrow = 2,
    heights = c(1, 1.2),
    peaks_tl, 
    wrap_plots(
        ncol = 2,
        widths = c(1, 3.5),
        peaks_ov,
        expeds_plot_seasons
)) +
    plot_annotation(title = "Himalayan mountaineering expedtions", 
                    caption = caption,
                    theme = theme_pomological_fancy(base_family = textfont,
                                                    text.color = col1, 
                                                    base_size = textsize) +
                        theme(plot.title = element_textbox(family = textfont, 
                                                           size = 18,
                                                           face = "bold",
                                                           halign = 0.5,
                                                           hjust = 0.5)))


paint_pomological(pw, width = 2000, height = 1000, res = 110) %>%
    magick::image_write(path = here("himalayans", "himalayans_CL.png"))

paint_pomological(peaks_tl, width = 800, height = 500, res = 110)
paint_pomological(peaks_ov, width = 400, height = 500)



# EXPORT ------------------------------------------------------------------






# NOTES -------------------------------------------------------------------

# peaks_tidy: some dates are missing the day component, replaced with 01