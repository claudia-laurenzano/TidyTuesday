
# SETUP -------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(here)
library(glue)
library(ggrepel)
library(patchwork)

# tidy tuesday week 51, 2023-12-19

tuesdata <- tidytuesdayR::tt_load(2023, week = 51)
episodes <- tuesdata$holiday_episodes
genres <- tuesdata$holiday_episode_genres

# https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-12-19#data-dictionary

source(here("..", "..", "Central Data", "theme_BC_CL2.R"))
source(here("..", "..", "Central Data", "colors_BC_CL2.R"))


# EXPLORE -----------------------------------------------------------------

episodes %>% glimpse

episodes %>% 
    tabyl(parent_primary_title) %>% 
    filter(n > 4) %>% 
    arrange(-n) %>% 
    print(n = Inf)

# The Great British Baking Show 11
# South Park  8
# The Office  8
# The Simpsons  6

titles = c("The Great British Baking Show",
           "South Park",
           "The Office",
           "The Simpsons")

episodes_shows = episodes %>% 
    filter(parent_primary_title %in% titles)

map(.x = unique(episodes_shows %>% pull(parent_primary_title)),
    .f = ~ episodes_shows %>% 
        filter(parent_primary_title == .x) %>% 
        select(year, season_number, primary_title, runtime_minutes, average_rating, num_votes, parent_start_year)) %>% 
    purrr::set_names(unique(episodes_shows %>% pull(parent_primary_title)))

# global 
# title = "Holiday Episodes"
# x = year
# caption = source etc.

# subtitle = parent_primary_title
# x = season_number
# y = average_rating
# size = num_votes
# label = primary_title



# PLOT PREP ---------------------------------------------------------------

title = "Holiday Episodes"
subtitle = "The average rating and number of votes of holiday-themed episodes from four different shows"
caption = glue(
    "Source: IMDb ",
    "| #TidyTuesday week 51, {format(ymd('2023-12-19'), format = '%B %d, %Y')} ",
    "| Visualization by Claudia Laurenzano ", 
    "| {format(today(), format = '%B %d, %Y')}",
    "| Created in R"
)


## fonts -----------------------------------------------------------------

font_add_google("Fira Sans", family = "Fira Sans")
font_add("gbbs", regular = "MostraNuova.ttf")
font_add("southpark", regular = "southpark.ttf")
font_add("office", regular = "american-typewriter.ttf")
font_add("simpsons", regular = "Simpsonfont DEMO.otf")
showtext_auto()


## colors ----------------------------------------------------------------

# GBBS:
c("#81B3A8",
  "#7A89A2", 
  "#BC8C82", 
  "#C1A387", 
  "#F4F4F4", 
  "#DB848C",
  "#89091A",
  "#D0C7BD")

# southpark: 
c("#006643",
  "#FFDD38",
  "#00A8B7",
  "#7AD0F2",
  "#4A559F", 
  "#F6E5B8",
  "#58BA63", 
  "#E4313D", 
  "#F7692B"
)

# office:
c("#A9B6CE", 
  "#AA2130", 
  "#373547", 
  "#505665", 
  "#DBDAE0", 
  "#D1AC38",
  "#211D1E",
  "#26384F")

# simpsons:
c("#FFCF38", 
  "#0070B6", 
  "#F45235", 
  "#CFE398", 
  "#5BC8F1", 
  "#FFFFFF", 
  "#5D625C")



# PLOT --------------------------------------------------------------------

create_dot_plot = function(show, 
                           font_header, 
                           color_title,
                           color_header, 
                           color_body,
                           color_points,
                           color_bg) {
    
    df <- episodes_shows %>% 
        filter(parent_primary_title == show)
    
    top3 <- df %>% 
        slice_max(average_rating, n = 3)
    
    df %>% ggplot(aes(x = season_number, y = average_rating, size = num_votes)) +
        geom_point(color = color_points) +
        geom_text_repel(data = top3, 
                        aes(label = primary_title),
                        family = font_header,
                        color = color_header,
                        size = 4,
                        show.legend = FALSE) +
        scale_y_continuous(limits = c(6, 10)) +
        scale_x_continuous(breaks = pretty_breaks(7)) +
        scale_size_continuous(
            labels = label_number(big.mark = ",")) +
        labs(subtitle = show, 
             y = "Average episode rating", 
             x = "Season",
             size = "Number of votes") +
        theme_bc(title_font = font_header,
                 dark_text = color_header, 
                 light_text = color_body) +
        theme(
            plot.subtitle = element_textbox_simple(
                family = font_header,
                color = color_title,
                size = 26,
                face = "bold", 
                halign = 0.5
            ),
            plot.background = element_rect(fill = color_bg, 
                                           color = "white",
                                           linewidth = 3),
            legend.background = element_rect(fill = color_bg),
            axis.title.y = element_text(margin = margin(0, 0, 0, 4))
        )
}



## patchwork --------------------------------------------------------------

wrap_plots(nrow = 2,
           create_dot_plot(
               show = titles[1], 
               font_header = "gbbs", 
               color_title = "#89091A", 
               color_header = "#F4F4F4", 
               color_body = "#F4F4F4", 
               color_points = "#D0C7BD", 
               color_bg = "#81B3A8"
               ),

           create_dot_plot(
               show = titles[2], 
               font_header = "southpark", 
               color_title = "#F7692B", 
               color_header = "#006643", 
               color_body = "#006643",
               color_points = "#4A559F", 
               color_bg = "#7AD0F2"
               ),
           
           create_dot_plot(
               show = titles[3],
               font_header = "office",
               color_title = "#26384F",
               color_header = "#373547",
               color_body = "#373547",
               color_points = "#AA2130",
               color_bg = "#DBDAE0"
               ),

           create_dot_plot(
               show = titles[4],
               font_header = "simpsons",
               color_title = "#0070B6",
               color_header = "#5D625C",
               color_body = "#5D625C",
               color_points = "#F45235",
               color_bg = "#FFCF38"
               )
           ) + 
    plot_annotation(
        title = title, 
        subtitle = subtitle, 
        caption = caption, 
        theme = theme(
            plot.title = element_textbox(
                family = "Fira Sans",
                face = "bold",
                size = 45,
                halign = 0.5,
                hjust = 0.5,
                color = "#EEB930", # imbd logo
                margin = margin(12, 0, 8, 0)
                ),
            plot.subtitle = element_textbox_simple(
                family = "Fira Sans", 
                size = 20, 
                halign = 0.5, 
                color = light_text, 
                margin = margin(8, 0, 4, 0)
                ),
            plot.caption = element_textbox_simple(
                family = "Fira Sans",
                size = 12,
                halign = 0.5,
                color = light_text,
                margin = margin(8, 0, 8, 4)
            )))


# EXPORT ------------------------------------------------------------------

ggsave(here("holiday episodes", "holiday_episodes_CL.svg"), 
       plot = last_plot(), 
       bg = "white", 
       height = 10, 
       width = 20)
