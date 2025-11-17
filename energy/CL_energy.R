# ABOUT

# Our World in Data's Energy Data Explorer
# #TidyTuesday week 23 (2023-06-06)

# ggclub contribution for November 2025

# updated data from OWID: retrieved 2025-10-28
# Data sources: U.S. Energy Information Administration (2025); Energy Institute -
# Statistical Review of World Energy (2025); Population based on various sources
# (2024) – with major processing by Our World in Data



# SETUP -------------------------------------------------------------------

library(tidyverse)
library(here)
library(janitor)
library(scales)
library(gghighlight)
library(glue)
library(patchwork)
library(cowplot)

# updated data from OWID:
energy_raw = read_csv(here("energy", "data", "clean", "energy_updated.csv")) %>%
    clean_names %>%
    select(-x1)

# energy_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-06/owid-energy.csv')



# DATA EXPLORATION --------------------------------------------------------

energy_raw %>% glimpse
energy_raw %>% distinct(country) %>% print(n = Inf)
energy_raw %>% pull(year) %>% range

energy_raw %>% 
    filter(!if_any(c(iso_code, gdp), is.na)) %>% 
    filter(year == max(year)) %>% 
    slice_max(order_by = gdp/population, n = 15)



# DATA WRANGLING ----------------------------------------------------------

# id columns
id_cols = energy_raw %>% 
    select(
        country:gdp, 
        starts_with("electricity"),
        starts_with("energy"),
        starts_with("net_elec"),
        starts_with("per_capita"),
        starts_with("primary"),
        starts_with("country")) %>% 
    colnames

# select interesting variables
metrics = c("consumption", "production", "electricity", "share_energy", "share_elec")

# filter and reshape data
energy = energy_raw %>% 
    select(all_of(id_cols), contains(metrics)) %>% 
    filter(!if_any(c(iso_code, gdp), is.na)) %>%
    pivot_longer(
        cols = -all_of(id_cols), 
        names_to = c("energy_source", "metric"), 
        names_pattern = paste0("(.*)_", "(", paste0(metrics, collapse = "|"), ")"),
        values_to = "value"
    ) %>% 
    filter(!str_detect(energy_source, "fossil|renewable|low_carbon")) %>% 
    mutate(
        category = case_when(
            energy_source %in% c("biofuel", "hydro", "solar", "wind") ~ "renewables", 
            energy_source == "nuclear" ~ "nuclear", 
            .default = "fossil fuels"
    ), 
    decade = floor(year/10) * 10, 
    year_date = paste0(year, "-01-01") %>% ymd
    )

map(
    unique(energy$category), 
    ~ energy %>% filter(category == .x) %>% distinct(energy_source)
    )

# top 5 countries by 2024 GDP
gdp_top5 = energy %>% 
    filter(year == max(year)) %>% 
    distinct(country, population, gdp, year) %>% 
    slice_max(order_by = gdp, n = 5) %>% 
    mutate(
        gdp_pop = gdp/population,
        gdp_pretty = number(gdp, scale_cut = cut_short_scale()),
        gdp_pop_pretty = number(gdp_pop, scale_cut = cut_short_scale()))

energy5 = energy %>% 
    filter(country %in% gdp_top5$country) %>% 
    mutate(country = factor(country, levels = gdp_top5$country))

energy_ranks_df = energy5 %>% 
    filter(metric %in% c("consumption", "electricity")) %>% 
    summarize(.by = c(energy_source, metric, category), 
              value = sum(value, na.rm = TRUE)) %>% 
    arrange(metric, -value) %>% 
    mutate(.by = metric, rank = row_number())

energy_ranks = energy_ranks_df %>% 
    filter(metric == "consumption") %>% 
    pull(energy_source)

energy_ranks_gen = energy_ranks_df %>% 
    filter(metric == "electricity") %>% 
    pull(energy_source)

energy_time = energy5 %>% 
    filter(metric %in% c("consumption", "electricity")) %>% 
    summarize(.by = c(year_date, energy_source, metric, category), 
              value = sum(value, na.rm = TRUE)) 
    

# FUNCTIONS ---------------------------------------------------------------

plot_lines = function(df, yvar, xmin, xmax, intv = "25 years") {
    df %>% 
        ggplot(aes(x = year_date, y = {{yvar}}, group = country)) +
        geom_line() +
        scale_x_date(
            expand = expansion(mult = c(0.05, 0.05)),
            limits = c(ymd(xmin), ymd(xmax)),
            breaks = seq(ymd(xmin), ymd(xmax), by = intv),
            labels = function(x) if_else(
                x %in% c(xmin, xmax), 
                format(x, "%Y"), str_c("'", format(x, "%y")))
        ) +
        scale_y_continuous(
            labels = label_number(scale_cut = cut_short_scale()),
            breaks = pretty_breaks(7),
            position = "right"
        ) +
        theme_sub_axis(
            text = element_text(),
            ticks = element_line()
        )
}

plot_areas = function(df, yvar, xmin, xmax, intv = "25 years") {
    df %>% 
        ggplot(aes(x = year_date, y = {{yvar}}, fill = factor(energy_source))) +
        geom_area() +
        scale_fill_manual(values = pal2, labels = str_to_sentence, name = "Energy source") +
        guides(fill = guide_legend(nrow = 1)) +
        scale_x_date(
            expand = expansion(mult = c(0.05, 0.05)),
            limits = c(ymd(xmin), ymd(xmax)),
            breaks = seq(ymd(xmin), ymd(xmax), by = intv),
            labels = function(x) if_else(
                x %in% c(xmin, xmax), 
                format(x, "%Y"), str_c("'", format(x, "%y")))
        ) +
        theme_sub_axis_y(text = element_blank(), ticks = element_blank()) +
        theme_sub_legend(
            position = "top", 
            direction = "horizontal", 
            key.width = unit(40, "pt"), 
            text.position = "top"
        )
}

# PLOT PREP ---------------------------------------------------------------

ink_col = "royalblue4"
paper_col = "linen"
light_col = col_mix(ink_col, paper_col, 0.75)


theme_set(
    theme_minimal(
        base_family = "American Typewriter",
        base_size = 8,
        ink = ink_col, 
        paper = paper_col
        ) +
        theme_sub_panel(grid = element_blank()) +
        theme_sub_axis(title = element_blank())
)


ggthemes::colorblind_pal() %>% show_col

pal2 = c(
    "biofuel" = "#009e73",  
    "coal" = colorspace::lighten("#000000", 0.2),       
    "gas" = "#e69f00",      
    "hydro" = "#0072b2",        
    "nuclear" = "#cc79a7",  
    "oil" = "#d55e00",         
    "solar" = "#f0e442",       
    "wind" = "#56b4e9"
)

cap = glue(
    "Source: Our World in Data ",
    "(retrieved {format(ymd('2025-10-28'), '%B %d, %Y')}). | ", 
    "#TidyTuesday week week 23 ({format(ymd('2023-06-06'), '%B %d, %Y')}) | ", 
    "Visualization by Claudia Laurenzano | ", 
    "{format(today(), '%B %d, %Y')} | ", 
    "Created in R")



# PLOTS -------------------------------------------------------------------

## GDP top 5 -------------------------------------------------------------

sub1 = glue(
    "**Top 5 countries by 2023 GDP^1^ (USD).** GDP per capita in parentheses.")

cap1 = glue(
    "^1^Total economic output of a country or region per year. This data is adjusted ",
    "for inflation and for differences in living costs between countries. ",
    "International-$ in 2011 prices."
    )

df1 = energy5 %>% mutate(yvar = gdp)
ref_df = df1 %>% 
    filter(year == max(year)) %>% 
    distinct(country, population, gdp, year) %>% 
    slice_max(order_by = gdp, n = 5) %>% 
    mutate(
        yvar = gdp,
        y_pop = yvar/population,
        y_pretty = number(yvar, scale_cut = cut_short_scale()),
        y_pop_pretty = number(y_pop, accuracy = 1L, scale_cut = cut_short_scale())
        )

p_gdp = map(
    .x = gdp_top5$country,
    .f = ~ df1 %>% 
        plot_lines(yvar = yvar, xmin = "1900-01-01", xmax = "2025-01-01") +
        gghighlight(
            country == .x,
            use_direct_label = FALSE,
            unhighlighted_params = list(color = light_col)
        ) +
        geom_point(data = df1 %>% filter(country == .x, year_date == max(year_date))) +
        annotate(
            "text",
            x = ymd("2000-01-01"),
            y = max(ref_df$yvar),
            label = glue(
                .x, "\n", 
                "$", ref_df %>% filter(country == .x) %>% pull(y_pretty), " \n", 
                "($", ref_df %>% filter(country == .x) %>% pull(y_pop_pretty), " p.c.)"
            ),
            hjust = 1
        )
) %>% 
    wrap_plots(ncol = 5) +
    plot_annotation(subtitle = sub1, 
                    caption = cap1,
                    theme = theme(
                        plot.subtitle = ggtext::element_textbox_simple(),
                        plot.caption = ggtext::element_textbox_simple()))






## electricity demand ----------------------------------------------------

df1 = energy5 %>% 
    filter(!is.na(electricity_demand)) %>% # TWh
    summarize(
        .by = c(country, year_date, population), 
        yvar = mean(electricity_demand)
    )

ref_df = df1 %>% 
    filter(year_date == max(year_date)) %>% 
    mutate(y_pop = yvar/population*1e6, # megawatt MWh
           y_pretty = number(yvar, scale_cut = cut_short_scale()), 
           y_pop_pretty = number(y_pop, accuracy = 1L, scale_cut = cut_short_scale())
           )

p_demand = map(
    .x = gdp_top5$country,
    .f = ~ df1 %>% 
        plot_lines(yvar = yvar, xmin = "1990-01-01", xmax = "2025-01-01", intv = "10 years") +
        gghighlight(
            country == .x,
            use_direct_label = FALSE,
            unhighlighted_params = list(color = light_col)
        ) +
        geom_point(data = df1 %>% filter(country == .x, year_date == max(year_date))) +
        annotate(
            "text",
            x = ymd("2015-01-01"),
            y = max(ref_df$yvar) * 0.85,
            label = glue(
                .x, "\n", 
                ref_df %>% filter(country == .x) %>% pull(y_pretty), " TWh\n", 
                "(", ref_df %>% filter(country == .x) %>% pull(y_pop_pretty), " MWh p.c.)"
            ),
            hjust = 1
        )
) %>% 
    wrap_plots(ncol = 5) +
    plot_annotation(subtitle = glue("**Electricity demand by country (TWh).** Demand per capita in parentheses (MWh)."), 
                    theme = theme(plot.subtitle = ggtext::element_textbox_simple()))



## electricity generation ------------------------------------------------

df1 = energy5 %>% 
    filter(!is.na(electricity_generation)) %>% # TWh
    summarize(
        .by = c(country, year_date, population), 
        yvar = mean(electricity_generation)
    )

ref_df = df1 %>% 
    filter(year_date == max(year_date)) %>% 
    mutate(y_pop = yvar/population*1e6, # megawatt
           y_pretty = number(yvar, scale_cut = cut_short_scale()), 
           y_pop_pretty = number(y_pop, accuracy = 1L, scale_cut = cut_short_scale())
           )

p_prod = map(
    .x = gdp_top5$country,
    .f = ~ df1 %>% 
        plot_lines(yvar = yvar, xmin = "1985-01-01", xmax = "2025-01-01", intv = "10 years") +
        gghighlight(
            country == .x,
            use_direct_label = FALSE,
            unhighlighted_params = list(color = light_col)
        ) +
        geom_point(data = df1 %>% filter(country == .x, year_date == max(year_date))) +
        annotate(
            "text",
            x = ymd("2015-01-01"),
            y = max(ref_df$yvar) * 0.85,
            label = glue(
                .x, "\n", 
                ref_df %>% filter(country == .x) %>% pull(y_pretty), " TWh\n", 
                "(", ref_df %>% filter(country == .x) %>% pull(y_pop_pretty), " MWh p.c.)"
            ),
            hjust = 1
        )
) %>% 
    wrap_plots(ncol = 5) +
    plot_annotation(subtitle = glue("**Electricity generation by country (TWh).** Production per capita in parentheses (MWh)."), 
                    theme = theme(plot.subtitle = ggtext::element_textbox_simple()))
    



## share consumption -----------------------------------------------------

df1 = energy5 %>% 
    mutate(energy_source = factor(energy_source, levels = energy_ranks)) %>% 
    filter(metric == "share_energy", !is.na(value))

p_cons_sh = map(
    .x = gdp_top5$country,
    .f = ~ df1 %>% 
        filter(country == .x) %>% 
        plot_areas(yvar = value, xmin = "1965-01-01", xmax = "2025-01-01", intv = "15 years") +
        annotate(
            "text", 
            x = ymd("1966-01-01"), 
            y = 91, 
            hjust = 0,
            label = .x,
            color = paper_col
        )
    ) %>% 
    wrap_plots(nrow = 1) +
    plot_layout(guides = "collect") +
    plot_annotation(
        subtitle = glue("**Energy consumption** by share of energy sources by country"),
        theme = theme(
            legend.position = "none", 
            plot.subtitle = ggtext::element_textbox_simple()))


## share generation ------------------------------------------------------

df1 = energy5 %>% 
    mutate(energy_source = factor(energy_source, levels = energy_ranks_gen)) %>% 
    filter(metric == "share_elec", !is.na(value))

p_prod_sh = map(.x = gdp_top5$country,
    .f = ~ df1 %>% 
        filter(country == .x) %>% 
        plot_areas(yvar = value, xmin = "1985-01-01", xmax = "2025-01-01", intv = "10 years") +
        annotate(
            "text", 
            x = ymd("1986-01-01"), 
            y = 91, 
            hjust = 0,
            label = .x,
            color = paper_col
        )
    ) %>% 
    wrap_plots(nrow = 1) +
    plot_layout(guides = "collect") +
    plot_annotation(
        subtitle = glue("**Energy generation** by share of energy sources by country"),
        theme = theme(
            legend.position = "none", 
            plot.subtitle = ggtext::element_textbox_simple()))



## share legends ---------------------------------------------------------

p_sh_lg = map(.x = unique(energy_ranks_df$metric), 
    .f <- ~ {
        lab <- if_else(.x == "consumption", .x, "generation")

        energy_time %>%
            left_join(energy_ranks_df %>% select(energy_source, metric, rank)) %>%
            filter(metric == .x) %>%
            ggplot(aes(x = year_date, y = value, fill = energy_source)) +
            facet_wrap(
                ~ reorder(energy_source, rank),
                labeller = as_labeller(str_to_sentence),
                nrow = 1, 
                strip.position = "bottom"
            ) +
            geom_area(show.legend = FALSE) +
            scale_fill_manual(values = pal2) +
            scale_x_date(
                expand = expansion(mult = c(0.05, 0.05)),
                breaks = c(ymd("1965-01-01"), ymd("2023-01-01")),
                date_labels = "%Y"
            ) +
            scale_y_continuous(
                labels = label_number(scale_cut = cut_short_scale()),
                breaks = pretty_breaks(7),
                expand = expansion(mult = c(0, 0))
            ) +
            labs(subtitle = glue("**Energy {lab}** by energy source in TWh")) +
            # theme_sub_axis_x(ticks = element_line()) +
            theme_sub_axis_x(text = element_blank()) +
            theme_sub_plot(subtitle = ggtext::element_textbox_simple()) 
    }) %>% 
    wrap_plots(nrow = 2)



# header ------------------------------------------------------------------

mix = glue(
    "Fossil fuels still dominate energy use in the top five economies, but their ",
    "shares are shifting. The U.S. is cutting coal while growing gas and ",
    "renewables. China remains coal-heavy but is rapidly expanding hydro, wind, ",
    "and solar. Japan relies mostly on oil and gas, with nuclear sharply down ",
    "since 2011. Germany and the U.K. have nearly phased out coal, replaced by ",
    "wind, solar, and lower overall demand.
    
    Electricity generation reflects these similar dynamics, but with renewables ",
    "taking up a larger share overall and rising across nations. Germany and the ",
    "U.K. lead in decarbonizing their power sources. Patterns highlight ",
    "both the persistence of coal and gas in energy systems and the uneven pace ", 
    "of the transition toward cleaner and more sustainable sources."
)

header = ggplot() + 
    geom_blank() +
    labs(
        title = glue(
            "<span style='font-size:{36}pt'>Energy</span><br> ",
            "use and production in the world's <br>",
            "<span style='font-size:{24}pt'>top 5 economies</span><br>"), 
        subtitle = glue(
            "In 2023, the countries with the largest economies by GDP ",
            "(2011 international $) were the ", 
            "{gdp_top5 %>% pull(country) %>% knitr::combine_words(and = 'and the ')}. ", 
            "The figure compares their economic growth with patterns of ",
            "electricity demand, generation, and energy mix over time. ",
            "China stands out from the group with a sharp rise in GDP over", 
            "recent decades, as well as steeply rising electricity ", 
            "demand and generation, outpacing the other ", 
            "four top economies.
            
            {mix}" 

    )) +
    theme_sub_plot(
        title = ggtext::element_textbox_simple(size = 16), 
        subtitle = ggtext::element_textbox_simple(size = 9)
    )

p_cap = ggplot() + geom_blank() +
    labs(caption = cap)



# PW ----------------------------------------------------------------------

pw = plot_grid(
    plot_grid(
        plot_grid(header, p_sh_lg, nrow = 2, rel_heights = c(2, 1.35)),
        plot_grid(p_gdp, p_demand, p_prod, p_cons_sh, p_prod_sh, ncol = 1),
        ncol = 2, 
        rel_widths = c(0.45, 1)
    ), 
    p_cap, 
    nrow = 2, 
    rel_heights = c(10, 0.3)
)


# EXPORT ------------------------------------------------------------------

ggsave(here("energy", "CL_energy.png"), plot = pw, 
       bg = paper_col, # to avoid artifact lines from plot_grid()
       height = 9, width = 11)
