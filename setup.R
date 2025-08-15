knitr::opts_chunk$set(echo = FALSE)

################## libraries ##################
# dir.create("/Users/ernestau/Library/CloudStorage/OneDrive-WesternMichiganUniversity")
# .libPaths("/Users/ernestau/Library/CloudStorage/OneDrive-WesternMichiganUniversity")

# Run below to install all necessary packages
#install.packages(c("tidyverse", "readxl", "ggtext", "ggrepel", "treemapify", "waffle", "patchwork", "showtext", "scales", "english", "kableExtra"))
#install.packages(c("bookdown", "tinytex"))

# TinyTeX is a custom LaTeX distribution based on TeX Live that is relatively small in size,
# but functions well in most cases, especially for R users.
# Above, we install the "tinytex" R package
# Below, we use the package to download "TinyTex" - the LaTeX distribution.
# tinytex::install_tinytex()

# install.packages("remotes")
# install.packages("MASS", type = "binary")


# Data manipulation and wrangling
library(tidyverse)  # A collection of packages for data manipulation, visualization, and more

# Reading data
library(readxl)     # To read Excel files into R

# Data visualization
library(ggtext)     # Allows for CSS-like text formatting in ggplot
library(ggrepel)    # For making sure labels in pie chart don't collide
library(treemapify) # For creating treemaps using ggplot2
library(waffle)     # For creating waffle or icon charts
# library(plotly)     # For creating pie chart (no longer in use)
library(patchwork)  # Combines multiple ggplot2 plots into a single layout

# Text and formatting
library(showtext)   # To add installed fonts to plots
library(scales)     # Add commas to big numbers (10470 -> 10,470)
library(english)    # Converts integers to English words
library(kableExtra) # For creating enhanced tables in R Markdown

################## load and clean ##################
# Read excel file into R
ATE_data <- read_excel("data/ATE Survey 2025 Cleaned Data.xlsx")
match_column_names <- read_excel("data/match_column_names.xlsx")

# Turn column names into generalized names
current_names <- tibble(before_naming_standard = colnames(ATE_data))

updated_names <- current_names %>%
  left_join(match_column_names, by = "before_naming_standard") %>%
  mutate(final_names = ifelse(is.na(after_naming_standard), before_naming_standard, after_naming_standard)) %>%
  pull(final_names)

colnames(ATE_data) <- updated_names

# Check for duplicates in column names (there should be no duplicates. happens often.)
# column_names <- colnames(ATE_data)
# duplicated_columns <- column_names[duplicated(column_names)]
# duplicated_columns_table <- table(column_names[column_names %in% duplicated_columns])
# View(duplicated_columns_table)

# Count the total number of ATE projects. This variable will be used throughout the report.
total_ATE_projects <- nrow(ATE_data)

################## process summaries ##################
# Calculates n and pct of specified categories in ATE_data based on mapping criteria.
process_summary <- function(data, mapping_df, question_column) {
  # Ensure the second column is of character type
  if (!is.character(mapping_df[[2]])) {
    stop("Error with second column in mapping df")
  }

  mapping <- sym(colnames(mapping_df)[2])

  # Ensure the question_column exists in the data
  if (!question_column %in% colnames(data)) {
    stop(paste("Column", question_column, "does not exist in the data."))
  }

  # Perform the data processing
  data %>%
    left_join(mapping_df, by = setNames("number", question_column)) %>%
    filter(!is.na(!!mapping)) %>%
    count(!!mapping, name = "n") %>%
    mutate(
      !!mapping := fct_reorder(!!mapping, n, .desc = TRUE),
      pct = n / sum(n) * 100
    ) %>%
    arrange(desc(n))
}

process_summary_2 <- function(data, starting_with = NULL, containing = NULL, ending_with = NULL, names_to = 'name', summary = "sum", remove_brackets = TRUE, remove_hyphen = TRUE) {
  # Check if the input data is a data frame
  if (!is.data.frame(data)) {
    stop("The input data must be a data frame.")
  }

  # Check if names_to is a character string
  if (!is.character(names_to) || length(names_to) != 1) {
    stop("The 'names_to' parameter must be a single character string.")
  }

  # Check if summary is one of "sum", "n", "count_ones" or "median"
  if (!summary %in% c("sum", "count_ones", "mean", "median")) {
    stop("The 'summary' parameter must be one of 'sum', 'count_ones', 'mean', or 'median'.")
  }

  # Select the columns based on the specified criteria
  selected_data <- data %>%
    select(
      if (!is.null(starting_with)) starts_with(starting_with) else everything(),
      if (!is.null(containing)) contains(containing) else NULL,
      if (!is.null(ending_with)) ends_with(ending_with) else NULL
    )

  if (ncol(selected_data) == 0) {
    stop("No columns matched the selection criteria.")
  }

  # Summarize the data based on the specified summary method
  summary_data <- selected_data %>%
    select_if(is.numeric) %>%
    summarise(across(everything(),
                     ~ case_when(
                       summary == "sum" ~ sum(.x, na.rm = TRUE),
                       summary == "count_ones" ~ sum(. == 1, na.rm = TRUE),
                       summary == "mean" ~ mean(.x, na.rm = TRUE),
                       summary == "median" ~ median(.x, na.rm = TRUE),
                       TRUE ~ NA_real_  # Return NA if an unsupported summary method is provided
                     )
    ))

  # Build the regex pattern based on parameters
  pattern_parts <- c("^Q(\\d+\\.)+ ")
  if (remove_brackets) {
    pattern_parts <- c(pattern_parts, " \\(.*")
  }
  if (remove_hyphen) {
    pattern_parts <- c(pattern_parts, " -.*")
  }
  pattern <- paste(pattern_parts, collapse = "|")

  # Reshape and clean the summarized data
  result <- summary_data %>%
    pivot_longer(cols = everything(), names_to = names_to, values_to = summary) %>%
    mutate(
      !!names_to := str_remove_all(!!sym(names_to), pattern),
      !!names_to := fct_reorder(!!sym(names_to), !!sym(summary), .desc = TRUE)
    ) %>%
    arrange(desc(!!sym(summary)))

  return(result)
}

################## plotting ##################
##### callout chart #####
create_callout_df <- function(icons, text, colors) {

  # Function to wrap numbers (with or without commas) and a trailing space in <span> and <b> tags
  wrap_numbers <- function(text) {
    # Match numbers with or without commas but exclude 4-digit numbers without commas (years)
    # and numbers preceded by '='. Include preceding '$' in the formatting if present.
    text <- str_replace_all(text, "(?<!\\=)(\\$?\\b(\\d{1,3}(?:,\\d{3})*)\\b|(?<!\\d)(\\d{1,3})\\b)(\\s*)",
                            function(x) {
                              if (!grepl("^\\d{4}$", x)) {
                                return(paste0("<span style='font-size:25px;'><b>", x, "</b></span>"))
                              } else {
                                return(x)
                              }
                            })
    return(text)
  }


  # Apply the wrap_numbers function to each text element
  wrapped_text <- sapply(text, wrap_numbers)

  # Create the tibble
  df <- tibble(
    y = rev(seq_along(icons)),  # Reverse sequence to get y values in decreasing order
    x = seq_along(icons),
    text = paste0("<span style='font-size:17px;color: ", colors, "'>", wrapped_text, "</span>"),
    icons = paste0("<span style='font-family:\"Font Awesome Solid\"; color: ", colors, ";'>", icons, ";</span>")
  )

  return(df)
}

create_callout_chart <- function(data) {
  theme_set(theme_void(base_family = "Calibri"))

  ggplot(data, aes(y = y)) +
    geom_richtext(aes(x = 1, label = icons),
                  family = "Calibri", hjust = 0, size = 15,
                  fill = NA, label.color = NA, # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt")) +
    geom_richtext(aes(x = 1.15, label = text),
                  family = "Calibri", hjust = 0, nudge_y = .03, lineheight = 2,
                  fill = NA, label.color = NA,  # remove background and outline
                  label.padding = grid::unit(rep(0, 4), "pt")) + # remove padding
    scale_x_continuous(limits = c(1, 2)) +
    scale_y_discrete()
}

##### bar chart #####
# https://www.cedricscherer.com/2023/10/26/yet-another-how-to-on-labelling-bar-graphs-in-ggplot2/
create_bar_chart <- function(data, x, y, pct = TRUE) {
  theme_set(theme_void(base_family = "Calibri"))
  theme_update(plot.margin = margin(10, 15, 10, 15))

  p <- data %>%
    ggplot(aes(x = {{ x }}, y = {{ y }}, fill = {{ y }})) +
    stat_summary(
      geom = "linerange", xmin = 0, xmax = 100, color = 'grey',
      linewidth = 0.5, alpha = 0.7, show.legend = FALSE, linetype = 'dotted', fun = "mean"
    ) +
    geom_col() +
    scale_fill_custom(guide = 'none') +
    facet_wrap(vars({{ y }}), ncol = 1, scales = "free_y")

  if (pct) {
    p <- p + scale_x_continuous(limits = c(0, 100), guide = "none", name = NULL, expand = c(0, 0))
  } else {
    p <- p + scale_x_continuous(guide = "none", name = NULL, expand = c(0, 0))
  }

  p <- p +
    scale_y_discrete(guide = "none", expand = expansion(add = c(.8, .6))) +
    theme(
      strip.text = element_text(
        hjust = 0, margin = margin(1, 0, 3, 0),
        size = 13, face = "bold"
      )
    )

  label_suffix <- if (pct) "%  " else "  "

  p +
    geom_text(
      aes(label = paste0("  ", round({{ x }}), label_suffix),
          color = {{ x }} > 5, hjust = {{ x }} > 5),
      size = 4, fontface = "bold", family = "Calibri"
    ) +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none")
}

##### bar chart proportional #####
create_bar_chart_proportional <- function(data, x, y, fill) {
  # Ensure y is a factor
  y_ <- enquo(y)
  if (!is.factor(data[[quo_name(y_)]])) {
    stop(paste("The column", quo_name(y_), "must be a factor."))
  }

  .GlobalEnv$evaluate_color_palette <- rev(evaluate_color_palette) # has to be reversed

  theme_set(theme_void(base_family = "Calibri"))

  fill_ <- enquo(fill)
  x_ <- enquo(x)


  p <- data %>%
    ggplot(aes(x = {{x}}, y = {{y}}, fill = {{fill}})) +
    geom_col(position = 'fill', color = 'white') +
    facet_wrap(vars({{y}}), ncol = 1, scales = "free_y") +
    scale_x_continuous(guide = "none", name = NULL, expand = expansion(mult = c(0, 0.2))) +
    scale_y_discrete(guide = "none", name = NULL, expand = expansion(add = c(.8, .6))) +
    theme(strip.text = element_markdown(
      hjust = 0, margin = margin(1, 0, 3, 0),
      size = 14, face = "bold"
    ))

  p + geom_text(
    aes(label = paste0("  ", {{fill}}, "  ", "\n", "  ", round({{x}}), "%  "),
        hjust = {{x}} > 3, color = {{x}} > 3),
    size = 4, fontface = "bold", family = "Calibri",  position = 'fill'
  ) +
    scale_fill_custom(guide = 'none') +
    scale_color_manual(values = c("TRUE" = "white", "FALSE" = "black"), guide = "none")
}

##### bar chart single #####
create_bar_chart_single <- function(data, x, fill, elevate_labels = TRUE, pct = TRUE) {
  # Ensure fill is a factor
  fill_ <- enquo(fill)
  if (!is.factor(data[[quo_name(fill_)]])) {
    stop(paste("The column", quo_name(fill_), "must be a factor."))
  }
  x_ <- enquo(x)

  .GlobalEnv$evaluate_color_palette <- rev(evaluate_color_palette) # has to be reversed. only when creating single bar chart

  theme_set(theme_void(base_family = "Calibri"))

  label_suffix <- if (pct) '%' else ''

  data <- data %>%
    mutate(
      pct_shifted = lag(cumsum(!!x_), default = 0),
      label_y_position =  1.6 + 0.2 * (row_number() - 1),
      hjust_value = case_when(
        row_number() == 1 ~ 0,
        row_number() == 2 ~ 0.8,
        TRUE ~ 0.9
      ),
      segment_start_x = ifelse(row_number() == 1, NA, pct_shifted*1.01),
      segment_start_y = ifelse(row_number() == 1, NA, 1.45)
    )

  plot <- data %>%
    ggplot(aes(x = {{x}}, y = factor(1), fill = fct_reorder({{fill}}, {{x}}, .desc = FALSE))) +
    geom_col(position = 'stack', color = 'white') +
    scale_fill_custom(guide = 'none') +
    scale_x_continuous(guide = "none", name = NULL, expand = expansion(mult = c(0, 0.1))) +
    scale_y_discrete(guide = "none", name = NULL, expand = expansion(add = c(.8, .6))) +
    theme(strip.text = element_text(
      hjust = 0, margin = margin(1, 0, 3, 0),
      size = 13
    )) +
    geom_text(aes(x = pct_shifted, label = ifelse({{x}} >= 6, paste0('  ', round({{x}}), label_suffix), "")),
              family = "Calibri", color = 'white', hjust = 0, fontface = "bold")

  if (elevate_labels) {
    plot <- plot +
      geom_text(aes(x = pct_shifted, y = label_y_position, label = ifelse({{x}} >= 6, paste0('  ', {{fill}}), paste0('  ', {{fill}}, ', ', round({{x}}), label_suffix)), hjust = hjust_value),
                family = "Calibri", color = rev(evaluate_color_palette), fontface = "bold") +
      geom_segment(aes(x = segment_start_x, y = segment_start_y, yend = label_y_position - 0.1),
                   color = rev(evaluate_color_palette), na.rm = TRUE) +
      expand_limits(y = c(.5, max(data$label_y_position) + .1))
  } else {
    plot <- plot +
      geom_text(aes(x = pct_shifted, y = 1.6, label = ifelse({{x}} >= 6, paste0('  ', {{fill}}), paste0('  ', {{fill}}, ', ', round({{x}}), label_suffix)), hjust = 0),
                family = "Calibri", color = rev(evaluate_color_palette), fontface = "bold") +
      expand_limits(y = c(.5, 1.7))
  }

  return(plot)
}

##### waffle icon chart #####
create_waffle_icon_chart <- function(data, label, value, adjust_left = 0) {
  theme_set(theme_void(base_family = "Calibri"))
  theme_update(plot.margin = margin(10, 15, 10, -100 + adjust_left))

  rounded_values <- round(data[[quo_name(enquo(value))]])

  data %>%
    ggplot(aes(label = {{label}}, values = round({{value}}))) +
    geom_pictogram(aes(colour = {{label}}), family = 'FontAwesome5Free-Solid') +
    scale_color_manual(
      name = NULL,
      values = evaluate_color_palette,
      labels = function(label) {
        paste0("<span style='color:", evaluate_color_palette, "'>",
               "<b>", label, " ", rounded_values, "%</b>",
               "</span>")
      }
    ) +
    scale_label_pictogram(
      guide = 'none',
      values = "male"
    ) +
    coord_fixed(ratio = 1.7) +
    theme(
      legend.key.height = unit(1.5, "lines"),
      legend.text = element_markdown(size = 12, vjust = 1)
    )
}

##### lollipop chart #####
# https://z3tt.github.io/exciting-extensions/slides.html#/20/2
create_lollipop_chart <- function(data, y, x) {
  set_color_palette(data)
  theme_set(theme_minimal(base_size = 16, base_family = "Calibri"))
  theme_update(
    text = element_text(),
    panel.grid = element_blank(),
    axis.line.y = element_blank(),
    axis.text.y = element_text(face = "bold", color = "black",
                               margin = margin(r = 15), lineheight = .9),
    axis.title = element_text(color = "grey40", face = "bold"),
    axis.title.x = element_text(margin = margin(t = 12)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.line = element_line(color = "grey80", linewidth = .4),
    plot.tag = element_text(size = 20, margin = margin(b = 15)),
    plot.background = element_rect(fill = "white", color = "white")
  )

  p1 <- ggplot(
    data,
    aes(x = reorder({{ x }}, {{ y }}), y = {{ y }}, color = {{ x }})
  ) +
    geom_hline(yintercept = 0, color = "grey80", linewidth = .4) +
    stat_summary(
      geom = "linerange", ymin = 0, ymax = 100, color = 'grey',
      linewidth = 0.5, alpha = 0.7, show.legend = FALSE, linetype = 'dotted', fun = "mean"
    ) +
    stat_summary(
      geom = "point", fun = "sum", size = 15
    ) +
    stat_summary(
      geom = "linerange", ymin = 0, fun.max = function(y) sum(y),
      linewidth = 2, show.legend = FALSE
    ) +
    coord_flip(ylim = c(0, 100), clip = "off") +
    scale_x_discrete(name = NULL) +
    scale_y_continuous(guide = "none", name = NULL, expand = c(0, 0)) +
    scale_color_custom(guide = 'none')

  p1 + geom_text(
    aes(label = paste0("  ", round({{ y }}, 0), "%  ")),
    color = 'white', size = 4, fontface = "bold", family = "Calibri"
  )
}

##### pie chart #####
create_pie_chart <- function(data, values, fill) {

  data <- data %>%
    arrange({{ values }}) %>%
    mutate({{ fill }} := fct_reorder({{ fill }}, {{ values }}, .desc = TRUE),
           text_y = cumsum({{ values }}) - {{ values }}/2)

  theme_set(theme_void(base_family = "Calibri"))

  ggplot(data = data, aes(x = 2, y = {{ values }}, fill = {{ fill }})) +
    geom_col(color = 'white') +
    coord_polar("y") +
    geom_text_repel(aes(label = paste0({{ fill }}, '\n', {{ values }}),
                        x = 2.4,
                        y = text_y,
                        color = {{ fill }}),
                    fontface = "bold",
                    min.segment.length = 0,
                    nudge_x = 0.4,
                    show.legend = F) +
    scale_fill_custom(name = NULL, guide = NULL) +
    scale_color_custom(name = NULL, guide = NULL) +
    xlim(.8, 2.8)
}


################## colors ##################
# color palettes according to EvaluATE branding guide
scale_fill_custom <- function(...) scale_fill_manual(values = evaluate_color_palette, ...)
scale_color_custom <- function(...) scale_color_manual(values = evaluate_color_palette, ...)
set_color_palette <- function(df) {
  evaluate_color_palette = c(color1, color2, color3, color4, color5, color6, color7, color8)
  length(evaluate_color_palette) = length(levels(df[[1]]))
  .GlobalEnv$evaluate_color_palette <- evaluate_color_palette
}

# give colors to markdown and LaTeX text
colorize <- function(x, color) {
  if (knitr::is_latex_output()) {
    # Ensure the color code does not have a "#" for LaTeX output
    color <- gsub("#", "", color)
    sprintf("\\textcolor{%s}{\\textbf{%s}}", color, x)
  } else if (knitr::is_html_output()) {
    sprintf("<span style='color: %s; font-weight: bold;'>%s</span>", color, x)
  } else {
    x
  }
}


color1 <- '#245075'
color2 <- '#B03E61'
color3 <- '#BD4C42'
color4 <- '#4B7F60'
color5 <- '#38377E'
color6 <- '#783A6B'
color7 <- '#327492'
color8 <- '#D4AB44'



################## rounding ##################
round2 <- function(x, digits = 0) {
  multiplier <- 10^digits
  result <- floor(x * multiplier + 0.5) / multiplier
}
# round to closest whole, turn to english, and capitalize first letter
round_and_english <- function(x) {return(str_to_sentence(as.english(round2(x))))}
# round participants, programs, and activities to nearest ten
round_to_ten <- function(x) {
  if (length(x) == 0 || all(is.na(x))) {
    return("0")  # Or "NA" or any default value you prefer
  }
  rounded <- round2(x, -1)
  if (rounded == 0) {
    return(comma(x))  # Return x unrounded with comma formatting
  } else {
    return(comma(rounded))  # Return rounded value with comma formatting
  }
}



################## mappings ##################

yes_no_planning_mapping = data.frame(
  number = 1:3,
  yes_no_planning = c('Yes', 'No', 'Planning to in the future')
)

alternate_yes_no_mapping <- data.frame(
  number = 5:6,
  yes_no = c('Yes', 'No')
)

################## fonts ##################

# use this to look for .ttf or .otf files
# font_files() %>% tibble() %>% filter(str_detect(family, 'Calibri'))
# font_files() %>% tibble() %>% filter(str_detect(family, 'Font Awesome'))

# add calibiri and font awesome
font_add(family = 'Calibri', regular = 'calibri.ttf', bold = 'calibrib.ttf', italic = 'calibrii.ttf')
font_add(family = 'FontAwesome5Free-Solid', regular = 'fa-solid-900.ttf')
font_add(family = 'Font Awesome Solid', regular = 'Font Awesome 6 Free-Solid-900.otf')

showtext::showtext_auto()
