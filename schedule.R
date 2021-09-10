
# Load libraries ----------------------------------------------------------

library(tidyverse)
library(rvest)
library(mlbstatsR)
library(ggtext)
library(ggimage)
library(here)

# Functions ---------------------------------------------------------------

asp_ratio <- 1.61833
theme_ivo <- function() {
  theme_minimal(base_size = 9, base_family = "Chivo") %+replace%
    theme(
      panel.grid.minor = element_line(colour = "#e1e1e1"),
      panel.grid.major = element_line(color = "#e1e1e1"),
      plot.background = element_rect(fill = "#f4f4f4", color = "#f4f4f4"),
      plot.caption = element_markdown(size = 8.5)
    )
}

get_mlb_wordmarks <- function() {
  message("MLB teams wordmarks and colors!")

  url <- "https://sportslogohistory.com/mlb-wordmark-logo" %>%
    rvest::read_html()


  l <- tibble(
    tm = url %>% rvest::html_elements("a") %>% rvest::html_attr("href") %>% .[39:68],
    wordmark = url %>% rvest::html_elements("img") %>% rvest::html_attr("src") %>% .[6:35]
  ) %>%
    dplyr::mutate(
      tm = stringr::str_replace_all(tm, c("/" = "", "-" = " ", " wordmark logo" = "")),
      tm = stringr::str_to_title(tm),
      tm = case_when(
        tm == "St Louis Cardinals" ~ "St. Louis Cardinals",
        TRUE ~ tm
      ),
      wordmark = case_when(
        wordmark == "https://sportslogohistory.com/wp-content/uploads/2018/04/los_angeles_dodgers_2012-pres_w.png" ~ "https://sportslogohistory.com/wp-content/uploads/2018/04/los_angeles_dodgers_2012-pres-w-150x150.png",
        wordmark == "https://sportslogohistory.com/wp-content/uploads/2019/01/miami_marlins_2019-pres-w.png" ~ "https://sportslogohistory.com/wp-content/uploads/2019/01/miami_marlins_2019-pres_w-150x150.png",
        wordmark == "https://sportslogohistory.com/wp-content/uploads/2018/03/chicago_cubs_1979-pres_w.png" ~ "https://sportslogohistory.com/wp-content/uploads/2018/03/chicago_cubs_1937-pres_w.png",
        TRUE ~ wordmark
      )
    )

  df <- utils::read.csv("https://raw.githubusercontent.com/IvoVillanueva/dataMLB/main/MLBlogos.csv", stringsAsFactors = FALSE) %>%
    dplyr::select(tm = name, team, division, primary:quaternary) %>%
    dplyr::mutate(
      tm = str_squish(tm),
      division = stringr::str_extract(division, "\\w+$")
    ) %>%
    left_join(l, by = c("tm"))

  return(df)
}
# Data Wrangler -----------------------------------------------------------


wins <- mlbstatsR::get_reference_team_standings() %>%
  select(tm, pyth_wl)
words <- get_mlb_wordmarks() %>%
  select(tm, wordmark)
df <- left_join(wins, words) %>% slice(1:30)

names <- get_png_logos() %>%
  select(abr = team_nickname, full_name)

url <- "http://www.tankathon.com/mlb/remaining_schedule_strength"

tanka <- tibble(
  abr = url %>% read_html() %>% html_elements("div.mobile") %>% html_text(),
  sos = url %>% read_html() %>% html_elements("td.remaining-sos") %>% html_text(),
  g_left = url %>% read_html() %>% html_elements("td.games-left") %>% html_text()
)

tanka <- left_join(tanka, names) %>%
  select(tm = full_name, sos) %>%
  left_join(df) %>%
  separate(pyth_wl, c("w", "l"), sep = "-") %>%
  select(-l)

# plot ------------------------------------------------------------------

p <- tanka %>%
  mutate(sos = as.numeric(sos)) %>%
  ggplot(aes(sos, w)) +
  geom_image(
    aes(
      image = wordmark
    ),
    size = 0.075, by = "width", asp = asp_ratio
  ) +
  coord_cartesian(clip = "off") +
  theme_ivo() +
  scale_x_continuous(
    breaks = seq(.42, .65, .025),
    labels = scales::percent,
    limits = c(.42, .62)
  ) +
  labs(
    x = str_to_title("\nstrength of schedule"),
    y = "Pythagorean Wins\n",
    title = str_to_title("Pythagorean Wins vs Remaining Schedule Strength"),
    subtitle = paste0("En la ***MLB*** Hasta el ", format(Sys.Date(), "%d %B, %Y")),
    caption = "<br>**Datos**: *mlbstatsR @thankathon @baseball_ref*  **Gráfico**: *Ivo Villanueva* **@elcheff**."
  ) +
  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(size = 22, face = "bold"),
    plot.subtitle = element_markdown(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.margin = margin(15, 25, 20, 5)
  )

ggsave("schedule.png", p, w = 12, h = 12, dpi = 300, type = "cairo")
