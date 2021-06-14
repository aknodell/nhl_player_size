#####
## Packages used:
## magrittr, readr, dplyr, stringr, lubridate, purrr, jsonlite, tibble,
## tidyselect, tidyr, ggplot2
###

library(magrittr)

## Read skater standard table

player_data_base <-
  readr::read_csv(.eh_skater_standard_filepath) %>%
  dplyr::rename_with(
    .fn =
      function(x) {
        x %>%
          stringr::str_replace_all(
            c(" " = "_")
          ) %>%
          stringr::str_to_lower()
      }
  ) %>%
  # Select just the columns we need
  dplyr::select(
    player, team, api_id, season, birthday, position, draft_ov, toi
  ) %>%
  # Create a season start date for calculating days on earth
  dplyr::mutate(
    api_id = as.character(api_id),
    season_start_date =
      stringr::str_c(
        "20",
        season %>% stringr::str_sub(end = 2),
        "-09-15"
      ) %>%
      lubridate::as_date(),
    days_on_earth =
      as.integer(season_start_date) - as.integer(birthday)
  )

## Get player height, weight, nationality from NHL API

player_bio_data <-
  player_data_base %>%
  dplyr::left_join(
    player_data_base %>%
      dplyr::select(api_id) %>%
      dplyr::distinct() %>%
      dplyr::mutate(
        api_id = as.character(api_id),
        player_bio =
          purrr::map(
            api_id,
            (function(id) {
              "http://statsapi.web.nhl.com/api/v1/people/api_id" %>%
                stringr::str_replace(pattern = "api_id", replacement = id) %>%
                readr::read_file() %>%
                jsonlite::parse_json() %>%
                .$people %>%
                unlist() %>%
                .[c("height", "weight", "nationality")] %>%
                list() %>%
                purrr::transpose() %>%
                purrr::flatten() %>%
                tibble::as_tibble() %>%
                dplyr::mutate(
                  weight = as.numeric(weight),
                  height =
                    height %>%
                    stringr::str_split("\\s+") %>%
                    unlist() %>%
                    stringr::str_remove_all("\\D") %>%
                    as.numeric() %>%
                    magrittr::multiply_by(c(12, 1)) %>%
                    sum()
                )
            })
          )
      ) %>%
      tidyr::unnest(cols = c(player_bio)) %>%
      dplyr::mutate(bmi = weight / (height ^ 2) * 703)
  )

## Summarize Weighted Team Data

weighted_team_data <-
  player_bio_data %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(
    # set draft_ov to 218 if undrafted
    draft_ov = ifelse(is.na(draft_ov), 218, draft_ov),
    total_toi = sum(toi),
    toi_perc = toi / total_toi,
    weighted_days_on_earth = days_on_earth * toi_perc,
    weighted_height = height * toi_perc,
    weighted_weight = weight * toi_perc,
    weighted_bmi = bmi * toi_perc,
    weighted_draft = draft_ov * toi_perc
  ) %>%
  dplyr::summarise(
    dplyr::across(
      tidyselect::starts_with("weighted"),
      sum
    )
  )

## 2020-21 Graphs

weighted_team_data %>%
  dplyr::filter(season == "20-21") %>%
  ggplot2::ggplot(
    ggplot2::aes(x = team, y = weighted_days_on_earth, fill = 1)
  ) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    ggplot2::aes(
      label = (weighted_days_on_earth / 365.25) %>% round(1),
      vjust = -1)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("NHL Team Age Weighted By TOI", "2020-21 Season") +
  ggplot2::xlab("") +
  ggplot2::ylab("Weighted Days On Earth") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_cartesian(ylim = c(9000, NA))

weighted_team_data %>%
  dplyr::filter(season == "20-21") %>%
  ggplot2::ggplot(ggplot2::aes(x = team, y = weighted_heights, fill = 1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(ggplot2::aes(
    label = (weighted_heights) %>% round(1), vjust = -1)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("NHL Team Height Weighted By TOI", "2020-21 Season") +
  ggplot2::xlab("") +
  ggplot2::ylab("Weighted Height On Earth") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_cartesian(ylim = c(72, NA))

weighted_team_data %>%
  dplyr::filter(season == "20-21") %>%
  ggplot2::ggplot(ggplot2::aes(x = team, y = weighted_weights, fill = 1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    ggplot2::aes(label = (weighted_weights) %>% round(1), vjust = -1)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("NHL Team Weight Weighted By TOI", "2020-21 Season") +
  ggplot2::xlab("") +
  ggplot2::ylab("Weighted Weight On Earth") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_cartesian(ylim = c(185, NA))

weighted_team_data %>%
  dplyr::filter(season == "20-21") %>%
  ggplot2::ggplot(ggplot2::aes(x = team, y = weighted_bmis, fill = 1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    ggplot2::aes(label = (weighted_bmis) %>% round(1), vjust = -1)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("NHL Team BMI Weighted By TOI", "2020-21 Season") +
  ggplot2::xlab("") +
  ggplot2::ylab("Weighted BMI On Earth") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_cartesian(ylim = c(25, NA))

weighted_team_data %>%
  dplyr::filter(season == "20-21") %>%
  ggplot2::ggplot(ggplot2::aes(x = team, y = weighted_draft, fill = 1)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    ggplot2::aes(label = (weighted_draft) %>% round(1), vjust = -1)
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle(
    "NHL Team Draft Position Weighted By TOI", "2020-21 Season"
  ) +
  ggplot2::xlab("") +
  ggplot2::ylab("Weighted Draft Position") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::coord_cartesian(ylim = c(40, NA))


## Summarize Nationality Data

weighted_team_nationalities <-
  player_bio_data %>%
  dplyr::group_by(season) %>%
  dplyr::mutate(
    total_toi = sum(toi),
    toi_perc = toi / total_toi
  ) %>%
  dplyr::group_by(nationality, add = T) %>%
  dplyr::summarise(weighted_nationality = sum(toi_perc))

## 2020-21 team nationality graph

weighted_team_nationalities %>%
  dplyr::filter(season == "20-21") %>%
  ggplot2::ggplot(
    ggplot2::aes(x = nationality, y = weighted_nationality, fill = 1)
  ) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(
    ggplot2::aes(
      label =
        (weighted_nationality * 100) %>%
        round(1) %>%
        stringr::str_c("%"),
      vjust = -1
    )
  ) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("NHL TOI By Nationality", "2020-21 Season") +
  ggplot2::xlab("") +
  ggplot2::ylab("TOI Percentage") +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_y_continuous(labels = scales::percent)

## Trends graph

# Team data is organized in columns, pivot_longer for faceting
dplyr::bind_rows(weighted_team_data, weighted_team_nationalities) %>%
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("weighted"),
    names_to = "descriptor"
  ) %>%
  dplyr::filter(
    nationality %in% c("CAN", "USA", "SWE", "RUS", "FIN", "CZE", NA),
    !is.na(value)
  ) %>%
  dplyr::mutate(
    # set factor levels for color ordering
    nationality =
      nationality %>%
      factor(levels = c("CAN", "USA", "SWE", "RUS", "CZE", "FIN")),
    descriptor =
      stringr::str_remove(descriptor, "weighted_") %>%
      stringr::str_replace_all(c("_" = " ", "draft" = "draft position")) %>%
      stringr::str_to_title() %>%
      stringr::str_replace("mi", "MI") %>%
      # set factor levels for facet arrangement
      factor(
        levels =
          c(
            "Days On Earth",
            "Height",
            "Weight",
            "BMI",
            "Draft Position",
            "Nationality"
          )
      )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x =
        # convert season to numeric so we can use geom_line
        purrr::map(
          dplyr::pull(., season),
          function(season)
            team_data %>%
            dplyr::pull(season) %>%
            factor() %>%
            levels() %>%
            stringr::str_which(season)
        ) %>% purrr::flatten_int(),
      y = value, color = nationality)) +
  ggplot2::geom_line(size = 1.2) +
  ggplot2::facet_wrap(ggplot2::vars(descriptor), ncol = 2, scales = "free") +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(
    breaks =
      # breaks so each season appears on x axis
      team_data %>%
      dplyr::pull(season) %>%
      factor() %>%
      levels() %>%
      length() %>%
      seq(),
    labels =
      # set so seasons appear as season, not numbers 1-14
      team_data %>%
      dplyr::pull(season) %>%
      factor() %>%
      levels()
  ) +
  ggplot2::ggtitle("Season By Season Trends") +
  ggplot2::ylab("Weighted Mean") +
  ggplot2::xlab("") +
  ggplot2::scale_color_viridis_d(
    "Nationality",
    # Don't show NA on color key
    breaks = c("CAN", "USA", "SWE", "RUS", "CZE", "FIN"),
    # Make trend line black for age, height, weight, bmi, and draft position
    na.value = "black"
  ) +
  # more breaks to make y-axis more interpretable
  ggplot2::scale_y_continuous(n.breaks = 8) +
  ggplot2::theme(legend.position = "right")
