#####
## Packages used:
## magrittr, readr, dplyr, stringr, purrr, tibble, jsonlite
## tidyselect, tidyr, ggplot2
###

skater_war <-
  readr::read_csv(.eh_war_filepath) %>%
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

skater_war <-
  skater_war %>%
  dplyr::left_join(
    skater_war %>%
      dplyr::select(api_id) %>%
      # filter unique to make it faster
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
      dplyr::mutate(
        bmi = weight / (height ^ 2) * 703,
        position = ifelse(position == "D", position, "F"),
        draft_ov = ifelse(is.na(draft_ov), 218, draft_ov)
      )
  )

## adapted from https://www.statology.org/weighted-standard-deviation-excel/

weighted_standard_error <- function(x, wt, wm, n) {
  sum(wt * (x - wm) ^ 2) %>%
    sqrt() %>%
    magrittr::divide_by(n %>% sqrt())
}

position_bio_means_all <-
  purrr::map(
    c("D", "F"),
    function(pos) {
      purrr::map(
        c("days_on_earth", "height", "weight", "bmi"),
        function(metric) {
          tib <-
            skater_war %>%
            dplyr::filter(position == pos)
          
          purrr::map(
            c("toi_all", "toi_ev", "toi_pp", "toi_sh"),
            function(str_state) {
              x <- tib %>% dplyr::pull(tidyselect::all_of(metric))
              
              wt <- 
                tib %>% 
                dplyr::pull(tidyselect::all_of(str_state)) %>%
                magrittr::divide_by(sum(.))
              
              wm <- weighted.mean(x = x, w = wt)
              
              w_se <-
                weighted_standard_error(x = x, wt = wt, wm = wm, n = nrow(tib))
                
              list(
                position = pos,
                metric = metric,
                str_state = str_state %>% stringr::str_remove("toi_"),
                w_mean = wm,
                w_se = w_se,
                lower_ci = wm - (2.576 * w_se),
                upper_ci = wm + (2.576 * w_se)
              )
            }
          ) %>%
            dplyr::bind_rows()
        }
      ) %>% 
        dplyr::bind_rows()
    }
  ) %>%
  dplyr::bind_rows()

position_bio_means_season <-
  purrr::map(
    skater_war %>% dplyr::pull(season) %>% unique() %>% sort(),
    function(ssn) {
      purrr::map(
        c("D", "F"),
        function(pos) {
          purrr::map(
            c("days_on_earth", "height", "weight", "bmi"),
            function(metric) {
              tib <-
                skater_war %>%
                dplyr::filter(season == ssn, position == pos)
              
              purrr::map(
                c("toi_all", "toi_ev", "toi_pp", "toi_sh"),
                function(str_state) {
                  x <- tib %>% dplyr::pull(tidyselect::all_of(metric))
                  
                  wt <- 
                    tib %>% 
                    dplyr::pull(tidyselect::all_of(str_state)) %>%
                    magrittr::divide_by(sum(.))
                  
                  wm <- weighted.mean(x = x, w = wt)
                  
                  w_se <-
                    weighted_standard_error(
                      x = x, 
                      wt = wt, 
                      wm = wm, 
                      n = nrow(tib)
                    )
                  
                  list(
                    season = ssn,
                    position = pos,
                    metric = metric,
                    str_state = str_state %>% stringr::str_remove("toi_"),
                    w_mean = wm,
                    w_se = w_se,
                    lower_ci = wm - (2.576 * w_se),
                    upper_ci = wm + (2.576 * w_se)
                  )
                }
              ) %>%
                dplyr::bind_rows()
            }
          ) %>% 
            dplyr::bind_rows()
        }
      ) %>%
        dplyr::bind_rows()
    }
  ) %>%
  dplyr::bind_rows()

## equal_breaks adapted from:
## https://stackoverflow.com/questions/28436855/change-the-number-of-breaks-using-facet-grid-in-ggplot2

equal_breaks <- function(n = 3, s = 0.05, ...) {
  function(x){
    # rescaling
    d <- s * diff(range(x)) / (1 + 2 * s)
    seq(min(x) + d, max(x) - d, length = n)
  }
}

equal_breaks_labels <- function(n = 3, s = 0.05, sigdig = 3, ...) {
  function(x) {
    # rescaling
    d <- s * diff(range(x)) / (1 + 2 * s)
    seq(min(x) + d, max(x) - d, length = n) %>%
      signif(digits = sigdig) %>%
      formatC(digits = sigdig, format = "fg", big.mark = ",")
  }
}

position_bio_means_all %>%
  dplyr::filter(str_state != "all") %>%
  dplyr::mutate(
    position = 
      position %>% 
      stringr::str_replace_all(c("F" = "Forward", "D" = "Defense")),
    str_state = 
      str_state %>%
      stringr::str_to_upper() %>%
      stringr::str_replace("SH", "PK") %>%
      factor(levels = c("EV", "PP", "PK")),
    metric =
      metric %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title() %>%
      stringr::str_replace("Bmi", "BMI") %>%
      factor(levels = c("Days On Earth", "Height", "Weight", "BMI"))
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = position, 
      y = w_mean, 
      color = str_state, 
      ymax = upper_ci, 
      ymin = lower_ci
    )
  ) +
  ggplot2::geom_pointrange(
    size = .8, 
    position = ggplot2::position_dodge(width = 0.5)
  ) +
  ggplot2::scale_color_viridis_d("") +
  ggplot2::scale_x_discrete("") +
  ggplot2::scale_y_continuous(
    "Mean", 
    breaks = equal_breaks(n = 5),
    labels = 
      equal_breaks_labels(n = 5, sigdig = 4)
  ) +
  ggplot2::facet_wrap(metric ~ ., ncol = 1, scales = "free") +
  ggplot2::theme_light() + 
  ggplot2::theme(
    legend.position = "bottom",
    panel.border = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank()
  ) +
  ggplot2::ggtitle(
    "Player Biographical Means With 99% Confidence Intervals",
    "2007-21 Aggregates"
  ) +
  # flip because it's a littler easier to read the chart this way, IMO
  ggplot2::coord_flip()

position_bio_means_season %>%
  dplyr::filter(str_state != "all") %>%
  dplyr::mutate(
    str_state = 
      str_state %>% 
      stringr::str_to_upper() %>%
      stringr::str_replace("SH", "PK") %>%
      factor(levels = c("EV", "PP", "PK")),
    metric =
      metric %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_to_title() %>%
      stringr::str_replace("Bmi", "BMI") %>%
      factor(levels = c("Days On Earth", "Height", "Weight", "BMI")),
    position = 
      position %>% 
      stringr::str_replace_all(c("F" = "Forward", "D" = "Defense")) %>%
      factor(levels = c("Forward", "Defense"))
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      ## make factor continous so that we can use geom_line/ribbon
      x = 
        season %>% 
        rank() %>%
        magrittr::subtract(0.5) %>% 
        magrittr::divide_by(24) %>% 
        magrittr::add(0.5), 
      y = w_mean, 
      color = str_state, 
      fill = str_state
    )
  ) +
  ggplot2::geom_ribbon(
    ggplot2::aes(ymin = lower_ci, ymax = upper_ci, color = NULL), 
    alpha = 0.3
  ) +
  ggplot2::geom_line(size = 0.8) +
  ggplot2::facet_grid(metric ~ position, scales = "free", switch = "y") +
  ggplot2::scale_x_continuous(
    "Season",
    breaks = 
      position_bio_means_season %>% 
      dplyr::pull(season) %>% 
      unique() %>%
      rank() %>%
      # take every other season so they don't overlap on x-axis
      .[seq(length(.)) %% 2 == 0],
    labels = 
      position_bio_means_season %>% 
      dplyr::pull(season) %>% 
      unique() %>%
      sort() %>% 
      # take every other season so they don't overlap on x-axis
      .[seq(length(.)) %% 2 == 0]
  ) +
  ggplot2::scale_y_continuous(
    "",
    breaks = equal_breaks(n = 4, s = 0.1),
    labels = equal_breaks_labels(n = 4, s = 0.1)
  ) +
  ggplot2::scale_fill_viridis_d("") +
  ggplot2::scale_color_viridis_d("") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom") +
  ggplot2::ggtitle(
    "Player Biographical Means With 99% Confidence Intervals",
    "Season-To-Season Trends"
  )
