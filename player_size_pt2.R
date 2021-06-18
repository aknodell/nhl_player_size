#####
## Packages used:
## magrittr, readr, dplyr, stringr, purrr, googlesheets4, tibble,
## tidyselect, tidyr, ggplot2
###

library(magrittr)

## Read skater biographical data and join with strength state TOI

full_player_toi_data <-
  # Read the bio data from my google sheet
  googlesheets4::range_speedread(
    "15_9h7RNDA2tM65jk_WdZa3KfO9yXQiLWUBtccr9nr6w", "player_bio_data"
  ) %>%
  dplyr::select(
    player, team, season, toi, days_on_earth, height, weight, bmi, draft_ov
  ) %>%
  dplyr::left_join(
    # Stored locally
    readr::read_csv(.eh_war_filepath) %>%
      dplyr::rename_with(.fn = stringr::str_to_lower) %>%
      # I like using PK better than SH, so I renamed that one
      dplyr::select(player, team, season, toi_ev, toi_pp, toi_pk = toi_sh)
  )

get_strength_state_weighted_averages <- 
  function(player_data, group_bys, strength_state) {
    player_data %>%
      dplyr::select(
        # This is a little complicated/ugly, basically I want to select all
        # the columns that don't contain "toi", plus the one with the suffix
        # passed to map
        tidyselect::all_of(
          colnames(.) %>% 
            .[
              stringr::str_detect(., "toi", negate = T) | 
                stringr::str_detect(
                  ., 
                  stringr::str_c("toi", strength_state, "$")
                )
            ]
        )
      ) %>%
        # name all tois the same to make mutate easier
        dplyr::rename_with(
          stringr::str_replace, pattern = "toi_.*", replacement = "toi"
        ) %>%
        dplyr::group_by(dplyr::across(tidyselect::all_of(group_bys))) %>%
        dplyr::mutate(
          draft_ov = ifelse(is.na(draft_ov), 218, draft_ov),
          total_toi = sum(toi),
          toi_perc = toi / total_toi,
          weighted_days_on_earth = days_on_earth * toi_perc,
          weighted_height = height * toi_perc,
          weighted_weight = weight * toi_perc,
          weighted_bmi = bmi * toi_perc,
          weighted_draft_ov = draft_ov * toi_perc
        ) %>%
        dplyr::summarise(
          dplyr::across(
            tidyselect::starts_with("weighted"),
            sum
          )
        ) %>%
        dplyr::ungroup() %>%
        dplyr::rename_with(
          .fn = function(x) stringr::str_c(x, strength_state),
          .cols = tidyselect::starts_with("weighted")
        )
  }
  
full_team_weighted_averages <-
  purrr::map(
    c("", "_ev", "_pp", "_pk"),
    function(strength_state) {
      full_player_toi_data %>%
        get_strength_state_weighted_averages(
          group_bys = c("season", "team"), 
          strength_state = strength_state
        )
    }
  ) %>%
    purrr::reduce(dplyr::left_join)

get_strength_state_deltas <- function(team_averages, metric) {
  team_averages %>%
  dplyr::select(tidyselect::starts_with(metric), "season") %>%
    dplyr::rename_with(
      .fn = stringr::str_replace,
      pattern = metric,
      replacement = "stat"
    ) %>% 
    tidyr::pivot_longer(
      cols = tidyselect::contains("_"),
      names_to = "Strength State"
    ) %>% 
    dplyr::mutate(
      delta = value - stat,
      `Strength State` = 
        stringr::str_extract(`Strength State`, ".{2}$") %>%
        stringr::str_to_upper() %>%
        factor(levels = c("EV", "PP", "PK"))
    )
}

metrics <-
  c(
    "weighted_days_on_earth", 
    "weighted_height", 
    "weighted_weight", 
    "weighted_bmi",
    "weighted_draft_ov"
  )

team_level_summaries <-
  purrr::map(
    metrics,
    function(metric) {
      full_team_weighted_averages %>%
        get_strength_state_deltas(metric) %>%
        tidyr::pivot_wider(
          id_cols = stat,
          names_from = `Strength State`,
          values_from = delta
        ) %>%
        dplyr::select(EV, PP, PK) %>%
        summary()
    }
  ) %>% 
    purrr::set_names(metrics)

team_level_graphs <-
  purrr::map(
    metrics,
    function(metric) {
      full_team_weighted_averages %>%
        get_strength_state_deltas(metric) %>%
        ggplot2::ggplot(ggplot2::aes(x = delta, fill = `Strength State`)) +
        ggplot2::geom_density(alpha = 0.33) +
        ggplot2::theme_minimal() +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::ggtitle(
          stringr::str_replace_all(metric, "_", " ") %>%
            stringr::str_to_title() %>%
            stringr::str_replace_all(
              c(
                "Weighted " = "",
                "Bmi" = "BMI",
                "Ov" = "Position"
              )
            ) %>%
            stringr::str_c(" Difference")
        ) +
        ggplot2::xlab("Difference From All Situations") +
        ggplot2::ylab("Density") +
        ggplot2::theme(legend.position = "bottom")
    }
  ) %>% 
    purrr::set_names(metrics)

full_league_weighted_averages <-
  purrr::map(
    c("", "_ev", "_pp", "_pk"),
    function(strength_state) {
      full_player_toi_data %>%
        get_strength_state_weighted_averages(
          group_bys = c("season"), 
          strength_state = strength_state
        )
    }
  ) %>%
  purrr::reduce(dplyr::left_join)

## Reshape league level data to use with ggplot
purrr::map(
  metrics,
  function(metric) {
    full_league_weighted_averages %>%
      get_strength_state_deltas(metric) %>%
      dplyr::mutate(
        # Format stat for facet headings
        stat = 
          metric %>%
          stringr::str_remove("weighted_") %>%
          stringr::str_replace_all("_", " ") %>%
          stringr::str_to_title() %>%
          stringr::str_replace_all(
            c(
              "Weighted " = "",
              "Bmi" = "BMI",
              "Ov" = "Position"
            )
          )
      )
  }
) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    stat = 
      factor(
        stat, 
        levels = c("Days On Earth", "Height", "Weight", "BMI", "Draft Position")
      )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = 
        # x and int so we can use geom_line
        purrr::map(
          dplyr::pull(., season),
          function(season)
            player_war_data %>%
            dplyr::pull(season) %>%
            factor() %>%
            levels() %>%
            stringr::str_which(season)
        ) %>% 
        purrr::flatten_int(), 
      y = delta, 
      color = `Strength State`
    )
  ) +
  ggplot2::geom_line(size = 1.2) +
  ggplot2::facet_wrap(ggplot2::vars(stat), ncol = 2, scales = "free") +
  ggplot2::scale_color_viridis_d() +
  ggplot2::scale_x_continuous(
    breaks =
      # breaks so each season appears on x axis
      player_war_data %>%
      dplyr::pull(season) %>%
      factor() %>%
      levels() %>%
      length() %>%
      seq(),
    labels =
      # set so seasons appear as season, not numbers 1-14
      player_war_data %>%
      dplyr::pull(season) %>%
      factor() %>%
      levels()
  ) +
  ggplot2::scale_y_continuous(n.breaks = 8) +
  ggplot2::theme_minimal() +
  ggplot2::ggtitle("Difference From All Situations Average") +
  ggplot2::xlab("") +
  ggplot2::ylab("Difference") +
  ggplot2::theme(legend.position = "bottom")
