## pivot longer to make analysis by strength state easier

full_team_weighted_averages <-
  purrr::map(
    metrics,
    function(metric) {
      full_team_weighted_averages %>%
        tidyr::pivot_longer(
          cols = tidyselect::starts_with(metric), 
          names_to = "strength_state", 
        ) %>% 
        dplyr::select(season, team, strength_state, value) %>%
        dplyr::rename_with(.fn = function(x) metric, .cols = value) %>%
        dplyr::mutate(
          strength_state = 
            strength_state %>%
            stringr::str_extract(("(ev)|(pp)|(pk)")) %>% 
            stringr::str_to_upper(),
          strength_state =
            ifelse(is.na(strength_state), "All", strength_state)
        )
    }
  ) %>% 
    purrr::reduce(dplyr::left_join)

full_league_weighted_averages <-
  purrr::map(
    metrics,
    function(metric) {
      full_league_weighted_averages %>%
        tidyr::pivot_longer(
          cols = tidyselect::starts_with(metric), 
          names_to = "strength_state", 
        ) %>% 
        dplyr::select(season, strength_state, value) %>%
        dplyr::rename_with(
          .fn = function(x) stringr::str_c(metric, ".league"), .cols = value
        ) %>%
        dplyr::mutate(
          strength_state = 
            strength_state %>%
            stringr::str_extract(("(ev)|(pp)|(pk)")) %>% 
            stringr::str_to_upper(),
          strength_state =
            ifelse(is.na(strength_state), "All", strength_state)
        )
    }
) %>% 
  purrr::reduce(dplyr::left_join)

full_team_weighted_average_deltas <-
  full_team_weighted_averages %>%
  dplyr::left_join(
    full_league_weighted_averages
  )

full_team_weighted_average_deltas <-
  purrr::map(
    metrics,
    function(metric) {
      full_team_weighted_average_deltas %>%
        dplyr::group_by(season, team, strength_state) %>%
        dplyr::rename_with(
          .fn = stringr::str_replace,
          pattern = metric,
          replacement = "metric"
        ) %>%
        dplyr::transmute(metric_delta = metric - metric.league) %>%
        dplyr::rename_with(
          .fn = stringr::str_replace,
          pattern = "metric",
          replacement = metric
        ) %>% dplyr::ungroup()
    }
  ) %>% 
  purrr::reduce(dplyr::left_join)

#####
## Build the data table with on-ice metrics
###

full_team_stats <-
  purrr::map(
    c(
      .eh_team_all_filepath,
      .eh_team_ev_filepath,
      .eh_team_pk_filepath,
      .eh_team_pp_filepath
    ),
    function(filepath) {
      readr::read_csv(filepath) %>%
        dplyr::rename_with(
          .fn =
            function(x) {
              x %>%
                stringr::str_to_lower() %>%
                stringr::str_replace_all(
                  c(
                    "%" = "_perc",
                    "±" = "_diff"
                  )
                )
            }
        ) %>%
        dplyr::group_by(season) %>%
        dplyr::mutate(
          xg_for_median = median(xgf),
          xg_against_median = median(xga),
          shot_quality_for_median = median(xgf / ff), 
          shot_quality_against_median = median(xga / fa),
          shot_quality_for = (xgf / ff) - shot_quality_for_median, 
          shot_quality_against = (xga / fa) - shot_quality_against_median,           
          shot_quantity_for_median = median(cf), 
          shot_quantity_against_median = median(ca),
          strength_state = 
            filepath %>%
            stringr::str_extract(("(ev)|(pp)|(pk)")) %>% 
            stringr::str_to_upper(),
          strength_state =
            ifelse(is.na(strength_state), "All", strength_state),
          xg_diff_per_60 = 
            ((xgf - xg_for_median) - (xga - xg_against_median)) / 
            toi *
            60,
          shot_quality_diff = shot_quality_for - shot_quality_against,
          shot_quantity_diff = 
            (
              (cf - shot_quantity_for_median) - 
                (ca - shot_quantity_against_median)
            ) / 
            toi * 
            60
        ) %>%
        dplyr::select(
          season, 
          team, 
          strength_state, 
          xg_diff_per_60, 
          shot_quality_diff, 
          shot_quantity_diff
        ) %>%
        dplyr::ungroup()
    }
  ) %>%
    dplyr::bind_rows()

#####
## Read the standings file
###

team_standings <-
  readr::read_csv(.eh_team_standings_filepath) %>%
    dplyr::rename_with(
      .fn =
        function(x) {
          x %>%
            stringr::str_to_lower() %>%
            stringr::str_replace_all(
              c(
                "%" = "_perc",
                "±" = "_diff"
              )
            )
        }
    ) %>%
    dplyr::mutate(win_perc = w / gp) %>%
    dplyr::select(season, team, win_perc, points_perc)

#####
## Combine biographical data with on-ice and standings info
###

full_team_stats <-
  purrr::reduce(
    list(
      full_team_weighted_average_deltas,
      full_team_stats,
      team_standings
    ),
    dplyr::left_join
  )

strength_states <- c("All", "EV", "PP", "PK")

#####
## List object of correlation tibbles
###

strength_state_correlations <-
  purrr::map(
    strength_states,
    function(state) {
      full_team_stats %>%
        dplyr::filter(strength_state == state) %>%
        dplyr::select(tidyselect::vars_select_helpers$where(is.numeric)) %>%
        corrr::correlate() %>%
        dplyr::select(term, xg_diff_per_60:points_perc) %>%
        dplyr::filter(term %>% stringr::str_starts("weighted"))
    }
  ) %>%
  purrr::set_names(strength_states)

## Check to see how correlated on-ice/standings data is

purrr::map(
  strength_states,
  function(state) {
    full_team_stats %>%
      dplyr::filter(strength_state == state) %>%
      dplyr::select(tidyselect::vars_select_helpers$where(is.numeric)) %>%
      corrr::correlate() %>%
      dplyr::select(term, xg_diff_per_60:points_perc) %>%
      dplyr::filter(term %>% stringr::str_starts("weighted", negate = T))
  }
) %>%
  purrr::set_names(strength_states)

## Check to see how correlated biographical data is

purrr::map(
  strength_states,
  function(state) {
    full_team_stats %>%
      dplyr::filter(strength_state == state) %>%
      dplyr::select(tidyselect::vars_select_helpers$where(is.numeric)) %>%
      corrr::correlate() %>%
      dplyr::select(term, tidyselect::starts_with("weighted")) %>%
      dplyr::filter(term %>% stringr::str_starts("weighted"))
  }
) %>%
  purrr::set_names(strength_states)

## Check correlations using rank instead, not significantly different

purrr::map(
  strength_states,
  function(state) {
    full_team_stats %>%
      dplyr::filter(strength_state == state) %>%
      dplyr::select(tidyselect::vars_select_helpers$where(is.numeric)) %>%
      dplyr::mutate(
        dplyr::across(.fns = rank)
      ) %>%
      corrr::correlate() %>%
      dplyr::select(term, xg_diff_per_60:points_perc) %>%
      dplyr::filter(term %>% stringr::str_starts("weighted"))
  }
) %>%
  purrr::set_names(strength_states)

strength_state_correlation_scatters <-
  purrr::map(
    strength_states,
    function(state) {
      full_team_stats %>%
      dplyr::filter(strength_state == state) %>%
      tidyr::pivot_longer(
        cols = tidyselect::starts_with("weighted"),
        names_to = "x_labels",
        values_to = "x_values"
      ) %>%
      tidyr::pivot_longer(
        cols = xg_diff_per_60:points_perc,
        names_to = "y_labels",
        values_to = "y_values"
      ) %>%
      dplyr::mutate(
        x_labels = 
          x_labels %>%
          stringr::str_replace_all(
            c(
              "_" = " ",
              "ov" = "position",
              "(delta)|(weighted)" = ""
            )
          ) %>%
          stringr::str_trim() %>%
          stringr::str_to_title() %>%
          stringr::str_replace("Bmi", "BMI") %>%
          factor(
            levels =
              c("Days On Earth", "Height", "Weight", "BMI", "Draft Position")
          ),
        y_labels =
          y_labels %>%
          stringr::str_replace_all(
            c(
              "_" = " ",
              "diff" = "",
              "[\\s_]perc" = "%"
            )
          ) %>%
          stringr::str_trim() %>%
          stringr::str_to_title() %>%
          stringr::str_replace_all(
            c(
              "Xg" = "xG",
              "\\s+" = " "
            )
          ) %>%
          factor(
            levels =
              c("Shot Quality", "Shot Quantity", "xG Per 60", "Win%", "Points%")
          )
      ) %>%
      ggplot2::ggplot(
        ggplot2::aes(x = x_values, y = y_values, color = y_labels)
      ) +
      ggplot2::geom_smooth(method = "lm") +
      ggplot2::geom_point(alpha = 0.33) +
      ggplot2::facet_grid(
        y_labels ~ x_labels, scales = "free", switch = "both"
      ) +
      ggplot2::ggtitle(
        "Team On-Ice Metrics vs. Team Biographical Metrics",
        stringr::str_replace_all(
          state,
          c(
            "All" = "All Situations",
            "EV" = "Even Strength",
            "PP" = "Power Play",
            "PK" = "Penalty Kill"
          )
        )
      ) +
      ggplot2::xlab("On-Ice Metrics Above League Average By Season") +
      ggplot2::ylab("Biographical Metrics Above League Average By Season") +
      ggplot2::theme_minimal() +
      ggplot2::scale_color_viridis_d() +
      ggplot2::theme(legend.position = "none")
    }
  ) %>%
  purrr::set_names(strength_states)

strength_state_correlation_heatmaps <-
  purrr::map(
    strength_states,
    function(state) {
      strength_state_correlations %>%
        purrr::pluck(state) %>%
        tidyr::pivot_longer(
          cols = -c(term),
          names_to = "y_labels",
          values_to = "cor"
        ) %>%
        dplyr::mutate(
          term = 
            term %>%
            stringr::str_replace_all(
              c(
                "_" = " ",
                "ov" = "position",
                "(delta)|(weighted)" = ""
              )
            ) %>%
            stringr::str_trim() %>%
            stringr::str_to_title() %>%
            stringr::str_replace("Bmi", "BMI") %>%
            factor(
              levels =
                c("Days On Earth", "Height", "Weight", "BMI", "Draft Position")
            ),
          y_labels =
            y_labels %>%
            stringr::str_replace_all(
              c(
                "_" = " ",
                # "[\\s_]per[\\s_]" = "/",
                "diff" = "",
                "[\\s_]perc" = "%"
              )
            ) %>%
            stringr::str_trim() %>%
            stringr::str_to_title() %>%
            stringr::str_replace_all(
              c(
                "Xg" = "xG",
                "\\s+" = " "
              )
            ) %>%
            factor(
              levels =
                c(
                  "Shot Quality",
                  "Shot Quantity",
                  "xG Per 60",
                  "Win%", 
                  "Points%"
                ) %>%
                rev()
            )
        ) %>%
        ggplot2::ggplot(ggplot2::aes(x = term, y = y_labels, fill = cor)) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(
          ggplot2::aes(label = cor %>% round(2)), color = "white"
        ) +
        ggplot2::scale_fill_viridis_c("Correlation", limits = c(-1, 1)) +
        ggplot2::theme_minimal() +
        ggplot2::ggtitle(
          "Team On-Ice Metrics Correlation With Team Biographical Metrics",
          stringr::str_replace_all(
            state,
            c(
              "All" = "All Situations",
              "EV" = "Even Strength",
              "PP" = "Power Play",
              "PK" = "Penalty Kill"
            )
          )
        ) +
        ggplot2::xlab("On-Ice Metrics Above League Average By Season") +
        ggplot2::ylab("Biographical Metrics Above League Average By Season")
    }
  ) %>%
  purrr::set_names(strength_states)

#####
## Look at the top/bottom 10 teams
###

## Age

full_team_stats %>%
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(dplyr::desc(weighted_days_on_earth_delta)) %>% 
  dplyr::select(team, season, weighted_days_on_earth_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

full_team_stats %>% 
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(weighted_days_on_earth_delta) %>%
  dplyr::select(team, season, weighted_days_on_earth_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

## Height

full_team_stats %>%
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(dplyr::desc(weighted_height_delta)) %>% 
  dplyr::select(team, season, weighted_height_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

full_team_stats %>% 
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(weighted_height_delta) %>%
  dplyr::select(team, season, weighted_height_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

## Weight

full_team_stats %>%
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(dplyr::desc(weighted_weight_delta)) %>% 
  dplyr::select(team, season, weighted_weight_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

full_team_stats %>% 
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(weighted_weight_delta) %>%
  dplyr::select(team, season, weighted_weight_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

## BMI

full_team_stats %>%
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(dplyr::desc(weighted_bmi_delta)) %>% 
  dplyr::select(team, season, weighted_bmi_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

full_team_stats %>% 
  dplyr::filter(strength_state == "All") %>%
  dplyr::arrange(weighted_bmi_delta) %>%
  dplyr::select(team, season, weighted_bmi_delta, points_perc) %>% 
  head(10) %>% 
  knitr::kable("simple")

#####
## Simple regressions to see how much these factors contribute to points perc
###

points_percentage_model_1 <-
  full_team_stats %>%
  dplyr::filter(strength_state == "EV") %>%
  lm(
    formula = 
      points_perc ~ 
      weighted_days_on_earth_delta + 
      weighted_height_delta +
      weighted_bmi_delta
  )

points_percentage_model_2 <-
  full_team_stats %>%
  dplyr::filter(strength_state == "EV") %>%
  lm(
    formula = 
      points_perc ~ weighted_days_on_earth_delta + weighted_weight_delta
  )

points_percentage_model_3 <-
  full_team_stats %>%
  tidyr::pivot_wider(
    id_cols = c(season, team, points_perc), 
    names_from = strength_state, 
    values_from = tidyselect::starts_with("weighted")
  ) %>% 
  tidyr::unnest(cols = tidyselect::starts_with("weighted")) %>% 
  dplyr::arrange((points_perc)) %>% 
  lm(
    formula = 
      points_perc ~ weighted_days_on_earth_delta_EV + weighted_weight_delta_PP
  )  

points_percentage_model_1 %>% summary()
points_percentage_model_2 %>% summary()
points_percentage_model_3 %>% summary()
