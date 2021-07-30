playoff_pbp <-
  purrr::map(
    list.files("data/", pattern = "eh_pbp.*playoffs.csv"),
    function(filepath) readr::read_csv(stringr::str_c("data/", filepath))
  ) %>%
  dplyr::bind_rows()

player_toi_game_logs <-
  playoff_pbp %>%
  dplyr::bind_rows() %>%
  dplyr::filter(event_length > 0) %>%
  dplyr::select(
    game_id,
    game_date,
    game_strength_state,
    event_length,
    home_score,
    away_score,
    tidyselect::starts_with("home_on"),
    tidyselect::starts_with("away_on")
  ) %>%
  tidyr::pivot_longer(
    cols = tidyselect::contains("on"),
    names_to = "team",
    values_to = "player"
  ) %>%
  dplyr::filter(!is.na(player)) %>%
  # head(100) %>%
  dplyr::mutate(
    team = stringr::str_extract(team, "(home)|(away)"),
    score_state =
      purrr::map2(
        home_score,
        away_score,
        function(home, away) {
          (home - away) %>% max(-3) %>% min(3)
        }
      ) %>%
      purrr::flatten_dbl(),
    team_strength_state =
      purrr::map2(
        team,
        game_strength_state,
        function(team, ss) {
          ifelse(
            stringr::str_detect(ss, "[E012i]"),
            "Other",
            {
              players_on <-
                ss %>%
                stringr::str_split("v") %>%
                purrr::flatten_chr() %>%
                purrr::set_names("home", "away")
              
              dplyr::case_when(
                magrittr::equals(
                  players_on[team],
                  players_on[names(players_on) != team]
                ) ~ "EV",
                magrittr::is_greater_than(
                  players_on[team],
                  players_on[names(players_on) != team]
                ) ~ "PP",
                magrittr::is_less_than(
                  players_on[team],
                  players_on[names(players_on) != team]
                ) ~ "PK"
              )
            }
          )
        }
      ) %>%
      purrr::flatten_chr()
  ) %>%
  dplyr::group_by(
    game_id, 
    game_date, 
    team, 
    score_state, 
    team_strength_state, 
    player
  ) %>%
  dplyr::summarise(toi = sum(event_length)) %>%
  dplyr::left_join(
    readr::read_csv("data/eh_skater_playoffs.csv") %>%
      dplyr::select(
        player = EH_ID, 
        api_id = `API ID`,
        position = Position, 
        birthday = Birthday, 
        draft_ov = `Draft Ov`
      ) %>%
      dplyr::distinct()
  ) %>%
  dplyr::mutate(
    days_on_earth = as.integer(game_date) - as.integer(birthday)
  ) %>%
  dplyr::filter(!is.na(position)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(
    dplyr::select(., api_id) %>%
      dplyr::distinct() %>% print() %>%
      dplyr::mutate(
        player_bio =
          purrr::map(
            api_id,
            (function(id) {
              "http://statsapi.web.nhl.com/api/v1/people/api_id" %>%
                stringr::str_replace(
                  pattern = "api_id", 
                  replacement = as.character(id)
                ) %>%
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

playoff_ss_toi <-
  playoff_pbp %>%
  dplyr::group_by(game_id, game_strength_state) %>%
  dplyr::summarise(
    toi = sum(event_length)
  ) %>%
  dplyr::ungroup()

on_ice_events <- 
  c("GOAL", "SHOT", "MISS", "BLOCK", "FAC", "PENL", "GIVE", "TAKE", "HIT", "XG")

playoff_ss_event_counts <-
  playoff_pbp %>%
  dplyr::select(game_id, game_strength_state) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    counting_stats =
      purrr::map2(
        game_id,
        game_strength_state,
        function(id, ss) {
          
          playoff_pbp %>%
            dplyr::filter(
              game_id == id,
              game_strength_state == ss,
              event_type %in% on_ice_events
            ) %>%
            dplyr::group_by(
              team = 
                ifelse(event_team == home_team, "home", "away") %>%
                as.character(),
              event_type
            ) %>%
            dplyr::summarise(
              n = dplyr::n(),
              xgoal = sum(pred_goal, na.rm = T)
            ) %>%
            tidyr::pivot_longer(
              cols = c(n, xgoal),
              names_to = "type",
              values_to = "n"
            ) %>%
            dplyr::mutate(
              event_type = 
                ifelse(type == "n", event_type, "XG") %>% as.character()
            ) %>%
            dplyr::group_by(
              team,
              event_type
            ) %>%
            dplyr::summarise(n = sum(n)) %>%
            dplyr::filter(!is.na(team)) %>%
            dplyr::full_join(
              tibble::tibble(
                team = rep(c("away", "home"), each = length(on_ice_events)),
                event_type = rep(on_ice_events, 2)
              )
            ) %>%
            dplyr::mutate(n = ifelse(is.na(n), 0, n))
        }
      )
  ) %>%
  tidyr::unnest(counting_stats) %>%
  tidyr::pivot_wider(names_from = team, values_from = n) %>%
  dplyr::left_join(
    playoff_ss_toi
  )

playoff_ss_event_counts_summary <-
  playoff_ss_event_counts %>%
  dplyr::mutate(
    home_strength_state =
      purrr::map(
        game_strength_state,
        function(ss) {
          ifelse(
            stringr::str_detect(ss, "[E012i]"),
            "Other",
            {
              players_on <-
                ss %>%
                stringr::str_split("v") %>%
                purrr::flatten_chr() %>%
                purrr::set_names("home", "away")
              
              dplyr::case_when(
                magrittr::equals(
                  players_on["home"],
                  players_on["away"]
                ) ~ "EV",
                magrittr::is_greater_than(
                  players_on["home"],
                  players_on["away"]
                ) ~ "PP",
                magrittr::is_less_than(
                  players_on["home"],
                  players_on["away"]
                ) ~ "PK"
              )
            }
          )
        }
      ) %>%
      purrr::flatten_chr()
  ) %>%
  dplyr::group_by(game_id, home_strength_state, event_type) %>%
  dplyr::summarise(
    away = sum(away),
    home = sum(home),
    toi = sum(toi)
  )

team_weighted_means <-
  player_toi_game_logs %>%
  dplyr::mutate(
    home_strength_state =
      ifelse(
        team == "home",
        team_strength_state,
        dplyr::case_when(
          team_strength_state == "PP" ~ "PK",
          team_strength_state == "PK" ~ "PP",
          TRUE ~ team_strength_state
        )
      )
  ) %>%
  dplyr::group_by(
    game_id, 
    team, 
    home_strength_state, 
    position = ifelse(position == "D", "D", "F")
  ) %>%
  dplyr::summarise(
    dplyr::across(
      .cols = c(days_on_earth, height, weight, bmi),
      .fns = weighted.mean
    )
  ) %>%
  dplyr::bind_rows(
    player_toi_game_logs %>%
      dplyr::mutate(
        home_strength_state =
          ifelse(
            team == "home",
            team_strength_state,
            dplyr::case_when(
              team_strength_state == "PP" ~ "PK",
              team_strength_state == "PK" ~ "PP",
              TRUE ~ team_strength_state
            )
          )
      ) %>%
      dplyr::group_by(
        game_id, team, position = ifelse(position == "D", "D", "F")
      ) %>%
      dplyr::summarise(
        dplyr::across(
          .cols = c(days_on_earth, height, weight, bmi),
          .fns = weighted.mean
        )
      ) %>%
      dplyr::mutate(home_strength_state = "All")
  ) %>%
  dplyr::bind_rows(
    player_toi_game_logs %>%
      dplyr::mutate(
        home_strength_state =
          ifelse(
            team == "home",
            team_strength_state,
            dplyr::case_when(
              team_strength_state == "PP" ~ "PK",
              team_strength_state == "PK" ~ "PP",
              TRUE ~ team_strength_state
            )
          )
      ) %>%
      dplyr::group_by(
        game_id, team, home_strength_state
      ) %>%
      dplyr::summarise(
        dplyr::across(
          .cols = c(days_on_earth, height, weight, bmi),
          .fns = weighted.mean
        )
      ) %>%
      dplyr::bind_rows(
        player_toi_game_logs %>%
          dplyr::mutate(
            home_strength_state =
              ifelse(
                team == "home",
                team_strength_state,
                dplyr::case_when(
                  team_strength_state == "PP" ~ "PK",
                  team_strength_state == "PK" ~ "PP",
                  TRUE ~ team_strength_state
                )
              )
          ) %>%
          dplyr::group_by(game_id, team) %>%
          dplyr::summarise(
            dplyr::across(
              .cols = c(days_on_earth, height, weight, bmi),
              .fns = weighted.mean
            )
          ) %>%
          dplyr::mutate(home_strength_state = "All")
      ) %>%
      dplyr::mutate(position = "Both")
  )

game_rates_and_sizes <-
  playoff_ss_event_counts_summary %>%
  dplyr::ungroup() %>%
  dplyr::bind_rows(
    playoff_ss_event_counts_summary %>%
      dplyr::group_by(game_id, event_type) %>%
      dplyr::summarise(
        dplyr::across(
          c(away, home, toi),
          sum
        )
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        home_strength_state = "All"
      )
  ) %>%
  dplyr::mutate(
    dplyr::across(
      c(away, home),
      function(n, t) magrittr::divide_by(n, t) %>% magrittr::multiply_by(3600),
      t = toi,
      .names = "{.col}_rate"
    )
  ) %>%
  tidyr::pivot_wider(
    id_cols = c(game_id, home_strength_state),
    names_from = event_type,
    values_from = c(away_rate, home_rate)
  ) %>%
  dplyr::arrange(game_id, home_strength_state) %>%
  dplyr::inner_join(
    team_weighted_means %>%
      tidyr::pivot_wider(
        names_from = c(team, position),
        values_from = c(days_on_earth, height, weight, bmi)
      ) %>%
      dplyr::arrange(game_id, home_strength_state)
  )

team_game_rates_and_sizes_for <-
  game_rates_and_sizes %>%
  dplyr::select(game_id, home_strength_state, tidyselect::contains("home")) %>%
  dplyr::rename_with(
    .fn = function(x)
      x %>% stringr::str_replace("home_", "")
  ) %>%
  dplyr::mutate(team = "home") %>%
  dplyr::bind_rows(
    game_rates_and_sizes %>%
      dplyr::select(game_id, home_strength_state, tidyselect::contains("away")) %>%
      dplyr::rename_with(
        .fn = function(x)
          x %>% stringr::str_replace("((away)|(home))_", "")
      ) %>%
      dplyr::mutate(
        strength_state =
          dplyr::case_when(
            strength_state == "PP" ~ "PK",
            strength_state == "PK" ~ "PP",
            T ~ strength_state
          ),
        team = "away"
      )
  ) %>%
  dplyr::mutate(
    rate_CORSI = rate_GOAL + rate_SHOT + rate_MISS + rate_BLOCK,
  ) %>%
  dplyr::select(
    game_id,
    team,
    strength_state,
    g_for = rate_GOAL,
    xg_for = rate_XG,
    c_for = rate_CORSI,
    tidyselect::ends_with("_F"),
    tidyselect::ends_with("_D"),
    tidyselect::ends_with("_Both")
  ) %>%
  dplyr::rename_with(
    .cols = tidyselect::matches("_[FDB]", ignore.case = F),
    .fn =
      function(x) x %>% stringr::str_c("_for")
  )

team_game_rates_and_size <-
  team_game_rates_and_sizes_for %>%
  dplyr::left_join(
    team_game_rates_and_sizes_for %>%
      dplyr::rename_with(
        .cols = tidyselect::ends_with("_for", ignore.case = F),
        .fn =
          function(x) x %>% stringr::str_replace("for", "against")
      ) %>%
      dplyr::mutate(
        team =
          dplyr::case_when(
            team == "home" ~ "away",
            team == "away" ~ "home"
          ),
        strength_state =
          dplyr::case_when(
            strength_state == "PP" ~ "PK",
            strength_state == "PK" ~ "PP",
            T ~ strength_state
          )
      )
  )

team_game_rates_and_size %>%
  dplyr::filter(strength_state == "EV") %>%
  dplyr::select(-c(game_id, team, strength_state)) %>%
  corrr::correlate() %>%
  dplyr::filter(stringr::str_detect(term, "[cg]_")) %>%
  dplyr::select(term, tidyselect::matches("[BDF].*_for", ignore.case = F)) %>%
  tidyr::pivot_longer(-c(term), names_to = "bio", values_to = "cor") %>%
  View()


bio_rate_cors <-
  purrr::map(
    c("All", "EV", "PP", "PK"),
    function(ss) {
      tidyr::expand_grid(
        on_ice_stat =
          team_game_rates_and_size %>%
          colnames() %>%
          .[stringr::str_detect(., "[cg]_")],
        bio_stat =
          team_game_rates_and_size %>%
          colnames() %>%
          .[stringr::str_detect(., "[BDF].*_for")]
      ) %>%
        dplyr::mutate(
          cor =
            purrr::map2(
              on_ice_stat,
              bio_stat,
              function(x, y) {
                cor(
                  x = team_game_rates_and_size %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(x),
                  y = team_game_rates_and_size %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(y),
                  use = "pairwise.complete.obs"
                )
              }
            ) %>%
            purrr::flatten_dbl(),
          p_value =
            purrr::map2(
              on_ice_stat,
              bio_stat,
              function(x, y) {
                cor.test(
                  x = team_game_rates_and_size %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(x),
                  y = team_game_rates_and_size %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(y),
                  use = "pairwise.complete.obs"
                ) %>%
                  .$p.value
              }
            ) %>%
            purrr::flatten_dbl()
        )
    }
  ) %>%
  purrr::set_names(c("All", "EV", "PP", "PK"))

team_game_diffs <-
  team_game_rates_and_size %>%
  dplyr::bind_cols(
    purrr::map(
      c("c", "xg", "g"),
      function(stat) {
        magrittr::subtract(
          team_game_rates_and_size %>%
            purrr::pluck(stringr::str_c(stat, "_for")),
          team_game_rates_and_size %>%
            purrr::pluck(stringr::str_c(stat, "_against"))
        )
      }
    ) %>%
      purrr::set_names(
        c("c", "xg", "g") %>%
          stringr::str_c("_diff")
      ) %>%
      dplyr::bind_cols()
  ) %>%
  dplyr::bind_cols(
    purrr::map(
      c("c", "xg", "g"),
      function(stat) {
        magrittr::divide_by(
          team_game_rates_and_size %>%
            purrr::pluck(stringr::str_c(stat, "_for")),
          magrittr::add(
            team_game_rates_and_size %>%
              purrr::pluck(stringr::str_c(stat, "_for")),
            team_game_rates_and_size %>%
              purrr::pluck(stringr::str_c(stat, "_against"))
          )
        )
      }
    ) %>%
      purrr::set_names(
        c("c", "xg", "g") %>%
          stringr::str_c("_share")
      ) %>%
      dplyr::bind_cols()
  ) %>%
  dplyr::bind_cols(
    purrr::map(
      c("days_on_earth", "height", "weight", "bmi"),
      function(bio_stat) {
        purrr::map(
          c("F", "D", "Both"),
          function(position_for) {
            purrr::map(
              c("F", "D", "Both"),
              function(position_against) {
                magrittr::subtract(
                  team_game_rates_and_size %>%
                    purrr::pluck(
                      stringr::str_c(bio_stat, position_for, "for", sep = "_")
                    ),
                  team_game_rates_and_size %>%
                    purrr::pluck(
                      stringr::str_c(bio_stat, position_against, "against", sep = "_")
                    )
                )
              }
            )
          }
        )
      }
    ) %>%
      purrr::flatten() %>%
      purrr::flatten() %>%
      purrr::set_names(
        stringr::str_c(
          c("days_on_earth", "height", "weight", "bmi") %>% rep(each = 9),
          c("F", "D", "Both") %>% rep(times = 4, each = 3),
          c("F", "D", "Both") %>% rep(times = 12),
          "diff",
          sep = "_"
        )
      ) %>%
      dplyr::bind_cols()
  )

bio_diff_cors <-
  purrr::map(
    c("All", "EV", "PP", "PK"),
    function(ss) {
      tidyr::expand_grid(
        on_ice_stat =
          team_game_diffs %>%
          colnames() %>%
          .[stringr::str_detect(., "[cg]_")],
        bio_stat =
          team_game_diffs %>%
          colnames() %>%
          .[stringr::str_detect(., "[BDF].*")]
      ) %>%
        dplyr::mutate(
          cor =
            purrr::map2(
              on_ice_stat,
              bio_stat,
              function(x, y) {
                cor(
                  x = team_game_diffs %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(x),
                  y = team_game_diffs %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(y),
                  use = "pairwise.complete.obs"
                )
              }
            ) %>%
            purrr::flatten_dbl(),
          p_value =
            purrr::map2(
              on_ice_stat,
              bio_stat,
              function(x, y) {
                cor.test(
                  x = team_game_diffs %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(x),
                  y = team_game_diffs %>%
                    dplyr::filter(strength_state == ss) %>%
                    purrr::pluck(y),
                  use = "pairwise.complete.obs"
                ) %>%
                  .$p.value
              }
            ) %>%
            purrr::flatten_dbl()
        )
    }
  ) %>%
  purrr::set_names(c("All", "EV", "PP", "PK"))

bio_diff_cors$All %>%
  dplyr::filter(stringr::str_detect(bio_stat, "for")) %>%
  View()

game_rates_and_sizes %>%
  dplyr::filter(home_strength_state == "All") %>%
  dplyr::transmute(
    home_diff_GOAL = home_rate_GOAL - away_rate_GOAL,
    home_diff_XG = home_rate_XG - away_rate_XG,
    home_diff_CORSI =
      (home_rate_GOAL + home_rate_SHOT + home_rate_MISS + home_rate_BLOCK) -
      (away_rate_GOAL + away_rate_SHOT + away_rate_MISS + away_rate_BLOCK),
    home_share_GOAL = home_rate_GOAL / (home_rate_GOAL + away_rate_GOAL),
    home_share_XG = home_rate_XG / (home_rate_XG + away_rate_XG),
    home_share_CORSI =
      home_rate_GOAL + home_rate_SHOT + home_rate_MISS + home_rate_BLOCK / (
        home_rate_GOAL + home_rate_SHOT + home_rate_MISS + home_rate_BLOCK +
          away_rate_GOAL + away_rate_SHOT + away_rate_MISS + away_rate_BLOCK
      ),
    home_age_D_v_D = days_on_earth_home_D - days_on_earth_away_D,
    home_age_D_v_F = days_on_earth_home_D - days_on_earth_away_F,
    home_age_D_v_Both = days_on_earth_home_D - days_on_earth_away_Both,
    home_age_F_v_D = days_on_earth_home_F - days_on_earth_away_D,
    home_age_F_v_F = days_on_earth_home_F - days_on_earth_away_F,
    home_age_F_v_Both = days_on_earth_home_F - days_on_earth_away_Both,
    home_age_Both_v_D = days_on_earth_home_Both - days_on_earth_away_D,
    home_age_Both_v_F = days_on_earth_home_Both - days_on_earth_away_F,
    home_age_Both_v_Both = days_on_earth_home_Both - days_on_earth_away_Both,
    home_height_D_v_D = height_home_D - height_away_D,
    home_height_D_v_F = height_home_D - height_away_F,
    home_height_D_v_Both = height_home_D - height_away_Both,
    home_height_F_v_D = height_home_F - height_away_D,
    home_height_F_v_F = height_home_F - height_away_F,
    home_height_F_v_Both = height_home_F - height_away_Both,
    home_height_Both_v_D = height_home_Both - height_away_D,
    home_height_Both_v_F = height_home_Both - height_away_F,
    home_height_Both_v_Both = height_home_Both - height_away_Both,
    home_weight_D_v_D = weight_home_D - weight_away_D,
    home_weight_D_v_F = weight_home_D - weight_away_F,
    home_weight_D_v_Both = weight_home_D - weight_away_Both,
    home_weight_F_v_D = weight_home_F - weight_away_D,
    home_weight_F_v_F = weight_home_F - weight_away_F,
    home_weight_F_v_Both = weight_home_F - weight_away_Both,
    home_weight_Both_v_D = weight_home_Both - weight_away_D,
    home_weight_Both_v_F = weight_home_Both - weight_away_F,
    home_weight_Both_v_Both = weight_home_Both - weight_away_Both,
    home_bmi_D_v_D = bmi_home_D - bmi_away_D,
    home_bmi_D_v_F = bmi_home_D - bmi_away_F,
    home_bmi_D_v_Both = bmi_home_D - bmi_away_Both,
    home_bmi_F_v_D = bmi_home_F - bmi_away_D,
    home_bmi_F_v_F = bmi_home_F - bmi_away_F,
    home_bmi_F_v_Both = bmi_home_F - bmi_away_Both,
    home_bmi_Both_v_D = bmi_home_Both - bmi_away_D,
    home_bmi_Both_v_F = bmi_home_Both - bmi_away_F,
    home_bmi_Both_v_Both = bmi_home_Both - bmi_away_Both
  ) %>%
  corrr::correlate() %>%
  dplyr::select(
    term,
    home_diff_GOAL,
    home_diff_XG,
    home_diff_CORSI,
    home_share_GOAL,
    home_share_XG,
    home_share_CORSI
  ) %>%
  dplyr::filter(
    stringr::str_detect(term, "home_((diff)|(share))", negate = T)
  ) %>%
  tidyr::pivot_longer(
    c(home_diff_GOAL, home_diff_XG, home_diff_CORSI, home_share_GOAL, home_share_XG, home_share_CORSI),
    names_to = "metric",
    values_to = "cor"
  ) %>%
  dplyr::mutate(
    p_value =
      purrr::map(
        cor,
        function(c) {
          (c / sqrt((1 - (c ^ 2)) / (1263))) %>%
            pt(1263, lower.tail = c < 0) %>%
            magrittr::multiply_by(2)
        }
      ) %>%
      purrr::flatten_dbl()
  ) %>%
  tidyr::separate(
    term,
    # into = c("home", "bio_metric", "home_position", "v", "away_position"),
    into = c("home", "bio_metric", "position_comparison"),
    sep = "_",
    extra = "merge"
  ) %>%
  dplyr::select(-c(home)) %>%
  dplyr::mutate(
    bio_metric =
      dplyr::case_when(
        bio_metric == "age" ~ "Days On Earth",
        bio_metric == "height" ~ "Height",
        bio_metric == "weight" ~ "Weight",
        bio_metric == "bmi" ~ "BMI"
      ) %>%
      factor(levels = c("Days On Earth", "Height", "Weight", "BMI")),
    metric =
      dplyr::case_when(
        metric == "home_share_XG" ~ "xGF%",
        metric == "home_diff_XG" ~ "xG +/- per 60",
        metric == "home_share_CORSI" ~ "CF%",
        metric == "home_diff_CORSI" ~ "C +/- per 60",
        metric == "home_share_GOAL" ~ "GF%",
        metric == "home_diff_GOAL" ~ "G +/- per 60"
      ) %>%
      factor(
        levels =
          c("GF%", "G +/- per 60", "xGF%", "xG +/- per 60", "CF%", "C +/- per 60")
      ),
    # dplyr::across(
    #   c(home_position, away_position),
    #   factor,
    #   levels = c("F", "D", "Both")
    # ),
    position_comparison =
      position_comparison %>%
      stringr::str_replace("_v_", " vs ") %>%
      factor(
        levels =
          unique(.) %>%
          sort() %>%
          rev()
      )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = position_comparison,
      y = metric,
      fill = cor
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(
      label =
        cor %>%
        formatC(digits = 2, format = "f") %>%
        stringr::str_c(
          ifelse(p_value < 0.05, "*", ""),
          ifelse(p_value < 0.01, "*", ""),
          ifelse(p_value < 0.001, "*", "")
        )
    ),
    color = "white"
  ) +
  ggplot2::facet_wrap(bio_metric ~ ., ncol = 1) +
  ggplot2::scale_fill_viridis_c("Correlation", limits = c(-1, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Weighted Biographical Comparison By Position Heatmap",
    subtitle = "All Situations",
    x = "Home Team Position Versus Away Team Position",
    y = "",
    caption = "*Denotes P-Value: * < 0.05, ** < 0.01, *** < 0.001"
  )

game_rates_and_sizes %>%
  dplyr::filter(stringr::str_detect(home_strength_state, "P[PK]")) %>%
  dplyr::transmute(
    pk_diff_GOAL =
      ifelse(home_strength_state == "PK", home_rate_GOAL, away_rate_GOAL) -
      ifelse(home_strength_state == "PK", away_rate_GOAL, home_rate_GOAL),
    pk_diff_XG =
      ifelse(home_strength_state == "PK", home_rate_XG, away_rate_XG) -
      ifelse(home_strength_state == "PK", away_rate_XG, home_rate_XG),
    pk_diff_CORSI =
      ifelse(
        home_strength_state == "PK",
        home_rate_GOAL + home_rate_SHOT + home_rate_MISS + home_rate_BLOCK,
        away_rate_GOAL + away_rate_SHOT + away_rate_MISS + away_rate_BLOCK
      ) -
      ifelse(
        home_strength_state == "PK",
        away_rate_GOAL + away_rate_SHOT + away_rate_MISS + away_rate_BLOCK,
        home_rate_GOAL + home_rate_SHOT + home_rate_MISS + home_rate_BLOCK
      ),
    pk_share_GOAL =
      ifelse(home_strength_state == "PK", home_rate_GOAL, away_rate_GOAL) /
      (home_rate_GOAL + away_rate_GOAL),
    pk_share_XG =
      ifelse(home_strength_state == "PK", home_rate_XG, away_rate_XG) /
      (home_rate_XG + away_rate_XG),
    pk_share_CORSI =
      ifelse(
        home_strength_state == "PK",
        home_rate_GOAL + home_rate_SHOT + home_rate_MISS + home_rate_BLOCK,
        away_rate_GOAL
      ) / (
        home_rate_GOAL + home_rate_SHOT + home_rate_MISS + home_rate_BLOCK +
          away_rate_GOAL + away_rate_SHOT + away_rate_MISS + away_rate_BLOCK
      ),
    pk_age_D = ifelse(home_strength_state == "PP", days_on_earth_away_D, days_on_earth_home_D),
    pk_height_D = ifelse(home_strength_state == "PP", height_away_D, height_home_D),
    pk_weight_D = ifelse(home_strength_state == "PP", weight_away_D, weight_home_D),
    pk_bmi_D = ifelse(home_strength_state == "PP", bmi_away_D, bmi_home_D),
    pk_age_F = ifelse(home_strength_state == "PP", days_on_earth_away_F, days_on_earth_home_F),
    pk_height_F = ifelse(home_strength_state == "PP", height_away_F, height_home_F),
    pk_weight_F = ifelse(home_strength_state == "PP", weight_away_F, weight_home_F),
    pk_bmi_F = ifelse(home_strength_state == "PP", bmi_away_F, bmi_home_F),
    pk_age_Both = ifelse(home_strength_state == "PP", days_on_earth_away_Both, days_on_earth_home_Both),
    pk_height_Both = ifelse(home_strength_state == "PP", height_away_Both, height_home_Both),
    pk_weight_Both = ifelse(home_strength_state == "PP", weight_away_Both, weight_home_Both),
    pk_bmi_Both = ifelse(home_strength_state == "PP", bmi_away_Both, bmi_home_Both),
    pp_age_D = ifelse(home_strength_state == "PK", days_on_earth_away_D, days_on_earth_home_D),
    pp_height_D = ifelse(home_strength_state == "PK", height_away_D, height_home_D),
    pp_weight_D = ifelse(home_strength_state == "PK", weight_away_D, weight_home_D),
    pp_bmi_D = ifelse(home_strength_state == "PK", bmi_away_D, bmi_home_D),
    pp_age_F = ifelse(home_strength_state == "PK", days_on_earth_away_F, days_on_earth_home_F),
    pp_height_F = ifelse(home_strength_state == "PK", height_away_F, height_home_F),
    pp_weight_F = ifelse(home_strength_state == "PK", weight_away_F, weight_home_F),
    pp_bmi_F = ifelse(home_strength_state == "PK", bmi_away_F, bmi_home_F),
    pp_age_Both = ifelse(home_strength_state == "PK", days_on_earth_away_Both, days_on_earth_home_Both),
    pp_height_Both = ifelse(home_strength_state == "PK", height_away_Both, height_home_Both),
    pp_weight_Both = ifelse(home_strength_state == "PK", weight_away_Both, weight_home_Both),
    pp_bmi_Both = ifelse(home_strength_state == "PK", bmi_away_Both, bmi_home_Both),
    pk_age_D_v_D = pk_age_D - pp_age_D,
    pk_age_D_v_F = pk_age_D - pp_age_F,
    pk_age_D_v_Both = pk_age_D - pp_age_Both,
    pk_age_F_v_D = pk_age_F - pp_age_D,
    pk_age_F_v_F = pk_age_F - pp_age_F,
    pk_age_F_v_Both = pk_age_F - pp_age_Both,
    pk_age_Both_v_D = pk_age_Both - pp_age_D,
    pk_age_Both_v_F = pk_age_Both - pp_age_F,
    pk_age_Both_v_Both = pk_age_Both - pp_age_Both,
    pk_height_D_v_D = pk_height_D - pp_height_D,
    pk_height_D_v_F = pk_height_D - pp_height_F,
    pk_height_D_v_Both = pk_height_D - pp_height_Both,
    pk_height_F_v_D = pk_height_F - pp_height_D,
    pk_height_F_v_F = pk_height_F - pp_height_F,
    pk_height_F_v_Both = pk_height_F - pp_height_Both,
    pk_height_Both_v_D = pk_height_Both - pp_height_D,
    pk_height_Both_v_F = pk_height_Both - pp_height_F,
    pk_height_Both_v_Both = pk_height_Both - pp_height_Both,
    pk_weight_D_v_D = pk_weight_D - pp_weight_D,
    pk_weight_D_v_F = pk_weight_D - pp_weight_F,
    pk_weight_D_v_Both = pk_weight_D - pp_weight_Both,
    pk_weight_F_v_D = pk_weight_F - pp_weight_D,
    pk_weight_F_v_F = pk_weight_F - pp_weight_F,
    pk_weight_F_v_Both = pk_weight_F - pp_weight_Both,
    pk_weight_Both_v_D = pk_weight_Both - pp_weight_D,
    pk_weight_Both_v_F = pk_weight_Both - pp_weight_F,
    pk_weight_Both_v_Both = pk_weight_Both - pp_weight_Both,
    pk_bmi_D_v_D = pk_bmi_D - pp_bmi_D,
    pk_bmi_D_v_F = pk_bmi_D - pp_bmi_F,
    pk_bmi_D_v_Both = pk_bmi_D - pp_bmi_Both,
    pk_bmi_F_v_D = pk_bmi_F - pp_bmi_D,
    pk_bmi_F_v_F = pk_bmi_F - pp_bmi_F,
    pk_bmi_F_v_Both = pk_bmi_F - pp_bmi_Both,
    pk_bmi_Both_v_D = pk_bmi_Both - pp_bmi_D,
    pk_bmi_Both_v_F = pk_bmi_Both - pp_bmi_F,
    pk_bmi_Both_v_Both = pk_bmi_Both - pp_bmi_Both
  ) %>%
  corrr::correlate() %>%
  dplyr::select(
    term,
    pk_diff_GOAL,
    pk_diff_XG,
    pk_diff_CORSI,
    pk_share_GOAL,
    pk_share_XG,
    pk_share_CORSI
  ) %>%
  dplyr::filter(
    stringr::str_detect(term, "pk_((rate)|(share))", negate = T),
    stringr::str_detect(term, "_v_")
  ) %>%
  tidyr::pivot_longer(
    c(pk_diff_GOAL, pk_diff_XG, pk_diff_CORSI, pk_share_GOAL, pk_share_XG, pk_share_CORSI),
    names_to = "metric",
    values_to = "cor"
  ) %>%
  dplyr::mutate(
    p_value =
      purrr::map(
        cor,
        function(c) {
          (c / sqrt((1 - (c ^ 2)) / (2491))) %>%
            pt(2491, lower.tail = c < 0) %>%
            magrittr::multiply_by(2)
        }
      ) %>%
      purrr::flatten_dbl()
  ) %>%
  tidyr::separate(
    term,
    # into = c("home", "bio_metric", "home_position", "v", "away_position"),
    into = c("home", "bio_metric", "position_comparison"),
    sep = "_",
    extra = "merge"
  ) %>%
  dplyr::select(-c(home)) %>%
  dplyr::mutate(
    bio_metric =
      dplyr::case_when(
        bio_metric == "age" ~ "Days On Earth",
        bio_metric == "height" ~ "Height",
        bio_metric == "weight" ~ "Weight",
        bio_metric == "bmi" ~ "BMI"
      ) %>%
      factor(levels = c("Days On Earth", "Height", "Weight", "BMI")),
    metric =
      dplyr::case_when(
        metric == "pk_share_XG" ~ "xGF%",
        metric == "pk_diff_XG" ~ "xG +/- per 60",
        metric == "pk_share_CORSI" ~ "CF%",
        metric == "pk_diff_CORSI" ~ "C +/- per 60",
        metric == "pk_share_GOAL" ~ "GF%",
        metric == "pk_diff_GOAL" ~ "G +/- per 60"
      ) %>%
      factor(
        levels =
          c("GF%", "G +/- per 60", "xGF%", "xG +/- per 60", "CF%", "C +/- per 60")
      ),
    position_comparison =
      position_comparison %>%
      stringr::str_replace("_v_", " vs ") %>%
      factor(
        levels =
          unique(.) %>%
          sort() %>%
          rev()
      )
  ) %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = position_comparison,
      y = metric,
      fill = cor
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(
      label =
        cor %>%
        formatC(digits = 2, format = "f") %>%
        stringr::str_c(
          ifelse(p_value < 0.05, "*", ""),
          ifelse(p_value < 0.01, "*", ""),
          ifelse(p_value < 0.001, "*", "")
        )
    ),
    color = "white"
  ) +
  ggplot2::facet_wrap(bio_metric ~ ., ncol = 1) +
  ggplot2::scale_fill_viridis_c("Correlation", limits = c(-1, 1)) +
  ggplot2::theme_minimal() +
  ggplot2::labs(
    title = "Weighted Biographical Comparison By Position Heatmap",
    subtitle = "Penalty Kill",
    x = "Home Team Position Versus Away Team Position",
    y = "",
    caption = "*Denotes P-Value: * < 0.05, ** < 0.01, *** < 0.001"
  )

