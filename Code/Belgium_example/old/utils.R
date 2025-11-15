# Functions ---------------------------------------------------------------

lift_curve_table <- function(
  data,
  predicted_loss_cost,
  observed_loss_cost,
  exposure,
  n = 10
) {
  predicted_loss_cost <- pull(data, {{ predicted_loss_cost }})
  observed_loss_cost <- pull(data, {{ observed_loss_cost }})
  exposure <- pull(data, {{ exposure }})

  mean_mod <- weighted.mean(predicted_loss_cost, exposure)
  # mean_obs <- weighted.mean(observed_loss_cost, exposure)

  tibble(
    pred_lc = predicted_loss_cost,
    obs_lc = observed_loss_cost,
    exp = exposure
  ) %>%
    arrange(pred_lc) %>%
    mutate(buckets = cut_interval(cumsum(exp), n = n, labels = 1:n)) %>%
    group_by(buckets) %>%
    summarise(
      Predicted_Risk_Premium = weighted.mean(pred_lc, exp, na.rm = TRUE) /
        mean_mod,
      Observed_Risk_Premium = weighted.mean(obs_lc, exp, na.rm = TRUE) /
        mean_mod
    )
}

lift_curve_plot <- function(
  tbl_in,
  pred_name = "Predicted",
  obs_name = "Observed"
) {
  tbl_in %>%
    tidyr::pivot_longer(c(Predicted_Risk_Premium, Observed_Risk_Premium)) %>%
    mutate(buckets = as.factor(buckets)) %>%
    ggplot() +
    geom_point(aes(x = buckets, y = value, col = name, group = name)) +
    geom_line(aes(x = buckets, y = value, col = name, group = name)) +
    scale_color_hue(
      labels = c(
        "Predicted_Risk_Premium" = pred_name,
        "Observed_Risk_Premium" = obs_name
      )
    ) +
    labs(x = "Bucket", y = "Relative Loss Cost") +
    # scale_y_continuous(limits = c(0, NA)) +
    labs(color = "")
}

double_lift_curve_table <- function(
  data,
  predicted_loss_cost_mod_1,
  predicted_loss_cost_mod_2,
  observed_loss_cost,
  exposure,
  n = 10,
  norm = FALSE
) {
  predicted_loss_cost_mod_1 <- pull(data, {{ predicted_loss_cost_mod_1 }})
  predicted_loss_cost_mod_2 <- pull(data, {{ predicted_loss_cost_mod_2 }})
  observed_loss_cost <- pull(data, {{ observed_loss_cost }})
  exposure <- pull(data, {{ exposure }})

  mean_mod_1 <- weighted.mean(predicted_loss_cost_mod_1, exposure)
  mean_mod_2 <- weighted.mean(predicted_loss_cost_mod_2, exposure)
  mean_obs <- weighted.mean(observed_loss_cost, exposure)

  dataset <- tibble(
    pred_lc_m1 = {{ predicted_loss_cost_mod_1 }},
    pred_lc_m2 = {{ predicted_loss_cost_mod_2 }},
    obs_lc = {{ observed_loss_cost }},
    exp = {{ exposure }}
  ) %>%
    mutate(sort_ratio = pred_lc_m1 / pred_lc_m2) %>%
    arrange(sort_ratio) %>%
    mutate(buckets = cut_interval(cumsum(exp), n = n, labels = 1:n)) %>%
    group_by(buckets) %>%
    summarise(
      Model_1_Predicted_Risk_Premium = weighted.mean(pred_lc_m1, exp) /
        mean_mod_1,
      Model_2_Predicted_Risk_Premium = weighted.mean(pred_lc_m2, exp) /
        mean_mod_2,
      Observed_Risk_Premium = weighted.mean(obs_lc, exp) / mean_obs,
      Exposure = sum(exp)
    )

  if (norm) {
    dataset <- dataset %>%
      mutate(
        Model_1_Predicted_Risk_Premium = Model_1_Predicted_Risk_Premium /
          Observed_Risk_Premium -
          1,
        Model_2_Predicted_Risk_Premium = Model_2_Predicted_Risk_Premium /
          Observed_Risk_Premium -
          1,
        Observed_Risk_Premium = Observed_Risk_Premium /
          Observed_Risk_Premium -
          1
      )
  }

  return(dataset)
}

double_lift_curve_plot <- function(tbl_in) {
  tbl_in %>%
    tidyr::pivot_longer(c(
      Model_1_Predicted_Risk_Premium,
      Model_2_Predicted_Risk_Premium,
      Observed_Risk_Premium
    )) %>%
    mutate(buckets = as.factor(buckets)) %>%
    ggplot() +
    geom_point(aes(x = buckets, y = value, col = name, group = name)) +
    geom_line(aes(x = buckets, y = value, col = name, group = name)) +
    labs(x = "Bucket", y = "Predicted Loss Cost") +
    labs(color = "")
}

gini_value <- function(
  data,
  observed_loss_cost,
  predicted_loss_cost,
  exposure
) {
  dataset <- tibble(
    obs_lc = pull(data, {{ observed_loss_cost }}),
    pred_lc = pull(data, {{ predicted_loss_cost }}),
    exp = pull(data, {{ exposure }})
  )

  dataset %>%
    arrange(pred_lc) %>%
    mutate(
      losses = obs_lc * exp,
      cum_exp = cumsum(exp) / sum(exp),
      cum_losses = cumsum(losses) / sum(losses)
    ) %$%
    {
      trapz(cum_exp, cum_losses) %>%
        add(-1) %>%
        abs() %>%
        subtract(.5) %>%
        multiply_by(2)
    }
}

gini_plot <- function(data, observed_loss_cost, predicted_loss_cost, exposure) {
  dataset <- tibble(
    obs_lc = pull(data, {{ observed_loss_cost }}),
    pred_lc = pull(data, {{ predicted_loss_cost }}),
    exp = pull(data, {{ exposure }})
  )

  dataset %>%
    arrange(pred_lc) %>%
    mutate(
      losses = obs_lc * exp,
      cum_exp = cumsum(exp) / sum(exp),
      cum_losses = cumsum(losses) / sum(losses)
    ) %>%
    ggplot() +
    geom_line(aes(x = cum_exp, y = cum_losses, group = 1)) +
    geom_abline(intercept = 0, slope = 1) +
    labs(x = "Exposure", y = "Losses") +
    labs(color = "")
}
