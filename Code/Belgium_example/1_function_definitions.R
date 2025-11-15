# ============================================================================
# Function Definitions
# ============================================================================
# This file contains custom functions for:
#   - Computing actuarial metrics (loss ratio, loss cost, frequency, severity)
#   - Creating visualizations for model evaluation
#   - Computing specialized metrics (Gini, Jensen-Shannon divergence, quantile loss)
#   - Generating lift curves and calibration plots
# ============================================================================

# ----------------------------------------------------------------------------
# act_metrics: Compute actuarial metrics by grouping variable
# ----------------------------------------------------------------------------
# Computes key insurance metrics (Loss Ratio, Loss Cost, Frequency, Severity)
# grouped by a specified variable. Supports both categorical and numerical
# grouping variables with various binning strategies.
#
# Arguments:
#   data_in: Input data frame
#   group_variable: Variable to group by (can be categorical or numerical)
#   losses: Total losses/claims amount
#   premium: Total premium
#   exposure: Total exposure (e.g., policy years)
#   numbers: Number of claims
#   numerical: If TRUE, treats group_variable as numerical and bins it
#   relative: If TRUE, returns metrics relative to overall average
#   n: Number of bins for numerical variables
#   bucket_type: Method for binning ("equal_range", "n_policies", "premium", "exposure")
#   var_labels: Labels for bins
#
# Returns:
#   Data frame with grouped metrics: LR (Loss Ratio), LC (Loss Cost),
#   Freq (Frequency), Sev (Severity)
# ----------------------------------------------------------------------------
act_metrics <- function(
  data_in,
  group_variable,
  losses,
  premium,
  exposure,
  numbers,
  numerical = FALSE,
  relative = FALSE,
  n = 10,
  bucket_type = c("equal_range", "n_policies", "premium", "exposure"),
  var_labels = FALSE
) {
  bucket_type <- match.arg(bucket_type)
  data_in <- data_in %>% arrange({{ group_variable }})

  if (numerical) {
    group_col <- switch(
      bucket_type,
      equal_range = cut_interval(
        jitter(pull(data_in, {{ group_variable }})),
        n,
        labels = var_labels
      ),
      n_policies = cut_number(
        jitter(pull(data_in, {{ group_variable }})),
        n,
        labels = var_labels
      ),
      exposure = cut_interval(
        cumsum(pull(data_in, {{ exposure }})),
        n,
        labels = var_labels
      ),
      premium = cut_interval(
        cumsum(pull(data_in, {{ premium }})),
        n,
        labels = var_labels
      )
    )

    data_in <- mutate(data_in, {{ group_variable }} := as_factor(group_col))
  }

  act_df <- data_in %>%
    group_by({{ group_variable }}) %>%
    summarise(
      across(
        c({{ losses }}, {{ numbers }}, {{ premium }}, {{ exposure }}),
        ~ sum(.x, na.rm = TRUE)
      ),
      .groups = "drop"
    ) %>%
    mutate(
      LR = ({{ losses }} / {{ premium }}),
      LC = ({{ losses }} / {{ exposure }}),
      Freq = ({{ numbers }} / {{ exposure }}),
      Sev = if_else({{ numbers }} == 0, 0, {{ losses }} / {{ numbers }})
    )

  if (relative) {
    totals <- data_in %>%
      summarise(across(
        c({{ losses }}, {{ numbers }}, {{ premium }}, {{ exposure }}),
        ~ sum(.x, na.rm = TRUE)
      )) %>%
      mutate(
        LR_avg = ({{ losses }} / {{ premium }}),
        LC_avg = ({{ losses }} / {{ exposure }}),
        Freq_avg = ({{ numbers }} / {{ exposure }}),
        Sev_avg = if_else({{ numbers }} == 0, 0, {{ losses }} / {{ numbers }})
      )

    act_df <- act_df %>%
      mutate(
        LR = LR / pull(totals, LR_avg),
        LC = LC / pull(totals, LC_avg),
        Freq = Freq / pull(totals, Freq_avg),
        Sev = Sev / pull(totals, Sev_avg)
      )
  }

  attr(act_df, "relative") <- relative
  attr(act_df, "vars") <- list(
    group_variable = enquo(group_variable),
    losses = enquo(losses),
    numbers = enquo(numbers),
    premium = enquo(premium),
    exposure = enquo(exposure)
  )

  act_df
}

# ----------------------------------------------------------------------------
# plot_metric: Create dual-axis plot for actuarial metrics
# ----------------------------------------------------------------------------
# Creates a visualization with a primary metric (line) and secondary metric
# (bars) on dual axes. Useful for comparing loss cost with exposure, etc.
#
# Arguments:
#   df: Data frame from act_metrics()
#   main_metric: Primary metric to plot as line (e.g., Loss Cost)
#   sec_metric: Secondary metric to plot as bars (e.g., Exposure)
#   main_label: Label for primary metric
#   sec_label: Label for secondary metric
#   line_color: Color for the line
#   bar_fill: Fill color for bars
#   percentage: If TRUE, format y-axis as percentage
#   var_order: Variable to order groups by (optional)
#
# Returns:
#   ggplot object
# ----------------------------------------------------------------------------
plot_metric <- function(
  df,
  main_metric,
  sec_metric,
  main_label,
  sec_label,
  line_color,
  bar_fill,
  percentage,
  var_order
) {
  relative <- attr(df, "relative")
  group_variable <- attr(df, "vars")$group_variable

  var_order <- enquo(var_order)

  if (rlang::quo_is_null(var_order)) {
    df <- df %>%
      mutate({{ group_variable }} := as_factor({{ group_variable }}))
  } else {
    df <- df %>%
      mutate(
        {{ group_variable }} := fct_reorder(
          as_factor({{ group_variable }}),
          !!var_order
        )
      )
  }

  ggplot(df, aes(x = {{ group_variable }})) +
    geom_bar(
      aes(
        y = {{ sec_metric }} * max({{ main_metric }}) / max({{ sec_metric }})
      ),
      stat = "identity",
      fill = bar_fill,
      alpha = 0.7
    ) +
    geom_line(
      aes(y = {{ main_metric }}, group = 1),
      color = line_color,
      linewidth = 2
    ) +
    geom_point(aes(y = {{ main_metric }}), color = line_color, size = 3) +
    geom_hline(
      yintercept = if (relative) 1 else
        weighted.mean(pull(df, {{ main_metric }}), pull(df, {{ sec_metric }})),
      linetype = "dashed"
    ) +
    scale_y_continuous(
      labels = if (percentage | relative) percent_format() else comma_format(),
      sec.axis = sec_axis(
        ~ . *
          max(pull(df, {{ sec_metric }})) /
          max(pull(df, {{ main_metric }})),
        name = sec_label,
        labels = comma_format()
      )
    ) +
    labs(
      x = as_label(group_variable),
      y = if (relative) glue("Relative {main_label}") else main_label,
      title = main_label
    ) +
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      text = element_text(size = 14)
    )
}

# ----------------------------------------------------------------------------
# act_plots: Generate all actuarial metric plots
# ----------------------------------------------------------------------------
# Creates four plots: Loss Ratio, Loss Cost, Frequency, and Severity
# using the act_metrics() function output.
#
# Arguments:
#   act_df: Output from act_metrics()
#   premium_name: Custom name for premium variable
#   exposure_name: Custom name for exposure variable
#   numbers_name: Custom name for numbers variable
#   var_order: Variable to order groups by
#   main_title: Main title for plots
#
# Returns:
#   List of four ggplot objects: lr_plot, lc_plot, freq_plot, sev_plot
# ----------------------------------------------------------------------------
act_plots <- function(
  act_df,
  premium_name = NULL,
  exposure_name = NULL,
  numbers_name = NULL,
  var_order = NULL,
  main_title = NULL
) {
  vars <- attr(act_df, "vars")
  group_variable <- vars$group_variable
  prem_var <- vars$premium
  exp_var <- vars$exposure
  num_var <- vars$numbers

  premium_name <- premium_name %||% as_label(prem_var)
  exposure_name <- exposure_name %||% as_label(exp_var)
  numbers_name <- numbers_name %||% as_label(num_var)

  lr_plot <- plot_metric(
    act_df,
    LR,
    !!prem_var,
    "Loss Ratio",
    premium_name,
    "#60D1BE",
    "#0B1233",
    TRUE,
    {{ var_order }}
  )
  lc_plot <- plot_metric(
    act_df,
    LC,
    !!exp_var,
    "Loss Cost",
    exposure_name,
    "#51127c",
    "#fc8961",
    FALSE,
    {{ var_order }}
  )
  freq_plot <- plot_metric(
    act_df,
    Freq,
    !!exp_var,
    "Frequency",
    exposure_name,
    "#60D1BE",
    "#224FF2",
    TRUE,
    {{ var_order }}
  )
  sev_plot <- plot_metric(
    act_df,
    Sev,
    !!num_var,
    "Severity",
    numbers_name,
    "#60D1BE",
    "#819FF8",
    FALSE,
    {{ var_order }}
  )

  # (lr_plot + lc_plot) /
  #   (freq_plot + sev_plot) +
  #   plot_annotation(
  #     title = if (is.null(main_title)) as_label(group_variable) else main_title
  #   )

  return(list(
    lr_plot = lr_plot,
    lc_plot = lc_plot,
    freq_plot = freq_plot,
    sev_plot = sev_plot
  ))
}

lift_curve <- function(
  data_in,
  lc_pred,
  lc_obs,
  exposure,
  n = 10,
  relative = FALSE,
  top_perc = NULL,
  top_n = 5
) {
  data_buckets <- data_in %>%
    arrange({{ lc_pred }}) %>%
    mutate(
      bucket = cut_interval(cumsum({{ exposure }}), n = n, label = FALSE) %>%
        as_factor()
    )

  if (!is_null(top_perc)) {
    data_buckets <- data_buckets %>%
      filter(bucket %in% top_perc) %>%
      mutate(
        bucket = cut_interval(
          cumsum({{ exposure }}),
          n = top_n,
          label = FALSE
        ) %>%
          as_factor()
      )
  }

  mean_pred <- ifelse(
    !relative,
    1,
    weighted.mean(
      pull(data_buckets, {{ lc_pred }}),
      pull(data_buckets, {{ exposure }})
    )
  )

  data_buckets %>%
    group_by(bucket) %>%
    summarise(
      mean_obs = weighted.mean({{ lc_obs }}, {{ exposure }}) / mean_pred,
      mean_pred = weighted.mean({{ lc_pred }}, {{ exposure }}) / mean_pred
    ) %>%
    pivot_longer(-bucket) %>%
    ggplot() +
    geom_line(
      aes(x = bucket, y = value, group = name, col = name),
      linewidth = 2
    ) +
    geom_point(aes(x = bucket, y = value, group = name, col = name), size = 4) +
    scale_color_manual(
      values = c("#451077FF", "#F1605DFF"),
      labels = c("Observed LC", "Predicted LC"),
      name = ""
    ) +
    ylab(if_else(relative, "Relative Loss Cost", "Loss Cost")) +
    xlab("Exposure bucket") +
    theme_bw(base_size = 22) +
    ggtitle("Lift Curve")
}

gini_index <- function(table_in, obs_losses, pred_lc, exposure, name_pred) {
  table_in %>%
    arrange({{ pred_lc }}) %>%
    mutate(
      EXP = cumsum({{ exposure }}) / sum({{ exposure }}),
      LOSSES = cumsum({{ obs_losses }}) / sum({{ obs_losses }})
    ) %>%
    {
      ggplot(.) +
        geom_line(aes(x = EXP, y = LOSSES), col = "#FD9567FF", linewidth = 2) +
        geom_ribbon(
          aes(x = EXP, ymin = LOSSES, ymax = EXP),
          alpha = .3,
          fill = "#FD9567FF"
        ) +
        geom_line(aes(x = LOSSES, y = LOSSES), col = "#0B1233", linewidth = 2) +
        scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
        scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
        ggtitle(glue(
          "{name_pred} Gini Index : { round(2*(.5 - trapz(.$EXP, .$LOSSES)),3)}"
        )) +
        xlab("EXPOSURE") +
        theme_bw(base_size = 22)
    }
}

lr_chart <- function(table_in, obs_losses, pred_lc, exposure, premium, n) {
  table_in %>%
    mutate(
      pred_LR = {{ pred_lc }} * {{ exposure }} / {{ premium }},
      obs_LR = {{ obs_losses }} / {{ premium }}
    ) %>%
    arrange(pred_LR) %>%
    mutate(bucket = cut_interval(cumsum({{ exposure }}), n, labels = FALSE)) %>%
    group_by(bucket) %>%
    summarise(LR = weighted.mean(obs_LR, {{ premium }})) %>%
    ggplot() +
    geom_bar(
      aes(x = as_factor(bucket), y = LR),
      stat = "identity",
      fill = "#819FF8"
    ) +
    theme_bw(base_size = 22) +
    ggtitle("Loss Ratio Chart") +
    xlab("Bucket") +
    scale_y_continuous(labels = percent_format())
}

var_avse <- function(
  data_in,
  var,
  pred_lc,
  obs_lc,
  exposure,
  sec_name = "Exposure",
  numerical = FALSE,
  n = 10,
  bucket_type = c("equal_range", "n_policies", "exposure"),
  var_labels = FALSE,
  relative = FALSE
) {
  mean_pred <- ifelse(
    !relative,
    1,
    weighted.mean(pull(data_in, {{ pred_lc }}), pull(data_in, {{ exposure }}))
  )

  bucket_type <- match.arg(bucket_type)
  data_in <- data_in %>% arrange({{ var }})

  if (numerical) {
    group_col <- switch(
      bucket_type,
      equal_range = cut_interval(
        jitter(pull(data_in, {{ var }})),
        n,
        labels = var_labels
      ),
      n_policies = cut_number(
        jitter(pull(data_in, {{ var }})),
        n,
        labels = var_labels
      ),
      exposure = cut_interval(
        cumsum(pull(data_in, {{ exposure }})),
        n,
        labels = var_labels
      )
    )

    data_in <- mutate(data_in, {{ var }} := as_factor(group_col))
  }

  df_summary <- data_in %>%
    group_by({{ var }}) %>%
    summarise(
      {{ obs_lc }} := weighted.mean({{ obs_lc }}, {{ exposure }}) / mean_pred,
      {{ pred_lc }} := weighted.mean({{ pred_lc }}, {{ exposure }}) / mean_pred,
      {{ exposure }} := sum({{ exposure }}) / mean_pred,
      .groups = "drop"
    )

  max_lc <- max(pull(df_summary, {{ obs_lc }}), pull(df_summary, {{ pred_lc }}))
  max_exposure <- max(pull(df_summary, {{ exposure }}))
  plot_scale_factor <- max_lc / max_exposure

  df_lc <- df_summary %>%
    select({{ var }}, {{ obs_lc }}, {{ pred_lc }}) %>%
    pivot_longer(
      cols = c({{ obs_lc }}, {{ pred_lc }}),
      names_to = "LC_Type",
      values_to = "Loss_Cost"
    )

  df_exposure <- df_summary %>%
    select({{ var }}, {{ exposure }})

  ggplot() +
    geom_bar(
      data = df_exposure,
      aes(x = as_factor({{ var }}), y = {{ exposure }} * plot_scale_factor),
      stat = "identity",
      fill = "#182556",
      alpha = 0.7
    ) +
    geom_line(
      data = df_lc,
      aes(
        x = as_factor({{ var }}),
        y = Loss_Cost,
        color = LC_Type,
        group = LC_Type
      ),
      linewidth = 1.5
    ) +
    geom_point(
      data = df_lc,
      aes(x = as_factor({{ var }}), y = Loss_Cost, color = LC_Type),
      size = 3
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ . / plot_scale_factor,
        name = sec_name,
        labels = comma_format()
      )
    ) +
    scale_color_manual(
      values = c("#819FF8", "#60D1BE"),
      labels = c("Observed LC", "Predicted LC"),
      name = ""
    ) +
    labs(
      x = as_label(enquo(var)),
      y = if (relative) "Relative Loss Cost" else "Loss Cost",
      title = glue("Observed vs Predicted Loss Cost by {as_label(enquo(var))}")
    ) +
    theme_bw(base_size = 22) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
}

agg_one_way <- function(..., numerical = TRUE) {
  vars <- enquos(...)

  map(
    vars,
    ~ {
      claims %>%
        act_metrics(
          !!.x,
          amount,
          exposure,
          exposure,
          nclaims,
          numerical = numerical,
          ,
          relative = TRUE
        ) %>%
        act_plots() %>%
        extract2("lc_plot")
    }
  )
}

# eval_regr <- function(data_in, .pred, .obs, outliers = FALSE) {
#
#   data_in <- data_in %>%
#     select(.pred = {{.pred}}, .obs = {{.obs}})
#
#   histogram <- data_in %>%
#       pivot_longer(everything()) %>%
#       ggplot(aes(x = value, fill = name)) +
#       geom_histogram(position = "dodge", bins = 10) +
#       scale_fill_manual(values = c(".pred" = "#451077FF", ".obs" = "#F1605DFF"),
#                       labels = c(".pred" = "Predicted", ".obs" = "Observed"),
#                       name = "") +
#     ggtitle("A vs E: Histogram")
#
#   density <- data_in %>%
#     pivot_longer(everything()) %>%
#     ggplot(aes(x = value, fill = name)) +
#     geom_density(alpha = 0.7) +
#     scale_fill_manual(values = c(".pred" = "#451077FF", ".obs" = "#F1605DFF"),
#                       labels = c(".pred" = "Predicted", ".obs" = "Observed"),
#                       name = "") +
#     ggtitle("A vs E: Density")
#
#   box <- data_in %>%
#     pivot_longer(everything()) %>%
#     ggplot(aes(x = name, y = value, fill = name)) +
#     geom_boxplot(outliers = outliers) +
#     scale_fill_manual(values = c(".pred" = "#451077FF", ".obs" = "#F1605DFF"),
#                       labels = c(".pred" = "Predicted", ".obs" = "Observed"),
#                       name = "") +
#     ggtitle("A vs E: Boxplot")
#
#   scatter <- data_in %>%
#     ggplot(aes(x = .obs, y = .pred)) +
#     geom_point(alpha = 0.6) +
#     geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
#     ggtitle("A vs E: Scatterplot") +
#     coord_obs_pred()
#
#   wrap_plots(list(histogram, density, box, scatter)) + plot_layout(ncol = 2, widths = c(1, 1))
# }

# ============================================================================
# Custom Metrics for Model Evaluation
# ============================================================================
# These functions implement specialized metrics for insurance model evaluation:
#   - Jensen-Shannon Divergence: Measures distributional similarity
#   - Gini Coefficient: Measures risk discrimination ability
#   - Quantile Loss: Evaluates performance at different quantiles
# ============================================================================

# ----------------------------------------------------------------------------
# js_divergence_vec: Weighted Jensen-Shannon divergence (vectorized)
# ----------------------------------------------------------------------------
# Jensen-Shannon divergence measures how different two probability distributions
# are. Lower values indicate better distributional alignment. This is useful
# for insurance models where the distribution of predictions should match
# the distribution of actual losses.
#
# Arguments:
#   truth: Observed values
#   estimate: Predicted values
#   case_weights: Weights (typically exposure)
#   na_rm: Remove NA values
#
# Returns:
#   Scalar JSD value (in bits)
# ----------------------------------------------------------------------------
js_divergence_vec <- function(
  truth,
  estimate,
  case_weights = NULL,
  na_rm = TRUE,
  ...
) {
  # Check for zero-length inputs
  if (length(truth) == 0) return(NA_real_)

  # Remove missing
  if (na_rm) {
    keep <- !(is.na(truth) | is.na(estimate))
    truth <- truth[keep]
    estimate <- estimate[keep]
    if (!is.null(case_weights)) case_weights <- case_weights[keep]
  }

  # Check again after NA removal
  if (length(truth) == 0) return(NA_real_)

  # Default weights
  if (is.null(case_weights)) case_weights <- rep(1, length(truth))

  # Non-negative
  truth <- pmax(truth, 0)
  estimate <- pmax(estimate, 0)
  case_weights <- pmax(case_weights, 0)

  # Weighted
  truth_w <- truth * case_weights
  estimate_w <- estimate * case_weights

  # Normalize
  truth_w <- truth_w / sum(truth_w)
  estimate_w <- estimate_w / sum(estimate_w)

  # KL helper
  KL <- function(p, q) sum(p * log((p + 1e-12) / (q + 1e-12)))

  # Mixture
  M <- 0.5 * (truth_w + estimate_w)

  # JSD in bits
  JSD_bits <- 0.5 * KL(truth_w, M) + 0.5 * KL(estimate_w, M)
  JSD_bits / log(2)
}

# ----------------------------------------------------------------------------
# js_divergence: Tidy wrapper for Jensen-Shannon divergence
# ----------------------------------------------------------------------------
# Wrapper function compatible with yardstick for use in tidymodels workflows.
# ----------------------------------------------------------------------------
js_divergence <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "js_divergence",
    fn = js_divergence_vec,
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    case_weights = {{ case_weights }}
  )
}

class(js_divergence) <- c("numeric_metric", "function")
attr(js_divergence, "direction") <- "minimize"

# Load required libraries for metric functions
library(yardstick)
library(DescTools)

# ----------------------------------------------------------------------------
# gini_vec: Gini coefficient (vectorized)
# ----------------------------------------------------------------------------
# The Gini coefficient measures inequality/concentration. For insurance models,
# it measures how well the model discriminates between high and low risk.
# Higher Gini = better discrimination.
#
# Arguments:
#   truth: Observed values (not used, but required for yardstick compatibility)
#   estimate: Predicted values (sorted by these)
#   case_weights: Weights (typically exposure)
#   na_rm: Remove NA values
#
# Returns:
#   Gini coefficient (0 to 1, higher is better)
# ----------------------------------------------------------------------------
gini_vec <- function(truth, estimate, case_weights = NULL, na_rm = TRUE, ...) {
  # Check for zero-length inputs
  if (length(truth) == 0) return(NA_real_)

  # Remove missing values
  if (na_rm) {
    keep <- !(is.na(truth) | is.na(estimate))
    truth <- truth[keep]
    estimate <- estimate[keep]
    if (!is.null(case_weights)) case_weights <- case_weights[keep]
  }

  # Check again after NA removal
  if (length(truth) == 0) return(NA_real_)

  # Default weights if not provided
  if (is.null(case_weights)) case_weights <- rep(1, length(truth))

  # Calculate Gini coefficient using DescTools
  # Gini measures concentration/inequality in the predictions
  DescTools::Gini(estimate, weights = case_weights)
}

gini_coef <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "gini",
    fn = gini_vec,
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    case_weights = {{ case_weights }}
  )
}

class(gini_coef) <- c("numeric_metric", "function")
attr(gini_coef, "direction") <- "maximize" # Higher Gini = better discrimination

# ----------------------------------------------------------------------------
# quantile_loss_vec: Quantile loss (pinball loss) - vectorized
# ----------------------------------------------------------------------------
# Quantile loss evaluates model performance at specific quantiles. This is
# important for insurance where tail risk (high quantiles) is critical.
# Lower values indicate better performance at that quantile.
#
# Arguments:
#   truth: Observed values
#   estimate: Predicted quantile values
#   case_weights: Weights (typically exposure)
#   na_rm: Remove NA values
#   tau: Quantile level (0.5 = median, 0.95 = 95th percentile)
#
# Returns:
#   Weighted quantile loss
# ----------------------------------------------------------------------------
quantile_loss_vec <- function(
  truth,
  estimate,
  case_weights = NULL,
  na_rm = TRUE,
  tau = 0.5,
  ...
) {
  # Check for zero-length inputs
  if (length(truth) == 0) return(NA_real_)

  # Remove missing values
  if (na_rm) {
    keep <- !(is.na(truth) | is.na(estimate))
    truth <- truth[keep]
    estimate <- estimate[keep]
    if (!is.null(case_weights)) case_weights <- case_weights[keep]
  }

  # Check again after NA removal
  if (length(truth) == 0) return(NA_real_)

  # Default weights if not provided
  if (is.null(case_weights)) case_weights <- rep(1, length(truth))

  # Calculate quantile loss (pinball loss)
  error <- truth - estimate
  loss <- ifelse(error >= 0, tau * error, (tau - 1) * error)

  # Return weighted average
  sum(loss * case_weights) / sum(case_weights)
}

quantile_loss <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  tau = 0.5,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = paste0("quantile_loss_", tau),
    fn = function(truth, estimate, case_weights = NULL, na_rm = TRUE, ...) {
      quantile_loss_vec(truth, estimate, case_weights, na_rm, tau = tau, ...)
    },
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    case_weights = {{ case_weights }}
  )
}

class(quantile_loss) <- c("numeric_metric", "function")
attr(quantile_loss, "direction") <- "minimize"

# ----------------------------------------------------------------------------
# quantile_loss_50: Quantile loss at 50th percentile (median)
# ----------------------------------------------------------------------------
# Specialized wrapper for median quantile loss.
# ----------------------------------------------------------------------------
quantile_loss_50 <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "quantile_loss_0.5",
    fn = function(truth, estimate, case_weights = NULL, na_rm = TRUE, ...) {
      quantile_loss_vec(truth, estimate, case_weights, na_rm, tau = 0.5, ...)
    },
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    case_weights = {{ case_weights }}
  )
}

class(quantile_loss_50) <- c("numeric_metric", "function")
attr(quantile_loss_50, "direction") <- "minimize"

# ----------------------------------------------------------------------------
# quantile_loss_75: Quantile loss at 75th percentile
# ----------------------------------------------------------------------------
# Specialized wrapper for 75th percentile quantile loss.
# ----------------------------------------------------------------------------
quantile_loss_75 <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "quantile_loss_0.75",
    fn = function(truth, estimate, case_weights = NULL, na_rm = TRUE, ...) {
      quantile_loss_vec(truth, estimate, case_weights, na_rm, tau = 0.75, ...)
    },
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    case_weights = {{ case_weights }}
  )
}

class(quantile_loss_75) <- c("numeric_metric", "function")
attr(quantile_loss_75, "direction") <- "minimize"

# ----------------------------------------------------------------------------
# quantile_loss_90: Quantile loss at 90th percentile
# ----------------------------------------------------------------------------
# Specialized wrapper for 90th percentile quantile loss.
# ----------------------------------------------------------------------------
quantile_loss_90 <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "quantile_loss_0.9",
    fn = function(truth, estimate, case_weights = NULL, na_rm = TRUE, ...) {
      quantile_loss_vec(truth, estimate, case_weights, na_rm, tau = 0.9, ...)
    },
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    case_weights = {{ case_weights }}
  )
}

class(quantile_loss_90) <- c("numeric_metric", "function")
attr(quantile_loss_90, "direction") <- "minimize"

# ----------------------------------------------------------------------------
# quantile_loss_95: Quantile loss at 95th percentile (tail risk)
# ----------------------------------------------------------------------------
# Specialized wrapper for 95th percentile quantile loss. Critical for
# evaluating tail risk prediction in insurance models.
# ----------------------------------------------------------------------------
quantile_loss_95 <- function(
  data,
  truth,
  estimate,
  na_rm = TRUE,
  case_weights = NULL,
  ...
) {
  yardstick::numeric_metric_summarizer(
    name = "quantile_loss_0.95",
    fn = function(truth, estimate, case_weights = NULL, na_rm = TRUE, ...) {
      quantile_loss_vec(truth, estimate, case_weights, na_rm, tau = 0.95, ...)
    },
    data = data,
    truth = {{ truth }},
    estimate = {{ estimate }},
    na_rm = na_rm,
    case_weights = {{ case_weights }}
  )
}

class(quantile_loss_95) <- c("numeric_metric", "function")
attr(quantile_loss_95, "direction") <- "minimize"
