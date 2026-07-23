# ============================================================================
# PILOT 1 — COMPLETE REPRODUCIBLE ANALYSIS
# Ratings, open-ended responses, statistical tests, tables, figures, and checks
# ============================================================================
#
# INPUT
#   pilot_1_data_.xlsx
#
# The workbook must contain one row per participant and olfactory cue
# (28 participants × 2 cues = 56 rows) in sheet "Foglio1".
#
# The script uses the LONG-FORMAT columns:
#   subject, session, cue, pleasantness, calories_rating, intensity,
#   sugar_rating, fat_rating, beverage_open_response,
#   ingredients_open_response, recode_0_1_2, recode_label,
#   analysis_status
#
# Any additional wide-format columns in the workbook are ignored.
#
# HOW TO USE
# 1. Place this script in the repository root or in a /scripts folder.
# 2. Place pilot_1_data_.xlsx in the repository root or in /data.
# 3. Start a fresh R session and run the entire script.
#
# The script:
#   - imports the single Pilot 1 workbook;
#   - verifies sample size and within-participant structure;
#   - reproduces all reported quantitative and qualitative analyses;
#   - generates only the supplementary tables needed for reporting;
#   - saves outputs in /outputs/Pilot1;
#   - saves sessionInfo();
#   - creates a final verification table;
#   - stops if any central reported result is not reproduced.
# ============================================================================


# ----------------------------------------------------------------------------
# 0. REPRODUCIBILITY AND PACKAGES
# ----------------------------------------------------------------------------

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  contrasts = c("contr.sum", "contr.poly")
)

set.seed(2026)

required_packages <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "patchwork",
  "tibble"
)

installed_packages <- rownames(installed.packages())
missing_packages <- setdiff(required_packages, installed_packages)

if (length(missing_packages) > 0) {
  install.packages(
    missing_packages,
    dependencies = TRUE
  )
}

invisible(
  lapply(
    required_packages,
    library,
    character.only = TRUE
  )
)


# ----------------------------------------------------------------------------
# 1. PROJECT PATHS
# ----------------------------------------------------------------------------

get_script_directory <- function() {

  command_args <- commandArgs(
    trailingOnly = FALSE
  )

  file_argument <- grep(
    "^--file=",
    command_args,
    value = TRUE
  )

  if (length(file_argument) > 0) {

    script_path <- sub(
      "^--file=",
      "",
      file_argument[1]
    )

    return(
      dirname(
        normalizePath(script_path)
      )
    )
  }

  if (
    requireNamespace(
      "rstudioapi",
      quietly = TRUE
    ) &&
    rstudioapi::isAvailable()
  ) {

    active_path <-
      rstudioapi::getSourceEditorContext()$path

    if (nzchar(active_path)) {

      return(
        dirname(
          normalizePath(active_path)
        )
      )
    }
  }

  normalizePath(
    getwd()
  )
}


script_directory <- get_script_directory()


project_root <- if (
  basename(script_directory) == "scripts"
) {
  dirname(script_directory)
} else {
  script_directory
}


resolve_input_file <- function(file_name) {

  candidate_paths <- unique(
    c(
      file.path(
        project_root,
        "data",
        file_name
      ),

      file.path(
        project_root,
        file_name
      ),

      file.path(
        getwd(),
        "data",
        file_name
      ),

      file.path(
        getwd(),
        file_name
      )
    )
  )

  existing_paths <-
    candidate_paths[
      file.exists(candidate_paths)
    ]

  if (length(existing_paths) == 0) {

    stop(
      paste0(
        "\nInput file not found: ",
        file_name,
        "\n\nExpected locations:\n",
        paste0(
          "  - ",
          candidate_paths,
          collapse = "\n"
        ),
        "\n\nPlace the workbook in the repository root or in /data."
      ),
      call. = FALSE
    )
  }

  normalizePath(
    existing_paths[1]
  )
}


input_file_name <- "pilot_1_data_.xlsx"
input_file_path <- resolve_input_file(
  input_file_name
)


output_root <- file.path(
  project_root,
  "outputs",
  "Pilot1"
)

tables_directory <- file.path(
  output_root,
  "tables"
)

figures_directory <- file.path(
  output_root,
  "figures"
)

logs_directory <- file.path(
  output_root,
  "logs"
)


dir.create(
  tables_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figures_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  logs_directory,
  recursive = TRUE,
  showWarnings = FALSE
)


cat("\n============================================================\n")
cat("PILOT 1 — COMPLETE REPRODUCIBLE ANALYSIS\n")
cat("============================================================\n")
cat("Project root:\n", project_root, "\n\n")
cat("Input workbook:\n", input_file_path, "\n\n")
cat("Output directory:\n", output_root, "\n")
cat("============================================================\n")


# ----------------------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ----------------------------------------------------------------------------

assert_true <- function(
  condition,
  label
) {

  if (!isTRUE(condition)) {

    stop(
      paste(
        "VERIFICATION FAILED —",
        label
      ),
      call. = FALSE
    )
  }

  message(
    "PASS — ",
    label
  )
}


assert_close <- function(
  actual,
  expected,
  tolerance,
  label
) {

  condition <-
    length(actual) == 1 &&
    !is.na(actual) &&
    abs(actual - expected) <= tolerance

  if (!condition) {

    stop(
      sprintf(
        paste0(
          "VERIFICATION FAILED — %s\n",
          "Observed: %.10f\n",
          "Expected: %.10f\n",
          "Tolerance: %.10f"
        ),
        label,
        actual,
        expected,
        tolerance
      ),
      call. = FALSE
    )
  }

  message(
    sprintf(
      "PASS — %s: %.6f",
      label,
      actual
    )
  )
}


safe_numeric <- function(x) {

  suppressWarnings(
    as.numeric(
      as.character(x)
    )
  )
}


format_p <- function(p_value) {

  ifelse(
    p_value < .001,
    "< .001",
    sub(
      "^0",
      "",
      sprintf(
        "%.3f",
        p_value
      )
    )
  )
}


significance_label <- function(p_value) {

  dplyr::case_when(
    p_value < .001 ~ "***",
    p_value < .01 ~ "**",
    p_value < .05 ~ "*",
    TRUE ~ "ns"
  )
}


write_table <- function(
  data,
  file_name,
  row_names = FALSE
) {

  write.csv(
    data,
    file.path(
      tables_directory,
      file_name
    ),
    row.names = row_names,
    na = ""
  )
}


# ============================================================================
# PART A — IMPORT, CLEANING, AND STRUCTURAL CHECKS
# ============================================================================

# ----------------------------------------------------------------------------
# 3. IMPORT THE SINGLE WORKBOOK
# ----------------------------------------------------------------------------

raw_data <- readxl::read_excel(
  path = input_file_path,
  sheet = "Foglio1"
)


required_columns <- c(
  "subject",
  "session",
  "cue",
  "pleasantness",
  "calories_rating",
  "intensity",
  "sugar_rating",
  "fat_rating",
  "beverage_open_response",
  "ingredients_open_response",
  "recode_0_1_2",
  "recode_label",
  "analysis_status"
)


missing_columns <- setdiff(
  required_columns,
  names(raw_data)
)


if (length(missing_columns) > 0) {

  stop(
    paste0(
      "Missing required columns in ",
      input_file_name,
      ":\n",
      paste0(
        "  - ",
        missing_columns,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# Only the canonical long-format columns are retained.
# Any redundant wide-format columns in the workbook are ignored.

pilot1_data <- raw_data %>%
  transmute(

    subject = as.character(
      subject
    ),

    session = as.character(
      session
    ),

    cue = as.character(
      cue
    ),

    pleasantness = safe_numeric(
      pleasantness
    ),

    calories = safe_numeric(
      calories_rating
    ),

    intensity = safe_numeric(
      intensity
    ),

    sugar = safe_numeric(
      sugar_rating
    ),

    fat = safe_numeric(
      fat_rating
    ),

    beverage_open_response = as.character(
      beverage_open_response
    ),

    ingredients_open_response = as.character(
      ingredients_open_response
    ),

    open_response_code = suppressWarnings(
      as.integer(
        as.character(
          recode_0_1_2
        )
      )
    ),

    open_response_label = as.character(
      recode_label
    ),

    analysis_status = as.character(
      analysis_status
    )
  ) %>%
  mutate(

    cue = factor(
      cue,
      levels = c(
        "Peach smoothie",
        "Chocolate-orange"
      )
    ),

    open_response_category = factor(
      open_response_code,
      levels = c(
        0,
        1,
        2
      ),
      labels = c(
        "Plain water",
        "Flavoured/infused or low-calorie",
        "Caloric/nutrient-related"
      )
    )
  )


# ----------------------------------------------------------------------------
# 4. SAMPLE AND WITHIN-PARTICIPANT STRUCTURE
# ----------------------------------------------------------------------------

assert_true(
  nrow(pilot1_data) == 56,
  "The workbook contains 56 participant × cue rows"
)


assert_true(
  dplyr::n_distinct(
    pilot1_data$subject
  ) == 28,
  "The workbook contains 28 unique participants"
)


assert_true(
  all(
    !is.na(
      pilot1_data$cue
    )
  ),
  "All rows have a recognised olfactory cue"
)


participant_cue_check <- pilot1_data %>%
  count(
    subject,
    cue,
    name = "n_rows"
  ) %>%
  tidyr::complete(
    subject,
    cue,
    fill = list(
      n_rows = 0
    )
  )


assert_true(
  nrow(participant_cue_check) == 28 * 2,
  "All participant × cue combinations are represented"
)


assert_true(
  all(
    participant_cue_check$n_rows == 1
  ),
  "Each participant has exactly one row for each cue"
)


rating_columns <- c(
  "pleasantness",
  "calories",
  "intensity",
  "sugar",
  "fat"
)


all_rating_values <- unlist(
  pilot1_data[
    rating_columns
  ],
  use.names = FALSE
)


assert_true(
  all(
    is.na(
      all_rating_values
    ) |
      (
        all_rating_values >= 0 &
        all_rating_values <= 100
      )
  ),
  "All non-missing ratings fall within the 0–100 scale"
)


missing_rating_summary <- pilot1_data %>%
  summarise(
    across(
      all_of(
        rating_columns
      ),
      ~ sum(
        is.na(.x)
      )
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "measure",
    values_to = "n_missing"
  )


# ============================================================================
# PART B — PERCEPTUAL AND NUTRITIONAL RATINGS
# ============================================================================

# ----------------------------------------------------------------------------
# 5. RESHAPE RATINGS INTO LONG FORMAT
# ----------------------------------------------------------------------------

ratings_long <- pilot1_data %>%
  select(
    subject,
    cue,
    pleasantness,
    calories,
    intensity,
    sugar,
    fat
  ) %>%
  pivot_longer(
    cols = c(
      pleasantness,
      calories,
      intensity,
      sugar,
      fat
    ),
    names_to = "measure_raw",
    values_to = "rating"
  ) %>%
  mutate(

    measure = case_when(
      measure_raw == "intensity" ~ "Intensity",
      measure_raw == "pleasantness" ~ "Pleasantness",
      measure_raw == "calories" ~ "Calories",
      measure_raw == "sugar" ~ "Sugar",
      measure_raw == "fat" ~ "Fat",
      TRUE ~ NA_character_
    ),

    measure = factor(
      measure,
      levels = c(
        "Intensity",
        "Pleasantness",
        "Calories",
        "Sugar",
        "Fat"
      )
    ),

    xpos = if_else(
      cue == "Peach smoothie",
      1,
      2
    )
  )


rating_structure_check <- ratings_long %>%
  count(
    subject,
    cue,
    measure,
    name = "n_rows"
  )


assert_true(
  nrow(
    rating_structure_check
  ) == 28 * 2 * 5,
  "Ratings contain every participant × cue × measure combination"
)


assert_true(
  all(
    rating_structure_check$n_rows == 1
  ),
  "No participant × cue × measure combination is duplicated"
)


ratings_analysis_data <- ratings_long %>%
  filter(
    !is.na(
      rating
    )
  )


perceptual_data <- ratings_analysis_data %>%
  filter(
    measure %in% c(
      "Intensity",
      "Pleasantness"
    )
  ) %>%
  droplevels()


nutritional_data <- ratings_analysis_data %>%
  filter(
    measure %in% c(
      "Calories",
      "Sugar",
      "Fat"
    )
  ) %>%
  droplevels()


# ----------------------------------------------------------------------------
# 6. RATING DESCRIPTIVE STATISTICS
# ----------------------------------------------------------------------------

rating_descriptives <- ratings_analysis_data %>%
  group_by(
    measure,
    cue
  ) %>%
  summarise(

    n = sum(
      !is.na(
        rating
      )
    ),

    mean = mean(
      rating,
      na.rm = TRUE
    ),

    sd = sd(
      rating,
      na.rm = TRUE
    ),

    median = median(
      rating,
      na.rm = TRUE
    ),

    q1 = unname(
      quantile(
        rating,
        .25,
        na.rm = TRUE
      )
    ),

    q3 = unname(
      quantile(
        rating,
        .75,
        na.rm = TRUE
      )
    ),

    minimum = min(
      rating,
      na.rm = TRUE
    ),

    maximum = max(
      rating,
      na.rm = TRUE
    ),

    .groups = "drop"
  )


write_table(
  rating_descriptives,
  "Table_S1_Pilot1_descriptives.csv"
)


# ----------------------------------------------------------------------------
# 7. PAIRED WILCOXON TESTS:
#    PEACH SMOOTHIE VS CHOCOLATE-ORANGE
# ----------------------------------------------------------------------------

ratings_paired_wide <- ratings_analysis_data %>%
  select(
    subject,
    measure,
    cue,
    rating
  ) %>%
  pivot_wider(
    names_from = cue,
    values_from = rating
  )


paired_tests <- ratings_paired_wide %>%
  group_by(
    measure
  ) %>%
  group_modify(
    ~ {

      complete_pairs <- complete.cases(
        .x$`Peach smoothie`,
        .x$`Chocolate-orange`
      )

      test_result <- wilcox.test(
        .x$`Peach smoothie`[
          complete_pairs
        ],
        .x$`Chocolate-orange`[
          complete_pairs
        ],
        paired = TRUE,
        exact = FALSE,
        alternative = "two.sided"
      )

      tibble(
        n_pairs = sum(
          complete_pairs
        ),
        V = unname(
          test_result$statistic
        ),
        p_raw = test_result$p.value
      )
    }
  ) %>%
  ungroup() %>%
  mutate(

    p_holm = p.adjust(
      p_raw,
      method = "holm"
    ),

    significance = significance_label(
      p_holm
    ),

    p_text = if_else(
      p_holm < .001,
      "pHolm < .001",
      paste0(
        "pHolm = ",
        format_p(
          p_holm
        )
      )
    ),

    plot_label = paste0(
      significance,
      " (",
      p_text,
      ")"
    )
  )


cat("\n============================================================\n")
cat("PAIRED WILCOXON TESTS: PEACH VS CHOCOLATE-ORANGE\n")
cat("============================================================\n")

print(
  paired_tests
)


write_table(
  paired_tests,
  "Table_S2_Pilot1_paired_comparisons.csv"
)


# ----------------------------------------------------------------------------
# 8. ONE-SAMPLE WILCOXON TESTS AGAINST ZERO
# ----------------------------------------------------------------------------

one_sample_tests <- nutritional_data %>%
  group_by(
    measure,
    cue
  ) %>%
  group_modify(
    ~ {

      observed <- .x$rating[
        !is.na(
          .x$rating
        )
      ]

      test_result <- wilcox.test(
        observed,
        mu = 0,
        exact = FALSE,
        alternative = "two.sided"
      )

      tibble(
        n = length(
          observed
        ),
        V = unname(
          test_result$statistic
        ),
        p_raw = test_result$p.value
      )
    }
  ) %>%
  ungroup() %>%
  mutate(

    p_holm = p.adjust(
      p_raw,
      method = "holm"
    ),

    significance = significance_label(
      p_holm
    ),

    xpos = if_else(
      cue == "Peach smoothie",
      1,
      2
    ),

    y_star = 102
  )


cat("\n============================================================\n")
cat("ONE-SAMPLE WILCOXON TESTS AGAINST ZERO\n")
cat("============================================================\n")

print(
  one_sample_tests
)


write_table(
  one_sample_tests,
  "Table_S3_Pilot1_one_sample_tests.csv"
)


# ----------------------------------------------------------------------------
# 9. COMBINED RATINGS FIGURE
# ----------------------------------------------------------------------------

annotations_A <- paired_tests %>%
  filter(
    measure %in% c(
      "Intensity",
      "Pleasantness"
    )
  ) %>%
  mutate(
    x_start = 1,
    x_end = 2,
    y_line = 105,
    y_tick = 102,
    y_text = 110
  )


annotations_B <- paired_tests %>%
  filter(
    measure %in% c(
      "Calories",
      "Sugar",
      "Fat"
    )
  ) %>%
  mutate(
    x_start = 1,
    x_end = 2,
    y_line = 105,
    y_tick = 102,
    y_text = 110
  )


annotations_C_text <- one_sample_tests %>%
  group_by(
    measure
  ) %>%
  summarise(
    all_below_001 = all(
      p_holm < .001
    ),
    .groups = "drop"
  ) %>%
  mutate(

    xpos = 1.5,

    ypos = 112,

    label = if_else(
      all_below_001,
      "Both pods > 0: pHolm < .001",
      "One-sample Wilcoxon vs 0"
    )
  )


pod_colors <- c(
  "Peach smoothie" = "#F2B544",
  "Chocolate-orange" = "#A9643A"
)


jitter_position <- position_jitter(
  width = 0.035,
  height = 0,
  seed = 2026
)


common_theme <- theme_classic(
  base_size = 13
) +
  theme(

    plot.title = element_text(
      face = "bold",
      size = 15,
      hjust = 0
    ),

    strip.background = element_blank(),

    strip.text = element_text(
      face = "bold",
      size = 13
    ),

    axis.title.y = element_text(
      size = 12
    ),

    axis.text.y = element_text(
      size = 10
    ),

    panel.spacing = grid::unit(
      1.2,
      "lines"
    ),

    plot.margin = margin(
      t = 10,
      r = 15,
      b = 10,
      l = 10
    )
  )


pA <- ggplot(
  perceptual_data,
  aes(
    x = xpos,
    y = rating
  )
) +

  geom_violin(
    aes(
      group = cue,
      fill = cue
    ),
    width = 0.72,
    trim = TRUE,
    alpha = 0.28,
    color = NA
  ) +

  geom_line(
    aes(
      group = subject
    ),
    color = "grey70",
    linewidth = 0.35,
    alpha = 0.70
  ) +

  geom_boxplot(
    aes(
      group = cue,
      fill = cue
    ),
    width = 0.14,
    alpha = 0.48,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.45
  ) +

  geom_point(
    aes(
      color = cue
    ),
    position = jitter_position,
    size = 1.9,
    alpha = 0.85
  ) +

  stat_summary(
    aes(
      group = cue
    ),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3.5,
    color = "black"
  ) +

  geom_segment(
    data = annotations_A,
    aes(
      x = x_start,
      xend = x_end,
      y = y_line,
      yend = y_line
    ),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +

  geom_segment(
    data = annotations_A,
    aes(
      x = x_start,
      xend = x_start,
      y = y_tick,
      yend = y_line
    ),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +

  geom_segment(
    data = annotations_A,
    aes(
      x = x_end,
      xend = x_end,
      y = y_tick,
      yend = y_line
    ),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +

  geom_text(
    data = annotations_A,
    aes(
      x = 1.5,
      y = y_text,
      label = plot_label
    ),
    inherit.aes = FALSE,
    size = 3.8
  ) +

  facet_wrap(
    ~ measure,
    nrow = 1
  ) +

  scale_x_continuous(
    breaks = c(
      1,
      2
    ),
    labels = c(
      "Peach smoothie",
      "Chocolate-orange"
    ),
    limits = c(
      0.55,
      2.45
    )
  ) +

  scale_y_continuous(
    breaks = seq(
      0,
      100,
      25
    )
  ) +

  scale_fill_manual(
    values = pod_colors
  ) +

  scale_color_manual(
    values = pod_colors
  ) +

  coord_cartesian(
    ylim = c(
      0,
      116
    ),
    clip = "off"
  ) +

  labs(
    title = "Perceptual ratings did not differ between pods",
    x = NULL,
    y = "VAS rating (0–100)"
  ) +

  guides(
    fill = "none",
    color = "none"
  ) +

  common_theme +

  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


pB <- ggplot(
  nutritional_data,
  aes(
    x = xpos,
    y = rating
  )
) +

  geom_violin(
    aes(
      group = cue,
      fill = cue
    ),
    width = 0.72,
    trim = TRUE,
    alpha = 0.28,
    color = NA
  ) +

  geom_line(
    aes(
      group = subject
    ),
    color = "grey70",
    linewidth = 0.35,
    alpha = 0.70
  ) +

  geom_boxplot(
    aes(
      group = cue,
      fill = cue
    ),
    width = 0.14,
    alpha = 0.48,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.45
  ) +

  geom_point(
    aes(
      color = cue
    ),
    position = jitter_position,
    size = 1.9,
    alpha = 0.85
  ) +

  stat_summary(
    aes(
      group = cue
    ),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3.5,
    color = "black"
  ) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.4,
    color = "grey45"
  ) +

  geom_segment(
    data = annotations_B,
    aes(
      x = x_start,
      xend = x_end,
      y = y_line,
      yend = y_line
    ),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +

  geom_segment(
    data = annotations_B,
    aes(
      x = x_start,
      xend = x_start,
      y = y_tick,
      yend = y_line
    ),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +

  geom_segment(
    data = annotations_B,
    aes(
      x = x_end,
      xend = x_end,
      y = y_tick,
      yend = y_line
    ),
    inherit.aes = FALSE,
    linewidth = 0.5
  ) +

  geom_text(
    data = annotations_B,
    aes(
      x = 1.5,
      y = y_text,
      label = plot_label
    ),
    inherit.aes = FALSE,
    size = 3.8
  ) +

  facet_wrap(
    ~ measure,
    nrow = 1
  ) +

  scale_x_continuous(
    breaks = c(
      1,
      2
    ),
    labels = c(
      "Peach smoothie",
      "Chocolate-orange"
    ),
    limits = c(
      0.55,
      2.45
    )
  ) +

  scale_y_continuous(
    breaks = seq(
      0,
      100,
      25
    )
  ) +

  scale_fill_manual(
    values = pod_colors
  ) +

  scale_color_manual(
    values = pod_colors
  ) +

  coord_cartesian(
    ylim = c(
      0,
      116
    ),
    clip = "off"
  ) +

  labs(
    title = "Nutritional ratings were comparable across pods",
    x = NULL,
    y = "VAS rating (0–100)"
  ) +

  guides(
    fill = "none",
    color = "none"
  ) +

  common_theme +

  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )


pC <- ggplot(
  nutritional_data,
  aes(
    x = xpos,
    y = rating
  )
) +

  geom_violin(
    aes(
      group = cue,
      fill = cue
    ),
    width = 0.72,
    trim = TRUE,
    alpha = 0.28,
    color = NA
  ) +

  geom_boxplot(
    aes(
      group = cue,
      fill = cue
    ),
    width = 0.14,
    alpha = 0.48,
    outlier.shape = NA,
    color = "black",
    linewidth = 0.45
  ) +

  geom_point(
    aes(
      color = cue
    ),
    position = jitter_position,
    size = 1.9,
    alpha = 0.85
  ) +

  stat_summary(
    aes(
      group = cue
    ),
    fun = mean,
    geom = "point",
    shape = 18,
    size = 3.5,
    color = "black"
  ) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.5,
    color = "grey40"
  ) +

  geom_text(
    data = one_sample_tests,
    aes(
      x = xpos,
      y = y_star,
      label = significance
    ),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 5
  ) +

  geom_text(
    data = annotations_C_text,
    aes(
      x = xpos,
      y = ypos,
      label = label
    ),
    inherit.aes = FALSE,
    size = 3.7
  ) +

  facet_wrap(
    ~ measure,
    nrow = 1
  ) +

  scale_x_continuous(
    breaks = c(
      1,
      2
    ),
    labels = c(
      "Peach smoothie",
      "Chocolate-orange"
    ),
    limits = c(
      0.55,
      2.45
    )
  ) +

  scale_y_continuous(
    breaks = seq(
      0,
      100,
      25
    )
  ) +

  scale_fill_manual(
    values = pod_colors
  ) +

  scale_color_manual(
    values = pod_colors
  ) +

  coord_cartesian(
    ylim = c(
      0,
      116
    ),
    clip = "off"
  ) +

  labs(
    title = "Nutritional ratings were reliably above zero",
    x = NULL,
    y = "VAS rating (0–100)"
  ) +

  guides(
    fill = "none",
    color = "none"
  ) +

  common_theme +

  theme(
    axis.text.x = element_text(
      size = 10,
      face = "bold"
    )
  )


combined_plot <- (
  pA /
    pB /
    pC
) +

  plot_layout(
    heights = c(
      0.95,
      1.10,
      1.10
    )
  ) +

  plot_annotation(

    title =
      "Pilot 1: Both pods elicited comparable nutritional associations",

    subtitle = paste(
      "Paired comparisons between pods and",
      "one-sample Wilcoxon tests against zero"
    ),

    caption = paste(
      "*** pHolm < .001;",
      "ns = non-significant after Holm correction"
    ),

    tag_levels = "A",

    theme = theme(

      plot.title = element_text(
        face = "bold",
        size = 19
      ),

      plot.subtitle = element_text(
        size = 12
      ),

      plot.caption = element_text(
        size = 10,
        hjust = 0
      ),

      plot.tag = element_text(
        face = "bold",
        size = 17
      )
    )
  )


print(
  combined_plot
)


ggsave(
  filename = file.path(
    figures_directory,
    "Pilot1_combined_panels.png"
  ),
  plot = combined_plot,
  width = 15,
  height = 17,
  units = "in",
  dpi = 300,
  bg = "white"
)


ggsave(
  filename = file.path(
    figures_directory,
    "Pilot1_combined_panels.pdf"
  ),
  plot = combined_plot,
  width = 15,
  height = 17,
  units = "in",
  bg = "white"
)


# ============================================================================
# PART C — OPEN-ENDED RESPONSES
# ============================================================================

# ----------------------------------------------------------------------------
# 10. DESCRIPTIVE OPEN-RESPONSE RESULTS
# ----------------------------------------------------------------------------

open_data <- pilot1_data %>%
  select(
    subject,
    cue,
    beverage_open_response,
    ingredients_open_response,
    open_response_code,
    open_response_category,
    open_response_label,
    analysis_status
  )


n_participants_open <- dplyr::n_distinct(
  open_data$subject
)


n_total_responses <- nrow(
  open_data
)


classifiable_data <- open_data %>%
  filter(
    open_response_code %in% c(
      0,
      1,
      2
    )
  )


n_classifiable <- nrow(
  classifiable_data
)


n_non_classifiable <-
  n_total_responses -
  n_classifiable


overall_categories <- classifiable_data %>%
  count(
    open_response_code,
    open_response_category,
    name = "n"
  ) %>%
  complete(
    open_response_code = 0:2,
    fill = list(
      n = 0
    )
  ) %>%
  mutate(

    open_response_category = factor(
      open_response_code,
      levels = c(
        0,
        1,
        2
      ),
      labels = c(
        "Plain water",
        "Flavoured/infused or low-calorie",
        "Caloric/nutrient-related"
      )
    ),

    denominator = n_classifiable,

    percentage =
      100 *
      n /
      denominator
  )


n_plain_water <- sum(
  classifiable_data$open_response_code == 0
)


n_beverage_like <- sum(
  classifiable_data$open_response_code %in% c(
    1,
    2
  )
)


n_caloric <- sum(
  classifiable_data$open_response_code == 2
)


overall_summary <- tibble(

  outcome = c(
    "Plain water",
    "Beverage-like identity",
    "Caloric/nutrient-related identity"
  ),

  n = c(
    n_plain_water,
    n_beverage_like,
    n_caloric
  ),

  denominator = n_classifiable,

  percentage =
    100 *
    n /
    denominator
)


cue_summary <- classifiable_data %>%
  count(
    cue,
    open_response_code,
    open_response_category,
    name = "n"
  ) %>%
  complete(
    cue,
    open_response_code = 0:2,
    fill = list(
      n = 0
    )
  ) %>%
  group_by(
    cue
  ) %>%
  mutate(

    denominator = sum(
      n
    ),

    percentage =
      100 *
      n /
      denominator,

    open_response_category = factor(
      open_response_code,
      levels = c(
        0,
        1,
        2
      ),
      labels = c(
        "Plain water",
        "Flavoured/infused or low-calorie",
        "Caloric/nutrient-related"
      )
    )
  ) %>%
  ungroup()


cat("\n============================================================\n")
cat("OPEN-ENDED RESPONSES: OVERALL SUMMARY\n")
cat("============================================================\n")

print(
  overall_summary
)


cat("\n============================================================\n")
cat("OPEN-ENDED RESPONSES: BY CUE\n")
cat("============================================================\n")

print(
  cue_summary
)


# One concise supplementary table contains both the across-cue and by-cue
# distributions, avoiding multiple overlapping descriptive output files.

open_response_table <- bind_rows(

  overall_categories %>%
    transmute(
      scope = "Across cues",
      cue = "Across cues",
      category = as.character(
        open_response_category
      ),
      n,
      denominator,
      percentage
    ),

  cue_summary %>%
    transmute(
      scope = "By cue",
      cue = as.character(
        cue
      ),
      category = as.character(
        open_response_category
      ),
      n,
      denominator,
      percentage
    )
)


write_table(
  open_response_table,
  "Table_S4_Pilot1_open_responses.csv"
)


# ----------------------------------------------------------------------------
# 11. PAIRED OPEN-RESPONSE DATA
# ----------------------------------------------------------------------------

open_paired_data <- open_data %>%
  select(
    subject,
    cue,
    open_response_code
  ) %>%
  pivot_wider(
    names_from = cue,
    values_from = open_response_code
  )


open_paired_complete <- open_paired_data %>%
  filter(
    `Peach smoothie` %in% c(
      0,
      1,
      2
    ),
    `Chocolate-orange` %in% c(
      0,
      1,
      2
    )
  )


open_paired_excluded <- open_paired_data %>%
  filter(
    !(
      `Peach smoothie` %in% c(
        0,
        1,
        2
      ) &
        `Chocolate-orange` %in% c(
          0,
          1,
          2
        )
    )
  )


transition_matrix <- table(

  Peach = factor(
    open_paired_complete$`Peach smoothie`,
    levels = c(
      0,
      1,
      2
    )
  ),

  Chocolate = factor(
    open_paired_complete$`Chocolate-orange`,
    levels = c(
      0,
      1,
      2
    )
  )
)


# ----------------------------------------------------------------------------
# 12. BOWKER TEST OF SYMMETRY
# ----------------------------------------------------------------------------

bowker_test <- function(
  square_table
) {

  if (
    nrow(square_table) !=
      ncol(square_table)
  ) {

    stop(
      "Bowker's test requires a square table.",
      call. = FALSE
    )
  }


  statistic <- 0
  degrees_freedom <- 0
  component_results <- list()
  component_index <- 1


  for (
    i in 1:(
      nrow(square_table) - 1
    )
  ) {

    for (
      j in (
        i + 1
      ):ncol(square_table)
    ) {

      n_ij <- square_table[
        i,
        j
      ]

      n_ji <- square_table[
        j,
        i
      ]

      denominator <-
        n_ij +
        n_ji


      if (denominator > 0) {

        component <-
          (
            n_ij -
              n_ji
          )^2 /
          denominator


        statistic <-
          statistic +
          component


        degrees_freedom <-
          degrees_freedom +
          1


        component_results[[component_index]] <- data.frame(

          category_i = i - 1,

          category_j = j - 1,

          n_i_to_j = as.numeric(
            n_ij
          ),

          n_j_to_i = as.numeric(
            n_ji
          ),

          component = as.numeric(
            component
          )
        )


        component_index <-
          component_index +
          1
      }
    }
  }


  p_value <- pchisq(
    statistic,
    df = degrees_freedom,
    lower.tail = FALSE
  )


  list(

    statistic = as.numeric(
      statistic
    ),

    df = degrees_freedom,

    p_value = as.numeric(
      p_value
    ),

    components = bind_rows(
      component_results
    )
  )
}


bowker_result <- bowker_test(
  transition_matrix
)


bowker_summary <- tibble(

  test =
    "Bowker test of symmetry",

  chi_square =
    bowker_result$statistic,

  df =
    bowker_result$df,

  p =
    bowker_result$p_value,

  n_complete_pairs =
    nrow(
      open_paired_complete
    )
)


cat("\n============================================================\n")
cat("BOWKER TEST OF SYMMETRY\n")
cat("============================================================\n")

print(
  bowker_summary
)


# ----------------------------------------------------------------------------
# 13. EXACT McNEMAR TEST
#     0 = non-caloric: original codes 0 or 1
#     1 = caloric/nutrient-related: original code 2
# ----------------------------------------------------------------------------

open_paired_binary <- open_paired_complete %>%
  transmute(

    subject,

    peach_binary = if_else(
      `Peach smoothie` == 2,
      1L,
      0L
    ),

    chocolate_binary = if_else(
      `Chocolate-orange` == 2,
      1L,
      0L
    )
  )


binary_transition_matrix <- table(

  Peach = factor(
    open_paired_binary$peach_binary,
    levels = c(
      0,
      1
    )
  ),

  Chocolate = factor(
    open_paired_binary$chocolate_binary,
    levels = c(
      0,
      1
    )
  )
)


noncaloric_to_caloric <- as.numeric(
  binary_transition_matrix[
    "0",
    "1"
  ]
)


caloric_to_noncaloric <- as.numeric(
  binary_transition_matrix[
    "1",
    "0"
  ]
)


mcnemar_exact <- binom.test(

  x = min(
    noncaloric_to_caloric,
    caloric_to_noncaloric
  ),

  n =
    noncaloric_to_caloric +
    caloric_to_noncaloric,

  p = .5,

  alternative = "two.sided"
)


mcnemar_summary <- tibble(

  test =
    "Exact McNemar test",

  noncaloric_to_caloric =
    noncaloric_to_caloric,

  caloric_to_noncaloric =
    caloric_to_noncaloric,

  discordant_pairs =
    noncaloric_to_caloric +
    caloric_to_noncaloric,

  exact_p =
    mcnemar_exact$p.value
)


cat("\n============================================================\n")
cat("EXACT McNEMAR TEST\n")
cat("============================================================\n")

print(
  mcnemar_summary
)


# A single categorical-results table combines the transition matrices and the
# two paired categorical tests. This avoids separate files for each component.

three_category_records <- as.data.frame(
  transition_matrix
) %>%
  transmute(
    record_type = "Transition matrix",
    analysis = "Three-category coding",
    row_category = as.character(
      Peach
    ),
    column_category = as.character(
      Chocolate
    ),
    n = as.numeric(
      Freq
    ),
    statistic = NA_real_,
    df = NA_real_,
    p = NA_real_,
    note = "Rows: Peach smoothie; columns: Chocolate-orange; 0=plain water, 1=flavoured/low-calorie, 2=caloric/nutrient-related"
  )


binary_records <- as.data.frame(
  binary_transition_matrix
) %>%
  transmute(
    record_type = "Transition matrix",
    analysis = "Binary caloric coding",
    row_category = as.character(
      Peach
    ),
    column_category = as.character(
      Chocolate
    ),
    n = as.numeric(
      Freq
    ),
    statistic = NA_real_,
    df = NA_real_,
    p = NA_real_,
    note = "Rows: Peach smoothie; columns: Chocolate-orange; 0=non-caloric, 1=caloric/nutrient-related"
  )


categorical_tests_table <- bind_rows(

  three_category_records,

  binary_records,

  tibble(
    record_type = "Statistical test",
    analysis = "Bowker test of symmetry",
    row_category = NA_character_,
    column_category = NA_character_,
    n = nrow(
      open_paired_complete
    ),
    statistic = bowker_result$statistic,
    df = as.numeric(
      bowker_result$df
    ),
    p = bowker_result$p_value,
    note = "Complete paired responses"
  ),

  tibble(
    record_type = "Statistical test",
    analysis = "Exact McNemar test",
    row_category = NA_character_,
    column_category = NA_character_,
    n =
      noncaloric_to_caloric +
      caloric_to_noncaloric,
    statistic = NA_real_,
    df = NA_real_,
    p = mcnemar_exact$p.value,
    note = paste0(
      "Non-caloric to caloric = ",
      noncaloric_to_caloric,
      "; caloric to non-caloric = ",
      caloric_to_noncaloric
    )
  )
)


write_table(
  categorical_tests_table,
  "Table_S5_Pilot1_categorical_results.csv"
)


# ----------------------------------------------------------------------------
# 14. OPEN-ENDED RESPONSE FIGURE
# ----------------------------------------------------------------------------

category_levels <- c(
  "Plain water",
  "Flavoured/infused or low-calorie",
  "Caloric/nutrient-related"
)


open_plot_data <- bind_rows(

  classifiable_data %>%
    transmute(
      group = as.character(
        cue
      ),
      category = as.character(
        open_response_category
      )
    ),

  classifiable_data %>%
    transmute(
      group = "Across cues",
      category = as.character(
        open_response_category
      )
    )
) %>%

  count(
    group,
    category,
    name = "n"
  ) %>%

  complete(

    group = c(
      "Peach smoothie",
      "Chocolate-orange",
      "Across cues"
    ),

    category = category_levels,

    fill = list(
      n = 0
    )
  ) %>%

  group_by(
    group
  ) %>%

  mutate(

    denominator = sum(
      n
    ),

    percentage =
      100 *
      n /
      denominator
  ) %>%

  ungroup() %>%

  mutate(

    group = factor(
      group,
      levels = c(
        "Peach smoothie",
        "Chocolate-orange",
        "Across cues"
      )
    ),

    category = factor(
      category,
      levels = category_levels
    )
  )


response_colors <- c(
  "Plain water" = "#F8766D",
  "Flavoured/infused or low-calorie" = "#00BA38",
  "Caloric/nutrient-related" = "#619CFF"
)


open_response_plot <- ggplot(

  open_plot_data,

  aes(
    x = group,
    y = percentage,
    fill = category
  )
) +

  geom_col(
    width = 0.67,
    color = "white",
    linewidth = 0.7
  ) +

  geom_text(

    aes(
      label = if_else(
        n > 0,
        paste0(
          n,
          "\n(",
          sprintf(
            "%.1f",
            percentage
          ),
          "%)"
        ),
        ""
      )
    ),

    position = position_stack(
      vjust = 0.5
    ),

    size = 4.3,

    lineheight = 1.05
  ) +

  annotate(
    geom = "text",
    x = 3,
    y = 106,
    label =
      "Beverage-like: 48/52 (92.3%)",
    fontface = "bold",
    size = 4.2
  ) +

  scale_fill_manual(
    values = response_colors,
    breaks = category_levels
  ) +

  scale_y_continuous(
    breaks = seq(
      0,
      100,
      20
    ),
    expand = expansion(
      mult = c(
        0,
        0
      )
    )
  ) +

  coord_cartesian(
    ylim = c(
      0,
      110
    ),
    clip = "off"
  ) +

  labs(

    title =
      "Pilot 1: Open-ended beverage identity",

    subtitle = paste(
      "Responses by olfactory cue and aggregated across cues;",
      "N/A responses were excluded"
    ),

    x = NULL,

    y =
      "Classifiable responses (%)",

    fill =
      "Response category",

    caption = paste0(

      "Across cues: plain water = 4/52 (7.7%); ",

      "beverage-like identity = 48/52 (92.3%); ",

      "caloric/nutrient-related = 22/52 (42.3%).\n",

      "Paired analyses: Bowker chi-square(3) = 4.67, p = .198; ",

      "exact McNemar p = .125."
    )
  ) +

  theme_classic(
    base_size = 14
  ) +

  theme(

    plot.title = element_text(
      face = "bold",
      size = 18
    ),

    plot.subtitle = element_text(
      size = 12,
      margin = margin(
        b = 14
      )
    ),

    plot.caption = element_text(
      hjust = 0,
      size = 10.5,
      lineheight = 1.15,
      margin = margin(
        t = 12
      )
    ),

    axis.title.y = element_text(
      size = 13
    ),

    axis.text.x = element_text(
      face = "bold",
      size = 12
    ),

    axis.text.y = element_text(
      size = 11
    ),

    legend.position = "bottom",

    legend.title = element_text(
      size = 11
    ),

    legend.text = element_text(
      size = 10.5
    ),

    plot.margin = margin(
      t = 18,
      r = 20,
      b = 10,
      l = 10
    )
  )


print(
  open_response_plot
)


ggsave(
  filename = file.path(
    figures_directory,
    "Pilot1_open_responses_with_across_cues.png"
  ),
  plot = open_response_plot,
  width = 11,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


ggsave(
  filename = file.path(
    figures_directory,
    "Pilot1_open_responses_with_across_cues.pdf"
  ),
  plot = open_response_plot,
  width = 11,
  height = 8,
  units = "in",
  bg = "white"
)


# ============================================================================
# PART D — FINAL VERIFICATION AND REPRODUCIBILITY OUTPUTS
# ============================================================================

# ----------------------------------------------------------------------------
# 15. FINAL VERIFICATION TABLE
# ----------------------------------------------------------------------------

verification_summary <- tibble(

  claim = c(

    "Participant × cue rows",

    "Unique participants",

    "Each participant has exactly two cue rows",

    "No paired pod comparison survives Holm correction",

    "All six nutritional ratings are above zero after Holm correction",

    "Total open responses",

    "Classifiable open responses",

    "Non-classifiable open responses",

    "Plain-water responses",

    "Beverage-like responses",

    "Caloric/nutrient-related responses",

    "Beverage-like percentage",

    "Complete pairs for categorical analyses",

    "Bowker chi-square",

    "Bowker degrees of freedom",

    "Bowker p value",

    "Non-caloric to caloric transitions",

    "Caloric to non-caloric transitions",

    "Exact McNemar p value"
  ),


  expected = c(

    "56",

    "28",

    "2 per participant",

    "All pHolm >= .05",

    "All pHolm < .001",

    "56",

    "52",

    "4",

    "4",

    "48",

    "22",

    "92.3%",

    "24",

    "4.6667",

    "3",

    ".197897",

    "6",

    "1",

    ".125"
  ),


  observed = c(

    as.character(
      nrow(
        pilot1_data
      )
    ),

    as.character(
      dplyr::n_distinct(
        pilot1_data$subject
      )
    ),

    paste(
      range(
        table(
          pilot1_data$subject
        )
      ),
      collapse = "–"
    ),

    paste0(
      "Minimum pHolm = ",
      sprintf(
        "%.6f",
        min(
          paired_tests$p_holm
        )
      )
    ),

    paste0(
      "Maximum pHolm = ",
      sprintf(
        "%.6g",
        max(
          one_sample_tests$p_holm
        )
      )
    ),

    as.character(
      n_total_responses
    ),

    as.character(
      n_classifiable
    ),

    as.character(
      n_non_classifiable
    ),

    as.character(
      n_plain_water
    ),

    as.character(
      n_beverage_like
    ),

    as.character(
      n_caloric
    ),

    paste0(
      sprintf(
        "%.1f",
        100 *
          n_beverage_like /
          n_classifiable
      ),
      "%"
    ),

    as.character(
      nrow(
        open_paired_complete
      )
    ),

    sprintf(
      "%.7f",
      bowker_result$statistic
    ),

    as.character(
      bowker_result$df
    ),

    sprintf(
      "%.7f",
      bowker_result$p_value
    ),

    as.character(
      noncaloric_to_caloric
    ),

    as.character(
      caloric_to_noncaloric
    ),

    sprintf(
      "%.6f",
      mcnemar_exact$p.value
    )
  ),


  pass = c(

    nrow(
      pilot1_data
    ) == 56,

    dplyr::n_distinct(
      pilot1_data$subject
    ) == 28,

    all(
      table(
        pilot1_data$subject
      ) == 2
    ),

    all(
      paired_tests$p_holm >= .05
    ),

    all(
      one_sample_tests$p_holm < .001
    ),

    n_total_responses == 56,

    n_classifiable == 52,

    n_non_classifiable == 4,

    n_plain_water == 4,

    n_beverage_like == 48,

    n_caloric == 22,

    round(
      100 *
        n_beverage_like /
        n_classifiable,
      1
    ) == 92.3,

    nrow(
      open_paired_complete
    ) == 24,

    isTRUE(
      all.equal(
        bowker_result$statistic,
        4.6666667,
        tolerance = 1e-6
      )
    ),

    bowker_result$df == 3,

    isTRUE(
      all.equal(
        bowker_result$p_value,
        0.1978971,
        tolerance = 1e-6
      )
    ),

    noncaloric_to_caloric == 6,

    caloric_to_noncaloric == 1,

    isTRUE(
      all.equal(
        mcnemar_exact$p.value,
        0.125,
        tolerance = 1e-10
      )
    )
  )
) %>%

  mutate(
    status = if_else(
      pass,
      "PASS",
      "FAIL"
    )
  )


write_table(
  verification_summary,
  "Pilot1_verification_summary.csv"
)


cat("\n============================================================\n")
cat("FINAL VERIFICATION SUMMARY\n")
cat("============================================================\n")

print(
  verification_summary
)


# ----------------------------------------------------------------------------
# 16. SESSION INFORMATION
# ----------------------------------------------------------------------------

writeLines(

  capture.output(
    sessionInfo()
  ),

  file.path(
    logs_directory,
    "Pilot1_sessionInfo.txt"
  )
)


# ----------------------------------------------------------------------------
# 17. FINAL STATUS
# ----------------------------------------------------------------------------

if (
  !all(
    verification_summary$pass
  )
) {

  failed_claims <- verification_summary %>%
    filter(
      !pass
    ) %>%
    pull(
      claim
    )


  stop(
    paste0(
      "\nPILOT 1 VERIFICATION FAILED.\n\n",
      "Failed checks:\n",
      paste0(
        "  - ",
        failed_claims,
        collapse = "\n"
      ),
      "\n\nSee:\n",
      file.path(
        tables_directory,
        "Pilot1_verification_summary.csv"
      )
    ),
    call. = FALSE
  )
}


cat("\n============================================================\n")
cat("ALL CENTRAL PILOT 1 RESULTS WERE REPRODUCED.\n")
cat("============================================================\n")
cat("Six supplementary tables saved in:\n", tables_directory, "\n\n")
cat("Figures saved in:\n", figures_directory, "\n\n")
cat("Logs saved in:\n", logs_directory, "\n")
cat("============================================================\n")
