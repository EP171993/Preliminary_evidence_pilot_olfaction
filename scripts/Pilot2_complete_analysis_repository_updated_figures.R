# ============================================================================
# PILOT 2 — COMPLETE REPRODUCIBLE ANALYSIS
# Olfactory Pod (P) versus Water Control (A), N = 47
# ============================================================================
#
# INPUT
#   pilot_2_data_.xlsx
#   The script searches automatically and opens a file-selection window if needed.
#
# The script automatically identifies the analysis-ready worksheet and:
#   - verifies the 47-participant, two-condition repeated-measures structure;
#   - reproduces all analyses reported in the Pilot 2 supplementary results;
#   - generates only the supplementary tables needed for reporting;
#   - recreates the main and exploratory figures;
#   - saves sessionInfo() and a final verification table;
#   - stops if a central reported result is not reproduced.
#
# IMPORTANT DESIGN CONSTANT
# Participants consumed 200 ml in both conditions. The column "Ingested Ml"
# contains the participant's ESTIMATED amount consumed, not actual intake.
# ============================================================================
# ----------------------------------------------------------------------------
# 0. PACKAGES
# ----------------------------------------------------------------------------
required_packages <- c(
  "readxl", "dplyr", "tidyr", "ggplot2", "stringr", "purrr",
  "ARTool", "emmeans", "psych", "broom", "patchwork", "tibble",
  "rstudioapi"
)

installed <- rownames(installed.packages())
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

invisible(lapply(required_packages, library, character.only = TRUE))

options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  contrasts = c("contr.sum", "contr.poly")
)

set.seed(2026)

# ----------------------------------------------------------------------------
# 1. PROJECT PATHS
# ----------------------------------------------------------------------------

get_script_directory <- function() {
  command_args <- commandArgs(trailingOnly = FALSE)
  file_argument <- grep("^--file=", command_args, value = TRUE)

  if (length(file_argument) > 0) {
    script_path <- sub("^--file=", "", file_argument[1])
    return(dirname(normalizePath(script_path)))
  }

  if (
    requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()
  ) {
    active_path <- rstudioapi::getSourceEditorContext()$path

    if (nzchar(active_path)) {
      return(dirname(normalizePath(active_path)))
    }
  }

  normalizePath(getwd())
}

script_directory <- get_script_directory()

initial_project_root <- if (basename(script_directory) == "scripts") {
  dirname(script_directory)
} else {
  script_directory
}

# Optional: enter a complete path here when the workbook is stored elsewhere.
# Leave as an empty string to use automatic search and, if needed, file selection.
manual_file_path <- "C:/Users/Eleonora/OneDrive/MyExperiments/sapienza/airup/bial 2026/analisi/REPOSITORY/pilot_2_data_.xlsx"

resolve_input_file <- function(file_name, manual_path = "") {

  # 1. Explicit path supplied by the user
  if (
    nzchar(manual_path) &&
      file.exists(manual_path)
  ) {
    return(
      normalizePath(
        manual_path,
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  # 2. Standard repository locations
  candidate_paths <- unique(
    c(
      file.path(
        initial_project_root,
        "data",
        file_name
      ),
      file.path(
        initial_project_root,
        file_name
      ),
      file.path(
        script_directory,
        "data",
        file_name
      ),
      file.path(
        script_directory,
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

  existing_paths <- candidate_paths[
    file.exists(candidate_paths)
  ]

  if (length(existing_paths) > 0) {
    return(
      normalizePath(
        existing_paths[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  # 3. Search recursively within the script folder, project root,
  #    and current working directory. This also tolerates minor
  #    filename variations such as an added suffix.
  search_roots <- unique(
    c(
      initial_project_root,
      script_directory,
      getwd()
    )
  )

  search_roots <- search_roots[
    dir.exists(search_roots)
  ]

  discovered_files <- unique(
    unlist(
      lapply(
        search_roots,
        function(root) {
          list.files(
            path = root,
            pattern = "^pilot[_ ]?2.*\\.xlsx$",
            recursive = TRUE,
            full.names = TRUE,
            ignore.case = TRUE
          )
        }
      ),
      use.names = FALSE
    )
  )

  if (length(discovered_files) == 1) {
    message(
      "Workbook found automatically: ",
      discovered_files[1]
    )

    return(
      normalizePath(
        discovered_files[1],
        winslash = "/",
        mustWork = TRUE
      )
    )
  }

  if (length(discovered_files) > 1) {
    message(
      "Several possible Pilot 2 workbooks were found."
    )

    if (interactive()) {
      selected_index <- utils::menu(
        choices = discovered_files,
        title = "Select the Pilot 2 workbook"
      )

      if (selected_index > 0) {
        return(
          normalizePath(
            discovered_files[selected_index],
            winslash = "/",
            mustWork = TRUE
          )
        )
      }
    }
  }

  # 4. Interactive file selection in RStudio/R GUI
  if (interactive()) {
    message(
      "\nThe workbook was not found automatically.\n",
      "Select pilot_2_data_.xlsx in the file-selection window."
    )

    selected_file <- file.choose()

    if (
      file.exists(selected_file) &&
        grepl(
          "\\.xlsx$",
          selected_file,
          ignore.case = TRUE
        )
    ) {
      return(
        normalizePath(
          selected_file,
          winslash = "/",
          mustWork = TRUE
        )
      )
    }
  }

  stop(
    paste0(
      "\nInput workbook not found: ",
      file_name,
      "\n\nSolutions:\n",
      "1. Put the workbook in the same folder as the script;\n",
      "2. Put it in a /data folder beside the script;\n",
      "3. Set manual_file_path to the complete workbook path.\n\n",
      "Standard locations checked:\n",
      paste0(
        "  - ",
        candidate_paths,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

file_name <- "pilot_2_data_.xlsx"

file_path <- resolve_input_file(
  file_name = file_name,
  manual_path = manual_file_path
)

# Outputs are written in the repository containing the selected workbook.
# If the workbook is inside /data, the parent of /data is used as repository root.
data_directory <- dirname(file_path)

project_root <- if (
  basename(data_directory) == "data"
) {
  dirname(data_directory)
} else {
  data_directory
}

output_root <- file.path(project_root, "outputs", "Pilot2")
tables_dir <- file.path(output_root, "tables")
figures_dir <- file.path(output_root, "figures")
logs_dir <- file.path(output_root, "logs")

dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_dir, recursive = TRUE, showWarnings = FALSE)

actual_volume_ml <- 200

cat("\n============================================================\n")
cat("PILOT 2 — COMPLETE REPRODUCIBLE ANALYSIS\n")
cat("============================================================\n")
cat("Input workbook:\n", file_path, "\n\n")
cat("Output directory:\n", output_root, "\n")
cat("============================================================\n")

# ----------------------------------------------------------------------------
# 2. HELPER FUNCTIONS
# ----------------------------------------------------------------------------
first_non_missing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  x[[1]]
}

assert_close <- function(actual, expected, tolerance, label) {
  if (length(actual) != 1 || is.na(actual) || abs(actual - expected) > tolerance) {
    stop(
      sprintf(
        "VERIFICATION FAILED - %s: actual = %.8f; expected = %.8f; tolerance = %.8f",
        label, actual, expected, tolerance
      )
    )
  }
  message(sprintf("PASS - %s: %.6f", label, actual))
}

assert_true <- function(condition, label) {
  if (!isTRUE(condition)) stop(paste("VERIFICATION FAILED -", label))
  message(paste("PASS -", label))
}

safe_shapiro <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) < 3 || length(x) > 5000) return(NA_real_)
  shapiro.test(x)$p.value
}

format_p <- function(p) {
  ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
}

write_table <- function(data, file_name, row_names = FALSE) {
  write.csv(
    data,
    file.path(tables_dir, file_name),
    row.names = row_names,
    na = ""
  )
}

# Tidy a base-R aovlist generated with Error(...)
tidy_aovlist <- function(model) {
  s <- summary(model)
  purrr::imap_dfr(s, function(stratum, stratum_name) {
    if (length(stratum) == 0 || is.null(stratum[[1]])) return(tibble())
    tab <- as.data.frame(stratum[[1]])
    tab$term <- rownames(tab)
    rownames(tab) <- NULL
    names(tab) <- make.names(names(tab))
    tab$error_stratum <- stratum_name
    tibble::as_tibble(tab)
  })
}

# ART alignment for a balanced two-factor design.
# The subsequent ANOVA is conducted on the aligned ranks.
make_art_aligned_rank <- function(data, dv, factor_a, factor_b, effect) {
  y <- data[[dv]]
  a <- data[[factor_a]]
  b <- data[[factor_b]]

  cell_mean <- ave(y, a, b, FUN = mean)
  mean_a <- ave(y, a, FUN = mean)
  mean_b <- ave(y, b, FUN = mean)
  grand_mean <- mean(y)

  aligned <- switch(
    effect,
    factor_a = y - cell_mean + mean_a,
    factor_b = y - cell_mean + mean_b,
    interaction = y - mean_a - mean_b + grand_mean,
    stop("effect must be factor_a, factor_b or interaction")
  )

  # ARTool rounds aligned responses before ranking to make ties stable.
  rank(round(aligned, digits = 12), ties.method = "average")
}

# ----------------------------------------------------------------------------
# 3. IMPORT AND CLEAN DATA
# ----------------------------------------------------------------------------

required_source_columns <- c(
  "SOGG", "Condition", "Combined response", "Binary code", "Binary label",
  "Pleasantness", "Intensity", "Sweet", "Salt", "Sour", "Bitter",
  "KCAL", "Sugar", "Far", "Ingested Ml",
  "sniffing sticks performance",
  "MAIA-Noticing", "MAIA-Not_Distracting", "MAIA-Not_Worrying",
  "MAIA-Attention_Regulation", "MAIA-Emotional_Awareness",
  "MAIA-Self_Regulation", "MAIA-Body_Listening", "MAIA-Trusting", "TAS"
)

# Detect the analysis sheet robustly. This avoids errors caused by differences
# in capitalization, spaces, underscores, or workbook versions.

available_sheets <- readxl::excel_sheets(file_path)

normalise_sheet_name <- function(x) {
  tolower(
    gsub(
      "[^[:alnum:]]",
      "",
      x
    )
  )
}

preferred_sheet_index <- which(
  normalise_sheet_name(available_sheets) == "codingbinary"
)

if (length(preferred_sheet_index) >= 1) {

  analysis_sheet <- available_sheets[
    preferred_sheet_index[1]
  ]

} else {

  # If the expected sheet name is absent, identify the sheet containing all
  # required analysis columns.

  sheet_column_check <- lapply(
    available_sheets,
    function(sheet_name) {

      header <- tryCatch(
        readxl::read_excel(
          file_path,
          sheet = sheet_name,
          n_max = 0
        ),
        error = function(e) NULL
      )

      if (is.null(header)) {
        return(
          data.frame(
            sheet = sheet_name,
            matched_columns = 0,
            all_required_columns = FALSE
          )
        )
      }

      imported_names <- stringr::str_squish(
        names(header)
      )

      data.frame(
        sheet = sheet_name,
        matched_columns = sum(
          required_source_columns %in% imported_names
        ),
        all_required_columns = all(
          required_source_columns %in% imported_names
        )
      )
    }
  )

  sheet_column_check <- dplyr::bind_rows(
    sheet_column_check
  )

  matching_sheets <- sheet_column_check$sheet[
    sheet_column_check$all_required_columns
  ]

  if (length(matching_sheets) == 0) {

    stop(
      paste0(
        "\nNo worksheet contains all required Pilot 2 columns.\n\n",
        "Worksheets found:\n  - ",
        paste(
          available_sheets,
          collapse = "\n  - "
        ),
        "\n\nColumns matched by worksheet:\n",
        paste0(
          "  - ",
          sheet_column_check$sheet,
          ": ",
          sheet_column_check$matched_columns,
          "/",
          length(required_source_columns),
          collapse = "\n"
        ),
        "\n\nPlease check that the selected Excel file is the final ",
        "pilot_2_data_.xlsx workbook."
      ),
      call. = FALSE
    )
  }

  analysis_sheet <- matching_sheets[1]
}

message(
  "Pilot 2 analysis sheet selected: ",
  analysis_sheet
)

raw <- readxl::read_excel(
  file_path,
  sheet = analysis_sheet
)

# Standardise Excel headers. readxl removes or normalises trailing spaces in
# column names, so "Ingested Ml " may be imported as "Ingested Ml".
names(raw) <- stringr::str_squish(
  names(raw)
)

missing_source_columns <- setdiff(required_source_columns, names(raw))
if (length(missing_source_columns) > 0) {
  stop(
    paste0(
      "The following expected Excel columns are missing: ",
      paste(missing_source_columns, collapse = ", "),
      "\nImported column names are: ",
      paste(names(raw), collapse = " | ")
    )
  )
}

dat <- raw %>%
  filter(!is.na(SOGG), Condition %in% c("A", "P")) %>%
  rename(
    subject = SOGG,
    condition = Condition,
    combined_response = `Combined response`,
    binary_code = `Binary code`,
    binary_label = `Binary label`,
    pleasantness = Pleasantness,
    intensity = Intensity,
    sweet = Sweet,
    salt = Salt,
    sour = Sour,
    bitter = Bitter,
    kcal = KCAL,
    sugar = Sugar,
    fat = Far,
    ingested_ml_estimate = `Ingested Ml`,
    sniffing = `sniffing sticks performance`,
    maia_noticing = `MAIA-Noticing`,
    maia_not_distracting = `MAIA-Not_Distracting`,
    maia_not_worrying = `MAIA-Not_Worrying`,
    maia_attention_regulation = `MAIA-Attention_Regulation`,
    maia_emotional_awareness = `MAIA-Emotional_Awareness`,
    maia_self_regulation = `MAIA-Self_Regulation`,
    maia_body_listening = `MAIA-Body_Listening`,
    maia_trusting = `MAIA-Trusting`,
    tas = TAS
  ) %>%
  mutate(
    subject = factor(subject),
    condition = factor(condition, levels = c("A", "P")),
    across(
      c(
        binary_code, pleasantness, intensity, sweet, salt, sour, bitter,
        kcal, sugar, fat, ingested_ml_estimate, sniffing,
        starts_with("maia_"), tas
      ),
      as.numeric
    )
  )

# ----------------------------------------------------------------------------
# 4. STRUCTURAL CHECKS
# ----------------------------------------------------------------------------
participant_condition_check <- dat %>%
  count(subject, condition, name = "n_rows") %>%
  tidyr::complete(subject, condition, fill = list(n_rows = 0))

assert_true(n_distinct(dat$subject) == 47, "N = 47 unique participants")
assert_true(nrow(dat) == 94, "94 rows = 47 participants x 2 conditions")
assert_true(all(participant_condition_check$n_rows == 1),
            "Each participant has exactly one A row and one P row")

trait_cols <- c(
  "sniffing",
  "maia_noticing", "maia_not_distracting", "maia_not_worrying",
  "maia_attention_regulation", "maia_emotional_awareness",
  "maia_self_regulation", "maia_body_listening", "maia_trusting",
  "tas"
)

trait_repeat_check <- dat %>%
  group_by(subject) %>%
  summarise(
    across(all_of(trait_cols), ~ n_distinct(.x[!is.na(.x)])),
    .groups = "drop"
  )

assert_true(
  all(as.matrix(trait_repeat_check[, trait_cols]) <= 1),
  "MAIA, TAS and Sniffing Sticks are identical across A/P rows within participant"
)

subject_traits <- dat %>%
  group_by(subject) %>%
  summarise(
    across(all_of(trait_cols), first_non_missing),
    .groups = "drop"
  )

condition_wide <- dat %>%
  select(
    subject, condition, pleasantness, intensity, sweet, salt, sour, bitter,
    kcal, sugar, fat, ingested_ml_estimate, binary_code
  ) %>%
  pivot_wider(
    names_from = condition,
    values_from = c(
      pleasantness, intensity, sweet, salt, sour, bitter,
      kcal, sugar, fat, ingested_ml_estimate, binary_code
    ),
    names_glue = "{.value}_{condition}"
  ) %>%
  left_join(subject_traits, by = "subject") %>%
  mutate(
    diff_pleasantness = pleasantness_P - pleasantness_A,
    diff_intensity = intensity_P - intensity_A,
    diff_sweet = sweet_P - sweet_A,
    diff_salt = salt_P - salt_A,
    diff_sour = sour_P - sour_A,
    diff_bitter = bitter_P - bitter_A,
    diff_kcal = kcal_P - kcal_A,
    diff_sugar = sugar_P - sugar_A,
    diff_fat = fat_P - fat_A,
    diff_ingested_estimate = ingested_ml_estimate_P - ingested_ml_estimate_A,
    abs_error_A = abs(ingested_ml_estimate_A - actual_volume_ml),
    abs_error_P = abs(ingested_ml_estimate_P - actual_volume_ml),
    diff_abs_error = abs_error_P - abs_error_A,
    z_kcal = as.numeric(scale(diff_kcal)),
    z_sugar = as.numeric(scale(diff_sugar)),
    z_fat = as.numeric(scale(diff_fat)),
    nutritional_composite_z = rowMeans(cbind(z_kcal, z_sugar, z_fat)),
    nutritional_composite_raw = rowMeans(cbind(diff_kcal, diff_sugar, diff_fat))
  )

# ----------------------------------------------------------------------------
# 5. NORMALITY DIAGNOSTICS AND ANALYTIC CHOICE
# ----------------------------------------------------------------------------
nutrition_long <- dat %>%
  select(subject, condition, kcal, sugar, fat) %>%
  pivot_longer(
    cols = c(kcal, sugar, fat),
    names_to = "nutritional_property",
    values_to = "nutritional_rating"
  ) %>%
  mutate(
    nutritional_property = factor(
      nutritional_property,
      levels = c("fat", "kcal", "sugar"),
      labels = c("Fat", "Calories", "Sugar")
    )
  )

taste_long <- dat %>%
  select(subject, condition, sweet, salt, sour, bitter) %>%
  pivot_longer(
    cols = c(sweet, salt, sour, bitter),
    names_to = "taste",
    values_to = "taste_rating"
  ) %>%
  mutate(
    taste = factor(
      taste,
      levels = c("bitter", "sour", "sweet", "salt"),
      labels = c("Bitter", "Sour", "Sweet", "Salt")
    )
  )

# Condition-level descriptives for all outcomes displayed in the figures.
descriptive_long <- dat %>%
  select(
    subject, condition, pleasantness, intensity,
    sweet, salt, sour, bitter, kcal, sugar, fat,
    ingested_ml_estimate
  ) %>%
  pivot_longer(
    cols = -c(subject, condition),
    names_to = "measure",
    values_to = "value"
  ) %>%
  mutate(
    condition_label = recode(
      as.character(condition),
      "A" = "Water Control",
      "P" = "Olfactory Pod"
    ),
    measure = recode(
      measure,
      "pleasantness" = "Pleasantness",
      "intensity" = "Intensity",
      "sweet" = "Sweetness",
      "salt" = "Saltiness",
      "sour" = "Sourness",
      "bitter" = "Bitterness",
      "kcal" = "Calories",
      "sugar" = "Sugar",
      "fat" = "Fat",
      "ingested_ml_estimate" = "Estimated amount consumed (ml)"
    )
  )

descriptive_table <- descriptive_long %>%
  group_by(measure, condition_label) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q1 = unname(quantile(value, .25, na.rm = TRUE)),
    q3 = unname(quantile(value, .75, na.rm = TRUE)),
    minimum = min(value, na.rm = TRUE),
    maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  )

write_table(
  descriptive_table,
  "Table_S1_Pilot2_descriptives.csv"
)

nutrition_raw_model <- lm(
  nutritional_rating ~ subject + condition * nutritional_property,
  data = nutrition_long
)

taste_raw_model <- lm(
  taste_rating ~ subject + condition * taste,
  data = taste_long
)

normality_table <- tibble(
  model = c("Nutritional factorial model", "Taste factorial model"),
  residual_shapiro_W = c(
    unname(shapiro.test(residuals(nutrition_raw_model))$statistic),
    unname(shapiro.test(residuals(taste_raw_model))$statistic)
  ),
  residual_shapiro_p = c(
    shapiro.test(residuals(nutrition_raw_model))$p.value,
    shapiro.test(residuals(taste_raw_model))$p.value
  ),
  analysis_selected = c("ART", "ART")
)

print(normality_table)
assert_true(all(normality_table$residual_shapiro_p < .05),
            "Factorial-model residuals violate normality; ART is justified")

# ----------------------------------------------------------------------------
# 6. NUTRITIONAL PROPERTIES - ART OMNIBUS MODEL
# ----------------------------------------------------------------------------
# Mixed-effects ART model reproducing the reported residual df = 230.
nutrition_art <- ARTool::art(
  nutritional_rating ~ condition * nutritional_property + (1 | subject),
  data = nutrition_long
)

nutrition_art_anova <- anova(nutrition_art)
print(nutrition_art_anova)

nutrition_art_anova_df <- as.data.frame(nutrition_art_anova) %>%
  rownames_to_column("term")
# Independent numerical replication of the reported interaction.
nutrition_long$rank_interaction <- make_art_aligned_rank(
  nutrition_long,
  dv = "nutritional_rating",
  factor_a = "condition",
  factor_b = "nutritional_property",
  effect = "interaction"
)

nutrition_interaction_lm <- lm(
  rank_interaction ~ subject + condition * nutritional_property,
  data = nutrition_long
)

nutrition_interaction_tab <- as.data.frame(anova(nutrition_interaction_lm)) %>%
  rownames_to_column("term")
names(nutrition_interaction_tab) <- make.names(names(nutrition_interaction_tab))

nutrition_interaction_row <- nutrition_interaction_tab %>%
  filter(term == "condition:nutritional_property")

assert_close(nutrition_interaction_row$F.value, 22.023, .02,
             "Nutrition Condition x Property F")
assert_true(nutrition_interaction_row$Df == 2,
            "Nutrition interaction numerator df = 2")
assert_true(df.residual(nutrition_interaction_lm) == 230,
            "Nutrition interaction residual df = 230")
assert_true(nutrition_interaction_row$Pr..F. < .001,
            "Nutrition interaction p < .001")

# ART-C planned comparisons. For a contrast involving all fixed factors,
# ART-C corresponds to ranking the original response and fitting the complete
# factorial model with participant as the repeated-measures block.
nutrition_artc_data <- nutrition_long %>%
  mutate(artc_rank = rank(nutritional_rating, ties.method = "average"))

nutrition_artc_model <- lm(
  artc_rank ~ subject + condition * nutritional_property,
  data = nutrition_artc_data
)

nutrition_emm <- emmeans::emmeans(
  nutrition_artc_model,
  ~ condition | nutritional_property
)

nutrition_contrasts <- summary(
  pairs(nutrition_emm, adjust = "none"),
  infer = c(TRUE, TRUE)
) %>%
  as.data.frame() %>%
  mutate(
    p_holm = p.adjust(p.value, method = "holm"),
    significance = case_when(
      p_holm < .001 ~ "***",
      p_holm < .01 ~ "**",
      p_holm < .05 ~ "*",
      TRUE ~ "ns"
    )
  )

print(nutrition_contrasts)
assert_true(nrow(nutrition_contrasts) == 3,
            "Three planned nutritional contrasts")
assert_true(all(nutrition_contrasts$p_holm < .001),
            "Fat, calories and sugar: all Holm-adjusted p < .001")

# ----------------------------------------------------------------------------
# 7. COHERENCE OF NUTRITIONAL INDICES
# Pearson correlations and standardized Cronbach's alpha
# ----------------------------------------------------------------------------

nutrition_indices <- condition_wide %>%
  select(
    diff_kcal,
    diff_sugar,
    diff_fat
  )

# Pearson correlations
nutrition_cor_matrix <- cor(
  nutrition_indices,
  method = "pearson",
  use = "complete.obs"
)

nutrition_cor_long <- as.data.frame(
  as.table(nutrition_cor_matrix)
) %>%
  rename(
    index_1 = Var1,
    index_2 = Var2,
    r = Freq
  )

# Standardized Cronbach's alpha
alpha_result <- psych::alpha(
  nutrition_indices,
  check.keys = FALSE,
  warnings = FALSE
)

standardized_alpha <- alpha_result$total$std.alpha

# Print results
cat("\n====================================================\n")
cat("COHERENCE OF NUTRITIONAL MODULATION INDICES\n")
cat("====================================================\n")

print(nutrition_cor_matrix)

cat(
  "\nStandardized Cronbach's alpha:",
  round(standardized_alpha, 6),
  "\n"
)

# Automatic verification
pairwise_r <- nutrition_cor_matrix[
  lower.tri(nutrition_cor_matrix)
]

assert_close(
  min(pairwise_r),
  0.4807414,
  0.0001,
  "Minimum Pearson nutritional-index correlation"
)

assert_close(
  max(pairwise_r),
  0.7394307,
  0.0001,
  "Maximum Pearson nutritional-index correlation"
)

assert_close(
  standardized_alpha,
  0.8163894,
  0.0001,
  "Standardized Cronbach's alpha"
)

cat("\nPASS: Pearson correlations and standardized alpha verified.\n")




# ----------------------------------------------------------------------------
# 8. TASTE PROFILE - REPEATED-MEASURES ART
# ----------------------------------------------------------------------------
# Full repeated-measures error structure reproducing df = 46 and 138.
taste_art <- ARTool::art(
  taste_rating ~ taste * condition + Error(subject / (taste * condition)),
  data = taste_long
)

taste_art_anova <- anova(taste_art)
print(taste_art_anova)

taste_art_anova_df <- as.data.frame(taste_art_anova) %>%
  rownames_to_column("term")
# Independent ART replication of each omnibus effect.
taste_art_tables <- list()

for (effect_name in c("factor_a", "factor_b", "interaction")) {
  rank_name <- paste0("rank_", effect_name)
  taste_long[[rank_name]] <- make_art_aligned_rank(
    taste_long,
    dv = "taste_rating",
    factor_a = "condition",
    factor_b = "taste",
    effect = effect_name
  )

  model <- aov(
    as.formula(paste0(rank_name, " ~ taste * condition + Error(subject / (taste * condition))")),
    data = taste_long
  )

  taste_art_tables[[effect_name]] <- tidy_aovlist(model) %>%
    mutate(alignment = effect_name)
}

taste_manual_art <- bind_rows(taste_art_tables)
# Interaction comes from the interaction-aligned model.
taste_interaction_row <- taste_manual_art %>%
  filter(
    alignment == "interaction",
    str_trim(term) %in% c("taste:condition", "condition:taste")
  )

assert_true(nrow(taste_interaction_row) == 1,
            "Taste ART interaction row found")
assert_close(taste_interaction_row$F.value, 82.254, .05,
             "Taste Condition x Taste F")
assert_true(taste_interaction_row$Df == 3,
            "Taste interaction numerator df = 3")
assert_true(taste_interaction_row$Pr..F. < .001,
            "Taste interaction p < .001")

# ART-C planned comparisons: A vs P within each taste.
taste_artc_data <- taste_long %>%
  mutate(artc_rank = rank(taste_rating, ties.method = "average"))

taste_artc_model <- lm(
  artc_rank ~ subject + condition * taste,
  data = taste_artc_data
)

taste_emm <- emmeans::emmeans(taste_artc_model, ~ condition | taste)

taste_contrasts <- summary(
  pairs(taste_emm, adjust = "none"),
  infer = c(TRUE, TRUE)
) %>%
  as.data.frame() %>%
  mutate(
    p_fdr = p.adjust(p.value, method = "BH"),
    significance = case_when(
      p_fdr < .001 ~ "***",
      p_fdr < .01 ~ "**",
      p_fdr < .05 ~ "*",
      TRUE ~ "ns"
    )
  )

print(taste_contrasts)
sig_tastes <- taste_contrasts %>% filter(taste %in% c("Sweet", "Sour", "Bitter"))
salt_row <- taste_contrasts %>% filter(taste == "Salt")

assert_true(all(sig_tastes$p_fdr <= .011),
            "Sweet, sour and bitter all FDR-adjusted p <= .010 approximately")
assert_close(salt_row$p_fdr, .900, .002,
             "Salt FDR-adjusted p")

nutrition_cor_pairs <- tibble(
  index_1 = c("Calories", "Calories", "Sugar"),
  index_2 = c("Sugar", "Fat", "Fat"),
  r = c(
    nutrition_cor_matrix["diff_kcal", "diff_sugar"],
    nutrition_cor_matrix["diff_kcal", "diff_fat"],
    nutrition_cor_matrix["diff_sugar", "diff_fat"]
  )
)

factorial_results_table <- bind_rows(
  normality_table %>%
    transmute(
      record_type = "Normality diagnostic",
      domain = model,
      outcome = NA_character_,
      term = "Residual Shapiro-Wilk test",
      comparison = NA_character_,
      estimate = NA_real_,
      SE = NA_real_,
      statistic = residual_shapiro_W,
      df1 = NA_real_,
      df2 = NA_real_,
      p_raw = residual_shapiro_p,
      p_adjusted = NA_real_,
      adjustment = NA_character_,
      r = NA_real_,
      standardized_alpha = NA_real_,
      note = analysis_selected
    ),

  tibble(
    record_type = "Omnibus interaction",
    domain = "Nutritional attribution",
    outcome = "Fat, Calories, Sugar",
    term = "Condition × Nutritional Property",
    comparison = NA_character_,
    estimate = NA_real_,
    SE = NA_real_,
    statistic = nutrition_interaction_row$F.value,
    df1 = nutrition_interaction_row$Df,
    df2 = df.residual(nutrition_interaction_lm),
    p_raw = nutrition_interaction_row$Pr..F.,
    p_adjusted = NA_real_,
    adjustment = NA_character_,
    r = NA_real_,
    standardized_alpha = NA_real_,
    note = "Aligned Rank Transform"
  ),

  nutrition_contrasts %>%
    transmute(
      record_type = "Planned contrast",
      domain = "Nutritional attribution",
      outcome = as.character(nutritional_property),
      term = "Water Control versus Olfactory Pod",
      comparison = as.character(contrast),
      estimate = estimate,
      SE = SE,
      statistic = t.ratio,
      df1 = NA_real_,
      df2 = df,
      p_raw = p.value,
      p_adjusted = p_holm,
      adjustment = "Holm",
      r = NA_real_,
      standardized_alpha = NA_real_,
      note = "ART-C rank-scale estimate"
    ),

  tibble(
    record_type = "Omnibus interaction",
    domain = "Taste profile",
    outcome = "Sweetness, Saltiness, Sourness, Bitterness",
    term = "Condition × Taste",
    comparison = NA_character_,
    estimate = NA_real_,
    SE = NA_real_,
    statistic = taste_interaction_row$F.value,
    df1 = taste_interaction_row$Df,
    df2 = 138,
    p_raw = taste_interaction_row$Pr..F.,
    p_adjusted = NA_real_,
    adjustment = NA_character_,
    r = NA_real_,
    standardized_alpha = NA_real_,
    note = "Repeated-measures Aligned Rank Transform"
  ),

  taste_contrasts %>%
    transmute(
      record_type = "Planned contrast",
      domain = "Taste profile",
      outcome = as.character(taste),
      term = "Water Control versus Olfactory Pod",
      comparison = as.character(contrast),
      estimate = estimate,
      SE = SE,
      statistic = t.ratio,
      df1 = NA_real_,
      df2 = df,
      p_raw = p.value,
      p_adjusted = p_fdr,
      adjustment = "Benjamini-Hochberg FDR",
      r = NA_real_,
      standardized_alpha = NA_real_,
      note = "ART-C rank-scale estimate"
    ),

  nutrition_cor_pairs %>%
    transmute(
      record_type = "Coherence",
      domain = "Nutritional attribution",
      outcome = paste(index_1, "with", index_2),
      term = "Pearson correlation",
      comparison = NA_character_,
      estimate = NA_real_,
      SE = NA_real_,
      statistic = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p_raw = NA_real_,
      p_adjusted = NA_real_,
      adjustment = NA_character_,
      r = r,
      standardized_alpha = NA_real_,
      note = "Correlation between Pod-minus-Control modulation indices"
    ),

  tibble(
    record_type = "Coherence",
    domain = "Nutritional attribution",
    outcome = "Calories, Sugar and Fat modulation indices",
    term = "Standardized Cronbach alpha",
    comparison = NA_character_,
    estimate = NA_real_,
    SE = NA_real_,
    statistic = NA_real_,
    df1 = NA_real_,
    df2 = NA_real_,
    p_raw = NA_real_,
    p_adjusted = NA_real_,
    adjustment = NA_character_,
    r = NA_real_,
    standardized_alpha = standardized_alpha,
    note = "Internal consistency of the three Pod-minus-Control indices"
  )
)

write_table(
  factorial_results_table,
  "Table_S2_Pilot2_nutrition_and_taste_ART.csv"
)

# ----------------------------------------------------------------------------
# 9. EXPLORATORY REGRESSION: TASTE -> NUTRITIONAL ATTRIBUTION
# ----------------------------------------------------------------------------
composite_model <- lm(
  nutritional_composite_z ~ diff_sweet + diff_salt + diff_sour + diff_bitter,
  data = condition_wide
)

fat_model <- lm(
  diff_fat ~ diff_sweet + diff_salt + diff_sour + diff_bitter,
  data = condition_wide
)

kcal_model <- lm(
  diff_kcal ~ diff_sweet + diff_salt + diff_sour + diff_bitter,
  data = condition_wide
)

sugar_model <- lm(
  diff_sugar ~ diff_sweet + diff_salt + diff_sour + diff_bitter,
  data = condition_wide
)

regression_fit <- bind_rows(
  broom::glance(composite_model) %>% mutate(model = "Composite z"),
  broom::glance(fat_model) %>% mutate(model = "Fat"),
  broom::glance(kcal_model) %>% mutate(model = "Calories"),
  broom::glance(sugar_model) %>% mutate(model = "Sugar")
) %>%
  select(model, everything())

regression_coefficients <- bind_rows(
  broom::tidy(composite_model) %>% mutate(model = "Composite z"),
  broom::tidy(fat_model) %>% mutate(model = "Fat"),
  broom::tidy(kcal_model) %>% mutate(model = "Calories"),
  broom::tidy(sugar_model) %>% mutate(model = "Sugar")
) %>%
  select(model, everything())

print(regression_fit)
print(regression_coefficients)
assert_close(summary(composite_model)$r.squared, .381, .002,
             "Composite nutritional regression R-squared")
assert_true(coef(summary(composite_model))["diff_sweet", "Pr(>|t|)"] < .001,
            "Sweetness predicts composite nutritional attribution")
assert_true(coef(summary(composite_model))["diff_bitter", "Pr(>|t|)"] < .05,
            "Bitterness predicts composite nutritional attribution")

assert_true(coef(summary(fat_model))["diff_sweet", "Pr(>|t|)"] < .05,
            "Sweetness predicts perceived fat")
assert_true(coef(summary(kcal_model))["diff_sweet", "Pr(>|t|)"] < .01,
            "Sweetness predicts perceived calories")
assert_true(coef(summary(sugar_model))["diff_sweet", "Pr(>|t|)"] < .001,
            "Sweetness predicts perceived sugar")
assert_true(coef(summary(fat_model))["diff_bitter", "Pr(>|t|)"] < .05,
            "Bitterness selectively predicts perceived fat")
assert_true(coef(summary(kcal_model))["diff_bitter", "Pr(>|t|)"] >= .05 &&
              coef(summary(sugar_model))["diff_bitter", "Pr(>|t|)"] >= .05,
            "Bitterness does not predict calories or sugar")

# Regression residual diagnostics
regression_normality <- tibble(
  model = c("Composite z", "Fat", "Calories", "Sugar"),
  residual_shapiro_p = c(
    safe_shapiro(residuals(composite_model)),
    safe_shapiro(residuals(fat_model)),
    safe_shapiro(residuals(kcal_model)),
    safe_shapiro(residuals(sugar_model))
  )
)




regression_normality <- regression_normality %>%
  mutate(
    interpretation = if_else(
      residual_shapiro_p >= .05,
      "No evidence of non-normal residuals",
      "Possible non-normal residuals"
    )
  )

print(regression_normality)






# ============================================================
# BOOTSTRAP CONFIDENCE INTERVALS FOR THE FAT MODEL
# ============================================================

set.seed(2026)

n_boot <- 10000

bootstrap_coefficients <- replicate(
  n_boot,
  {
    sampled_rows <- sample(
      seq_len(nrow(condition_wide)),
      replace = TRUE
    )
    
    bootstrap_model <- lm(
      diff_fat ~
        diff_sweet +
        diff_salt +
        diff_sour +
        diff_bitter,
      data = condition_wide[sampled_rows, ]
    )
    
    coef(bootstrap_model)[
      c(
        "diff_sweet",
        "diff_bitter"
      )
    ]
  }
)

bootstrap_CI <- data.frame(
  predictor = c(
    "Sweetness",
    "Bitterness"
  ),
  
  estimate = c(
    coef(fat_model)["diff_sweet"],
    coef(fat_model)["diff_bitter"]
  ),
  
  CI_lower = apply(
    bootstrap_coefficients,
    1,
    quantile,
    probs = .025,
    na.rm = TRUE
  ),
  
  CI_upper = apply(
    bootstrap_coefficients,
    1,
    quantile,
    probs = .975,
    na.rm = TRUE
  )
)

cat("\n========================================\n")
cat("FAT MODEL: BOOTSTRAP 95% CONFIDENCE INTERVALS\n")
cat("========================================\n")

print(bootstrap_CI)

regression_results_table <- bind_rows(
  regression_fit %>%
    transmute(
      record_type = "Model fit",
      model,
      term = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = statistic,
      df1 = df,
      df2 = df.residual,
      p = p.value,
      r_squared = r.squared,
      adjusted_r_squared = adj.r.squared,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      note = NA_character_
    ),

  regression_coefficients %>%
    transmute(
      record_type = "Coefficient",
      model,
      term,
      estimate,
      std_error = std.error,
      statistic,
      df1 = NA_real_,
      df2 = NA_real_,
      p = p.value,
      r_squared = NA_real_,
      adjusted_r_squared = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      note = "Multiple regression coefficient"
    ),

  regression_normality %>%
    transmute(
      record_type = "Residual diagnostic",
      model,
      term = "Residual Shapiro-Wilk test",
      estimate = NA_real_,
      std_error = NA_real_,
      statistic = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p = residual_shapiro_p,
      r_squared = NA_real_,
      adjusted_r_squared = NA_real_,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      note = interpretation
    ),

  bootstrap_CI %>%
    transmute(
      record_type = "Bootstrap confidence interval",
      model = "Fat",
      term = predictor,
      estimate,
      std_error = NA_real_,
      statistic = NA_real_,
      df1 = NA_real_,
      df2 = NA_real_,
      p = NA_real_,
      r_squared = NA_real_,
      adjusted_r_squared = NA_real_,
      ci_lower = CI_lower,
      ci_upper = CI_upper,
      note = "Percentile bootstrap 95% CI; 10,000 resamples"
    )
)

write_table(
  regression_results_table,
  "Table_S3_Pilot2_regressions.csv"
)

# ----------------------------------------------------------------------------
# 10. ESTIMATED INGESTION AMOUNT: POD VS WATER CONTROL
# ----------------------------------------------------------------------------

# Dataset con una riga per partecipante e una coppia completa A/P
ingestion_data <- condition_wide %>%
  select(
    subject,
    ingested_ml_estimate_A,
    ingested_ml_estimate_P
  ) %>%
  drop_na() %>%
  mutate(
    # Differenza negativa = quantità stimata inferiore nella condizione Pod
    diff_ingested_estimate =
      ingested_ml_estimate_P - ingested_ml_estimate_A
  )


# Verifica del numero di coppie complete
assert_true(
  nrow(ingestion_data) == 47,
  "Estimated-ingestion analysis includes 47 complete paired observations"
)


# ----------------------------------------------------------------------------
# 10a. NORMALITY OF THE PAIRED DIFFERENCES
# Nel paired t-test si controlla la distribuzione delle differenze P - A
# ----------------------------------------------------------------------------

ingestion_normality <- shapiro.test(
  ingestion_data$diff_ingested_estimate
)

cat("\n====================================================\n")
cat("NORMALITY OF ESTIMATED-INGESTION DIFFERENCES: P - A\n")
cat("====================================================\n")

print(ingestion_normality)

assert_true(
  ingestion_normality$p.value >= .05,
  "Estimated-ingestion difference scores are compatible with normality"
)


# ----------------------------------------------------------------------------
# 10b. PAIRED T-TEST: POD VS WATER CONTROL
# ----------------------------------------------------------------------------

ingestion_t <- t.test(
  ingestion_data$ingested_ml_estimate_P,
  ingestion_data$ingested_ml_estimate_A,
  paired = TRUE,
  alternative = "two.sided",
  conf.level = .95
)


# Descriptive statistics
mean_A <- mean(
  ingestion_data$ingested_ml_estimate_A
)

mean_P <- mean(
  ingestion_data$ingested_ml_estimate_P
)

sd_A <- sd(
  ingestion_data$ingested_ml_estimate_A
)

sd_P <- sd(
  ingestion_data$ingested_ml_estimate_P
)

mean_difference <- mean(
  ingestion_data$diff_ingested_estimate
)

sd_difference <- sd(
  ingestion_data$diff_ingested_estimate
)


# Final results table
ingestion_results <- tibble(
  n = nrow(ingestion_data),
  
  mean_water_control = mean_A,
  sd_water_control = sd_A,
  
  mean_olfactory_pod = mean_P,
  sd_olfactory_pod = sd_P,
  
  mean_difference_P_minus_A = mean_difference,
  sd_difference = sd_difference,
  
  t = unname(ingestion_t$statistic),
  df = unname(ingestion_t$parameter),
  p = ingestion_t$p.value,
  
  ci_lower = ingestion_t$conf.int[1],
  ci_upper = ingestion_t$conf.int[2],
  
  shapiro_W = unname(ingestion_normality$statistic),
  shapiro_p = ingestion_normality$p.value
)


cat("\n====================================================\n")
cat("ESTIMATED INGESTION: POD VS WATER CONTROL\n")
cat("====================================================\n")

print(ingestion_results)

print(ingestion_t)


# ----------------------------------------------------------------------------
# 10c. AUTOMATIC VERIFICATION OF THE REPORTED RESULTS
# ----------------------------------------------------------------------------

assert_close(
  mean_difference,
  -31.051,
  .01,
  "Mean estimated-ingestion difference P - A"
)

assert_close(
  unname(ingestion_t$statistic),
  -2.267,
  .01,
  "Estimated-ingestion paired t statistic"
)

assert_true(
  unname(ingestion_t$parameter) == 46,
  "Estimated-ingestion paired t-test df = 46"
)

assert_close(
  ingestion_t$p.value,
  .028134,
  .001,
  "Estimated-ingestion paired t-test p value"
)

cat("\n====================================================\n")
cat("PASS: ESTIMATED-INGESTION DIFFERENCE VERIFIED.\n")
cat("====================================================\n")

# ----------------------------------------------------------------------------
# 10d. ABSOLUTE ESTIMATION ERROR RELATIVE TO THE ACTUAL 200 ML
# ----------------------------------------------------------------------------
# Absolute error quantifies estimation accuracy irrespective of direction:
# |estimated amount - actual amount consumed|

absolute_error_data <- condition_wide %>%
  select(
    subject,
    abs_error_A,
    abs_error_P,
    diff_abs_error
  ) %>%
  drop_na()

assert_true(
  nrow(absolute_error_data) == 47,
  "Absolute-error analysis includes 47 complete paired observations"
)

# Normality of the paired difference in absolute error
absolute_error_normality <- shapiro.test(
  absolute_error_data$diff_abs_error
)

cat("\n====================================================\n")
cat("NORMALITY OF ABSOLUTE-ERROR DIFFERENCES: POD - WATER\n")
cat("====================================================\n")

print(absolute_error_normality)

# Because the paired differences are non-normal, use a paired Wilcoxon test
absolute_error_wilcox <- wilcox.test(
  absolute_error_data$abs_error_P,
  absolute_error_data$abs_error_A,
  paired = TRUE,
  exact = FALSE,
  alternative = "two.sided"
)

absolute_error_results <- tibble(
  n = nrow(absolute_error_data),
  
  mean_absolute_error_water = mean(
    absolute_error_data$abs_error_A
  ),
  
  sd_absolute_error_water = sd(
    absolute_error_data$abs_error_A
  ),
  
  median_absolute_error_water = median(
    absolute_error_data$abs_error_A
  ),
  
  mean_absolute_error_pod = mean(
    absolute_error_data$abs_error_P
  ),
  
  sd_absolute_error_pod = sd(
    absolute_error_data$abs_error_P
  ),
  
  median_absolute_error_pod = median(
    absolute_error_data$abs_error_P
  ),
  
  mean_difference_P_minus_A = mean(
    absolute_error_data$diff_abs_error
  ),
  
  Wilcoxon_V = unname(
    absolute_error_wilcox$statistic
  ),
  
  p = absolute_error_wilcox$p.value,
  
  shapiro_W = unname(
    absolute_error_normality$statistic
  ),
  
  shapiro_p = absolute_error_normality$p.value
)

cat("\n====================================================\n")
cat("ABSOLUTE ESTIMATION ERROR: POD VS WATER CONTROL\n")
cat("====================================================\n")

print(absolute_error_results)
print(absolute_error_wilcox)

# Automatic verification based on the final dataset
assert_close(
  mean(absolute_error_data$abs_error_A),
  80.8617,
  .01,
  "Mean absolute error in Water Control"
)

assert_close(
  mean(absolute_error_data$abs_error_P),
  77.6362,
  .01,
  "Mean absolute error in Olfactory Pod"
)

assert_close(
  unname(absolute_error_wilcox$statistic),
  605,
  .01,
  "Absolute-error Wilcoxon V"
)

assert_true(
  absolute_error_wilcox$p.value >= .05,
  "Absolute estimation error does not differ between conditions"
)

cat("\n====================================================\n")
cat("PASS: ABSOLUTE ESTIMATION ERROR VERIFIED.\n")
cat("====================================================\n")


# ----------------------------------------------------------------------------
# 11. OLFACTORY IDENTIFICATION, MAIA AND TAS
# ----------------------------------------------------------------------------
# 11a. Original targeted analyses
nutrition_sniffing_vars <- c(
  "diff_kcal", "diff_sugar", "diff_fat",
  "nutritional_composite_raw", "nutritional_composite_z"
)

nutrition_sniffing <- purrr::map_dfr(nutrition_sniffing_vars, function(v) {
  test <- cor.test(
    condition_wide[[v]], condition_wide$sniffing,
    method = "spearman", exact = FALSE
  )
  tibble(effect = v, rho = unname(test$estimate), p = test$p.value)
}) %>%
  mutate(p_holm = p.adjust(p, method = "holm"))

taste_sniffing_vars <- c("diff_sweet", "diff_salt", "diff_sour", "diff_bitter")

taste_sniffing <- purrr::map_dfr(taste_sniffing_vars, function(v) {
  test <- cor.test(condition_wide[[v]], condition_wide$sniffing, method = "pearson")
  tibble(effect = v, r = unname(test$estimate), p = test$p.value)
}) %>%
  mutate(p_holm = p.adjust(p, method = "holm"))

maia_tas_vars <- c(
  "maia_noticing", "maia_not_distracting", "maia_not_worrying",
  "maia_attention_regulation", "maia_emotional_awareness",
  "maia_self_regulation", "maia_body_listening", "maia_trusting", "tas"
)

ingestion_maia_tas <- purrr::map_dfr(maia_tas_vars, function(v) {
  test <- cor.test(
    condition_wide$diff_ingested_estimate,
    condition_wide[[v]],
    method = "spearman",
    exact = FALSE
  )
  tibble(moderator = v, rho = unname(test$estimate), p = test$p.value)
}) %>%
  mutate(p_holm = p.adjust(p, method = "holm"))
print(ingestion_maia_tas)


ingestion_nutrition_vars <- c(
  "diff_kcal", "diff_sugar", "diff_fat", "nutritional_composite_z"
)

ingestion_nutrition <- purrr::map_dfr(ingestion_nutrition_vars, function(v) {
  test <- cor.test(
    condition_wide$diff_ingested_estimate,
    condition_wide[[v]],
    method = "pearson"
  )
  tibble(effect = v, r = unname(test$estimate), p = test$p.value)
}) %>%
  mutate(p_holm = p.adjust(p, method = "holm"))

assert_true(all(nutrition_sniffing$p_holm >= .05),
            "Nutritional effects are not robustly associated with Sniffing Sticks")
assert_true(all(taste_sniffing$p_holm >= .05),
            "Taste effects are not robustly associated with Sniffing Sticks")
assert_true(all(ingestion_maia_tas$p_holm >= .05),
            "Estimated-ingestion effect is not robustly associated with MAIA or TAS")
assert_true(all(ingestion_nutrition$p_holm >= .05),
            "Estimated-ingestion effect is not robustly associated with nutritional attribution")

# 11b. Exhaustive robustness matrix supporting the broad grant-level sentence.
primary_effect_indices <- c(
  "diff_kcal", "diff_sugar", "diff_fat", "nutritional_composite_z",
  "diff_sweet", "diff_salt", "diff_sour", "diff_bitter",
  "diff_ingested_estimate", "diff_intensity", "diff_pleasantness"
)

moderators <- c("sniffing", maia_tas_vars)

all_moderator_correlations <- tidyr::crossing(
  effect = primary_effect_indices,
  moderator = moderators
) %>%
  mutate(
    test = purrr::map2(effect, moderator, ~ cor.test(
      condition_wide[[.x]], condition_wide[[.y]],
      method = "spearman", exact = FALSE
    )),
    rho = purrr::map_dbl(test, ~ unname(.x$estimate)),
    p = purrr::map_dbl(test, ~ .x$p.value),
    p_fdr_global = p.adjust(p, method = "BH")
  ) %>%
  select(-test)

assert_true(all(all_moderator_correlations$p_fdr_global >= .05),
            "No primary effect is robustly explained by Sniffing, MAIA or TAS after global FDR correction")


individual_difference_table <- bind_rows(
  nutrition_sniffing %>%
    transmute(
      analysis_family = "Nutritional effects versus olfactory identification",
      effect,
      moderator = "Sniffin' Sticks identification performance",
      method = "Spearman",
      coefficient = rho,
      p,
      p_adjusted = p_holm,
      adjustment = "Holm"
    ),

  taste_sniffing %>%
    transmute(
      analysis_family = "Taste effects versus olfactory identification",
      effect,
      moderator = "Sniffin' Sticks identification performance",
      method = "Pearson",
      coefficient = r,
      p,
      p_adjusted = p_holm,
      adjustment = "Holm"
    ),

  ingestion_maia_tas %>%
    transmute(
      analysis_family = "Estimated-intake effect versus MAIA/TAS",
      effect = "diff_ingested_estimate",
      moderator,
      method = "Spearman",
      coefficient = rho,
      p,
      p_adjusted = p_holm,
      adjustment = "Holm"
    ),

  ingestion_nutrition %>%
    rename(
      moderator = effect
    ) %>%
    transmute(
      analysis_family = "Estimated-intake effect versus nutritional attribution",
      effect = "diff_ingested_estimate",
      moderator,
      method = "Pearson",
      coefficient = r,
      p,
      p_adjusted = p_holm,
      adjustment = "Holm"
    ),

  all_moderator_correlations %>%
    transmute(
      analysis_family = "Global robustness matrix",
      effect,
      moderator,
      method = "Spearman",
      coefficient = rho,
      p,
      p_adjusted = p_fdr_global,
      adjustment = "Benjamini-Hochberg FDR across all effect–moderator tests"
    )
)

write_table(
  individual_difference_table,
  "Table_S5_Pilot2_individual_differences.csv"
)

# ----------------------------------------------------------------------------
# 12. INTENSITY AND PLEASANTNESS
# Paired Wilcoxon signed-rank tests
# ----------------------------------------------------------------------------

# Normality checks on paired difference scores
intensity_shapiro <- shapiro.test(
  condition_wide$intensity_P - condition_wide$intensity_A
)

pleasantness_shapiro <- shapiro.test(
  condition_wide$pleasantness_P - condition_wide$pleasantness_A
)

print(intensity_shapiro)
print(pleasantness_shapiro)

# Paired Wilcoxon tests
intensity_wilcox <- wilcox.test(
  condition_wide$intensity_A,
  condition_wide$intensity_P,
  paired = TRUE,
  exact = FALSE,
  alternative = "two.sided"
)

pleasantness_wilcox <- wilcox.test(
  condition_wide$pleasantness_A,
  condition_wide$pleasantness_P,
  paired = TRUE,
  exact = FALSE,
  alternative = "two.sided"
)

# Descriptive medians
intensity_median_A <- median(
  condition_wide$intensity_A,
  na.rm = TRUE
)

intensity_median_P <- median(
  condition_wide$intensity_P,
  na.rm = TRUE
)

pleasantness_median_A <- median(
  condition_wide$pleasantness_A,
  na.rm = TRUE
)

pleasantness_median_P <- median(
  condition_wide$pleasantness_P,
  na.rm = TRUE
)

# Results table
intensity_pleasantness_results <- tibble(
  outcome = c(
    "Intensity",
    "Pleasantness"
  ),
  
  median_water_control = c(
    intensity_median_A,
    pleasantness_median_A
  ),
  
  median_olfactory_pod = c(
    intensity_median_P,
    pleasantness_median_P
  ),
  
  shapiro_difference_p = c(
    intensity_shapiro$p.value,
    pleasantness_shapiro$p.value
  ),
  
  Wilcoxon_V = c(
    unname(intensity_wilcox$statistic),
    unname(pleasantness_wilcox$statistic)
  ),
  
  p = c(
    intensity_wilcox$p.value,
    pleasantness_wilcox$p.value
  )
)

cat("\n====================================================\n")
cat("INTENSITY AND PLEASANTNESS: PAIRED WILCOXON TESTS\n")
cat("====================================================\n")

print(intensity_pleasantness_results)

print(intensity_wilcox)
print(pleasantness_wilcox)

# Automatic verification
assert_close(
  unname(intensity_wilcox$statistic),
  166,
  .01,
  "Intensity Wilcoxon V"
)

assert_true(
  intensity_wilcox$p.value < .001,
  "Pod is rated as more intense than water"
)

assert_close(
  unname(pleasantness_wilcox$statistic),
  585,
  .01,
  "Pleasantness Wilcoxon V"
)

assert_close(
  pleasantness_wilcox$p.value,
  .631,
  .002,
  "Pleasantness Wilcoxon p"
)

cat("\n====================================================\n")
cat("PASS: INTENSITY AND PLEASANTNESS RESULTS VERIFIED.\n")
cat("====================================================\n")


single_outcome_table <- bind_rows(
  ingestion_results %>%
    transmute(
      outcome = "Estimated amount consumed (ml)",
      descriptive_metric = "Mean (SD)",
      water_control = paste0(
        sprintf("%.2f", mean_water_control),
        " (", sprintf("%.2f", sd_water_control), ")"
      ),
      olfactory_pod = paste0(
        sprintf("%.2f", mean_olfactory_pod),
        " (", sprintf("%.2f", sd_olfactory_pod), ")"
      ),
      difference_P_minus_A = mean_difference_P_minus_A,
      test = "Paired t-test",
      statistic = t,
      df = df,
      p = p,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      shapiro_difference_p = shapiro_p,
      note = "Actual intake was fixed at 200 ml in both conditions"
    ),

  intensity_pleasantness_results %>%
    transmute(
      outcome,
      descriptive_metric = "Median",
      water_control = sprintf("%.2f", median_water_control),
      olfactory_pod = sprintf("%.2f", median_olfactory_pod),
      difference_P_minus_A = NA_real_,
      test = "Paired Wilcoxon signed-rank test",
      statistic = Wilcoxon_V,
      df = NA_real_,
      p = p,
      ci_lower = NA_real_,
      ci_upper = NA_real_,
      shapiro_difference_p = shapiro_difference_p,
      note = if_else(
        outcome == "Intensity",
        "Olfactory Pod rated as more intense",
        "No reliable difference in pleasantness"
      )
    )
)

write_table(
  single_outcome_table,
  "Table_S4_Pilot2_estimated_intake_and_perceptual_controls.csv"
)




# ----------------------------------------------------------------------------
# 13. OPEN-ENDED BINARY RESPONSES
# ----------------------------------------------------------------------------
open_by_condition <- dat %>%
  count(condition, binary_code, name = "n") %>%
  group_by(condition) %>%
  mutate(N = sum(n), percent = 100 * n / N) %>%
  ungroup()

open_paired <- condition_wide %>%
  mutate(
    changed_category = as.integer(binary_code_A != binary_code_P),
    pattern = case_when(
      binary_code_A == 0 & binary_code_P == 1 ~ "Water Control plain water -> Olfactory Pod attributed beverage identity",
      binary_code_A == 1 & binary_code_P == 1 ~ "Attributed beverage identity in both conditions",
      binary_code_A == 1 & binary_code_P == 0 ~ "Water Control attributed beverage identity -> Olfactory Pod plain water",
      binary_code_A == 0 & binary_code_P == 0 ~ "Plain water in both conditions",
      TRUE ~ "Unexpected/missing"
    )
  )

open_patterns <- open_paired %>%
  count(pattern, name = "n") %>%
  mutate(N = nrow(open_paired), percent = 100 * n / N)

open_matrix <- table(
  A = factor(open_paired$binary_code_A, levels = c(0, 1)),
  P = factor(open_paired$binary_code_P, levels = c(0, 1))
)

open_mcnemar_exact <- binom.test(
  x = min(open_matrix["0", "1"], open_matrix["1", "0"]),
  n = open_matrix["0", "1"] + open_matrix["1", "0"],
  p = .5,
  alternative = "two.sided"
)

water_plain_n <- open_by_condition %>%
  filter(condition == "A", binary_code == 0) %>% pull(n)
pod_plain_n <- open_by_condition %>%
  filter(condition == "P", binary_code == 0) %>% pull(n)
pod_nonwater_n <- open_by_condition %>%
  filter(condition == "P", binary_code == 1) %>% pull(n)
changed_n <- sum(open_paired$changed_category)
expected_shift_n <- sum(open_paired$binary_code_A == 0 & open_paired$binary_code_P == 1)
both_nonwater_n <- sum(open_paired$binary_code_A == 1 & open_paired$binary_code_P == 1)
both_water_n <- sum(open_paired$binary_code_A == 0 & open_paired$binary_code_P == 0)
reverse_shift_n <- sum(open_paired$binary_code_A == 1 & open_paired$binary_code_P == 0)

assert_true(water_plain_n == 31,
            "Water Control plain-water identifications = 31/47")
assert_true(pod_plain_n == 2,
            "Olfactory Pod plain-water identifications = 2/47")
assert_true(pod_nonwater_n == 45,
            "Olfactory Pod attributed beverage identities = 45/47")
assert_true(changed_n == 31,
            "Participants changing binary category = 31/47")
assert_true(expected_shift_n == 30,
            "Expected Water-Control-to-Pod shifts = 30/47")
assert_true(reverse_shift_n == 1,
            "Reverse Pod-to-Water-Control shift = 1/47")
assert_true(both_nonwater_n == 15 && both_water_n == 1,
            "Among 16 non-shifters: 15 attributed beverage identity in both and 1 plain water in both")
assert_close(
  open_mcnemar_exact$p.value,
  2.980232e-08,
  1e-10,
  "Exact McNemar p value"
)

assert_close(100 * water_plain_n / 47, 66.0, .1,
             "Water Control identified as plain water (%)")
assert_close(100 * pod_plain_n / 47, 4.3, .1,
             "Olfactory Pod identified as plain water (%)")
assert_close(100 * pod_nonwater_n / 47, 95.7, .1,
             "Olfactory Pod attributed beverage identity (%)")
assert_close(100 * changed_n / 47, 66.0, .1,
             "Changed binary category (%)")
assert_close(100 * expected_shift_n / 47, 63.8, .1,
             "Expected Water-Control-to-Pod shift (%)")

open_condition_records <- open_by_condition %>%
  mutate(
    condition_label = recode(
      as.character(condition),
      "A" = "Water Control",
      "P" = "Olfactory Pod"
    ),
    response_label = if_else(
      binary_code == 0,
      "Plain water",
      "Attributed beverage identity"
    )
  ) %>%
  transmute(
    record_type = "Condition distribution",
    condition = condition_label,
    pattern = response_label,
    row_category = NA_character_,
    column_category = NA_character_,
    n,
    N,
    percent,
    test = NA_character_,
    statistic = NA_real_,
    df = NA_real_,
    p = NA_real_,
    note = NA_character_
  )

open_pattern_records <- open_patterns %>%
  transmute(
    record_type = "Paired pattern",
    condition = NA_character_,
    pattern,
    row_category = NA_character_,
    column_category = NA_character_,
    n,
    N,
    percent,
    test = NA_character_,
    statistic = NA_real_,
    df = NA_real_,
    p = NA_real_,
    note = NA_character_
  )

open_matrix_records <- as.data.frame(open_matrix) %>%
  transmute(
    record_type = "Transition matrix",
    condition = NA_character_,
    pattern = NA_character_,
    row_category = if_else(
      A == "0",
      "Water Control: plain water",
      "Water Control: attributed beverage identity"
    ),
    column_category = if_else(
      P == "0",
      "Olfactory Pod: plain water",
      "Olfactory Pod: attributed beverage identity"
    ),
    n = as.numeric(Freq),
    N = 47,
    percent = 100 * n / N,
    test = NA_character_,
    statistic = NA_real_,
    df = NA_real_,
    p = NA_real_,
    note = NA_character_
  )

open_test_record <- tibble(
  record_type = "Statistical test",
  condition = NA_character_,
  pattern = NA_character_,
  row_category = NA_character_,
  column_category = NA_character_,
  n = expected_shift_n + reverse_shift_n,
  N = 47,
  percent = NA_real_,
  test = "Exact McNemar test",
  statistic = NA_real_,
  df = NA_real_,
  p = open_mcnemar_exact$p.value,
  note = paste0(
    "Expected shift = ", expected_shift_n,
    "; reverse shift = ", reverse_shift_n
  )
)

open_response_table <- bind_rows(
  open_condition_records,
  open_pattern_records,
  open_matrix_records,
  open_test_record
)

write_table(
  open_response_table,
  "Table_S6_Pilot2_open_responses.csv"
)

# ----------------------------------------------------------------------------
# 14. VERIFICATION SUMMARY
# ----------------------------------------------------------------------------

verification_summary <- tibble(
  claim = c(
    "N participants",
    "Rows equal 47 participants × 2 conditions",
    "Nutrition Condition × Property F",
    "Nutrition interaction numerator df",
    "Nutrition interaction denominator df",
    "Nutrition interaction p < .001",
    "All three nutritional contrasts pHolm < .001",
    "Minimum nutritional-index Pearson r",
    "Maximum nutritional-index Pearson r",
    "Standardized alpha",
    "Taste Condition × Taste F",
    "Taste interaction numerator df",
    "Taste interaction p < .001",
    "Sweetness, sourness and bitterness pFDR <= .011",
    "Saltiness pFDR approximately .900",
    "Composite taste-to-nutrition R-squared",
    "Sweetness predicts composite nutritional attribution",
    "Bitterness predicts composite nutritional attribution",
    "Sweetness predicts fat, calories and sugar",
    "Bitterness selectively predicts fat",
    "Fat-model bootstrap intervals exclude zero",
    "Mean estimated-consumption difference P minus A",
    "Estimated-consumption paired t",
    "Estimated-consumption p",
    "Intensity Wilcoxon V",
    "Intensity p < .001",
    "Pleasantness Wilcoxon V",
    "Pleasantness p approximately .631",
    "No global individual-difference association survives FDR",
    "Water Control plain-water identifications",
    "Olfactory Pod plain-water identifications",
    "Olfactory Pod attributed beverage identities",
    "Expected qualitative shifts",
    "Reverse qualitative shifts",
    "Exact McNemar p < .001"
  ),

  expected = c(
    "47",
    "94",
    "22.02",
    "2",
    "230",
    "TRUE",
    "TRUE",
    ".481",
    ".739",
    ".816",
    "82.25",
    "3",
    "TRUE",
    "TRUE",
    ".900",
    ".381",
    "TRUE",
    "TRUE",
    "TRUE",
    "TRUE",
    "TRUE",
    "-31.05 ml",
    "-2.27",
    ".028",
    "166",
    "TRUE",
    "585",
    ".631",
    "TRUE",
    "31/47",
    "2/47",
    "45/47",
    "30/47",
    "1/47",
    "TRUE"
  ),

  observed = c(
    as.character(n_distinct(dat$subject)),
    as.character(nrow(dat)),
    sprintf("%.4f", nutrition_interaction_row$F.value),
    as.character(nutrition_interaction_row$Df),
    as.character(df.residual(nutrition_interaction_lm)),
    as.character(nutrition_interaction_row$Pr..F. < .001),
    as.character(all(nutrition_contrasts$p_holm < .001)),
    sprintf("%.6f", min(pairwise_r)),
    sprintf("%.6f", max(pairwise_r)),
    sprintf("%.6f", standardized_alpha),
    sprintf("%.4f", taste_interaction_row$F.value),
    as.character(taste_interaction_row$Df),
    as.character(taste_interaction_row$Pr..F. < .001),
    as.character(all(sig_tastes$p_fdr <= .011)),
    sprintf("%.6f", salt_row$p_fdr),
    sprintf("%.6f", summary(composite_model)$r.squared),
    as.character(coef(summary(composite_model))["diff_sweet", "Pr(>|t|)"] < .001),
    as.character(coef(summary(composite_model))["diff_bitter", "Pr(>|t|)"] < .05),
    as.character(
      coef(summary(fat_model))["diff_sweet", "Pr(>|t|)"] < .05 &&
      coef(summary(kcal_model))["diff_sweet", "Pr(>|t|)"] < .01 &&
      coef(summary(sugar_model))["diff_sweet", "Pr(>|t|)"] < .001
    ),
    as.character(
      coef(summary(fat_model))["diff_bitter", "Pr(>|t|)"] < .05 &&
      coef(summary(kcal_model))["diff_bitter", "Pr(>|t|)"] >= .05 &&
      coef(summary(sugar_model))["diff_bitter", "Pr(>|t|)"] >= .05
    ),
    as.character(all(bootstrap_CI$CI_lower > 0 | bootstrap_CI$CI_upper < 0)),
    sprintf("%.6f", mean_difference),
    sprintf("%.6f", unname(ingestion_t$statistic)),
    sprintf("%.6f", ingestion_t$p.value),
    sprintf("%.6f", unname(intensity_wilcox$statistic)),
    as.character(intensity_wilcox$p.value < .001),
    sprintf("%.6f", unname(pleasantness_wilcox$statistic)),
    sprintf("%.6f", pleasantness_wilcox$p.value),
    as.character(all(all_moderator_correlations$p_fdr_global >= .05)),
    paste0(water_plain_n, "/47"),
    paste0(pod_plain_n, "/47"),
    paste0(pod_nonwater_n, "/47"),
    paste0(expected_shift_n, "/47"),
    paste0(reverse_shift_n, "/47"),
    as.character(open_mcnemar_exact$p.value < .001)
  ),

  pass = c(
    n_distinct(dat$subject) == 47,
    nrow(dat) == 94,
    abs(nutrition_interaction_row$F.value - 22.023) <= .02,
    nutrition_interaction_row$Df == 2,
    df.residual(nutrition_interaction_lm) == 230,
    nutrition_interaction_row$Pr..F. < .001,
    all(nutrition_contrasts$p_holm < .001),
    abs(min(pairwise_r) - 0.4807414) <= .0001,
    abs(max(pairwise_r) - 0.7394307) <= .0001,
    abs(standardized_alpha - 0.8163894) <= .0001,
    abs(taste_interaction_row$F.value - 82.254) <= .05,
    taste_interaction_row$Df == 3,
    taste_interaction_row$Pr..F. < .001,
    all(sig_tastes$p_fdr <= .011),
    abs(salt_row$p_fdr - .900) <= .002,
    abs(summary(composite_model)$r.squared - .381) <= .002,
    coef(summary(composite_model))["diff_sweet", "Pr(>|t|)"] < .001,
    coef(summary(composite_model))["diff_bitter", "Pr(>|t|)"] < .05,
    coef(summary(fat_model))["diff_sweet", "Pr(>|t|)"] < .05 &&
      coef(summary(kcal_model))["diff_sweet", "Pr(>|t|)"] < .01 &&
      coef(summary(sugar_model))["diff_sweet", "Pr(>|t|)"] < .001,
    coef(summary(fat_model))["diff_bitter", "Pr(>|t|)"] < .05 &&
      coef(summary(kcal_model))["diff_bitter", "Pr(>|t|)"] >= .05 &&
      coef(summary(sugar_model))["diff_bitter", "Pr(>|t|)"] >= .05,
    all(bootstrap_CI$CI_lower > 0 | bootstrap_CI$CI_upper < 0),
    abs(mean_difference - (-31.051)) <= .01,
    abs(unname(ingestion_t$statistic) - (-2.267)) <= .01,
    abs(ingestion_t$p.value - .028134) <= .001,
    abs(unname(intensity_wilcox$statistic) - 166) <= .01,
    intensity_wilcox$p.value < .001,
    abs(unname(pleasantness_wilcox$statistic) - 585) <= .01,
    abs(pleasantness_wilcox$p.value - .631) <= .002,
    all(all_moderator_correlations$p_fdr_global >= .05),
    water_plain_n == 31,
    pod_plain_n == 2,
    pod_nonwater_n == 45,
    expected_shift_n == 30,
    reverse_shift_n == 1,
    open_mcnemar_exact$p.value < .001
  )
) %>%
  mutate(
    status = if_else(pass, "PASS", "FAIL")
  )

write_table(
  verification_summary,
  "Pilot2_verification_summary.csv"
)

cat("\n============================================================\n")
cat("FINAL VERIFICATION SUMMARY\n")
cat("============================================================\n")
print(verification_summary)

if (!all(verification_summary$pass)) {
  failed_claims <- verification_summary %>%
    filter(!pass) %>%
    pull(claim)

  stop(
    paste0(
      "\nPILOT 2 VERIFICATION FAILED.\n",
      "Failed checks:\n",
      paste0("  - ", failed_claims, collapse = "\n"),
      "\nSee: ",
      file.path(tables_dir, "Pilot2_verification_summary.csv")
    ),
    call. = FALSE
  )
}

# ----------------------------------------------------------------------------
# 15. PLOTS
# ----------------------------------------------------------------------------
condition_labels <- c("A" = "Water Control", "P" = "Olfactory Pod")
condition_colors <- c("A" = "#4C78A8", "P" = "#E45756")

base_theme <- theme_classic(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = .5)
  )

p_nutrition <- ggplot(
  nutrition_long,
  aes(x = condition, y = nutritional_rating, group = subject)
) +
  geom_line(color = "grey75", linewidth = .3, alpha = .65) +
  geom_point(aes(color = condition), position = position_jitter(width = .035),
             size = 1.7, alpha = .8) +
  stat_summary(aes(group = condition), fun = mean, geom = "point",
               shape = 18, size = 3.4, color = "black") +
  facet_wrap(~ nutritional_property, nrow = 1) +
  scale_color_manual(values = condition_colors, labels = condition_labels) +
  scale_x_discrete(labels = condition_labels) +
  coord_cartesian(ylim = c(0, 110), clip = "off") +
  geom_text(
    data = nutrition_contrasts,
    aes(x = 1.5, y = 106, label = "***"),
    inherit.aes = FALSE,
    size = 5
  ) +
  labs(
    title = "A. Pod-induced nutritional attribution",
    subtitle = "All planned ART-C contrasts: pHolm < .001",
    x = NULL, y = "Rating (0-100)", color = NULL
  ) +
  base_theme

p_taste <- ggplot(
  taste_long,
  aes(x = condition, y = taste_rating, group = subject)
) +
  geom_line(color = "grey78", linewidth = .28, alpha = .6) +
  geom_point(aes(color = condition), position = position_jitter(width = .035),
             size = 1.5, alpha = .75) +
  stat_summary(aes(group = condition), fun = mean, geom = "point",
               shape = 18, size = 3.2, color = "black") +
  facet_wrap(~ taste, nrow = 1) +
  scale_color_manual(values = condition_colors, labels = condition_labels) +
  scale_x_discrete(labels = condition_labels) +
  coord_cartesian(ylim = c(0, 110), clip = "off") +
  geom_text(
    data = taste_contrasts,
    aes(x = 1.5, y = 106, label = significance),
    inherit.aes = FALSE,
    size = 4.5
  ) +
  labs(
    title = "B. Pod-induced taste profile",
    subtitle = "FDR-corrected ART-C comparisons",
    x = NULL, y = "Rating (0-100)", color = NULL
  ) +
  base_theme

p_ingestion <- condition_wide %>%
  select(subject, Water = ingested_ml_estimate_A, Pod = ingested_ml_estimate_P) %>%
  pivot_longer(c(Water, Pod), names_to = "condition_plot", values_to = "estimated_ml") %>%
  mutate(condition_plot = factor(condition_plot, levels = c("Water", "Pod"))) %>%
  ggplot(aes(x = condition_plot, y = estimated_ml, group = subject)) +
  #geom_hline(yintercept = actual_volume_ml, linetype = "dashed", color = "grey45") +
  geom_line(color = "grey72", linewidth = .35, alpha = .7) +
  geom_point(aes(color = condition_plot), size = 1.8, alpha = .8) +
  stat_summary(aes(group = condition_plot), fun = mean, geom = "point",
               shape = 18, size = 3.5, color = "black") +
  scale_color_manual(values = c("Water" = unname(condition_colors["A"]), "Pod" = unname(condition_colors["P"]))) +
  annotate("text", x = 1.5, y = max(condition_wide$ingested_ml_estimate_A,
                                     condition_wide$ingested_ml_estimate_P) + 25,
           label = "t(46) = -2.27, p = .028", size = 4) +
  labs(
    title = "C. Estimated amount consumed",
    subtitle = "Actual intake was fixed at 200 ml",
    x = NULL, y = "Estimated amount (ml)", color = NULL
  ) +
  coord_cartesian(clip = "off") +
  base_theme

intensity_pleasantness_long <- dat %>%
  select(subject, condition, intensity, pleasantness) %>%
  pivot_longer(c(intensity, pleasantness), names_to = "outcome", values_to = "rating") %>%
  mutate(outcome = factor(outcome, levels = c("intensity", "pleasantness"),
                          labels = c("Intensity", "Pleasantness")))

ip_annotations <- tibble(
  outcome = factor(c("Intensity", "Pleasantness"), levels = c("Intensity", "Pleasantness")),
  label = c("***", "ns"), y = c(106, 106)
)

p_ip <- ggplot(
  intensity_pleasantness_long,
  aes(x = condition, y = rating, group = subject)
) +
  geom_line(color = "grey75", linewidth = .3, alpha = .65) +
  geom_point(aes(color = condition), position = position_jitter(width = .035),
             size = 1.7, alpha = .8) +
  stat_summary(aes(group = condition), fun = mean, geom = "point",
               shape = 18, size = 3.4, color = "black") +
  facet_wrap(~ outcome, nrow = 1) +
  scale_color_manual(values = condition_colors, labels = condition_labels) +
  scale_x_discrete(labels = condition_labels) +
  coord_cartesian(ylim = c(0, 110), clip = "off") +
  geom_text(data = ip_annotations,
            aes(x = 1.5, y = y, label = label), inherit.aes = FALSE, size = 5) +
  labs(
    title = "D. Intensity and pleasantness",
    subtitle = "Paired Wilcoxon signed-rank tests",
    x = NULL, y = "Rating (0-100)", color = NULL
  ) +
  base_theme

open_plot_data <- open_by_condition %>%
  mutate(
    condition = factor(condition, levels = c("A", "P"), labels = c("Water Control", "Olfactory Pod")),
    response = factor(binary_code, levels = c(0, 1),
                      labels = c("Plain water", "Attributed beverage identity"))
  )

p_open <- ggplot(open_plot_data, aes(x = condition, y = percent, fill = response)) +
  geom_col(width = .68, color = "white") +
  geom_text(
    aes(label = paste0(n, "/", N, "\n(", sprintf("%.1f", percent), "%)")),
    position = position_stack(vjust = .5), size = 4
  ) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0, .02))) +
  labs(
    title = "E. Open-ended beverage identity",
    subtitle = "Binary coding of plain water versus attributed beverage identity",
    x = NULL, y = "Participants (%)", fill = NULL
  ) +
  base_theme

main_figure <- (p_nutrition / p_taste) / (p_ingestion | p_ip | p_open) +
  patchwork::plot_annotation(
    title = "Pilot 2: Validation of the Olfactory Pod versus Water Control contrast",
    caption = "*** p < .001; ** p < .01; * p < .05; ns = non-significant. Nutritional and taste analyses use ART/ART-C."
  )

ggsave(
  file.path(figures_dir, "Pilot2_main_results_panels.png"),
  main_figure, width = 16, height = 15, dpi = 300, bg = "white"
)
ggsave(
  file.path(figures_dir, "Pilot2_main_results_panels.pdf"),
  main_figure, width = 16, height = 15, bg = "white"
)

# Exploratory relationship plots
p_reg_sweet <- ggplot(condition_wide, aes(diff_sweet, nutritional_composite_z)) +
  geom_point(size = 2, alpha = .8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Sweetness and nutritional attribution",
       subtitle = "Full-model coefficient: b = .019, p < .001",
       x = "Pod - Water sweetness", y = "Nutritional composite (z)") +
  base_theme + theme(legend.position = "none")

p_reg_bitter <- ggplot(condition_wide, aes(diff_bitter, nutritional_composite_z)) +
  geom_point(size = 2, alpha = .8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Bitterness and nutritional attribution",
       subtitle = "Full-model coefficient: b = .030, p = .031",
       x = "Pod - Water bitterness", y = "Nutritional composite (z)") +
  base_theme + theme(legend.position = "none")

p_reg_fat_bitter <- ggplot(condition_wide, aes(diff_bitter, diff_fat)) +
  geom_point(size = 2, alpha = .8) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Selective bitterness-fat association",
       subtitle = "Fat-model coefficient: b = .637, p = .011",
       x = "Pod - Water bitterness", y = "Pod - Water perceived fat") +
  base_theme + theme(legend.position = "none")

exploratory_figure <- (p_reg_sweet | p_reg_bitter | p_reg_fat_bitter) +
  patchwork::plot_annotation(
    title = "Pilot 2: Exploratory taste–nutrition associations",
    caption = paste(
      "Points and lines with 95% confidence bands show descriptive bivariate associations.",
      "Reported coefficients derive from multiple regression models including",
      "pod-related changes in sweetness, saltiness, sourness and bitterness."
    ),
    tag_levels = "A",
    theme = theme(
      plot.title = element_text(face = "bold", size = 15),
      plot.caption = element_text(size = 9.5, hjust = 0),
      plot.tag = element_text(face = "bold", size = 14)
    )
  )

ggsave(
  file.path(figures_dir, "Pilot2_exploratory_regressions.png"),
  exploratory_figure, width = 14, height = 5, dpi = 300, bg = "white"
)

ggsave(
  file.path(figures_dir, "Pilot2_exploratory_regressions.pdf"),
  exploratory_figure, width = 14, height = 5, bg = "white"
)

# ----------------------------------------------------------------------------
# 16. SESSION INFORMATION AND FINAL MESSAGE
# ----------------------------------------------------------------------------
writeLines(
  capture.output(sessionInfo()),
  file.path(logs_dir, "Pilot2_sessionInfo.txt")
)

cat("\n============================================================\n")
cat("ALL CENTRAL PILOT 2 RESULTS WERE REPRODUCED.\n")
cat("============================================================\n")
cat("Tables saved in:\n", tables_dir, "\n\n")
cat("Figures saved in:\n", figures_dir, "\n\n")
cat("Session information saved in:\n", logs_dir, "\n")
cat("============================================================\n")
