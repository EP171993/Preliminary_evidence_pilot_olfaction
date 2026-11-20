# =============================================================================
# PILOTA 1 - ANALISI COMPLETA (PASSO PASSO)
# Pod pesca (Peach smoothie) vs pod cioccolato-arancia (Chocolate-orange), N = 28
#
# Ogni partecipante ha bevuto 200 ml d'acqua con ciascuno dei due pod.
# Una riga del file Excel = un partecipante con un pod (28 x 2 = 56 righe).
#
# Lo script non apre finestre. Legge data/pilot_1_data_.xlsx, esegue tutte
# le analisi del Pilot 1 riportate nel Supplementary e salva i risultati in:
#   outputs/Pilot1/tables   (Table_S1 ... Table_S5, Pilot1_verification_summary.csv)
#   outputs/Pilot1/figures  (Figure S1 e S2)
#   outputs/Pilot1/logs     (Pilot1_sessionInfo.txt)
# nella cartella del repository (quella che contiene "data" e "outputs").
#
# Pilot1_verification_summary.csv confronta ogni risultato riportato nel
# Supplementary con quello ottenuto: se un numero non torna, la riga dice FAIL.
# Ogni sezione indica a quale figura del Supplementary e a quale slide del
# deck "Pilot 1 e Pilot 2 - Supplementary results" si riferisce.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. IMPOSTAZIONI - L'UNICA PARTE DA MODIFICARE
# -----------------------------------------------------------------------------
# Percorso completo del file Excel (usare le barre "/").
data_file <- "C:/Users/Eleonora/OneDrive/MyExperiments/sapienza/airup/bial 2026/analisi/repository 19-11/data/pilot_1_data_.xlsx"

# Se questo percorso non esiste (ad esempio su un altro computer, dopo aver
# scaricato il repository da GitHub), lo script cerca il file nella cartella
# "data" del repository: accanto alla cartella dello script, oppure partendo
# dalla cartella di lavoro di R.
if (!file.exists(data_file)) {
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (is.null(script_path)) {
    script_path <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1])
  }
  search_folders <- c(".", "..")
  if (!is.na(script_path)) search_folders <- c(dirname(dirname(script_path)), dirname(script_path), search_folders)
  candidate_files <- file.path(search_folders, "data", basename(data_file))
  candidate_files <- candidate_files[file.exists(candidate_files)]
  if (length(candidate_files) == 0) {
    stop("File Excel non trovato. Scrivi il percorso completo in 'data_file' (sezione 0).",
         call. = FALSE)
  }
  data_file <- normalizePath(candidate_files[1], winslash = "/")
}

# Cartella del repository: quella che contiene le cartelle "data" e "outputs".
# (Il file Excel sta in <repository>/data, quindi e' la cartella sopra.)
repository_folder <- dirname(dirname(data_file))


# -----------------------------------------------------------------------------
# 1. PACCHETTI E FUNZIONI DI SERVIZIO
# -----------------------------------------------------------------------------
options(repos = c(CRAN = "https://cloud.r-project.org"))   # niente finestra del mirror

packages <- c("readxl", "dplyr", "tidyr", "tibble", "ggplot2", "patchwork")
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
  stop("Pacchetti installati: riavvia R (Session > Restart R) e lancia di nuovo lo script.",
       call. = FALSE)
}

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(patchwork)

# Alcuni pacchetti (ad esempio "car" o "MASS"), se gia' caricati nella stessa
# sessione di R, hanno funzioni con lo stesso nome di quelle di dplyr
# (recode, select, filter). Qui si usano esplicitamente quelle di dplyr,
# cosi' lo script funziona anche senza riavviare R.
select <- dplyr::select
filter <- dplyr::filter

set.seed(2026)

# Cartelle dei risultati
tables_folder  <- file.path(repository_folder, "outputs", "Pilot1", "tables")
figures_folder <- file.path(repository_folder, "outputs", "Pilot1", "figures")
logs_folder    <- file.path(repository_folder, "outputs", "Pilot1", "logs")
dir.create(tables_folder,  recursive = TRUE, showWarnings = FALSE)
dir.create(figures_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(logs_folder,    recursive = TRUE, showWarnings = FALSE)

save_table <- function(x, file_name) {
  write.csv(x, file.path(tables_folder, file_name), row.names = FALSE, na = "")
}

# p in formato APA: "< .001" oppure ".038"
format_p <- function(p) {
  ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
}

significance_label <- function(p) {
  case_when(p < .001 ~ "***", p < .01 ~ "**", p < .05 ~ "*", TRUE ~ "ns")
}

# print_sig() stampa una tabella con 3 cifre significative e aggiunge,
# accanto a ogni colonna di p, una colonna con gli asterischi:
#   *** p < .001   ** p < .01   * p < .05   ns = non significativo
print_sig <- function(table) {
  table <- as.data.frame(table)
  p_columns <- grep("^p$|^p_fdr$|^p\\.value$", names(table), value = TRUE)
  for (column in rev(p_columns)) {
    stars <- as.character(cut(table[[column]], breaks = c(-Inf, .001, .01, .05, Inf),
                              labels = c("***", "**", "*", "ns"), right = FALSE))
    stars[is.na(stars)] <- ""
    star_name <- if (length(p_columns) == 1) "sig" else paste0("sig_", column)
    position <- match(column, names(table))
    table <- cbind(table[seq_len(position)], setNames(data.frame(stars), star_name),
                   table[-seq_len(position)])
  }
  print(table, digits = 3)
  invisible(table)
}

section <- function(title) {
  cat("\n=============================================================\n")
  cat(title, "\n")
  cat("=============================================================\n")
}

cat("\nLegenda: *** p < .001   ** p < .01   * p < .05   ns = non significativo\n")


# -----------------------------------------------------------------------------
# 2. IMPORTA DATI
# -----------------------------------------------------------------------------
if (!file.exists(data_file)) {
  stop("File non trovato. Controlla il percorso in 'data_file' (sezione 0).")
}

# Il file viene letto da una copia temporanea: funziona anche se e' aperto in
# Excel o sincronizzato da OneDrive.
temporary_copy <- tempfile(fileext = ".xlsx")
file.copy(data_file, temporary_copy, overwrite = TRUE)
raw <- read_excel(temporary_copy, sheet = "Foglio1")

p1 <- raw %>%
  transmute(
    subject = as.character(subject),
    cue     = factor(cue, levels = c("Peach smoothie", "Chocolate-orange")),
    # valutazioni VAS 0-100
    intensity    = as.numeric(intensity),
    pleasantness = as.numeric(pleasantness),
    calories     = as.numeric(calories_rating),
    sugar        = as.numeric(sugar_rating),
    fat          = as.numeric(fat_rating),
    # risposta aperta sull'identita' della bevanda, codificata:
    #   0 = acqua semplice
    #   1 = bevanda aromatizzata, infusa o poco calorica
    #   2 = bevanda calorica o con nutrienti
    #   vuoto/altro = non classificabile (identita' ambigua o non una bevanda)
    open_code = suppressWarnings(as.integer(as.character(recode_0_1_2)))
  )

cat("\nPartecipanti:", n_distinct(p1$subject), "| righe:", nrow(p1), "\n")

# Controlli essenziali: 28 partecipanti, ognuno con una riga per pod,
# valutazioni tra 0 e 100
rating_values <- unlist(p1[c("intensity", "pleasantness", "calories", "sugar", "fat")])
stopifnot(
  nrow(p1) == 56,
  n_distinct(p1$subject) == 28,
  all(table(p1$subject, p1$cue) == 1),
  all(is.na(rating_values) | (rating_values >= 0 & rating_values <= 100))
)


# -----------------------------------------------------------------------------
# 3. FORMATO LUNGO: una riga per partecipante x pod x misura (28 x 2 x 5 = 280)
# -----------------------------------------------------------------------------
measure_labels <- c(intensity = "Intensity", pleasantness = "Pleasantness",
                    calories = "Calories", sugar = "Sugar", fat = "Fat")

# (L'ordine delle colonne decide dove cadono i punti "sparpagliati" nelle
#  figure: non cambiarlo, altrimenti le figure cambiano leggermente.)
ratings_long <- p1 %>%
  select(subject, cue, pleasantness, calories, intensity, sugar, fat) %>%
  pivot_longer(c(pleasantness, calories, intensity, sugar, fat),
               names_to = "measure", values_to = "rating") %>%
  mutate(measure = factor(measure_labels[measure], levels = measure_labels),
         xpos = if_else(cue == "Peach smoothie", 1, 2)) %>%
  filter(!is.na(rating))

# Formato largo per i test appaiati: una colonna per pod
ratings_wide <- ratings_long %>%
  select(subject, measure, cue, rating) %>%
  pivot_wider(names_from = cue, values_from = rating)


# -----------------------------------------------------------------------------
# 4. DESCRITTIVE  (SLIDE 5: media e DS)
# -----------------------------------------------------------------------------
section("4. DESCRITTIVE")

descriptives <- ratings_long %>%
  group_by(measure, cue) %>%
  summarise(
    n = n(), mean = mean(rating), sd = sd(rating), median = median(rating),
    q1 = unname(quantile(rating, .25)), q3 = unname(quantile(rating, .75)),
    minimum = min(rating), maximum = max(rating),
    .groups = "drop"
  )
print(descriptives, n = Inf)
# Calorie 13.0 (17.3) pesca, 16.2 (18.6) cioccolato; zuccheri 16.6 (20.3), 15.4 (17.7);
# grassi 7.2 (12.5), 9.8 (14.3)


# -----------------------------------------------------------------------------
# 5. PESCA VS CIOCCOLATO-ARANCIA  (SLIDE 3 e 4, Figura S1A-B)
# -----------------------------------------------------------------------------
# Test: Wilcoxon appaiato (pesca vs cioccolato) per ognuna delle 5 misure
#       (intensita', piacevolezza, calorie, zuccheri, grassi).
# Correzione: Benjamini-Hochberg (FDR) sui 5 test insieme.
section("5. WILCOXON APPAIATO PESCA VS CIOCCOLATO (FDR sui 5)")

paired_tests <- bind_rows(lapply(levels(ratings_wide$measure), function(m) {
  d  <- ratings_wide %>% filter(measure == m)
  ok <- complete.cases(d$`Peach smoothie`, d$`Chocolate-orange`)
  test <- wilcox.test(d$`Peach smoothie`[ok], d$`Chocolate-orange`[ok],
                      paired = TRUE, exact = FALSE)
  tibble(measure = m, n_pairs = sum(ok), V = unname(test$statistic), p = test$p.value)
})) %>%
  mutate(p_fdr = p.adjust(p, method = "BH"),
         significance = significance_label(p_fdr),
         plot_label = paste0(significance, " (", if_else(p_fdr < .001, "pFDR < .001",
                                                          paste0("pFDR = ", format_p(p_fdr))), ")"))
print_sig(paired_tests %>% select(measure, n_pairs, V, p, p_fdr))
# Slide 3: intensita' e piacevolezza pFDR = .563
# Slide 4: calorie .277, zuccheri .563, grassi .277


# -----------------------------------------------------------------------------
# 6. VALUTAZIONI NUTRIZIONALI CONTRO ZERO  (SLIDE 5, Figura S1C)
# -----------------------------------------------------------------------------
# Domanda: i pod fanno attribuire calorie, zuccheri e grassi all'acqua?
# Test: Wilcoxon a un campione contro 0, per ogni pod e proprieta'
#       (2 x 3 = 6 test). Correzione FDR sui 6.
# Attenzione: senza una condizione senza pod non si puo' separare l'effetto
# del pod da come viene valutata l'acqua di per se' (vedi Pilota 2).
section("6. WILCOXON CONTRO 0 (FDR sui 6)")

one_sample_tests <- ratings_long %>%
  filter(measure %in% c("Calories", "Sugar", "Fat")) %>%
  group_by(measure, cue) %>%
  summarise(
    n = n(),
    V = unname(wilcox.test(rating, mu = 0, exact = FALSE)$statistic),
    p = wilcox.test(rating, mu = 0, exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(p_fdr = p.adjust(p, method = "BH"),
         significance = significance_label(p_fdr),
         xpos = if_else(cue == "Peach smoothie", 1, 2))
print_sig(one_sample_tests %>% select(measure, cue, n, V, p, p_fdr))   # tutti pFDR < .001


# -----------------------------------------------------------------------------
# 7. RISPOSTE APERTE: CONTEGGI  (SLIDE 6, Figura S2)
# -----------------------------------------------------------------------------
# Categorie assegnate dalla domanda sull'identita' della bevanda; la domanda
# sugli ingredienti serviva solo come supporto qualitativo.
section("7. RISPOSTE APERTE: CONTEGGI")

category_levels <- c("Plain water", "Flavoured/infused or low-calorie", "Caloric/nutrient-related")

classifiable <- p1 %>%
  filter(open_code %in% c(0, 1, 2)) %>%
  mutate(category = factor(open_code, levels = 0:2, labels = category_levels))
n_classifiable <- nrow(classifiable)
cat("Risposte classificabili:", n_classifiable, "su", nrow(p1),
    "(non classificabili:", nrow(p1) - n_classifiable, ")\n")   # 52 su 56

# Riassunto usato sulla slide 6
open_summary <- tibble(
  outcome = c("Plain water (0)", "Beverage-like identity (1 + 2)", "Caloric or nutrient-related (2)"),
  n = c(sum(classifiable$open_code == 0),
        sum(classifiable$open_code %in% c(1, 2)),
        sum(classifiable$open_code == 2)),
  of = n_classifiable,
  percent = 100 * n / n_classifiable
)
print(open_summary)   # 4/52 (7.7%), 48/52 (92.3%), 22/52 (42.3%)

# Categorie per pod e su entrambi i pod insieme
open_by_cue <- bind_rows(
  classifiable %>% transmute(group = as.character(cue), category),
  classifiable %>% transmute(group = "Across cues", category)
) %>%
  count(group, category, name = "n") %>%
  complete(group = c("Peach smoothie", "Chocolate-orange", "Across cues"),
           category = factor(category_levels, levels = category_levels),
           fill = list(n = 0)) %>%
  group_by(group) %>%
  mutate(denominator = sum(n), percent = 100 * n / denominator) %>%
  ungroup() %>%
  mutate(group = factor(group, levels = c("Peach smoothie", "Chocolate-orange", "Across cues")))
print(open_by_cue, n = Inf)


# -----------------------------------------------------------------------------
# 8. RISPOSTE APERTE: CONFRONTO TRA I POD  (SLIDE 6)
# -----------------------------------------------------------------------------
# Coppie: partecipanti con risposta classificabile per entrambi i pod.
section("8. RISPOSTE APERTE: BOWKER E McNEMAR ESATTO")

open_pairs <- p1 %>%
  select(subject, cue, open_code) %>%
  pivot_wider(names_from = cue, values_from = open_code) %>%
  filter(`Peach smoothie` %in% 0:2, `Chocolate-orange` %in% 0:2)
cat("Coppie complete:", nrow(open_pairs), "\n")   # 24

# Tabella 3 x 3: categoria con la pesca (righe) x categoria con il cioccolato (colonne)
transition_matrix <- table(Peach     = factor(open_pairs$`Peach smoothie`, levels = 0:2),
                           Chocolate = factor(open_pairs$`Chocolate-orange`, levels = 0:2))
print(transition_matrix)

# 8a. Test di Bowker (simmetria): gli spostamenti tra due categorie sono
#     ugualmente frequenti nelle due direzioni?
#     Per ogni coppia di categorie (i, j): (n_ij - n_ji)^2 / (n_ij + n_ji); poi si somma.
#     Gradi di liberta' = numero di coppie di categorie con almeno uno spostamento.
bowker_chi2 <- 0
bowker_df   <- 0
for (i in 1:2) {
  for (j in (i + 1):3) {
    n_ij <- transition_matrix[i, j]
    n_ji <- transition_matrix[j, i]
    if (n_ij + n_ji > 0) {
      bowker_chi2 <- bowker_chi2 + (n_ij - n_ji)^2 / (n_ij + n_ji)
      bowker_df   <- bowker_df + 1
    }
  }
}
bowker_p <- pchisq(bowker_chi2, df = bowker_df, lower.tail = FALSE)
cat(sprintf("Bowker: chi2(%d) = %.2f, p = %s\n", bowker_df, bowker_chi2, format_p(bowker_p)))   # chi2(3) = 4.67, p = .198

# 8b. McNemar esatto: calorica (2) vs non calorica (0 o 1).
#     Si usano solo i partecipanti che cambiano categoria tra i due pod
#     (test binomiale contro 50/50).
binary_matrix <- table(Peach     = factor(if_else(open_pairs$`Peach smoothie` == 2, 1, 0), levels = 0:1),
                       Chocolate = factor(if_else(open_pairs$`Chocolate-orange` == 2, 1, 0), levels = 0:1))
print(binary_matrix)
noncaloric_to_caloric <- binary_matrix["0", "1"]   # non calorica con pesca -> calorica con cioccolato
caloric_to_noncaloric <- binary_matrix["1", "0"]
mcnemar_exact <- binom.test(min(noncaloric_to_caloric, caloric_to_noncaloric),
                            noncaloric_to_caloric + caloric_to_noncaloric, p = .5)
cat(sprintf("McNemar esatto: %d vs %d, p = %s\n", noncaloric_to_caloric,
            caloric_to_noncaloric, format_p(mcnemar_exact$p.value)))   # 6 vs 1, p = .125

open_tests <- tibble(
  test = c("Bowker test of symmetry", "Exact McNemar (caloric vs non-caloric)"),
  n = c(nrow(open_pairs), noncaloric_to_caloric + caloric_to_noncaloric),
  statistic = c(bowker_chi2, NA), df = c(bowker_df, NA),
  p = c(bowker_p, mcnemar_exact$p.value),
  note = c("complete pairs",
           paste0(noncaloric_to_caloric, " non-caloric -> caloric; ",
                  caloric_to_noncaloric, " caloric -> non-caloric"))
)
print_sig(open_tests)


# -----------------------------------------------------------------------------
# 9. FIGURE
# -----------------------------------------------------------------------------
pod_colors <- c("Peach smoothie" = "#F2B544", "Chocolate-orange" = "#A9643A")
jitter_position <- position_jitter(width = 0.035, height = 0, seed = 2026)

common_theme <- theme_classic(base_size = 13) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 13),
        axis.title.y = element_text(size = 12),
        axis.text.y = element_text(size = 10),
        panel.spacing = grid::unit(1.2, "lines"),
        plot.margin = margin(t = 10, r = 15, b = 10, l = 10))

# Funzione per un pannello: violino, box, punti, linee dei partecipanti
ratings_panel <- function(data, title, show_lines = TRUE, show_zero = FALSE,
                          zero_width = 0.4, zero_color = "grey45") {
  p <- ggplot(data, aes(x = xpos, y = rating)) +
    geom_violin(aes(group = cue, fill = cue), width = 0.72, trim = TRUE, alpha = 0.28, color = NA)
  if (show_lines) {
    p <- p + geom_line(aes(group = subject), color = "grey70", linewidth = 0.35, alpha = 0.70)
  }
  p <- p +
    geom_boxplot(aes(group = cue, fill = cue), width = 0.14, alpha = 0.48,
                 outlier.shape = NA, color = "black", linewidth = 0.45) +
    geom_point(aes(color = cue), position = jitter_position, size = 1.9, alpha = 0.85) +
    stat_summary(aes(group = cue), fun = mean, geom = "point", shape = 18, size = 3.5, color = "black")
  if (show_zero) {
    p <- p + geom_hline(yintercept = 0, linetype = "dashed", linewidth = zero_width, color = zero_color)
  }
  p +
    facet_wrap(~ measure, nrow = 1) +
    scale_x_continuous(breaks = c(1, 2), labels = c("Peach smoothie", "Chocolate-orange"),
                       limits = c(0.55, 2.45)) +
    scale_y_continuous(breaks = seq(0, 100, 25)) +
    scale_fill_manual(values = pod_colors) +
    scale_color_manual(values = pod_colors) +
    coord_cartesian(ylim = c(0, 116), clip = "off") +
    labs(title = title, x = NULL, y = "VAS rating (0\u2013100)") +
    guides(fill = "none", color = guide_legend(title = NULL, override.aes = list(size = 3, alpha = 1))) +
    common_theme
}

# Parentesi con il pFDR del confronto tra i pod (pannelli A e B)
add_brackets <- function(p, annotations) {
  p +
    geom_segment(data = annotations, aes(x = 1, xend = 2, y = 105, yend = 105), inherit.aes = FALSE, linewidth = 0.5) +
    geom_segment(data = annotations, aes(x = 1, xend = 1, y = 102, yend = 105), inherit.aes = FALSE, linewidth = 0.5) +
    geom_segment(data = annotations, aes(x = 2, xend = 2, y = 102, yend = 105), inherit.aes = FALSE, linewidth = 0.5) +
    geom_text(data = annotations, aes(x = 1.5, y = 110, label = plot_label), inherit.aes = FALSE, size = 3.8)
}

perceptual_data  <- ratings_long %>% filter(measure %in% c("Intensity", "Pleasantness")) %>% droplevels()
nutritional_data <- ratings_long %>% filter(measure %in% c("Calories", "Sugar", "Fat")) %>% droplevels()
annotations_A <- paired_tests %>% filter(measure %in% c("Intensity", "Pleasantness")) %>%
  mutate(measure = factor(measure, levels = levels(perceptual_data$measure)))
annotations_B <- paired_tests %>% filter(measure %in% c("Calories", "Sugar", "Fat")) %>%
  mutate(measure = factor(measure, levels = levels(nutritional_data$measure)))
annotations_C <- one_sample_tests %>%
  group_by(measure) %>%
  summarise(label = if_else(all(p_fdr < .001), "Both pods > 0: pFDR < .001", "One-sample Wilcoxon vs 0"),
            .groups = "drop")

# 9a. Figura S1: pannelli A (slide 3), B (slide 4), C (slide 5)
pA <- add_brackets(ratings_panel(perceptual_data, "Perceptual ratings did not differ between pods"),
                   annotations_A) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
pB <- add_brackets(ratings_panel(nutritional_data, "Nutritional ratings were comparable across pods",
                                 show_zero = TRUE), annotations_B) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
pC <- ratings_panel(nutritional_data, "Nutritional ratings were reliably above zero",
                    show_lines = FALSE, show_zero = TRUE, zero_width = 0.5, zero_color = "grey40") +
  geom_text(data = one_sample_tests, aes(x = xpos, y = 102, label = significance),
            inherit.aes = FALSE, fontface = "bold", size = 5) +
  geom_text(data = annotations_C, aes(x = 1.5, y = 112, label = label), inherit.aes = FALSE, size = 3.7) +
  theme(axis.text.x = element_text(size = 10, face = "bold"))

ratings_figure <- (pA / pB / pC) +
  plot_layout(heights = c(0.95, 1.10, 1.10)) +
  plot_annotation(
    title = "Pilot 1: Both pods elicited comparable nutritional associations",
    subtitle = "Paired comparisons between pods and one-sample Wilcoxon tests against zero",
    caption = "*** pFDR < .001; ns = non-significant after FDR correction",
    tag_levels = "A",
    theme = theme(plot.title = element_text(face = "bold", size = 19),
                  plot.subtitle = element_text(size = 12),
                  plot.caption = element_text(size = 10, hjust = 0),
                  plot.tag = element_text(face = "bold", size = 17))
  ) &
  theme(legend.position = "bottom", legend.direction = "horizontal",
        legend.justification = "center", legend.box.just = "center",
        legend.key = element_blank(), legend.text = element_text(size = 11),
        legend.margin = margin(t = 6, r = 0, b = 2, l = 0))
ggsave(file.path(figures_folder, "Pilot1_combined_panels.png"), ratings_figure,
       width = 15, height = 17, dpi = 300, bg = "white")
ggsave(file.path(figures_folder, "Pilot1_combined_panels.pdf"), ratings_figure,
       width = 15, height = 17, bg = "white")

# 9b. Figura S2: risposte aperte (slide 6)
response_colors <- c("Plain water" = "#F8766D", "Flavoured/infused or low-calorie" = "#00BA38",
                     "Caloric/nutrient-related" = "#619CFF")
# Righe in ordine alfabetico (cambia solo l'ordine in cui vengono disegnati
# i bordi delle barre).
open_plot_data <- open_by_cue %>% arrange(as.character(group), as.character(category))
open_figure <- ggplot(open_plot_data, aes(x = group, y = percent, fill = category)) +
  geom_col(width = 0.67, color = "white", linewidth = 0.7) +
  geom_text(aes(label = if_else(n > 0, paste0(n, "\n(", sprintf("%.1f", percent), "%)"), "")),
            position = position_stack(vjust = 0.5), size = 4.3, lineheight = 1.05) +
  annotate("text", x = 3, y = 106,
           label = paste0("Beverage-like: ", open_summary$n[2], "/", n_classifiable,
                          " (", sprintf("%.1f", open_summary$percent[2]), "%)"),
           fontface = "bold", size = 4.2) +
  scale_fill_manual(values = response_colors, breaks = category_levels) +
  scale_y_continuous(breaks = seq(0, 100, 20), expand = expansion(mult = c(0, 0))) +
  coord_cartesian(ylim = c(0, 110), clip = "off") +
  labs(title = "Pilot 1: Open-ended beverage identity",
       subtitle = "Responses by olfactory cue and aggregated across cues; N/A responses were excluded",
       x = NULL, y = "Classifiable responses (%)", fill = "Response category",
       caption = paste0("Across cues: plain water = 4/52 (7.7%); beverage-like identity = 48/52 (92.3%); ",
                        "caloric/nutrient-related = 22/52 (42.3%).\n",
                        "Paired analyses: Bowker chi-square(3) = ", sprintf("%.2f", bowker_chi2),
                        ", p = ", format_p(bowker_p), "; exact McNemar p = ",
                        format_p(mcnemar_exact$p.value), ".")) +
  theme_classic(base_size = 14) +
  theme(plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 12, margin = margin(b = 14)),
        plot.caption = element_text(hjust = 0, size = 10.5, lineheight = 1.15, margin = margin(t = 12)),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(face = "bold", size = 12),
        axis.text.y = element_text(size = 11),
        legend.position = "bottom",
        legend.title = element_text(size = 11),
        legend.text = element_text(size = 10.5),
        plot.margin = margin(t = 18, r = 20, b = 10, l = 10))
ggsave(file.path(figures_folder, "Pilot1_open_responses_with_across_cues.png"), open_figure,
       width = 11, height = 8, dpi = 300, bg = "white")
ggsave(file.path(figures_folder, "Pilot1_open_responses_with_across_cues.pdf"), open_figure,
       width = 11, height = 8, bg = "white")


# -----------------------------------------------------------------------------
# 10. CONTROLLO FINALE: i numeri principali delle slide
# -----------------------------------------------------------------------------
# Se una riga dice "NON TORNA", qualcosa e' cambiato (dati o script).
section("10. CONTROLLO FINALE")

final_check <- tibble(
  slide = c("3", "3", "4", "4", "4", "5", "5", "5", "6", "6", "6", "6"),
  result = c("Intensity pFDR", "Pleasantness pFDR", "Calories pFDR", "Sugar pFDR", "Fat pFDR",
             "All six against zero pFDR < .001", "Lowest mean (fat, peach)", "Highest mean (sugar, peach)",
             "Beverage-like n (of 52)", "Caloric n (of 52)", "Bowker p", "McNemar p"),
  obtained = c(
    paired_tests$p_fdr[paired_tests$measure == "Intensity"],
    paired_tests$p_fdr[paired_tests$measure == "Pleasantness"],
    paired_tests$p_fdr[paired_tests$measure == "Calories"],
    paired_tests$p_fdr[paired_tests$measure == "Sugar"],
    paired_tests$p_fdr[paired_tests$measure == "Fat"],
    as.numeric(all(one_sample_tests$p_fdr < .001)),
    min(descriptives$mean[descriptives$measure %in% c("Calories", "Sugar", "Fat")]),
    max(descriptives$mean[descriptives$measure %in% c("Calories", "Sugar", "Fat")]),
    open_summary$n[2], open_summary$n[3], bowker_p, mcnemar_exact$p.value),
  expected = c(.563, .563, .277, .563, .277, 1, 7.25, 16.6, 48, 22, .198, .125)
) %>%
  mutate(check = if_else(abs(obtained - expected) <= .01, "OK", "NON TORNA"))
print(as.data.frame(final_check), digits = 4)


# -----------------------------------------------------------------------------
# 11. SALVATAGGIO DEI RISULTATI
# -----------------------------------------------------------------------------
# Nessuna analisi nuova: i risultati delle sezioni precedenti vengono salvati
# nelle tabelle del Supplementary, in outputs/Pilot1/tables.
section("11. SALVATAGGIO DEI RISULTATI")

# Table S1: descrittive
save_table(descriptives, "Table_S1_Pilot1_descriptives.csv")

# Table S2: Wilcoxon appaiati pesca vs cioccolato
table_s2 <- paired_tests %>%
  transmute(measure, n_pairs, V, p_raw = p, p_fdr, significance,
            p_text = if_else(p_fdr < .001, "pFDR < .001", paste0("pFDR = ", format_p(p_fdr))),
            plot_label)
save_table(table_s2, "Table_S2_Pilot1_paired_comparisons.csv")

# Table S3: Wilcoxon contro zero
table_s3 <- one_sample_tests %>%
  transmute(measure, cue, n, V, p_raw = p, p_fdr, significance, xpos, y_star = 102)
save_table(table_s3, "Table_S3_Pilot1_one_sample_tests.csv")

# Table S4: risposte aperte (prima su entrambi i pod, poi per pod)
table_s4 <- open_by_cue %>%
  mutate(group = as.character(group), category = as.character(category)) %>%
  arrange(match(group, c("Across cues", "Peach smoothie", "Chocolate-orange")),
          match(category, category_levels)) %>%
  transmute(scope = if_else(group == "Across cues", "Across cues", "By cue"),
            cue = group, category, n, denominator, percentage = percent)
save_table(table_s4, "Table_S4_Pilot1_open_responses.csv")

# Table S5: tabelle di transizione, Bowker e McNemar esatto
table_s5 <- bind_rows(
  as.data.frame(transition_matrix) %>%
    transmute(record_type = "Transition matrix", analysis = "Three-category coding",
              row_category = as.character(Peach), column_category = as.character(Chocolate),
              n = as.numeric(Freq), statistic = NA_real_, df = NA_real_, p = NA_real_,
              note = "Rows: Peach smoothie; columns: Chocolate-orange; 0=plain water, 1=flavoured/low-calorie, 2=caloric/nutrient-related"),
  as.data.frame(binary_matrix) %>%
    transmute(record_type = "Transition matrix", analysis = "Binary caloric coding",
              row_category = as.character(Peach), column_category = as.character(Chocolate),
              n = as.numeric(Freq), statistic = NA_real_, df = NA_real_, p = NA_real_,
              note = "Rows: Peach smoothie; columns: Chocolate-orange; 0=non-caloric, 1=caloric/nutrient-related"),
  tibble(record_type = "Statistical test", analysis = "Bowker test of symmetry",
         row_category = NA_character_, column_category = NA_character_,
         n = nrow(open_pairs), statistic = bowker_chi2, df = as.numeric(bowker_df),
         p = bowker_p, note = "Complete paired responses"),
  tibble(record_type = "Statistical test", analysis = "Exact McNemar test",
         row_category = NA_character_, column_category = NA_character_,
         n = noncaloric_to_caloric + caloric_to_noncaloric, statistic = NA_real_, df = NA_real_,
         p = mcnemar_exact$p.value,
         note = paste0("Non-caloric to caloric = ", noncaloric_to_caloric,
                       "; caloric to non-caloric = ", caloric_to_noncaloric))
)
save_table(table_s5, "Table_S5_Pilot1_categorical_results.csv")

# Riepilogo delle verifiche: ogni riga confronta un risultato riportato nel
# Supplementary con quello ottenuto (PASS = torna, FAIL = non torna)
n_total_responses <- nrow(p1)
n_plain_water     <- sum(classifiable$open_code == 0)
n_beverage_like   <- sum(classifiable$open_code %in% c(1, 2))
n_caloric         <- sum(classifiable$open_code == 2)
verification_summary <- tibble(
  claim = c("Participant × cue rows", "Unique participants",
            "Each participant has exactly two cue rows",
            "No paired pod comparison survives FDR correction",
            "All six nutritional ratings are above zero after FDR correction",
            "Total open responses", "Classifiable open responses", "Non-classifiable open responses",
            "Plain-water responses", "Beverage-like responses", "Caloric/nutrient-related responses",
            "Beverage-like percentage", "Complete pairs for categorical analyses",
            "Bowker chi-square", "Bowker degrees of freedom", "Bowker p value",
            "Non-caloric to caloric transitions", "Caloric to non-caloric transitions",
            "Exact McNemar p value"),
  expected = c("56", "28", "2 per participant", "All pFDR >= .05", "All pFDR < .001",
               "56", "52", "4", "4", "48", "22", "92.3%", "24",
               "4.6667", "3", ".197897", "6", "1", ".125"),
  observed = c(
    as.character(nrow(p1)), as.character(n_distinct(p1$subject)),
    paste(range(table(p1$subject)), collapse = "–"),
    paste0("Minimum pFDR = ", sprintf("%.6f", min(paired_tests$p_fdr))),
    paste0("Maximum pFDR = ", sprintf("%.6g", max(one_sample_tests$p_fdr))),
    as.character(n_total_responses), as.character(n_classifiable),
    as.character(n_total_responses - n_classifiable),
    as.character(n_plain_water), as.character(n_beverage_like), as.character(n_caloric),
    paste0(sprintf("%.1f", 100 * n_beverage_like / n_classifiable), "%"),
    as.character(nrow(open_pairs)),
    sprintf("%.7f", bowker_chi2), as.character(bowker_df), sprintf("%.7f", bowker_p),
    as.character(noncaloric_to_caloric), as.character(caloric_to_noncaloric),
    sprintf("%.6f", mcnemar_exact$p.value)),
  pass = c(
    nrow(p1) == 56, n_distinct(p1$subject) == 28, all(table(p1$subject) == 2),
    all(paired_tests$p_fdr >= .05), all(one_sample_tests$p_fdr < .001),
    n_total_responses == 56, n_classifiable == 52, n_total_responses - n_classifiable == 4,
    n_plain_water == 4, n_beverage_like == 48, n_caloric == 22,
    round(100 * n_beverage_like / n_classifiable, 1) == 92.3, nrow(open_pairs) == 24,
    isTRUE(all.equal(bowker_chi2, 4.6666667, tolerance = 1e-6)), bowker_df == 3,
    isTRUE(all.equal(bowker_p, 0.1978971, tolerance = 1e-6)),
    noncaloric_to_caloric == 6, caloric_to_noncaloric == 1,
    isTRUE(all.equal(mcnemar_exact$p.value, 0.125, tolerance = 1e-10)))
) %>%
  mutate(status = if_else(pass, "PASS", "FAIL"))
save_table(verification_summary, "Pilot1_verification_summary.csv")
print(as.data.frame(verification_summary))

# Informazioni sulla sessione di R (cartella logs)
writeLines(capture.output(sessionInfo()), file.path(logs_folder, "Pilot1_sessionInfo.txt"))

cat("\nFatto. Tabelle in:", tables_folder, "\nFigure in:", figures_folder,
    "\nLog in:", logs_folder, "\n")
