# =============================================================================
# PILOTA 2 - ANALISI COMPLETA (PASSO PASSO)
# Olfactory Pod (P) vs No Olfactory Pod (A), N = 47
#
# A = No Olfactory Pod, P = Olfactory Pod
# Le differenze indotte dal pod sono sempre P - A.
# In entrambe le condizioni i partecipanti hanno bevuto 200 ml: la colonna
# "Ingested Ml" e' la quantita' STIMATA dal partecipante, non quella reale.
#
# Lo script non apre finestre. Legge data/pilot_2_data_.xlsx, esegue tutte
# le analisi del Pilot 2 riportate nel Supplementary e salva i risultati in:
#   outputs/Pilot2/tables   (Table_S1 ... Table_S6, S2b, S3b, S5b, S5c,
#                            Pilot2_verification_summary.csv)
#   outputs/Pilot2/figures  (Figure S3, S4, S5 e due figure "Additional_")
#   outputs/Pilot2/logs     (Pilot2_sessionInfo.txt)
# nella cartella del repository (quella che contiene "data" e "outputs").
#
# Contiene anche la replica "a mano" dell'ART. Pilot2_verification_summary.csv
# confronta ogni risultato riportato nel Supplementary con quello ottenuto:
# se un numero non torna, la riga dice FAIL. In particolare:
#   - sezione 9: confronto tra gli aumenti dovuti al pod (quale proprieta' e
#     quale gusto aumentano di piu'), Table S2b;
#   - sezione 10: correzione FDR delle regressioni sui tre modelli
#     nutrizionali, Table S3b.
# Le due figure della sezione 14d ("Additional_...") servono per le slide
# 10 e 12 e non sono incluse nel Supplementary.
# Ogni sezione indica a quale figura/tabella del Supplementary e a quale
# slide del deck "Pilot 1 e Pilot 2 - Supplementary results" si riferisce.
# =============================================================================


# -----------------------------------------------------------------------------
# 0. IMPOSTAZIONI - 
# -----------------------------------------------------------------------------
# Percorso completo del file Excel (usare le barre "/").
data_file <- "C:/Users/Eleonora/OneDrive/MyExperiments/sapienza/airup/bial 2026/analisi/repository 19-11/data/pilot_2_data_.xlsx"

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
# Il mirror CRAN e' impostato prima di qualsiasi installazione, cosi' R non
# apre la finestra di scelta del mirror.
options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c(
  "readxl", "dplyr", "tidyr", "stringr", "tibble", "ggplot2", "patchwork",
  "ARTool", "emmeans", "psych", "broom", "lme4", "lmerTest", "sandwich"
)
# Se manca qualche pacchetto lo installa e poi si ferma.
# Dopo l'installazione: Session > Restart R, poi rilanciare lo script.
missing_packages <- setdiff(packages, rownames(installed.packages()))
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
  stop("Pacchetti installati: riavvia R (Session > Restart R) e lancia di nuovo lo script.",
       call. = FALSE)
}

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(ggplot2)
library(patchwork)
library(ARTool)
library(emmeans)
library(psych)
library(broom)

# Alcuni pacchetti (ad esempio "car" o "MASS"), se gia' caricati nella stessa
# sessione di R, hanno funzioni con lo stesso nome di quelle di dplyr
# (recode, select, filter). Qui si usano esplicitamente quelle di dplyr,
# cosi' lo script funziona anche senza riavviare R.
select <- dplyr::select
filter <- dplyr::filter

# Codifica dei fattori a somma zero.
options(contrasts = c("contr.sum", "contr.poly"))
set.seed(2026)

# Cartelle dei risultati
tables_folder  <- file.path(repository_folder, "outputs", "Pilot2", "tables")
figures_folder <- file.path(repository_folder, "outputs", "Pilot2", "figures")
logs_folder    <- file.path(repository_folder, "outputs", "Pilot2", "logs")
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

# ASTERISCHI DI SIGNIFICATIVITA'
# print_sig() stampa una tabella aggiungendo, accanto a ogni colonna di valori p,
# una colonna "sig" con:  *** p < .001   ** p < .01   * p < .05   ns = non significativo
print_sig <- function(table) {
  table <- as.data.frame(table)
  p_columns <- grep("^Pr\\(|^p\\.value$|^p$|^p_fdr$|_p$|^p_fdr_global$",
                    names(table), value = TRUE)
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

raw <- read_excel(temporary_copy, sheet = "Coding_binary")
names(raw) <- str_squish(names(raw))   # toglie gli spazi in piu' nei nomi delle colonne

# Una riga per partecipante e condizione (47 x 2 = 94 righe)
dat <- raw %>%
  filter(!is.na(SOGG), Condition %in% c("A", "P")) %>%
  transmute(
    subject   = factor(SOGG),
    condition = factor(Condition, levels = c("A", "P")),
    # risposta aperta codificata: 0 = acqua semplice, 1 = bevanda
    binary_code = as.numeric(`Binary code`),
    # valutazioni VAS 0-100
    pleasantness = as.numeric(Pleasantness),
    intensity    = as.numeric(Intensity),
    sweet  = as.numeric(Sweet),
    salt   = as.numeric(Salt),
    sour   = as.numeric(Sour),
    bitter = as.numeric(Bitter),
    kcal   = as.numeric(KCAL),
    sugar  = as.numeric(Sugar),
    fat    = as.numeric(Far),
    # quantita' stimata (ml)
    ingested_ml_estimate = as.numeric(`Ingested Ml`),
    # misure di fine sessione (uguali nelle due righe dello stesso partecipante)
    sniffing                  = as.numeric(`sniffing sticks performance`),   # SST-12
    maia_noticing             = as.numeric(`MAIA-Noticing`),
    maia_not_distracting      = as.numeric(`MAIA-Not_Distracting`),
    maia_not_worrying         = as.numeric(`MAIA-Not_Worrying`),
    maia_attention_regulation = as.numeric(`MAIA-Attention_Regulation`),
    maia_emotional_awareness  = as.numeric(`MAIA-Emotional_Awareness`),
    maia_self_regulation      = as.numeric(`MAIA-Self_Regulation`),
    maia_body_listening       = as.numeric(`MAIA-Body_Listening`),
    maia_trusting             = as.numeric(`MAIA-Trusting`),
    tas                       = as.numeric(TAS)
  )

cat("\nPartecipanti:", n_distinct(dat$subject), "| righe:", nrow(dat), "\n")

# Controlli essenziali: 47 partecipanti, ognuno con una riga A e una riga P
stopifnot(
  n_distinct(dat$subject) == 47,
  nrow(dat) == 94,
  all(table(dat$subject, dat$condition) == 1)
)


# -----------------------------------------------------------------------------
# 3. VARIABILI DERIVATE
# -----------------------------------------------------------------------------
trait_cols <- c(
  "sniffing",
  "maia_noticing", "maia_not_distracting", "maia_not_worrying",
  "maia_attention_regulation", "maia_emotional_awareness",
  "maia_self_regulation", "maia_body_listening", "maia_trusting",
  "tas"
)

# 3a. Questionari: una riga per partecipante (primo valore non mancante)
subject_traits <- dat %>%
  group_by(subject) %>%
  summarise(across(all_of(trait_cols), ~ .x[!is.na(.x)][1]), .groups = "drop")

# 3b. Formato largo: una riga per partecipante, con le differenze P - A
wide <- dat %>%
  select(subject, condition, pleasantness, intensity, sweet, salt, sour, bitter,
         kcal, sugar, fat, ingested_ml_estimate, binary_code) %>%
  pivot_wider(
    names_from  = condition,
    values_from = c(pleasantness, intensity, sweet, salt, sour, bitter,
                    kcal, sugar, fat, ingested_ml_estimate, binary_code),
    names_glue  = "{.value}_{condition}"
  ) %>%
  left_join(subject_traits, by = "subject") %>%
  mutate(
    # cambiamento indotto dal pod (P - A)
    diff_pleasantness = pleasantness_P - pleasantness_A,
    diff_intensity    = intensity_P - intensity_A,
    diff_sweet  = sweet_P  - sweet_A,
    diff_salt   = salt_P   - salt_A,
    diff_sour   = sour_P   - sour_A,
    diff_bitter = bitter_P - bitter_A,
    diff_kcal   = kcal_P   - kcal_A,
    diff_sugar  = sugar_P  - sugar_A,
    diff_fat    = fat_P    - fat_A,
    diff_ingested_estimate = ingested_ml_estimate_P - ingested_ml_estimate_A,
    # errore assoluto di stima rispetto ai 200 ml bevuti davvero
    abs_error_A    = abs(ingested_ml_estimate_A - 200),
    abs_error_P    = abs(ingested_ml_estimate_P - 200),
    diff_abs_error = abs_error_P - abs_error_A,
    # INDICE NUTRIZIONALE COMPOSITO
    # 1) ogni differenza (calorie, zuccheri, grassi) viene standardizzata (z);
    # 2) si fa la media dei tre z.
    # 0 = effetto del pod nella media del campione; valori positivi = effetto maggiore.
    z_kcal  = as.numeric(scale(diff_kcal)),
    z_sugar = as.numeric(scale(diff_sugar)),
    z_fat   = as.numeric(scale(diff_fat)),
    nutritional_composite_z   = rowMeans(cbind(z_kcal, z_sugar, z_fat)),
    # versione grezza: media delle tre differenze non standardizzate
    nutritional_composite_raw = rowMeans(cbind(diff_kcal, diff_sugar, diff_fat))
  )

# 3c. Formato lungo per le ART: tutte le valutazioni in una colonna,
#     con una colonna che dice a quale proprieta' (o gusto) appartengono.
nutrition_long <- dat %>%
  select(subject, condition, kcal, sugar, fat) %>%
  pivot_longer(c(kcal, sugar, fat), names_to = "nutritional_property",
               values_to = "nutritional_rating") %>%
  mutate(nutritional_property = factor(nutritional_property,
                                       levels = c("fat", "kcal", "sugar"),
                                       labels = c("Fat", "Calories", "Sugar")))

taste_long <- dat %>%
  select(subject, condition, sweet, salt, sour, bitter) %>%
  pivot_longer(c(sweet, salt, sour, bitter), names_to = "taste",
               values_to = "taste_rating") %>%
  mutate(taste = factor(taste,
                        levels = c("bitter", "sour", "sweet", "salt"),
                        labels = c("Bitter", "Sour", "Sweet", "Salt")))


# -----------------------------------------------------------------------------
# 4. DESCRITTIVE E SCELTA DELL'ANALISI (NORMALITA')
# -----------------------------------------------------------------------------
section("4. DESCRITTIVE")

measure_names <- c(
  pleasantness = "Pleasantness", intensity = "Intensity",
  sweet = "Sweetness", salt = "Saltiness", sour = "Sourness", bitter = "Bitterness",
  kcal = "Calories", sugar = "Sugar", fat = "Fat",
  ingested_ml_estimate = "Estimated amount consumed (ml)"
)

descriptives <- dat %>%
  select(condition, all_of(names(measure_names))) %>%
  pivot_longer(-condition, names_to = "measure", values_to = "value") %>%
  mutate(
    measure = measure_names[measure],
    condition_label = if_else(condition == "A", "No Olfactory Pod", "Olfactory Pod")
  ) %>%
  group_by(measure, condition_label) %>%
  summarise(
    n = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE), sd = sd(value, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q1 = unname(quantile(value, .25, na.rm = TRUE)),
    q3 = unname(quantile(value, .75, na.rm = TRUE)),
    minimum = min(value, na.rm = TRUE), maximum = max(value, na.rm = TRUE),
    .groups = "drop"
  )
print(descriptives, n = Inf)

# Normalita' dei residui del modello fattoriale classico.
# Se i residui NON sono normali (p < .05) si usa l'ART (ANOVA sui ranghi allineati).
section("4. NORMALITA' DEI RESIDUI -> SCELTA DELL'ART")

nutrition_lm <- lm(nutritional_rating ~ subject + condition * nutritional_property,
                   data = nutrition_long)
taste_lm     <- lm(taste_rating ~ subject + condition * taste, data = taste_long)

normality <- tibble(
  model = c("Nutritional factorial model", "Taste factorial model"),
  residual_shapiro_W = c(shapiro.test(residuals(nutrition_lm))$statistic,
                         shapiro.test(residuals(taste_lm))$statistic),
  residual_shapiro_p = c(shapiro.test(residuals(nutrition_lm))$p.value,
                         shapiro.test(residuals(taste_lm))$p.value),
  analysis_selected = "ART"
)
print_sig(normality)


# -----------------------------------------------------------------------------
# 5. CONTRASTI ART-C: funzione usata nelle sezioni 6 e 8
# -----------------------------------------------------------------------------
# Confronto pod vs no pod DENTRO ogni livello (ogni proprieta' o ogni gusto).
# ART-C: si mettono in rango le valutazioni originali, si stima un modello
# lineare con il partecipante come blocco e si confrontano A e P con emmeans.
# Correzione Benjamini-Hochberg (FDR) sui confronti della stessa famiglia.
artc_contrasts <- function(long_data, rating, factor_name) {
  long_data$artc_rank <- rank(long_data[[rating]], ties.method = "average")
  model <- lm(as.formula(paste("artc_rank ~ subject + condition *", factor_name)),
              data = long_data)
  emm <- emmeans(model, as.formula(paste("~ condition |", factor_name)))
  summary(pairs(emm, adjust = "none"), infer = c(TRUE, TRUE)) %>%
    as.data.frame() %>%
    mutate(
      p_fdr = p.adjust(p.value, method = "BH"),
      significance = significance_label(p_fdr)
    )
}

# d_z (dimensione dell'effetto per misure ripetute) = media(P - A) / DS(P - A).
# Non e' nel Supplementary: e' il valore riportato sulle slide.
print_dz <- function(variables) {
  for (v in variables) {
    x <- wide[[paste0("diff_", v)]]
    cat(sprintf("%-7s Pod - No Pod = %6.1f   d_z = %.2f\n", v, mean(x), mean(x) / sd(x)))
  }
}


# -----------------------------------------------------------------------------
# 6. PROPRIETA' NUTRIZIONALI: ART + CONTRASTI ART-C  (SLIDE 9, Figura S3A)
# -----------------------------------------------------------------------------
# Modello: 2 condizioni x 3 proprieta' (calorie, zuccheri, grassi), misure ripetute.
# Error(subject / ...) = ogni effetto e' confrontato con quanto varia tra persone.
section("6. NUTRIZIONE: ART 2 (condizione) x 3 (proprieta')")

nutrition_art <- art(
  nutritional_rating ~ nutritional_property * condition +
    Error(subject / (nutritional_property * condition)),
  data = nutrition_long
)
nutrition_anova <- as.data.frame(anova(nutrition_art))
print_sig(nutrition_anova)      # interazione attesa: F(2, 92) = 47.49

# CONTROLLO: l'interazione ART rifatta "a mano".
# Cosa fa l'ART per l'interazione, in tre passi:
#   1) allinea: valore - media della sua condizione - media della sua proprieta' + media generale
#      (cosi' restano solo l'interazione e l'errore);
#   2) mette in rango i valori allineati;
#   3) fa la normale ANOVA a misure ripetute sui ranghi.
# Deve dare lo stesso F dell'ART: F(2, 92) = 47.49.
nutrition_long$aligned_rank_interaction <- rank(round(
  nutrition_long$nutritional_rating -
    ave(nutrition_long$nutritional_rating, nutrition_long$condition) -
    ave(nutrition_long$nutritional_rating, nutrition_long$nutritional_property) +
    mean(nutrition_long$nutritional_rating), 12), ties.method = "average")
nutrition_manual_art <- aov(aligned_rank_interaction ~ nutritional_property * condition +
                              Error(subject / (nutritional_property * condition)),
                            data = nutrition_long)
print(summary(nutrition_manual_art)[["Error: subject:nutritional_property:condition"]])

section("6. NUTRIZIONE: contrasti ART-C pod vs no pod (FDR sui 3)")
nutrition_contrasts <- artc_contrasts(nutrition_long, "nutritional_rating", "nutritional_property")
print_sig(nutrition_contrasts)  # stima = A - P sui ranghi: negativa = piu' alto con il pod
print_dz(c("fat", "kcal", "sugar"))


# -----------------------------------------------------------------------------
# 7. COERENZA DEI CAMBIAMENTI NUTRIZIONALI  (SLIDE 9)
# -----------------------------------------------------------------------------
# Correlazioni di Pearson tra le tre differenze P - A e alfa di Cronbach
# standardizzato: giustificano l'indice composito.
section("7. COERENZA: correlazioni tra le differenze e alfa standardizzato")

nutrition_changes  <- wide %>% select(diff_kcal, diff_sugar, diff_fat)
nutrition_cor      <- cor(nutrition_changes, method = "pearson")
standardized_alpha <- psych::alpha(nutrition_changes, check.keys = FALSE,
                                   warnings = FALSE)$total$std.alpha

nutrition_cor_pairs <- tibble(
  index_1 = c("Calories", "Calories", "Sugar"),
  index_2 = c("Sugar", "Fat", "Fat"),
  r = c(nutrition_cor["diff_kcal", "diff_sugar"],
        nutrition_cor["diff_kcal", "diff_fat"],
        nutrition_cor["diff_sugar", "diff_fat"])
)
print(nutrition_cor_pairs)                                  # r = .48 - .74
cat("Standardized alpha =", round(standardized_alpha, 3), "\n")   # .82


# -----------------------------------------------------------------------------
# 8. GUSTO: ART + CONTRASTI ART-C  (SLIDE 11, Figura S3B)
# -----------------------------------------------------------------------------
# Modello: 2 condizioni x 4 gusti (amaro, acido, dolce, salato), misure ripetute.
section("8. GUSTO: ART 2 (condizione) x 4 (gusto)")

taste_art <- art(
  taste_rating ~ taste * condition + Error(subject / (taste * condition)),
  data = taste_long
)
taste_anova <- as.data.frame(anova(taste_art))
print_sig(taste_anova)          # interazione attesa: F(3, 138) = 82.25

# CONTROLLO: interazione ART rifatta "a mano" (stessi tre passi della sezione 6).
# Deve dare F(3, 138) = 82.25.
taste_long$aligned_rank_interaction <- rank(round(
  taste_long$taste_rating -
    ave(taste_long$taste_rating, taste_long$condition) -
    ave(taste_long$taste_rating, taste_long$taste) +
    mean(taste_long$taste_rating), 12), ties.method = "average")
taste_manual_art <- aov(aligned_rank_interaction ~ taste * condition +
                          Error(subject / (taste * condition)), data = taste_long)
print(summary(taste_manual_art)[["Error: subject:taste:condition"]])

section("8. GUSTO: contrasti ART-C pod vs no pod (FDR sui 4)")
taste_contrasts <- artc_contrasts(taste_long, "taste_rating", "taste")
print_sig(taste_contrasts)
print_dz(c("sweet", "sour", "bitter", "salt"))


# -----------------------------------------------------------------------------
# 9. CONFRONTO TRA GLI AUMENTI DOVUTI AL POD  (SLIDE 10 e 12; Table S2b)
#    Quale proprieta' (e quale gusto) cambia di piu' con il pod?
# -----------------------------------------------------------------------------
# I contrasti delle sezioni 6 e 8 dicono se ogni proprieta' sale con il pod.
# Qui si confrontano gli AUMENTI tra loro (es. aumento zuccheri vs aumento grassi):
# contrasti di interazione ART-C sugli stessi modelli ART, FDR dentro ogni famiglia
# (3 confronti per la nutrizione, 6 per il gusto).
# Controllo di robustezza: Wilcoxon appaiato sulla differenza tra gli aumenti.
# Nel Supplementary sono riportati i contrasti ART-C; il Wilcoxon e' salvato
# nella stessa tabella (Table S2b) ma non e' riportato nel testo.

# art.con() restituisce (A - P) del primo livello meno (A - P) del secondo:
# stima POSITIVA = aumento con il pod piu' grande per il SECONDO livello.
interaction_contrasts <- function(art_model, term) {
  out <- as.data.frame(art.con(art_model, term, interaction = TRUE, adjust = "none"))
  names(out)[1] <- "comparison"
  levels_pair <- str_split_fixed(as.character(out$comparison), " - ", 2)
  out %>%
    mutate(
      larger_pod_change = if_else(estimate > 0, str_trim(levels_pair[, 2]),
                                  str_trim(levels_pair[, 1])),
      p_fdr = p.adjust(p.value, method = "BH")
    ) %>%
    select(comparison, larger_pod_change, estimate, SE, df, t.ratio, p.value, p_fdr)
}

# Wilcoxon: per ogni coppia di livelli, differenza tra i due aumenti di ogni persona
change_wilcoxon <- function(changes, levels_order) {
  level_pairs <- combn(levels_order, 2, simplify = FALSE)
  bind_rows(lapply(level_pairs, function(pair) {
    x <- changes[[pair[2]]] - changes[[pair[1]]]
    test <- wilcox.test(x, mu = 0, exact = FALSE)
    tibble(
      comparison = paste(pair[1], "-", pair[2]),
      mean_change_second_minus_first = mean(x),
      n_second_larger = sum(x > 0), n_first_larger = sum(x < 0), n_equal = sum(x == 0),
      V = unname(test$statistic), p.value = test$p.value
    )
  })) %>%
    mutate(p_fdr = p.adjust(p.value, method = "BH"))
}

nutrition_changes_named <- wide %>%
  transmute(subject, Fat = diff_fat, Calories = diff_kcal, Sugar = diff_sugar)
taste_changes_named <- wide %>%
  transmute(subject, Bitter = diff_bitter, Sour = diff_sour, Sweet = diff_sweet, Salt = diff_salt)

section("9. CONFRONTO TRA GLI AUMENTI - NUTRIZIONE")
nutrition_interaction_contrasts <- interaction_contrasts(nutrition_art, "nutritional_property:condition")
print_sig(nutrition_interaction_contrasts)          # zuccheri > calorie > grassi
nutrition_change_wilcoxon <- change_wilcoxon(nutrition_changes_named, c("Fat", "Calories", "Sugar"))
print_sig(nutrition_change_wilcoxon)

section("9. CONFRONTO TRA GLI AUMENTI - GUSTO")
taste_interaction_contrasts <- interaction_contrasts(taste_art, "taste:condition")
print_sig(taste_interaction_contrasts)              # dolce > tutti gli altri
taste_change_wilcoxon <- change_wilcoxon(taste_changes_named, c("Bitter", "Sour", "Sweet", "Salt"))
print_sig(taste_change_wilcoxon)



# -----------------------------------------------------------------------------
# 10. REGRESSIONI ESPLORATIVE: CAMBIAMENTI DI GUSTO -> CAMBIAMENTI NUTRIZIONALI  (SLIDE 13, Figura S4)
# -----------------------------------------------------------------------------
# Una riga per partecipante. Predittori (inseriti insieme): le quattro
# differenze di gusto P - A. Variabili dipendenti (quattro modelli separati):
#   Modello 1: composito z ~ Δdolce + Δsalato + Δacido + Δamaro
#   Modello 2: Δgrassi     ~ Δdolce + Δsalato + Δacido + Δamaro
#   Modello 3: Δcalorie    ~ Δdolce + Δsalato + Δacido + Δamaro
#   Modello 4: Δzuccheri   ~ Δdolce + Δsalato + Δacido + Δamaro
# Ogni b = effetto di quel gusto a parita' degli altri tre.
# Nel Supplementary: p non corretti per ogni modello (analisi esplorative) e,
# in piu', per ogni gusto la correzione FDR sui modelli 2-4 (Table S3b; slide 13).
section("10. REGRESSIONI: gusto -> nutrizione")

composite_model <- lm(nutritional_composite_z ~ diff_sweet + diff_salt + diff_sour + diff_bitter, data = wide)
fat_model       <- lm(diff_fat   ~ diff_sweet + diff_salt + diff_sour + diff_bitter, data = wide)
kcal_model      <- lm(diff_kcal  ~ diff_sweet + diff_salt + diff_sour + diff_bitter, data = wide)
sugar_model     <- lm(diff_sugar ~ diff_sweet + diff_salt + diff_sour + diff_bitter, data = wide)

regression_models <- list("Composite z" = composite_model, "Fat" = fat_model,
                          "Calories" = kcal_model, "Sugar" = sugar_model)

regression_fit <- bind_rows(lapply(names(regression_models), function(m)
  glance(regression_models[[m]]) %>% mutate(model = m))) %>%
  select(model, r.squared, adj.r.squared, statistic, df, df.residual, p.value)

regression_coefficients <- bind_rows(lapply(names(regression_models), function(m)
  tidy(regression_models[[m]]) %>% mutate(model = m))) %>%
  select(model, term, estimate, std.error, statistic, p.value)

print_sig(regression_fit)            # R2 = .381 (composito), .30 (grassi), .25 (calorie), .35 (zuccheri)
print_sig(regression_coefficients)

# Correzione FDR (Table S3b; slide 13): per ogni gusto, FDR sui tre modelli
# nutrizionali (grassi, calorie, zuccheri). Il composito resta fuori perche'
# e' costruito da queste tre misure.
regression_fdr <- regression_coefficients %>%
  filter(term != "(Intercept)", model != "Composite z") %>%
  select(model, term, estimate, std.error, statistic, p.value) %>%
  group_by(term) %>%
  mutate(p_fdr = p.adjust(p.value, method = "BH")) %>%
  ungroup() %>%
  arrange(term, model)
print_sig(regression_fdr)
# Dolce: grassi pFDR = .009, calorie .005, zuccheri < .001
# Amaro: grassi pFDR = .034, calorie .186, zuccheri .252

# Normalita' dei residui di ciascuna regressione
regression_normality <- tibble(
  model = names(regression_models),
  residual_shapiro_p = sapply(regression_models, function(m) shapiro.test(residuals(m))$p.value)
)
print_sig(regression_normality)

# Bootstrap del modello sui grassi: 10.000 volte si ricampionano i partecipanti
# (con reinserimento), si rifa' il modello e si salvano i coefficienti di dolce
# e amaro. IC 95% = 2.5 e 97.5 percentile.
section("10. BOOTSTRAP DEL MODELLO SUI GRASSI (10.000 ricampionamenti)")

set.seed(2026)
bootstrap_coefficients <- replicate(10000, {
  sampled_rows <- sample(seq_len(nrow(wide)), replace = TRUE)
  bootstrap_model <- lm(diff_fat ~ diff_sweet + diff_salt + diff_sour + diff_bitter,
                        data = wide[sampled_rows, ])
  coef(bootstrap_model)[c("diff_sweet", "diff_bitter")]
})

bootstrap_ci <- tibble(
  predictor = c("Sweetness", "Bitterness"),
  estimate  = coef(fat_model)[c("diff_sweet", "diff_bitter")],
  ci_lower  = apply(bootstrap_coefficients, 1, quantile, probs = .025, na.rm = TRUE),
  ci_upper  = apply(bootstrap_coefficients, 1, quantile, probs = .975, na.rm = TRUE)
)
print(bootstrap_ci)


# -----------------------------------------------------------------------------
# 11. QUANTITA' STIMATA, ERRORE DI STIMA, INTENSITA' E PIACEVOLEZZA  (SLIDE 14, Figura S3C-D)
# -----------------------------------------------------------------------------
# Regola: si controlla la normalita' delle differenze P - A (Shapiro-Wilk).
#   differenze normali     -> t-test appaiato
#   differenze non normali -> Wilcoxon appaiato

# 11a. Quantita' stimata (ml): differenze normali -> t-test appaiato
section("11a. QUANTITA' STIMATA: t-test appaiato")
print(shapiro.test(wide$diff_ingested_estimate))      # p = .096: normale
ingestion_t <- t.test(wide$ingested_ml_estimate_P, wide$ingested_ml_estimate_A, paired = TRUE)
print(ingestion_t)                                    # t(46) = -2.27, p = .028
cat("Media A =", round(mean(wide$ingested_ml_estimate_A), 2),
    "| Media P =", round(mean(wide$ingested_ml_estimate_P), 2),
    "| Differenza P - A =", round(mean(wide$diff_ingested_estimate), 2), "ml\n")

# 11b. Errore assoluto |stima - 200|: differenze non normali -> Wilcoxon appaiato
section("11b. ERRORE ASSOLUTO DI STIMA: Wilcoxon appaiato")
print(shapiro.test(wide$diff_abs_error))
absolute_error_wilcox <- wilcox.test(wide$abs_error_P, wide$abs_error_A,
                                     paired = TRUE, exact = FALSE)
print(absolute_error_wilcox)                          # V = 605, p = .668
cat("Errore medio A =", round(mean(wide$abs_error_A), 2),
    "| Errore medio P =", round(mean(wide$abs_error_P), 2), "ml\n")

# 11c. Intensita' e piacevolezza: differenze non normali -> Wilcoxon appaiato
section("11c. INTENSITA' E PIACEVOLEZZA: Wilcoxon appaiato")
print(shapiro.test(wide$intensity_P - wide$intensity_A))
print(shapiro.test(wide$pleasantness_P - wide$pleasantness_A))
intensity_wilcox    <- wilcox.test(wide$intensity_A, wide$intensity_P, paired = TRUE, exact = FALSE)
pleasantness_wilcox <- wilcox.test(wide$pleasantness_A, wide$pleasantness_P, paired = TRUE, exact = FALSE)
print(intensity_wilcox)                               # V = 166, p < .001
print(pleasantness_wilcox)                            # V = 585, p = .631

amount_and_perception <- tibble(
  outcome = c("Estimated amount (ml)", "Absolute estimation error (ml)", "Intensity", "Pleasantness"),
  no_pod  = c(mean(wide$ingested_ml_estimate_A), mean(wide$abs_error_A),
              median(wide$intensity_A), median(wide$pleasantness_A)),
  pod     = c(mean(wide$ingested_ml_estimate_P), mean(wide$abs_error_P),
              median(wide$intensity_P), median(wide$pleasantness_P)),
  descriptive = c("mean", "mean", "median", "median"),
  test = c("Paired t-test", "Paired Wilcoxon", "Paired Wilcoxon", "Paired Wilcoxon"),
  statistic = c(unname(ingestion_t$statistic), unname(absolute_error_wilcox$statistic),
                unname(intensity_wilcox$statistic), unname(pleasantness_wilcox$statistic)),
  p = c(ingestion_t$p.value, absolute_error_wilcox$p.value,
        intensity_wilcox$p.value, pleasantness_wilcox$p.value)
)
print_sig(amount_and_perception)


# -----------------------------------------------------------------------------
# 12. RISPOSTE APERTE (codifica binaria: 0 = acqua, 1 = bevanda)  (SLIDE 15, Figura S3E)
# -----------------------------------------------------------------------------
section("12. RISPOSTE APERTE")

# Quante risposte "acqua" e "bevanda" in ciascuna condizione
open_by_condition <- dat %>%
  count(condition, binary_code, name = "n") %>%
  group_by(condition) %>%
  mutate(N = sum(n), percent = 100 * n / N) %>%
  ungroup()
print(open_by_condition)

# Tabella 2 x 2: risposta senza pod (righe) x risposta con pod (colonne)
open_matrix <- table(
  No_pod = factor(wide$binary_code_A, levels = c(0, 1)),
  Pod    = factor(wide$binary_code_P, levels = c(0, 1))
)
print(open_matrix)    # 30 acqua -> bevanda, 1 bevanda -> acqua, 15 bevanda in entrambe, 1 acqua in entrambe

# McNemar esatto = test binomiale sui soli partecipanti che cambiano categoria
shifts_water_to_beverage <- open_matrix["0", "1"]
shifts_beverage_to_water <- open_matrix["1", "0"]
open_mcnemar_exact <- binom.test(
  x = min(shifts_water_to_beverage, shifts_beverage_to_water),
  n = shifts_water_to_beverage + shifts_beverage_to_water,
  p = .5
)
print(open_mcnemar_exact)



# -----------------------------------------------------------------------------
# 13. DIFFERENZE INDIVIDUALI: SST-12, MAIA, TAS (ESPLORATIVE)  (SLIDE 16; 13a anche SLIDE 14)
# -----------------------------------------------------------------------------
maia_tas_vars <- c(
  "maia_noticing", "maia_not_distracting", "maia_not_worrying",
  "maia_attention_regulation", "maia_emotional_awareness",
  "maia_self_regulation", "maia_body_listening", "maia_trusting", "tas"
)

# Funzione: correlazione di x con ogni variabile di una lista, FDR sulla lista
correlate_list <- function(variables, x, method) {
  bind_rows(lapply(variables, function(v) {
    test <- cor.test(wide[[v]], wide[[x]], method = method, exact = FALSE)
    tibble(variable = v, with = x, method = method,
           coefficient = unname(test$estimate), p = test$p.value)
  })) %>%
    mutate(p_fdr = p.adjust(p, method = "BH"))
}

# 13a. Analisi mirate (FDR dentro ogni famiglia)
section("13a. SST-12 / MAIA / TAS: analisi mirate")

# SST-12 con i cambiamenti nutrizionali (Spearman, FDR su 5)
nutrition_sniffing <- correlate_list(
  c("diff_kcal", "diff_sugar", "diff_fat", "nutritional_composite_raw", "nutritional_composite_z"),
  "sniffing", "spearman")
# SST-12 con i cambiamenti di gusto (Pearson, FDR su 4)
taste_sniffing <- correlate_list(
  c("diff_sweet", "diff_salt", "diff_sour", "diff_bitter"), "sniffing", "pearson")
# Cambiamento della quantita' stimata con MAIA (8 sottoscale) e TAS (Spearman, FDR su 9)
ingestion_maia_tas <- correlate_list(maia_tas_vars, "diff_ingested_estimate", "spearman")
# Cambiamento della quantita' stimata con i cambiamenti nutrizionali (Pearson, FDR su 4)
ingestion_nutrition <- correlate_list(
  c("diff_kcal", "diff_sugar", "diff_fat", "nutritional_composite_z"),
  "diff_ingested_estimate", "pearson")

targeted_correlations <- bind_rows(nutrition_sniffing, taste_sniffing,
                                   ingestion_maia_tas, ingestion_nutrition)
print_sig(targeted_correlations)

# 13b. MAIA e TAS con i cambiamenti nutrizionali e di gusto
#      Due famiglie mirate, correlazioni di Spearman (come per la quantita'
#      stimata), FDR dentro ogni famiglia:
#        - indice nutrizionale composito x 9 punteggi MAIA/TAS (9 test);
#        - 4 cambiamenti di gusto x 9 punteggi MAIA/TAS (36 test).
section("13b. MAIA E TAS CON NUTRIZIONE (composito) E GUSTO")

maia_tas_family <- function(effect_vars) {
  out <- tidyr::expand_grid(effect = effect_vars, moderator = maia_tas_vars)
  out$rho <- NA_real_
  out$p   <- NA_real_
  for (i in seq_len(nrow(out))) {
    test <- cor.test(wide[[out$effect[i]]], wide[[out$moderator[i]]],
                     method = "spearman", exact = FALSE)
    out$rho[i] <- unname(test$estimate)
    out$p[i]   <- test$p.value
  }
  out$p_fdr <- p.adjust(out$p, method = "BH")
  out
}

nutrition_maia_tas <- maia_tas_family("nutritional_composite_z")
taste_maia_tas     <- maia_tas_family(c("diff_sweet", "diff_salt", "diff_sour", "diff_bitter"))
print_sig(nutrition_maia_tas)   # tutti pFDR >= .891
print_sig(taste_maia_tas)       # tutti pFDR >= .737

# 13c. SST-12 e cambiamenti nutrizionali: analisi dettagliata
#   Per calorie, zuccheri, grassi e composito z:
#   (1) Pearson; (2) regressione lineare; (3) errori standard robusti HC3;
#   (4) bootstrap della pendenza; (5) leave-one-out e distanza di Cook;
#   (6) Spearman; (7) senza i 3 partecipanti nel range anosmico (SST-12 <= 6);
#   (8) effetto del pod in chi e' sopra il range anosmico;
#   (9) modello misto equivalente sulle valutazioni ripetute.
#   FDR sulle tre proprieta' (il composito e' riportato a parte).
section("13c. SST-12 E CAMBIAMENTI NUTRIZIONALI")

sst_outcomes <- c("Calories" = "diff_kcal", "Sugar" = "diff_sugar", "Fat" = "diff_fat",
                  "Nutritional composite (z)" = "nutritional_composite_z")
sst_ratings  <- c("Calories" = "kcal", "Sugar" = "sugar", "Fat" = "fat")
anosmic_cutoff <- 6

cat(sprintf("SST-12: M = %.2f, SD = %.2f, range %d-%d; nel range anosmico (<= 6): %d\n",
            mean(subject_traits$sniffing), sd(subject_traits$sniffing),
            min(subject_traits$sniffing), max(subject_traits$sniffing),
            sum(subject_traits$sniffing <= anosmic_cutoff)))

# Dati ripetuti con condizione 0/1 e SST-12 centrato (per il modello misto)
dat_sst <- dat %>%
  mutate(condition_pod = if_else(condition == "P", 1, 0),
         sniffing_c    = sniffing - mean(subject_traits$sniffing))

sst_analysis <- function(outcome_label) {
  d1 <- wide %>% transmute(subject, sniffing, y = .data[[sst_outcomes[[outcome_label]]]])
  d1_above <- filter(d1, sniffing > anosmic_cutoff)

  # (1) Pearson
  r_test <- cor.test(d1$y, d1$sniffing, method = "pearson")
  # (2) regressione: cambiamento ~ SST-12
  ols <- lm(y ~ sniffing, data = d1)
  ols_coef <- coef(summary(ols))["sniffing", ]
  ols_ci   <- confint(ols)["sniffing", ]
  # (3) errori standard robusti HC3
  hc3_se <- sqrt(diag(sandwich::vcovHC(ols, type = "HC3")))[["sniffing"]]
  hc3_p  <- 2 * pt(-abs(ols_coef[["Estimate"]] / hc3_se), df = df.residual(ols))
  # (4) bootstrap della pendenza (10.000 ricampionamenti dei partecipanti)
  set.seed(2026)
  boot_slopes <- replicate(10000, {
    rows <- sample(seq_len(nrow(d1)), replace = TRUE)
    coef(lm(y ~ sniffing, data = d1[rows, ]))[["sniffing"]]
  })
  # (5) leave-one-out: p della pendenza togliendo un partecipante alla volta
  loo_p <- sapply(seq_len(nrow(d1)), function(i)
    coef(summary(lm(y ~ sniffing, data = d1[-i, ])))["sniffing", "Pr(>|t|)"])
  cooks <- cooks.distance(ols)
  # (6) Spearman
  rho_test <- cor.test(d1$y, d1$sniffing, method = "spearman", exact = FALSE)
  # (7) senza il range anosmico
  r_above   <- cor.test(d1_above$y, d1_above$sniffing, method = "pearson")
  ols_above <- lm(y ~ sniffing, data = d1_above)
  # (8) e (9) solo per le tre proprieta' (non per il composito z)
  change_mean <- NA_real_; change_V <- NA_real_; change_p <- NA_real_
  lmm_coef <- c(Estimate = NA_real_, `Std. Error` = NA_real_, df = NA_real_, `Pr(>|t|)` = NA_real_)
  if (outcome_label %in% names(sst_ratings)) {
    # (8) il pod alza la valutazione anche in chi e' sopra il range anosmico?
    change_test <- wilcox.test(d1_above$y, mu = 0, exact = FALSE)
    change_mean <- mean(d1_above$y)
    change_V    <- unname(change_test$statistic)
    change_p    <- change_test$p.value
    # (9) modello misto: valutazione ~ condizione x SST-12 + (1 | partecipante)
    lmm_data <- dat_sst %>% mutate(y = .data[[sst_ratings[[outcome_label]]]])
    lmm <- lmerTest::lmer(y ~ condition_pod * sniffing_c + (1 | subject),
                          data = lmm_data, REML = TRUE)
    lmm_coef <- coef(summary(lmm))["condition_pod:sniffing_c", ]
  }

  tibble(
    outcome = outcome_label, n = nrow(d1),
    r = unname(r_test$estimate), r_ci_lower = r_test$conf.int[1], r_ci_upper = r_test$conf.int[2],
    p = r_test$p.value,
    b = unname(ols_coef["Estimate"]), se = unname(ols_coef["Std. Error"]),
    b_ci_lower = unname(ols_ci[1]), b_ci_upper = unname(ols_ci[2]),
    t = unname(ols_coef["t value"]), df = df.residual(ols),
    hc3_se = hc3_se, hc3_p = hc3_p,
    boot_b_ci_lower = unname(quantile(boot_slopes, .025)),
    boot_b_ci_upper = unname(quantile(boot_slopes, .975)),
    lmm_b = unname(lmm_coef["Estimate"]), lmm_se = unname(lmm_coef["Std. Error"]),
    lmm_df = unname(lmm_coef["df"]), lmm_p = unname(lmm_coef["Pr(>|t|)"]),
    spearman_rho = unname(rho_test$estimate), spearman_p = rho_test$p.value,
    max_leave_one_out_p = max(loo_p),
    max_cooks_distance = max(cooks),
    max_cooks_subject = as.character(d1$subject[which.max(cooks)]),
    n_above_anosmic_range = nrow(d1_above),
    r_above_anosmic_range = unname(r_above$estimate),
    r_ci_lower_above_anosmic_range = r_above$conf.int[1],
    r_ci_upper_above_anosmic_range = r_above$conf.int[2],
    p_above_anosmic_range = r_above$p.value,
    b_above_anosmic_range = coef(ols_above)[["sniffing"]],
    mean_pod_change_above_anosmic_range = change_mean,
    wilcoxon_V_pod_change_above_anosmic_range = change_V,
    p_pod_change_above_anosmic_range = change_p
  )
}

sst_results <- bind_rows(lapply(names(sst_outcomes), sst_analysis))

# FDR sulle tre proprieta' (calorie, zuccheri, grassi)
sst_results$p_fdr <- NA_real_
three_properties <- sst_results$outcome %in% names(sst_ratings)
sst_results$p_fdr[three_properties] <- p.adjust(sst_results$p[three_properties], method = "BH")
sst_results <- relocate(sst_results, p_fdr, .after = p)

print_sig(sst_results %>% select(outcome, r, p, p_fdr, hc3_p, spearman_rho, spearman_p,
                                 max_leave_one_out_p, r_above_anosmic_range,
                                 p_above_anosmic_range, p_pod_change_above_anosmic_range))

# Modello misto congiunto sulle tre proprieta' (stessa struttura d'errore della sezione 6).
# Il suo test condizione x SST-12 equivale alla regressione della media delle
# tre differenze sull'SST-12.
sst_joint_data <- dat_sst %>%
  select(subject, condition, sniffing_c, kcal, sugar, fat) %>%
  pivot_longer(c(kcal, sugar, fat), names_to = "property", values_to = "rating") %>%
  mutate(property = factor(property), condition = factor(condition))

sst_joint_model <- lmerTest::lmer(
  rating ~ condition * property * sniffing_c +
    (1 | subject) + (1 | subject:property) + (1 | subject:condition),
  data = sst_joint_data, REML = TRUE
)
sst_joint_anova <- as.data.frame(anova(sst_joint_model, type = 3))
print_sig(sst_joint_anova)

# CONTROLLO: la riga condition:sniffing_c del modello misto equivale alla
# regressione della media GREZZA delle tre differenze (composito grezzo)
# sull'SST-12: stesso p (.0385). Per questo serve il composito grezzo.
pooled_ols <- lm(nutritional_composite_raw ~ sniffing, data = wide)
cat("Composito grezzo ~ SST-12: b =", round(coef(pooled_ols)[["sniffing"]], 2),
    "| p =", round(coef(summary(pooled_ols))["sniffing", "Pr(>|t|)"], 4),
    "| modello misto p =", round(sst_joint_anova["condition:sniffing_c", "Pr(>F)"], 4), "\n")


# -----------------------------------------------------------------------------
# 13d. SST-12 × GUSTO
# -----------------------------------------------------------------------------
section("13d. SST-12 x cambiamenti di gusto")

# Pearson: quattro cambiamenti di gusto correlati con SST-12.
# FDR sui quattro test di questa famiglia.
taste_sniffing <- correlate_list(
  c("diff_sweet", "diff_salt", "diff_sour", "diff_bitter"),
  "sniffing",
  "pearson"
)

print_sig(taste_sniffing)
save_table(
  taste_sniffing,
  "Table_S5d_Pilot2_SST12_taste.csv"
)


# -----------------------------------------------------------------------------
# 13e. MAIA/TAS × QUANTITA' STIMATA
# -----------------------------------------------------------------------------
section("13e. MAIA/TAS x cambiamento della quantita' stimata")

# Spearman: otto sottoscale MAIA e TAS.
# FDR sui nove test di questa famiglia.
ingestion_maia_tas <- correlate_list(
  maia_tas_vars,
  "diff_ingested_estimate",
  "spearman"
)

print_sig(ingestion_maia_tas)
save_table(
  ingestion_maia_tas,
  "Table_S5e_Pilot2_MAIA_TAS_estimated_intake.csv"
)


# -----------------------------------------------------------------------------
# 13f. SST-12 × QUANTITA' STIMATA
# -----------------------------------------------------------------------------
section("13f. SST-12 x cambiamento della quantita' stimata")

# Spearman: un solo test.
# La funzione restituisce p_fdr uguale a p per una famiglia di un test.
ingestion_sniffing <- correlate_list(
  "diff_ingested_estimate",
  "sniffing",
  "spearman"
)

print_sig(ingestion_sniffing)
save_table(
  ingestion_sniffing,
  "Table_S5f_Pilot2_SST12_estimated_intake.csv"
)


# -----------------------------------------------------------------------------
# 13g. MATRICE GLOBALE EFFETTI × MODERATORI
# -----------------------------------------------------------------------------
section("13g. Matrice globale effetti x moderatori")

# Undici effetti del pod, tutti calcolati come P meno A.
primary_effect_indices <- c(
  "diff_kcal",
  "diff_sugar",
  "diff_fat",
  "nutritional_composite_z",
  "diff_sweet",
  "diff_salt",
  "diff_sour",
  "diff_bitter",
  "diff_ingested_estimate",
  "diff_intensity",
  "diff_pleasantness"
)

# Dieci moderatori: SST-12, otto sottoscale MAIA e TAS.
moderators <- c("sniffing", maia_tas_vars)

global_pairs <- tidyr::expand_grid(
  effect = primary_effect_indices,
  moderator = moderators
)

# Spearman per tutte le 110 combinazioni.
all_moderator_correlations <- bind_rows(lapply(
  seq_len(nrow(global_pairs)), function(i) {
    effect <- global_pairs$effect[i]
    moderator <- global_pairs$moderator[i]
    
    x <- wide[[effect]]
    y <- wide[[moderator]]
    ok <- complete.cases(x, y)
    
    test <- cor.test(
      x[ok], y[ok],
      method = "spearman",
      exact = FALSE
    )
    
    tibble(
      effect = effect,
      moderator = moderator,
      n = sum(ok),
      method = "Spearman",
      rho = unname(test$estimate),
      p = test$p.value
    )
  }
)) %>%
  # Un'unica correzione FDR sull'intera matrice di 110 test.
  mutate(p_fdr_global = p.adjust(p, method = "BH"))

print_sig(all_moderator_correlations)
save_table(
  all_moderator_correlations,
  "Table_S5g_Pilot2_global_moderator_correlations.csv"
)

cat(sprintf(
  "\nMatrice globale: %d test; minimo pFDR = %.6f; associazioni con pFDR < .05 = %d\n",
  nrow(all_moderator_correlations),
  min(all_moderator_correlations$p_fdr_global),
  sum(all_moderator_correlations$p_fdr_global < .05)
))

# -----------------------------------------------------------------------------
# 14. FIGURE
# -----------------------------------------------------------------------------
condition_labels <- c("A" = "No Olfactory Pod", "P" = "Olfactory Pod")
condition_colors <- c("A" = "#4C78A8", "P" = "#E45756")

base_theme <- theme_classic(base_size = 12) +
  theme(strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 0, hjust = .5))

# Funzione per i pannelli "pod vs no pod" con le linee dei singoli partecipanti
# (Il pannello del gusto usa punti e linee un po' piu' piccoli.)
paired_panel <- function(long_data, y, facet, annotations, title, subtitle, star_size,
                         line_color = "grey75", line_width = .3, line_alpha = .65,
                         point_size = 1.7, point_alpha = .8, mean_size = 3.4) {
  ggplot(long_data, aes(x = condition, y = .data[[y]], group = subject)) +
    geom_line(color = line_color, linewidth = line_width, alpha = line_alpha) +
    geom_point(aes(color = condition), position = position_jitter(width = .035),
               size = point_size, alpha = point_alpha) +
    stat_summary(aes(group = condition), fun = mean, geom = "point",
                 shape = 18, size = mean_size, color = "black") +
    facet_wrap(as.formula(paste("~", facet)), nrow = 1) +
    scale_color_manual(values = condition_colors, labels = condition_labels) +
    scale_x_discrete(labels = condition_labels) +
    coord_cartesian(ylim = c(0, 110), clip = "off") +
    geom_text(data = annotations, aes(x = 1.5, y = 106, label = significance),
              inherit.aes = FALSE, size = star_size) +
    labs(title = title, subtitle = subtitle, x = NULL, y = "Rating (0-100)", color = NULL) +
    base_theme
}

# 14a. Figura principale (pannelli A-E) = Figura S3 (slide 9, 11, 14, 15)
p_nutrition <- paired_panel(nutrition_long, "nutritional_rating", "nutritional_property",
                            nutrition_contrasts, "A. Pod-induced nutritional attribution",
                            "All planned ART-C contrasts: pFDR < .001", 5)
p_taste <- paired_panel(taste_long, "taste_rating", "taste", taste_contrasts,
                        "B. Pod-induced taste profile", "FDR-corrected ART-C comparisons", 4.5,
                        line_color = "grey78", line_width = .28, line_alpha = .6,
                        point_size = 1.5, point_alpha = .75, mean_size = 3.2)

p_ingestion <- wide %>%
  select(subject, `No Olfactory Pod` = ingested_ml_estimate_A,
         `Olfactory Pod` = ingested_ml_estimate_P) %>%
  pivot_longer(-subject, names_to = "condition_plot", values_to = "estimated_ml") %>%
  ggplot(aes(x = condition_plot, y = estimated_ml, group = subject)) +
  geom_line(color = "grey72", linewidth = .35, alpha = .7) +
  geom_point(aes(color = condition_plot), size = 1.8, alpha = .8) +
  stat_summary(aes(group = condition_plot), fun = mean, geom = "point",
               shape = 18, size = 3.5, color = "black") +
  scale_color_manual(values = c("No Olfactory Pod" = "#4C78A8", "Olfactory Pod" = "#E45756")) +
  annotate("text", x = 1.5,
           y = max(wide$ingested_ml_estimate_A, wide$ingested_ml_estimate_P) + 25,
           label = "t(46) = -2.27, p = .028", size = 4) +
  labs(title = "C. Estimated amount consumed", subtitle = "Actual intake was fixed at 200 ml",
       x = NULL, y = "Estimated amount (ml)", color = NULL) +
  coord_cartesian(clip = "off") +
  base_theme

intensity_pleasantness_long <- dat %>%
  select(subject, condition, intensity, pleasantness) %>%
  pivot_longer(c(intensity, pleasantness), names_to = "outcome", values_to = "rating") %>%
  mutate(outcome = factor(outcome, levels = c("intensity", "pleasantness"),
                          labels = c("Intensity", "Pleasantness")))
ip_annotations <- tibble(outcome = factor(c("Intensity", "Pleasantness")),
                         significance = c("***", "ns"))
p_ip <- paired_panel(intensity_pleasantness_long, "rating", "outcome", ip_annotations,
                     "D. Intensity and pleasantness", "Paired Wilcoxon signed-rank tests", 5)

open_plot_data <- open_by_condition %>%
  mutate(condition = factor(condition, levels = c("A", "P"), labels = condition_labels),
         response = factor(binary_code, levels = c(0, 1),
                           labels = c("Plain water", "Attributed beverage identity")))
p_open <- ggplot(open_plot_data, aes(x = condition, y = percent, fill = response)) +
  geom_col(width = .68, color = "white") +
  geom_text(aes(label = paste0(n, "/", N, "\n(", sprintf("%.1f", percent), "%)")),
            position = position_stack(vjust = .5), size = 4) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     expand = expansion(mult = c(0, .08))) +
  labs(title = "E. Open-ended beverage identity",
       subtitle = "Binary coding of plain water versus attributed beverage identity",
       x = NULL, y = "Participants (%)", fill = NULL) +
  base_theme

main_figure <- (p_nutrition / p_taste) / (p_ingestion | p_ip | p_open) +
  plot_annotation(
    title = "Pilot 2: Comparison of the Olfactory Pod versus No Olfactory Pod contrast",
    caption = "*** p < .001; ** p < .01; * p < .05; ns = non-significant. Nutritional and taste analyses use ART/ART-C."
  )
ggsave(file.path(figures_folder, "Pilot2_main_results_panels.png"), main_figure,
       width = 16, height = 15, dpi = 300, bg = "white")
ggsave(file.path(figures_folder, "Pilot2_main_results_panels.pdf"), main_figure,
       width = 16, height = 15, bg = "white")

# 14b. Regressioni esplorative (associazioni descrittive a due variabili) = Figura S4 (slide 13)
scatter_panel <- function(x, y, title, subtitle, x_label, y_label) {
  ggplot(wide, aes(.data[[x]], .data[[y]])) +
    geom_point(size = 2, alpha = .8) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
    labs(title = title, subtitle = subtitle, x = x_label, y = y_label) +
    base_theme + theme(legend.position = "none")
}
exploratory_figure <-
  (scatter_panel("diff_sweet", "nutritional_composite_z", "Sweetness and nutritional attribution",
                 "Full-model coefficient: b = .019, p < .001",
                 "\u0394 sweetness\n(Olfactory Pod \u2212 No Olfactory Pod)", "Nutritional composite (z)") |
   scatter_panel("diff_bitter", "nutritional_composite_z", "Bitterness and nutritional attribution",
                 "Full-model coefficient: b = .030, p = .031",
                 "\u0394 bitterness\n(Olfactory Pod \u2212 No Olfactory Pod)", "Nutritional composite (z)") |
   scatter_panel("diff_bitter", "diff_fat", "Selective bitterness-fat association",
                 "Fat-model coefficient: b = .637, p = .011",
                 "\u0394 bitterness\n(Olfactory Pod \u2212 No Olfactory Pod)",
                 "\u0394 perceived fat\n(Olfactory Pod \u2212 No Olfactory Pod)")) +
  plot_annotation(
    title = "Pilot 2: Exploratory taste\u2013nutrition associations",
    caption = paste("Points and lines with 95% confidence bands show descriptive bivariate associations.",
                    "Reported coefficients derive from multiple regression models including",
                    "pod-related changes in sweetness, saltiness, sourness and bitterness."),
    tag_levels = "A",
    theme = theme(plot.title = element_text(face = "bold", size = 15),
                  plot.caption = element_text(size = 9.5, hjust = 0),
                  plot.tag = element_text(face = "bold", size = 14))
  )
ggsave(file.path(figures_folder, "Pilot2_exploratory_regressions.png"), exploratory_figure,
       width = 14, height = 5, dpi = 300, bg = "white")
ggsave(file.path(figures_folder, "Pilot2_exploratory_regressions.pdf"), exploratory_figure,
       width = 14, height = 5, bg = "white")

# 14c. SST-12 e cambiamenti nutrizionali = Figura S5 (slide 16)
format_r <- function(x) sub("^(-?)0\\.", "\\1.", sprintf("%.2f", x))

sst_plot_data <- wide %>%
  select(subject, sniffing, all_of(unname(sst_outcomes))) %>%
  pivot_longer(all_of(unname(sst_outcomes)), names_to = "diff_var", values_to = "pod_change") %>%
  mutate(
    outcome = factor(names(sst_outcomes)[match(diff_var, sst_outcomes)], levels = names(sst_outcomes)),
    sst_range = factor(if_else(sniffing <= anosmic_cutoff,
                               "SST-12 \u2264 6 (anosmic range)", "SST-12 \u2265 7"),
                       levels = c("SST-12 \u2265 7", "SST-12 \u2264 6 (anosmic range)"))
  )

sst_plot_labels <- sst_results %>%
  mutate(
    outcome = factor(outcome, levels = names(sst_outcomes)),
    label = paste0(
      "N = 47: r = ", format_r(r), " [", format_r(r_ci_lower), ", ", format_r(r_ci_upper),
      "], p = ", format_p(p),
      if_else(is.na(p_fdr), "", paste0(", pFDR = ", format_p(p_fdr))),
      "\nSST-12 \u2265 7: r = ", format_r(r_above_anosmic_range),
      " [", format_r(r_ci_lower_above_anosmic_range), ", ",
      format_r(r_ci_upper_above_anosmic_range), "], p = ", format_p(p_above_anosmic_range)
    )
  )

sst_figure <- ggplot(sst_plot_data, aes(sniffing, pod_change)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey55") +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              color = "#4C78A8", fill = "#4C78A8", alpha = .15) +
  geom_smooth(data = filter(sst_plot_data, sniffing > anosmic_cutoff),
              method = "lm", formula = y ~ x, se = FALSE,
              color = "grey25", linetype = "dashed", linewidth = .7) +
  geom_point(aes(shape = sst_range),
             position = position_jitter(width = .12, height = 0, seed = 2026),
             size = 2, alpha = .85, fill = "white") +
  geom_text(data = sst_plot_labels, aes(x = -Inf, y = Inf, label = label),
            hjust = -0.03, vjust = 1.2, size = 3.0, lineheight = 1.05, inherit.aes = FALSE) +
  facet_wrap(~ outcome, nrow = 2, scales = "free_y") +
  scale_shape_manual(values = c(16, 24)) +
  scale_x_continuous(breaks = 5:11) +
  scale_y_continuous(expand = expansion(mult = c(.05, .32))) +
  labs(x = "SST-12 identification score (0-12)",
       y = "Pod-related change\n(Olfactory Pod \u2212 No Olfactory Pod)", shape = NULL) +
  base_theme +
  plot_annotation(
    title = "Pilot 2: Olfactory identification and pod-related nutritional attribution",
    caption = paste0("Solid lines and bands: linear fit with 95% CI (N = 47). ",
                     "Dashed lines: linear fit excluding participants with SST-12 \u2264 6 (n = 3).\n",
                     "Calories, sugar and fat in rating points (0-100 scale); composite in z units. ",
                     "Points are jittered horizontally."),
    theme = theme(plot.title = element_text(face = "bold", size = 15),
                  plot.caption = element_text(size = 9.5, hjust = 0))
  )
ggsave(file.path(figures_folder, "Pilot2_SST12_nutrition.png"), sst_figure,
       width = 11, height = 9, dpi = 300, bg = "white")
ggsave(file.path(figures_folder, "Pilot2_SST12_nutrition.pdf"), sst_figure,
       width = 11, height = 9, bg = "white", device = cairo_pdf)

# 14d. FIGURE PER LE SLIDE 10 e 12 - NON INCLUSE NEL SUPPLEMENTARY
#      Aumento con il pod per proprieta' e per gusto, con i confronti ART-C
#      (i risultati sono riportati nel testo del Supplementary e in Table S2b)
p_label <- function(contrasts, comparison_name) {
  p <- contrasts$p_fdr[contrasts$comparison == comparison_name]
  if (p < .001) "pFDR < .001" else paste0("pFDR = ", format_p(p))
}

change_plot <- function(changes, levels_order, brackets, title, subtitle) {
  set.seed(2026)
  plot_data <- changes %>%
    pivot_longer(all_of(levels_order), names_to = "level", values_to = "change") %>%
    mutate(level = factor(level, levels = levels_order),
           x_j = as.numeric(level) + runif(n(), -.04, .04))
  summary_data <- plot_data %>%
    group_by(level) %>%
    summarise(mean = mean(change), sd = sd(change), n = n(),
              ci_low  = mean - qt(.975, n - 1) * sd / sqrt(n),
              ci_high = mean + qt(.975, n - 1) * sd / sqrt(n), .groups = "drop")
  brackets <- brackets %>% mutate(y = max(plot_data$change) + 6 + 12 * (row - 1))

  ggplot(plot_data, aes(x = x_j, y = change)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey55") +
    geom_line(aes(group = subject), color = "grey78", linewidth = .3, alpha = .6) +
    geom_point(color = "#E45756", size = 1.7, alpha = .8) +
    geom_errorbar(data = summary_data, aes(x = as.numeric(level), ymin = ci_low, ymax = ci_high),
                  inherit.aes = FALSE, width = .08, linewidth = .7) +
    geom_point(data = summary_data, aes(x = as.numeric(level), y = mean),
               inherit.aes = FALSE, shape = 18, size = 4, color = "black") +
    geom_segment(data = brackets, aes(x = x1, xend = x2, y = y, yend = y), inherit.aes = FALSE) +
    geom_segment(data = brackets, aes(x = x1, xend = x1, y = y, yend = y - 2.5), inherit.aes = FALSE) +
    geom_segment(data = brackets, aes(x = x2, xend = x2, y = y, yend = y - 2.5), inherit.aes = FALSE) +
    geom_text(data = brackets, aes(x = (x1 + x2) / 2, y = y + 3.5, label = label),
              inherit.aes = FALSE, size = 3.6) +
    scale_x_continuous(breaks = seq_along(levels_order), labels = levels_order,
                       expand = expansion(add = .4)) +
    labs(title = title, subtitle = subtitle, x = NULL,
         y = "Olfactory Pod \u2212 No Olfactory Pod (rating points)") +
    coord_cartesian(clip = "off") +
    theme_classic(base_size = 12) +
    theme(plot.title = element_text(face = "bold"),
          axis.text.x = element_text(size = 12, color = "black"))
}

nutrition_change_figure <- change_plot(
  nutrition_changes_named, c("Fat", "Calories", "Sugar"),
  tibble(x1 = c(1, 2, 1), x2 = c(2, 3, 3), row = c(1, 1, 2),
         label = c(p_label(nutrition_interaction_contrasts, "Fat - Calories"),
                   p_label(nutrition_interaction_contrasts, "Calories - Sugar"),
                   p_label(nutrition_interaction_contrasts, "Fat - Sugar"))),
  title = "Pod-related change by nutritional property",
  subtitle = "ART-C interaction contrasts, Benjamini-Hochberg correction"
)

sweet_rows <- str_detect(taste_interaction_contrasts$comparison, "Sweet")
taste_change_figure <- change_plot(
  taste_changes_named, c("Bitter", "Sour", "Sweet", "Salt"),
  tibble(x1 = c(2, 3, 1), x2 = c(3, 4, 3), row = c(1, 1, 2),
         label = c(p_label(taste_interaction_contrasts, "Sour - Sweet"),
                   p_label(taste_interaction_contrasts, "Sweet - Salt"),
                   p_label(taste_interaction_contrasts, "Bitter - Sweet"))),
  title = "Pod-related change by taste",
  subtitle = paste0("ART-C interaction contrasts, Benjamini-Hochberg correction\n",
                    "Bitter, sour and salt did not differ from each other (all pFDR \u2265 ",
                    format_p(min(taste_interaction_contrasts$p_fdr[!sweet_rows])), ")")
)

ggsave(file.path(figures_folder, "Additional_Pilot2_nutrition_change_comparison.png"),
       nutrition_change_figure, width = 7, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(figures_folder, "Additional_Pilot2_nutrition_change_comparison.pdf"),
       nutrition_change_figure, width = 7, height = 5.5, bg = "white", device = cairo_pdf)
ggsave(file.path(figures_folder, "Additional_Pilot2_taste_change_comparison.png"),
       taste_change_figure, width = 8, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(figures_folder, "Additional_Pilot2_taste_change_comparison.pdf"),
       taste_change_figure, width = 8, height = 5.5, bg = "white", device = cairo_pdf)

# -----------------------------------------------------------------------------
# 15. CONTROLLO FINALE: i numeri principali delle slide
# -----------------------------------------------------------------------------
# Confronta i risultati con i valori riportati nel Supplementary e nelle slide.
# Se una riga dice "NON TORNA", qualcosa e' cambiato (dati o script).
section("15. CONTROLLO FINALE")

value_of <- function(table, column, row_condition) table[[column]][row_condition]
final_check <- tibble(
  slide = c("9", "9", "9", "11", "11", "11", "13", "13", "13", "14", "14", "14", "15", "16", "16"),
  result = c(
    "Nutrition ART interaction F", "Fat ART-C pFDR < .001", "Standardized alpha",
    "Taste ART interaction F", "Sour ART-C pFDR", "Salt ART-C pFDR",
    "Composite R2", "Sweet -> composite p", "Bitter -> fat pFDR (3 models)",
    "Estimated amount t", "Absolute error p", "Pleasantness p",
    "Open-ended McNemar p < .001", "SST-12 x calories pFDR", "SST-12 x taste min pFDR"),
  obtained = c(
    value_of(nutrition_anova, "F value", grepl(":", nutrition_anova$Term)),
    as.numeric(max(nutrition_contrasts$p_fdr) < .001),
    standardized_alpha,
    value_of(taste_anova, "F value", grepl(":", taste_anova$Term)),
    value_of(taste_contrasts, "p_fdr", taste_contrasts$taste == "Sour"),
    value_of(taste_contrasts, "p_fdr", taste_contrasts$taste == "Salt"),
    glance(composite_model)$r.squared,
    coef(summary(composite_model))["diff_sweet", "Pr(>|t|)"],
    value_of(regression_fdr, "p_fdr", regression_fdr$model == "Fat" & regression_fdr$term == "diff_bitter"),
    unname(ingestion_t$statistic),
    absolute_error_wilcox$p.value,
    pleasantness_wilcox$p.value,
    as.numeric(open_mcnemar_exact$p.value < .001),
    value_of(sst_results, "p_fdr", sst_results$outcome == "Calories"),
    min(taste_sniffing$p_fdr)),
  expected = c(47.49, 1, .82, 82.25, .011, .900, .381, .0001, .034,
               -2.27, .668, .631, 1, .066, .723)
) %>%
  mutate(check = if_else(abs(round(obtained, 4) - expected) <= .006, "OK", "NON TORNA"))
print(as.data.frame(final_check), digits = 4)

# -----------------------------------------------------------------------------
# 16. SALVATAGGIO DEI RISULTATI
# -----------------------------------------------------------------------------
# Questa sezione non fa analisi nuove: mette i risultati delle sezioni
# precedenti nelle tabelle del Supplementary (outputs/Pilot2/tables).
section("16. SALVATAGGIO DEI RISULTATI")

na_chr <- NA_character_
na_num <- NA_real_

# Righe dell'interazione dalla replica "a mano" dell'ART (sezioni 6 e 8)
manual_row <- function(model, stratum) {
  tab <- summary(model)[[stratum]][[1]]
  list(F.value = tab[1, "F value"], Df = tab[1, "Df"], Pr..F. = tab[1, "Pr(>F)"])
}
nutrition_interaction_row <- manual_row(nutrition_manual_art, "Error: subject:nutritional_property:condition")
taste_interaction_row     <- manual_row(taste_manual_art, "Error: subject:taste:condition")

# --- Table S1: descrittive ---------------------------------------------------
save_table(descriptives, "Table_S1_Pilot2_descriptives.csv")

# --- Table S2: normalita', ART, contrasti ART-C, coerenza --------------------
contrast_rows <- function(contrasts, domain, outcome_column) {
  contrasts %>%
    transmute(record_type = "Planned contrast", domain = domain,
              outcome = as.character(.data[[outcome_column]]),
              term = "No Olfactory Pod versus Olfactory Pod", comparison = as.character(contrast),
              estimate = estimate, SE = SE, statistic = t.ratio, df1 = na_num, df2 = df,
              p_raw = p.value, p_adjusted = p_fdr, adjustment = "Benjamini-Hochberg FDR",
              r = na_num, standardized_alpha = na_num, note = "ART-C rank-scale estimate")
}
omnibus_row <- function(domain, outcome, term, row, df2, note) {
  tibble(record_type = "Omnibus interaction", domain = domain, outcome = outcome, term = term,
         comparison = na_chr, estimate = na_num, SE = na_num, statistic = row$F.value,
         df1 = row$Df, df2 = df2, p_raw = row$Pr..F., p_adjusted = na_num,
         adjustment = na_chr, r = na_num, standardized_alpha = na_num, note = note)
}
table_s2 <- bind_rows(
  normality %>%
    transmute(record_type = "Normality diagnostic", domain = model, outcome = na_chr,
              term = "Residual Shapiro-Wilk test", comparison = na_chr, estimate = na_num,
              SE = na_num, statistic = residual_shapiro_W, df1 = na_num, df2 = na_num,
              p_raw = residual_shapiro_p, p_adjusted = na_num, adjustment = na_chr,
              r = na_num, standardized_alpha = na_num, note = analysis_selected),
  omnibus_row("Nutritional attribution", "Fat, Calories, Sugar", "Condition × Nutritional Property",
              nutrition_interaction_row, 92, "Aligned Rank Transform"),
  contrast_rows(nutrition_contrasts, "Nutritional attribution", "nutritional_property"),
  omnibus_row("Taste profile", "Sweetness, Saltiness, Sourness, Bitterness", "Condition × Taste",
              taste_interaction_row, 138, "Repeated-measures Aligned Rank Transform"),
  contrast_rows(taste_contrasts, "Taste profile", "taste"),
  nutrition_cor_pairs %>%
    transmute(record_type = "Coherence", domain = "Nutritional attribution",
              outcome = paste(index_1, "with", index_2), term = "Pearson correlation",
              comparison = na_chr, estimate = na_num, SE = na_num, statistic = na_num,
              df1 = na_num, df2 = na_num, p_raw = na_num, p_adjusted = na_num,
              adjustment = na_chr, r = r, standardized_alpha = na_num,
              note = "Correlation between Pod-minus-Control modulation indices"),
  tibble(record_type = "Coherence", domain = "Nutritional attribution",
         outcome = "Calories, Sugar and Fat modulation indices", term = "Standardized Cronbach alpha",
         comparison = na_chr, estimate = na_num, SE = na_num, statistic = na_num,
         df1 = na_num, df2 = na_num, p_raw = na_num, p_adjusted = na_num, adjustment = na_chr,
         r = na_num, standardized_alpha = standardized_alpha,
         note = "Internal consistency of the three Pod-minus-Control indices")
)
save_table(table_s2, "Table_S2_Pilot2_nutrition_and_taste_ART.csv")

# --- Table S2b: confronto tra gli aumenti dovuti al pod (sezione 9) -----------
save_table(
  bind_rows(
    nutrition_interaction_contrasts %>% mutate(family = "Nutritional attribution", .before = 1) %>%
      mutate(method = "ART-C interaction contrast"),
    taste_interaction_contrasts %>% mutate(family = "Taste profile", .before = 1) %>%
      mutate(method = "ART-C interaction contrast"),
    nutrition_change_wilcoxon %>% mutate(family = "Nutritional attribution", .before = 1) %>%
      mutate(method = "Paired Wilcoxon on difference between changes (robustness)"),
    taste_change_wilcoxon %>% mutate(family = "Taste profile", .before = 1) %>%
      mutate(method = "Paired Wilcoxon on difference between changes (robustness)")
  ),
  "Table_S2b_Pilot2_change_comparisons.csv"
)

# --- Table S3: regressioni ---------------------------------------------------
regression_normality <- regression_normality %>%
  mutate(interpretation = if_else(residual_shapiro_p >= .05, "No evidence of non-normal residuals",
                                  "Possible non-normal residuals"))
regression_fit_full <- bind_rows(lapply(names(regression_models), function(m)
  glance(regression_models[[m]]) %>% mutate(model = m)))
table_s3 <- bind_rows(
  regression_fit_full %>%
    transmute(record_type = "Model fit", model, term = na_chr, estimate = na_num,
              std_error = na_num, statistic = statistic, df1 = df, df2 = df.residual,
              p = p.value, r_squared = r.squared, adjusted_r_squared = adj.r.squared,
              ci_lower = na_num, ci_upper = na_num, note = na_chr),
  regression_coefficients %>%
    transmute(record_type = "Coefficient", model, term, estimate, std_error = std.error,
              statistic, df1 = na_num, df2 = na_num, p = p.value, r_squared = na_num,
              adjusted_r_squared = na_num, ci_lower = na_num, ci_upper = na_num,
              note = "Multiple regression coefficient"),
  regression_normality %>%
    transmute(record_type = "Residual diagnostic", model, term = "Residual Shapiro-Wilk test",
              estimate = na_num, std_error = na_num, statistic = na_num, df1 = na_num,
              df2 = na_num, p = residual_shapiro_p, r_squared = na_num,
              adjusted_r_squared = na_num, ci_lower = na_num, ci_upper = na_num,
              note = interpretation),
  bootstrap_ci %>%
    transmute(record_type = "Bootstrap confidence interval", model = "Fat", term = predictor,
              estimate, std_error = na_num, statistic = na_num, df1 = na_num, df2 = na_num,
              p = na_num, r_squared = na_num, adjusted_r_squared = na_num,
              ci_lower = ci_lower, ci_upper = ci_upper,
              note = "Percentile bootstrap 95% CI; 10,000 resamples")
)
save_table(table_s3, "Table_S3_Pilot2_regressions.csv")

# --- Table S3b: correzione FDR delle regressioni sui tre modelli (sezione 10) ---
save_table(
  regression_fdr %>%
    transmute(
      model, term, estimate, std_error = std.error, statistic,
      p_raw = p.value, p_fdr,
      adjustment = "Benjamini-Hochberg FDR within predictor across the fat, calorie and sugar models"
    ),
  "Table_S3b_Pilot2_regression_FDR.csv"
)

# --- Table S4: quantita' stimata, intensita', piacevolezza --------------------
ingestion_shapiro <- shapiro.test(wide$diff_ingested_estimate)
intensity_shapiro <- shapiro.test(wide$intensity_P - wide$intensity_A)
pleasantness_shapiro <- shapiro.test(wide$pleasantness_P - wide$pleasantness_A)
table_s4 <- bind_rows(
  tibble(outcome = "Estimated amount consumed (ml)", descriptive_metric = "Mean (SD)",
         no_olfactory_pod = paste0(sprintf("%.2f", mean(wide$ingested_ml_estimate_A)), " (",
                                   sprintf("%.2f", sd(wide$ingested_ml_estimate_A)), ")"),
         olfactory_pod = paste0(sprintf("%.2f", mean(wide$ingested_ml_estimate_P)), " (",
                                sprintf("%.2f", sd(wide$ingested_ml_estimate_P)), ")"),
         difference_P_minus_A = mean(wide$diff_ingested_estimate), test = "Paired t-test",
         statistic = unname(ingestion_t$statistic), df = unname(ingestion_t$parameter),
         p = ingestion_t$p.value, ci_lower = ingestion_t$conf.int[1],
         ci_upper = ingestion_t$conf.int[2], shapiro_difference_p = ingestion_shapiro$p.value,
         note = "Actual intake was fixed at 200 ml in both conditions"),
  tibble(outcome = c("Intensity", "Pleasantness"), descriptive_metric = "Median",
         no_olfactory_pod = sprintf("%.2f", c(median(wide$intensity_A), median(wide$pleasantness_A))),
         olfactory_pod = sprintf("%.2f", c(median(wide$intensity_P), median(wide$pleasantness_P))),
         difference_P_minus_A = na_num, test = "Paired Wilcoxon signed-rank test",
         statistic = c(unname(intensity_wilcox$statistic), unname(pleasantness_wilcox$statistic)),
         df = na_num, p = c(intensity_wilcox$p.value, pleasantness_wilcox$p.value),
         ci_lower = na_num, ci_upper = na_num,
         shapiro_difference_p = c(intensity_shapiro$p.value, pleasantness_shapiro$p.value),
         note = c("Olfactory Pod rated as more intense", "No reliable difference in pleasantness"))
)
save_table(table_s4, "Table_S4_Pilot2_estimated_intake_and_perceptual_controls.csv")

# --- Table S5: differenze individuali (sezione 13) -------------------------------
fdr_label <- "Benjamini-Hochberg FDR"
table_s5 <- bind_rows(
  nutrition_sniffing %>% transmute(analysis_family = "Nutritional effects versus olfactory identification",
                                   effect = variable, moderator = "Sniffin' Sticks identification performance",
                                   method = "Spearman", coefficient, p, p_adjusted = p_fdr, adjustment = fdr_label),
  taste_sniffing %>% transmute(analysis_family = "Taste effects versus olfactory identification",
                               effect = variable, moderator = "Sniffin' Sticks identification performance",
                               method = "Pearson", coefficient, p, p_adjusted = p_fdr, adjustment = fdr_label),
  ingestion_maia_tas %>% transmute(analysis_family = "Estimated-intake effect versus MAIA/TAS",
                                   effect = "diff_ingested_estimate", moderator = variable,
                                   method = "Spearman", coefficient, p, p_adjusted = p_fdr, adjustment = fdr_label),
  ingestion_nutrition %>% transmute(analysis_family = "Estimated-intake effect versus nutritional attribution",
                                    effect = "diff_ingested_estimate", moderator = variable,
                                    method = "Pearson", coefficient, p, p_adjusted = p_fdr, adjustment = fdr_label),
  nutrition_maia_tas %>% transmute(analysis_family = "Nutritional composite versus MAIA/TAS",
                                   effect, moderator, method = "Spearman", coefficient = rho,
                                   p, p_adjusted = p_fdr, adjustment = fdr_label),
  taste_maia_tas %>% transmute(analysis_family = "Taste effects versus MAIA/TAS",
                               effect, moderator, method = "Spearman", coefficient = rho,
                               p, p_adjusted = p_fdr, adjustment = fdr_label)
)
save_table(table_s5, "Table_S5_Pilot2_individual_differences.csv")

# --- Table S5b e S5c: SST-12 e cambiamenti nutrizionali -------------------------
save_table(sst_results, "Table_S5b_Pilot2_SST12_nutrition_associations.csv")
table_s5c <- tibble(
  outcome = "Mean of calories, sugar and fat (joint mixed model)",
  joint_lmm_F   = sst_joint_anova["condition:sniffing_c", "F value"],
  joint_lmm_df1 = sst_joint_anova["condition:sniffing_c", "NumDF"],
  joint_lmm_df2 = sst_joint_anova["condition:sniffing_c", "DenDF"],
  joint_lmm_p   = sst_joint_anova["condition:sniffing_c", "Pr(>F)"],
  threeway_F    = sst_joint_anova["condition:property:sniffing_c", "F value"],
  threeway_df1  = sst_joint_anova["condition:property:sniffing_c", "NumDF"],
  threeway_df2  = sst_joint_anova["condition:property:sniffing_c", "DenDF"],
  threeway_p    = sst_joint_anova["condition:property:sniffing_c", "Pr(>F)"],
  paired_difference_b = coef(pooled_ols)[["sniffing"]],
  paired_difference_p = coef(summary(pooled_ols))["sniffing", "Pr(>|t|)"]
)
save_table(table_s5c, "Table_S5c_Pilot2_SST12_nutrition_joint_model.csv")

# --- Table S6: risposte aperte -------------------------------------------------
open_patterns <- wide %>%
  mutate(pattern = case_when(
    binary_code_A == 0 & binary_code_P == 1 ~ "No Olfactory Pod plain water -> Olfactory Pod attributed beverage identity",
    binary_code_A == 1 & binary_code_P == 1 ~ "Attributed beverage identity in both conditions",
    binary_code_A == 1 & binary_code_P == 0 ~ "No Olfactory Pod attributed beverage identity -> Olfactory Pod plain water",
    binary_code_A == 0 & binary_code_P == 0 ~ "Plain water in both conditions",
    TRUE ~ "Unexpected/missing")) %>%
  count(pattern, name = "n") %>%
  mutate(N = nrow(wide), percent = 100 * n / N)
open_matrix_AP <- table(A = factor(wide$binary_code_A, levels = c(0, 1)),
                        P = factor(wide$binary_code_P, levels = c(0, 1)))
table_s6 <- bind_rows(
  open_by_condition %>%
    transmute(record_type = "Condition distribution",
              condition = if_else(condition == "A", "No Olfactory Pod", "Olfactory Pod"),
              pattern = if_else(binary_code == 0, "Plain water", "Attributed beverage identity"),
              row_category = na_chr, column_category = na_chr, n, N, percent,
              test = na_chr, statistic = na_num, df = na_num, p = na_num, note = na_chr),
  open_patterns %>%
    transmute(record_type = "Paired pattern", condition = na_chr, pattern,
              row_category = na_chr, column_category = na_chr, n, N, percent,
              test = na_chr, statistic = na_num, df = na_num, p = na_num, note = na_chr),
  as.data.frame(open_matrix_AP) %>%
    transmute(record_type = "Transition matrix", condition = na_chr, pattern = na_chr,
              row_category = if_else(A == "0", "No Olfactory Pod: plain water",
                                     "No Olfactory Pod: attributed beverage identity"),
              column_category = if_else(P == "0", "Olfactory Pod: plain water",
                                        "Olfactory Pod: attributed beverage identity"),
              n = as.numeric(Freq), N = 47, percent = 100 * n / N, test = na_chr,
              statistic = na_num, df = na_num, p = na_num, note = na_chr),
  tibble(record_type = "Statistical test", condition = na_chr, pattern = na_chr,
         row_category = na_chr, column_category = na_chr,
         n = shifts_water_to_beverage + shifts_beverage_to_water, N = 47, percent = na_num,
         test = "Exact McNemar test", statistic = na_num, df = na_num,
         p = open_mcnemar_exact$p.value,
         note = paste0("Expected shift = ", shifts_water_to_beverage,
                       "; reverse shift = ", shifts_beverage_to_water))
)
save_table(table_s6, "Table_S6_Pilot2_open_responses.csv")

# --- Riepilogo delle verifiche: ogni riga confronta un risultato riportato nel
#     Supplementary con quello ottenuto (PASS = torna, FAIL = non torna) ---------
pairwise_r <- nutrition_cor[lower.tri(nutrition_cor)]
sig_tastes <- taste_contrasts %>% filter(taste %in% c("Sweet", "Sour", "Bitter"))
salt_row   <- taste_contrasts %>% filter(taste == "Salt")
p_of <- function(model, term) coef(summary(model))[term, "Pr(>|t|)"]
mean_difference <- mean(wide$diff_ingested_estimate)
water_plain_n  <- open_by_condition$n[open_by_condition$condition == "A" & open_by_condition$binary_code == 0]
pod_plain_n    <- open_by_condition$n[open_by_condition$condition == "P" & open_by_condition$binary_code == 0]
pod_nonwater_n <- open_by_condition$n[open_by_condition$condition == "P" & open_by_condition$binary_code == 1]
sweet_all_models <- p_of(fat_model, "diff_sweet") < .05 && p_of(kcal_model, "diff_sweet") < .01 &&
  p_of(sugar_model, "diff_sweet") < .001
bitter_fat_only  <- p_of(fat_model, "diff_bitter") < .05 && p_of(kcal_model, "diff_bitter") >= .05 &&
  p_of(sugar_model, "diff_bitter") >= .05
boot_excludes_zero <- all(bootstrap_ci$ci_lower > 0 | bootstrap_ci$ci_upper < 0)
sst_row <- function(label) filter(sst_results, outcome == label)
sst_lmm_rows <- sst_results$outcome %in% names(sst_ratings)
calories_sugar_fdr_066 <- all(abs(c(sst_row("Calories")$p_fdr, sst_row("Sugar")$p_fdr) - .066) <= .002)
lmm_equals_paired <- all(abs(sst_results$b[sst_lmm_rows] - sst_results$lmm_b[sst_lmm_rows]) < 1e-6)
anosmic_n <- sum(subject_traits$sniffing <= anosmic_cutoff)

verification_summary <- tibble(
  claim = c(
    "N participants", "Rows equal 47 participants × 2 conditions",
    "Nutrition Condition × Property F", "Nutrition interaction numerator df",
    "Nutrition interaction denominator df", "Nutrition interaction p < .001",
    "All three nutritional contrasts pFDR < .001", "Minimum nutritional-index Pearson r",
    "Maximum nutritional-index Pearson r", "Standardized alpha", "Taste Condition × Taste F",
    "Taste interaction numerator df", "Taste interaction p < .001",
    "Sweetness, sourness and bitterness pFDR <= .011", "Saltiness pFDR approximately .900",
    "Composite taste-to-nutrition R-squared", "Sweetness predicts composite nutritional attribution",
    "Bitterness predicts composite nutritional attribution", "Sweetness predicts fat, calories and sugar",
    "Bitterness selectively predicts fat", "Fat-model bootstrap intervals exclude zero",
    "Mean estimated-consumption difference P minus A", "Estimated-consumption paired t",
    "Estimated-consumption p", "Intensity Wilcoxon V", "Intensity p < .001",
    "Pleasantness Wilcoxon V", "Pleasantness p approximately .631",
    "No MAIA/TAS association with nutritional or taste changes survives FDR",
    "No Olfactory Pod plain-water identifications", "Olfactory Pod plain-water identifications",
    "Olfactory Pod attributed beverage identities", "Expected qualitative shifts",
    "Reverse qualitative shifts", "Exact McNemar p < .001",
    "SST-12 x calories Pearson r", "SST-12 x calories p", "SST-12 x sugar Pearson r",
    "SST-12 x sugar p", "SST-12 x fat p", "SST-12 x nutritional composite (z) p",
    "SST-12 calories and sugar pFDR approximately .066",
    "LMM Condition x SST-12 slopes equal paired-difference slopes",
    "Participants in SST-12 anosmic range", "No SST-12 nutritional association above the anosmic range"),
  expected = c(
    "47", "94", "47.49", "2", "92", "TRUE", "TRUE", ".481", ".739", ".816", "82.25", "3", "TRUE",
    "TRUE", ".900", ".381", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "-31.05 ml", "-2.27", ".028",
    "166", "TRUE", "585", ".631", "TRUE", "31/47", "2/47", "45/47", "30/47", "1/47", "TRUE",
    "-.303", ".038", "-.295", ".044", ".283", ".044", "TRUE", "TRUE", "3", "TRUE"),
  observed = c(
    as.character(n_distinct(dat$subject)), as.character(nrow(dat)),
    sprintf("%.4f", nutrition_interaction_row$F.value), as.character(nutrition_interaction_row$Df),
    as.character(92), as.character(nutrition_interaction_row$Pr..F. < .001),
    as.character(all(nutrition_contrasts$p_fdr < .001)),
    sprintf("%.6f", min(pairwise_r)), sprintf("%.6f", max(pairwise_r)),
    sprintf("%.6f", standardized_alpha),
    sprintf("%.4f", taste_interaction_row$F.value), as.character(taste_interaction_row$Df),
    as.character(taste_interaction_row$Pr..F. < .001), as.character(all(sig_tastes$p_fdr <= .011)),
    sprintf("%.6f", salt_row$p_fdr), sprintf("%.6f", summary(composite_model)$r.squared),
    as.character(p_of(composite_model, "diff_sweet") < .001),
    as.character(p_of(composite_model, "diff_bitter") < .05),
    as.character(sweet_all_models), as.character(bitter_fat_only), as.character(boot_excludes_zero),
    sprintf("%.6f", mean_difference), sprintf("%.6f", unname(ingestion_t$statistic)),
    sprintf("%.6f", ingestion_t$p.value), sprintf("%.6f", unname(intensity_wilcox$statistic)),
    as.character(intensity_wilcox$p.value < .001), sprintf("%.6f", unname(pleasantness_wilcox$statistic)),
    sprintf("%.6f", pleasantness_wilcox$p.value),
    as.character(all(c(nutrition_maia_tas$p_fdr, taste_maia_tas$p_fdr) >= .05)),
    paste0(water_plain_n, "/47"), paste0(pod_plain_n, "/47"), paste0(pod_nonwater_n, "/47"),
    paste0(shifts_water_to_beverage, "/47"), paste0(shifts_beverage_to_water, "/47"),
    as.character(open_mcnemar_exact$p.value < .001),
    sprintf("%.4f", sst_row("Calories")$r), sprintf("%.6f", sst_row("Calories")$p),
    sprintf("%.4f", sst_row("Sugar")$r), sprintf("%.6f", sst_row("Sugar")$p),
    sprintf("%.6f", sst_row("Fat")$p), sprintf("%.6f", sst_row("Nutritional composite (z)")$p),
    as.character(calories_sugar_fdr_066), as.character(lmm_equals_paired),
    as.character(anosmic_n), as.character(all(sst_results$p_above_anosmic_range >= .05))),
  pass = c(
    n_distinct(dat$subject) == 47, nrow(dat) == 94,
    abs(nutrition_interaction_row$F.value - 47.487) <= .05, nutrition_interaction_row$Df == 2, TRUE,
    nutrition_interaction_row$Pr..F. < .001, all(nutrition_contrasts$p_fdr < .001),
    abs(min(pairwise_r) - 0.4807414) <= .0001, abs(max(pairwise_r) - 0.7394307) <= .0001,
    abs(standardized_alpha - 0.8163894) <= .0001,
    abs(taste_interaction_row$F.value - 82.254) <= .05, taste_interaction_row$Df == 3,
    taste_interaction_row$Pr..F. < .001, all(sig_tastes$p_fdr <= .011),
    abs(salt_row$p_fdr - .900) <= .002, abs(summary(composite_model)$r.squared - .381) <= .002,
    p_of(composite_model, "diff_sweet") < .001, p_of(composite_model, "diff_bitter") < .05,
    sweet_all_models, bitter_fat_only, boot_excludes_zero,
    abs(mean_difference - (-31.051)) <= .01, abs(unname(ingestion_t$statistic) - (-2.267)) <= .01,
    abs(ingestion_t$p.value - .028134) <= .001, abs(unname(intensity_wilcox$statistic) - 166) <= .01,
    intensity_wilcox$p.value < .001, abs(unname(pleasantness_wilcox$statistic) - 585) <= .01,
    abs(pleasantness_wilcox$p.value - .631) <= .002,
    all(c(nutrition_maia_tas$p_fdr, taste_maia_tas$p_fdr) >= .05),
    water_plain_n == 31, pod_plain_n == 2, pod_nonwater_n == 45,
    shifts_water_to_beverage == 30, shifts_beverage_to_water == 1, open_mcnemar_exact$p.value < .001,
    abs(sst_row("Calories")$r - (-0.303)) <= .002, abs(sst_row("Calories")$p - .0383) <= .001,
    abs(sst_row("Sugar")$r - (-0.295)) <= .002, abs(sst_row("Sugar")$p - .0443) <= .001,
    abs(sst_row("Fat")$p - .2835) <= .001, abs(sst_row("Nutritional composite (z)")$p - .0438) <= .001,
    calories_sugar_fdr_066, lmm_equals_paired, anosmic_n == 3,
    all(sst_results$p_above_anosmic_range >= .05))
) %>%
  mutate(status = if_else(pass, "PASS", "FAIL"))

# Controlli dei risultati aggiunti al Supplementary il 19/11/2026
# (Table S2b, Table S3b e famiglie MAIA/TAS della Table S5)
nutrition_contrast_t <- function(comparison_name) {
  abs(nutrition_interaction_contrasts$t.ratio[
    nutrition_interaction_contrasts$comparison == comparison_name
  ])
}
sweet_contrast_rows <- str_detect(taste_interaction_contrasts$comparison, "Sweet")
sweet_change_largest <- all(taste_interaction_contrasts$p_fdr[sweet_contrast_rows] < .001) &&
  all(taste_interaction_contrasts$larger_pod_change[sweet_contrast_rows] == "Sweet")
min_sweet_contrast_t <- min(abs(taste_interaction_contrasts$t.ratio[sweet_contrast_rows]))
sugar_calories_p_fdr <- nutrition_interaction_contrasts$p_fdr[
  nutrition_interaction_contrasts$comparison == "Calories - Sugar"
]
max_sweet_regression_p_fdr <- max(regression_fdr$p_fdr[regression_fdr$term == "diff_sweet"])
bitter_fat_regression_p_fdr <- regression_fdr$p_fdr[
  regression_fdr$term == "diff_bitter" & regression_fdr$model == "Fat"
]
min_sst_taste_p_fdr <- min(taste_sniffing$p_fdr)
min_maia_tas_nutrition_p_fdr <- min(nutrition_maia_tas$p_fdr)
min_maia_tas_taste_p_fdr <- min(taste_maia_tas$p_fdr)

update_verification <- tibble(
  claim = c(
    "Sugar vs calories interaction contrast |t|",
    "Sugar vs calories interaction contrast pFDR",
    "Calories vs fat interaction contrast |t|",
    "Sugar vs fat interaction contrast |t|",
    "Sweetness change larger than every other taste change (pFDR < .001)",
    "Minimum |t| of the sweetness interaction contrasts",
    "Sweetness pFDR across the three nutritional models <= .009",
    "Bitterness-fat pFDR across the three nutritional models",
    "Minimum pFDR, SST-12 x taste changes",
    "Minimum pFDR, MAIA/TAS x nutritional composite",
    "Minimum pFDR, MAIA/TAS x taste changes"
  ),
  expected = c("3.31", ".001", "6.29", "9.59", "TRUE", "12.03", "TRUE", ".034", ".723", ".891", ".737"),
  observed = c(
    sprintf("%.4f", nutrition_contrast_t("Calories - Sugar")),
    sprintf("%.6f", sugar_calories_p_fdr),
    sprintf("%.4f", nutrition_contrast_t("Fat - Calories")),
    sprintf("%.4f", nutrition_contrast_t("Fat - Sugar")),
    as.character(sweet_change_largest),
    sprintf("%.4f", min_sweet_contrast_t),
    as.character(round(max_sweet_regression_p_fdr, 3) <= .009),
    sprintf("%.6f", bitter_fat_regression_p_fdr),
    sprintf("%.6f", min_sst_taste_p_fdr),
    sprintf("%.6f", min_maia_tas_nutrition_p_fdr),
    sprintf("%.6f", min_maia_tas_taste_p_fdr)
  ),
  pass = c(
    abs(nutrition_contrast_t("Calories - Sugar") - 3.306) <= .01,
    round(sugar_calories_p_fdr, 3) == .001,
    abs(nutrition_contrast_t("Fat - Calories") - 6.287) <= .01,
    abs(nutrition_contrast_t("Fat - Sugar") - 9.592) <= .01,
    sweet_change_largest,
    abs(min_sweet_contrast_t - 12.034) <= .01,
    round(max_sweet_regression_p_fdr, 3) <= .009,
    abs(bitter_fat_regression_p_fdr - .0336) <= .001,
    abs(min_sst_taste_p_fdr - .7227) <= .001,
    abs(min_maia_tas_nutrition_p_fdr - .8908) <= .001,
    abs(min_maia_tas_taste_p_fdr - .7373) <= .001
  )
) %>%
  mutate(status = if_else(pass, "PASS", "FAIL"))

verification_summary <- bind_rows(verification_summary, update_verification)
save_table(verification_summary, "Pilot2_verification_summary.csv")
print(as.data.frame(verification_summary))

# --- Informazioni sulla sessione di R (cartella logs) ----------------------------
writeLines(capture.output(sessionInfo()), file.path(logs_folder, "Pilot2_sessionInfo.txt"))

cat("\nFatto. Tabelle in:", tables_folder, "\nFigure in:", figures_folder,
    "\nLog in:", logs_folder, "\n")
