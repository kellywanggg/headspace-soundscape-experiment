# =============================================================================
# HEADSPACE SOUNDSCAPE EXPERIMENT — ANALYSIS SCRIPT
# Master's Thesis | University of Amsterdam
# Research Question: Does background audio (Headspace "Making Space") vs.
#   silence affect (1) reading comprehension accuracy, (2) subjective focus,
#   (3) alertness/activeness (SSSQ), and (4) emotional state (SAM)?
# Design: Within-subjects, 2 conditions (Audio vs. Silence), counterbalanced
#   across 4 groups, N = 33
# =============================================================================


# ── 0. PACKAGES ───────────────────────────────────────────────────────────────
# Install any missing packages before loading
required_packages <- c("tidyverse", "lme4", "lmerTest", "emmeans",
                       "psych", "car", "ggpubr", "rstatix",
                       "knitr", "corrplot", "reshape2")

installed <- installed.packages()[, "Package"]
to_install <- setdiff(required_packages, installed)
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)    # data wrangling & ggplot2
library(lme4)         # linear mixed models
library(lmerTest)     # p-values for lmer (Satterthwaite df)
library(emmeans)      # estimated marginal means & contrasts
library(psych)        # describe(), alpha()
library(car)          # Levene's test, ANOVA
library(ggpubr)       # publication-ready plots
library(rstatix)      # pipe-friendly stats helpers
library(corrplot)     # correlation matrix visualisation
library(reshape2)     # melt() helper



# ── 1. LOAD DATA ──────────────────────────────────────────────────────────────
# Main long-format dataset (one row per participant × condition block)
df_long <- read_csv("data/headspace_long.csv", na = c("", "NA")) %>%
  # Remove any trailing empty rows that Qualtrics exports sometimes add
  filter(!is.na(ID))

# SP-HHQ (Sound Preference & Hearing Habits Questionnaire) — one row per person
df_sphhq <- read_csv("data/sp_hhq.csv", na = c("", "NA")) %>%
  filter(!is.na(ID))

# Preview
glimpse(df_long)
glimpse(df_sphhq)



# ── 2. CLEAN & RECODE ─────────────────────────────────────────────────────────

# 2a. Factor coding ─────────────────────────────────────────────────────────
df_long <- df_long %>%
  mutate(
    ID         = as.factor(ID),
    CB_Group   = as.factor(CB_Group),
    Block      = as.factor(Block),
    # "Audio" column uses "Audio" / "Silence" — make it a tidy factor
    Condition  = factor(Audio, levels = c("Silence", "Audio"),
                        labels = c("Silence", "Audio")),
    Passage    = as.factor(Passage),
    # Rename for clarity throughout
    Accuracy          = Accuracy,          # 0–6 comprehension questions correct
    Valence           = Valence,           # SAM 1–9
    Arousal           = Arousal,           # SAM 1–9
    Alert             = `SSSQ Alert`,      # 1–5
    Active            = `SSSQ Active`,     # 1–5
    SSSQ_Engagement   = `SSSQ Engagement Mean`, # mean of 6 engagement items
    Familiarity       = Familiarity        # 1–5 topic familiarity
  )

# 2b. SP-HHQ subscale scoring ────────────────────────────────────────────────
# The SP-HHQ (Meis et al., 2018) uses a 5-point scale (1 = "not apply at all"
# to 5 = "fully apply"). We collected 13 items covering three subscales that
# are theoretically relevant to how participants respond to background audio:
#
#   F1 — Annoyance/Distraction by Background Noise (7 items)
#         Q163_1  F1_K1: In films I would prefer ambient noise removed
#         Q163_2  F1_K2: I turn off the radio if there are loud sounds in the car
#         Q163_3  F1_K3: Background noise really annoys me when making a phone call
#         Q163_4  F1_K4: I am easily distracted by ambient noises during conversations
#         Q163_5  F1_K5: Disturbing noises should be suppressed in mobile phones
#         Q163_6  F1_K7: Making conversation is no fun while the radio plays
#         Q163_7  F1_K8: I am really annoyed by ambient noise in a restaurant
#         NOTE: F1_K26 was omitted from the survey, leaving 7 of the 8 F1 items.
#         This is acknowledged as a limitation; the subscale mean is used so
#         scores remain on the original 1–5 scale for comparability.
#
#   F3 — Noise Sensitivity (3 items)
#         Q163_8  F3_K36: I react more sensitively to loud sounds than most people
#         Q163_9  F3_K38: I am sensitive to noise
#         Q163_10 F3_K11: I react more sensitively to sharp sounds than my fellow humans
#
#   F4 — Avoidance of Unpredictable Sounds (3 items)
#         Q163_11 F4_K41: I avoid uncertain and unexpected listening situations
#         Q163_12 F4_K42: I don't like unexpected sounds like a suddenly approaching car
#         Q163_13 F4_K44: I prefer familiar acoustic situations rather than unknown ones
#
# All items are positively keyed (no reverse scoring required).
# Subscale scores are computed as row means so they stay on the 1–5 scale.

f1_items <- paste0("Q163_", 1:7)    # 7 of 8 F1 items (F1_K26 missing)
f3_items <- paste0("Q163_", 8:10)   # 3 F3 items
f4_items <- paste0("Q163_", 11:13)  # 3 F4 items
all_sphhq_items <- paste0("Q163_", 1:13)

df_sphhq <- df_sphhq %>%
  mutate(
    ID = as.factor(ID),
    # Subscale means (1–5 scale)
    SPHHQ_F1 = rowMeans(select(., all_of(f1_items)),  na.rm = TRUE),  # Annoyance/distraction
    SPHHQ_F3 = rowMeans(select(., all_of(f3_items)),  na.rm = TRUE),  # Noise sensitivity
    SPHHQ_F4 = rowMeans(select(., all_of(f4_items)),  na.rm = TRUE),  # Avoidance of unpredictable sounds
    # Overall mean across all 13 collected items (for descriptive reference)
    SPHHQ_Overall = rowMeans(select(., all_of(all_sphhq_items)), na.rm = TRUE)
  )

# Internal consistency (Cronbach's alpha) per subscale
cat("\n--- SP-HHQ Cronbach's alpha by subscale ---\n")
alpha_f1 <- psych::alpha(df_sphhq %>% select(all_of(f1_items)))
alpha_f3 <- psych::alpha(df_sphhq %>% select(all_of(f3_items)))
alpha_f4 <- psych::alpha(df_sphhq %>% select(all_of(f4_items)))

cat("F1 (Annoyance/Distraction by BG Noise, 7 items):\n")
print(alpha_f1$total["raw_alpha"])
cat("F3 (Noise Sensitivity, 3 items):\n")
print(alpha_f3$total["raw_alpha"])
cat("F4 (Avoidance of Unpredictable Sounds, 3 items):\n")
print(alpha_f4$total["raw_alpha"])

# Descriptives per subscale
cat("\n--- SP-HHQ subscale descriptives ---\n")
print(psych::describe(df_sphhq %>% select(SPHHQ_F1, SPHHQ_F3, SPHHQ_F4, SPHHQ_Overall)))

# 2c. Merge SP-HHQ subscales into the main long dataset ─────────────────────
df_merged <- df_long %>%
  left_join(
    df_sphhq %>% select(ID, SPHHQ_F1, SPHHQ_F3, SPHHQ_F4, SPHHQ_Overall),
    by = "ID"
  )

# 2d. Median-splits per subscale for moderation analyses ─────────────────────
# Each subscale gets its own High/Low split so moderation can be tested
# separately per theoretically distinct individual-difference dimension.
df_merged <- df_merged %>%
  mutate(
    # F1: High = more annoyed/distracted by background noise
    NS_F1 = if_else(SPHHQ_F1 >= median(SPHHQ_F1, na.rm = TRUE),
                    "High_Annoyance", "Low_Annoyance") %>% as.factor(),
    # F3: High = more noise sensitive
    NS_F3 = if_else(SPHHQ_F3 >= median(SPHHQ_F3, na.rm = TRUE),
                    "High_Sensitivity", "Low_Sensitivity") %>% as.factor(),
    # F4: High = stronger avoidance of unpredictable sounds
    NS_F4 = if_else(SPHHQ_F4 >= median(SPHHQ_F4, na.rm = TRUE),
                    "High_Avoidance", "Low_Avoidance") %>% as.factor()
  )

# Quick sanity check
cat("\nMissing values per column:\n")
print(colSums(is.na(df_merged)))
cat("\nCondition distribution:\n")
print(table(df_merged$Condition))



# ── 3. DESCRIPTIVE STATISTICS ─────────────────────────────────────────────────

# 3a. Per-condition means & SDs for all outcome variables ───────────────────
outcomes <- c("Accuracy", "Valence", "Arousal", "Alert", "Active",
              "SSSQ_Engagement", "Familiarity")

desc_by_condition <- df_merged %>%
  group_by(Condition) %>%
  summarise(across(all_of(outcomes),
                   list(M  = ~mean(.x, na.rm = TRUE),
                        SD = ~sd(.x,   na.rm = TRUE)),
                   .names = "{.col}_{.fn}"),
            n = n(), .groups = "drop")

cat("\n--- Descriptives by Condition ---\n")
print(desc_by_condition)

# 3b. Overall participant-level descriptives (collapsed across conditions) ─
cat("\n--- Overall descriptives ---\n")
print(psych::describe(df_merged %>% select(all_of(outcomes))))



# ── 4. ASSUMPTION CHECKS ──────────────────────────────────────────────────────
# Because the design is within-subjects and we will use paired tests /
# linear mixed models, we mainly check:
#   (a) Normality of difference scores (for paired t-test fallback)
#   (b) Outliers

# 4a. Create wide format for difference-score tests ─────────────────────────
df_wide <- df_merged %>%
  select(ID, Condition, all_of(outcomes)) %>%
  pivot_wider(names_from  = Condition,
              values_from = all_of(outcomes),
              names_glue  = "{.value}_{Condition}")

# Difference scores (Audio minus Silence)
df_wide <- df_wide %>%
  mutate(
    diff_Accuracy        = Accuracy_Audio        - Accuracy_Silence,
    diff_Valence         = Valence_Audio          - Valence_Silence,
    diff_Arousal         = Arousal_Audio          - Arousal_Silence,
    diff_Alert           = Alert_Audio            - Alert_Silence,
    diff_Active          = Active_Audio           - Active_Silence,
    diff_SSSQ_Engagement = SSSQ_Engagement_Audio  - SSSQ_Engagement_Silence
  )

diff_cols <- c("diff_Accuracy", "diff_Valence", "diff_Arousal",
               "diff_Alert", "diff_Active", "diff_SSSQ_Engagement")

# Shapiro-Wilk normality on difference scores (appropriate for N = 33)
cat("\n--- Shapiro-Wilk on difference scores ---\n")
sw_results <- map_dfr(diff_cols, function(col) {
  test <- shapiro.test(df_wide[[col]])
  tibble(Variable = col, W = test$statistic, p = test$p.value)
})
print(sw_results)
# Interpretation: p > .05 → normality not rejected → use paired t-test
#                 p < .05 → use Wilcoxon signed-rank test as non-parametric alt.

# 4b. Outlier detection (boxplot rule: ±3 IQR) on difference scores ─────────
cat("\n--- Outliers in difference scores (±3 SD rule) ---\n")
outliers <- map_dfr(diff_cols, function(col) {
  x   <- df_wide[[col]]
  zsc <- abs(scale(x))
  idx <- which(zsc > 3)
  if (length(idx) == 0) return(NULL)
  tibble(Variable = col, ID = df_wide$ID[idx], z_score = zsc[idx])
})
if (nrow(outliers) == 0) cat("No extreme outliers detected.\n") else print(outliers)



# ── 5. PRIMARY ANALYSES ───────────────────────────────────────────────────────
# For each outcome we run both:
#   (A) Paired t-test (or Wilcoxon if normality violated) — simple, interpretable
#   (B) Linear Mixed Model (LMM) — accounts for random participant effects and
#       allows covariates such as Passage, Block order, and Familiarity

run_paired_test <- function(data_wide, diff_col, normal = TRUE) {
  if (normal) {
    # One-sample t-test on the difference score (equivalent to paired t-test)
    t.test(data_wide[[diff_col]], mu = 0)
  } else {
    # Wilcoxon signed-rank; conf.int = TRUE gives a pseudo-median CI
    wilcox.test(data_wide[[diff_col]], mu = 0, exact = FALSE, conf.int = TRUE)
  }
}

# Helper: compute Cohen's d from two paired numeric vectors using base R.
# Uses the SD of the difference score in the denominator (paired d_z convention),
# which is the correct approach for within-subjects designs.
paired_cohens_d <- function(x_audio, x_silence) {
  diffs <- x_audio - x_silence
  round(mean(diffs, na.rm = TRUE) / sd(diffs, na.rm = TRUE), 3)
}

# Helper: for Wilcoxon results, compute rank-biserial correlation r as effect size.
# r = Z / sqrt(N), where Z is back-calculated from the W statistic.
wilcoxon_r <- function(diff_vec) {
  n    <- sum(!is.na(diff_vec))
  wres <- wilcox.test(diff_vec, mu = 0, exact = FALSE)
  # Back-calculate Z from the normal approximation p-value (two-tailed)
  z    <- qnorm(wres$p.value / 2)
  round(abs(z) / sqrt(n), 3)
}

# ── 5.1 H1: Reading Comprehension Accuracy ─────────────────────────────────
# H1: Accuracy will differ between Audio and Silence conditions.

cat("\n====== H1: ACCURACY ======\n")

# (A) Paired t-test / Wilcoxon
acc_normal <- sw_results %>% filter(Variable == "diff_Accuracy") %>% pull(p) > .05
acc_test   <- run_paired_test(df_wide, "diff_Accuracy", acc_normal)
print(acc_test)

# Effect size: Cohen's d_z (normal) or Wilcoxon r (non-normal)
if (acc_normal) {
  cat("Cohen's d_z:", paired_cohens_d(df_wide$Accuracy_Audio, df_wide$Accuracy_Silence), "\n")
} else {
  cat("Wilcoxon r:", wilcoxon_r(df_wide$diff_Accuracy), "\n")
}

# (B) LMM — random intercept for participant; fixed effects: Condition + Passage + Block
lmm_acc <- lmer(Accuracy ~ Condition + Passage + Block + Familiarity +
                  (1 | ID),
                data   = df_merged,
                REML   = TRUE)
cat("\n--- LMM: Accuracy ---\n")
print(summary(lmm_acc))
cat("\n--- Type III ANOVA (LMM: Accuracy) ---\n")
print(anova(lmm_acc))


# ── 5.2 H2: Subjective Focus (SSSQ Engagement) ─────────────────────────────
# H2: Subjective engagement will differ between conditions.

cat("\n====== H2: SSSQ ENGAGEMENT (Subjective Focus) ======\n")

eng_normal <- sw_results %>% filter(Variable == "diff_SSSQ_Engagement") %>% pull(p) > .05
eng_test   <- run_paired_test(df_wide, "diff_SSSQ_Engagement", eng_normal)
print(eng_test)

if (eng_normal) {
  cat("Cohen's d_z:", paired_cohens_d(df_wide$SSSQ_Engagement_Audio, df_wide$SSSQ_Engagement_Silence), "\n")
} else {
  cat("Wilcoxon r:", wilcoxon_r(df_wide$diff_SSSQ_Engagement), "\n")
}

lmm_eng <- lmer(SSSQ_Engagement ~ Condition + Passage + Block + Familiarity +
                  (1 | ID),
                data = df_merged, REML = TRUE)
cat("\n--- LMM: SSSQ Engagement ---\n")
print(summary(lmm_eng))
cat("\n--- Type III ANOVA (LMM: Engagement) ---\n")
print(anova(lmm_eng))


# ── 5.3 H3: Alertness and Activeness (SSSQ subscales) ──────────────────────
# H3: Alertness and activeness will differ between conditions.

for (outcome_var in c("Alert", "Active")) {
  diff_col <- paste0("diff_", outcome_var)
  cat("\n====== H3:", toupper(outcome_var), "======\n")
  
  normal_flag <- sw_results %>% filter(Variable == diff_col) %>% pull(p) > .05
  test_result <- run_paired_test(df_wide, diff_col, normal_flag)
  print(test_result)
  
  if (normal_flag) {
    cat("Cohen's d_z:", paired_cohens_d(df_wide[[paste0(outcome_var, "_Audio")]],
                                        df_wide[[paste0(outcome_var, "_Silence")]]), "\n")
  } else {
    cat("Wilcoxon r:", wilcoxon_r(df_wide[[diff_col]]), "\n")
  }
  
  frm <- as.formula(paste(outcome_var,
                          "~ Condition + Passage + Block + Familiarity + (1 | ID)"))
  lmm_mod <- lmer(frm, data = df_merged, REML = TRUE)
  cat("\n--- LMM:", outcome_var, "---\n")
  print(summary(lmm_mod))
  print(anova(lmm_mod))
}


# ── 5.4 H4: Emotional State — Valence and Arousal (SAM) ────────────────────
# H4: Valence and/or arousal will differ between conditions.

for (outcome_var in c("Valence", "Arousal")) {
  diff_col <- paste0("diff_", outcome_var)
  cat("\n====== H4:", toupper(outcome_var), "(SAM) ======\n")
  
  normal_flag <- sw_results %>% filter(Variable == diff_col) %>% pull(p) > .05
  test_result <- run_paired_test(df_wide, diff_col, normal_flag)
  print(test_result)
  
  if (normal_flag) {
    cat("Cohen's d_z:", paired_cohens_d(df_wide[[paste0(outcome_var, "_Audio")]],
                                        df_wide[[paste0(outcome_var, "_Silence")]]), "\n")
  } else {
    cat("Wilcoxon r:", wilcoxon_r(df_wide[[diff_col]]), "\n")
  }
  
  frm <- as.formula(paste(outcome_var,
                          "~ Condition + Passage + Block + Familiarity + (1 | ID)"))
  lmm_mod <- lmer(frm, data = df_merged, REML = TRUE)
  cat("\n--- LMM:", outcome_var, "---\n")
  print(summary(lmm_mod))
  print(anova(lmm_mod))
}


# ── 5.5 Passage Counterbalancing Check ──────────────────────────────────────
# Verify that Passage (Banana vs. Kilian) does not confound results.
cat("\n====== PASSAGE EFFECT CHECK ======\n")
lmm_passage_acc <- lmer(Accuracy ~ Passage + Condition + Familiarity + (1 | ID),
                        data = df_merged, REML = TRUE)
cat("--- Passage effect on Accuracy ---\n")
print(anova(lmm_passage_acc))

# ── 5.6 Order / Block Effect Check ──────────────────────────────────────────
# Check whether doing the task in Block 1 vs. Block 2 matters.
cat("\n====== BLOCK ORDER EFFECT CHECK ======\n")
lmm_block_acc <- lmer(Accuracy ~ Block + Condition + Familiarity + (1 | ID),
                      data = df_merged, REML = TRUE)
print(anova(lmm_block_acc))



# ── 6. MODERATOR ANALYSIS: SP-HHQ SUBSCALES ──────────────────────────────────
# Exploratory: Does the effect of Condition on outcomes differ by each SP-HHQ
# subscale? Three separate moderation tests, one per subscale, each entered
# as a Condition × Subscale_Split interaction in the LMM.
#
# Theoretical rationale:
#   F1 (Annoyance): People easily annoyed by background noise may be more
#      disrupted by the audio condition → predict larger Condition effect.
#   F3 (Noise Sensitivity): Noise-sensitive participants may perceive the
#      soundscape as intrusive → negative interaction with audio benefit.
#   F4 (Avoidance of Unpredictable Sounds): Participants who prefer familiar
#      acoustic environments may differ in how they habituate to the track.

cat("\n====== MODERATOR ANALYSES: SP-HHQ SUBSCALES ======\n")

# We test moderation for the three most theoretically central outcomes:
# Accuracy (objective performance), SSSQ_Engagement (subjective focus), Alert.
mod_outcomes  <- c("Accuracy", "SSSQ_Engagement", "Alert")
mod_subscales <- list(
  F1_Annoyance   = "NS_F1",
  F3_NoiseSens   = "NS_F3",
  F4_Avoidance   = "NS_F4"
)

for (subscale_label in names(mod_subscales)) {
  split_var <- mod_subscales[[subscale_label]]
  cat("\n--- Moderator:", subscale_label, "---\n")
  
  for (outcome_var in mod_outcomes) {
    frm_mod <- as.formula(paste(
      outcome_var,
      "~ Condition *", split_var,
      "+ Passage + Block + Familiarity + (1 | ID)"
    ))
    
    lmm_mod <- lmer(frm_mod, data = df_merged, REML = TRUE)
    aov_mod  <- anova(lmm_mod)
    cat("\n  Outcome:", outcome_var, "×", subscale_label, "\n")
    print(aov_mod)
    
    # If the interaction is significant, probe with emmeans simple effects
    int_row <- paste0("Condition:", split_var)
    int_p   <- tryCatch(aov_mod[int_row, "Pr(>F)"], error = function(e) NA)
    
    if (!is.na(int_p) && int_p < .05) {
      cat("  *** Significant interaction (p =", round(int_p, 4),
          ") — simple effects:\n")
      em <- emmeans(lmm_mod, ~ Condition | !!sym(split_var))
      print(pairs(em, adjust = "holm"))
    }
  }
}

# ── 6b. Continuous moderation (SP-HHQ subscales as numeric predictors) ──────
# Complements the median-split approach by treating subscales as continuous
# to avoid information loss. Interaction term: Condition × SPHHQ_Fx (numeric).
cat("\n--- Continuous moderation (subscale scores as numeric predictors) ---\n")

for (subscale_cont in c("SPHHQ_F1", "SPHHQ_F3", "SPHHQ_F4")) {
  for (outcome_var in mod_outcomes) {
    frm_cont <- as.formula(paste(
      outcome_var,
      "~ Condition *", subscale_cont,
      "+ Passage + Block + Familiarity + (1 | ID)"
    ))
    lmm_cont <- lmer(frm_cont, data = df_merged, REML = TRUE)
    aov_cont  <- anova(lmm_cont)
    int_row   <- paste0("Condition:", subscale_cont)
    int_p     <- tryCatch(aov_cont[int_row, "Pr(>F)"], error = function(e) NA)
    
    if (!is.na(int_p) && int_p < .10) {   # report marginal effects too
      cat("\n  Marginal/significant interaction:", outcome_var, "×",
          subscale_cont, "(p =", round(int_p, 4), ")\n")
      print(aov_cont)
    }
  }
}
cat("(Only interactions with p < .10 printed above to reduce output clutter)\n")



# ── 7. CORRELATION ANALYSIS ───────────────────────────────────────────────────
# Explore correlations among outcome variables and all three SP-HHQ subscales.
# Using Spearman because several variables are ordinal / non-normal.

cat("\n====== CORRELATION MATRIX ======\n")
cor_vars <- c("Accuracy", "SSSQ_Engagement", "Alert", "Active",
              "Valence", "Arousal",
              "SPHHQ_F1", "SPHHQ_F3", "SPHHQ_F4")

cor_data   <- df_merged %>% select(all_of(cor_vars)) %>% drop_na()
cor_matrix <- cor(cor_data, method = "spearman", use = "complete.obs")

# Friendly column/row labels for the printed matrix
rownames(cor_matrix) <- colnames(cor_matrix) <-
  c("Accuracy", "Engagement", "Alert", "Active", "Valence", "Arousal",
    "F1: Annoyance", "F3: NoiseSens", "F4: Avoidance")

cat("Spearman correlation matrix (outcome variables + SP-HHQ subscales):\n")
print(round(cor_matrix, 2))



# ── 8. VISUALISATIONS ─────────────────────────────────────────────────────────

# Colour palette (colourblind-friendly)
cond_colours <- c("Silence" = "#4477AA", "Audio" = "#EE6677")

# 8a. Paired plots — Accuracy ───────────────────────────────────────────────
p_acc <- ggpaired(df_wide,
                  cond1 = "Accuracy_Silence", cond2 = "Accuracy_Audio",
                  fill   = "condition",
                  palette = c("#4477AA", "#EE6677"),
                  line.color = "gray70", line.size = 0.4,
                  xlab = "Condition", ylab = "Accuracy (0–6)",
                  title = "Reading Comprehension Accuracy by Condition") +
  scale_x_discrete(labels = c("Silence", "Audio"))

# 8b. Boxplots for all outcomes ─────────────────────────────────────────────
plot_box <- function(outcome, y_label, title) {
  ggplot(df_merged, aes(x = Condition, y = .data[[outcome]], fill = Condition)) +
    geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.size = 2) +
    geom_jitter(width = 0.12, size = 1.5, alpha = 0.5, colour = "gray30") +
    scale_fill_manual(values = cond_colours) +
    labs(x = "Condition", y = y_label, title = title) +
    theme_pubr() +
    theme(legend.position = "none")
}

p_eng    <- plot_box("SSSQ_Engagement", "SSSQ Engagement Mean (1–5)",
                     "Subjective Engagement by Condition")
p_alert  <- plot_box("Alert",    "Alertness (1–5)",
                     "SSSQ Alertness by Condition")
p_active <- plot_box("Active",   "Activeness (1–5)",
                     "SSSQ Activeness by Condition")
p_val    <- plot_box("Valence",  "SAM Valence (1–9)",
                     "Emotional Valence by Condition")
p_aro    <- plot_box("Arousal",  "SAM Arousal (1–9)",
                     "Emotional Arousal by Condition")

# 8c. Combined panel ────────────────────────────────────────────────────────
panel <- ggarrange(p_acc, p_eng, p_alert, p_active, p_val, p_aro,
                   ncol = 3, nrow = 2,
                   common.legend = TRUE, legend = "bottom")
annotate_figure(panel,
                top = text_grob("Outcome Variables: Audio vs. Silence",
                                face = "bold", size = 14))
ggsave("outputs/figures/headspace_boxplots_panel.png", panel, width = 12, height = 8, dpi = 300)

# 8d. SP-HHQ correlation plot ───────────────────────────────────────────────
png("headspace_correlation_matrix.png", width = 800, height = 800, res = 120)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.75,
         tl.col = "black", tl.srt = 45,
         title = "Spearman Correlations (All Outcome Variables + SP-HHQ)",
         mar = c(0, 0, 2, 0))
dev.off()

# 8e. SP-HHQ subscale distributions ────────────────────────────────────────
sphhq_long <- df_sphhq %>%
  select(ID, SPHHQ_F1, SPHHQ_F3, SPHHQ_F4) %>%
  pivot_longer(cols = -ID,
               names_to  = "Subscale",
               values_to = "Score") %>%
  mutate(Subscale = case_match(Subscale,
                               "SPHHQ_F1" ~ "F1: Annoyance/\nDistraction",
                               "SPHHQ_F3" ~ "F3: Noise\nSensitivity",
                               "SPHHQ_F4" ~ "F4: Avoidance of\nUnpredictable Sounds"))

p_sphhq <- ggplot(sphhq_long, aes(x = Score, fill = Subscale)) +
  geom_histogram(bins = 10, colour = "white", alpha = 0.85) +
  geom_vline(data = sphhq_long %>%
               group_by(Subscale) %>%
               summarise(M = mean(Score, na.rm = TRUE)),
             aes(xintercept = M), linetype = "dashed", colour = "gray20") +
  facet_wrap(~ Subscale, ncol = 3) +
  scale_fill_manual(values = c("#88CCEE", "#44AA99", "#DDCC77")) +
  labs(x = "Mean Score (1–5)", y = "Count",
       title = "SP-HHQ Subscale Distributions (N = 33)",
       caption = "Dashed line = subscale mean") +
  theme_pubr() +
  theme(legend.position = "none", strip.text = element_text(size = 9))

ggsave("outputs/figures/headspace_sphhq_subscales.png", p_sphhq, width = 9, height = 4, dpi = 300)

# 8f. Scatter: SP-HHQ subscales × accuracy difference score ─────────────────
# Join subscale scores onto wide data for scatter plots
df_wide_sphhq <- df_wide %>%
  left_join(df_sphhq %>% select(ID, SPHHQ_F1, SPHHQ_F3, SPHHQ_F4), by = "ID")

plot_scatter_sphhq <- function(subscale, sub_label) {
  ggplot(df_wide_sphhq,
         aes(x = .data[[subscale]], y = diff_Accuracy)) +
    geom_point(colour = "#4477AA", size = 2.5, alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, colour = "#EE6677", fill = "#EE6677",
                alpha = 0.15) +
    geom_hline(yintercept = 0, linetype = "dotted", colour = "gray50") +
    labs(x = sub_label,
         y = "Accuracy: Audio − Silence",
         title = paste("Accuracy Difference ×", sub_label)) +
    theme_pubr()
}

p_s1 <- plot_scatter_sphhq("SPHHQ_F1", "F1: Annoyance/Distraction")
p_s3 <- plot_scatter_sphhq("SPHHQ_F3", "F3: Noise Sensitivity")
p_s4 <- plot_scatter_sphhq("SPHHQ_F4", "F4: Avoidance")

scatter_panel <- ggarrange(p_s1, p_s3, p_s4, ncol = 3)
ggsave("outputs/figures/headspace_sphhq_scatter.png", scatter_panel, width = 12, height = 4, dpi = 300)

# 8g. F4 × Condition interaction plots ─────────────────────────────────────
# These are the key moderation figures for your thesis results section.
# Two versions per outcome:
#   (i)  Median-split: shows High/Low Avoidance groups side by side — intuitive
#         for readers, easy to describe in APA prose.
#   (ii) Continuous: shows each participant as a dot with two regression lines
#         (one per condition) — demonstrates the interaction more precisely.

# Prepare a plotting dataset with F4 median-split labels
df_f4_plot <- df_merged %>%
  mutate(
    F4_Group = case_match(NS_F4,
                          "High_Avoidance" ~ "High F4\n(Avoids Unpredictable Sounds)",
                          "Low_Avoidance"  ~ "Low F4\n(Tolerates Unpredictable Sounds)")
  )

# (i-a) Median-split interaction — ACCURACY
p_f4_acc_split <- ggplot(df_f4_plot,
                         aes(x = Condition, y = Accuracy,
                             colour = F4_Group, group = F4_Group)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun = mean, geom = "line",  linewidth = 1.1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15,
               linewidth = 0.8) +
  scale_colour_manual(values = c("#4477AA", "#EE6677"),
                      name = "SP-HHQ F4") +
  labs(x = "Condition", y = "Mean Accuracy (0–6)",
       title = "F4 × Condition Interaction: Accuracy",
       subtitle = "Error bars = ±1 SE",
       caption = "High F4 = avoids unpredictable sounds (median split)") +
  theme_pubr() +
  theme(legend.position = "right")

# (i-b) Median-split interaction — SSSQ ENGAGEMENT
p_f4_eng_split <- ggplot(df_f4_plot,
                         aes(x = Condition, y = SSSQ_Engagement,
                             colour = F4_Group, group = F4_Group)) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun = mean, geom = "line",  linewidth = 1.1) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15,
               linewidth = 0.8) +
  scale_colour_manual(values = c("#4477AA", "#EE6677"),
                      name = "SP-HHQ F4") +
  labs(x = "Condition", y = "Mean SSSQ Engagement (1–5)",
       title = "F4 × Condition Interaction: Engagement",
       subtitle = "Error bars = ±1 SE",
       caption = "High F4 = avoids unpredictable sounds (median split)") +
  theme_pubr() +
  theme(legend.position = "right")

# Combine into one panel
p_f4_split_panel <- ggarrange(p_f4_acc_split, p_f4_eng_split,
                              ncol = 2, common.legend = TRUE,
                              legend = "right")
p_f4_split_panel <- annotate_figure(
  p_f4_split_panel,
  top = text_grob("SP-HHQ F4 (Avoidance) × Condition Interactions",
                  face = "bold", size = 13)
)
ggsave("outputs/figures/headspace_f4_interaction_split.png", p_f4_split_panel,
       width = 10, height = 5, dpi = 300)

# (ii) Continuous interaction — ACCURACY
# Each dot = one participant; two regression lines show how the
# Condition effect on accuracy changes as a function of F4 score.
p_f4_acc_cont <- ggplot(df_wide_sphhq,
                        aes(x = SPHHQ_F4)) +
  geom_point(aes(y = Accuracy_Audio),    colour = "#EE6677", size = 2.5,
             alpha = 0.7) +
  geom_point(aes(y = Accuracy_Silence),  colour = "#4477AA", size = 2.5,
             alpha = 0.7) +
  geom_smooth(aes(y = Accuracy_Audio,   colour = "Audio"),
              method = "lm", se = TRUE, fill = "#EE6677", alpha = 0.12,
              linewidth = 1.1) +
  geom_smooth(aes(y = Accuracy_Silence, colour = "Silence"),
              method = "lm", se = TRUE, fill = "#4477AA", alpha = 0.12,
              linewidth = 1.1) +
  scale_colour_manual(values = c("Audio" = "#EE6677", "Silence" = "#4477AA"),
                      name = "Condition") +
  labs(x = "SP-HHQ F4: Avoidance of Unpredictable Sounds (1–5)",
       y = "Accuracy (0–6)",
       title = "F4 × Condition: Accuracy (Continuous)",
       subtitle = "Diverging slopes indicate the significant interaction (p = .016)") +
  theme_pubr() +
  theme(legend.position = "right")

# (ii) Continuous interaction — SSSQ ENGAGEMENT
p_f4_eng_cont <- ggplot(df_wide_sphhq,
                        aes(x = SPHHQ_F4)) +
  geom_point(aes(y = SSSQ_Engagement_Audio),   colour = "#EE6677", size = 2.5,
             alpha = 0.7) +
  geom_point(aes(y = SSSQ_Engagement_Silence), colour = "#4477AA", size = 2.5,
             alpha = 0.7) +
  geom_smooth(aes(y = SSSQ_Engagement_Audio,   colour = "Audio"),
              method = "lm", se = TRUE, fill = "#EE6677", alpha = 0.12,
              linewidth = 1.1) +
  geom_smooth(aes(y = SSSQ_Engagement_Silence, colour = "Silence"),
              method = "lm", se = TRUE, fill = "#4477AA", alpha = 0.12,
              linewidth = 1.1) +
  scale_colour_manual(values = c("Audio" = "#EE6677", "Silence" = "#4477AA"),
                      name = "Condition") +
  labs(x = "SP-HHQ F4: Avoidance of Unpredictable Sounds (1–5)",
       y = "SSSQ Engagement (1–5)",
       title = "F4 × Condition: Engagement (Continuous)",
       subtitle = "Diverging slopes indicate the significant interaction (p = .018)") +
  theme_pubr() +
  theme(legend.position = "right")

p_f4_cont_panel <- ggarrange(p_f4_acc_cont, p_f4_eng_cont,
                             ncol = 2, common.legend = TRUE,
                             legend = "right")
p_f4_cont_panel <- annotate_figure(
  p_f4_cont_panel,
  top = text_grob("SP-HHQ F4 (Avoidance) × Condition — Continuous Moderation",
                  face = "bold", size = 13)
)
ggsave("outputs/figures/headspace_f4_interaction_continuous.png", p_f4_cont_panel,
       width = 11, height = 5, dpi = 300)

# 8h. Emmeans marginal means plot for F4 × Condition (from the LMM) ─────────
# This is the most statistically rigorous version to show in your thesis:
# model-estimated means with 95% CIs, controlling for Passage, Block, Familiarity.

lmm_acc_f4 <- lmer(Accuracy ~ Condition * SPHHQ_F4 + Passage + Block +
                     Familiarity + (1 | ID),
                   data = df_merged, REML = TRUE)
lmm_eng_f4 <- lmer(SSSQ_Engagement ~ Condition * SPHHQ_F4 + Passage + Block +
                     Familiarity + (1 | ID),
                   data = df_merged, REML = TRUE)

# Predicted values across the range of F4, separately per condition
f4_range <- seq(min(df_merged$SPHHQ_F4, na.rm = TRUE),
                max(df_merged$SPHHQ_F4, na.rm = TRUE),
                length.out = 50)

pred_acc <- expand.grid(
  SPHHQ_F4    = f4_range,
  Condition   = c("Silence", "Audio"),
  Passage     = "Banana",   # held at reference level
  Block       = factor(1, levels = c("1","2")),
  Familiarity = mean(df_merged$Familiarity, na.rm = TRUE)
) %>%
  mutate(Accuracy = predict(lmm_acc_f4, newdata = ., re.form = NA))

pred_eng <- expand.grid(
  SPHHQ_F4        = f4_range,
  Condition       = c("Silence", "Audio"),
  Passage         = "Banana",
  Block           = factor(1, levels = c("1","2")),
  Familiarity     = mean(df_merged$Familiarity, na.rm = TRUE)
) %>%
  mutate(SSSQ_Engagement = predict(lmm_eng_f4, newdata = ., re.form = NA))

p_lmm_acc <- ggplot(pred_acc,
                    aes(x = SPHHQ_F4, y = Accuracy, colour = Condition,
                        fill = Condition)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = Accuracy - 0.3, ymax = Accuracy + 0.3), alpha = 0.10,
              colour = NA) +
  scale_colour_manual(values = cond_colours) +
  scale_fill_manual(values   = cond_colours) +
  labs(x = "SP-HHQ F4: Avoidance of Unpredictable Sounds",
       y = "Predicted Accuracy",
       title = "LMM-Predicted Accuracy by F4 Score",
       subtitle = "Passage, Block & Familiarity held at reference/mean") +
  theme_pubr()

p_lmm_eng <- ggplot(pred_eng,
                    aes(x = SPHHQ_F4, y = SSSQ_Engagement, colour = Condition,
                        fill = Condition)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = SSSQ_Engagement - 0.1,
                  ymax = SSSQ_Engagement + 0.1), alpha = 0.10, colour = NA) +
  scale_colour_manual(values = cond_colours) +
  scale_fill_manual(values   = cond_colours) +
  labs(x = "SP-HHQ F4: Avoidance of Unpredictable Sounds",
       y = "Predicted SSSQ Engagement",
       title = "LMM-Predicted Engagement by F4 Score",
       subtitle = "Passage, Block & Familiarity held at reference/mean") +
  theme_pubr()

p_lmm_panel <- ggarrange(p_lmm_acc, p_lmm_eng,
                         ncol = 2, common.legend = TRUE, legend = "right")
p_lmm_panel <- annotate_figure(
  p_lmm_panel,
  top = text_grob("Model-Predicted F4 × Condition Interactions (LMM)",
                  face = "bold", size = 13)
)
ggsave("outputs/figures/headspace_f4_lmm_predicted.png", p_lmm_panel,
       width = 11, height = 5, dpi = 300)

cat("\nF4 interaction plots saved:\n")
cat("  headspace_f4_interaction_split.png      (median-split, SE bars)\n")
cat("  headspace_f4_interaction_continuous.png (raw data + regression lines)\n")
cat("  headspace_f4_lmm_predicted.png          (LMM predicted values)\n")



# ── 9. MULTIPLE COMPARISONS CORRECTION ────────────────────────────────────────
# We test 6 outcomes (Accuracy, Engagement, Alert, Active, Valence, Arousal).
# Apply Holm (step-down Bonferroni) correction to the paired-test p-values.

cat("\n====== MULTIPLE COMPARISONS CORRECTION (Holm) ======\n")

# Re-run all paired tests, collect p-values
all_outcomes_diff <- c("diff_Accuracy", "diff_SSSQ_Engagement",
                       "diff_Alert",    "diff_Active",
                       "diff_Valence",  "diff_Arousal")

raw_p <- map_dbl(all_outcomes_diff, function(col) {
  normal_flag <- sw_results %>% filter(Variable == col) %>% pull(p) > .05
  test <- run_paired_test(df_wide, col, normal_flag)
  test$p.value
})

# Effect sizes: d_z for normal outcomes, Wilcoxon r for non-normal
es_values <- map_chr(all_outcomes_diff, function(col) {
  normal_flag <- sw_results %>% filter(Variable == col) %>% pull(p) > .05
  base_var    <- sub("^diff_", "", col)
  if (normal_flag) {
    d <- paired_cohens_d(df_wide[[paste0(base_var, "_Audio")]],
                         df_wide[[paste0(base_var, "_Silence")]])
    paste0("d_z = ", d)
  } else {
    r <- wilcoxon_r(df_wide[[col]])
    paste0("r = ", r)
  }
})

holm_results <- tibble(
  Outcome    = all_outcomes_diff,
  Test       = map_chr(all_outcomes_diff, function(col) {
    if (sw_results %>% filter(Variable == col) %>% pull(p) > .05) "t-test" else "Wilcoxon"
  }),
  Effect_Size = es_values,
  p_raw      = round(raw_p, 4),
  p_holm     = round(p.adjust(raw_p, method = "holm"), 4),
  Sig_holm   = p_holm < .05
)

cat("\nHolm-corrected results:\n")
print(holm_results)



# ── 10. SUMMARY TABLE ─────────────────────────────────────────────────────────
# Compile a clean results summary suitable for a thesis table.

cat("\n====== FINAL RESULTS SUMMARY ======\n")

# Means per condition
means_audio   <- df_wide %>% summarise(across(ends_with("_Audio"),   mean, na.rm = TRUE))
means_silence <- df_wide %>% summarise(across(ends_with("_Silence"), mean, na.rm = TRUE))
sds_audio     <- df_wide %>% summarise(across(ends_with("_Audio"),   sd,   na.rm = TRUE))
sds_silence   <- df_wide %>% summarise(across(ends_with("_Silence"), sd,   na.rm = TRUE))

# Effect sizes: Cohen's d_z for each outcome (using base-R paired_cohens_d helper)
all_outcomes_base <- c("Accuracy", "SSSQ_Engagement", "Alert", "Active",
                       "Valence",  "Arousal")
# Note: d_z uses SD of difference scores — appropriate for within-subjects designs.
# For outcomes where normality was violated, Wilcoxon r is also reported in the
# Holm correction table above; d_z is still included here for completeness and
# comparability across outcomes.
d_values <- map_dbl(all_outcomes_base, function(v) {
  paired_cohens_d(df_wide[[paste0(v, "_Audio")]],
                  df_wide[[paste0(v, "_Silence")]])
})

summary_table <- tibble(
  Outcome          = all_outcomes_base,
  M_Audio          = map_dbl(all_outcomes_base, ~round(mean(df_wide[[paste0(.x, "_Audio")]],   na.rm=TRUE), 2)),
  SD_Audio         = map_dbl(all_outcomes_base, ~round(sd(df_wide[[paste0(.x, "_Audio")]],     na.rm=TRUE), 2)),
  M_Silence        = map_dbl(all_outcomes_base, ~round(mean(df_wide[[paste0(.x, "_Silence")]], na.rm=TRUE), 2)),
  SD_Silence       = map_dbl(all_outcomes_base, ~round(sd(df_wide[[paste0(.x, "_Silence")]], na.rm=TRUE), 2)),
  Cohen_d          = d_values,
  p_raw            = round(raw_p, 4),
  p_holm           = round(p.adjust(raw_p, method = "holm"), 4)
)

print(summary_table)
write_csv(summary_table, "outputs/tables/headspace_results_summary.csv")
cat("\nResults summary saved to headspace_results_summary.csv\n")



# ── 11. NOTES FOR INTERPRETATION ──────────────────────────────────────────────
cat("
=============================================================================
INTERPRETATION GUIDE
=============================================================================

Primary outcomes and what to look for:

1. ACCURACY (H1)
   - The paired test on diff_Accuracy tells you whether the audio condition
     improved or worsened reading comprehension accuracy.
   - The LMM additionally controls for Passage difficulty, Block order,
     and topic Familiarity.

2. SSSQ ENGAGEMENT (H2)
   - A significant effect (p < .05 after Holm correction) would suggest
     that participants felt more or less engaged under one condition.

3. ALERTNESS & ACTIVENESS (H3)
   - These two SSSQ subscales capture attentional/energetic state.
   - Both are single-item ratings (1–5), so interpret effect sizes carefully.

4. VALENCE & AROUSAL (H4, SAM)
   - SAM is a visual scale (1–9). Audio conditions often increase arousal.
   - A valence effect could indicate mood differences between conditions.

5. SP-HHQ SUBSCALE MODERATION
   Three theoretically distinct moderators are tested separately:

   F1 — Annoyance/Distraction by Background Noise (7 items; F1_K26 missing)
     A significant Condition × F1 interaction would mean that participants
     who are chronically more annoyed by background sounds respond differently
     to the audio condition — likely showing reduced accuracy or engagement.
     NOTE: Only 7 of the 8 original F1 items were collected (F1_K26 was
     omitted); this is a limitation to acknowledge in your thesis. The
     subscale mean is still valid; Cronbach's alpha will indicate reliability.

   F3 — Noise Sensitivity (3 items; K36, K38, K11)
     Captures a general trait of sensitivity to loud/sharp sounds. A
     significant interaction here suggests the soundscape differentially
     affects participants who are physiologically more reactive to noise.

   F4 — Avoidance of Unpredictable Sounds (3 items; K41, K42, K44)
     Participants with high F4 prefer familiar acoustic environments. An
     interaction with Condition could indicate that these participants find
     the Headspace track less predictable and therefore less beneficial.

   Both median-split and continuous moderation are run:
     - Median-split: easy to visualise and report in APA tables
     - Continuous: more power, avoids information loss

6. EFFECT SIZE BENCHMARKS (Cohen's d):
   - Small  : |d| ≈ 0.2
   - Medium : |d| ≈ 0.5
   - Large  : |d| ≈ 0.8

7. PASSAGE & BLOCK EFFECTS
   - If neither Passage nor Block is significant in the LMMs, it supports
     the success of your counterbalancing procedure.

8. CRONBACH'S ALPHA BENCHMARKS:
   - Acceptable : α ≥ 0.70
   - Good       : α ≥ 0.80
   - F3 and F4 have only 3 items each; lower alpha is expected but still
     reported in the paper (α = .82 and .79 in the original validation).
     Your sample is much smaller (N = 33) so alpha estimates will have
     wider confidence intervals.
=============================================================================
")