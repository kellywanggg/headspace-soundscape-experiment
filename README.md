# Headspace Soundscape Focus Study

**MSc Information Systems | University of Amsterdam**  
**Author: Kelly Li-Chu Wang**

This repository contains the full analysis pipeline for a master's thesis investigating whether personalised background soundscapes from the Headspace app influence reading focus. The project spans a controlled laboratory experiment, automated passage readability verification, and a supplementary Reddit discourse analysis.

---

## Overview

The study used a counterbalanced within-subject design (N = 33) in which participants completed reading comprehension tasks under two conditions: **Audio** (Headspace "Making Space" soundscape) and **Silence**. Subjective attentional and affective state was measured after each block. Individual differences in sound sensitivity were assessed via the SP-HHQ questionnaire.

---

## Research Questions

1. Does background audio improve objective reading comprehension accuracy?
2. Does background audio increase subjective engagement (perceived focus)?
3. Does background audio influence alertness, activeness, valence, or arousal?
4. Do individual differences in sound sensitivity (SP-HHQ) moderate these effects?

---

## Key Findings

- No significant main effect of audio on reading comprehension accuracy.
- No significant main effect on alertness, activeness, valence, or arousal.
- Engagement showed a suggestive increase in the audio condition, but did not survive Holm correction for multiple comparisons.
- SP-HHQ Factor 4 (avoidance of unpredictable sounds) significantly moderated the effect of condition on both accuracy (*p* = .016) and engagement (*p* = .018).

> Soundscapes are not universally beneficial — their effects appear to depend on listener characteristics, particularly sound avoidance tendencies.

---

## Repository Structure

```
.
├── data/
│   ├── headspace_long.csv          # Long-format experiment data (1 row per participant × condition)
│   ├── sp_hhq.csv                  # SP-HHQ questionnaire scores (1 row per participant)
│   └── Headspace_Reddit_Review.xlsx  # Scraped Reddit posts and comments
│
├── scripts/
│   └── headspace_analysis.R        # Full R analysis pipeline
│
├── python/
│   ├── Reading_Passage_Evaluation.ipynb     # Readability scoring of stimulus passages
│   └── Headspace_Reddit_Topic_Clustering.ipynb  # TF-IDF + KMeans Reddit discourse analysis
│
├── figures/
│   ├── headspace_boxplots_panel.png
│   ├── headspace_correlation_matrix.png
│   ├── headspace_f4_interaction_continuous.png
│   ├── headspace_f4_interaction_split.png
│   ├── headspace_f4_lmm_predicted.png
│   ├── headspace_sphhq_scatter.png
│   └── headspace_sphhq_subscales.png
│
└── README.md
```

---

## Analysis Components

### 1. R — Primary Experiment Analysis (`scripts/headspace_analysis.R`)

The main statistical pipeline for the within-subject experiment. Covers data cleaning, assumption checks, inferential tests, moderation analyses, and figure generation.

**Statistical methods:**
- Paired *t*-tests and Wilcoxon signed-rank tests (selected based on Shapiro-Wilk normality)
- Linear Mixed-Effects Models with random intercept per participant (`lme4`)
- Moderation analyses using SP-HHQ subscales (median-split and continuous)
- Spearman correlations across outcome and individual difference variables
- Holm correction across 6 outcomes (Accuracy, Engagement, Alertness, Activeness, Valence, Arousal)

**Covariates controlled for:** Passage, Block order, Familiarity

**SP-HHQ subscales used:**
| Subscale | Items | Description |
|---|---|---|
| F1 | 7 items | Annoyance/Distraction by Background Noise |
| F3 | 3 items | Noise Sensitivity |
| F4 | 3 items | Avoidance of Unpredictable Sounds |

---

### 2. Python — Reading Passage Evaluation (`python/Reading_Passage_Evaluation.ipynb`)

Automated readability scoring of the stimulus passages used in the experiment to verify that comprehension difficulty was matched across conditions.

**Tools:** `textstat` (Flesch Reading Ease, Flesch-Kincaid Grade, Gunning Fog, and more)

**Passages evaluated:**
- Passage A: *A Threat to Bananas* (British Council LearnEnglish, C1) — the history of the Gros Michel and Cavendish banana fungal crisis (*Fusarium oxysporum*)
- Passage B: *A Biography of Kilian Jornet* (British Council LearnEnglish, C1) — a biography of the ultra-distance mountain runner who summited Everest solo in 17 hours

Both passages are rated C1 (advanced) by the British Council, providing a basis for comparing readability scores and confirming difficulty was matched across the two experimental conditions.

This step ensures experimental validity by confirming that any accuracy differences between conditions are attributable to the audio manipulation, not differences in passage difficulty.

---

### 3. Python — Reddit Topic Clustering (`python/Headspace_Reddit_Topic_Clustering.ipynb`)

A supplementary qualitative-to-quantitative analysis of Reddit discourse around focus music and the Headspace app, contextualising the experiment within real-world user behaviour.

**Data:** `Headspace_Reddit_Review.xlsx` — scraped posts and comments from r/Headspace and r/Productivity

**Pipeline:**
1. Text cleaning (lowercasing, URL removal, punctuation stripping)
2. TF-IDF vectorisation (unigrams + bigrams, `min_df=2`, `max_df=0.85`)
3. KMeans clustering with silhouette score selection across *k* = 4–8
4. Top-term extraction and representative example review per cluster
5. Export to `reddit_clustered_output.csv`

**Identified clusters (k=8):** Topics span structured focus music (e.g. Hans Zimmer, lo-fi), genre preferences (jazz, ambient, metal), binaural beats, and app-specific experiences (Headspace, Endel).

---

## Reproducibility

### R Analysis

Requires R (≥ 4.1) and the following packages, which auto-install on first run:

```
tidyverse, lme4, lmerTest, emmeans, psych, car, ggpubr, rstatix, knitr, corrplot, reshape2
```

To reproduce all results and figures:
```r
source("scripts/headspace_analysis.R")
```

All outputs are saved to `outputs/figures/` and `outputs/tables/`.

### Python Notebooks

Requires Python 3.x. Install dependencies with:

```bash
pip install pandas scikit-learn textstat openpyxl
```

Both notebooks were developed in Google Colab and can be run cell-by-cell. The clustering notebook expects `Headspace_Reddit_Review.xlsx` (or `Headspace_Reddit_CLEAN.csv` after first run) in the working directory.

---

## Data Availability

All experimental data is anonymised. Participant identifiers are coded (P01–P33) with no personally identifiable information retained.

If ethical access restrictions apply, the analysis scripts are provided for methodological transparency and replication structure.

---

## Measures Reference

| Measure | Scale | Description |
|---|---|---|
| Accuracy | 0–6 | Reading comprehension correct answers |
| SSSQ Engagement | 1–5 | Mean of 6 subjective focus items |
| SSSQ Alertness | 1–5 | Single-item alertness rating |
| SSSQ Activeness | 1–5 | Single-item activeness rating |
| SAM Valence | 1–9 | Self-Assessment Manikin — emotional valence |
| SAM Arousal | 1–9 | Self-Assessment Manikin — arousal |
| SP-HHQ F1/F3/F4 | 1–5 | Sound Preference & Hearing Habits subscales |
