# Headspace Soundscape Focus Study

This repository contains the statistical analysis pipeline for a master's thesis investigating the impact of personalised background soundscapes (Headspace) on reading focus.

The study combines a controlled within-subject laboratory experiment with individual difference measures to examine whether background audio influences objective performance and subjective attentional experience.

------------------------------------------------------------------------

## Research Questions

This project investigates whether listening to a structured soundscape while reading:

1.  Improves objective reading comprehension accuracy\
2.  Increases subjective engagement (perceived focus)\
3.  Influences alertness, activeness, valence, or arousal\
4.  Interacts with individual differences in sound sensitivity

The experiment used a counterbalanced within-subject design (Audio vs Silence).

------------------------------------------------------------------------

## Key Findings (Summary)

-   No significant main effect of audio on reading comprehension accuracy.
-   No significant main effect on alertness, activeness, valence, or arousal.
-   Engagement showed a suggestive increase in the audio condition, but this did not survive correction for multiple comparisons.
-   Individual differences in avoidance of unpredictable sounds (SP-HHQ F4) significantly moderated the relationship between condition and both accuracy and engagement.

The findings suggest that soundscapes are not universally beneficial, and their effects may depend on listener characteristics.

------------------------------------------------------------------------

## Repository Structure

. ├── data/ │ ├── headspace_long.csv │ └── sp_hhq.csv ├── scripts/ │ └── headspace_analysis.R ├── figures/ │ ├── headspace_boxplots_panel.png │ ├── headspace_correlation_matrix.png │ ├── headspace_f4_interaction_continuous.png │ ├── headspace_f4_interaction_split.png │ ├── headspace_f4_lmm_predicted.png │ ├── headspace_sphhq_scatter.png │ └── headspace_sphhq_subscales.png └── README.md

------------------------------------------------------------------------

## Statistical Approach

Analyses include:

-   Paired t-tests and Wilcoxon signed-rank tests (depending on normality)
-   Linear Mixed-Effects Models (random intercept for participant)
-   Moderator analyses using SP-HHQ subscales (continuous and median split approaches)
-   Spearman correlations
-   Holm correction for multiple comparisons

Models control for:

-   Passage
-   Block
-   Familiarity

------------------------------------------------------------------------

## Reproducibility

All analyses were conducted in R.

To reproduce results:

1.  Clone this repository\
2.  Open the R project\
3.  Run:

source("script/headspace_analysis.R")

All figures and statistical outputs will be generated automatically.

------------------------------------------------------------------------

## Data Availability

The dataset included in this repository is anonymised and contains no personally identifiable information.

If ethical restrictions apply, the analysis script is provided for transparency and replication structure only.

------------------------------------------------------------------------

## Author

Kelly Li-Chu Wang\
MSc Information Systems\
University of Amsterdam
