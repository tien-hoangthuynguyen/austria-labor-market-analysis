# austria-labor-market-analysis
Scraped and mined 15,599 Austrian job postings using R; identified that non-Vienna postings are systematically less graduate-relevant, less English-friendly, and less remote-work-accessible than Vienna postings, using LPM, Lasso, and Random Forest models.

---

## Research question

Austria faces a persistent paradox: one of the EU's highest vacancy rates, yet international Master's graduates disproportionately cluster in Vienna or leave the country. Existing research focuses on supply-side explanations (worker mobility, language barriers, social ties). This project shifts the lens to **employers** — asking whether regional job postings are structured in ways that are accessible and relevant to internationally trained graduates at all.

---

## What this repo contains

```
├── job_in_austria_bigdata.R     # Full analysis pipeline
├── data/
│   └── ams_jobs_austria_all.csv # Raw scraped data (15,714 postings)
├── plots/
│   ├── plot_posting_volume.png
│   ├── plot_vienna_comparison.png
│   ├── plot_regional_heatmap.png
│   ├── plot_rf_importance.png
│   ├── plot_roc_curve.png
│   └── plot_lasso_coefficients.png
└── README.md
```

---

## Data

**Source:** Austrian Public Employment Service (AMS) vacancy portal — `arbeitsmarktservice.at`  
**Collection date:** 18 March 2026  
**Coverage:** Five metropolitan labour markets — Vienna, Linz, Graz, Salzburg, Innsbruck  
**Time range of postings:** May 2025 – March 2026  
**Final analytical sample:** 15,599 postings (after removing missing values)  
**Split:** 47.3% Vienna / 52.7% non-Vienna

Data was collected via automated web scraping using R. AMS covers approximately 50–60% of all Austrian vacancies; graduate-level postings are 30–40% underrepresented on the platform relative to their true share, meaning findings are likely conservative estimates of the true regional gap.

---

## Variables

Six binary dependent variables were extracted from posting text via regular expression matching on a merged title + cleaned description field:

| Variable | What it captures |
|---|---|
| `degree_required` | Posting mentions university degree (Studium, Bachelor, Master, FH-Abschluss) |
| `english_friendly` | English referenced as working language, or job title written in English |
| `german_required` | Explicit German fluency demand (Deutschkenntnisse, fließend, C1/C2, etc.) |
| `salary_disclosed` | Salary figure present in any Austrian format (EUR, €, Jahresbruttogehalt, KV) |
| `remote_option` | Remote or hybrid work mentioned (Homeoffice, remote, hybrid, mobiles Arbeiten) |
| `is_junior` | Entry-level signals (Absolvent, Berufseinsteiger, Trainee, ohne Berufserfahrung) |

The independent variable of interest is `is_vienna` — a binary indicator equal to 1 for Vienna postings and 0 for non-Vienna. Working time (full-time / part-time / both / unknown) is included as a control.

---

## Methods

**Linear probability models (LPM)** — primary inference tool. Each of the six dependent variables is regressed on `is_vienna` and working time controls. Coefficients represent percentage point differences relative to Vienna postings.

**Lasso regression with 10-fold cross-validation** — variable selection check. Confirms which variables carry genuine regional signal after penalised shrinkage. `is_junior` is the only variable zeroed out, consistent with its non-significant LPM coefficient.

**Random Forest classifier** — exploratory complement. Trained on all six variables plus working time and contract type. Reports permutation-based variable importance scores. AUC: 0.583; accuracy: 57.6% — modest but informative: Vienna and non-Vienna postings are not sharply separable, suggesting regional differences are statistically real but moderate in magnitude.

---

## Key findings

Non-Vienna postings are significantly less likely to:
- **Require a degree** (-3.8 pp) — graduate-level demand is concentrated in Vienna
- **Be English-friendly** (-2.8 pp) — internationally trained graduates face a language access gap
- **Offer remote or hybrid work** (-2.6 pp) — a characteristic known to expand geographic applicant pools
- **Explicitly require German** (-6.7 pp) — non-Vienna employers appear to *assume* German proficiency rather than state it, creating an invisible but structurally demanding barrier

Non-Vienna postings are significantly *more* likely to:
- **Disclose salary** (+6.1 pp) — the one dimension where non-Vienna outperforms Vienna

Junior role orientation shows no significant regional variation — the barrier is not a shortage of entry-level jobs but the structural characteristics of those jobs.

---

## How to run

```r
# Install dependencies
install.packages(c("tidyverse", "dplyr", "modelsummary", "readxl",
                   "ranger", "rsample", "yardstick", "glmnet"))

# Place ams_jobs_austria_all.csv in your working directory
# Run the full pipeline
source("job_in_austria_bigdata.R")
```

Outputs: six plots saved as `.png` files in the working directory.

---

## Policy implications

The findings reframe the regional talent attraction problem. If non-Vienna employers want to attract internationally trained graduates, the most direct levers are:

1. **Degree-relevant role design** — signal graduate-level demand explicitly
2. **English-language or bilingual postings** — lower the language access barrier at the discovery stage
3. **Remote work options** — remove geographic friction at the application stage

Regional employer support programmes should focus on posting-level signals, not only on broader incentive schemes.

---

## Author

**Nguyen Hoang Thuy Tien**  
MA Public Policy, Central European University (Vienna)  
[LinkedIn](https://www.linkedin.com/in/tien-nguyen)
