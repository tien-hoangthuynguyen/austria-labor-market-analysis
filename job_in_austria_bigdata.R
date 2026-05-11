# Job Postings Analysis 

setwd("~/CEU/Coding/Big Data Project")

library(tidyverse)
library(dplyr)
library(modelsummary)
library(readxl)
library(ranger)
library(rsample)
library(yardstick)
library(glmnet)

df_raw <- read_csv("ams_jobs_austria_all.csv")

print(names(df_raw))


# How many postings per region?
df_raw %>%
  count(`workingLocation.federalState`, sort = TRUE) %>%
  print(n = 20)

df_raw %>%
  select(title, `workingLocation.federalState`, summary) %>%
  slice_sample(n = 3) %>%
  pull(summary) %>%
  substr(1, 400) %>%
  cat(sep = "\n---\n")

#Clean data
df <- df_raw %>%
  mutate(
    
    # --- Strip HTML from summary field ---
    # Your summaries contain <p>, <br />, <ul> etc. — remove all tags
    text_clean = summary %>%
      str_remove_all("<[^>]+>") %>% 
      str_replace_all("&amp;", "&") %>%
      str_replace_all("&lt;", "<") %>%
      str_replace_all("&gt;", ">") %>%
      str_replace_all("&nbsp;", " ") %>%
      str_replace_all("\u00ef\u00be\u0083|\u00ef\u00bf\u00bd", "") %>%  
      str_squish(),                   
    

    region = `workingLocation.federalState`,
    
    region = case_when(
      str_detect(region, "Wien|Vienna")                          ~ "Wien",
      str_detect(region, "Nieder|Lower Austria")                 ~ "Niederösterreich",
      str_detect(region, "Ober|Upper Austria")                   ~ "Oberösterreich",
      str_detect(region, "Steier|Styria")                        ~ "Steiermark",
      str_detect(region, "Tirol|Tyrol")                          ~ "Tirol",
      str_detect(region, "Vorarlberg")                           ~ "Vorarlberg",
      str_detect(region, "Salzburg")                             ~ "Salzburg",
      str_detect(region, "K.rnten|Carinthia")                    ~ "Kärnten",
      str_detect(region, "Burgenland")                           ~ "Burgenland",
      TRUE                                                        ~ NA_character_
    ),
    
    is_vienna = if_else(region == "Wien", 1L, 0L),
    
    date = as.Date(lastUpdatedAt),
    year  = year(date),
    month = month(date),
    
    working_time = case_when(
      str_detect(coalesce(`workingTime.description`, ""),
                 regex("Vollzeit|full.?time|40.?[Hh]|38.?[Hh]", ignore_case = TRUE)) &
        !str_detect(coalesce(`workingTime.description`, ""),
                    regex("Teilzeit|part.?time", ignore_case = TRUE)) ~ "fulltime",
      str_detect(coalesce(`workingTime.description`, ""),
                 regex("Teilzeit|part.?time", ignore_case = TRUE)) &
        !str_detect(coalesce(`workingTime.description`, ""),
                    regex("Vollzeit|full.?time", ignore_case = TRUE)) ~ "parttime",
      str_detect(coalesce(`workingTime.description`, ""),
                 regex("Teilzeit.{0,5}Vollzeit|Vollzeit.{0,5}Teilzeit|Teil.*oder.*Voll",
                       ignore_case = TRUE))                           ~ "both",
      TRUE                                                             ~ "unknown"
    ),
    
    contract_type = case_when(
      str_detect(coalesce(`employmentRelationship.description`, ""),
                 regex("unbefristet", ignore_case = TRUE)) |
        str_detect(coalesce(text_clean, ""),
                   regex("unbefristete[rns]? (Arbeits|Dienst|Stellen|Anstellung)",
                         ignore_case = TRUE))                          ~ "permanent",
      str_detect(coalesce(`employmentRelationship.description`, ""),
                 regex("befristet", ignore_case = TRUE)) |
        str_detect(coalesce(text_clean, ""),
                   regex("befristet|bis (\\d{1,2}\\.\\d{1,2}\\.|Ende|Dezember|Juni)",
                         ignore_case = TRUE))                          ~ "temporary",
      TRUE                                                              ~ "unknown"
    )
    
  )

cat("\nRegion distribution after cleaning:\n")
df %>% count(region, sort = TRUE) %>% print(n = 15)

cat("\nWorking time distribution:\n")
df %>% count(working_time) %>% print()

cat("\nContract type distribution:\n")
df %>% count(contract_type) %>% print()

# EXTRACT BINARY VARIABLES FROM TEXT

df <- df %>%
  mutate(
    
    text_full = str_c(coalesce(title, ""), " ", coalesce(text_clean, "")),
    
    # ----------------------------------------------------------
    # GERMAN LANGUAGE REQUIREMENT
    # Catches: explicit German fluency demands
    # Note: most ads are IN German, but we want ads that
    # *require* German as a job requirement, not just the language
    # of the ad itself
    # ----------------------------------------------------------
    german_required = if_else(
  str_detect(text_full, regex(
    paste0(
      # Original patterns
      "Deutschkenntnisse|",
      "Deutsch.*flie.end|flie.end.*Deutsch|",
      "Deutsch.*verhandlungssicher|verhandlungssicher.*Deutsch|",
      "Muttersprache.*Deutsch|Deutsch.*Muttersprache|",
      "sehr gute.*Deutsch|gute.*Deutschkenntnisse|",
      "C1.*Deutsch|C2.*Deutsch|Deutsch.*C1|Deutsch.*C2|",
      
      # NEW: structured language block formats (AMS-style)
      "Deutsch:\\s*(Muttersprache|sehr gut|gut|flie.end|verhandlungssicher)|",
      "Sprachkenntnisse.*Deutsch|",
      "Deutsch in Wort und Schrift|",
      "Deutsch auf Muttersprachniveau|",
      "Deutsch.*vorausgesetzt|",
      
      # NEW: short formulations
      "gute Deutsch|sehr gute Deutsch|",
      "Deutschkurs|Deutschniveau|",
      
      # NEW: level codes without the word Deutsch nearby
      "Sprachniveau.*[CB][12]|[CB][12].*Deutsch"
    ),
    ignore_case = TRUE
  )),
  1L, 0L
),

english_friendly = if_else(
  str_detect(text_full, regex(
    paste0(
      "working language.*[Ee]nglish|[Ee]nglish.*working language|",
      "business [Ee]nglish|",
      "Englischkenntnisse|",
      "English skills|",
      "proficient in English|",
      "fluent.*English|English.*fluent|",
      "spoken.*English|written.*English"
    ),
    ignore_case = TRUE
  )) |
    # Ad is written in English (proxy: common English phrases in title)
    str_detect(coalesce(title, ""), regex(
      "\\b(the|and|for|with|our|you|your|will|are|we|this)\\b",
      ignore_case = TRUE
    )),
  1L, 0L
),

implicit_german = if_else(
  english_friendly == 0 &
    # Check the ad has substantial German text
    str_detect(text_full, regex(
      "und|oder|mit|für|die|der|das|Sie|wir|Ihre|eine|einer",
      ignore_case = FALSE  # case-sensitive: these are German function words
    )) &
    nchar(coalesce(text_clean, "")) > 100,
  1L, 0L
),
    
  # ----------------------------------------------------------
  # JUNIOR / ENTRY-LEVEL ACCESSIBLE
  # ----------------------------------------------------------
  is_junior = if_else(
    str_detect(text_full, regex(
      paste0(
        "junior|",
        "Berufseinsteiger|Berufseinstieg|",
        "Absolvent|Absolventin|",
        "ohne Berufserfahrung|keine.*Erfahrung erforderlich|",
        "Einsteiger|",
        "Nachwuchs|",
        "erste Berufserfahrung|erste.*Erfahrung|",
        "frisch.*Studium|Studienabg.nger|",
        "Traineeprogramm|Trainee"
      ),
        ignore_case = TRUE
    )),
    1L, 0L
  ),
    
  # ----------------------------------------------------------
  # SENIOR / EXPERIENCED REQUIRED
  # (useful as contrast to junior — and for mismatch argument)
  #----------------------------------------------------------
  is_senior = if_else(
    str_detect(text_full, regex(
      paste0(
        "\\bsenior\\b|",
        "mehrj.hrige.*Berufserfahrung|",
        "mind(estens)?\\s+[3-9]\\s+Jahr|",
        "langjährige|langjahrige|",
        "Führungserfahrung|Leitungserfahrung|",
        "[3-9]\\+?\\s+years.*experience|years of experience"
      ),
      ignore_case = TRUE
    )),
    1L, 0L
  ),
  is_junior_broad = if_else(
  is_senior == 0,
      1L, 0L
  ),

  # ----------------------------------------------------------
  # SALARY DISCLOSED
  # ----------------------------------------------------------
  salary_disclosed = if_else(
    str_detect(text_full, regex(
      paste0(
        # Original patterns
        "EUR\\s*\\d|€\\s*\\d|",
        "Jahresbruttogehalt|Jahresgehalt|Monatsgehalt|",
        "Bruttogehalt|Bruttolohn|Bruttomonatsgehalt|",
        "Mindestgehalt|KV-Mindest|kollektivvertraglich|",
        "Gehaltsangabe|Gehalt.*EUR|EUR.*Gehalt|",
        "ab EUR|ab €|EUR \\d|€ \\d|",
        
        # NEW: number before € symbol (Austrian format: 1.580 €)
        "\\d+[.,]\\d+\\s*€|",        # 1.580 € or 1,580 €
        "\\d+\\s*€|",                 # 580 €
        "€\\s*\\d+[.,]\\d+|",        # € 1.580
        
        # NEW: written-out formats
        "Bruttomonatslohn|",
        "Grundgehalt|",
        "Einstiegsgehalt|",
        "Lohn.*€|€.*Lohn|",
        "Gehalt.*\\d|\\d.*Gehalt"    # any number near the word Gehalt
      ),
      ignore_case = TRUE
    )),
    1L, 0L
  ),
    
  # ----------------------------------------------------------
  # REMOTE / HYBRID OPTION
  # ----------------------------------------------------------
   remote_option = if_else(
    str_detect(text_full, regex(
      paste0(
        "Homeoffice|Home-Office|Home Office|",
        "\\bremote\\b|",
        "hybrid|",
        "mobiles Arbeiten|mobile.*Arbeit|",
        "Telearbeit|",
        "ortsunabhängig|ortsunabh.ngig|",
        "work from home|",
        "flexibler Arbeitsort"
      ),
      ignore_case = TRUE
    )),
    1L, 0L
  ),
    
  # ----------------------------------------------------------
  # UNIVERSITY DEGREE REQUIRED
  # ----------------------------------------------------------
  degree_required = if_else(
    str_detect(text_full, regex(
      paste0(
        "abgeschlossenes? (Hochschul|Universitäts|Uni|FH)|",
        "Studium|Hochschulabschluss|Universitätsabschluss|",
        "\\bBachelor\\b|\\bMaster\\b|\\bMagister\\b|\\bDiplom\\b|",
        "FH-Abschluss|FH Abschluss|",
        "akademische(r|n|m)? Abschluss|",
        "Akademiker"
      ),
      ignore_case = TRUE
    )),
    1L, 0L
  ),
    
  # ----------------------------------------------------------
  # AUSTRIAN DEGREE SPECIFICALLY REQUIRED
  # (relevant to discrimination/access argument)
  # ----------------------------------------------------------
  austrian_credential = if_else(
    str_detect(text_full, regex(
      paste0(
        "österreichische(r|n|s)? (Führerschein|Ausbildung|Abschluss|Zertifikat)|",
        "Nostrifikation|nostrifi|",
        "österreichische(r|n|s)? Berufsanerkennung|",
        "inländische(r|n|s)? Abschluss"
      ),
      ignore_case = TRUE
    )),
    1L, 0L
  )
)


cat("\n=== FEATURE PREVALENCE (share of all postings) ===\n")

df %>%
  summarise(
    across(
      c(german_required, english_friendly, is_junior, is_junior_broad, is_senior,
        salary_disclosed, remote_option, degree_required, austrian_credential),
      ~ mean(.x, na.rm = TRUE)
    )
  ) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "share") %>%
  mutate(share = scales::percent(share, accuracy = 0.1)) %>%
  arrange(desc(share)) %>%
  print()


cat("\n=== VIENNA vs NON-VIENNA: feature shares ===\n")

comparison <- df %>%
  filter(!is.na(is_vienna)) %>%
  group_by(is_vienna) %>%
  summarise(
    n_postings      = n(),
    pct_german      = mean(german_required,    na.rm = TRUE),
    pct_english     = mean(english_friendly,   na.rm = TRUE),
    pct_junior      = mean(is_junior,          na.rm = TRUE),
    pct_junior_broad= mean(is_junior_broad,    na.rm = TRUE),
    pct_senior      = mean(is_senior,          na.rm = TRUE),
    pct_salary      = mean(salary_disclosed,   na.rm = TRUE),
    pct_remote      = mean(remote_option,      na.rm = TRUE),
    pct_degree      = mean(degree_required,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(location = if_else(is_vienna == 1, "Vienna", "Non-Vienna")) %>%
  select(location, n_postings, everything(), -is_vienna)

print(comparison)

# By Bundesland 
cat("\n=== BY BUNDESLAND ===\n")

by_region <- df %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    n_postings  = n(),
    pct_german  = mean(german_required,  na.rm = TRUE),
    pct_english = mean(english_friendly, na.rm = TRUE),
    pct_junior  = mean(is_junior,        na.rm = TRUE),
    pct_salary  = mean(salary_disclosed, na.rm = TRUE),
    pct_remote  = mean(remote_option,    na.rm = TRUE),
    pct_degree  = mean(degree_required,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_postings))

print(by_region, n = 20)


# VALIDATION SAMPLE

set.seed(42)

validation <- df %>%
  filter(!is.na(text_clean), nchar(text_clean) > 100) %>%
  select(
    title, region,
    text_clean,
    german_required, english_friendly,
    is_junior, salary_disclosed,
    remote_option, degree_required
  ) %>%
  slice_sample(n = 50)

write_xlsx(validation, "validation_sample.xlsx")

df_clean <- df %>%
  select(
    # Identifiers
    id, title, `company.name`,
    # Location
    region, is_vienna,
    `workingLocation.municipality`, `workingLocation.zipCode`,
    # Date
    date, year, month,
    # Structured columns (cleaned)
    working_time, contract_type,
    # Extracted binary variables
    german_required, english_friendly, implicit_german,
    is_junior, is_junior_broad, is_senior,
    salary_disclosed, remote_option,
    degree_required, austrian_credential,
    # Raw text
    text_clean,
    # Other useful columns
    educationLevels, occupations,
    jobPortalName, urlToJobOffer
  )

write_csv(df_clean, "jobs_clean.csv")

# Baseline regression

df_model <- df_clean %>%
  filter(
    !is.na(is_vienna),
    !is.na(german_required),
    !is.na(english_friendly),
    !is.na(is_junior),
    !is.na(salary_disclosed),
    !is.na(remote_option),
    !is.na(degree_required)
  ) %>%
  mutate(
    non_vienna   = 1L - is_vienna,
    working_time = relevel(factor(working_time), ref = "fulltime")
  )

outcomes <- c("degree_required", "english_friendly", "is_junior",
              "salary_disclosed", "remote_option", "german_required")

models_list <- map(outcomes, ~ lm(
  as.formula(paste(.x, "~ non_vienna + working_time")),
  data = df_model
)) %>%
  set_names(c("Degree required", "English-friendly", "Junior role",
              "Salary disclosed", "Remote/hybrid", "German required"))

modelsummary(
  models_list,
  coef_map = c("non_vienna" = "Non-Vienna"),
  stars    = TRUE,
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  title    = "Effect of regional location on posting characteristics",
  notes    = "LPM. IV = non-Vienna indicator. Coefficients = percentage point difference relative to Vienna. Working time controls included. + p<0.1, * p<0.05, ** p<0.01, *** p<0.001",
)


# Add controls: rough sector classification from job titles
df_model <- df_model %>%
  mutate(
    sector = case_when(
      str_detect(title, regex("IT|Software|Developer|Engineer|Tech|Data|Digital", 
                              ignore_case = TRUE))  ~ "IT",
      str_detect(title, regex("Manager|Director|Lead|Head|Chief|CEO|CFO",
                              ignore_case = TRUE))  ~ "Management",
      str_detect(title, regex("Sales|Verkauf|Account|Customer|Kundenbe",
                              ignore_case = TRUE))  ~ "Sales",
      str_detect(title, regex("Koch|Kellner|Hotel|Restaurant|Küche|Service",
                              ignore_case = TRUE))  ~ "Hospitality",
      str_detect(title, regex("Pflege|Arzt|Ärztin|Kranken|Gesundheit|Medical",
                              ignore_case = TRUE))  ~ "Healthcare",
      str_detect(title, regex("Bau|Elektro|Mechanik|Techniker|Industrie",
                              ignore_case = TRUE))  ~ "Trades",
      TRUE ~ "Other"
    )
  )

models_list_v2 <- map(outcomes, ~ lm(
  as.formula(paste(.x, "~ non_vienna + working_time + sector")),
  data = df_model
)) %>%
  set_names(c("Degree required", "English-friendly", "Junior role",
              "Salary disclosed", "Remote/hybrid", "German required"))

modelsummary(
  models_list_v2,
  coef_map = c("non_vienna" = "Non-Vienna"),
  stars    = TRUE,
  gof_map  = c("nobs", "r.squared", "adj.r.squared"),
  title    = "Effect of regional location on posting characteristics",
  notes    = "LPM. IV = non-Vienna indicator. Coefficients = percentage point difference relative to Vienna. Working time controls included. + p<0.1, * p<0.05, ** p<0.01, *** p<0.001",
)

df_clean %>%
  filter(!is.na(region)) %>%
  count(region) %>%
  mutate(region = fct_reorder(region, n)) %>%
  ggplot(aes(x = n, y = region)) +
  geom_col(fill = "#4472C4") +
  geom_text(aes(label = scales::comma(n)),
            hjust = -0.1, size = 3.5, color = "grey30") +
  scale_x_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title   = "Job posting volume by Austrian region",
    subtitle = "Total postings in dataset",
    x       = "Number of postings",
    y       = NULL,
    caption = "Source: AMS job postings data, 2025."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank()
  )

ggsave("plot_posting_volume.png", width = 7, height = 5, dpi = 300)


# RANDOM FOREST

set.seed(123)

# Build RF dataset
df_rf <- df_clean %>%
  filter(!is.na(is_vienna)) %>%
  transmute(
    is_vienna        = factor(ifelse(is_vienna == 1, "Vienna", "NonVienna"),
                              levels = c("NonVienna", "Vienna")),
    german_required  = factor(german_required),
    english_friendly = factor(english_friendly),
    is_junior        = factor(is_junior),
    salary_disclosed = factor(salary_disclosed),
    remote_option    = factor(remote_option),
    degree_required  = factor(degree_required),
    working_time     = factor(working_time),
    contract_type    = factor(contract_type)
  )

# Train/test split (80/20, stratified)
split <- initial_split(df_rf, prop = 0.8, strata = is_vienna)
train <- training(split)
test  <- testing(split)

cat("Train rows:", nrow(train), "\n")
cat("Test rows: ", nrow(test),  "\n")

# Fit Random Forest
rf_fit <- ranger(
  is_vienna ~ .,
  data          = train,
  num.trees     = 500,
  mtry          = floor(sqrt(ncol(train) - 1)),
  min.node.size = 25,
  importance    = "permutation",
  probability   = TRUE,
  seed          = 123
)

rf_fit

# Null model Brier score (predicting the base rate for everyone)
p <- mean(train$is_vienna == "Vienna")
brier_null <- p * (1 - p)
cat("Share Vienna in training:", round(p, 3), "\n")
cat("Null Brier score:        ", round(brier_null, 3), "\n")
cat("Model Brier score:       ", round(rf_fit$prediction.error, 3), "\n")
cat("Improvement over null:   ", round(brier_null - rf_fit$prediction.error, 4), "\n")

# Evaluate on test set
pred_prob  <- predict(rf_fit, data = test)$predictions[, "Vienna"]
pred_class <- factor(ifelse(pred_prob >= 0.5, "Vienna", "NonVienna"),
                     levels = c("NonVienna", "Vienna"))

results <- tibble(
  truth        = test$is_vienna,
  .pred_Vienna = pred_prob,
  .pred_class  = pred_class
)

# Performance metrics
cat("=== TEST SET PERFORMANCE ===\n")
roc_auc(results,  truth, .pred_Vienna, event_level = "second")  %>% print()
accuracy(results, truth, .pred_class)                           %>% print()
conf_mat(results, truth, .pred_class)                           %>% print()

# Variable importance
imp_df <- tibble(
  variable   = names(rf_fit$variable.importance),
  importance = as.numeric(rf_fit$variable.importance)
) %>%
  arrange(desc(importance))

cat("\n=== VARIABLE IMPORTANCE ===\n")
print(imp_df)


# LASSO
# Prepare matrix — glmnet needs matrices not dataframes
X <- df_model %>%
  select(degree_required, german_required, english_friendly,
         is_junior, salary_disclosed, remote_option) %>%
  as.matrix()

y <- df_model$is_vienna

set.seed(42)
lasso_cv <- cv.glmnet(X, y, alpha = 1, family = "binomial", nfolds = 10)

# Plot cross-validation curve
plot(lasso_cv)
title("Lasso: cross-validation deviance vs log(lambda)", line = 3)

# Coefficients at two lambda values
cat("=== LASSO COEFFICIENTS ===\n")
cat("\nlambda.min (lowest CV error — keeps most variables):\n")
print(coef(lasso_cv, s = "lambda.min"))

cat("\nlambda.1se (simplest model within 1 SE — more shrinkage):\n")
print(coef(lasso_cv, s = "lambda.1se"))






# PLOTS
# --- Plot 1: Variable importance ---
imp_df_clean <- imp_df %>%
  mutate(
    variable = case_when(
      variable == "salary_disclosed" ~ "Salary disclosed",
      variable == "german_required"  ~ "German required",
      variable == "degree_required"  ~ "Degree required",
      variable == "remote_option"    ~ "Remote/hybrid",
      variable == "working_time"     ~ "Working time (control)",
      variable == "contract_type"    ~ "Contract type (control)",
      variable == "english_friendly" ~ "English-friendly",
      variable == "is_junior"        ~ "Junior role",
      TRUE ~ variable
    ),
    is_control = str_detect(variable, "control")
  )

p_imp <- ggplot(imp_df_clean,
                aes(x = importance,
                    y = reorder(variable, importance),
                    fill = is_control)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(importance, 4)),
            hjust = -0.1, size = 3.2, color = "grey30") +
  scale_fill_manual(values = c("FALSE" = "#4472C4", "TRUE" = "#B0B0B0"),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(
    title    = "Random Forest: variable importance",
    subtitle = "Permutation importance — contribution to Vienna vs. non-Vienna classification\nGrey bars = control variables",
    x        = "Mean decrease in accuracy",
    y        = NULL,
    caption  = "Random Forest with 500 trees, 80/20 train-test split."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 9),
    plot.caption       = element_text(color = "grey50", size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 11)
  )

print(p_imp)
ggsave("plot_rf_importance.png", p_imp, width = 7, height = 5, dpi = 300)
cat("Saved: plot_rf_importance.png\n")


# --- Plot 2: ROC curve ---
roc_data <- roc_curve(results, truth, .pred_Vienna, event_level = "second")

p_roc <- ggplot(roc_data, aes(x = 1 - specificity, y = sensitivity)) +
  geom_path(color = "#4472C4", linewidth = 1) +
  geom_abline(linetype = "dashed", color = "grey50") +
  annotate("text", x = 0.75, y = 0.25,
           label = paste0("AUC = 0.583"),
           size = 4, color = "#4472C4") +
  labs(
    title    = "ROC curve — Random Forest classifier",
    subtitle = "Vienna vs. non-Vienna job postings",
    x        = "False positive rate (1 - specificity)",
    y        = "True positive rate (sensitivity)",
    caption  = "Dashed line = random classifier (AUC = 0.5)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 10),
    plot.caption  = element_text(color = "grey50", size = 8)
  )

print(p_roc)
ggsave("plot_roc_curve.png", p_roc, width = 6, height = 5, dpi = 300)
cat("Saved: plot_roc_curve.png\n")

print(p_imp)
print(p_roc)

# --- Plot 3: Posting volume by region ---
p_volume <- df_clean %>%
  filter(!is.na(region)) %>%
  count(region) %>%
  mutate(
    region    = fct_reorder(region, n),
    is_vienna = region == "Wien"
  ) %>%
  ggplot(aes(x = n, y = region, fill = is_vienna)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = scales::comma(n)),
            hjust = -0.1, size = 3.5, color = "grey30") +
  scale_fill_manual(values = c("TRUE" = "#4472C4", "FALSE" = "#B0C4DE"),
                    guide = "none") +
  scale_x_continuous(
    labels = scales::comma_format(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Job posting volume by Austrian region",
    subtitle = "Total postings in dataset — Vienna highlighted",
    x        = "Number of postings",
    y        = NULL,
    caption  = "Source: AMS job postings data, 2025."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    plot.caption       = element_text(color = "grey50", size = 8),
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 11)
  )

print(p_volume)
ggsave("plot_posting_volume.png", p_volume, width = 7, height = 5, dpi = 300)
cat("Saved: plot_posting_volume.png\n")


# --- Plot 4: Vienna vs Non-Vienna bar chart ---
bar_data <- df_model %>%
  mutate(location = if_else(is_vienna == 1, "Vienna", "Non-Vienna")) %>%
  group_by(location) %>%
  summarise(
    `Degree required`  = mean(degree_required,  na.rm = TRUE),
    `German required`  = mean(german_required,  na.rm = TRUE),
    `English-friendly` = mean(english_friendly, na.rm = TRUE),
    `Junior role`      = mean(is_junior,        na.rm = TRUE),
    `Salary disclosed` = mean(salary_disclosed, na.rm = TRUE),
    `Remote/hybrid`    = mean(remote_option,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-location, names_to = "variable", values_to = "share") %>%
  mutate(variable = factor(variable, levels = c(
    "Degree required", "Remote/hybrid", "German required",
    "English-friendly", "Junior role", "Salary disclosed"
  )))

p_bar <- ggplot(bar_data, aes(x = share, y = variable, fill = location)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = scales::percent(share, accuracy = 0.1)),
            position = position_dodge(width = 0.7),
            hjust = -0.1, size = 3, color = "grey30") +
  scale_fill_manual(
    values = c("Vienna" = "#4472C4", "Non-Vienna" = "#ED7D31"),
    name   = ""
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.90),
    expand = c(0, 0)
  ) +
  labs(
    title    = "Demand-side frictions: Vienna vs. non-Vienna job postings",
    subtitle = "Share of postings with each characteristic",
    x        = "Share of postings",
    y        = NULL,
    caption  = "Source: AMS job postings data, 2025."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 10),
    plot.caption       = element_text(color = "grey50", size = 8),
    legend.position    = "top",
    panel.grid.minor   = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.text.y        = element_text(size = 11)
  )

print(p_bar)
ggsave("plot_vienna_comparison.png", p_bar, width = 8, height = 5, dpi = 300)
cat("Saved: plot_vienna_comparison.png\n")


# --- Plot 5: Regional heatmap ---
region_long <- df_model %>%
  filter(!is.na(region)) %>%
  group_by(region) %>%
  summarise(
    n                  = n(),
    `Degree required`  = mean(degree_required,  na.rm = TRUE),
    `German required`  = mean(german_required,  na.rm = TRUE),
    `English-friendly` = mean(english_friendly, na.rm = TRUE),
    `Junior role`      = mean(is_junior,        na.rm = TRUE),
    `Salary disclosed` = mean(salary_disclosed, na.rm = TRUE),
    `Remote/hybrid`    = mean(remote_option,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(-c(region, n), names_to = "variable", values_to = "share")

region_order <- df_model %>%
  filter(!is.na(region)) %>%
  count(region, sort = TRUE) %>%
  pull(region)

region_long <- region_long %>%
  mutate(
    region_label = factor(region, levels = rev(region_order)),
    share_label  = scales::percent(share, accuracy = 1)
  )

p_heatmap <- ggplot(region_long,
                    aes(x = variable, y = region_label, fill = share)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = share_label), size = 3,
            color = ifelse(region_long$share > 0.55, "white", "grey20")) +
  scale_fill_gradient2(
    low      = "#f7f7f7",
    mid      = "#4472C4",
    high     = "#1a3a6b",
    midpoint = 0.35,
    labels   = scales::percent_format(accuracy = 1),
    name     = "Share"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Job posting characteristics by Austrian Bundesland",
    subtitle = "Share of postings with each feature",
    x        = NULL,
    y        = NULL,
    caption  = "Source: AMS job postings data, 2025. Regions ordered by posting volume."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey40", size = 9),
    plot.caption  = element_text(color = "grey50", size = 8),
    axis.text.x   = element_text(angle = 30, hjust = 0, size = 10),
    axis.text.y   = element_text(size = 10),
    panel.grid    = element_blank()
  )

print(p_heatmap)
ggsave("plot_regional_heatmap.png", p_heatmap, width = 9, height = 5.5, dpi = 300)
cat("Saved: plot_regional_heatmap.png\n")

cat("\nAll plots saved:\n")
cat("  plot_posting_volume.png\n")
cat("  plot_vienna_comparison.png\n")
cat("  plot_regional_heatmap.png\n")
cat("  plot_rf_importance.png\n")
cat("  plot_roc_curve.png\n")


# --- Lasso coefficient plot ---
lasso_coefs <- coef(lasso_cv, s = "lambda.1se") %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  rename(coefficient = lambda.1se) %>%
  filter(variable != "(Intercept)") %>%
  mutate(
    variable = case_when(
      variable == "degree_required"  ~ "Degree required",
      variable == "german_required"  ~ "German required",
      variable == "english_friendly" ~ "English-friendly",
      variable == "is_junior"        ~ "Junior role",
      variable == "salary_disclosed" ~ "Salary disclosed",
      variable == "remote_option"    ~ "Remote/hybrid",
      TRUE ~ variable
    ),
    zeroed    = coefficient == 0,
    direction = case_when(
      coefficient > 0  ~ "Vienna",
      coefficient < 0  ~ "Non-Vienna",
      TRUE             ~ "Removed"
    )
  )

p_lasso <- ggplot(lasso_coefs,
                  aes(x = coefficient,
                      y = reorder(variable, coefficient),
                      fill = direction)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text(
    aes(label = ifelse(zeroed, "removed by Lasso",
                       round(coefficient, 3)),
        hjust = ifelse(coefficient >= 0, -0.1, 1.1)),
    size = 3.2, color = "grey30"
  ) +
  scale_fill_manual(
    values = c("Vienna" = "#4472C4", "Non-Vienna" = "#ED7D31", "Removed" = "#D3D3D3"),
    name   = "Associated with"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  labs(
    title    = "Lasso regression: surviving coefficients",
    subtitle = "lambda.1se — variables shrunk to zero are removed from the model",
    x        = "Coefficient",
    y        = NULL,
    caption  = "Logistic Lasso with 10-fold cross-validation. Positive = more likely in Vienna."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 13),
    plot.subtitle      = element_text(color = "grey40", size = 9),
    plot.caption       = element_text(color = "grey50", size = 8),
    legend.position    = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.y        = element_text(size = 11)
  )

print(p_lasso)
ggsave("plot_lasso_coefficients.png", p_lasso, width = 7, height = 5, dpi = 300)
cat("Saved: plot_lasso_coefficients.png\n")


