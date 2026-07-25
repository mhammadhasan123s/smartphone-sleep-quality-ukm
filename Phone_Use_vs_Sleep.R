# =============================================================================
# Phone Use vs Sleep Quality Among Students
# Course  : STQD6214 — Mathematical Statistics with Computing
# Authors : Mhamad Shhab Aldeen Hasan    (P166175)
#           Hasma Nizam Bin Mohamad Hassan (P160404)
# Supervisor: Assoc. Prof. Dr. Noratiqah Mohd Ariff
# Institution: Universiti Kebangsaan Malaysia (UKM)
# =============================================================================
# RESEARCH QUESTION
#   How does daily mobile phone usage affect students' sleep quality?
#
# HOW TO RUN
#   1. Place this file and the raw CSV in the same folder.
#   2. Open R / RStudio and set the working directory to that folder:
#         setwd("path/to/your/folder")
#   3. Source the whole file:
#         source("Phone_Use_vs_Sleep.R")
#   All plots are displayed on screen; results are printed to the console.
# =============================================================================

# ── Package ───────────────────────────────────────────────────────────────────
if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")
library(tidyverse)

# ── Shared colour palette (used in every plot) ────────────────────────────────
PAL <- c(
  blue   = "#2E86C1",
  teal   = "#117A65",
  orange = "#E67E22",
  purple = "#7D3C98",
  slate  = "#566573"
)

cat("\n", strrep("=", 65), "\n")
cat("  Phone Use vs Sleep Quality — Full Analysis\n")
cat(strrep("=", 65), "\n\n")


# =============================================================================
# SECTION 1 — DATA LOADING
# =============================================================================
cat(strrep("-", 65), "\n")
cat("SECTION 1 · Data Loading\n")
cat(strrep("-", 65), "\n\n")

# The raw file is the direct CSV export from KoboToolbox (semicolon-separated).
# Adjust the filename if yours differs.
raw <- read.csv(
  "Phone_Use_vs_Sleep_Quality_Raw_data.csv",
  sep              = ";",
  header           = TRUE,
  stringsAsFactors = FALSE,
  encoding         = "latin1"
)

cat(sprintf("Raw export loaded: %d rows × %d columns\n\n", nrow(raw), ncol(raw)))
cat("Column names in raw file:\n")
print(names(raw))


# =============================================================================
# SECTION 2 — DATA CLEANING & TRANSFORMATION
# =============================================================================
cat("\n", strrep("-", 65), "\n")
cat("SECTION 2 · Data Cleaning & Transformation\n")
cat(strrep("-", 65), "\n\n")

# ── 2a. Select the 8 meaningful survey columns ────────────────────────────────
cat("Step 2a — Selecting relevant columns …\n")

clean <- raw %>%
  select(
    age            = `Age..years.`,
    gender         = Gender,
    study_level    = `Current.level.of.study`,
    phone_hours    = `On.average..how.many.hours.per.day.do.you.use.your.smartphone.`,
    app_type       = `What.type.of.application.do.you.use.most.frequently.on.your.smartphone.`,
    sleep_hours    = `On.average..how.many.hours.do.you.sleep.per.night.`,
    sleep_latency  = `On.average..how.long.does.it.take.you.to.fall.asleep.at.night..minutes.`,
    sleep_quality  = `Overall..how.would.you.rate.your.sleep.quality.`
  )

cat(sprintf("  Columns reduced: %d → 8\n", ncol(raw)))

# ── 2b. Drop the one fully-blank row (KoboToolbox sometimes adds a trailer) ───
cat("\nStep 2b — Removing fully-blank rows …\n")
before <- nrow(clean)
clean  <- clean %>% filter(!if_all(everything(), is.na))
cat(sprintf("  Rows: %d → %d  (removed %d blank row(s))\n",
            before, nrow(clean), before - nrow(clean)))

# ── 2c. Fix character-encoding issues from KoboToolbox export ─────────────────
# KoboToolbox exports UTF-8 curly quotes/dashes that get mangled in latin1.
# "Bachelor's Degree" arrives as "Bachelorâs Degree"
# "16–30 minutes"     arrives as "16â\x80\x9330 minutes"
cat("\nStep 2c — Fixing encoding (mojibake) in text columns …\n")

fix_encoding <- function(x) {
  if (!is.character(x)) return(x)
  x <- gsub("\x80\x93", "-",  x, fixed = TRUE)   # en-dash  → hyphen
  x <- gsub("\x80\x99", "'",  x, fixed = TRUE)   # curly '  → straight '
  x <- gsub("â",        "",   x, fixed = TRUE)   # leftover byte
  x <- gsub("--",       "-",  x, fixed = TRUE)   # double-hyphen artefact
  trimws(x)
}

clean$study_level   <- fix_encoding(clean$study_level)
clean$sleep_latency <- fix_encoding(clean$sleep_latency)
clean$app_type      <- fix_encoding(clean$app_type)

cat("  study_level  values after fix:", paste(unique(clean$study_level),  collapse = " | "), "\n")
cat("  sleep_latency values after fix:", paste(unique(clean$sleep_latency), collapse = " | "), "\n")

# ── 2d. Recode ordinal text → numeric scores ──────────────────────────────────
# Higher score = WORSE sleep (enables correlation, t-tests, regression)
cat("\nStep 2d — Recoding ordinal variables to numeric scores …\n")

clean$sleep_quality_score <- case_when(
  clean$sleep_quality == "Very good"   ~ 1L,
  clean$sleep_quality == "Fairly good" ~ 2L,
  clean$sleep_quality == "Fairly bad"  ~ 3L,
  clean$sleep_quality == "Very bad"    ~ 4L
)

clean$sleep_latency_ord <- case_when(
  clean$sleep_latency == "Less than 15 minutes" ~ 1L,
  clean$sleep_latency == "16-30 minutes"        ~ 2L,
  clean$sleep_latency == "31-60 minutes"        ~ 3L,
  clean$sleep_latency == "More than 60 minutes" ~ 4L
)

cat("  sleep_quality_score distribution:\n")
print(table(clean$sleep_quality_score, useNA = "ifany"))

# ── 2e. Factor encoding ───────────────────────────────────────────────────────
cat("\nStep 2e — Converting categorical columns to factors …\n")

clean$gender <- factor(clean$gender)

clean$study_level <- factor(
  clean$study_level,
  levels = c("Diploma", "Bachelor's Degree", "Master", "PhD")
)

clean$app_type <- factor(clean$app_type)

clean$sleep_quality <- factor(
  clean$sleep_quality,
  levels  = c("Very good", "Fairly good", "Fairly bad", "Very bad"),
  ordered = TRUE
)

clean$sleep_latency <- factor(
  clean$sleep_latency,
  levels  = c("Less than 15 minutes", "16-30 minutes",
               "31-60 minutes", "More than 60 minutes"),
  ordered = TRUE
)

# ── 2f. Derived variable — user group ─────────────────────────────────────────
cat("\nStep 2f — Creating user_group variable …\n")

clean$user_group <- cut(
  clean$phone_hours,
  breaks         = c(-Inf, 4, 10, Inf),
  labels         = c("Light user (≤4 h)", "Mid user (4–10 h)", "Heavy user (>10 h)"),
  right          = TRUE,
  ordered_result = TRUE
)

cat("  User group counts:\n")
print(table(clean$user_group, useNA = "ifany"))

# ── 2g. Missing value check ───────────────────────────────────────────────────
cat("\nStep 2g — Missing value check …\n")
na_counts <- colSums(is.na(clean))
print(na_counts)

# Drop rows missing either key analysis variable
n_before   <- nrow(clean)
clean      <- clean %>% drop_na(phone_hours, sleep_quality_score)
cat(sprintf("\n  Rows after dropping key-NA: %d → %d\n", n_before, nrow(clean)))

# ── 2h. Save the cleaned dataset ─────────────────────────────────────────────
write.csv(clean, "phone_sleep_clean.csv", row.names = FALSE)
cat(sprintf("\n  ✅ Cleaned dataset saved: phone_sleep_clean.csv  (%d rows × %d cols)\n",
            nrow(clean), ncol(clean)))

cat("\nFinal structure of clean data:\n")
glimpse(clean)


# =============================================================================
# SECTION 3 — DESCRIPTIVE STATISTICS
# =============================================================================
cat("\n", strrep("-", 65), "\n")
cat("SECTION 3 · Descriptive Statistics\n")
cat(strrep("-", 65), "\n\n")

n <- nrow(clean)

# ── 3a. Phone usage ───────────────────────────────────────────────────────────
cat("── Phone Usage (hours/day) ──────────────────────────────\n")
cat(sprintf("  n       = %d\n",   n))
cat(sprintf("  Mean    = %.2f h\n", mean(clean$phone_hours)))
cat(sprintf("  SD      = %.2f h\n", sd(clean$phone_hours)))
cat(sprintf("  Median  = %.0f h\n", median(clean$phone_hours)))
cat(sprintf("  Q1–Q3   = %.0f – %.0f h\n",
            quantile(clean$phone_hours, 0.25),
            quantile(clean$phone_hours, 0.75)))
cat(sprintf("  Min–Max = %.0f – %.0f h\n\n",
            min(clean$phone_hours), max(clean$phone_hours)))

cat("  Insight: Students use smartphones an average of 7.8 h/day\n")
cat("  (median 6 h). Right-skewed — a minority of heavy users\n")
cat("  pulls the mean above the median.\n\n")

# ── 3b. Sleep quality ─────────────────────────────────────────────────────────
cat("── Sleep Quality Score (1 = Very good … 4 = Very bad) ──\n")
cat(sprintf("  Mean    = %.2f\n", mean(clean$sleep_quality_score)))
cat(sprintf("  SD      = %.2f\n", sd(clean$sleep_quality_score)))
cat(sprintf("  Median  = %.0f\n", median(clean$sleep_quality_score)))
cat("\n  Frequency table:\n")
print(table(clean$sleep_quality))

cat("\n  Insight: Most students report 'Fairly good' sleep (score 2).\n")
cat("  Very bad sleep is rare (only 1 respondent).\n\n")

# ── 3c. Sleep hours ───────────────────────────────────────────────────────────
cat("── Sleep Hours per Night ────────────────────────────────\n")
cat(sprintf("  Mean = %.2f h   SD = %.2f h\n\n",
            mean(clean$sleep_hours, na.rm = TRUE),
            sd(clean$sleep_hours,   na.rm = TRUE)))

# ── 3d. Demographics ──────────────────────────────────────────────────────────
cat("── Demographics ─────────────────────────────────────────\n")
cat("  Gender:\n"); print(table(clean$gender))
cat("\n  Study level:\n"); print(table(clean$study_level))
cat("\n  App type:\n");    print(table(clean$app_type))

# ── 3e. Visualisations ────────────────────────────────────────────────────────
cat("\n  Generating descriptive plots …\n")

# Plot 1 — Histogram: Daily phone usage
hist(clean$phone_hours,
     main   = "Daily Smartphone Usage",
     xlab   = "Hours per Day",
     ylab   = "Number of Students",
     col    = PAL["blue"],
     border = "white",
     breaks = 12,
     las    = 1)
abline(v = mean(clean$phone_hours), col = PAL["orange"], lwd = 2, lty = 2)
legend("topright",
       legend = sprintf("Mean = %.1f h", mean(clean$phone_hours)),
       col = PAL["orange"], lty = 2, lwd = 2, bty = "n")

# Plot 2 — Histogram: Sleep quality score
hist(clean$sleep_quality_score,
     main   = "Sleep Quality Score Distribution",
     xlab   = "Score  (1 = Very good … 4 = Very bad)",
     ylab   = "Number of Students",
     col    = PAL["teal"],
     border = "white",
     breaks = 4,
     las    = 1)
abline(v = mean(clean$sleep_quality_score), col = PAL["orange"], lwd = 2, lty = 2)
legend("topright",
       legend = sprintf("Mean = %.2f", mean(clean$sleep_quality_score)),
       col = PAL["orange"], lty = 2, lwd = 2, bty = "n")

# Plot 3 — Pie chart: App types
app_counts <- sort(table(clean$app_type), decreasing = TRUE)
pie(app_counts,
    main   = "Distribution of Application Types",
    col    = unname(PAL),
    labels = paste0(names(app_counts), "\n(n=", app_counts, ")"),
    cex    = 0.82)

# Plot 4 — Pie chart: User groups
grp_counts <- table(clean$user_group)
pie(grp_counts,
    main   = "Distribution of User Groups",
    col    = c(PAL["blue"], PAL["orange"], PAL["teal"]),
    labels = paste0(names(grp_counts), "\n(n=", grp_counts, ")"),
    cex    = 0.88)

# Plot 5 — Boxplot: Sleep quality by user group
boxplot(sleep_quality_score ~ user_group,
        data    = clean,
        col     = c(PAL["blue"], PAL["orange"], PAL["teal"]),
        border  = "grey30",
        main    = "Sleep Quality Score by Phone Usage Group",
        xlab    = "Phone Usage Group",
        ylab    = "Sleep Quality Score  (higher = worse)",
        las     = 1)

# Plot 6 — Bar chart: Gender × Study level
tab <- table(clean$study_level, clean$gender)
barplot(tab,
        beside  = TRUE,
        col     = c(PAL["blue"], PAL["teal"], PAL["orange"], PAL["purple"]),
        legend  = rownames(tab),
        main    = "Respondents by Gender and Study Level",
        xlab    = "Gender",
        ylab    = "Count",
        args.legend = list(x = "topright", bty = "n"),
        las     = 1)

cat("  ✅ Descriptive plots complete.\n")


# =============================================================================
# SECTION 4 — HYPOTHESIS TESTING & REGRESSION
# =============================================================================
cat("\n", strrep("-", 65), "\n")
cat("SECTION 4 · Hypothesis Testing & Regression\n")
cat(strrep("-", 65), "\n\n")

# ── 4a. Hypothesis Test 1: Phone Usage ───────────────────────────────────────
# H₀: μ_phone = 6 h/day  (national daily average benchmark)
# H₁: μ_phone > 6 h/day  (one-sided, α = 0.05)
cat("── Hypothesis Test 1: Daily Phone Usage ─────────────────\n")
cat("  H₀: μ = 6 h/day\n")
cat("  H₁: μ > 6 h/day  (one-sided, α = 0.05)\n\n")

t1 <- t.test(clean$phone_hours, mu = 6, alternative = "greater")
print(t1)

cat(sprintf("\n  Decision   : %s (p = %.4f)\n",
            ifelse(t1$p.value < 0.05, "REJECT H₀", "FAIL TO REJECT H₀"),
            t1$p.value))
cat("  Interpretation: No significant evidence that mean usage\n")
cat("  exceeds 6 h/day, despite the sample mean being 7.8 h.\n\n")

# ── 4b. Hypothesis Test 2: Sleep Quality ─────────────────────────────────────
# H₀: μ_sleep_score = 2  (score = "Fairly good")
# H₁: μ_sleep_score ≠ 2  (two-sided, α = 0.05)
cat("── Hypothesis Test 2: Sleep Quality Score ───────────────\n")
cat("  H₀: μ = 2  ('Fairly good')\n")
cat("  H₁: μ ≠ 2  (two-sided, α = 0.05)\n\n")

t2 <- t.test(clean$sleep_quality_score, mu = 2, alternative = "two.sided")
print(t2)

cat(sprintf("\n  Decision   : %s (p = %.4f)\n",
            ifelse(t2$p.value < 0.05, "REJECT H₀", "FAIL TO REJECT H₀"),
            t2$p.value))
cat("  Interpretation: Sleep quality is statistically\n")
cat("  consistent with 'Fairly good' across the sample.\n\n")

# ── 4c. Pearson Correlation ───────────────────────────────────────────────────
cat("── Pearson Correlation: Phone Hours vs Sleep Quality ────\n")

cor_result <- cor.test(clean$phone_hours,
                       clean$sleep_quality_score,
                       method = "pearson")
print(cor_result)

r <- cor_result$estimate
cat(sprintf("\n  r = %.3f  →  %s positive relationship\n",
            r,
            ifelse(abs(r) < 0.3, "Weak",
                   ifelse(abs(r) < 0.6, "Moderate", "Strong"))))
cat(sprintf("  p = %.4f  →  %s\n\n",
            cor_result$p.value,
            ifelse(cor_result$p.value < 0.05,
                   "Statistically significant at α = 0.05",
                   "Not statistically significant at α = 0.05")))

# ── 4d. Simple Linear Regression ─────────────────────────────────────────────
cat("── Simple Linear Regression ─────────────────────────────\n")
cat("  Model: sleep_quality_score ~ phone_hours\n\n")

model <- lm(sleep_quality_score ~ phone_hours, data = clean)
print(summary(model))

b0 <- coef(model)[1]
b1 <- coef(model)[2]
cat(sprintf("\n  Equation : sleep_quality_score = %.4f + %.4f × phone_hours\n", b0, b1))
cat(sprintf("  R²       = %.4f  (%.1f%% of variance explained)\n",
            summary(model)$r.squared,
            summary(model)$r.squared * 100))
cat(sprintf("  Slope p  = %.4f  →  %s\n\n",
            summary(model)$coefficients[2, 4],
            ifelse(summary(model)$coefficients[2, 4] < 0.05,
                   "Slope is significant",
                   "Slope is NOT statistically significant")))
cat("  Interpretation: Higher phone use is slightly associated\n")
cat("  with worse sleep, but the effect is weak and not\n")
cat("  statistically significant in this sample.\n\n")

# ── 4e. Regression visualisations ────────────────────────────────────────────
cat("  Generating regression plots …\n")

# Plot 7 — Scatter plot with regression line
plot(clean$phone_hours,
     clean$sleep_quality_score,
     xlab = "Phone Usage (Hours/Day)",
     ylab = "Sleep Quality Score  (1 = Best, 4 = Worst)",
     main = "Phone Usage vs Sleep Quality",
     pch  = 19,
     col  = adjustcolor(PAL["blue"], alpha.f = 0.75),
     cex  = 1.2,
     las  = 1,
     ylim = c(0.5, 4.5))
abline(model, col = PAL["orange"], lwd = 2.5, lty = 2)
legend("topleft",
       legend = c("Respondent",
                  sprintf("y = %.2f + %.3f·x  (r = %.2f)", b0, b1, r)),
       col  = c(PAL["blue"], PAL["orange"]),
       pch  = c(19, NA),
       lty  = c(NA, 2),
       lwd  = c(NA, 2.5),
       bty  = "n", cex = 0.85)

# Plot 8 — Regression diagnostics (2×2)
par(mfrow = c(2, 2))
plot(model, col = PAL["blue"], pch = 19)
par(mfrow = c(1, 1))

# Plot 9 — Boxplot: sleep quality by sleep latency group
boxplot(sleep_quality_score ~ sleep_latency,
        data   = clean,
        col    = unname(PAL[1:4]),
        border = "grey30",
        main   = "Sleep Quality Score by Time to Fall Asleep",
        xlab   = "Sleep Latency",
        ylab   = "Sleep Quality Score  (higher = worse)",
        las    = 1,
        cex.axis = 0.75)

cat("  ✅ Hypothesis testing & regression plots complete.\n")


# =============================================================================
# SECTION 5 — MONTE CARLO SIMULATION
# =============================================================================
cat("\n", strrep("-", 65), "\n")
cat("SECTION 5 · Monte Carlo Simulation (1,000 resamples)\n")
cat(strrep("-", 65), "\n\n")

set.seed(2025)   # ensures reproducible results
m <- 1000        # number of bootstrap resamples

cat(sprintf("Parameters: m = %d resamples,  n = %d observations\n\n", m, n))

# ── 5a. Sampling distributions of phone usage statistics ─────────────────────
cat("── Part A: Bootstrap Distributions of Phone Usage Stats ─\n\n")

# Build m × n resample matrix (each row is one resample)
x_phone <- matrix(
  sample(clean$phone_hours, size = m * n, replace = TRUE),
  nrow = m
)

mc_means   <- apply(x_phone, 1, mean)
mc_sds     <- apply(x_phone, 1, sd)
mc_medians <- apply(x_phone, 1, median)
mc_ranges  <- apply(x_phone, 1, function(r) diff(range(r)))

cat("  Bootstrap 95% Confidence Intervals:\n")
cat(sprintf("  Mean   : %.2f  [%.2f, %.2f]\n",
            mean(mc_means),
            quantile(mc_means, 0.025),
            quantile(mc_means, 0.975)))
cat(sprintf("  SD     : %.2f  [%.2f, %.2f]\n",
            mean(mc_sds),
            quantile(mc_sds, 0.025),
            quantile(mc_sds, 0.975)))
cat(sprintf("  Median : %.2f  [%.2f, %.2f]\n",
            mean(mc_medians),
            quantile(mc_medians, 0.025),
            quantile(mc_medians, 0.975)))
cat(sprintf("  Range  : %.2f  [%.2f, %.2f]\n\n",
            mean(mc_ranges),
            quantile(mc_ranges, 0.025),
            quantile(mc_ranges, 0.975)))

# Observed values (for reference lines on plots)
obs_mean   <- mean(clean$phone_hours)
obs_sd     <- sd(clean$phone_hours)
obs_median <- median(clean$phone_hours)
obs_range  <- diff(range(clean$phone_hours))

# Plot 10 — Monte Carlo: phone usage statistics (2×2)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

hist(mc_means,
     xlab   = "Mean Phone Usage (hours)",
     main   = "Monte Carlo Means",
     col    = PAL["blue"],
     border = "white",
     breaks = 30, las = 1)
abline(v = obs_mean, col = PAL["orange"], lwd = 2, lty = 2)
legend("topright", sprintf("Observed = %.1f", obs_mean),
       col = PAL["orange"], lty = 2, lwd = 2, bty = "n", cex = 0.8)

hist(mc_sds,
     xlab   = "Standard Deviation",
     main   = "Monte Carlo SDs",
     col    = PAL["teal"],
     border = "white",
     breaks = 30, las = 1)
abline(v = obs_sd, col = PAL["orange"], lwd = 2, lty = 2)
legend("topright", sprintf("Observed = %.1f", obs_sd),
       col = PAL["orange"], lty = 2, lwd = 2, bty = "n", cex = 0.8)

hist(mc_medians,
     xlab   = "Median Phone Usage",
     main   = "Monte Carlo Medians",
     col    = PAL["orange"],
     border = "white",
     breaks = 15, las = 1)
abline(v = obs_median, col = PAL["purple"], lwd = 2, lty = 2)
legend("topright", sprintf("Observed = %.0f", obs_median),
       col = PAL["purple"], lty = 2, lwd = 2, bty = "n", cex = 0.8)

hist(mc_ranges,
     xlab   = "Range",
     main   = "Monte Carlo Ranges",
     col    = PAL["purple"],
     border = "white",
     breaks = 25, las = 1)
abline(v = obs_range, col = PAL["orange"], lwd = 2, lty = 2)
legend("topright", sprintf("Observed = %.0f", obs_range),
       col = PAL["orange"], lty = 2, lwd = 2, bty = "n", cex = 0.8)

par(mfrow = c(1, 1))

# ── 5b. Bootstrap correlation distribution ────────────────────────────────────
cat("── Part B: Bootstrap Distribution of Pearson Correlation ─\n\n")

mc_corrs <- numeric(m)
for (i in seq_len(m)) {
  s           <- clean[sample(seq_len(n), replace = TRUE), ]
  mc_corrs[i] <- cor(s$phone_hours, s$sleep_quality_score)
}

obs_corr <- cor(clean$phone_hours, clean$sleep_quality_score)
ci_corr  <- quantile(mc_corrs, c(0.025, 0.975))

cat(sprintf("  Observed Pearson r       : %.4f\n",   obs_corr))
cat(sprintf("  Bootstrap mean r         : %.4f\n",   mean(mc_corrs)))
cat(sprintf("  Bootstrap SD of r        : %.4f\n",   sd(mc_corrs)))
cat(sprintf("  95%% Bootstrap CI         : [%.4f, %.4f]\n",
            ci_corr[1], ci_corr[2]))
cat(sprintf("  Proportion of r > 0      : %.1f%%\n", 100 * mean(mc_corrs > 0)))
cat(sprintf("  Proportion of |r| < 0.3  : %.1f%%  (weak relationship)\n\n",
            100 * mean(abs(mc_corrs) < 0.3)))

# Plot 11 — Monte Carlo: correlation distribution
hist(mc_corrs,
     xlab   = "Pearson Correlation Coefficient (r)",
     main   = "Monte Carlo Correlation Distribution",
     col    = PAL["slate"],
     border = "white",
     breaks = 30,
     las    = 1)
abline(v = obs_corr,   col = PAL["orange"], lwd = 2.5, lty = 2)
abline(v = ci_corr[1], col = PAL["teal"],   lwd = 1.5, lty = 3)
abline(v = ci_corr[2], col = PAL["teal"],   lwd = 1.5, lty = 3)
abline(v = 0,          col = "red",          lwd = 1,   lty = 1)
legend("topright",
       legend = c(sprintf("Observed r = %.2f",          obs_corr),
                  sprintf("95%% CI [%.2f, %.2f]",       ci_corr[1], ci_corr[2]),
                  "r = 0  (no correlation)"),
       col  = c(PAL["orange"], PAL["teal"], "red"),
       lty  = c(2, 3, 1),
       lwd  = c(2.5, 1.5, 1),
       bty  = "n", cex = 0.85)

cat("  ✅ Monte Carlo simulation complete.\n")


# =============================================================================
# SECTION 6 — CONCLUSION & SUMMARY
# =============================================================================
cat("\n", strrep("=", 65), "\n")
cat("SECTION 6 · Summary of Findings\n")
cat(strrep("=", 65), "\n\n")

cat("  Sample        : n =", n, "UKM students\n")
cat(sprintf("  Phone usage   : Mean = %.1f h/day (SD = %.1f), Median = %.0f h\n",
            mean(clean$phone_hours), sd(clean$phone_hours),
            median(clean$phone_hours)))
cat(sprintf("  Sleep quality : Mean score = %.2f ('Fairly good')\n\n",
            mean(clean$sleep_quality_score)))

cat("  Hypothesis Test 1 (Phone Usage > 6 h):\n")
cat(sprintf("    p = %.4f → %s\n\n",
            t1$p.value,
            ifelse(t1$p.value < 0.05, "REJECT H₀", "FAIL TO REJECT H₀")))

cat("  Hypothesis Test 2 (Sleep Quality = 2):\n")
cat(sprintf("    p = %.4f → %s\n\n",
            t2$p.value,
            ifelse(t2$p.value < 0.05, "REJECT H₀", "FAIL TO REJECT H₀")))

cat(sprintf("  Correlation   : r = %.3f  (weak positive, not significant)\n\n", r))

cat(sprintf("  Regression    : sleep_score = %.2f + %.3f × phone_hours\n",
            b0, b1))
cat(sprintf("                  R² = %.1f%% of variance explained\n\n",
            summary(model)$r.squared * 100))

cat("  Monte Carlo   : Bootstrap confirms r ≈ 0.2 is a weak,\n")
cat("                  consistent but unreliable signal.\n")
cat(sprintf("                  95%% CI for r: [%.2f, %.2f]\n\n",
            ci_corr[1], ci_corr[2]))

cat("  CONCLUSION:\n")
cat("  While heavy smartphone use is common among UKM students,\n")
cat("  its impact on sleep quality appears WEAK in this sample.\n")
cat("  No statistically significant relationship was found.\n")
cat("  Larger samples and objective sleep measures (e.g.,\n")
cat("  actigraphy) are recommended for future studies.\n\n")

cat(strrep("=", 65), "\n")
cat("  Analysis complete. 11 plots displayed.\n")
cat("  Cleaned data saved to: phone_sleep_clean.csv\n")
cat(strrep("=", 65), "\n")
