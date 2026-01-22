# ============================
# ReproIND thresholds
# ============================

# ---- DATA INPUT ----
df <- read.csv2("malf.csv")   # use read.csv() if comma-separated
df <- subset(df, fec > 0)     # safety

# Create binary indicator for affected females
df$fem <- ifelse(df$malf > 1, 1, 0)

# ---- PARAMETERS ----
set.seed(42)
n_resamples <- 100000
group_size <- 50

# Storage
prop_embryo <- numeric(n_resamples)
prop_fem    <- numeric(n_resamples)

# ---- BOOTSTRAP LOOP ----
for (i in 1:n_resamples) {
  
  idx <- sample(nrow(df), size = group_size, replace = TRUE)
  
  # ReproIND endpoint 1: embryo malformation proportion
  prop_embryo[i] <- sum(df$malf[idx]) / sum(df$fec[idx])
  
  # ReproIND endpoint 2: proportion of affected females
  prop_fem[i] <- mean(df$fem[idx])
}

# ---- SUMMARY STATISTICS ----

# Embryo malformation proportion
emb_mean <- mean(prop_embryo)
emb_p90  <- quantile(prop_embryo, 0.90)
emb_ci   <- quantile(prop_embryo, c(0.05, 0.95))  # 90% CI

# Proportion of affected females
fem_mean <- mean(prop_fem)
fem_p90  <- quantile(prop_fem, 0.90)
fem_ci   <- quantile(prop_fem, c(0.05, 0.95))     # 90% CI

# ---- OUTPUT ----
cat("\n=== EMBRYO MALFORMATION PROPORTION ===\n")
cat("Mean:", round(emb_mean, 4), "\n")
cat("P90 :", round(emb_p90, 4), "\n")
cat("90% CI:", round(emb_ci[1], 4), "-", round(emb_ci[2], 4), "\n")

cat("\n=== PROPORTION OF AFFECTED FEMALES ===\n")
cat("Mean:", round(fem_mean, 4), "\n")
cat("P90 :", round(fem_p90, 4), "\n")
cat("90% CI:", round(fem_ci[1], 4), "-", round(fem_ci[2], 4), "\n")

# ---- OPTIONAL VISUALIZATION ----
par(mfrow = c(1, 2))

hist(prop_embryo, breaks = 50,
     main = "Embryo aberrations",
     xlab = "Aberrant embryo frequency",
     ylab = "Number of Bootstrap Samples",
     col = "lightblue", border = "black")
abline(v = emb_p90, col = "red", lwd = 2, lty = 2)

hist(prop_fem, breaks = 50,
     main = "Affected females",
     xlab = "Frequency of females with aberrant embryos",
     ylab = "Number of Bootstrap Samples",
     col = "lightgreen", border = "black")
abline(v = fem_p90, col = "red", lwd = 2, lty = 2)

par(mfrow = c(1, 1))

# ---- VISUALIZE P90 WITH ERROR BARS ----

# Build a data frame for plotting
df_plot <- data.frame(
  Endpoint = c("Aberrant embryos", "Affected Females"),
  P90 = c(emb_p90, fem_p90),
  Lower = c(emb_ci[1], fem_ci[1]),
  Upper = c(emb_ci[2], fem_ci[2])
)

# Load ggplot2
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Plot
ggplot(df_plot, aes(x = Endpoint, y = P90, fill = Endpoint)) +
  geom_bar(stat = "identity", color = "black", width = 0.6) +
  geom_errorbar(aes(ymin = Lower, ymax = Upper),
                width = 0.2, color = "black", linewidth = 0.8) +
  scale_fill_manual(values = c(
    "Aberrant embryos" = "skyblue",
    "Affected Females" = "lightgreen"
  )) +
  labs(title = "P90 Thresholds with 90% Confidence Intervals",
       y = "Proportion",
       x = "") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "none"
  )

