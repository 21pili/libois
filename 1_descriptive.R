# Préambule
source("0_aux.R")



cleaned <- detect_and_remove_collinear(score, fmeanscore3, remove = FALSE)
score <- cleaned$data


# Génère le résumé des statistiques avec deux chiffres significatifs
stats <- function(data) {
    summary <- data %>%
        reframe(
            var = colnames(.),
            median = sapply(., function(x) signif(median(x, na.rm = TRUE), 2)),
            mean = sapply(., function(x) signif(mean(x, na.rm = TRUE), 2)),
            min = sapply(., function(x) signif(min(x, na.rm = TRUE), 2)),
            max = sapply(., function(x) signif(max(x, na.rm = TRUE), 2)),
            sd = sapply(., function(x) signif(sd(x, na.rm = TRUE), 2)),
            iqr = sapply(., function(x) signif(IQR(x, na.rm = TRUE), 2)),
            skewness = sapply(., function(x) signif(skewness(x, na.rm = TRUE), 2)),
            kurtosis = sapply(., function(x) signif(kurtosis(x, na.rm = TRUE), 2)),
            non_missing = sapply(., function(x) signif(mean(!is.na(x)), 2))
        )
    return(summary)
}

stats_score <- stats(score)


stargazer(stats_score,
    type = "latex",
    summary = FALSE,
    rownames = FALSE, digits = 2,
    title = "Robust Descriptive Statistics",
    out = "OUTPUT/TABLES/summary_statistics.tex",
    font.size = "scriptsize",
    column.sep.width = "0pt"
)