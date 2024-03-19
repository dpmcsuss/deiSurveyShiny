library(ggplot2)
library(scales)

stack_freq_prop <- function(g_22, g_24, title) {
  g1 <- g_22 +
    geom_col(
      aes(y = count),
      position = position_dodge2(preserve = "single")
    ) +
    scale_x_discrete("", guide = guide_axis(n.dodge = 2), drop = FALSE) +
    scale_fill_discrete(guide = "none", drop = FALSE) +
    geom_label(aes(y = count, label = as.character(count)), vjust = "inward", colour = "black", fontface = "bold", fill = "white", position = position_dodge(width = .9)) +
    ggtitle(paste0(title, "\n2022:"), subtitle = "Top: absolute counts. Bottom: relative proportions.") +
    theme(plot.title = element_text(size = 20, face = "bold"))
  g2 <- g_22 +
    geom_col(
      aes(y = prop_22),
      position = position_dodge(preserve = "single")
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2), drop = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete("", drop = FALSE) +
    geom_label(aes(y = round(prop_22, digits = 2), label = as.character(percent(round(prop_22, digits = 2)))), vjust = "inward", colour = "black", fill = "white", fontface = "bold", position = position_dodge(width = .9)) +
    theme(legend.position = "bottom")

  g1_2024 <- g_24 +
    geom_col(
      aes(y = count),
      position = position_dodge2(preserve = "single")
    ) +
    scale_x_discrete("", guide = guide_axis(n.dodge = 2), drop = FALSE) +
    scale_fill_discrete(guide = "none", drop = FALSE) +
    geom_label(aes(y = count, label = as.character(count)), vjust = "inward", colour = "black", fontface = "bold", fill = "white", position = position_dodge(width = .9)) +
    ggtitle("2024") +
    theme(plot.title = element_text(size = 20, face = "bold"))
  g2_2024 <- g_24 +
    geom_col(
      aes(y = prop_24),
      position = position_dodge(preserve = "single")
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2), drop = FALSE) +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_discrete("", drop = FALSE) +
    geom_label(aes(y = round(prop_24, digits = 2), label = as.character(percent(round(prop_24, digits = 2)))), vjust = "inward", colour = "black", fill = "white", fontface = "bold", position = position_dodge(width = .9)) +
    theme(legend.position = "bottom")

  ggpubr::ggarrange(g1, g2, g1_2024, g2_2024, nrow = 4, align = "v")
}


count_prop_complete <- function(df, ..., .fill = TRUE) {
  fill_vals <- list(prop = NA, count =  NA)
  if(.fill){
    fill_vals <- list(prop = 0, count = 0)
  }

  agreement_level <- c(
    "Strongly disagree",
    "Disagree",
    "Agree",
    "Strongly agree"
  )

  frequency_level <- c(
    "Never",
    "Rarely",
    "Sometimes",
    "Often times"
  )

  if(any(as.character(df$response) %in% agreement_level)){
    df <- df %>% mutate(response = fct_drop(response, only = frequency_level))
  } else if(any(as.character(df$response) %in% frequency_level)){
    df <- df %>% mutate(response = fct_drop(response, only = agreement_level))
  }

  df %>%
    group_by(..., response, year) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(ct_22 = ifelse(year == 2022, count, 0),
           ct_24 = ifelse(year == 2024, count, 0)) %>%
    mutate(prop_22 = count / sum(ct_22)) %>%
    mutate(prop_24 = count / sum(ct_24)) %>%
    mutate(prop = prop_24) %>%
    complete(..., response, fill = fill_vals) %>%
    group_by(...)
}
