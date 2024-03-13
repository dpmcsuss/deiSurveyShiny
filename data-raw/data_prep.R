library(tidyverse)

survey_file_24 <- "data-raw/2024 DEITC Student Survey_March 11, 2024_Final All.csv"
headers_24 <- read_csv(survey_file_24, n_max = 1)
df_24 <- read_csv(survey_file_24,
                  skip = 2,
                  col_names = FALSE) %>%
    setNames(names(headers_24)) %>%
    mutate(year = 2024)

survey_file_22 <- "data-raw/Computing and Data Sciences Climate Survey_April 21, 2023_08.24.csv"
headers_22 <- read_csv(survey_file_22, n_max = 1)
df_22 <- read_csv(survey_file_22,
                  skip = 3,
                  col_names = FALSE) %>%
    setNames(names(headers_22)) %>%
    mutate(year = 2022)

original_question_df <- headers_24 %>%
    pivot_longer(everything(), names_to = "question_id", values_to = "question_text")


save(original_question_df, file = "R/original_question_df.rda")

ddf <- df_24 %>%
    mutate(StartDate = as_datetime(StartDate),
           EndDate = as_datetime(EndDate),
           RecordedDate = as_datetime(RecordedDate)) %>%
    bind_rows(df_22) %>%
    mutate(work_status = Q21) %>%
    mutate(prep = Q10) %>%
    rename(
        major = Q1,
        major_other_txt = Q1_7_TEXT,
        grad_year = Q2,
        minor = Q3,
        minor_other_txt = Q3_6_TEXT,
        first_gen = Q4,
        international = Q5,
        international_other_txt = Q5_3_TEXT,
        gender = Q6,
        gender_other = Q6_4_TEXT,
        race = Q7,
        race_other_txt = Q7_8_TEXT,
        ## pronouns = Q8,
        ## pronouns_other_txt = Q8_15_TEXT
        )
save(ddf, file = "R/ddf.rda")
