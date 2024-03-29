
library(tidyverse)

source("source_all.R")

ddf_s <- ddf %>%
  mutate(
    gender = as_factor(ifelse(
      str_detect(gender, "not listed"),
      "Pref. not listed",
      gender
    )),
    international = as_factor(replace_na(international, "No response")),
    first_gen = as_factor(replace_na(first_gen, "No response"))
  ) %>%
  simplify_race_var()


ddf_s <- ddf_s %>%
  mutate(
    Q34 = ifelse(str_detect(Q34, "No"), "No", Q34),
    Q34 = ifelse(str_detect(Q34, "difficult"), "Yes, because classes are difficult.", Q34),
    Q34 = ifelse(str_detect(Q34, "interest"), "Yes, because I found a new interest.", Q34),
    Q34 = ifelse(str_detect(Q34, "Other"), "Other", Q34),
    Q9 = ifelse(str_detect(Q9, "AP"), "I have taken an AP course related to Math and Computer Science.", Q9),
    Q9 = ifelse(str_detect(Q9, "never"), "I have never participated in anything related to my major before college.", Q9),
    Q9 = ifelse(str_detect(Q9, "workshop"), "I have participated in a computer science related project, workshop, or hackathon.", Q9),
    Q9 = ifelse(str_detect(Q9, "project"), "I have participated in a computer science related project, workshop, or hackathon.", Q9)
  )

original_question_df <- original_question_df %>%
  mutate(
    question_text = ifelse(str_detect(question_text, "dropping"), "Have you ever considered dropping your current major? If so, why?", question_text)
  )

course_num_re <- "[A-Z]{3} [A-Z]{2}\\W*[0-9]{3}"
course_satisfaction_q <- original_question_df %>%
  filter(
    str_detect(question_text, "satisfied"),
    str_detect(question_text, "agreement", negate = TRUE)
  ) %>%
  mutate(question_text = str_extract(question_text, course_num_re))
satisfaction_level <-
  c(
    "Extremely dissatisfied",
    "Somewhat dissatisfied",
    "Neither satisfied nor dissatisfied",
    "Somewhat satisfied",
    "Extremely satisfied"
  )

course_ldf <- ddf_s %>%
  pivot_longer(
    cols = course_satisfaction_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  filter(response != "Did not take this course.") %>%
  left_join(course_satisfaction_q, by = "question_id") %>%
  mutate(
    response =
      factor(
        response,
        levels = satisfaction_level,
        ordered = TRUE
      )
  )

# Get "agreement" questions

agreement_level <- c(
  "Strongly disagree",
  "Never",
  "Disagree",
  "Rarely",
  "Agree",
  "Sometimes",
  "Strongly agree",
  "Often times",
  "Prefer not to say"
)

frequency_level <- c(
  "Strongly disagree",
  "Never",
  "Disagree",
  "Rarely",
  "Agree",
  "Sometimes",
  "Strongly agree",
  "Often times",
  "Prefer not to say"
)

agreement_q <- original_question_df %>%
  filter(
    str_detect(question_text, "agreement")
  ) %>%
  mutate(question_text = str_c(" ", str_extract(question_text, "(?<=: - ).*")))

agreement_ldf <- ddf_s %>%
  pivot_longer(
    cols = agreement_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(agreement_q, by = "question_id") %>%
  mutate(response = factor(response, levels = agreement_level))

# Adjectives

adjectives_q <- original_question_df %>%
  filter(
    str_detect(question_text, "adjectives that best represents")
  ) %>%
  mutate(question_text = str_extract(question_text, "(?<=\\. - ).*"))

adj_ldf <- ddf_s %>%
  pivot_longer(
    cols = adjectives_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(adjectives_q, by = "question_id") %>%
  mutate(response = factor(response, levels = c("1", "2", "3", "4", "5")))

dis_q <- original_question_df %>%
  filter(question_id == "Q36" | question_id == "Q38")

dis_ldf <- ddf_s %>%
  pivot_longer(
    cols = dis_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(dis_q, by = "question_id")

race_q <- original_question_df %>%
  filter(question_id == "Q7")

gender_q <- original_question_df %>%
  filter(question_id == "Q6")

firstgen_q <- original_question_df %>%
  filter(question_id == "Q4")

major_q <- original_question_df %>%
  filter(question_id == "Q1")

international_q <- original_question_df %>%
  filter(question_id == "Q5")

race_ldf <- ddf_s %>%
  pivot_longer(
    cols = "race",
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(race_q, by = "question_id")

gender_ldf <- ddf_s %>%
  pivot_longer(
    cols = "gender",
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(gender_q, by = "question_id")

firstgen_ldf <- ddf_s %>%
  pivot_longer(
    cols = "first_gen",
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(firstgen_q, by = "question_id")

major_ldf <- ddf_s %>%
  pivot_longer(
    cols = "major",
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(firstgen_q, by = "question_id")

international_ldf <- ddf_s %>%
  pivot_longer(
    cols = "international",
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(firstgen_q, by = "question_id")


discrimination_q_tbl <-
  tibble(
    selected_q =
      sort(unique(dis_q$question_text))
  ) %>%
  mutate(
    selected_q_code = str_replace_all(selected_q, "\\W+", "_"),
    title = str_glue("{selected_q}"),
    subsection_title = "Discrimination",
    which_df = "dis"
  )

dep_tbl <-
    tibble(
      selected_q =
        sort(unique(str_sub(course_satisfaction_q$question_text, 1, 6)))
    ) %>%
    mutate(
      selected_q_code = str_replace_all(selected_q, "\\W+", "_"),
      title = str_glue("{selected_q} Department Satisfaction"),
      subsection_title = "Satisfaction",
      which_df = "course"
    )




misc_q <-
  original_question_df %>%
  filter(question_id == "Q19" | question_id == "Q21" | question_id == "Q17" | question_id == "Q34" | question_id == "Q10" | question_id == "Q9")


misc_ldf <- ddf_s %>%
  pivot_longer(
    cols = misc_q$question_id,
    names_to = "question_id",
    values_to = "response",
    values_drop_na = TRUE
  ) %>%
  left_join(misc_q, by = "question_id")

