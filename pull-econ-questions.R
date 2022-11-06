## Load required packages
library(haven)
library(dplyr)
library(tidyr)
library(readr)
## Pull the economic questions from each year's data and join them together
dat = Reduce(full_join, lapply(2012:2017, function(y) {
    tmp = read_sav(paste0("TAPSdata", y, ".sav"))
    nms = names(tmp)[grepl("^econ[124][AB]", names(tmp), ignore.case = TRUE)]
    return(tmp[ , c("WUSTLID", nms)])
}))
## Convert the responses to their character representation
## (have to do this first to get a consistent numeric coding, some years differ)
as_character = function(labelled_item) as.character(as_factor(labelled_item))
dat = dat %>%
    mutate(across(starts_with("ECON"), as_character))
## Generate a "long" dataset, where there are only 2 + {n_questions} columns
long_dat = dat %>%
    pivot_longer(
        !WUSTLID,
        names_to = c(".value", "wave"),
        names_pattern = "(.+)S(.+)"
    )
## Generate ascending numeric coding of questions
long_dat = long_dat %>%
    mutate(
        ECON1A = case_when(
            grepl("Getting much worse", ECON1A, ignore.case = TRUE) ~ 1,
            grepl("Getting somewhat worse", ECON1A, ignore.case = TRUE) ~ 2,
            grepl("Not changing much", ECON1A, ignore.case = TRUE) ~ 3,
            grepl("Getting somewhat better", ECON1A, ignore.case = TRUE) ~ 4,
            grepl("Getting much better", ECON1A, ignore.case = TRUE) ~ 5,
            TRUE ~ NA_real_
        ),
        ECON1B = case_when(
            grepl("Getting much worse", ECON1B, ignore.case = TRUE) ~ 1,
            grepl("Getting somewhat worse", ECON1B, ignore.case = TRUE) ~ 2,
            grepl("Not changing much", ECON1B, ignore.case = TRUE) ~ 3,
            grepl("Getting somewhat better", ECON1B, ignore.case = TRUE) ~ 4,
            grepl("Getting much better", ECON1B, ignore.case = TRUE) ~ 5,
            TRUE ~ NA_real_
        ),
        ECON2A = case_when(
            grepl("Poor", ECON2A, ignore.case = TRUE) ~ 1,
            grepl("Only fair", ECON2A, ignore.case = TRUE) ~ 2,
            grepl("Good", ECON2A, ignore.case = TRUE) ~ 3,
            grepl("Excellent", ECON2A, ignore.case = TRUE) ~ 4,
            TRUE ~ NA_real_
        ),
        ECON2B = case_when(
            grepl("Poor", ECON2B, ignore.case = TRUE) ~ 1,
            grepl("Only fair", ECON2B, ignore.case = TRUE) ~ 2,
            grepl("Good", ECON2B, ignore.case = TRUE) ~ 3,
            grepl("Excellent", ECON2B, ignore.case = TRUE) ~ 4,
            TRUE ~ NA_real_
        ),
        ECON4A = case_when(
            grepl("Much less", ECON4A, ignore.case = TRUE) ~ 1,
            grepl("Less", ECON4A, ignore.case = TRUE) ~ 2,
            grepl("About the same", ECON4A, ignore.case = TRUE) ~ 3,
            grepl("Much more", ECON4A, ignore.case = TRUE) ~ 5,
            grepl("More", ECON4A, ignore.case = TRUE) ~ 4,
            TRUE ~ NA_real_
        ),
        ECON4B = case_when(
            grepl("Much less", ECON4B, ignore.case = TRUE) ~ 1,
            grepl("Less", ECON4B, ignore.case = TRUE) ~ 2,
            grepl("About the same", ECON4B, ignore.case = TRUE) ~ 3,
            grepl("Much more", ECON4B, ignore.case = TRUE) ~ 5,
            grepl("More", ECON4B, ignore.case = TRUE) ~ 4,
            TRUE ~ NA_real_
        )
    )
## Generate a wider dataset where each question-wave is treated as a separate Q
wide_dat = long_dat %>%
    pivot_longer(!c(WUSTLID, wave), names_to = "question") %>%
    pivot_wider(names_from = WUSTLID, values_from = value)
## Write out results
write_csv(long_dat, "TAPS-econ-data-long.csv")
write_csv(wide_dat, "TAPS-econ-data-wide.csv")
