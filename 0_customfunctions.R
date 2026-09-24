#Function to dummy code multiple select variables (created by Claude)
split_multiselect <- function(data, var) {
  data %>%
   select(Participant_ID, response = all_of(var)) %>%
    drop_na(response) %>%
    separate_rows(response, sep = ",") %>%
    pivot_wider(
      names_from = response,
      values_from = response,
      values_fill = list(response = "0")) %>%
    mutate(across(-Participant_ID, ~ as.factor(as.numeric(. != "0")))) %>%
    rename_with(~ str_c(var, "_", .), -Participant_ID)
}

#Function to make demographics frequency tables that works with either single-select or multiple-select variables
#single-select: counts and percent of each response
#multiple-select: n and percent selecting each response (assumes dummy coding)
demo_freq <- function(data, var) {
  dummy_cols <- str_subset(names(data), str_c("^", var, "_[0-9]+$")) 
#Creates a search for variables like dem_race_1, dem_race_2 etc. because that is how dummy-coded multiple select variables can be identified 

    if(length(dummy_cols) > 0) {
#Asks whether the list has anything in it (a non-empty list means a multiple select variable was identified)
    data %>%
        select(all_of(dummy_cols)) %>%
      summarize(across(everything(),
                       list(n = ~ sum(. == 1, na.rm = TRUE),
                            percent = ~ mean(. == 1, na.rm = TRUE) * 100)))
  } else {
    data %>%
      count(across(all_of(var))) %>%
      mutate(percent = (n / sum(n)) * 100)
  }
}