HFDparse = function (DF) 
{
  if (any(c("Age", "Cohort", "ARDY") %in% colnames(DF))) {
    if ("Age" %in% colnames(DF)) {
      DF <- mutate(DF, 
                   Age = as.character(Age),
                   Age = parse_number(.data$Age), 
                   OpenInterval = .data$Age %in% range(.data$Age))
    }
    if ("ARDY" %in% colnames(DF)) {
      DF <- mutate(DF, ARDY = parse_number(.data$ARDY), 
                   OpenInterval = .data$ARDY %in% range(.data$ARDY))
    }
    if ("Cohort" %in% colnames(DF)) {
      DF <- relocate(mutate(DF, OpenInterval = grepl(pattern = "\\+", 
                                                     .data$Cohort) | grepl(pattern = "\\-", .data$Cohort), 
                            Cohort = as.character(Cohort),
                            Cohort = parse_number(.data$Cohort)), .data$OpenInterval, 
                     .after = last_col())
    }
  }
  DF
}
