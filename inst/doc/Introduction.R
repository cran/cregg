## ----opts, echo=FALSE-------------------------------------------------------------------------------------------------
options(width = 120)
knitr::opts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = TRUE, size="small", fig.width = 10, fig.height = 10)

## ----load-------------------------------------------------------------------------------------------------------------
library("cregg")
data("immigration")

## ----mmplot-----------------------------------------------------------------------------------------------------------
# descriptive plotting
f1 <- ChosenImmigrant ~ Gender + LanguageSkills + PriorEntry +
       Education * Job + CountryOfOrigin * ReasonForApplication +
       JobExperience + JobPlans
plot(mm(immigration, f1, id = ~ CaseID), vline = 0.5)

## ----amce-------------------------------------------------------------------------------------------------------------
# estimation
amces <- cj(immigration, f1, id = ~ CaseID)
head(amces[c("feature", "level", "estimate", "std.error")], 20L)

## ----plot_amce--------------------------------------------------------------------------------------------------------
# plotting of AMCEs
plot(amces)

## ----amce_diagnostic, fig.height = 5----------------------------------------------------------------------------------
amce_diagnostic <- amce_by_reference(immigration, 
                                     ChosenImmigrant ~ LanguageSkills, 
                                     ~LanguageSkills, 
                                     id = ~ CaseID)
plot(amce_diagnostic, group = "REFERENCE", legend_title = "Reference Category")

## ----reorder, fig.height = 5------------------------------------------------------------------------------------------
# calculate marginal means and order output by estimate
country_mms <- mm(immigration, 
                  ChosenImmigrant ~ CountryOfOrigin, 
                  id = ~CaseID)
country_mms <- country_mms[order(country_mms$estimate),]
# reorder feature in preference ascending order
immigration$CountryOfOrigin2 <- factor(immigration$CountryOfOrigin,
                                       levels = c("Iraq", "Somalia", "Sudan", 
                                                  "China", "India", "Mexico",
                                                  "France", "Poland", "Philippines",
                                                  "Germany"))
country2_amces <- amce(immigration, 
                       ChosenImmigrant ~ CountryOfOrigin2, 
                       id = ~CaseID)
plot(country2_amces)

## ----mm_by, fig.height=8----------------------------------------------------------------------------------------------
immigration$contest <- factor(immigration$contest_no)
mm_by <- cj(immigration, 
            ChosenImmigrant ~ Gender + Education + LanguageSkills,
            id = ~ CaseID, 
            estimate = "mm", 
            by = ~ contest)
plot(mm_by, group = "contest", vline = 0.5)

## ----cj_anova---------------------------------------------------------------------------------------------------------
cj_anova(immigration, 
         ChosenImmigrant ~ Gender + Education + LanguageSkills, 
         by = ~ contest_no)

## ----conditional_mms--------------------------------------------------------------------------------------------------
## the original plot is split at the value of 10, which is not a true median split
immigration$ethnosplit <- NA_real_
immigration$ethnosplit[immigration$ethnocentrism <= 10] <- 1L
immigration$ethnosplit[immigration$ethnocentrism > 10] <- 2L
immigration$ethnosplit <- factor(immigration$ethnosplit, 
                                 1:2, c("Low Ethnocentrism", "High Ethnocentrism"))
x <- cj(na.omit(immigration), 
        f1, 
        id = ~ CaseID, 
        estimate = "mm", 
        by = ~ ethnosplit)
plot(x, group = "ethnosplit", vline = 0.5)

## ----conditional_amces, dependson = c("conditional_mms"), fig.height = 4----------------------------------------------
# calculate conditional AMCEs
amces <- cj(na.omit(immigration), 
            ChosenImmigrant ~ ReasonForApplication + LanguageSkills,
            id = ~ CaseID, 
            estimate = "amce", 
            by = ~ ethnosplit)
diff_amces <- cj(na.omit(immigration), 
                 ChosenImmigrant ~ ReasonForApplication + LanguageSkills,
                 id = ~ CaseID, 
                 estimate = "amce_diff", 
                 by = ~ ethnosplit)
plot(rbind(amces, diff_amces)) + ggplot2::facet_wrap(~BY, ncol = 3L)

## ----conditional_differences, dependson = c("conditional_amces"), fig.height = 4--------------------------------------
# calculate conditional MMs
mms <- cj(na.omit(immigration), 
          ChosenImmigrant ~ ReasonForApplication + LanguageSkills,
          id = ~ CaseID, 
          estimate = "mm", 
          by = ~ ethnosplit)
diff_mms <- cj(na.omit(immigration), 
               ChosenImmigrant ~ ReasonForApplication + LanguageSkills,
               id = ~ CaseID, 
               estimate = "mm_diff", 
               by = ~ ethnosplit)
plot(rbind(mms, diff_mms)) + ggplot2::facet_wrap(~BY, ncol = 3L)

## ----amce_vs_mm, fig.height = 4---------------------------------------------------------------------------------------
diff_amces$Estimate <- "AMCE Difference"
diff_mms$Estimate <- "MM Difference"
plot(rbind(diff_amces, diff_mms), feature_headers = FALSE) +
  ggplot2::facet_wrap(~Estimate, ncol = 2L)

## ----interaction_amces, dependson = c("conditional_amces"), fig.height = 4--------------------------------------------
# calculate interaction AMCEs (ACIEs)
amces_2 <- cj(immigration,
            ChosenImmigrant ~ LanguageSkills,
            id = ~CaseID, estimate = "amce", 
            by = ~Gender)
diff_amces_2 <- cj(immigration, 
                 ChosenImmigrant ~ LanguageSkills, 
                 id = ~CaseID, estimate = "amce_diff", 
                 by = ~Gender)
plot(rbind(amces_2, diff_amces_2)) + ggplot2::facet_wrap(~BY, ncol = 3L)

## ----interaction_amces_2, dependson = c("interaction_amces"), fig.height = 6------------------------------------------
# calculate interaction AMCEs (ACIEs)
amces_2 <- cj(immigration,
            ChosenImmigrant ~ LanguageSkills,
            id = ~CaseID, estimate = "amce", 
            by = ~Job)
plot(amces_2) + ggplot2::facet_wrap(~BY, ncol = 3L)

## ----interaction_mms, dependson = c("interaction_amces_2"), fig.height = 6--------------------------------------------
# calculate interaction MMs
mms <- cj(immigration,
            ChosenImmigrant ~ LanguageSkills,
            id = ~CaseID, estimate = "mm", 
            by = ~Job)
plot(mms, vline = 0.5) + ggplot2::facet_wrap(~BY, ncol = 3L)

## ----interactions, dependson = c("interaction_mms"), fig.height = 5---------------------------------------------------
immigration$language_entry <- 
  interaction(immigration$LanguageSkills, immigration$PriorEntry, sep = "_")
# average component interaction effects
acies <- amce(immigration,
              ChosenImmigrant ~ language_entry,
              id = ~CaseID)
plot(acies)
# higher order MMs
interaction_mms <- mm(immigration, 
                      ChosenImmigrant ~ language_entry,
                      id = ~CaseID)
plot(interaction_mms, vline = 0.5)
                     

## ----plot_freqs-------------------------------------------------------------------------------------------------------
# plotting of display frequencies
plot(cj_freqs(immigration, f1, id = ~ CaseID))

## ----table_freqs------------------------------------------------------------------------------------------------------
subset(cj_props(immigration, ~ Job + Education, id = ~ CaseID), Proportion == 0)
subset(cj_props(immigration, ~ CountryOfOrigin + ReasonForApplication, id = ~ CaseID), Proportion == 0)

## ----balance_testing, fig.height=8------------------------------------------------------------------------------------
plot(mm(immigration,
        ethnocentrism ~ Job + Education + CountryOfOrigin + ReasonForApplication,
        id = ~ CaseID),
     xlim = c(10,30),
     vline = mean(immigration$ethnocentrism, na.rm = TRUE))

## ----leftright--------------------------------------------------------------------------------------------------------
immigration$profile_fac <- factor(immigration$profile)
plot(cj(immigration, f1, id = ~CaseID, by = ~ profile_fac, estimate = "mm"),
     group = "profile_fac", vline = 0.5)

