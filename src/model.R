library(Amelia)
library(MASS)
library(arsenal)
library(tidyverse)

yrbs_17 <- read_csv("./data/YRBS_2017.csv") %>%
  dplyr::select(HowOldAreYou, RaceEth, WhatIsYourSex, FruitEating, GreenSaladEating, CarrotEating, OtherVegetableEating, RidingWithADrinkingDriver, SafetyConcernsAtSchool, WereThreatenedOrInjuredWithAWeaponOnSchoolProperty, PhysicalFighting, SexualViolence, PhysicalDatingViolence, BullyingAtSchool, ElectronicBullying, PhysicalActivity5OrMoreDays, TelevisionWatching, ComputerUse, SportsTeamParticipation) %>%
  mutate(ThreatenedWithWeapon = WereThreatenedOrInjuredWithAWeaponOnSchoolProperty) %>%
  dplyr::select(-WereThreatenedOrInjuredWithAWeaponOnSchoolProperty)

ggplot(yrbs_17, aes(x = RidingWithADrinkingDriver)) + geom_histogram()
ggplot(yrbs_17, aes(x = SafetyConcernsAtSchool)) + geom_histogram()
ggplot(yrbs_17, aes(x = ThreatenedWithWeapon)) + geom_histogram()
ggplot(yrbs_17, aes(x = PhysicalFighting)) + geom_histogram()
ggplot(yrbs_17, aes(x = SexualViolence)) + geom_histogram()
ggplot(yrbs_17, aes(x = PhysicalDatingViolence)) + geom_histogram()

proportion_of_missing_values <- data.frame(variable = colnames(yrbs_17), missingness = colSums(is.na(yrbs_17)) / nrow(yrbs_17)) %>%
  filter(missingness > 0) %>%
  ggplot(aes(x = reorder(variable, -missingness), y = missingness)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  xlab("Proportion") +
  ylab("Variable") +
  ggtitle("Proportion of Missing Values")
proportion_of_missing_values
ggsave("./figures/proportion_of_missing_values.png", proportion_of_missing_values)

missmap(yrbs_17, margins = c(10, 5))
png("./figures/missingness_map.png")
missmap(yrbs_17, margins = c(10, 5))
dev.off()

servings_per_day <- Vectorize(function(servings) {
  return(switch(servings, 0, 0, 0, 1, 2, 3, 4))
})

screen_per_day <- Vectorize(function(hours) {
  return(switch(hours, 0, 0, 1, 2, 3, 4, 5))
})

fv <- yrbs_17 %>%
  drop_na() %>%
  mutate(Age = sapply(HowOldAreYou, switch, 12, 13, 14, 15, 16, 17, 18)) %>%
  mutate(Ethnicity = factor(RaceEth, labels = c("American Indian/Alaska Native", "Asian", "Black", "Native Hawaiian or Other Pacific Islander", "White", "Hispanic/Latino", "Multiple - Hispanic/Latino", "Multiple - Non-Hispanic/Latino"))) %>%
  mutate(Ethnicity = relevel(Ethnicity, ref = "White")) %>%
  mutate(Sex = factor(WhatIsYourSex, labels = c("Female", "Male"))) %>%
  mutate(FruitEating = servings_per_day(FruitEating)) %>%
  mutate(GreenSaladEating = servings_per_day(GreenSaladEating)) %>%
  mutate(CarrotEating = servings_per_day(CarrotEating)) %>%
  mutate(OtherVegetableEating = servings_per_day(OtherVegetableEating)) %>%
  mutate(VegetableEating = GreenSaladEating + CarrotEating + OtherVegetableEating) %>%
  mutate(FV = (FruitEating + VegetableEating) >= 5) %>%
  mutate(DrinkingDriver = RidingWithADrinkingDriver > 1) %>%
  mutate(SafetyConcerns = SafetyConcernsAtSchool > 1) %>%
  mutate(Threatened = ThreatenedWithWeapon > 1) %>%
  mutate(Fighting = PhysicalFighting > 1) %>%
  mutate(SexualViolence = SexualViolence > 1) %>%
  mutate(PhysicalDatingViolence = PhysicalDatingViolence > 1) %>%
  mutate(BullyingAtSchool = BullyingAtSchool > 1) %>%
  mutate(ElectronicBullying = ElectronicBullying > 1) %>%
  mutate(Bullying = BullyingAtSchool | ElectronicBullying) %>%
  mutate(PhysicalActivity = PhysicalActivity5OrMoreDays) %>%
  mutate(TelevisionWatching = screen_per_day(TelevisionWatching)) %>%
  mutate(ComputerUse = screen_per_day(ComputerUse)) %>%
  mutate(ScreenTime = TelevisionWatching + ComputerUse) %>%
  mutate(Sports = sapply(SportsTeamParticipation, switch, 0, 1, 2, 3)) %>%
  dplyr::select(FV, Sports, Ethnicity, Age, Sex, DrinkingDriver, SafetyConcerns, Threatened, Fighting, SexualViolence, PhysicalDatingViolence, Bullying, PhysicalActivity, ScreenTime)

descriptive_table <- tableby(FV ~ ., data = fv)
summary(descriptive_table)
capture.output(summary(descriptive_table), file = "./figures/descriptive_statistics.md")

OR_table <- function(model) {
  cbind(coef(model), confint(model)) %>%
    exp() %>%
    round(2) %>%
    as.data.frame() %>%
    mutate(Sig = ifelse(1 >= `2.5 %` & 1 <= `97.5 %`, "", "*")) %>%
    return()
}

crude_model <- glm(FV ~ Sports, data = fv, family = "binomial")
OR_table(crude_model)

model <- glm(FV ~ Sports + Ethnicity + ., data = fv, family = "binomial")
OR_table(model)

ethnicity_or_table <- data.frame()
for (ethnicity in unique(fv$Ethnicity)) {
  or_table <- cbind(ethnicity, OR_table(glm(FV ~ Sports + ., data = dplyr::select(filter(fv, Ethnicity == ethnicity), -Ethnicity), family = "binomial"))[2,])
  row.names(or_table) <- NULL
  ethnicity_or_table <- rbind(ethnicity_or_table, or_table)
}
ethnicity_or_table

ethnicity_or_forest_plot <- ggplot(ethnicity_or_table, aes(x = V1, y = ethnicity)) +
  geom_point(shape = 18, size = 5) +
  geom_errorbarh(aes(xmin = `2.5 %`, xmax = `97.5 %`), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  xlab("Odds Ratio (95% CI)") + 
  ylab("Race and Ethnicity") +
  coord_cartesian(xlim = c(0.5, 4))
ethnicity_or_forest_plot
ggsave("./figures/ethnicity_or_forest_plot.png", ethnicity_or_forest_plot)
