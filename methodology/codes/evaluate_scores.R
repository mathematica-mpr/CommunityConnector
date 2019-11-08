setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)

data <- read.csv('../../data/final_data.csv')
dict <- read.csv('../../data/final_data_dictionary.csv')

score_cor <- function(data){
  cor(data[,names(data)[grepl("sdoh_score", names(data))]]) 
}
score_cor(data)
# Econ & Edu, Food, Community, & Healthcare
# Phys & Edu
# Educ & Food, Community, Healthcare
# Food & Community
# Community & Healthcare

# variation in outcome by score plots
outcomes <- dict %>% 
  filter(outcome == 1) %>% 
  select(column_name) %>% 
  pull() %>% 
  as.character()

score_outcome_plots <- function(outcomes, data){
  par(mfrow = c(2, 3), mar = c(0.5, 0.5, 0.5, 0.5))
  for(outcome in outcomes){
    for(sdoh in names(data)[grepl("sdoh_score", names(data))]){
      plot(data[,outcome], data[,sdoh], main = paste0(outcome, " vs. ", sdoh)) 
      form <- as.formula(paste0(sdoh, " ~ ", outcome))
      abline(lm(form, data = data), col = "purple")
    }
  } 
}
score_outcome_plots(outcomes, data)
# strongest relationships: eduction & economic scores & neighborhood
# the spend variables are kind of confusing - neighborhood is the only one that with a higher score, has a lower std spend (quality)

# scores X econ scores
score_score_plots <- function(data){
  par(mfrow = c(2, 3), mar = c(0.5, 0.5, 0.5, 0.5))
  for(sdoh1 in names(data)[grepl("sdoh_score", names(data))]){
    for(sdoh in names(data)[grepl("sdoh_score", names(data))]){
      if(sdoh1 != sdoh){
        plot(data[,sdoh1], data[,sdoh], main = paste0(sdoh1, " vs. ", sdoh)) 
        form <- as.formula(paste0(sdoh, " ~ ", sdoh1))
        abline(lm(form, data = data), col = "purple") 
      }
    }
  } 
}
score_score_plots(data)
# 2 and 6 is slightly negative

# look only at the econ scores
score_Escore_plots <- function(data){
  par(mfrow = c(2, 3), mar = c(0.5, 0.5, 0.5, 0.5))
  sdoh1 <- "sdoh_score_1"
  for(sdoh in names(data)[grepl("sdoh_score", names(data))]){
    if(sdoh1 != sdoh){
      plot(data[,sdoh1], data[,sdoh], main = paste0(sdoh1, " vs. ", sdoh)) 
      form <- as.formula(paste0(sdoh, " ~ ", sdoh1))
      abline(lm(form, data = data), col = "purple") 
    }
  }
}
score_Escore_plots(data)

# Different ways to adjust to bring correlations down/less driven by economic score
# 1. Divide by Econ Score, re-standardize
meth1 <- data.frame("sdoh_score_1" = data$sdoh_score_1, apply(data[,names(data)[grepl("sdoh_score", names(data)) & names(data) != "sdoh_score_1"]], 2,
                                                              function(x) x/data$sdoh_score_1))
meth1 <- meth1 %>% 
  cbind(data[,!grepl("sdoh_score", names(data))])
# the ones that weren't related before are now more correlated
score_cor(data)
score_cor(meth1)
score_outcome_plots(outcomes, meth1)
score_score_plots(meth1)

# 2. Use correlation with Econ Score as a weight so the unrelated variables don't become more related
cors <- as.data.frame(score_cor(data))$sdoh_score_1
meth2 <- data
for(score in c(2:6)){
  new_scores <- ((data[,paste0("sdoh_score_", score)]/data$sdoh_score_1)*cors[score]) +
    (1-cors[score])*data[,paste0("sdoh_score_", score)]
  # then need to re-standardize
  final_scores <- (new_scores - min(new_scores))/(max(new_scores) - min(new_scores))
  print(new_scores - final_scores)
  meth2[,paste0("sdoh_score_", score)] = final_scores
}
score_cor(meth2)
# they are way less correlated now!
# highest correlation = .36 between 2 and 3 or .33 between 4 and 5
score_Escore_plots(meth2)
# 3. Correlation squared?

# 4. Subtract from econ score? - what do differences in scores look like by county?
# econ score X econ score-other score

# Let's use method 2
# How to old scores compare to new scores?
new_old_scores <- function(old_data, new_data){
  for(score in c(2:6)){
    plot(old_data[,paste0("sdoh_score_", score)], new_data[,paste0("sdoh_score_", score)],
         main = paste0("Score #", score)) 
  }
}
new_old_scores(data, meth2)
# the most adjustments should happen with the most correlated variables
score_cor(data)
score_outcome_plots(outcomes, meth2)

# Measuring adjustments:
# scores should differentiate performance
# how well do the scores predict performance?
# relationship between one another
orig_mod <- lm(overobese_pct ~ sdoh_score_1 + sdoh_score_2 + sdoh_score_3 + sdoh_score_4 + sdoh_score_5 + sdoh_score_6, data = data)
summary(orig_mod)
inter_mod <- lm(overobese_pct ~ sdoh_score_1 + sdoh_score_2 + sdoh_score_3 + sdoh_score_4 + sdoh_score_5 + sdoh_score_6, data = meth1)
summary(inter_mod)
mod <- lm(overobese_pct ~ sdoh_score_1 + sdoh_score_2 + sdoh_score_3 + sdoh_score_4 + sdoh_score_5 + sdoh_score_6, data = meth2)
summary(mod)
# at least econ shows up now as impactful!

# which counties had the largest change in scores?
for(score in c(2:6)){
  data[,paste0("new_score_", score)] <- meth2[,paste0("sdoh_score_", score)] 
  data[,paste0("new_score_", score, "_diff")] <- data[,paste0("new_score_", score)] - data[,paste0("sdoh_score_", score)] 
  print(data[,c(paste0("new_score_", score, "_diff"), paste0("new_score_", score), paste0("sdoh_score_", score), "sdoh_score_1", "county")] %>% 
          arrange(!!rlang::sym(paste0("new_score_", score, "_diff"))))
}
# if econ > score, score should have gone down or vice versa