library(foreign)
library(plyr)
library(magrittr)
library(dplyr)
library(tidyr)

gfk_names <- read.csv("\\\\50184_PCI\\DC1\\5. Analysis\\Microwave\\GfK microwave\\Contents of file.csv")
gfk_raw <- read.csv("../Data/GfK_raw_no_free_response.csv")
data_understand <- read.csv("../Data/understandability_index.csv")
data_loweffort <- read.csv("../Data/loweffort_screen.csv")
data_dropout <- read.csv("../Data/dropout_status.csv")
data_usability <- read.csv("../Data/usability_vars.csv") %>% rename(RespId = respid)

facs <-  c("n", "l", "s", "g", "o")
facs2 <- sapply(1:4, function(x) sapply((x+1):5, function(y) paste0(facs[x], facs[y])))
nlevs <- c(
  n = 2,
  l = 3,
  s = 2,
  g = 3,
  o = 2
)
facnames <- c(
  n = "Reference",
  l = "Attributes",
  s = "Source",
  g = "Format",
  o = "Sort"
)
levnames <- list(
  n = c("No", "Yes"),
  l = c("Low", "High", "Progressive"), 
  s = c("Objective", "Both"),
  g = c("Numbers", "Graphs", "Icons"),
  o = c("Distance", "Academics")
)
demo_covars <- c(
  "choice_option",
  "child_male",
  "child_iep",
  "child_hispanic",
  "child_black",
  "parent_35plus",
  "parent_degree",
  "low_income",
  "low_internet"#,
  #"missing_internet"
)
A10_encode = c(
  "A10_4" = "Distance",
  "A10_3" = "Academics",
  "A10_7" = "Safety",
  "A10_6" = "Resources",
  "A10_1" = "SpecNeeds",
  "A10_5" = "Diversity"
)

url_var_trans <- gfk_names %>% 
  filter(grepl("^LF_QTA_URL", Variable)) %>% 
  select(Variable, Label) #%$%
  #`names<-`(Label, Variable)

url_factor <- gfk_raw %>% 
  select(starts_with("LF_QTA_URL")) %>% 
  # mutate_each(funs(. %>% 
  #                    mapvalues(c("No", "Yes"), c(0, 1)) %>% 
  #                    as.character %>% 
  #                    as.numeric)) %>% 
  as.matrix() %>% 
  multiply_by_matrix(1:ncol(.)) %>% 
  drop %>% 
  factor(labels = url_var_trans$Label)

#Use this code to generate export variable of treatment arm
# gfk_raw %>% 
#   select(RespId) %>% 
#   mutate(finder_url = url_factor) %>% 
#   {tidyr::extract(., finder_url, facnames, "N(\\d)L(\\d)S(\\d)G(\\d)O(\\d)", 
#                   remove = FALSE, convert = TRUE)} %>% 
#   mutate(
#     Reference = factor(Reference, 1:nlevs["n"], levnames[["n"]]),
#     Attributes = factor(Attributes, 1:nlevs["l"], levnames[["l"]]),
#     Source = factor(Source, 1:nlevs["s"], levnames[["s"]]),
#     Format = factor(Format, 1:nlevs["g"], levnames[["g"]]),
#     Sort = factor(Sort, 1:nlevs["o"], levnames[["o"]])
#   ) %>% 
#   mutate_each(funs(lab = as.character, num = as.numeric),
#               one_of(facnames)) %>% 
#   select(-one_of(facnames)) %>% 
#   write.csv("../Data/treatment_arms.csv")
# 
# gfk_raw %>% 
#   transmute(
#     RespId,
#     choice_option = mapvalues(A3, c(0, 1, 998, 4), c(F, T, NA, NA)),
#     child_male = as.logical(A9_1),
#     child_iep = as.logical(A9_2),
#     child_hispanic = (C7 == 1),
#     child_black = (C8 == 3),
#     parent_35plus = (C1 >= 3),
#     parent_degree = (C4 >= 3),
#     low_income = (C5 <= 2),
#     low_internet = (B2 <= 2)
#   ) %>% 
#   mutate_each(funs("na" = is.na, "raw" = identity), one_of(demo_covars)) %>% 
#   #`names<-`(gsub("_identity", "", names(.))) %>% 
#   #select(-matches("_identity")) %>% 
#   mutate_each(funs(mapvalues(., NA, 0)), one_of(demo_covars)) %>% 
#   mutate_each(funs(as.numeric)) %>% 
#   write.csv("../Data/demographic_covariates.csv")
  
rdat <- gfk_raw %>% 
  left_join(data_loweffort) %>% 
  left_join(data_understand) %>% 
  left_join(data_dropout) %>% 
  left_join(data_usability) %>% 
  mutate(finder_url = url_factor) %>% 
  filter(!is.na(finder_url)) %>% 
  {tidyr::extract(., finder_url, facnames, "N(\\d)L(\\d)S(\\d)G(\\d)O(\\d)", 
          remove = FALSE, convert = TRUE)} %>% 
  mutate(
    Reference = factor(Reference, 1:nlevs["n"], levnames[["n"]]),
    Attributes = factor(Attributes, 1:nlevs["l"], levnames[["l"]]),
    Source = factor(Source, 1:nlevs["s"], levnames[["s"]]),
    Format = factor(Format, 1:nlevs["g"], levnames[["g"]]),
    Sort = factor(Sort, 1:nlevs["o"], levnames[["o"]]),
    choice_option = mapvalues(A3, c(0, 1, 998, 4), c(F, T, NA, NA)),
    child_male = as.logical(A9_1),
    child_iep = as.logical(A9_2),
    child_hispanic = (C7 == 1),
    child_black = (C8 == 3),
    parent_35plus = (C1 >= 3),
    parent_degree = (C4 >= 3),
    low_income = (C5 <= 2),
    low_internet = (B2 <= 2),
    #missing_internet = is.na(B2),
    subgroup_low_education = (C4 <= 2),
    subgroup_low_income = (C5 <= 3),
    subgroup_nonhigh_internet = (B2 <= 4),
    subgroup_choice_experience = (A3 == 1) | (A4 == 1)
  ) %>% 
  filter(
    #loweffort == FALSE,
    # !is.na(usab_ease),
    # !is.na(usab_satis),
    # !is.na(Understand_index),
    # !is.na(Understand_timebonus),
    dropoutstat == 5
  ) %>%
  select(
    RespId,
    one_of(facnames), 
    one_of(demo_covars),
    starts_with("Understand"),
    starts_with("SORTRANKING"),
    one_of(names(A10_encode)),
    starts_with("usab"),
    starts_with("subgroup"),
    dropoutstat,
    loweffort
  ) %>%
  mutate_each(funs(as.numeric), starts_with("Understand"))
nrow(rdat)

#rdat <- rdat[,sapply(rdat, function(x) !all(is.na(x)))]

rdat_proc <- rdat %>% 
  mutate_each(funs("na" = is.na, identity), one_of(demo_covars)) %>% 
  #`names<-`(gsub("_identity", "", names(.))) %>% 
  select(-matches("_identity")) %>% 
  mutate_each(funs(mapvalues(., NA, 0, warn = FALSE)), one_of(demo_covars))

demo_covars_plusna <- c(demo_covars, paste0(demo_covars, "_na"))

rdat <- rdat_proc
#demo_covars <- demo_covars_plusna
rdat <- filter(rdat, loweffort == FALSE)

#write.csv(gfk_fix, "\\\\mathematica.Net\\NDrive\\Project\\50184_PCI\\DC1\\5. Analysis\\Data\\GfK_final_data_proc_2.csv")

###Additional data validation checks
rdat_classes <- sapply(rdat, class)
# message(sprintf(
#   "Produced data frame `rdat` with %d rows and variables %s",
#   nrow(rdat), 
#   paste0(names(rdat), " (", rdat_classes, ")", collapse = ", ")
# ))
message(sprintf("Produced data frame `rdat` with %d rows", nrow(rdat)))