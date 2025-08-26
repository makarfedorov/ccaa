library(yaml); library(dplyr)
source("R/clean_cjk.R"); source("R/load_poems.R")
P <- yaml::read_yaml("config/params.yaml")
poems <- read_poetry_dir(P$paths$tang_traditional) |> 
  mutate(text_clean = clean_cjk(text)) |> 
  filter(nchar(text_clean) >= P$filters$min_chars)
print(dim(poems))
print(head(poems[, c("title","author")]))
