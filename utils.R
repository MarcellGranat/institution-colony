library(tidyverse)

NiceName <- function(x) {
  case_when(
    x == "logpgp95" ~ "Log GDP per capita (PPP) in 1995",
    x == "loghjypl" ~ "Log output pper worker in 1988",
    x == "avexpr" ~ "Average protection against expropriation risk",
    x == "cons1" ~ "Constrain on executive in 1900",
    x == "democ00a" ~ "Democracy in 1900",
    x == "euro1900" ~ "Eurpean settlements in 1900",
    x == "logem4" ~ "Log European settler mortaility",
    TRUE ~ x
  )
}
