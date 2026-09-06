library(data.table)
library(lubridate)

war_dates <- data.table(
  war_name      = c("Fountain War", "Bloodbath of B-R5RB",
                    "World War Bee II (Casino War)", "Massacre at M2-XFE"),
  window        = c("a", "a", "b", "b"),
  start_date    = as.Date(c("2013-06-01", "2014-01-27", "2020-07-01", "2020-12-30")),
  end_date      = as.Date(c("2013-10-31", "2014-01-27", "2021-08-31", "2020-12-31")),
  baseline_type = c("pre_war", "pre_war", "pre_war", "war_own_average"),
  parent_war    = c(NA, NA, NA, "World War Bee II (Casino War)")
)