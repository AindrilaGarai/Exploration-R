library(tidyverse)
x1 <- c("Dec", "Apr", "Jan", "Mar")
x2 <- c("Dec", "Apr", "Jam", "Mar")

sort(x1) # sorting by starting alphabet
 
month_levels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

y1 <- factor(x1, levels = month_levels)
y1 # leveling
sort(y1) # now it can sort according to level

y2 <- factor(x2, levels = month_levels)
y2 # As there is no level 'Jam'

y2 <- fct(x2, levels = month_levels) # is a stricter version of factor() that errors if your specification of levels is inconsistent with the values in x.

factor(x1) # simple understanding
fct(x1)
levels(y2)

csv <- "
month,value
Jan,12
Feb,56
Mar,12"; csv

# how a tibble can be formed
df <- read_csv(csv, col_types = cols(month = col_factor(month_levels))) 
df$month

# exploring with gss_cat data
gss_cat

gss_cat |>
  count(race)

relig_summary <- gss_cat |>
  group_by(relig) |>
  summarize(
    tvhours = mean(tvhours, na.rm = TRUE),
    n = n()
  )

ggplot(relig_summary, aes(x = tvhours, y = relig)) +
  geom_point()

ggplot(relig_summary, aes(x = tvhours, y = fct_reorder(relig, tvhours))) +
  geom_point()

relig_summary |>
  mutate(
    relig = fct_reorder(relig, tvhours) # reordering based on median
  ) |>
  ggplot(aes(x = tvhours, y = relig)) +
  geom_point()

rincome_summary <- gss_cat |>
  group_by(rincome) |>
  summarize(
    age = mean(age, na.rm = TRUE),
    n = n()
  )

ggplot(rincome_summary, aes(x = age, y = fct_reorder(rincome, age))) +
  geom_point()

ggplot(rincome_summary, aes(x = age, y = fct_relevel(rincome, "Not applicable"))) +
  geom_point() # that allows you to move any number of levels to any location.

by_age <- gss_cat |>
  filter(!is.na(age)) |>
  count(age, marital) |>
  group_by(age) |>
  mutate(
    prop = n / sum(n)
  )

ggplot(by_age, aes(x = age, y = prop, color = marital)) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1")

ggplot(by_age, aes(x = age, y = prop, color = fct_reorder2(marital, age, prop))) +
  geom_line(linewidth = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(color = "marital")

gss_cat |> # by number of observations with each level (largest first), Reverse order of factor levels
  mutate(marital = marital |> fct_infreq() |> fct_rev()) |>
  ggplot(aes(x = marital)) +
  geom_bar()

gss_cat |> count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid, # Change factor levels manually
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat"
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(
    partyid = fct_recode(partyid,
                         "Republican, strong"    = "Strong republican",
                         "Republican, weak"      = "Not str republican",
                         "Independent, near rep" = "Ind,near rep",
                         "Independent, near dem" = "Ind,near dem",
                         "Democrat, weak"        = "Not str democrat",
                         "Democrat, strong"      = "Strong democrat",
                         "Other"                 = "No answer",
                         "Other"                 = "Don't know",
                         "Other"                 = "Other party"
    )
  )

gss_cat |>
  mutate(
    partyid = fct_collapse(partyid, # Collapse factor levels into manually defined groups
                           "other" = c("No answer", "Don't know", "Other party"),
                           "rep" = c("Strong republican", "Not str republican"),
                           "ind" = c("Ind,near rep", "Independent", "Ind,near dem"),
                           "dem" = c("Not str democrat", "Strong democrat")
    )
  ) |>
  count(partyid)

gss_cat |>
  mutate(relig = fct_lump_lowfreq(relig)) |> # lumps together the least frequent levels, ensuring that "other" is still the smallest level.
  count(relig)

gss_cat |>
  mutate(relig = fct_lump_n(relig, n = 10)) |> # lumps all levels except for the n most frequent 
  count(relig, sort = TRUE)

ordered(c("a", "b", "c")) # ordered according to alphabet

