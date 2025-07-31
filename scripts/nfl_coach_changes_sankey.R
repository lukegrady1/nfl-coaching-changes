#!/usr/bin/env Rscript
# ------------------------------------------------------------------
# nfl_coach_changes_sankey.R
# Generate an interactive Sankey diagram of NFL coaching changes
# from 2024 → 2025, colored by role (HC / OC / DC).
# ------------------------------------------------------------------

# 1. Load libraries
library(tidyverse)
library(networkD3)
library(htmlwidgets)

# 2. Read the data
coaches_2024 <- read_csv("data/coaches_2024.csv")
coaches_2025 <- read_csv("data/coaches_2025.csv")

# 3. Pivot to long form
long24 <- coaches_2024 %>%
  pivot_longer(
    cols = c(head_coach, offensive_coordinator, defensive_coordinator),
    names_to  = "role",
    values_to = "coach_2024"
  )

long25 <- coaches_2025 %>%
  pivot_longer(
    cols = c(head_coach, offensive_coordinator, defensive_coordinator),
    names_to  = "role",
    values_to = "coach_2025"
  )

# 4. Join and count flows
flows <- long24 %>%
  inner_join(long25, by = c("team", "role")) %>%
  count(coach_2024, coach_2025, role, name = "value")

# 5. Build nodes & links tables
nodes <- data.frame(name = unique(c(flows$coach_2024, flows$coach_2025)))

links <- flows %>%
  mutate(
    source = match(coach_2024, nodes$name) - 1,  # zero-based
    target = match(coach_2025, nodes$name) - 1
  ) %>%
  select(source, target, value, role)

# 6. Define a color scale for the three roles
colourScale <- 'd3.scaleOrdinal()
  .domain(["head_coach","offensive_coordinator","defensive_coordinator"])
  .range(["#1f77b4","#ff7f0e","#2ca02c"])'

# 7. Create the Sankey widget
sankey <- sankeyNetwork(
  Links       = links,
  Nodes       = nodes,
  Source      = "source",
  Target      = "target",
  Value       = "value",
  NodeID      = "name",
  LinkGroup   = "role",       # color flows by role
  colourScale = JS(colourScale),
  fontSize    = 12,
  nodeWidth   = 30
)

# 8. Save standalone HTML
saveWidget(
  widget     = sankey,
  file       = "output/nfl_coach_changes.html",
  selfcontained = TRUE
)
