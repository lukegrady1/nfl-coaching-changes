#!/usr/bin/env Rscript

# app.R ─── A polished Shiny Sankey for NFL Coaching Changes ─────────────

library(shiny)
library(tidyverse)
library(networkD3)
library(bslib)

#── 1. Load & wrangle data
coaches_2024 <- read_csv("data/coaches_2024.csv")
coaches_2025 <- read_csv("data/coaches_2025.csv")

long24 <- coaches_2024 %>%
  pivot_longer(
    cols      = c(head_coach, offensive_coordinator, defensive_coordinator),
    names_to  = "role",
    values_to = "coach_2024"
  )

long25 <- coaches_2025 %>%
  pivot_longer(
    cols      = c(head_coach, offensive_coordinator, defensive_coordinator),
    names_to  = "role",
    values_to = "coach_2025"
  )

# keep team through the join so we can order by it later
flow_raw <- long24 %>%
  inner_join(long25, by = c("team", "role"))

# aggregate counts (in case coaches repeat across roles)
all_flows <- flow_raw %>%
  count(team, coach_2024, coach_2025, role, name = "value")

#── 2. D3 color scale for roles
colourScale <- JS('d3.scaleOrdinal()
  .domain(["head_coach","offensive_coordinator","defensive_coordinator"])
  .range(["#2c3e50","#18bc9c","#f39c12"])')

#── 3. UI ───────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "🏈 NFL Coaching Changes 2024→2025",
  id    = "mainNav",
  theme = bs_theme(
    version    = 4,
    bootswatch = "flatly",
    primary    = "#2c3e50",
    secondary  = "#18bc9c",
    base_font  = font_google("Roboto")
  ),
  collapsible = TRUE,

  tabPanel(
    "Sankey Diagram",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        tags$h4("NFL Coaching Changes"),
        tags$p(class = "small",
               "Hover over flows or nodes for details. Coaches are grouped by team order.")
      ),
      mainPanel(
        width = 9,
        tags$div(
          style = "display: flex;
                   justify-content: space-between;
                   font-weight: bold;
                   margin-bottom: -10px;",
          tags$span("2024 Coaches"),
          tags$span("2025 Coaches")
        ),
        sankeyNetworkOutput("sankeyPlot", height = "1200px")
      )
    )
  )
)

#── 4. Server ───────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # No filtering, always show all flows
  flows <- reactive({
    all_flows
  })

  sankeyData <- reactive({
    df <- flows()

    wrap_name <- function(x) str_wrap(x, width = 15)

    # Always keep team order for both sides, all roles
    left_nodes <- long24 %>%
      arrange(team, role) %>%
      mutate(node = paste(team, "(2024):", coach_2024, "-", role)) %>%
      distinct(node, team, coach_2024, role)

    right_nodes <- long25 %>%
      arrange(team, role) %>%
      mutate(node = paste(team, "(2025):", coach_2025, "-", role)) %>%
      distinct(node, team, coach_2025, role)

    all_nodes <- bind_rows(left_nodes, right_nodes) %>%
      distinct(node, .keep_all = TRUE)

    nodes <- data.frame(
      name = wrap_name(all_nodes$node),
      stringsAsFactors = FALSE
    )

    links <- df %>%
      mutate(
        source = match(paste(team, "(2024):", coach_2024, "-", role), all_nodes$node) - 1,
        target = match(paste(team, "(2025):", coach_2025, "-", role), all_nodes$node) - 1
      ) %>%
      select(source, target, value, role)

    list(nodes = nodes, links = links)
  })

  output$sankeyPlot <- renderSankeyNetwork({
    dat <- sankeyData()

    sankeyNetwork(
      Links       = dat$links,
      Nodes       = dat$nodes,
      Source      = "source",
      Target      = "target",
      Value       = "value",
      NodeID      = "name",
      LinkGroup   = "role",
      colourScale = colourScale,
      fontSize    = 10,
      fontFamily  = "Roboto",
      nodeWidth   = 25,
      nodePadding = 20,
      margin      = list(top = 80, right = 80, bottom = 80, left = 80)
    )
  })
}

# launch the app
shinyApp(ui, server)
