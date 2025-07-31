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
        tags$h4("Filter by role"),
        selectInput(
          inputId = "role",
          label   = NULL,
          choices = c("All", "head_coach", "offensive_coordinator", "defensive_coordinator"),
          selected = "All"
        ),
        tags$p(class = "small",
               "Hover over flows or nodes for details. Coaches are grouped by team order.")
      ),
      mainPanel(
        width = 9,
        # ← side‐labels for clarity
        tags$div(
          style = "display: flex;
                   justify-content: space-between;
                   font-weight: bold;
                   margin-bottom: -10px;",
          tags$span("2024 Coaches"),
          tags$span("2025 Coaches")
        ),
        sankeyNetworkOutput("sankeyPlot", height = "650px")
      )
    )
  )
)

#── 4. Server ───────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # filter by role if requested
  flows <- reactive({
    if (input$role == "All") {
      all_flows
    } else {
      all_flows %>% filter(role == input$role)
    }
  })

  # build nodes & links with team‐based ordering
  sankeyData <- reactive({
    df <- flows()

    # helper to wrap long names
    wrap_name <- function(x) str_wrap(x, width = 15)

    # get coaches in 2024 sorted by team
    left_coaches <- df %>%
      arrange(team) %>%
      distinct(coach_2024) %>%
      pull(coach_2024)

    # get coaches in 2025 sorted by team
    right_coaches <- df %>%
      arrange(team) %>%
      distinct(coach_2025) %>%
      pull(coach_2025)

    # combine, preserving left-first, then any new right‐side coaches
    all_coaches <- unique(c(left_coaches, right_coaches))

    # create the node list with wrapped labels
    nodes <- data.frame(
      name = wrap_name(all_coaches),
      stringsAsFactors = FALSE
    )

    # build the link table with zero-based indices
    links <- df %>%
      mutate(
        source = match(coach_2024, all_coaches) - 1,
        target = match(coach_2025, all_coaches) - 1
      ) %>%
      select(source, target, value, role)

    list(nodes = nodes, links = links)
  })

  # render the Sankey
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
      fontSize    = 11,
      fontFamily  = "Roboto",
      nodeWidth   = 20,
      nodePadding = 15,
      margin      = list(top = 20, right = 20, bottom = 20, left = 20)
    )
  })
}

# launch the app
shinyApp(ui, server)
