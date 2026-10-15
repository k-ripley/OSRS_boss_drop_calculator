#############################
# OSRS BOSS DROP CALCULATOR #
#############################

library(shiny)
library(dplyr)
library(readr)
library(tidyr)
library(DT)
library(scales)
library(plotly)


# DATA
######

data_file <- "osrs_boss_loot.csv"

boss_loot_data <- read_csv(
  data_file,
  col_types = c("c", "c", "c")
)

# This is for parsing the rarity variable that is entered exactly as shown
# on the OSRS wiki drop tables.

# It supports current wiki rarity value conventions, but that may change because
# the separate_wider_delim() function is currently in the experimental stage

# Unit Fractions (1/100)
# Proper Fractions (2/100)
# Unit Fractions with decimal denominators (1/181.1)

boss_loot_data <- boss_loot_data %>%
  separate_wider_delim(
    cols = rarity,
    delim = "/",
    names = c("num", "den"),
    cols_remove = FALSE
  ) %>%
  mutate(
    num = as.numeric(num),
    den = as.numeric(den),
    probability = num / den
  )


# UI
####

ui <- fluidPage(
  titlePanel("OSRS Boss Drop Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "boss",
        label = "Select Boss:",
        choices = c(
          "Select a boss..." = "",
          sort(unique(boss_loot_data$boss))
        ),
        selected = ""
      ),
      
      numericInput(
        inputId = "kills",
        label = "Number of kills:",
        value = 0,
        min = 0,
        step = 1
      ),
      
      conditionalPanel(
        condition = "input.boss != ''",
        
        uiOutput("graph_item_selector"),
        
        hr(),
        
        h4("Your Actual Drops"),
        
        p("Enter the number of each unique item you have actually received."),
        
        uiOutput("actual_drop_inputs")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.boss == ''",
        
        h3("Select a boss to get started"),
        
        p("Choose a boss from the dropdown menu to view its unique drops.")
      ),
      
      conditionalPanel(
        condition = "input.boss != ''",
        
        h3(textOutput("boss_title")),
        
        DTOutput("drop_table"),
        
        h3("Probability of Receiving at Least One Drop"),
        
        plotlyOutput("probability_plot", height = "500px")
      ),
      
      div(
        p("*Note: These tables do not include guaranteed drops, such as Gryphon Feathers or Frozen Tears."),
        
        p("**Note: This calculator does not include group bosses whose drop probabilities scale off of player performance, such as Zalcano or Nightmare."),
        
        p("RuneScape is a registered trademark of Jagex Ltd."),
        
        p(
          "Data sourced from the ",
          a(
            "Old School RuneScape Wiki",
            href = "https://oldschool.runescape.wiki/",
            target = "_blank"
          )
        )
      )
    )
  )
)


# SERVER
########

server <- function(input, output, session) {
  
  boss_data <- reactive({
    req(input$boss != "")
    
    boss_loot_data %>%
      filter(boss == input$boss)
  })
  
  
  output$boss_title <- renderText({
    req(input$boss != "")
    
    paste(input$boss, "Unique Drop Results")
  })
  
  
  output$graph_item_selector <- renderUI({
    req(input$boss != "")
    
    data <- boss_data()
    
    selectInput(
      inputId = "graph_item",
      label = "Select which item to graph:",
      choices = data$item,
      selected = data$item[1]
    )
  })
  
  
  output$actual_drop_inputs <- renderUI({
    req(input$boss != "")
    
    data <- boss_data()
    
    tagList(
      lapply(
        seq_len(nrow(data)),
        function(i) {
          numericInput(
            inputId = paste0("actual_", i),
            label = data$item[i],
            value = 0,
            min = 0,
            step = 1
          )
        }
      )
    )
  })
  
  
  actual_drops <- reactive({
    data <- boss_data()
    
    sapply(
      seq_len(nrow(data)),
      function(i) {
        value <- input[[paste0("actual_", i)]]
        
        if (is.null(value) || is.na(value)) {
          return(0)
        }
        
        value
      }
    )
  })
  
  
  output$drop_table <- renderDT({
    data <- boss_data()
    
    kills <- input$kills
    actual <- actual_drops()
    
    expected <- kills * data$probability
    
    exactly <- dbinom(
      actual,
      size = kills,
      prob = data$probability
    )
    
    at_most <- pbinom(
      actual,
      size = kills,
      prob = data$probability
    )
    
    more_than <- 1 - at_most
    
    results <- data.frame(
      Item = data$item,
      `Drop Rate` = data$rarity,
      `Actual Drops` = actual,
      
      `Expected Drops` = format(
        round(expected, 2),
        big.mark = ",",
        nsmall = 2
      ),
      
      `Chance of Exactly Actual Drops` = percent(
        exactly,
        accuracy = 0.001
      ),
      
      `Chance of Actual Drops or Fewer` = percent(
        at_most,
        accuracy = 0.001
      ),
      
      `Chance of More than Actual Drops` = percent(
        more_than,
        accuracy = 0.001
      ),
      # Three decimal places were used to allow for more accuracy when
      # users are going very dry on a drop. Not sure I like how it looks.
      check.names = FALSE
    )
    
    datatable(
      results,
      rownames = FALSE,
      selection = "none",
      options = list(
        paging = FALSE,
        searching = FALSE,
        lengthChange = FALSE,
        info = FALSE,
        scrollX = FALSE,
        autoWidth = FALSE,
        columnDefs = list(
          list(targets = 0, width = "180px"),
          list(targets = 1, width = "90px"),
          list(targets = 2, width = "100px"),
          list(targets = 3, width = "110px"),
          list(targets = 4, width = "145px"),
          list(targets = 5, width = "155px"),
          list(targets = 6, width = "155px")
          # This is specifying how it's displayed. The column widths are present
          # to keep the table from behaving weird when switching bosses. Columns kept
          # breaking when they didn't have specified widths.
        )
      )
    )
  })
  
  
  output$probability_plot <- renderPlotly({
    req(
      input$boss != "",
      input$graph_item
    )
    
    data <- boss_data()
    
    selected_item <- data %>%
      filter(item == input$graph_item)
    
    req(nrow(selected_item) == 1)
    
    p <- selected_item$probability
    current_kills <- input$kills
    
    # This determines the range of the x-axis on the graph to keep the curve and
    # asymptote (which are the points of interest) in view based on the
    # specific drop rate of the item selected and current kill count reported
    if (p >= 0.1) {
      max_kills <- max(100, current_kills * 1.25)
      
    } else if (p >= 0.01) {
      max_kills <- max(500, current_kills * 1.25)
      
    } else if (p >= 0.001) {
      max_kills <- max(5000, current_kills * 1.25)
      
    } else {
      max_kills <- max(25000, current_kills * 1.25)
    }
    
    max_kills <- ceiling(max_kills)
    
    kill_values <- seq(
      0,
      max_kills,
      length.out = 500
    )
    # 500 points is enough to keep the curve smooth without generating
    # unnecessary data and slowing the performance
    
    probabilities <- 1 - (1 - p)^kill_values
    
    graph_data <- data.frame(
      kills = kill_values,
      probability = probabilities
    )
    
    current_probability <- 1 - (1 - p)^current_kills
    
    plot <- plot_ly(
      data = graph_data,
      x = ~kills,
      y = ~probability,
      type = "scatter",
      mode = "lines",
      hovertemplate = paste0(
        "Kills: %{x:,.0f}",
        "<br>",
        "Chance: %{y:.2%}",
        "<extra></extra>"
      )
    )
    
    if (current_kills <= max_kills) {
      
      plot <- plot %>%
        add_segments(
          x = current_kills,
          xend = current_kills,
          y = 0,
          yend = current_probability,
          hoverinfo = "skip",
          inherit = FALSE
        )
      
      plot <- plot %>%
        add_markers(
          x = current_kills,
          y = current_probability,
          hovertemplate = paste0(
            "Your KC: ",
            format(
              current_kills,
              big.mark = ",",
              scientific = FALSE
            ),
            "<br>",
            "Chance: ",
            sprintf(
              "%.2f%%",
              current_probability * 100
            ),
            "<extra></extra>"
          ),
          inherit = FALSE
        )
    }
    
    plot %>%
      layout(
        xaxis = list(
          title = "Number of Kills",
          rangemode = "tozero",
          tickformat = ",d"
        ),
        
        yaxis = list(
          title = "Chance of At Least One Drop",
          tickformat = ".0%",
          range = c(0, 1)
        ),
        
        hovermode = "x unified",
        showlegend = FALSE
      )
  })
}


# RUN IT
########

shinyApp(
  ui = ui,
  server = server
)
