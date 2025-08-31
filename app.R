library(shiny)
library(dplyr)
library(ggplot2)

ui <- fluidPage(

    titlePanel("Breakeven Analysis Dashboard"),
    sidebarLayout(
        sidebarPanel(
          numericInput("sales_price","Sales Price (per unit)",value = 20),
          numericInput("variable_cost","Variable Cost (per unit)",value = 10),
          numericInput("fixed_cost","Fixed Cost",value = 5000),
          numericInput("budgeted_sales","Budgeted Sales Volume",value = 800),
          actionButton("update","Update Analysis")
        ),
        mainPanel(
           h3("Breakeven Analysis Results"),
           tableOutput("results"),
           plotOutput("cvp_chart")
        )
    )
)
server <- function(input, output) {
  breakeven_analysis = eventReactive(input$update,{
    sales_price = input$sales_price
    variable_cost = input$variable_cost
    fixed_cost = input$fixed_cost
    budgeted_sales = input$budgeted_sales
    
    breakeven_units = fixed_cost / (sales_price - variable_cost)
    brekeven_revenue = breakeven_units * sales_price
    
    margin_of_safety_units = budgeted_sales - breakeven_units
    margin_of_safety_revenue = margin_of_safety_units * sales_price
    margin_of_safety_percentage = ( margin_of_safety_revenue/ (budgeted_sales * sales_price)) * 100
    
    list(
      breakeven_units = breakeven_units,
      brekeven_revenue = brekeven_revenue,
      margin_of_safety_units = margin_of_safety_units,
      margin_of_safety_revenue = margin_of_safety_revenue,
      margin_of_safety_percentage = margin_of_safety_percentage,
      sales_price = sales_price,
      variable_cost = variable_cost,
      fixed_cost =fixed_cost,
      budgeted_sales = budgeted_sales
    )
  })
  output$results = renderTable({
    results = breakeven_analysis()
    data.frame(
      "Sales_Price" = results$sales_price,
      "variable_Cost" = results$variable_cost,
      "Fixed_Cost" = results$fixed_cost,
      "Budgeted Sales Volume" = results$budgeted_sales,
      "Breakeven Units" = results$breakeven_units,
      "Breakeven Revenue" = results$breakeven_revenue,
      "Margin of Safety Units" = results$margin_of_safety_units,
      "Margin of Safety Revenue" = results$margin_of_safety_revenue,
      "Margin of Safety (%)" = round(results$margin_of_safety_percentage, 2)
    )
  })
  output$cvp_chart = renderPlot({
    results = breakeven_analysis()
    
    sales_volume <- seq(0, max(results$budgeted_sales, results$breakeven_units + 200), by = 10)
    total_revenue <- sales_volume * results$sales_price
    total_cost <- results$fixed_cost + sales_volume * results$variable_cost
    
    ggplot() +
      geom_line(aes(x = sales_volume, y = total_revenue), color = "blue", size = 1.2) +
      geom_line(aes(x = sales_volume, y = total_cost), color = "red", size = 1.2) +
      geom_vline(xintercept = results$breakeven_units, linetype = "dashed", color = "black") +
      geom_vline(xintercept = results$budgeted_sales, linetype = "dashed", color = "green") +
      geom_hline(yintercept = results$fixed_cost, linetype = "dotted", color = "gray") +
      annotate("text", x = results$breakeven_units + 10, y = results$fixed_cost + 500, label = "Breakeven Point", color = "black") +
      annotate("text", x = results$budgeted_sales + 10, y = results$fixed_cost + 500, label = "Budgeted Sales", color = "green") +
      labs(title = "Cost-Volume-Profit (CVP) Chart", x = "Sales Volume (Units)", y = "Cost / Revenue") +
      theme_minimal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
