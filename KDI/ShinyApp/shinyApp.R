library(shiny)
library(tidyverse)
library(data.table)

data <- fread("ShinyInput.csv")

ui <- fluidPage(
  titlePanel("VHA Feature Extraction Results Explorer"),
  sidebarLayout(
    sidebarPanel(
      selectInput("domain", "Choose Domain:",
        choices = c("Labs", "Vitals", "Conditions", "Medications", "Procedures")
      ),
      uiOutput("feature_ui"),
      
      conditionalPanel(
        condition = "input.domain == 'Labs'",
        selectInput("plot_type", "Choose Plot Type", choices = c("Lab Name", "Value Categories"))
      ),
      
      selectInput("variable", "Choose Variable to Plot:",
        choices = c(
          "Number of Records" = "n_records",
          "Number of Patients" = "n_patients",
          "Percent of Patients" = "percent_of_patients"
        )
      ),
      conditionalPanel(
        condition = "input.variable != 'percent_of_patients'",
        checkboxInput("log_scale", "Log Scale", value = F)
      ),
      uiOutput("row_selection_ui")
    ),
    mainPanel(
      plotOutput("barplot"),
      DT::dataTableOutput("results_table")
    )
  )
)

server <- function(input, output, session) {
  # Display the list of available features for the chosen domain
  output$feature_ui <- renderUI({
    req(input$domain)
    selected_features <- unique(data %>% filter(domain == input$domain) %>% 
                                  pull(feature_name))
    selected_features <- selected_features[!(selected_features %in% c(
      "DBP", # not categorized, don't display
      "Pain" # Ref not captured, don't display
    ))]
    selectInput("feature", "Choose Feature:", choices = selected_features)
  })

  # Get the data for the chosen feature and sort by the chosen variable
  data_feature <- reactive({
    req(input$domain, input$feature, input$variable)

    data %>%
      filter(domain == input$domain & feature_name == input$feature) %>%
      arrange(desc(get(input$variable)))
  })

  # Choose the variable for the 'fill' color aesthetic 
  fill_var <- reactive({
    req(input$domain)
    case_when(
      input$domain == "Conditions" ~ "code_vocabulary",
      input$domain == "Procedures" ~ "code_vocabulary",
      input$domain == "Labs" ~ "",
      input$domain == "Vitals" ~ "",
      input$domain == "Medications" ~ "VAClass"
    )
  })

  # Get the variable to map to 'y'
  y_var <- reactive({
    req(input$domain)
    case_when(
      input$domain == "Conditions" ~ "code",
      input$domain == "Procedures" ~ "code",
      input$domain == "Labs" & input$plot_type == "Lab Name" ~ "LabChemTestName",
      input$domain == "Labs" & input$plot_type == "Value Categories" ~ "feature_subcategory",
      input$domain == "Vitals" ~ "feature_subcategory",
      input$domain == "Medications" ~ "drug_name"
    )
  })

  data_relevant <- reactive({

    req(data_feature(), y_var())

    if (y_var() == "feature_subcategory") {
      data_feature() %>%
        filter(!(feature_subcategory %in% c("", NA))) %>%
        distinct()
    } else if (y_var() == "LabChemTestName") {
      data_feature() %>% filter(feature_subcategory %in% c("", NA)) %>%
        distinct(LabChemTestName, .keep_all = T)
    } else {
      data_feature() %>% ungroup()
    }
  })

  # Plot max of 20 rows - choose rows to plot
  output$row_selection_ui <- renderUI({
    req(data_relevant())
    n_rows <- nrow(data_relevant())
    if (n_rows > 20) {
      choices <- seq(0, n_rows, by = 20)
      choices <- paste0(choices + 1, "-", c(choices[-1], n_rows))
      selectInput("row_selection", "Show Rows: ", choices = choices)
    }
  })

  # Dataset for plot - select rows
  data_final <- reactive({
    req(data_relevant(), input$row_selection)

    if (nrow(data_relevant()) > 20 && !is.null(input$row_selection)) {
      selected_range <- strsplit(input$row_selection, "-")[[1]]
      start_row <- as.numeric(selected_range[1])
      end_row <- as.numeric(selected_range[2])
      data_use <- data_relevant() %>% 
        filter(row_number() >= start_row & row_number() <= end_row)
    } else {
      data_use <- data_relevant()
    }
    data_use %>% mutate(Description = ifelse(Description == "", NA, Description))
  })

  # Generate the bar plot
  output$barplot <- renderPlot({
    req(data_final())

    # Initialize plot object, then add elements by plot type/domain/etc
    p <- ggplot(data_final(), 
                aes(y = reorder(get(y_var()), get(input$variable)),
                    x = get(input$variable))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = Description, x = 0, hjust = 0), color = "black") + 
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90),
            panel.grid = element_blank()) + # , hjust = 1
      labs(
        y = str_to_title(gsub("ChemTest", " ", gsub("_", " ", y_var()))), 
        x = str_to_title(gsub("_", " ", input$variable)),
        title = input$feature_name
      )


    # Add fill/color and legend, if any
    if (fill_var() != "") {
      # if (nrow(unique(data_plot[fill_var])) > 1) {
      p <- p + aes_string(fill = fill_var()) + theme(
        legend.position = c(0.95, 0.05),
        legend.justification = c("right", "bottom"),
        legend.background = element_rect(fill = alpha('white', 0.9))
      ) + labs(fill = gsub("code_", "", fill_var()))
      # facet_wrap(reformulate(fill_var), scales = "free")
      # }
    } else {
      p <- p + geom_col(fill = "slateblue")
    }

    if (input$log_scale & input$variable != "percent_of_patients") {
      p <- p + scale_x_log10()
    }

    print(p)
  })

  output$results_table <- DT::renderDataTable({
    req(data_final())

    if (input$feature == "Creat_BSP" & input$plot_type == "Value Categories") {
      to_display <- data.frame(Creat_BSP = "See EGFR")
    } else if (nrow(data_final()) == 0) {

      to_display <- data.frame(x = "No data to display")
      names(to_display) <- input$feature
    } else if (!(input$feature == "EGFR" & input$plot_type == "Lab Name")) {
      to_display <- data_final() %>%
        select(feature_subcategory, drug_name, VAClass, code, 
               vocabulary = code_vocabulary, Description, everything()) %>%
        mutate(percent_of_patients = signif(percent_of_patients, 3)) %>%
        select(-c(prop_patients, lower, upper))
      if (nrow(to_display) > 1){
        to_display <- to_display %>% select_if(~ n_distinct(., na.rm = T) > 1)
      } else {
        to_display <- to_display %>% select_if(~ any(!(. %in% c("", NA)))) %>%
          select(-any_of(c("domain", "feature_name", "VAClassification_Description",
                           "VAClassification", "VAClass", "n_cohort")))
      }
      names(to_display) <- gsub("LabChemTestName", "Lab Name", names(to_display))
      names(to_display) <- str_to_title(gsub("_", " ", names(to_display)))
    } else {
      to_display <- data_final() %>% select_if(~ any(!(. %in% c("", NA))))
    }
    if (nrow(to_display) == 0) {

      to_display <- data.frame(x = "No data to display")
      names(to_display) <- input$feature
    }

    DT::datatable(to_display)
  })
}

shinyApp(ui = ui, server = server)
