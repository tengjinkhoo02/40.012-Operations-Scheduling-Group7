library(shiny)
library(shinyjs)
library(dplyr)
library(readr)
library(readxl)
library(lubridate)
library(DT)
library(ggplot2)
library(gurobi)

# Function to generate a random color
generate_random_color <- function() {
  paste0("#", sprintf("%06X", sample(0:16777215, 1)))
}

optimize_schedule <- function(data, end_date, M = 10000) {
  # Ensure predecessor is NA where it's empty
  data$Components[is.na(data$Components)] <- ""
  
  # Define the end date
  today <- as.Date("2024-01-01")
  due_date <- as.numeric(difftime(end_date, today, units = "days"))
  
  # Number of operations
  n <- nrow(data)
  
  # Model setup
  model <- list()
  
  # Objective: maximize the sum of end times
  model$obj <- c(rep(0, n), rep(1, n), rep(0, n * n * 2))
  model$modelsense <- "max"
  
  # Variable bounds
  model$lb <- c(rep(0, n * 2), rep(0, n * n * 2))
  model$ub <- c(rep(Inf, n * 2), rep(1, n * n * 2))
  
  # Variable types (continuous for start and end times, binary for precedence)
  model$vtype <- c(rep("C", n * 2), rep("B", n * n * 2))
  
  # Constraint matrix
  A <- matrix(0, nrow = 0, ncol = n * 2 + n * n * 2)
  
  # RHS vector and sense
  rhs <- c()
  sense <- c()
  
  # Constraint: end_time[i] == start_time[i] + processing_time[i]
  for (i in seq_len(n)) {
    row <- rep(0, n * 2 + n * n * 2)
    row[i] <- -1
    row[n + i] <- 1
    A <- rbind(A, row)
    rhs <- c(rhs, data$ProcessingTime[i])
    sense <- c(sense, "=")
  }
  
  # Constraint: end_time[i] <= due_date
  for (i in seq_len(n)) {
    row <- rep(0, n * 2 + n * n * 2)
    row[n + i] <- 1
    A <- rbind(A, row)
    rhs <- c(rhs, due_date)
    sense <- c(sense, "<=")
  }
  
  # Order constraints: start_time[i] >= end_time[j] for operations in the same part in ascending order
  for (part in unique(data$Part)) {
    part_ops <- data %>% filter(Part == part) %>% arrange(Order)
    for (i in seq_len(nrow(part_ops) - 1)) {
      current_op_index <- which(data$Part == part & data$Order == part_ops$Order[i])
      next_op_index <- which(data$Part == part & data$Order == part_ops$Order[i + 1])
      row <- rep(0, n * 2 + n * n * 2)
      row[next_op_index] <- 1
      row[n + current_op_index] <- -1
      A <- rbind(A, row)
      rhs <- c(rhs, 0)
      sense <- c(sense, ">=")
    }
  }
  
  # Components constraints: start_time[i] >= end_time[j] for parts with components
  for (i in seq_len(n)) {
    if (data$Components[i] != "") {
      comps <- unlist(strsplit(data$Components[i], ","))
      for (comp in comps) {
        comp_ops <- data %>% filter(Part == comp)
        comp_last_op <- comp_ops %>% filter(Order == max(Order))
        row <- rep(0, n * 2 + n * n * 2)
        row[i] <- 1
        row[n + which(data$Part == comp_last_op$Part & data$Order == comp_last_op$Order)] <- -1
        A <- rbind(A, row)
        rhs <- c(rhs, 0)
        sense <- c(sense, ">=")
      }
    }
  }
  
  # Big-M constraints for unidirectional precedence on the same workcenter using binary variables
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (data$Workcenter[i] == data$Workcenter[j] && i != j) {
        delta_ij <- n * 2 + (i - 1) * n + j
        delta_ji <- n * 2 + (j - 1) * n + i
        
        # Ensure delta_ij + delta_ji = 1
        row <- rep(0, n * 2 + n * n * 2)
        row[delta_ij] <- 1
        row[delta_ji] <- 1
        A <- rbind(A, row)
        rhs <- c(rhs, 1)
        sense <- c(sense, "=")
        
        # Constraint: start_time[i] - end_time[j] >= M(delta_ij - 1)
        row <- rep(0, n * 2 + n * n * 2)
        row[i] <- 1
        row[n + j] <- -1
        row[delta_ij] <- -M
        A <- rbind(A, row)
        rhs <- c(rhs, -M)
        sense <- c(sense, ">=")
      }
    }
  }
  
  # Add constraints to the model
  model$A <- A
  model$rhs <- rhs
  model$sense <- sense
  
  # Solve the model with detailed output
  result <- gurobi(model, params = list(OutputFlag = 1))
  
  # Debugging: check the Gurobi result
  if (is.null(result$x)) {
    stop("Gurobi did not return a valid solution. Please check the model setup.")
  }
  
  # Extract and display the results
  start_times <- result$x[1:n]
  end_times <- result$x[(n + 1):(2 * n)]
  
  # Create the schedule dataframe
  schedule <- data.frame(
    Part = data$Part,
    Operation = data$Operation,
    StartTime = start_times,
    EndTime = end_times,
    Workcenter = factor(data$Workcenter) # Ensure Workcenter is treated as a factor
  )
  
  # Find the maximum end time to calculate the schedule start date
  makespan <- max(schedule$EndTime)
  schedule_start_date <- as.Date(end_date) - makespan
  
  # Convert start and end times to dates
  schedule$StartDate <- as.Date(schedule_start_date + schedule$StartTime)
  schedule$EndDate <- as.Date(schedule$StartDate + (schedule$EndTime - schedule$StartTime))
  
  # Format dates to DD/MM/YYYY
  schedule$StartDate <- format(schedule$StartDate, "%d/%m/%Y")
  schedule$EndDate <- format(schedule$EndDate, "%d/%m/%Y")
  
  return(schedule)
}

# Function to generate Gantt chart
generate_gantt_chart <- function(schedule, schedule_name) {
  # Create unique shades of blue for each workcenter
  workcenters <- unique(schedule$Workcenter)
  colors <- colorRampPalette(c("#6baed6", "#08519c"))(length(workcenters))
  workcenter_colors <- setNames(colors, workcenters)
  
  # Create a combined label for y-axis
  schedule$PartOperation <- paste(schedule$Part, schedule$Operation, sep = " - ")
  
  # Generate the Gantt chart
  ggplot(schedule, aes(x = as.Date(StartDate, format = "%d/%m/%Y"), 
                       xend = as.Date(EndDate, format = "%d/%m/%Y"), 
                       y = reorder(PartOperation, as.Date(StartDate, format = "%d/%m/%Y")), 
                       yend = PartOperation, color = Workcenter)) +
    geom_segment(size = 6) +
    geom_text(aes(label = StartDate, x = as.Date(StartDate, format = "%d/%m/%Y")), vjust = -1.5, size = 3) +
    geom_text(aes(label = EndDate, x = as.Date(EndDate, format = "%d/%m/%Y")), vjust = -1.5, size = 3) +
    theme_minimal() +
    labs(title = paste("Gantt Chart:", schedule_name), x = "Date", y = "Operations", color = "Workcenter") +
    scale_color_manual(values = workcenter_colors) +
    theme(axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 0, hjust = 0.5)) +
    coord_cartesian(clip = 'off') # Prevent cropping of labels
}

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "animate.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "icon_styles.css"),
    tags$script(HTML("
      $(document).ready(function() {
        var words = ['a schedule', 'an optimized system', 'greater productivity'];
        var i = 0;
        setInterval(function() {
          $('#rotate').fadeOut(function() {
            $(this).text(words[i = (i + 1) % words.length]).fadeIn();
          });
        }, 2000);

        // Slider functionality
        var $navLinks = $('.navbar-nav .nav-item');
        $navLinks.on('click', function() {
          $navLinks.removeClass('active');
          $(this).addClass('active');
        });

        // Info icon tooltips
        $('.info-icon[data-toggle=\"tooltip\"]').tooltip();
      });
    "))
  ),
  navbarPage(
    title = div(class = "navbar-title", "Operations Scheduling"),
    id = "navbar",
    tabPanel("Home",
             div(
               class = "main-content concept",
               div(
                 class = "content-wrapper",
                 div(
                   class = "left-content",
                   div(class = "welcome-container",
                       h1("Welcome,", class = "welcome-text slide-in-left"),
                       img(src = "welcome.png", class = "welcome-icon")
                   ),
                   div(
                     class = "rotating-text-wrapper",
                     h2("Let’s get you ", class = "subtitle-text slide-in-left"),
                     h2(span(id = "rotate", class = "rotating-text", "a schedule"), class = "subtitle-text slide-in-left")
                   ),
                   div(class = "button-container",
                       actionButton("go_to_schedule_btn", "Make Schedule", class = "button-63 large-button")
                   )
                 )
               ),
               img(src = "hourglass.png", class = "hourglass-animation large-hourglass")
             )
    ),
    tabPanel("Getting Started",
             fluidPage(
               h1("About Our System", class = "about-title"),
               div(
                 class = "section",
                 div(
                   class = "section-title-wrapper",
                   img(src = "gettingstarted.png", class = "getting-started-icon"),
                   h2("How the Operation Scheduling Works", class = "section-title")
                 ),
                 p("This section explains the workings of the operation scheduling system in detail. Here, the processes and methodologies used in the system are covered. The system involves inputting data either by uploading a file or by manually inputting the routings for each part of the product. These inputs are then processed by the Gurobi backend to generate an optimized operation schedule with the objective of delaying the start date of the operation.", class = "section-content"),
                 div(class = "flowchart-container",
                     img(src = "flowchart.png", class = "flowchart-image pop-up-animation small-image-left"),
                     div(class = "qr-container",
                         img(src = "QR.png", class = "qr-image"),
                         p("Scan the QR code or click on this link to view a tutorial guide on the use of our interface", class = "scan-me-text"),
                         tags$ul(class = "example-2",
                                 tags$li(class = "icon-content",
                                         tags$a(href = "https://youtu.be/wrKvAAuy9XQ", target = "_blank", `data-social` = "youtube",
                                                tags$svg(
                                                  xmlns = "http://www.w3.org/2000/svg",
                                                  viewBox = "0 0 24 24",
                                                  tags$path(d = "M19.615 3.184c-4.712-1.25-14.518-1.25-19.229 0-4.377 1.163-4.377 11.638 0 12.8 4.711 1.25 14.518 1.25 19.229 0 4.378-1.163 4.378-11.637 0-12.8zm-10.615 9.316v-6l6 3-6 3z")
                                                ),
                                                div(class = "filled")
                                         ),
                                         div(class = "tooltip", "YouTube")
                                 )
                         )
                     )
                 )
               )
             )
    ),
    tabPanel("Create Schedule", value = "create_schedule",
             fluidPage(
               h2("Schedule Input", class = "schedule-input-title"),
               img(src = "schedulebutton.png", class = "schedule-button-icon"),
               fluidRow(
                 column(6,
                        textInput("schedule_name", "Schedule Name:", placeholder = "Enter schedule name")
                 ),
                 column(6,
                        div(class = "button-container",
                            actionButton("reset_btn", "Refresh", class = "button")
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        div(class = "field-label-container",
                            numericInput("total_quantity", "Quantity Required for Finished Product:", value = 1, min = 1),
                            img(src = "info.png", class = "info-icon", title = "Input the total quantity of the finished product needed", `data-toggle` = "tooltip")
                        )
                 ),
                 column(6,
                        div(class = "field-label-container",
                            dateInput("end_date", "Due Date", format = "yyyy-mm-dd", width = "100%"),
                            img(src = "info.png", class = "info-icon", title = "Input the latest due date for the entire operating system", `data-toggle` = "tooltip")
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        h3("Input the Bill-of-Materials"),
                        div(class = "upload-container",
                            fileInput("file1", "Upload File to Autofill (optional)", accept = c(".xlsx", ".csv")),
                            tags$a(href = "fileformat.png", download = "fileformat.png",
                                   img(src = "fileicon.png", class = "file-icon", title = "Press the file graphic icon to download a sample file of the format. Ensure that the column number corresponds with the nature of the data. for e.g. column 1 should be part name etc", `data-toggle` = "tooltip")
                            ),
                            img(src = "info.png", class = "info-icon", title = "Press the file graphic icon to download a sample file of the format. Ensure that the column number corresponds with the nature of the data. for e.g. column 1 should be part name etc", `data-toggle` = "tooltip")
                        ),
                        tableOutput("file_info")
                 )
               ),
               fluidRow(
                 column(12,
                        h3("Manual Input"),
                        uiOutput("dynamic_fields"),
                        div(class = "button-container-inline",
                            actionButton("add_field", "Add More", class = "btn btn-primary small-btn"),
                            actionButton("preview_bom_btn", "Preview BOM", class = "btn btn-primary small-btn"),
                            actionButton("make_schedule_btn", span("Make Schedule"), class = "btn btn-primary small-btn")
                        )
                 )
               ),
               hidden(
                 div(id = "schedule_result_section",
                     fluidRow(
                       column(12,
                              h2("Schedule Result", class = "result-title"),
                              tableOutput("schedule"),
                              actionButton("generate_gantt_btn", "Generate Gantt Chart", class = "btn btn-primary small-btn"),
                              plotOutput("gantt_chart_plot", height = "800px"),
                              div(class = "button-container",
                                  downloadButton("download_gantt_btn", "Download Gantt Chart", class = "btn gradient-button")
                              ),
                              verbatimTextOutput("status")
                       )
                     )
                 )
               )
             )
    ),
    tabPanel("History",
             fluidPage(
               h2("History", class = "history-title"),
               dataTableOutput("history_table"),
               actionButton("view_schedule_btn", "View Schedule", class = "btn btn-primary small-btn")
             )
    ),
    tabPanel("About the Team",
             fluidPage(
               div(class = "about-title-container",
                   img(src = "aboutus.png", class = "about-icon"),
                   h1("About the Team", class = "about-title")
               ),
               p("Hello! We are Group 7, and we are excited to introduce our operation scheduling interface. Meet the team below!", class = "section-content team-description"),
               div(class = "team-member",
                   img(src = "phoebe.png", class = "team-photo fade-in-short"),
                   p(class = "team-name", "Phoebe")
               ),
               div(class = "team-member",
                   img(src = "luvena.png", class = "team-photo fade-in-short"),
                   p(class = "team-name", "Luvena")
               ),
               div(class = "team-member",
                   img(src = "ernest.png", class = "team-photo fade-in-short"),
                   p(class = "team-name", "Ernest")
               ),
               div(class = "team-member",
                   img(src = "kinyong.png", class = "team-photo fade-in-short"),
                   p(class = "team-name", "Kin Yong")
               ),
               div(class = "team-member",
                   img(src = "tengjin.png", class = "team-photo fade-in-short"),
                   p(class = "team-name", "Teng Jin")
               ),
               div(class = "team-member",
                   img(src = "asher.png", class = "team-photo fade-in-short"),
                   p(class = "team-name", "Asher")
               ),
               div(class = "team-member",
                   img(src = "julianna.png", class = "team-photo fade-in-short"),
                   p(class = "team-name", "Julianna")
               )
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store the number of input fields and color fields
  rv <- reactiveValues(
    fields = list(list(
      part = NULL, operation = NULL, order = NULL, components = NULL,
      processing_time = NULL, workcenter = NULL
    )),
    internal_data = NULL,
    colors = list(generate_random_color()),
    schedule = NULL,
    history = data.frame(ScheduleName = character(), Date = character(), stringsAsFactors = FALSE)
  )
  
  # Function to get the current inputs
  get_current_inputs <- function() {
    lapply(seq_along(rv$fields), function(i) {
      list(
        part = input[[paste0("part_", i)]],
        operation = input[[paste0("operation_", i)]],
        order = input[[paste0("order_", i)]],
        components = input[[paste0("components_", i)]],
        processing_time = input[[paste0("processing_time_", i)]],
        workcenter = input[[paste0("workcenter_", i)]]
      )
    })
  }
  
  # Observe to update the input method UI
  output$dynamic_fields <- renderUI({
    fields <- lapply(seq_along(rv$fields), function(i) {
      fluidRow(
        column(2, textInput(paste0("part_", i), "Part Name", placeholder = "Part Name", value = rv$fields[[i]]$part)),
        column(2, textInput(paste0("operation_", i), "Operation", placeholder = "Operation", value = rv$fields[[i]]$operation)),
        column(1, textInput(paste0("order_", i), "Order", placeholder = "Order", value = rv$fields[[i]]$order)),
        column(2, textInput(paste0("components_", i), "Components", placeholder = "Components", value = rv$fields[[i]]$components)),
        column(2, numericInput(paste0("processing_time_", i), "Processing Time (days)", value = rv$fields[[i]]$processing_time, min = 0)),
        column(1, textInput(paste0("workcenter_", i), "Workcenter", placeholder = "Workcenter", value = rv$fields[[i]]$workcenter)),
        column(1, div(style = "margin-top: 25px;", actionButton(paste0("remove_row_", i), label = icon("trash-alt"), class = "btn btn-danger small-btn", onclick = sprintf('Shiny.setInputValue("delete_row", %d, {priority: "event"})', i))))
      )
    })
    do.call(tagList, fields)
  })
  
  # Add more fields
  observeEvent(input$add_field, {
    rv$fields <- get_current_inputs()
    rv$fields <- append(rv$fields, list(list(
      part = NULL, operation = NULL, order = NULL, components = NULL,
      processing_time = NULL, workcenter = NULL
    )))
    rv$colors <- append(rv$colors, list(generate_random_color()))
  })
  
  # Remove a specific row
  observeEvent(input$delete_row, {
    rv$fields <- get_current_inputs()
    if (length(rv$fields) > 1) {
      rv$fields <- rv$fields[-input$delete_row]
      rv$colors <- rv$colors[-input$delete_row]
    }
  })
  
  # Handle the Excel or CSV file upload
  observeEvent(input$file1, {
    req(input$file1)
    tryCatch({
      if (grepl("\\.csv$", input$file1$name)) {
        data <- read.csv(input$file1$datapath)
      } else {
        data <- read_excel(input$file1$datapath)
      }
      
      # Rename columns to match expected format
      colnames(data) <- c("Part", "Operation", "Order", "Components", "ProcessingTime", "Workcenter")
      
      # Validate the uploaded file based on column number
      if (ncol(data) < 6) {
        output$file_error <- renderText("The file must contain at least 6 columns.")
        return(NULL)
      } else {
        output$file_error <- renderText("")
        rv$fields <- lapply(1:nrow(data), function(i) {
          list(
            part = data[i, 1], operation = data[i, 2], order = data[i, 3],
            components = data[i, 4], processing_time = as.numeric(data[i, 5]), workcenter = data[i, 6]
          )
        })
        rv$colors <- lapply(1:nrow(data), function(i) generate_random_color())
        
        output$dynamic_fields <- renderUI({
          fields <- lapply(seq_along(rv$fields), function(i) {
            fluidRow(
              column(2, textInput(paste0("part_", i), "Part Name", placeholder = "Part Name", value = rv$fields[[i]]$part)),
              column(2, textInput(paste0("operation_", i), "Operation", placeholder = "Operation", value = rv$fields[[i]]$operation)),
              column(1, textInput(paste0("order_", i), "Order", placeholder = "Order", value = rv$fields[[i]]$order)),
              column(2, textInput(paste0("components_", i), "Components", placeholder = "Components", value = rv$fields[[i]]$components)),
              column(2, numericInput(paste0("processing_time_", i), "Processing Time (days)", value = rv$fields[[i]]$processing_time, min = 0)),
              column(1, textInput(paste0("workcenter_", i), "Workcenter", placeholder = "Workcenter", value = rv$fields[[i]]$workcenter)),
              column(1, div(style = "margin-top: 25px;", actionButton(paste0("remove_row_", i), label = icon("trash-alt"), class = "btn btn-danger small-btn", onclick = sprintf('Shiny.setInputValue("delete_row", %d, {priority: "event"})', i))))
            )
          })
          do.call(tagList, fields)
        })
      }
    }, error = function(e) {
      shinyjs::alert("Error reading the file. Please check the format and try again.")
    })
  })
  
  # Make schedule
  observeEvent(input$make_schedule_btn, {
    showModal(modalDialog(
      title = "Please wait...",
      div(style = "text-align: center;", img(src = "car.png", height = "100px"), h4("Loading...")),
      footer = NULL,
      easyClose = FALSE
    ))
    
    req(rv$fields)
    data <- do.call(rbind, lapply(rv$fields, as.data.frame, stringsAsFactors = FALSE))
    
    # Convert input data to appropriate column names
    colnames(data) <- c("Part", "Operation", "Order", "Components", "ProcessingTime", "Workcenter")
    
    # Get the due date from user input
    end_date <- input$end_date
    
    # Check for empty fields
    if (any(data$Part == "") | any(data$Operation == "") | any(data$Order == "") | any(is.na(data$ProcessingTime)) | any(data$ProcessingTime < 0) | any(data$Workcenter == "") | is.null(input$schedule_name) | is.null(input$end_date)) {
      removeModal()
      showModal(modalDialog(
        title = "Error",
        div(style = "text-align: center;", img(src = "error.png", height = "100px"), h4("Error: Check your field input again")),
        footer = modalButton("Close"),
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    # Call the optimize_schedule function
    tryCatch({
      rv$schedule <- optimize_schedule(data, end_date)
      
      # Update history
      rv$history <- rbind(rv$history, data.frame(ScheduleName = input$schedule_name, Date = Sys.Date(), stringsAsFactors = FALSE))
      
      output$schedule <- renderTable({
        rv$schedule %>% select(Part, Operation, Workcenter, StartDate, EndDate) %>% arrange(as.Date(StartDate, format = "%d/%m/%Y"))
      })
      
      output$status <- renderText({
        "Schedule created successfully!"
      })
      
      shinyjs::show(id = "schedule_result_section")
      removeModal()
    }, error = function(e) {
      removeModal()
      shinyjs::alert(paste("Error in optimization:", e$message))
    })
  })
  
  # Render Gantt chart based on the generated table
  observeEvent(input$generate_gantt_btn, {
    req(rv$schedule)
    
    output$gantt_chart_plot <- renderPlot({
      generate_gantt_chart(rv$schedule, input$schedule_name)
    })
  })
  
  # Download Gantt chart
  output$download_gantt_btn <- downloadHandler(
    filename = function() {
      paste("gantt_chart", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      g <- generate_gantt_chart(rv$schedule, input$schedule_name)
      ggsave(file, plot = g, device = "png")
    }
  )
  
  # Reset fields
  observeEvent(input$reset_btn, {
    rv$fields <- list(list(
      part = NULL, operation = NULL, order = NULL, components = NULL,
      processing_time = NULL, workcenter = NULL
    ))
    rv$colors <- list(generate_random_color())
    rv$schedule <- NULL
    updateTextInput(session, "schedule_name", value = "")
    updateNumericInput(session, "total_quantity", value = 1)
    updateDateInput(session, "end_date", value = NULL)
    shinyjs::reset("file1")
    shinyjs::hide(id = "schedule_result_section")
    output$dynamic_fields <- renderUI({
      fields <- lapply(seq_along(rv$fields), function(i) {
        fluidRow(
          column(2, textInput(paste0("part_", i), "Part Name", placeholder = "Part Name", value = rv$fields[[i]]$part)),
          column(2, textInput(paste0("operation_", i), "Operation", placeholder = "Operation", value = rv$fields[[i]]$operation)),
          column(1, textInput(paste0("order_", i), "Order", placeholder = "Order", value = rv$fields[[i]]$order)),
          column(2, textInput(paste0("components_", i), "Components", placeholder = "Components", value = rv$fields[[i]]$components)),
          column(2, numericInput(paste0("processing_time_", i), "Processing Time (days)", value = rv$fields[[i]]$processing_time, min = 0)),
          column(1, textInput(paste0("workcenter_", i), "Workcenter", placeholder = "Workcenter", value = rv$fields[[i]]$workcenter)),
          column(1, div(style = "margin-top: 25px;", actionButton(paste0("remove_row_", i), label = icon("trash-alt"), class = "btn btn-danger small-btn", onclick = sprintf('Shiny.setInputValue("delete_row", %d, {priority: "event"})', i))))
        )
      })
      do.call(tagList, fields)
    })
  })
  
  # Preview BOM
  observeEvent(input$preview_bom_btn, {
    # Gather manual inputs
    rv$fields <- get_current_inputs()
    manual_data <- do.call(rbind, lapply(seq_along(rv$fields), function(i) {
      data.frame(
        Part = input[[paste0("part_", i)]],
        Operation = input[[paste0("operation_", i)]],
        Order = input[[paste0("order_", i)]],
        Components = input[[paste0("components_", i)]],
        ProcessingTime = input[[paste0("processing_time_", i)]],
        Workcenter = input[[paste0("workcenter_", i)]],
        stringsAsFactors = FALSE
      )
    }))
    rv$internal_data <- manual_data
    
    showModal(modalDialog(
      title = "Preview BOM",
      DT::dataTableOutput("bom_preview"),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l" # This makes the modal large
    ))
    
    output$bom_preview <- DT::renderDataTable({
      rv$internal_data
    }, options = list(
      pageLength = 25, # Number of rows to show per page
      scrollX = TRUE # Enable horizontal scrolling
    ))
  })
  
  # Navigation
  observeEvent(input$go_to_schedule_btn, {
    updateTabsetPanel(session, "navbar", selected = "create_schedule")
  })
  
  # History
  output$history_table <- renderDataTable({
    rv$history
  })
  
  # View schedule from history
  observeEvent(input$view_schedule_btn, {
    selected_schedule <- input$history_table_rows_selected
    if (length(selected_schedule) > 0) {
      schedule_name <- rv$history$ScheduleName[selected_schedule]
      showModal(modalDialog(
        title = paste("Schedule:", schedule_name),
        DT::dataTableOutput("history_schedule"),
        div(class = "button-container",
            downloadButton("download_gantt_history_btn", "Download Gantt Chart", class = "btn gradient-button")
        ),
        easyClose = TRUE,
        footer = modalButton("Close"),
        size = "l"
      ))
      output$history_schedule <- DT::renderDataTable({
        rv$schedule %>% select(Part, Operation, Workcenter, StartDate, EndDate) %>% arrange(as.Date(StartDate, format = "%d/%m/%Y"))
      }, options = list(
        pageLength = 25,
        scrollX = TRUE
      ))
    } else {
      shinyjs::alert("Please select a schedule to view.")
    }
  })
  
  # Download Gantt chart from history
  output$download_gantt_history_btn <- downloadHandler(
    filename = function() {
      paste("gantt_chart_history", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      selected_schedule <- input$history_table_rows_selected
      if (length(selected_schedule) > 0) {
        schedule_name <- rv$history$ScheduleName[selected_schedule]
        g <- generate_gantt_chart(rv$schedule, schedule_name)
        ggsave(file, plot = g, device = "png")
      }
    }
  )
  
  # Information table for file format
  output$file_info <- renderTable({
    data.frame(
      Column = c("Column 1", "Column 2", "Column 3", "Column 4", "Column 5", "Column 6"),
      Description = c("Part Name", "Operation", "Order", "Components", "Processing Time (Days)", "Workcenter")
    )
  }, align = "c")
}

shinyApp(ui = ui, server = server)
