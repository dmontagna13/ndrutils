library(shiny)
library(shinyWidgets)
#library(shinyjqui)
library(sortable)
library(tidyverse)
library(ggplot2)
library(ggprism)
library(readxl)
library(writexl)
library(rvest)
library(xslt)
library(drc)
library(jsonlite)
library(tools)
library(rsvg)
# --------------------------------- External sources

source("qpcr_module_r1.R")

# --------------------------------- Sanitizer function

#string_scrubber <- function(din){
#  
#  cleaned_string <- gsub("[^a-zA-Z0-9]", "_", din)
#  return(cleaned_string)
#  }

string_scrubber <- function(input) {
  # Check if the input is a dataframe
  if (is.data.frame(input)) {
    # Apply sanitize_input to each character column in the dataframe
    input[] <- lapply(input, function(x) {
      if (is.character(x)) {
        return(string_scrubber(x))  # Recursively apply sanitize_input to character columns
      } else {
        return(x)  # Leave non-character columns unchanged
      }
    })
  } else if (is.character(input)) {
    # If input is a string, apply the original sanitize_input function to it
    input <- gsub("[^a-zA-Z0-9\\s\\-\\.]", "_", input)
  }
  return(input)
}

# --------------------------------- Load XML file (d360 printmap)

parse_printmap <- function(file){
  
  printmap <- read_xml(file)
  ns <- xml_ns(printmap)
  
  
  # Find the "Tabular" worksheet
  tabular_worksheet <- xml_find_first(printmap, ".//ss:Worksheet[@ss:Name='Tabular']", ns)
  
  if (is.na(tabular_worksheet)) {
    stop("Tabular worksheet not found in the XML file.")
  }
  
  # Extract the Table node
  table_node <- xml_find_first(tabular_worksheet, ".//ss:Table", ns)
  
  if (is.na(table_node)) {
    stop("Table node not found in the Tabular worksheet.")
  }
  
  # Extract rows
  rows <- xml_find_all(table_node, ".//ss:Row", ns)
  
  if (length(rows) == 0) {
    stop("No rows found in the Tabular worksheet.")
  }
  
  # Process each row while accounting for ss:Index
  extract_row_data <- function(row) {
    cell_nodes <- xml_find_all(row, ".//ss:Cell", ns)
    
    row_data <- character()  # Initialize an empty row
    col_index <- 1  # Start at column 1
    
    for (cell in cell_nodes) {
      index_attr <- xml_attr(cell, "ss:Index", ns)
      
      if (!is.na(index_attr)) {
        col_index <- as.integer(index_attr)  # Move to explicitly defined column index
      }
      
      # Extract data content
      data_node <- xml_find_first(cell, ".//ss:Data", ns)
      data_value <- if (!is.na(data_node)) xml_text(data_node) else NA
      
      # Ensure row_data has space up to the required index
      length(row_data) <- max(length(row_data), col_index)
      row_data[col_index] <- data_value  # Assign value at correct column index
      
      col_index <- col_index + 1  # Move to the next column position
    }
    
    return(row_data)
  }
  
  # Extract all rows into a list
  data_list <- lapply(rows, extract_row_data)
  
  # Determine max number of columns
  max_cols <- max(sapply(data_list, length))
  
  # Convert list to a dataframe, filling in empty spaces
  df <- do.call(rbind, lapply(data_list, function(x) c(x, rep(NA, max_cols - length(x))))) %>%
    as.data.frame(stringsAsFactors = FALSE)
  
  # Set column names using the first row
  colnames(df) <- df[1, ]
  df <- df[-1, , drop = FALSE]
  
  # Clean column names
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub(" ", ".", colnames(df))
  colnames(df) <- gsub("\n", ".", colnames(df))
  colnames(df) <- gsub("%", "percent", colnames(df))
  

printmap.df <- df
colnames(printmap.df)
printmap.df2 <- printmap.df %>%
  dplyr::select(plate, dispensed.well, well.contents, single.fluid.concentration, dmso.percent) %>%
  mutate(compound = case_when(
    well.contents == "0" ~ "DMSO",                          # If value is "0", set to "DMSO"
    grepl("^[0-9]+$", well.contents) ~ paste0("BIO-", well.contents),  # If only numbers, add "BIO-"
    TRUE ~ well.contents                                    # Otherwise, keep original value
  )) %>%
  dplyr::mutate(concentration = as.numeric(single.fluid.concentration)) %>%
  dplyr::select(!c(single.fluid.concentration, well.contents)) %>%
  dplyr::mutate(concentration = ifelse(concentration == "0", NA, round(concentration, 2)))

return(printmap.df2)

}


# -------------------------- Annotate data

generate_dataframe <- function(plate, readout, filename) {
  # Your function logic where the plate, readout, and filename values are used
  # Example: Create a dataframe based on these inputs
  df <- data.frame(
    plate = plate,
    readout = readout,
    filename = filename,
    value = sample(1:100, 1)  # Just an example of adding a value column
  )
  return(df)
}

# ------------------------- .eds file sample annotator (initial)


process_eds_files <- function(e){
  
temp_dir <- tempdir()

#files.in.eds <- unzip(eds.file, list = TRUE)
files.in.eds <- unzip(e, list = TRUE)
unzip(e, files = "setup/plate_setup.json", exdir = temp_dir)

plate.setup <- fromJSON(file.path(temp_dir, "setup/plate_setup.json"))

if (is.character(plate.setup$samples$name) || length(plate.setup$samples$name) == 1) {
  flag <- "PASS"
} else {
  flag <- "FLAG. More than one sample per plate"
}

plate.attributes.df <- data.frame(
  file.name = e,
  plate.barcode = plate.setup$plateBarcode,
  sample.name = plate.setup$samples$name,
  data.type = "QuantStudio qPCR Data (.eds)",
  file.type = "eds",
  flag = flag,
  plate.assignment = NA
)
print(plate.attributes.df)
return(plate.attributes.df)
  
}

# --------------------- .xls/x file sample annotator (initial)
process_xl_files <- function(xl){
  
  file_path <- xl
  sheets <- excel_sheets(file_path)
  
  sl <- list()
  for(s in sheets){
    sl[[s]] <- data.frame(
      file.name = file_path,
      plate.barcode = NA,
      sample.name = s,
      data.type = "Synergy H1 data",
      file.type = file_ext(file_path),
      flag = "PASS",
      plate.assignment = NA
    )
  }
  
  df <- do.call(rbind, sl)
  
  return(df)
  }

# ---------------- .zip file sample annotator (initial)
process_zip_files <- function(z, nme) {
  
  temp_dir <- tempdir()
  target_extension <- "\\.fsa$" 
  
  zip_contents <- unzip(z, list = TRUE)$Name
  
  files_to_extract <- zip_contents[grepl(target_extension, zip_contents, ignore.case = TRUE)]
  
  num.files <- length(files_to_extract)
  
  if(num.files > 0){
    if(num.files <= 96){
      data.type <- paste0("Fragment analysis data (", num.files, "x .fsa files)")
      file.type <- "fsa"
      flag = "PASS"
    }else{
      data.type <- "Fragment analysis data (> 96 .fsa files)"
      file.type <- "fsa"
      flag = "FLAG: DOES NOT REPRESENT 1x 96-WELL PLATE"
    }
  }else{
    data.type <- "Unrecognized format"
    file.type <- "ERROR"
    flag = "FLAG: FILES NOT SUPPORTED"
  }
  
  df <- data.frame(
    file.name = z,
    plate.barcode = NA,
    sample.name = tools::file_path_sans_ext(nme),
    data.type = data.type,
    file.type = file.type,
    flag = flag,
    plate.assignment = NA
  )
return(df)
}

# -------------------------- Global vars

exp.id.type.list <- c("ELE #", "Other")



# --------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .tab-animate {
        animation: flash 0.25s linear 6 alternate;
      }
      
      @keyframes flash {
        0% { background-color: white; 
        color: black; }
        25% { background-color: #4C8719; 
        color: white; }
        50% { background-color: #19871D; 
        color: white; }
        75% {background-color: #198754; 
        color: white; }
        100% { background-color: #198387; 
        color: white; }
      }
    ")),
#    tags$script(HTML("
#      Shiny.addCustomMessageHandler('animateTab', function(tabId) {
#        var tab = document.querySelector('a[data-value=\"' + tabId + '\"]');
#        if (tab) {
#        
#          tab.classList.add('tab-animate');
#          setTimeout(function() { tab.classList.remove('tab-animate'); }, 3000);
#        }
#      });
 #   "))
    tags$script(HTML("
Shiny.addCustomMessageHandler('animateTab', function(tabId) {
  var tab = document.querySelector('a[data-value=\"' + tabId + '\"]');
  var tabListItem = tab ? tab.parentElement : null;  // Get the <li> parent element
  
  if (tab) {
    // If the tab has the 'disabled' class on the <li> parent, remove it
    if (tabListItem && tabListItem.classList.contains('disabled')) {
      tabListItem.classList.remove('disabled');  // Enable the tab by removing the 'disabled' class
    }
    
    // Add the animation class to the <a> element (for visual effect)
    tab.classList.add('tab-animate');
    
    // Remove the animation class after 3 seconds to allow the animation to complete
    setTimeout(function() {
      tab.classList.remove('tab-animate');
    }, 3000);
  }
});

    
     "))   
  ),
  titlePanel("HD/ApoE Group CRC Exporter"),
  
  # Welcome Screen
  uiOutput("welcome_screen"),
  
  # Conditional Panel for "Analyze New Data" Workflow
  conditionalPanel(
    condition = "output.selected_workflow == 'Analyze New Data'",
    tabsetPanel(id = "workflow_tabs_analyze", selected = "1: Experiment Information",
                tabPanel("Home", value = "back_to_welcome", ""),
                tabPanel("1: Experiment Information",
                         "Content for Step 1 (Analyze New Data)",
                         textInput("exp.id", "Experiment ID"),
                         radioButtons("exp.id.type", "Experiment ID type", exp.id.type.list),
                         dateInput("exp.date", "Experiment Date"),
                         "Files:",
                         fileInput("sop.file", NULL, buttonLabel = "Add SOP documentation (optional) ...", multiple = FALSE),
                         fileInput("exp.design", NULL, buttonLabel = "Other documents (optional) ..."),
                         textAreaInput("exp.notes", "Comments (optional)", rows = 3),
                         # Placeholder for the "Next" button that will be conditionally rendered
                         uiOutput("next_button_ui")),  # Placeholder for next button UI),
                tabPanel(
                  "2: Condition Mapping",
                  value = "nav2",
                  h4("Select the XML report generated by the HP d300 digital dispenser"),
                  fileInput("xml.report.file", NULL, buttonLabel = "Select XML Report ...", accept = ".XML", multiple = FALSE),
                  uiOutput("plate_selector"),  # Dynamically generated plate selector
                  uiOutput("treatment_selector"),
                  uiOutput("next_button_ui2"),
                  #uiOutput("tab2contents")
                ),
                tags$script(
                  '
                  var tab = $(\'a[data-value="nav2"]\').parent().addClass("disabled");
                  $(function(){
                  $(tab.parent()).on("click", "li.disabled", function(e) {
                  e.preventDefault();
                  return false;
                  });
                  });
                  '
                ),
                tabPanel(
                  "3: Upload Data",
                  value = "nav3",
                  h4("Select experiment data (.eds, .xls/x, .csv, or zipped .fsa files)"),
                  # File Input
                  fileInput("data.input", NULL, buttonLabel = "Add data file(s) ...", multiple = TRUE),
                  
                  # Dropdown for "How many files per plate?"
                  uiOutput("files_per_plate_ui"),
                  
                  # Text Inputs for Readouts
                  uiOutput("readout_inputs"),
                  
                  # UI for Drag-and-Drop
                  uiOutput("drag_and_drop_ui"),
                  
                  # Display Matches
                  verbatimTextOutput("match_result"),
                  tableOutput("processed_data"),
                  tableOutput("assignments_table"),
                  
                  
                  
                  
                  uiOutput("next_button_ui3")
                  
                  
                  ),
                tags$script(
                  '
                  var tab3 = $(\'a[data-value="nav3"]\').parent().addClass("disabled");
                  $(function(){
                  $(tab.parent()).on("click", "li.disabled", function(e) {
                  e.preventDefault();
                  return false;
                  });
                  });
                  '
                  ),
                tabPanel(
                  "4: Data Analysis",
                  value = "nav4",
                  h4("analysis"),
                  actionButton("qpcr1", "Unpack .eds files"),
                  tableOutput("broken_eds_df")
                  
                ),
                tags$script(
                  '
                  var tab4 = $(\'a[data-value="nav4"]\').parent().addClass("disabled");
                  $(function(){
                  $(tab.parent()).on("click", "li.disabled", function(e) {
                  e.preventDefault();
                  return false;
                  });
                  });
                  '
                ),
                )
  ),
  
  # Conditional Panel for "Prepare Report with Replicate Data" Workflow
  conditionalPanel(
    condition = "output.selected_workflow == 'Prepare Report with Replicate Data'",
    tabsetPanel(id = "workflow_tabs_replicate", selected = "Step 1",
                tabPanel("Home", value = "back_to_welcome", ""),
                tabPanel("Step 1", "Content for Step 1 (Replicate Data)"),
                tabPanel("Step 2", "Content for Step 2 (Replicate Data)"),
                tabPanel("Step 3", "Final Step (Replicate Data)")
    )
  )
)

server <- function(input, output, session) {
  
  options(shiny.maxRequestSize=50*1024^2) 
  # Reactive value to store the selected workflow
  selected_workflow <- reactiveVal(NULL)
  
  output$welcome_screen <- renderUI({
    if (is.null(selected_workflow())) {
      tagList(
        h2("Select a Workflow"),
        actionButton("new_data", "Analyze New Data"),
        actionButton("replicate_data", "Prepare Report with Replicate Data")
      )
    } else {
      NULL  # Remove welcome screen once selection is made
    }
  })
  
  # Observe button clicks to set the workflow
  observeEvent(input$new_data, {
    selected_workflow("Analyze New Data")
    showNotification("You selected: Analyze New Data", type = "message")
  })
  
  observeEvent(input$replicate_data, {
    selected_workflow("Prepare Report with Replicate Data")
    showNotification("You selected: Prepare Report with Replicate Data", type = "message")
  })
  
  # Observe the dummy "Back to Welcome Screen" tab click event
  observeEvent(input$workflow_tabs_analyze, {
    if (input$workflow_tabs_analyze == "back_to_welcome") {
      selected_workflow(NULL)  # Reset workflow to show welcome screen
    }
  })
  
  observeEvent(input$workflow_tabs_replicate, {
    if (input$workflow_tabs_replicate == "back_to_welcome") {
      selected_workflow(NULL)  # Reset workflow to show welcome screen
    }
  })
  
  # Output the selected workflow for conditional panels
  output$selected_workflow <- reactive({
    selected_workflow()
  })
  outputOptions(output, "selected_workflow", suspendWhenHidden = FALSE)
  
  # ------------------------------------------------ Analyze experiment
  
  # ----------------------------------------- Step 1: Experiment Information
  
  # ----------- Dynamic "next" button
  
  animate_nav2 <- reactiveVal("FALSE")

  
  output$next_button_ui <- renderUI({
    # Check if exp.id has content
    if (nzchar(input$exp.id)) {
      actionButton("next_button_ui", "Next", onclick = "$(tab).removeClass('disabled')")
    } else {
      NULL  # No button when exp.id is empty
    }
  })
  
  # Observe changes in input$exp.id and update animate_nav2 accordingly
  observe({
    if (!is.null(input$exp.id) && nzchar(input$exp.id)) {
      animate_nav2("TRUE")  # Set animate_nav2 to TRUE when input$exp.id has text
    } else {
      animate_nav2("FALSE")  # Reset to NULL if input$exp.id is empty
    }
  })
  
  # Observe the reactive value animate_nav2
  observe({
    if (animate_nav2() == "TRUE") {
      updateTabsetPanel(session, "tabs", selected = "nav2")
      session$sendCustomMessage("animateTab", "nav2")
      print("hello")
    }
  })
  
  
  observeEvent(input$next_button_ui, {
    updateTabsetPanel(session, "workflow_tabs_analyze", selected = "nav2")
  })

  
  
  # ----------------------------------------- Step 2: Plate Map
  
  
  # Reactive expression to process the uploaded XML file
#  parsed_data <- reactive({
#    req(input$xml.report.file)  # Ensure file is uploaded before proceeding
#    df <- parse_printmap(input$xml.report.file$datapath)  # Process the file
#    print(input$xml.report.file$datapath)
#    print(head(df))
#    return(df)
#  })
 
  parsed_data <- reactive({
    req(input$xml.report.file)  # Ensure file is uploaded before proceeding
    
    withProgress(message = "Reading file...", value = 0, {
      incProgress(0.3, detail = "Processing print report...")
      df <- parse_printmap(input$xml.report.file$datapath)  # Process the file
      
      incProgress(0.8, detail = "Finalizing map...")
      print(input$xml.report.file$datapath)
      print(head(df))
      
      return(df)
    })
  })
  
  # This reactive value is a stand-in for a future module that will allow user to add secondary assignments to the printmap
  
  final_plate_map <- reactive({
    req(parsed_data())
    return(parsed_data())
  })
  
  
   
  # Generate unique plate options
  unique_plates <- reactive({
    df <- final_plate_map()
    print(unique(df$plate))
    unique(df$plate)
  })
  
  # Render multiple selection input based on unique plates
  output$plate_selector <- renderUI({
    req(unique_plates())  # Ensure unique plates exist
    #selectInput("selected_plates", "Select Plates", choices = unique_plates(), multiple = TRUE)
    pickerInput(
      "spid",
      label = "Select plates to analyze:",
      choices = unique_plates(),
      selected = unique_plates(),  # Default select all plates
      options = pickerOptions(
        actionsBox = TRUE,
        size = 10,
        selectedTextFormat = "count > 4"
      ),
      multiple = TRUE)
      
  })
  
  # Generate a list of conditions present on all plates
  
  treatment_ls <- reactive({
    req(input$spid)  # Ensure at least one plate is selected
    selected_plates <- input$spid
    platemap_data <- final_plate_map()
    
    # Filter data for selected plates
    selected_plate_data <- platemap_data %>%
      dplyr::filter(plate %in% selected_plates)
    
    # Find treatments that appear in ALL selected plates
    treatment_list <- selected_plate_data %>%
      dplyr::group_by(compound) %>%
      dplyr::summarize(n_plates = n_distinct(plate)) %>%
      dplyr::filter(n_plates == length(selected_plates)) %>%  # Keep only treatments in ALL plates
      dplyr::pull(compound)  # Extract as a vector
    
    return(treatment_list) 
  })
  
  # Drop-down menu list
  
  output$treatment_selector <- renderUI({
    req(treatment_ls())  # Ensure treatment_ls() has returned a value
    
    # Check if "DMSO" exists in the treatment list
    default_treatment <- if ("DMSO" %in% treatment_ls()) {
      "DMSO"  # If "DMSO" exists, set it as the default
    } else {
      NA  # Otherwise, set "NA" as the default
    }
    
    selectInput(
      inputId = "selected_treatment",
      label = "Select control treatment for normalization:",
      choices = treatment_ls(),  # Use the reactive treatment list
      selected = default_treatment,
      multiple = FALSE
    )
  })
  
  
  output$next_button_ui2 <- renderUI({
    req(input$selected_treatment)  # Ensure a selection has been made
    
    actionButton("next_button_ui2", "Next", onclick = "$(tab3).removeClass('disabled')")
  })
  
  observeEvent(input$selected_treatment, {
    if (nzchar(input$selected_treatment)) {
      updateTabsetPanel(session, "tabs", selected = "nav3")
      session$sendCustomMessage("animateTab", "nav3")
      sel.norm.treatment <- input$selected_treatment
      #print(sel.norm.treatment)
      select.norm.treatment.message <- paste0("You selected ", sel.norm.treatment, " as the control treatment. Measurements for other treatment conditions will be normalized to the mean measurment for ", sel.norm.treatment, ".")
      showNotification(select.norm.treatment.message, type = "message")
      #print("hello!")  # Print when a selection is made
    } else {
      print("Something terrible has happened")  # Print when no selection is made
    }
  })
  
  observeEvent(input$next_button_ui2, {
    updateTabsetPanel(session, "workflow_tabs_analyze", selected = "nav3")
  })
  
  
  # ------------------------------------- Step 3: Upload & assign data
  
  input_plates <- reactive({
    req(input$data.input)  # Ensure files are uploaded
    files <- input$data.input #$name  # Get uploaded file names
    print(files)
    files$ext <- tolower(file_ext(files$name))
    
    # Initialize an empty list to store dataframes
    df_list <- list()
    
    eds.list <- files %>%
      dplyr::filter(ext %in% c("eds"))
    if(length(eds.list) > 0){
      df_list[["eds"]] <- purrr::map(eds.list$datapath, process_eds_files) %>%
        bind_rows()
    }
    
    xl.list <- files %>%
      dplyr::filter(ext %in% c("xls", "xlsx"))
    if(length(xl.list) > 0){
      df_list[["xl"]] <- purrr::map(xl.list$datapath, process_xl_files)
    }
    
    zip.list <- files %>%
      dplyr::filter(ext %in% c("zip"))
    if(length(xl.list) > 0){
      df_list[["zip"]] <- purrr::map2(zip.list$datapath, zip.list$name, process_zip_files)
    }
    
    df_list <- df_list
    
    # Bind all dataframes together
    if (length(df_list) > 0) {
      dout <- bind_rows(df_list)# , .id = "source")
      dout <- dout %>%
        #dplyr::mutate(file.type = file_ext(file.name)) %>%
        dplyr::mutate(unique.plate = paste(sample.name, file.type, sep = "_")) %>%
        dplyr::relocate(plate.assignment, .after = last_col())
      return(dout)
      #return(bind_rows(df_list, .id = "source"))
    } else {
      return(data.frame())  # Return empty dataframe if nothing is processed
    }

  })
  
  
  #output$processed_data <- renderTable({
  #  input_plates()
  #})
  
  plates_df <- reactive({
    df <- input_plates()
    p_df <- df %>%
      dplyr::filter(flag == "PASS") %>%
      group_by(unique.plate) %>%
      dplyr::summarise(
        unique.plate = first(unique.plate),
        sample.name = first(sample.name),
        file.type = first(file.type),
        plate.assignment = NA,
        .groups = "drop"
      ) %>%
      ungroup()
    
    return(p_df)
    
  })
  
  
  
  # Render drag-and-drop UI
  output$drag_and_drop_ui <- renderUI({
    req(input$spid, plates_df())  # Ensure necessary data exists
    
    file_names <- plates_df()$sample.name  # Extract sample names
    plate_names <- input$spid  # Extract plate IDs
    
    file_types <- plates_df()$file.type  # Extract file types
    svg_files <- paste0("./filetype_icons/", file_types, "_icon.png")  # Adjust this if your SVG naming convention differs
    #svg_files <- paste0(file_types, "_icon.svg")  # Ensure this matches your naming convention
    
    
    
    tagList(
      h4("Drag files to the matching plates"),
      fluidRow(
        column(
          width = 4,
          h5("Files"),
          rank_list(
            input_id = "unassigned_files",
            text = "Unassigned Files",
            #labels = file_names,  # Use extracted sample names
            labels = lapply(1:length(file_names), function(i) {
              # Convert raw SVG content to a data URL
              #svg_data_url <- paste0("data:image/svg+xml;base64,", base64enc::base64encode(charToRaw(paste(merged_df$svg.content[[i]], collapse = ""))))
              #svg_data_url <- "./filetype_icons/eds_icon.svg"
              svg_data_url <- svg_files[i]
              # For each file, create a div containing the image and the name
              tags$div(
                tags$img(
                  src = svg_data_url,  # Use the data URL as the src
                  width = "50px",       # Set the width of the SVG icon (adjust as needed)
                  style = "object-fit: contain;"  # Ensure the aspect ratio is preserved
                ),
                tags$span(file_names[i])  # Display the sample name next to the icon
              )
            }),
            options = sortable_options(group = "fileGroup")
          )
        ),
        
        column(
          width = 8,
          h5("Plates"),
          fluidRow(
            lapply(plate_names, function(plate) {
              column(
                width = 4,
                rank_list(
                  input_id = paste0("plate_", plate),
                  text = paste("Plate", plate),
                  labels = NULL,  # Initially empty
                  options = sortable_options(group = "fileGroup")
                )
              )
            })
          )
        )
      )
    )
  })
  
  # Reactive Values to track the assignments
  assignments <- reactiveVal(data.frame(sample.name = character(), plate = character(), stringsAsFactors = FALSE))
  
  # Observe the drag-and-drop updates to capture the file assignments to plates
  observe({
    # Iterate over each plate in input$spid
    plate_names <- input$spid
    
    # Initialize an empty list to store the file assignments
    file_assignments <- list()
    
    # For each plate, check the corresponding input for files assigned
    for (plate in plate_names) {
      # Get the assigned files in each target region (plate)
      assigned_files <- input[[paste0("plate_", plate)]]
      
      # If there are files assigned to this plate, store the assignment
      if (length(assigned_files) > 0) {
        file_assignments[[plate]] <- assigned_files
      } else {
        file_assignments[[plate]] <- NULL  # If no files are assigned to this plate
      }
    }
    
    # Create a new dataframe with sample.name and corresponding plate assignments
    new_assignments <- data.frame(
      sample.name = unlist(file_assignments),  # Flatten the list of files
      plate = rep(names(file_assignments), times = sapply(file_assignments, length)),  # Repeat plate names for each file assigned
      stringsAsFactors = FALSE
    )
    
    # Update the reactive assignments with the new dataframe
    assignments(new_assignments)

  })
  
  # Reactive dataframe output: Show the assignments table
  output$assignments_table <- renderTable({
    assignments()  # Display the current assignments
  })
  
  
  updated_data <- reactive({
    req(input_plates())  # Always require input_plates to be available
    
    # If assignments is available, perform the left join to update plate.assignment
    if (nrow(assignments()) > 0) {
      df <- input_plates() %>%
        left_join(assignments(), by = "sample.name") %>%
        dplyr::select(!plate.assignment) %>%
        dplyr::rename(plate.assignment = plate)
      #return(left_join(input_plates(), assignments(), by = "sample.name"))
      return(df)
    }
    
    # If assignments are not yet available, just return the input_plates data
    return(input_plates())
  })
  
  output$processed_data <- renderTable({
    updated_data()  # This will render either the raw or merged data
  })
  
  observe({
    plate_names <- input$spid
    plate_inputs <- sapply(plate_names, function(plate) input[[paste0("plate_", plate)]], simplify = FALSE)
    
    # Check if at least one plate has an assignment
    req(any(sapply(plate_inputs, function(x) !is.null(x) && length(x) > 0)))
    updateTabsetPanel(session, "tabs", selected = "nav4")
    session$sendCustomMessage("animateTab", "nav4")
    print("At least one plate has an assignment. Proceeding...")
  })
  
  # --------------------------------- Step 4: Data analysis
  
  broken_eds <- reactiveVal(NULL)
  
  observeEvent(input$qpcr1, {
    # Capture input data
    plate_assignments <- updated_data()
    plate_assignments <- plate_assignments %>%
      dplyr::filter(file.type == "eds")
    
    print(plate_assignments)
    
    p.in <- unique(plate_assignments$sample.name)
  
    # Pass it to external function
    processed_output <- purrr::map(p.in, eds_extraction_by_samplename, data = plate_assignments) %>%
      bind_rows()
    
    df.amp <<- string_scrubber(processed_output)
    posummary <- head(processed_output, 10)
    
    # Store the result for display
    broken_eds(posummary)
  })
  
  output$broken_eds_df <- renderTable({
    broken_eds()  # This will render either the raw or merged data
  })
  
  
  #final_plate_map()
  
}
shinyApp(ui, server)
