####
# Version: 0.10.2
# Author:  Christian Jaeger (christian.jaeger@uk-halle.de)
# 20250312
####

## Main Changes:
# 0.10.2 Added Export Button
################

## Beschreibung:
# Diese Shiny-App ermöglicht das Hochladen und Zusammenführen von zwei CSV- oder TXT-Dateien.
# Falls die Dateien keine Spaltennamen enthalten, können diese aus einer zugehörigen SQL-Dump-Datei extrahiert werden.
# Der Nutzer kann auswählen, welche Spalten als ID für den Merge-Prozess verwendet werden sollen.
# Zudem kann zwischen verschiedenen Join-Typen (Inner, Outer, Left, Right) gewählt werden.
# Das zusammengeführte Ergebnis wird als interaktive Tabelle angezeigt und kann als CSV-Datei gespeichert werden.
# Optional kann der Nutzer einen eigenen Dateinamen für den Export vergeben.
################

library(shiny)
library(readr)
library(writexl)
library(stringr)
library(DT)
library(shinyjs)

extract_column_names <- function(sql_file) {
  sql_text <- readLines(sql_file, warn = FALSE)
  table_start <- grep("CREATE TABLE", sql_text)
  if (length(table_start) == 0) return(NULL)
  
  sql_text <- sql_text[(table_start + 1):length(sql_text)]
  table_end <- grep("\\) ENGINE", sql_text)[1]  # Ensure capturing all column definitions
  if (is.na(table_end)) return(NULL)
  
  column_lines <- sql_text[1:(table_end - 1)]
  column_names <- str_extract_all(column_lines, "`([^`]*)`")
  column_names <- unlist(column_names)
  column_names <- column_names[!is.na(column_names)]
  column_names <- gsub("`", "", column_names)
  
  return(column_names)
}

clean_column_names <- function(col_names) {
  col_names <- make.names(col_names, unique = TRUE)
  col_names <- gsub("\\.+(\\d+)$", "", col_names)  # Entfernt ...[Zahl] komplett
  return(col_names)
}

rename_columns <- function(df, file_name) {
  file_suffix <- tools::file_path_sans_ext(basename(file_name))
  colnames(df) <- paste0(clean_column_names(colnames(df)), "_", file_suffix)
  return(df)
}

# UI der Shiny-App
ui <- fluidPage(
  useShinyjs(),
  tags$head(tags$title("CSV/SQL - Merger - DIZ & Biomedical Data Science")),
  titlePanel(
    div(
      tags$img(src = "Logo_DIZ_DE.jpg", height = "80px", style = "margin-right: 10px;"),
      div(
        h1("CSV/SQL - Merger für File Preprocessings - 0.10.2", style = "margin-bottom: 0px;"),
        h4("Ein Service des Datenintegrationszentrums (DIZ) und der AG (Bio-) Medical Data Science", 
           style = "margin-top: 5px; color: gray; font-weight: normal;")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      checkboxInput("has_headers1", "Datei 1 enthält Spaltennamen", value = FALSE),
      
      fileInput("file1", "Lade CSV- oder TXT-Datei 1 hoch", accept = c(".csv", ".txt")),
      fileInput("sql1", "Lade zugehörige SQL-Dump-Datei 1 hoch", accept = ".sql"),
      hr(),
      checkboxInput("has_headers2", "Datei 2 enthält Spaltennamen", value = FALSE),
      fileInput("file2", "Lade CSV- oder TXT-Datei 2 hoch", accept = c(".csv", ".txt")),
      fileInput("sql2", "Lade zugehörige SQL-Dump-Datei 2 hoch", accept = ".sql"),
      
      uiOutput("select_id1"),
      uiOutput("select_id2"),
      
      hr(),
      selectInput("join_type", "Join-Typ auswählen:", 
                  choices = list("Inner Join" = "inner", 
                                 "Outer Join" = "outer", 
                                 "Left Join" = "left", 
                                 "Right Join" = "right")),
      actionButton("merge", "Merge-this-IDs"),
      hr(),
      textInput("filename", "Name der Datei: (optional)", value = ""),
      downloadButton("download", "Download Merged CSV"),
      downloadButton("download_excel", "Download Merged Excel"),
      hr(),
      tags$a(href = "readme.html", "Dokumentation öffnen", target = "_blank"),
      br(), br(),
      h4("Kontakt"),
      tags$p("Fragen? Schreiben Sie an: "),
      tags$a(href = "mailto:christian.jaeger@uk-halle.de", "christian.jaeger@uk-halle.de"),
    ),
    mainPanel(
      h3("Vorschau der Dateien"),
      fluidRow(
        column(6, DTOutput("preview1")),
        column(6, DTOutput("preview2"))
      ),
      h3("Zusammengeführte Datei"),
      DTOutput("merged_table")
    )
  )
)

server <- function(input, output, session) {
  observe({
    toggleState("sql1", condition = !input$has_headers1)
    toggleState("sql2", condition = !input$has_headers2)
  })
  
  data1 <- reactive({
    req(input$file1)
    if (!grepl("\\.csv$|\\.txt$", input$file1$name, ignore.case = TRUE)) {
      showNotification("Fehler: Ungültiges Dateiformat für Datei 1.", type = "error")
      return(NULL)
    }
    if (input$has_headers1) {
      df <- read_csv(input$file1$datapath, show_col_types = FALSE)
    } else {
      req(input$sql1)
      col_names <- extract_column_names(input$sql1$datapath)
      if (is.null(col_names)) return(NULL)
      df <- read_csv(input$file1$datapath, col_names = col_names, skip = 0, show_col_types = FALSE)
    }
    rename_columns(df, input$file1$name)
  })
  
  data2 <- reactive({
    req(input$file2)
    if (!grepl("\\.csv$|\\.txt$", input$file2$name, ignore.case = TRUE)) {
      showNotification("Fehler: Ungültiges Dateiformat für Datei 2.", type = "error")
      return(NULL)
    }
    if (input$has_headers2) {
      df <- read_csv(input$file2$datapath, show_col_types = FALSE)
    } else {
      req(input$sql2)
      col_names <- extract_column_names(input$sql2$datapath)
      if (is.null(col_names)) return(NULL)
      df <- read_csv(input$file2$datapath, col_names = col_names, skip = 0, show_col_types = FALSE)
    }
    rename_columns(df, input$file2$name)
  })
  
  output$preview1 <- renderDT({
    req(data1())
    datatable(data1(), options = list(scrollX = TRUE))
  })
  
  output$preview2 <- renderDT({
    req(data2())
    datatable(data2(), options = list(scrollX = TRUE))
  })
  
  output$select_id1 <- renderUI({
    req(data1())
    selectInput("id1", "Wähle ID-Spalte Datei 1:", choices = names(data1()))
  })
  
  output$select_id2 <- renderUI({
    req(data2())
    selectInput("id2", "Wähle ID-Spalte Datei 2:", choices = names(data2()))
  })
  
  merged_data <- reactiveVal()
  
  observeEvent(input$merge, {
    req(input$id1, input$id2)
    
    join_type <- input$join_type
    
    merged <- switch(join_type,
                     "inner" = merge(data1(), data2(), by.x = input$id1, by.y = input$id2),
                     "outer" = merge(data1(), data2(), by.x = input$id1, by.y = input$id2, all = TRUE),
                     "left" = merge(data1(), data2(), by.x = input$id1, by.y = input$id2, all.x = TRUE),
                     "right" = merge(data1(), data2(), by.x = input$id1, by.y = input$id2, all.y = TRUE))
    
    merged_data(merged)
  })
  
  output$merged_table <- renderDT({
    req(merged_data())
    datatable(merged_data(), options = list(scrollX = TRUE))
  })
  
  output$download <- downloadHandler(
    filename = function() {
      if (input$filename != "") {
        paste0(input$filename, ".csv")
      } else {
        paste0(tools::file_path_sans_ext(input$file1$name), "_", 
               tools::file_path_sans_ext(input$file2$name), ".csv")
      }
    },
    content = function(file) {
      req(merged_data())
      write_csv(merged_data(), file)
    }
  )
  
  output$download_excel <- downloadHandler(
    filename = function() {
      if (input$filename != "") {
        paste0(input$filename, ".xlsx")
      } else {
        paste0(tools::file_path_sans_ext(input$file1$name), "_", 
               tools::file_path_sans_ext(input$file2$name), ".xlsx")
      }
    },
    content = function(file) {
      req(merged_data())
      write_xlsx(merged_data(), file)
    }
  )
 }

shinyApp(ui, server)
