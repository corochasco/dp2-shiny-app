library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(viridis)
library(shinycssloaders)

map_export_available <- requireNamespace("mapview", quietly = TRUE) && requireNamespace("webshot2", quietly = TRUE)

DP2_iterativo <- function(file, sheet, referencia = "minimum", tol = 1e-6, max_iter = 100) {
  df <- read_excel(file, sheet = sheet)
  id <- df[[1]]
  X <- df[,-1, drop = FALSE]
  
  ref <- if (referencia == "minimum") sapply(X, min, na.rm = TRUE) else sapply(X, max, na.rm = TRUE)
  sigma <- sapply(X, function(x) {
    s <- sd(x, na.rm = TRUE)
    if (isTRUE(all.equal(s, 0))) NA_real_ else s
  })
  
  Z <- sweep(X, 2, ref, "-")
  Z <- sweep(Z, 2, sigma, "/")
  Z <- as.data.frame(Z)
  
  DP2_old <- sqrt(rowSums(Z^2))
  historial <- list()
  iter <- 0
  converged <- FALSE
  
  while (!converged && iter < max_iter) {
    iter <- iter + 1
    cors <- sapply(Z, function(z) suppressWarnings(cor(z, DP2_old)))
    ord <- names(sort(abs(cors), decreasing = TRUE))
    
    DP2_new <- rep(0, nrow(Z))
    pesos <- numeric(length(ord))
    names(pesos) <- ord
    
    for (i in seq_along(ord)) {
      v <- ord[i]
      if (i == 1) {
        w <- 1
      } else {
        form <- as.formula(paste(v, "~", paste(ord[1:(i-1)], collapse = "+")))
        fit <- lm(form, data = Z)
        w <- 1 - summary(fit)$r.squared
      }
      pesos[v] <- w
      DP2_new <- DP2_new + Z[[v]] * w
    }
    
    historial[[iter]] <- list(DP2 = DP2_new, orden = ord, pesos = pesos)
    if (max(abs(DP2_new - DP2_old), na.rm = TRUE) < tol) converged <- TRUE
    DP2_old <- DP2_new
  }
  
  IBS <- 100 * DP2_old / mean(DP2_old, na.rm = TRUE)
  cor_matrix <- cor(X, use = "pairwise.complete.obs")
  cor_DP2 <- sapply(X, function(x) suppressWarnings(cor(x, IBS, use = "pairwise.complete.obs")))
  
  list(
    indice = data.frame(ID = id, DP2 = IBS, check.names = FALSE),
    correlaciones_entre_variables = cor_matrix,
    correlaciones_con_DP2 = cor_DP2,
    pesos_finales = pesos,
    orden_final = ord,
    iteraciones = iter,
    historial_iteraciones = historial
  )
}

ui <- fluidPage(
  titlePanel("DP2 Index Calculator"),
  fluidRow(
    column(4,
           fileInput("excel_file", "Upload Excel File", accept = ".xlsx"),
           textInput("sheet_name", "Sheet Name", value = "Sheet1"),
           radioButtons("referencia", "Reference for normalization:", choices = c("minimum", "maximum"), selected = "minimum"),
           fileInput("shapefile", "Upload shapefile (.zip)", accept = ".zip"),
           numericInput("tol", "Tolerance:", value = 1e-6, min = 1e-10, step = 1e-6),
           numericInput("max_iter", "Maximum iterations:", value = 100, min = 10, step = 10),
           actionButton("run_dp2", "Run DP2 Index"),
           downloadButton("download_excel", "Download Full Results"),
           actionButton("reset_app", "Reset App")
    ),
    column(8,
           tabsetPanel(
             tabPanel("Results Summary",
                      textOutput("convergence_warning"),
                      verbatimTextOutput("iterations"),
                      tableOutput("dp2_table"),
                      leafletOutput("dp2_map") %>% withSpinner()
             ),
             tabPanel("Charts",
                      plotOutput("histogram") %>% withSpinner(),
                      plotOutput("iteration_plot") %>% withSpinner()
             ),
             tabPanel("README",
                      verbatimTextOutput("readme")
             )
           )
    )
  )
)

server <- function(input, output, session) {
  resultado <- reactiveVal(NULL)
  mapa_union <- reactiveVal(NULL)
  
  observeEvent(input$run_dp2, {
    req(input$excel_file)
    excel_path <- input$excel_file$datapath
    
    sheets <- excel_sheets(excel_path)
    if (!(input$sheet_name %in% sheets)) {
      showNotification(paste("Sheet", input$sheet_name, "not found."), type = "error")
      return(NULL)
    }
    
    res <- DP2_iterativo(excel_path, input$sheet_name, input$referencia,
                         tol = input$tol, max_iter = input$max_iter)
    resultado(res)
    
    if (res$iteraciones >= input$max_iter) {
      output$convergence_warning <- renderText({
        "⚠ Warning: DP2 did not converge within the maximum number of iterations."
      })
    } else {
      output$convergence_warning <- renderText({ "" })
    }
    
    # Try to load shapefile
    shp_path <- NULL
    if (!is.null(input$shapefile)) {
      unzip(input$shapefile$datapath, exdir = tempdir())
      shp_path <- list.files(tempdir(), pattern = "\\.shp$", full.names = TRUE)[1]
    } else if (file.exists("ExtremaduraMunis2.shp")) {
      shp_path <- "ExtremaduraMunis2.shp"
    }
    
    if (!is.null(shp_path)) {
      mapa <- tryCatch(st_read(shp_path, quiet = TRUE), error = function(e) NULL)
      if (!is.null(mapa) && "CODE" %in% names(mapa)) {
        mapa_union(left_join(mapa, res$indice, by = c("CODE" = "ID")))
      } else {
        mapa_union(NULL)
        showNotification("Shapefile missing CODE field or cannot be read.", type = "error")
      }
    } else {
      mapa_union(NULL)
      showNotification("No shapefile provided.", type = "warning")
    }
  })
  
  output$iterations <- renderPrint({
    req(resultado())
    paste("Number of iterations:", resultado()$iteraciones)
  })
  
  output$dp2_table <- renderTable({
    req(resultado())
    head(resultado()$indice)
  })
  
  output$dp2_map <- renderLeaflet({
    req(resultado())
    if (!is.null(mapa_union())) {
      leaflet(mapa_union()) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~colorNumeric(viridis(100), DP2)(DP2),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          popup = ~paste("Municipality:", MUNICIPIO, "<br>DP2:", round(DP2, 2))
        ) %>%
        addLegend(pal = colorNumeric(viridis(100), mapa_union()$DP2),
                  values = mapa_union()$DP2, title = "DP2 Index", position = "bottomright")
    } else {
      leaflet() %>% addTiles()
    }
  })
  
  output$histogram <- renderPlot({
    req(resultado())
    ggplot(resultado()$indice, aes(x = DP2)) +
      geom_histogram(fill = "steelblue", color = "white", bins = 20) +
      theme_minimal() + labs(title = "Histogram of DP2 Index")
  })
  
  output$iteration_plot <- renderPlot({
    req(resultado())
    iter_values <- sapply(resultado()$historial_iteraciones, function(x) mean(x$DP2))
    ggplot(data.frame(Iteration = 1:length(iter_values), MeanDP2 = iter_values),
           aes(x = Iteration, y = MeanDP2)) +
      geom_line(color = "darkgreen") + theme_minimal() +
      labs(title = "DP2 Evolution by Iteration")
  })
  
  output$readme <- renderText({
    if (file.exists("README.md")) {
      paste(readLines("README.md"), collapse = "\n")
    } else {
      "README.md not found."
    }
  })
  
  output$download_excel <- downloadHandler(
    filename = function() {
      "DP2_outcomes_iterations.xlsx"
    },
    content = function(file) {
      req(resultado())
      writexl::write_xlsx(resultado()$indice, file)
    }
  )
  
  observeEvent(input$reset_app, {
    resultado(NULL)
    mapa_union(NULL)
    updateTextInput(session, "sheet_name", value = "Sheet1")
    updateRadioButtons(session, "referencia", selected = "minimum")
    session$reload()
  })
}

shinyApp(ui, server)