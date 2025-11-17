
# Load necessary libraries
library(shiny)
library(readxl)
library(openxlsx)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)

# DP2_iterative function with minimum/maximum selector
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

# Function to export results
exportar_DP2_function <- function(res, archivo_salida = "DP2_outcomes_iterations.xlsx") {
  wb <- createWorkbook()
  addWorksheet(wb, "Final Index")
  writeData(wb, "Final Index", res$indice)

  addWorksheet(wb, "Correlations")
  writeData(wb, "Correlations", res$correlaciones_entre_variables, rowNames = TRUE)

  addWorksheet(wb, "Correlations with DP2")
  writeData(wb, "Correlations with DP2", data.frame(Variable = names(res$correlaciones_con_DP2), Correlacion = res$correlaciones_con_DP2))

  addWorksheet(wb, "Final Weights")
  pesos_df <- data.frame(Variable = names(res$pesos_finales), Peso = res$pesos_finales, Orden = match(names(res$pesos_finales), res$orden_final))
  pesos_df <- pesos_df[order(pesos_df$Orden), ]
  writeData(wb, "Final Weights", pesos_df)

  for (i in seq_along(res$historial_iteraciones)) {
    iter <- res$historial_iteraciones[[i]]
    sheet_name <- paste0("Iteration_", i)
    addWorksheet(wb, sheet_name)
    iter_df <- data.frame(ID = res$indice$ID, DP2 = iter$DP2)
    writeData(wb, sheet_name, iter_df, startCol = 1)
    pesos_iter <- data.frame(Variable = names(iter$pesos), Peso = iter$pesos)
    writeData(wb, sheet_name, pesos_iter, startCol = 4, startRow = 1)
    orden_iter <- data.frame(Orden = iter$orden)
    writeData(wb, sheet_name, orden_iter, startCol = 6, startRow = 1)
  }

  saveWorkbook(wb, archivo_salida, overwrite = TRUE)
}

# User interface
ui <- fluidPage(
  titlePanel("DP2 Index"),
  sidebarLayout(
    sidebarPanel(
      fileInput("excel_file", "Excel File", accept = ".xlsx"),
      textInput("sheet_name", "Sheet Name", value = "Sheet1"),
      radioButtons("referencia", "Reference to normalize:", choices = c("minimum", "maximum"), selected = "minimum"),
      fileInput("shapefile", "Upload shapefile (.zip)", accept = ".zip"),
      actionButton("run_dp2", "Run DP2 Index")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Outcomes (summary)",
          verbatimTextOutput("iterations"),
          tableOutput("dp2_table"),
          leafletOutput("dp2_map")
        ),
        tabPanel("README",
          verbatimTextOutput("readme")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  resultado <- reactiveVal(NULL)

  observeEvent(input$run_dp2, {
    req(input$excel_file)
    excel_path <- input$excel_file$datapath
    res <- DP2_iterativo(excel_path, input$sheet_name, input$referencia)
    resultado(res)
    exportar_DP2_function(res)
  })

  
  output$iterations <- renderPrint({
    req(resultado())
    resultado()$iteraciones
  })
  
  output$dp2_table <- renderTable({
    req(resultado())
    head(resultado()$indice)
  })
  
  output$dp2_map <- renderLeaflet({
    req(resultado())
    shp_path <- NULL
    if (!is.null(input$shapefile)) {
      unzip(input$shapefile$datapath, exdir = tempdir())
      shp_path <- list.files(tempdir(), pattern = "\\.shp$", full.names = TRUE)[1]
    } else if (file.exists("ExtremaduraMunis2.shp")) {
      shp_path <- "ExtremaduraMunis2.shp"
    }
    req(shp_path)
    mapa <- st_read(shp_path, quiet = TRUE)
    mapa_union <- left_join(mapa, resultado()$indice, by = c("CODITO" = "ID"))
    leaflet(mapa_union) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorNumeric("plasma", DP2)(DP2),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        popup = ~paste("Municipio:", MUNICIPIO, "<br>DP2:", round(DP2, 2))
      )
  })

  output$readme <- renderText({
    if (file.exists("README.md")) {
      paste(readLines("README.md"), collapse = "\n")
    } else {
      "README.md not found."
    }
  })
}

# Run the app
shinyApp(ui, server)
