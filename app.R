library(shiny)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(janitor)
library(openxlsx)
library(DT)

# -----------------------------
# Utilidades
# -----------------------------
normaliza_cas <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- gsub("\u00A0", " ", x, fixed = TRUE)
  x <- gsub("\\s+", "", x)
  x <- gsub("[^0-9-]", "", x)
  x[x == ""] <- NA_character_
  x
}

corrige_utf8_df <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.character(col)) {
      col <- iconv(col, from = "", to = "UTF-8", sub = "")
      col[is.na(col)] <- ""
    }
    col
  })
  df
}

leer_tabla <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    suppressWarnings({
      df <- tryCatch(
        readr::read_csv(path, show_col_types = FALSE, locale = locale(encoding = "Latin1")),
        error = function(e) readr::read_csv(path, show_col_types = FALSE)
      )
    })
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path)
  } else {
    stop("Formato no soportado. Usa CSV o Excel.")
  }
  janitor::clean_names(df)
}

buscar_columna <- function(df, candidatas, etiqueta) {
  hit <- intersect(candidatas, names(df))
  if (length(hit) == 0) {
    stop(
      paste0(
        "No se encontró la columna de ", etiqueta, ". Columnas disponibles: ",
        paste(names(df), collapse = ", ")
      )
    )
  }
  hit[1]
}

clasifica_estatus <- function(kg_generados, umbral) {
  dplyr::case_when(
    is.na(umbral) ~ "CAS no RETC / sin umbral en catálogo",
    is.na(kg_generados) ~ "Sin dato suficiente",
    kg_generados >= umbral ~ "Reportar",
    kg_generados < umbral ~ "No reportar",
    TRUE ~ "Revisar"
  )
}

margen_umbral_pct <- function(kg_generados, umbral) {
  ifelse(!is.na(kg_generados) & !is.na(umbral) & umbral > 0,
         ((kg_generados - umbral) / umbral) * 100,
         NA_real_)
}

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  titlePanel("REPROSA - Matrices RETC"),
  sidebarLayout(
    sidebarPanel(
      fileInput("insumos", "Base de insumos (CSV/XLSX)", accept = c(".csv", ".xlsx", ".xls")),
      fileInput("catalogo", "Catálogo RETC NOM-165 (CSV)", accept = c(".csv")),
      hr(),
      h4("Paso 1"),
      actionButton("procesar_previo", "Clasificar insumos con/sin RETC", class = "btn-primary"),
      downloadButton("descargar_previo", "Descargar Excel clasificación"),
      hr(),
      h4("Paso 2"),
      actionButton("procesar_retc", "Generar matriz RETC", class = "btn-success"),
      downloadButton("descargar_retc_xlsx", "Descargar Excel RETC"),
      downloadButton("descargar_retc_csv", "Descargar CSV RETC"),
      width = 3
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Clasificación previa",
                 h4("Insumos con al menos una sustancia RETC"),
                 DTOutput("tabla_con_retc"),
                 br(),
                 h4("Insumos sin sustancias RETC"),
                 DTOutput("tabla_sin_retc")
        ),
        tabPanel("Matriz RETC",
                 h4("Resultado RETC"),
                 DTOutput("tabla_retc"),
                 br(),
                 h4("CAS no encontrados"),
                 DTOutput("tabla_no_encontrados")
        )
      )
    )
  )
)

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {
  previo <- reactiveVal(NULL)
  retc <- reactiveVal(NULL)

  # Paso 1
  observeEvent(input$procesar_previo, {
    req(input$insumos, input$catalogo)

    insumos <- leer_tabla(input$insumos$datapath)
    catalogo <- leer_tabla(input$catalogo$datapath)

    col_producto <- buscar_columna(insumos,
                                   c("comercial", "nombre_insumo", "insumo", "producto", "nombre_producto"),
                                   "producto / insumo")
    col_cas <- buscar_columna(insumos,
                              c("cas", "no_cas", "n_cas", "numero_cas", "n_cas_"),
                              "CAS")

    if (!all(c("cas", "sustancia_retc") %in% names(catalogo))) {
      stop("El catálogo debe contener al menos las columnas: cas, sustancia_retc")
    }

    insumos2 <- insumos |>
      mutate(
        producto_base = as.character(.data[[col_producto]]),
        cas = normaliza_cas(.data[[col_cas]])
      )

    catalogo2 <- catalogo |>
      mutate(cas = normaliza_cas(cas)) |>
      distinct(cas, .keep_all = TRUE)

    insumos_cruce <- insumos2 |>
      left_join(catalogo2 |> select(cas, sustancia_retc), by = "cas") |>
      mutate(es_retc = !is.na(sustancia_retc))

    resumen_producto <- insumos_cruce |>
      group_by(producto_base) |>
      summarise(
        total_filas = dplyr::n(),
        total_cas_unicos = dplyr::n_distinct(cas, na.rm = TRUE),
        contiene_retc = any(es_retc, na.rm = TRUE),
        total_sustancias_retc = sum(es_retc, na.rm = TRUE),
        cas_retc = paste(sort(unique(cas[es_retc & !is.na(cas)])), collapse = " | "),
        sustancias_retc = paste(sort(unique(sustancia_retc[es_retc & !is.na(sustancia_retc)])), collapse = " | "),
        .groups = "drop"
      ) |>
      mutate(contiene_retc = ifelse(contiene_retc, "Sí", "No"))

    con_retc <- resumen_producto |> filter(contiene_retc == "Sí") |> arrange(producto_base) |> corrige_utf8_df()
    sin_retc <- resumen_producto |> filter(contiene_retc == "No") |> arrange(producto_base) |> corrige_utf8_df()

    previo(list(con_retc = con_retc, sin_retc = sin_retc))
  })

  output$tabla_con_retc <- renderDT({
    req(previo())
    datatable(previo()$con_retc, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$tabla_sin_retc <- renderDT({
    req(previo())
    datatable(previo()$sin_retc, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$descargar_previo <- downloadHandler(
    filename = function() paste0("clasificacion_previa_insumos_retc_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(previo())
      wb <- createWorkbook()
      addWorksheet(wb, "insumos_con_retc")
      writeDataTable(wb, "insumos_con_retc", previo()$con_retc, withFilter = TRUE)
      addWorksheet(wb, "insumos_sin_retc")
      writeDataTable(wb, "insumos_sin_retc", previo()$sin_retc, withFilter = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # Paso 2
  observeEvent(input$procesar_retc, {
    req(input$insumos, input$catalogo)

    insumos <- leer_tabla(input$insumos$datapath)
    catalogo <- leer_tabla(input$catalogo$datapath)

    required_insumos <- c("cas", "cantidad_anual", "unidad_codigo", "factor_kg_por_unidad")
    required_catalogo <- c("cas", "sustancia_retc", "umbral_mpu_kg_anual")
    if (!all(required_insumos %in% names(insumos))) {
      stop(paste("La base de insumos debe contener:", paste(required_insumos, collapse = ", ")))
    }
    if (!all(required_catalogo %in% names(catalogo))) {
      stop(paste("El catálogo debe contener:", paste(required_catalogo, collapse = ", ")))
    }

    insumos <- insumos |>
      mutate(
        cas = normaliza_cas(cas),
        cantidad_anual = suppressWarnings(as.numeric(cantidad_anual)),
        factor_kg_por_unidad = suppressWarnings(as.numeric(factor_kg_por_unidad)),
        unidad_codigo = stringr::str_to_upper(stringr::str_trim(as.character(unidad_codigo))),
        kg_generados_fila = cantidad_anual * factor_kg_por_unidad,
        conversion_valida = !is.na(kg_generados_fila)
      )

    catalogo <- catalogo |>
      mutate(
        cas = normaliza_cas(cas),
        umbral_mpu_kg_anual = suppressWarnings(as.numeric(umbral_mpu_kg_anual))
      ) |>
      distinct(cas, .keep_all = TRUE)

    columnas_a_conservar <- intersect(c("nombre_compuesto", "nombre_insumo", "cliente", "observaciones"), names(insumos))

    insumos_consolidados <- insumos |>
      group_by(cas) |>
      summarise(
        kg_generados_anuales = sum(kg_generados_fila, na.rm = TRUE),
        filas_fuente = dplyr::n(),
        unidades_detectadas = paste(sort(unique(unidad_codigo[!is.na(unidad_codigo)])), collapse = ", "),
        cantidades_originales = paste(cantidad_anual[!is.na(cantidad_anual)], collapse = " | "),
        factores_originales = paste(factor_kg_por_unidad[!is.na(factor_kg_por_unidad)], collapse = " | "),
        conversion_incompleta = any(!conversion_valida),
        across(
          all_of(columnas_a_conservar),
          ~ paste(unique(stats::na.omit(as.character(.x))), collapse = " | "),
          .names = "{.col}"
        ),
        .groups = "drop"
      )

    resultado <- insumos_consolidados |>
      left_join(catalogo, by = "cas") |>
      mutate(
        es_sustancia_retc = !is.na(sustancia_retc),
        diferencia_vs_umbral_kg = kg_generados_anuales - umbral_mpu_kg_anual,
        porcentaje_umbral = ifelse(!is.na(kg_generados_anuales) & !is.na(umbral_mpu_kg_anual) & umbral_mpu_kg_anual > 0,
                                   (kg_generados_anuales / umbral_mpu_kg_anual) * 100,
                                   NA_real_),
        margen_umbral_porcentaje = margen_umbral_pct(kg_generados_anuales, umbral_mpu_kg_anual),
        estatus_reporte_mpu = clasifica_estatus(kg_generados_anuales, umbral_mpu_kg_anual),
        prioridad_revision = dplyr::case_when(
          !es_sustancia_retc ~ "Baja",
          conversion_incompleta ~ "Alta",
          estatus_reporte_mpu == "Reportar" ~ "Alta",
          !is.na(porcentaje_umbral) & porcentaje_umbral >= 80 ~ "Media",
          TRUE ~ "Baja"
        ),
        fundamento_reporte = dplyr::case_when(
          es_sustancia_retc ~ "NOM-165-SEMARNAT-2013, umbral MPU",
          TRUE ~ "CAS no localizado en catálogo RETC"
        )
      )

    for (col in c("categoria", "fundamento", "nombre_compuesto", "nombre_insumo", "cliente", "observaciones")) {
      if (!col %in% names(resultado)) resultado[[col]] <- NA_character_
    }

    resultado_retc <- resultado |>
      filter(es_sustancia_retc) |>
      arrange(desc(estatus_reporte_mpu == "Reportar"), desc(kg_generados_anuales)) |>
      transmute(
        sustancia_retc,
        cas,
        kg_generados_anuales = round(kg_generados_anuales, 6),
        umbral_reporte_mpu_kg_anual = umbral_mpu_kg_anual,
        estatus_reporte_mpu,
        porcentaje_umbral = round(porcentaje_umbral, 2),
        diferencia_vs_umbral_kg = round(diferencia_vs_umbral_kg, 6),
        margen_umbral_porcentaje = round(margen_umbral_porcentaje, 2),
        prioridad_revision,
        conversion_incompleta = ifelse(conversion_incompleta, "Sí", "No"),
        filas_fuente,
        unidades_detectadas,
        cantidades_originales,
        factores_originales,
        nombre_compuesto,
        nombre_insumo,
        cliente,
        observaciones,
        categoria,
        fundamento = dplyr::coalesce(fundamento, fundamento_reporte)
      ) |>
      corrige_utf8_df()

    no_encontrados <- resultado |>
      filter(!es_sustancia_retc) |>
      arrange(cas) |>
      transmute(
        cas,
        kg_generados_anuales = round(kg_generados_anuales, 6),
        filas_fuente,
        unidades_detectadas,
        cantidades_originales,
        factores_originales,
        conversion_incompleta = ifelse(conversion_incompleta, "Sí", "No"),
        nombre_compuesto,
        nombre_insumo,
        cliente,
        observaciones,
        motivo = "CAS no localizado en catálogo RETC NOM-165"
      ) |>
      corrige_utf8_df()

    retc(list(resultado_retc = resultado_retc, no_encontrados = no_encontrados))
  })

  output$tabla_retc <- renderDT({
    req(retc())
    datatable(retc()$resultado_retc, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$tabla_no_encontrados <- renderDT({
    req(retc())
    datatable(retc()$no_encontrados, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$descargar_retc_xlsx <- downloadHandler(
    filename = function() paste0("retc_resultado_", Sys.Date(), ".xlsx"),
    content = function(file) {
      req(retc())
      wb <- createWorkbook()
      addWorksheet(wb, "RETC_resultado")
      writeDataTable(wb, "RETC_resultado", retc()$resultado_retc, withFilter = TRUE)
      addWorksheet(wb, "CAS_no_encontrados")
      writeDataTable(wb, "CAS_no_encontrados", retc()$no_encontrados, withFilter = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$descargar_retc_csv <- downloadHandler(
    filename = function() paste0("retc_resultado_", Sys.Date(), ".csv"),
    content = function(file) {
      req(retc())
      write_csv(retc()$resultado_retc, file, na = "")
    }
  )
}

shinyApp(ui, server)
