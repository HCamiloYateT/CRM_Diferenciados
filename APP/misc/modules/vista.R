# Módulo reutilizable por vista ----

.acciones_vista <- c(contactos = "contacto", actividades = "gestion", oportunidades = "oportunidad",
                     muestras = "muestra", solicitudes = "muestra", tareas = "tarea", notas = "tarea")

VistaUI <- function(id, vista) {
  ns <- NS(id)
  accion <- unname(.acciones_vista[id])
  div(class = "crm-vista",
      div(class = "crm-vista-cabecera", h1(vista$titulo),
          if (!is.na(accion)) .boton(ns("accion"), .acciones[[accion]]$titulo, "plus")),
      uiOutput(ns("estado")),
      if (length(.acciones_pendientes[[id]])) {
        div(class = "crm-panel", h2("Acciones del registro"),
            div(class = "crm-controles-registro", lapply(.acciones_pendientes[[id]], function(etiqueta) {
              tags$button(type = "button", disabled = NA, class = "btn btn-sm btn-outline-secondary", etiqueta)
            })),
            tags$small("Pendiente de vínculo con el registro, permisos y servicio validado de gestión."))
      },
      if (id == "resumen") uiOutput(ns("resumen")),
      if (id == "cohortes") shiny::div(class = "crm-indicadores-filtros",
        shiny::dateRangeInput(ns("periodo"), "Período de primera compra",
                              start = .HoyProductos() - 365, end = .HoyProductos(), format = "dd/mm/yyyy",
                              language = "es"), shiny::textOutput(ns("total_cohorte"))),
      if (id %in% c("individual", "detalle")) uiOutput(ns("seleccion")),
      if (id == "individual") uiOutput(ns("ficha")),
      if (!is.null(vista$grafico)) uiOutput(ns("panel_grafico")),
      if (id == "kanban") uiOutput(ns("tablero")),
      if (id != "individual") div(class = "crm-panel", h2(if (id == "estado") "Registro de fuentes" else "Detalle"),
                                  .tabla_ui(ns("tabla"))),
      if (id %in% c("jerarquia", "productos", "tier_admin")) uiOutput(ns("administrar")),
      uiOutput(ns("dependencias")), uiOutput(ns("nota")))
}

.datos_grafico <- function(tipo, datos, operativo) {
  if (is.null(datos) || !nrow(datos)) return(NULL)
  if (tipo == "pedidos") {
    fechas <- datos$fecha_creacion[!is.na(datos$fecha_creacion)]
    if (!length(fechas)) return(NULL)
    conteo <- table(format(fechas, "%Y-%m"))
  } else if (tipo == "cohortes") {
    conteo <- table(datos$cohorte)
  } else {
    pais <- ifelse(is.na(datos$PaiCodDtn) | datos$PaiCodDtn == "", "Sin código", datos$PaiCodDtn)
    if (tipo == "evolucion") {
      pedidos <- operativo$pedidos
      fecha <- pedidos$fecha_creacion[match(datos$pedido_id, pedidos$pedido_id)]
      filas <- unique(data.frame(pedido = datos$pedido_id, periodo = format(fecha, "%Y-%m"), pais = pais))
      filas <- filas[!is.na(filas$periodo), , drop = FALSE]
      if (!nrow(filas)) return(NULL)
      agregado <- aggregate(list(total = rep(1L, nrow(filas))),
                            list(etiqueta = filas$periodo, grupo = filas$pais), sum)
      return(agregado)
    }
    grupos <- unique(data.frame(pedido = datos$pedido_id, pais = pais))
    conteo <- table(grupos$pais)
  }
  data.frame(etiqueta = names(conteo), total = as.integer(conteo), grupo = rep("Total", length(conteo)))
}

.formato_cantidad <- function(x) {
  if (!length(x) || is.na(x)) return("No disponible")
  format(x, big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE, digits = 10L)
}

.directorio_ui_datos <- function(datos, operativo) {
  if (is.null(datos) || is.null(operativo$pedidos)) return(datos)
  pedidos <- operativo$pedidos
  grupos <- split(pedidos, pedidos$PerCod)
  ultimo <- vapply(grupos, function(x) {
    fechas <- x$fecha_creacion[!is.na(x$fecha_creacion)]
    if (length(fechas)) as.character(max(fechas)) else NA_character_
  }, character(1))
  datos$ultimo_pedido <- unname(ultimo[match(datos$PerCod, names(ultimo))])
  datos$pedidos_del_universo <- as.integer(table(pedidos$PerCod)[datos$PerCod])
  datos$sacos_del_universo <- .dp_agregar(pedidos$PerCod, pedidos$sacos, datos$PerCod)
  datos
}

VistaServer <- function(id, vista, publicaciones, operativo, filtros, abrir_accion, cargando) {
  moduleServer(id, function(input, output, session) {
    datos <- reactive({
      tabla <- .vista_datos(publicaciones(), operativo(), vista$fuente, filtros())
      if (id %in% c("prospectos", "leads") && !is.null(tabla)) {
        if (!"etapa" %in% names(tabla)) return(NULL)
        etapa <- if (id == "prospectos") "PROSPECTO" else "LEAD"
        tabla <- tabla[which(tabla$etapa == etapa), , drop = FALSE]
      }
      if (id == "cohortes" && !is.null(tabla)) {
        rango <- input$periodo
        shiny::req(length(rango) == 2L, !anyNA(rango), rango[1L] <= rango[2L])
        tabla <- tabla[which(tabla$fecha_creacion >= rango[1L] & tabla$fecha_creacion <= rango[2L]), , drop = FALSE]
      }
      if (id == "directorio") tabla <- .directorio_ui_datos(tabla, operativo())
      if (id == "detalle" && !is.null(tabla) && nzchar(input$registro %||% "")) {
        tabla <- tabla[tabla$pedido_id == input$registro, , drop = FALSE]
      }
      tabla
    })
    if (id == "cohortes") output$total_cohorte <- shiny::renderText({
      registros <- datos()
      shiny::req(!is.null(registros))
      paste("Clientes nuevos en el período:", .formato_cantidad(.prod_contar(registros$PerCod)))
    })
    output$estado <- renderUI({
      if (cargando()) return(.nota("Cargando publicación…"))
      estado <- if (vista$fuente == "registro") "Disponible" else .estado_fuente(publicaciones(), vista$fuente)
      if (estado != "Disponible") {
        tipo <- if (estado %in% c("Error de lectura", "Esquema incompatible")) "error" else "muted"
        return(.nota(paste(estado, "·", gsub("_", " ", vista$fuente)), tipo))
      }
      if (!is.null(datos()) && !nrow(datos())) return(.nota("Sin registros para los filtros activos.", "empty"))
      NULL
    })
    output$dependencias <- renderUI({
      fuentes <- .dependencias_vista[[id]]
      if (!length(fuentes)) return(NULL)
      filas <- lapply(seq_along(fuentes), function(i) {
        estado <- .estado_fuente(publicaciones(), unname(fuentes[i]))
        tags$li(strong(names(fuentes)[i]), paste("·", estado))
      })
      tags$details(class = "crm-panel", tags$summary("Disponibilidad de componentes"), tags$ul(filas))
    })
    output$nota <- renderUI({
      pub <- publicaciones()$operacional
      corte <- pub$manifiesto$procedencia$corte
      if (id %in% c("resumen", "pedidos") && length(corte)) {
        fecha <- format(as.Date(corte), "%d/%m/%Y")
        return(.nota(paste("Corte", fecha, "·", vista$nota %||% "Pedidos de línea 20000.")))
      }
      if (!is.null(vista$nota)) .nota(vista$nota)
    })
    if (id != "individual") .tabla_server("tabla", datos, output)

    if (!is.null(vista$grafico)) {
      serie <- reactive(.datos_grafico(vista$grafico, datos(), operativo()))
      output$panel_grafico <- renderUI({
        dat <- serie()
        titulo <- switch(vista$grafico, pedidos = "Pedidos por mes de creación", cohortes = "Clientes por cohorte",
                         destino = "Pedidos por destino", evolucion = "Pedidos por mes y destino")
        salida <- if (is.null(dat) || !nrow(dat)) {
          .nota("Serie no disponible para esta selección.", "muted")
        } else if (requireNamespace("plotly", quietly = TRUE)) {
          plotly::plotlyOutput(session$ns("grafico"), height = "500px")
        } else plotOutput(session$ns("grafico"), height = "500px")
        div(class = "crm-panel", h2(titulo), salida)
      })
      if (requireNamespace("plotly", quietly = TRUE)) {
        output$grafico <- plotly::renderPlotly({
          dat <- serie()
          validate(need(!is.null(dat) && nrow(dat), "Serie no disponible para esta selección."))
          p <- plotly::plot_ly(dat, x = ~etiqueta, y = ~total, color = ~grupo, type = "bar",
                               colors = c("#B8261C", "#37545A", "#7C5E38", "#505A77"),
                               hovertemplate = "%{x}<br>%{y}<extra>%{fullData.name}</extra>")
          tema <- if (.disponible_api("racafeGraph", "tema_racafe_plotly")) {
            racafeGraph::tema_racafe_plotly()
          } else list()
          ejes <- list(barmode = "group", showlegend = length(unique(dat$grupo)) > 1L,
                       xaxis = list(title = "", automargin = TRUE), yaxis = list(title = "Cantidad"))
          do.call(plotly::layout, c(list(p), utils::modifyList(tema, ejes)))
        })
      } else {
        output$grafico <- renderPlot({
          dat <- serie()
          validate(need(!is.null(dat) && nrow(dat), "Serie no disponible para esta selección."))
          matriz <- stats::xtabs(total ~ grupo + etiqueta, dat)
          par(mar = c(7, 4, 1, 1))
          barplot(matriz, las = 2, col = c("#B8261C", "#37545A", "#7C5E38", "#505A77"), beside = TRUE,
                  ylab = "Cantidad", legend.text = nrow(matriz) > 1L, cex.names = 0.75)
        })
      }
    }

    if (id %in% c("individual", "detalle")) {
      output$seleccion <- renderUI({
        origen <- if (id == "individual") operativo()$clientes else operativo()$pedidos
        if (!is.null(origen) && nrow(origen)) {
          opciones <- if (id == "individual") {
            setNames(origen$PerCod, paste(origen$PerCod, origen$PerRazSoc))
          } else setNames(origen$pedido_id, paste(origen$CiaCod, origen$PdcCod, sep = " / "))
          return(selectInput(session$ns("registro"), if (id == "individual") "Cliente" else "Pedido",
                             c("Seleccione…" = "", opciones), selected = isolate(input$registro %||% "")))
        }
        .nota("Sin opciones en el universo filtrado.", "muted")
      })
    }

    if (id == "individual") {
      output$ficha <- renderUI({
        if (!nzchar(input$registro %||% "")) return(NULL)
        do.call(tabsetPanel, lapply(seq_along(.ficha), function(i) {
          nombre <- unname(.ficha[i])
          fuente <- .vista_datos(publicaciones(), operativo(), nombre, filtros())
          estado <- .estado_fuente(publicaciones(), nombre)
          salida <- if (is.null(fuente)) {
            .nota(estado, "muted")
          } else if (!"PerCod" %in% names(fuente)) {
            .nota("Pendiente de vínculo verificable con PerCod.", "muted")
          } else if (!any(fuente$PerCod == input$registro, na.rm = TRUE)) {
            .nota("Sin registros para el cliente seleccionado.", "empty")
          } else .tabla_ui(session$ns(paste0("ficha_", i)))
          tabPanel(names(.ficha)[i], div(class = "crm-panel", salida))
        }))
      })
      for (i in seq_along(.ficha)) local({
        indice <- i
        .tabla_server(paste0("ficha_", indice), reactive({
          fuente <- .vista_datos(publicaciones(), operativo(), unname(.ficha[indice]), filtros())
          if (is.null(fuente) || !"PerCod" %in% names(fuente)) return(NULL)
          fuente[which(fuente$PerCod == input$registro), , drop = FALSE]
        }), output)
      })
    }

    if (id == "resumen") {
      fuentes_kpi <- c("Empresas registradas" = "empresas", "Contactos" = "contactos",
                       "Prospectos" = "empresas", "Leads" = "empresas",
                       "Clientes históricos" = "clientes_comerciales", "Clientes nuevos" = "primer_pedido_cliente",
                       "Pedidos creados" = "pedidos", "Sacos pedidos" = "pedidos", .fuentes_pendientes)
      valor <- function(i) {
        etiqueta <- names(fuentes_kpi)[i]
        fuente <- unname(fuentes_kpi[i])
        datos_kpi <- .vista_datos(publicaciones(), operativo(), fuente, filtros())
        if (is.null(datos_kpi)) return("No disponible")
        if (etiqueta %in% names(.fuentes_pendientes)) return(.estado_fuente(publicaciones(), fuente))
        if (etiqueta %in% c("Prospectos", "Leads")) {
          if (!"etapa" %in% names(datos_kpi)) return("No disponible")
          return(.formato_cantidad(sum(datos_kpi$etapa == if (etiqueta == "Prospectos") "PROSPECTO" else "LEAD",
                                       na.rm = TRUE)))
        }
        if (etiqueta == "Clientes históricos") return(.formato_cantidad(sum(datos_kpi$tiene_pedidos, na.rm = TRUE)))
        if (etiqueta == "Sacos pedidos") return(.formato_cantidad(.dp_suma(datos_kpi$sacos)))
        .formato_cantidad(nrow(datos_kpi))
      }
      output$resumen <- renderUI({
        div(class = "crm-kpis", lapply(seq_along(fuentes_kpi), function(i) {
          .kpi_ui(session$ns(paste0("kpi_", i)), names(fuentes_kpi)[i])
        }))
      })
      for (i in seq_along(fuentes_kpi)) local({
        indice <- i
        .kpi_server(paste0("kpi_", indice), reactive(valor(indice)), output)
      })
    }
    if (id == "kanban") output$tablero <- renderUI({
      registros <- datos()
      if (is.null(registros) || !all(c("etapa", "empresa") %in% names(registros))) return(NULL)
      div(class = "crm-kpis", lapply(c("CONTACTO", "PROSPECTO", "LEAD"), function(etapa) {
        grupo <- registros[which(registros$etapa == etapa), , drop = FALSE]
        div(class = "crm-panel", h2(etapa), lapply(grupo$empresa, function(empresa) {
          if (.disponible_api("racafeModulos", "TarjetaKanbanUI")) return(racafeModulos::TarjetaKanbanUI(empresa))
          div(class = "crm-kpi", empresa)
        }))
      }))
    })
    if (id %in% c("jerarquia", "productos", "tier_admin")) output$administrar <- renderUI({
      campos <- switch(id, jerarquia = c("PerCod", "Cuenta padre", "Evidencia", "Vigencia"),
                       productos = c("Código original", "Descripción", "Marca", "Categoría", "Producto", "Vigencia"),
                       tier_admin = c("Cuenta padre", "Versión de método", "Tier propuesto", "Vigencia"))
      div(class = "crm-panel", h2("Propuesta y aprobación"),
          tags$fieldset(class = "crm-form-grid", disabled = NA, lapply(seq_along(campos), function(i) {
            textInput(session$ns(paste0("campo_", i)), campos[i])
          })),
          .bloqueado("Aprobar cambios", "Pendiente de contrato aprobado, permisos y servicio de escritura."))
    })
    if (id %in% names(.acciones_vista)) observeEvent(input$accion, abrir_accion(unname(.acciones_vista[id])))
  })
}
