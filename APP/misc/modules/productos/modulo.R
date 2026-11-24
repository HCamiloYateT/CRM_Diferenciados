# Tablero, relaciones, calidad y Market Basket ----

.prod_kpis <- c("Marcas", "Nombres comerciales", "Clientes", "Pedidos", "Sacos pedidos", "Sin atribución",
                "Sacos embarcados", "MNFCC COP/kg", "Peso FCC (kg)", "Lotes con FCC", "Concentración top")

.prod_colores <- function(n) {
  if (.disponible_api("racafeGraph", "ColoresRacafe")) return(racafeGraph::ColoresRacafe(n))
  rep(c("#DA291C", "#495057", "#198754", "#C0392B"), length.out = n)
}

ProductosUI <- function(id) {
  ns <- shiny::NS(id)
  panel <- function(titulo, contenido) shiny::tabPanel(titulo, div(class = "crm-panel", contenido))
  shiny::div(class = "crm-vista", shiny::h1("Productos"),
             shiny::div(class = "crm-indicadores-filtros",
                        shiny::selectInput(ns("dimension"), "Dimensión",
                                           c("Marca comercial" = "marca", "Nombre comercial" = "nombre")),
                        shiny::dateRangeInput(ns("periodo"), "Período", start = .HoyProductos() - 365,
                                              end = .HoyProductos(), format = "dd/mm/yyyy", language = "es"),
                        shiny::uiOutput(ns("filtro_comercial")),
                        shiny::numericInput(ns("top"), "Elementos del ranking", 10, min = 1, max = 50),
                        shiny::selectInput(ns("medida"), "Medida",
                                           c("Sacos pedidos" = "Sacos", "Sacos embarcados" = "Embarcado",
                                             "Clientes" = "Clientes", "Pedidos" = "Pedidos",
                                             "Frecuencia" = "Frecuencia", "Lotes" = "Lotes",
                                             "Margen total (COP)" = "MargenCOP", "MNFCC (COP/kg)" = "MNFCC"))),
             shiny::uiOutput(ns("estado")),
             bs4Dash::tabsetPanel(
               panel("Tablero de ejecución", shiny::tagList(shiny::uiOutput(ns("kpis")), shiny::h2("Ranking y Pareto"),
                     shiny::plotOutput(ns("pareto"), height = "500px"), .tabla_ui(ns("ranking")),
                     shiny::h2("Evolución mensual"), shiny::plotOutput(ns("serie"), height = "500px"),
                     .tabla_ui(ns("evolucion")), shiny::h2("País de destino"), .tabla_ui(ns("paises")),
                     shiny::h2("Crecimiento entre mitades del período"), .tabla_ui(ns("crecimiento")))),
               panel("Market Basket", shiny::tagList(
                 div(class = "crm-form-grid",
                     shiny::selectInput(ns("cesta"), "Unidad de cesta", c("Pedido" = "pedido",
                       "Cliente y mes" = "mes", "Cliente y período" = "periodo")),
                     shiny::numericInput(ns("soporte"), "Soporte mínimo", .03, min = 0, max = 1, step = .01),
                     shiny::numericInput(ns("confianza"), "Confianza mínima", .5, min = 0, max = 1, step = .05),
                     shiny::numericInput(ns("lift"), "Lift mínimo", 1, min = 0, step = .1),
                     shiny::numericInput(ns("minimo"), "Cestas mínimas con el conjunto", 3, min = 1, step = 1),
                     shiny::numericInput(ns("longitud"), "Longitud máxima", 3, min = 2, max = 5, step = 1),
                     shiny::checkboxInput(ns("temporal"), "Validación temporal (pedido o cliente–mes)"),
                     shiny::checkboxInput(ns("estadistica"), "Fisher con ajuste BH", TRUE)),
                 .boton(ns("calcular"), "Calcular Market Basket", "calculator"), shiny::uiOutput(ns("evidencia")),
                 shiny::h2("Reglas de asociación"), .tabla_ui(ns("reglas")),
                 bs4Dash::tabsetPanel(panel("Ítems", .tabla_ui(ns("items"))),
                                    panel("Conjuntos", .tabla_ui(ns("conjuntos"))),
                                    panel("Combinaciones",
                                          shiny::tagList(shiny::plotOutput(ns("matriz"), height = "500px"),
                                                         .tabla_ui(ns("pares")))),
                                    panel("Tamaños de cesta", .tabla_ui(ns("tamanos")))),
                 shiny::h2("Recomendaciones de cross-selling"),
                 .selector_clientes(ns("cliente_basket"), "Cliente para recomendaciones"),
                 .tabla_ui(ns("recomendaciones")), shiny::uiOutput(ns("aceptacion")))),
               panel("Relaciones comerciales", shiny::tagList(shiny::h2("Cruce observado marca × nombre comercial"),
                     .tabla_ui(ns("relaciones")), shiny::h2("Operaciones observadas"),
                     .tabla_ui(ns("traza")), shiny::uiOutput(ns("seleccionar_item")),
                     .tabla_ui(ns("compradores")), shiny::h2("Penetración y cambios por cliente"),
                     .tabla_ui(ns("penetracion")))),
               panel("Calidad y homologación futura", shiny::tagList(.tabla_ui(ns("calidad")),
                     shiny::h2("Catálogo por código de origen"), .tabla_ui(ns("catalogo")),
                     .nota("La relación observada entre marca y nombre comercial no establece una jerarquía.")))
             ),
             div(class = "crm-panel", shiny::selectInput(ns("descarga_tipo"), "Resultado para descargar",
                 c("Ranking" = "ranking", "Evolución" = "evolucion", "Países" = "paises",
                   "Relaciones" = "relaciones", "Compradores" = "compradores", "Penetración" = "penetracion",
                   "Crecimiento" = "crecimiento", "Operaciones" = "traza", "Calidad" = "calidad",
                   "Reglas" = "reglas", "Ítems" = "items", "Conjuntos" = "conjuntos",
                   "Combinaciones" = "pares", "Tamaños" = "tamanos", "Recomendaciones" = "recomendaciones")),
                 shiny::uiOutput(ns("descarga_ui"))))
}

.HoyProductos <- function() as.Date(Sys.time(), tz = "America/Bogota")

.prod_calidad <- function(datos, catalogo) {
  duplicados <- function(x) sum(duplicated(x[!is.na(x) & nzchar(x)]))
  lote <- !is.na(datos$lote)
  sin_marca <- is.na(datos$marca)
  sin_nombre <- is.na(datos$nombre)
  sin_item <- is.na(datos$item)
  data.frame(Validación = c("Pedidos sin marca", "Pedidos sin nombre comercial", "Lotes con atributos faltantes",
                            "Filas con código sin descripción", "Códigos con descripciones distintas",
                            "Descripciones repetidas en distintos códigos", "Conflictos entre línea y lote",
                            "Sacos sin atribución", "Pedidos sin PerCod", "Líneas con sacos desconocidos"),
             Resultado = c(.prod_contar(datos$pedido_id[sin_marca]), .prod_contar(datos$pedido_id[sin_nombre]),
                           if (any(lote)) .prod_contar(datos$lote[lote & (sin_marca | sin_nombre)]) else NA_real_,
                           sum(!sin_item & is.na(datos$descripcion)), sum(catalogo$conflicto),
                           duplicados(catalogo$nombre), if (any(lote)) sum(datos$conflicto) else NA_real_,
                           .dp_suma(datos$sacos[sin_item]), .prod_contar(datos$pedido_id[is.na(datos$PerCod)]),
                           sum(is.na(datos$sacos))))
}

.prod_compradores <- function(datos, historico) {
  resumen <- .prod_resumen(datos, c("PerCod", "pais"))
  grupos <- split(historico, historico$PerCod)
  salidas <- lapply(grupos, function(d) {
    fechas <- d$fecha[!is.na(d$fecha)]
    data.frame(PerCod = d$PerCod[1L], Primero = if (length(fechas)) min(fechas) else as.Date(NA_character_),
               Ultimo = if (length(fechas)) max(fechas) else as.Date(NA_character_),
               SacosHistoricos = .dp_suma(d$sacos))
  })
  if (!length(salidas)) return(resumen)
  merge(resumen, do.call(rbind, salidas), by = "PerCod", all.x = TRUE, sort = FALSE)
}

ProductosServer <- function(id, publicaciones, filtros, raiz, al_aceptar) {
  shiny::moduleServer(id, function(input, output, session) {
    pendiente <- shiny::reactiveVal(NULL)
    hechos <- shiny::reactive({
      pub <- publicaciones()$operacional
      preparado <- pub$tablas$hechos_productos
      if (is.data.frame(preparado)) return(preparado)
      tryCatch(.prod_hechos(pub), error = identity)
    })
    usar <- function() {
      d <- hechos()
      shiny::req(!is.null(d), !inherits(d, "error"))
      d
    }
    base <- shiny::reactive({
      d <- usar()
      clientes <- publicaciones()$operacional$tablas$clientes
      d$pais_empresa <- clientes$codigo_pais[match(d$PerCod, clientes$PerCod)]
      if (length(filtros()$pais)) d <- d[d$pais_empresa %in% filtros()$pais, , drop = FALSE]
      if (length(filtros()$embarque)) {
        pedidos <- publicaciones()$operacional$tablas$pedidos
        ids <- pedidos$pedido_id[pedidos$periodo_cabecera %in% filtros()$embarque]
        d <- d[d$pedido_id %in% ids, , drop = FALSE]
      }
      global <- filtros()$periodo
      if (length(global) == 2L) d <- d[which(d$fecha >= global[1L] & d$fecha <= global[2L]), , drop = FALSE]
      if (length(input$comercial)) d <- d[d$comercial %in% input$comercial, , drop = FALSE]
      .prod_dimension(d, input$dimension)
    })
    periodo <- shiny::reactive({
      p <- input$periodo
      shiny::req(length(p) == 2L, !anyNA(p), p[1L] <= p[2L])
      p
    })
    pedidos <- shiny::reactive(.prod_filtrar(base(), filtros(), periodo()))
    ejecucion <- shiny::reactive({
      usar_movimiento <- input$medida == "Embarcado"
      d <- if (usar_movimiento) .prod_filtrar(base(), filtros(), periodo(), "fecha_embarque") else pedidos()
      d$periodo <- format(if (usar_movimiento) d$fecha_embarque else d$fecha, "%Y-%m")
      d
    })
    catalogo_corte <- shiny::reactive({
      pub <- publicaciones()$operacional
      nombre <- if (input$dimension == "marca") "dim_marcas" else "dim_nombres_comerciales"
      preparado <- pub$tablas[[nombre]]
      if (is.data.frame(preparado)) return(preparado)
      .prod_catalogo(.prod_dimension(usar(), input$dimension), pub$manifiesto$procedencia$corte)
    })
    catalogo <- shiny::reactive({
      cat <- catalogo_corte()
      cat[cat$item %in% pedidos()$item, , drop = FALSE]
    })
    ranking <- shiny::reactive({
      d <- ejecucion()
      d <- d[!is.na(d$item), , drop = FALSE]
      r <- .prod_resumen(d, c("item", "CiaCod", "codigo"))
      r <- r[order(-r[[input$medida]], r$item, na.last = TRUE), , drop = FALSE]
      volumen <- if (input$medida == "Embarcado") r$Embarcado else r$Sacos
      total <- .dp_suma(volumen)
      r$Participacion <- if (is.finite(total) && total > 0) volumen / total else rep(NA_real_, nrow(r))
      pareto <- order(-volumen)
      if (input$medida == "Embarcado") r$Sacos <- rep(NA_real_, nrow(r))
      r$Acumulada <- rep(NA_real_, nrow(r))
      r$Acumulada[pareto] <- cumsum(r$Participacion[pareto])
      cat <- catalogo_corte()
      r$Nombre <- cat$nombre[match(r$item, cat$item)]
      shiny::req(length(input$top) == 1L, is.finite(input$top), input$top >= 1)
      head(r, min(50L, as.integer(input$top)))
    })
    evolucion <- shiny::reactive({
      d <- ejecucion()
      d$grupo <- ifelse(is.na(d$item), "Sin atribución", ifelse(d$item %in% ranking()$item, d$item, "Otros"))
      .prod_resumen(d[!is.na(d$periodo), ], c("periodo", "grupo"))
    })
    paises <- shiny::reactive({
      d <- ejecucion()
      d$pais[is.na(d$pais)] <- "Sin código"
      r <- .prod_resumen(d, c("pais", "item"))
      volumen <- if (input$medida == "Embarcado") r$Embarcado else r$Sacos
      identificados <- !is.na(r$item)
      totales <- .dp_agregar(r$pais[identificados], volumen[identificados], r$pais)
      r$Participacion <- ifelse(identificados & is.finite(totales) & totales > 0, volumen / totales, NA_real_)
      r$VariedadPais <- as.integer(table(r$pais[identificados])[r$pais])
      r[order(r$pais, -r[[input$medida]], na.last = TRUE), , drop = FALSE]
    })
    relaciones <- shiny::reactive({
      d <- pedidos()
      .prod_resumen(d[!is.na(d$marca) & !is.na(d$nombre), , drop = FALSE], c("CiaCod", "marca", "nombre"))
    })
    traza <- shiny::reactive({
      d <- pedidos()
      preparado <- publicaciones()$operacional$tablas$relacion_marca_nombre
      if (is.data.frame(preparado)) {
        ids <- .prod_id(d$pedido_id, d$PdcLin)
        return(preparado[.prod_id(preparado$pedido_id, preparado$PdcLin) %in% ids, , drop = FALSE])
      }
      campos <- c("CiaCod", "pedido_id", "PdcLin", "lote", "fecha", "fuente", "marca", "nombre", "PerCod", "sacos")
      d[!is.na(d$marca) & !is.na(d$nombre), campos, drop = FALSE]
    })
    compradores <- shiny::reactive({
      shiny::req(nzchar(input$item %||% ""))
      d <- pedidos()
      h <- usar()
      h <- .prod_dimension(h, input$dimension)
      .prod_compradores(d[which(d$item == input$item), ], h[which(h$item == input$item), ])
    })
    penetracion <- shiny::reactive({
      d <- pedidos()
      d <- d[!is.na(d$item) & !is.na(d$PerCod), , drop = FALSE]
      mitad <- periodo()[1L] + floor(as.numeric(diff(periodo())) / 2)
      dplyr::summarise(dplyr::group_by(d, PerCod),
                       Items = .prod_contar(item), ItemsInicial = .prod_contar(item[fecha <= mitad]),
                       ItemsFinal = .prod_contar(item[fecha > mitad]),
                       DejaronDeAparecer = length(setdiff(unique(item[fecha <= mitad]), unique(item[fecha > mitad]))),
                       SacosInicial = .dp_suma(sacos[fecha <= mitad]), SacosFinal = .dp_suma(sacos[fecha > mitad]),
                       VariacionSacos = SacosFinal - SacosInicial, .groups = "drop")
    })

    muestra <- shiny::reactive({
      d <- .prod_filtrar(base(), filtros(), periodo(), comparable = TRUE)
      if (isTRUE(input$temporal) && identical(input$cesta, "periodo")) {
        stop("Use pedido o cliente y mes para una validación temporal sin traslape.")
      }
      .prod_cestas(d, input$cesta)
    })
    analisis <- shiny::eventReactive(input$calcular, {
      tryCatch({
        inputs <- c(input$soporte, input$minimo, input$longitud, input$confianza, input$lift)
        if (length(inputs) != 5L || any(!is.finite(inputs)) || input$minimo < 1 ||
            input$minimo != floor(input$minimo) || !input$longitud %in% 2:5 ||
            input$soporte < 0 || input$soporte > 1 || input$confianza < 0 || input$confianza > 1 || input$lift < 0) {
          stop("Revise los parámetros de Market Basket.")
        }
        cestas <- muestra()
        if (length(cestas$ids) < input$minimo) stop("Cestas insuficientes para el mínimo elegido.")
        resultado <- .prod_analizar_cestas(cestas, input$soporte, input$minimo, input$longitud, input$confianza,
                                            input$lift, input$temporal, input$estadistica)
        d <- .prod_filtrar(base(), filtros(), periodo(), comparable = TRUE)
        resultado$pedidos <- c(cestas$pedidos, .prod_contar(d$pedido_id))
        resultado$clientes <- c(cestas$clientes, .prod_contar(d$PerCod))
        mapa <- .prod_catalogo(d, publicaciones()$operacional$manifiesto$procedencia$corte)
        resultado$items$Codigo <- mapa$codigo[match(resultado$items$Item, mapa$item)]
        resultado$items$Nombre <- mapa$nombre[match(resultado$items$Item, mapa$item)]
        resultado$periodo <- periodo()
        resultado$dimension <- input$dimension
        resultado$filtros <- filtros()[c("destino", "pais", "estado", "periodo", "creador", "embarque")]
        resultado$comercial <- input$comercial
        resultado$parametros <- c(input$cesta, input$soporte, input$minimo, input$longitud,
                                  input$confianza, input$lift, input$temporal, input$estadistica)
        resultado$generacion <- publicaciones()$operacional$manifiesto$id
        resultado
      }, error = identity)
    })
    vigente <- shiny::reactive({
      a <- analisis()
      if (inherits(a, "error")) return(a)
      if (!identical(a$dimension, input$dimension) || !identical(a$periodo, periodo()) ||
          !identical(a$generacion, publicaciones()$operacional$manifiesto$id) ||
          !identical(a$filtros, filtros()[c("destino", "pais", "estado", "periodo", "creador", "embarque")]) ||
          !identical(a$comercial, input$comercial) ||
          !identical(a$parametros, c(input$cesta, input$soporte, input$minimo, input$longitud,
                                    input$confianza, input$lift, input$temporal, input$estadistica))) {
        return(simpleError("Vuelva a calcular para este corte, período y dimensión."))
      }
      a
    })
    recomendaciones <- shiny::reactive({
      a <- vigente()
      if (inherits(a, "error") || !nzchar(input$cliente_basket %||% "")) return(NULL)
      h <- .prod_dimension(usar(), input$dimension)
      h <- h[which(h$fecha >= a$periodo[1L] & h$fecha <= a$periodo[2L]), , drop = FALSE]
      .prod_recomendar(a, h, input$cliente_basket, a$dimension, a$periodo)
    })

    salidas <- list(ranking = ranking, evolucion = evolucion, paises = paises, relaciones = relaciones,
                    compradores = compradores, penetracion = penetracion, catalogo = catalogo, traza = traza,
                    crecimiento = shiny::reactive(.prod_crecimiento(pedidos(), periodo())),
                    calidad = shiny::reactive(.prod_calidad(pedidos(), catalogo())), recomendaciones = recomendaciones)
    for (nombre in c("reglas", "items", "conjuntos", "pares", "tamanos")) local({
      objeto <- nombre
      salidas[[objeto]] <<- shiny::reactive({
        a <- vigente()
        if (!inherits(a, "error")) a[[objeto]]
      })
    })
    for (nombre in names(salidas)) local({
      id_tabla <- nombre
      .tabla_server(id_tabla, salidas[[id_tabla]], output)
    })
    output$estado <- shiny::renderUI({
      d <- hechos()
      if (is.null(d)) return(.nota("Se requiere un corte operacional con pedidos y líneas.", "muted"))
      if (inherits(d, "error")) return(.nota(conditionMessage(d), "error"))
      if (!nrow(pedidos())) return(.nota("Sin líneas 20000 para el período y filtros activos.", "empty"))
      if (all(is.na(d$embarcado)) || all(is.na(d$fcc))) {
        .nota("La ejecución y el margen requieren embarques y asignaciones FCC conciliados por lote.", "muted")
      }
    })
    valores_kpi <- shiny::reactive({
      d <- pedidos()
      e <- .prod_filtrar(base(), filtros(), periodo(), "fecha_embarque")
      r <- .prod_resumen(d, character())
      sacos_e <- if (nrow(e) && any(!is.na(e$embarcado))) sum(e$embarcado, na.rm = TRUE) else NA_real_
      valores <- c("Marcas" = .prod_contar(.prod_id(d$CiaCod, d$marca)),
                   "Nombres comerciales" = .prod_contar(.prod_id(d$CiaCod, d$nombre)),
                   "Clientes" = r$Clientes, "Pedidos" = r$Pedidos, "Sacos pedidos" = r$Sacos,
                   "Sin atribución" = .dp_suma(d$sacos[is.na(d$item)]),
                   "Sacos embarcados" = sacos_e, "MNFCC COP/kg" = r$MNFCC, "Peso FCC (kg)" = r$PesoFCC,
                   "Lotes con FCC" = if (all(is.na(d$lote))) NA_real_ else .prod_contar(d$lote[is.finite(d$fcc)]),
                   "Concentración top" = if (any(is.finite(ranking()$Participacion))) {
                     .dp_suma(ranking()$Participacion)
                   } else NA_real_)
      valores
    })
    output$kpis <- shiny::renderUI({
      div(class = "crm-kpis", lapply(seq_along(.prod_kpis), function(i) {
        .kpi_ui(session$ns(paste0("indicador_", i)), .prod_kpis[i])
      }))
    })
    for (i in seq_along(.prod_kpis)) local({
      posicion <- i
      .kpi_server(paste0("indicador_", posicion), shiny::reactive({
        valor <- valores_kpi()[posicion]
        if (.prod_kpis[posicion] == "Concentración top" && is.finite(valor)) {
          paste0(.formato_cantidad(100 * valor), "%")
        } else .formato_cantidad(valor)
      }), output)
    })
    output$pareto <- shiny::renderPlot({
      d <- ranking()
      y <- d[[input$medida]]
      shiny::validate(shiny::need(any(is.finite(y)), "Sin valores verificables para esta medida."))
      graphics::par(mar = c(8, 4, 3, 4))
      x <- graphics::barplot(y, names.arg = d$codigo, col = "#DA291C", las = 2L, cex.names = .8)
      if (input$medida %in% c("Sacos", "Embarcado") && any(is.finite(d$Acumulada))) {
        graphics::par(new = TRUE)
        graphics::plot(x, d$Acumulada, type = "b", ylim = c(0, 1), axes = FALSE, ann = FALSE, col = "#37545A")
        graphics::axis(4, at = seq(0, 1, .2), labels = paste0(seq(0, 100, 20), "%"), las = 1)
      }
    })
    output$serie <- shiny::renderPlot({
      d <- evolucion()
      d <- d[is.finite(d[[input$medida]]), , drop = FALSE]
      shiny::validate(shiny::need(nrow(d), "Sin series verificables para esta medida."))
      y <- stats::xtabs(stats::reformulate(c("grupo", "periodo"), input$medida), data = d)
      graphics::par(mar = c(7, 4, 2, 1))
      graphics::barplot(y, beside = TRUE, las = 2, col = .prod_colores(nrow(y)), cex.names = .75)
    })
    output$matriz <- shiny::renderPlot({
      a <- vigente()
      shiny::validate(shiny::need(!inherits(a, "error") && nrow(a$matriz) > 1L, "Sin pares para la selección."))
      m <- a$matriz
      eje <- seq_len(nrow(m))
      graphics::par(mar = c(8, 8, 2, 1))
      graphics::image(eje, eje, m, axes = FALSE, xlab = "", ylab = "",
                      col = grDevices::hcl.colors(24L, "Reds", rev = TRUE))
      graphics::axis(1L, eje, rownames(m), las = 2L, cex.axis = .75)
      graphics::axis(2L, eje, colnames(m), las = 2L, cex.axis = .75)
      graphics::text(rep(eje, times = length(eje)), rep(eje, each = length(eje)), as.vector(m), cex = .7)
    })
    output$evidencia <- shiny::renderUI({
      if (!shiny::isTruthy(input$calcular)) return(.nota("Calcule las cestas para el período seleccionado."))
      a <- vigente()
      if (inherits(a, "error")) return(.nota(conditionMessage(a), "warning"))
      dobles <- sum(a$tamanos$Cestas[a$tamanos$Items > 1L])
      .nota(paste("Modelo:", a$cestas_modelo, "cestas · Validación:", a$cestas_validacion,
                  "· Múltiples ítems:", dobles, "de", sum(a$tamanos$Cestas),
                  "· Pedidos cubiertos:", paste(a$pedidos, collapse = "/"),
                  "· Clientes cubiertos:", paste(a$clientes, collapse = "/"),
                  "· Período:", paste(a$periodo, collapse = " a ")))
    })
    output$filtro_comercial <- shiny::renderUI({
      d <- usar()
      opciones <- sort(unique(d$comercial[!is.na(d$comercial)]))
      if (!length(opciones)) return(.nota("Comercial: pendiente de asignación verificable.", "muted"))
      shiny::selectInput(session$ns("comercial"), "Comercial", opciones, multiple = TRUE)
    })
    output$seleccionar_item <- shiny::renderUI({
      c <- catalogo()
      opciones <- stats::setNames(c$item, paste(c$CiaCod, c$codigo, c$nombre, sep = " · "))
      shiny::selectizeInput(session$ns("item"), "Consultar compradores", c("Seleccione…" = "", opciones))
    })
    shiny::observeEvent(publicaciones()$operacional, {
      clientes <- .tabla_fuente(publicaciones(), "clientes")
      .actualizar_selector_clientes(session, "cliente_basket", clientes, input$cliente_basket)
    }, ignoreNULL = FALSE)
    output$aceptacion <- shiny::renderUI({
      r <- recomendaciones()
      if (is.null(r) || !nrow(r)) return(NULL)
      if (!.prod_autorizado(session$user)) return(.nota("La aceptación requiere un usuario autorizado.", "muted"))
      shiny::tagList(shiny::selectInput(session$ns("sugerida"), "Ítem sugerido", r$sugerido),
                     .boton(session$ns("aceptar"), "Revisar y aceptar", "check"))
    })
    shiny::observeEvent(input$aceptar, {
      shiny::req(.prod_autorizado(session$user))
      r <- recomendaciones()
      fila <- r[which(r$sugerido == input$sugerida), , drop = FALSE]
      shiny::req(nrow(fila) == 1L)
      pendiente(list(fila = fila, analisis = vigente()))
      shiny::showModal(shiny::modalDialog(title = "Aceptar oportunidad de cross-selling",
        shiny::tags$p(paste("PerCod", fila$PerCod, "· Ítem", fila$sugerido, "· Responsable", session$user)),
        shiny::textInput(session$ns("motivo"), "Motivo de aceptación"),
        footer = shiny::tagList(shiny::modalButton("Cancelar"),
                                 .boton(session$ns("confirmar"), "Confirmar aceptación", "check"))))
    })
    shiny::observeEvent(input$confirmar, {
      solicitud <- pendiente()
      shiny::req(!is.null(solicitud), nzchar(trimws(input$motivo %||% "")),
                 identical(solicitud$analisis, vigente()), identical(solicitud$fila$PerCod, input$cliente_basket))
      fila <- solicitud$fila
      tryCatch({
        .prod_aceptar(raiz, fila, session$user, trimws(input$motivo))
        pendiente(NULL)
        shiny::removeModal()
        al_aceptar()
        shiny::showNotification("Oportunidad registrada en Gestión Comercial.", type = "message")
      }, error = function(e) shiny::showNotification(conditionMessage(e), type = "error"))
    })
    output$descarga_ui <- shiny::renderUI({
      shiny::req(input$descarga_tipo %in% names(salidas))
      datos <- salidas[[input$descarga_tipo]]()
      if (!is.null(datos) && nrow(datos)) shiny::downloadButton(session$ns("descarga"), "Descargar CSV")
    })
    output$descarga <- shiny::downloadHandler(
      filename = function() paste0("productos-", input$dimension, "-", input$descarga_tipo, ".csv"),
      content = function(ruta) {
        shiny::req(input$descarga_tipo %in% names(salidas))
        d <- salidas[[input$descarga_tipo]]()
        shiny::req(!is.null(d), nrow(d))
        d[] <- lapply(d, function(x) {
          if (is.character(x)) ifelse(grepl("^[[:space:]]*[=+@-]", x), paste0("'", x), x) else x
        })
        utils::write.csv(d, ruta, row.names = FALSE, na = "", fileEncoding = "UTF-8")
      })
  })
}
