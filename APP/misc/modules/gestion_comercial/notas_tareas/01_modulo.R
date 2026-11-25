# Tablero de notas y tareas ----

TableroTareasUI <- function(id) {
  ns <- NS(id)
  div(class = "crm-vista crm-tareas-form",
      racafeForms::racafeForm(
        variant = "plain",
        racafeForms::racafeActionBar(left = tagAppendAttributes(
          racafeForms::racafeButton("crear", "Nueva tarea o nota", icon = "plus", ns = ns),
          `data-rf-controlbar-tab` = "Tareas y notas"
        )),
        racafeForms::racafeFormRow(
          racafeForms::racafeInputText("buscar", "Buscar", placeholder = "Título, descripción o actividad", ns = ns),
          uiOutput(ns("filtro_etiquetas")),
          racafeForms::racafeInputSelect("prioridad", "Prioridad", names(.colores_prioridad_crm),
                                         multiple = TRUE, empty_label = "Todas", ns = ns),
          racafeForms::racafeInputSelect("tipo", "Tipo", c("Todos" = "", "Nota" = "text", "Tarea" = "list"),
                                         ns = ns), columns = 4L, responsive = "fixed"
        ),
        div(class = "crm-tareas-switches",
            racafeForms::racafeInputSwitch("archivadas", "Incluir archivadas", ns = ns),
            racafeForms::racafeInputSwitch("cumplidas", "Incluir cumplidas", ns = ns)),
        .tablero_notas_ui(ns)
      ))
}

# Estructura fija del tablero ----
.tablero_notas_corporativo <- function() {
  all(vapply(c("TarjetaKanbanUI", "ColumnaKanbanUI", "TableroKanbanUI", "MenuAccionesUI"),
             function(api) .disponible_api("racafeModulos", api), logical(1)))
}

.tablero_notas_ui <- function(ns) {
  colores <- .colores_prioridad_crm
  corporativo <- .tablero_notas_corporativo()
  columnas <- lapply(names(colores), function(prioridad) {
    ancla <- tags$template(id = ns(paste0("fin_", prioridad)))
    vacia <- conditionalPanel(sprintf("output.cantidad_%s === '0'", prioridad),
                              .nota("Sin registros en esta prioridad."), ns = ns)
    cuerpo <- tagList(ancla, vacia)
    contador <- textOutput(ns(paste0("cantidad_", prioridad)), inline = TRUE)
    filtro <- uiOutput(ns(paste0("filtro_", prioridad)), inline = TRUE)
    colapsar <- tags$button(type = "button", class = "kb-collapse-toggle", `data-kb-toggle` = "true",
                            title = paste("Colapsar", prioridad), `aria-expanded` = "true", icon("chevron-down"))
    columna <- if (corporativo) {
      contenido <- racafeModulos::ColumnaKanbanUI(prioridad, cuerpo = cuerpo, cantidad = 0,
                                                  color = colores[[prioridad]], header_filled = FALSE,
                                                  acciones = tagList(filtro, colapsar))
      htmltools::tagQuery(contenido)$find(".kpb-col-count")$empty()$append(contador)$allTags()
    } else div(class = "crm-panel", h2(prioridad), contador, filtro, cuerpo)
    condicion <- sprintf("!input.prioridad || !input.prioridad.length || input.prioridad.includes('%s')", prioridad)
    conditionalPanel(condicion, columna, ns = ns, class = "kpb-slot", `data-kb-stage` = prioridad)
  })
  if (corporativo) {
    tablero <- racafeModulos::TableroKanbanUI(columnas, ns = ns)
    return(tagAppendAttributes(tablero, class = "rm-task-board-shared"))
  }
  tagList(racafeForms::racafeFormRow(selectInput(ns("registro"), "Registro", character()),
                                    uiOutput(ns("operaciones")), responsive = "fixed"),
          div(class = "crm-kpis", columnas))
}

.tablero_notas_datos <- function(tablas, usuario, grupos = character(), sesion_local = FALSE) {
  d <- .gestion_filas(tablas, "tareas")
  propios <- toupper(d$responsable) %in% toupper(usuario)
  if (sesion_local) propios <- propios & is.na(d$PerCod)
  admin <- !sesion_local && .crm_es_admin(usuario, grupos)
  d <- d[which((admin | propios) &
                d$tipo %in% c("NOTA", "SEGUIMIENTO")), ,
         drop = FALSE]
  prioridades <- .gestion_filas(tablas, "tarea_prioridad")
  archivo <- .gestion_filas(tablas, "tarea_archivo")
  d$Prioridad <- prioridades$prioridad[match(d$tarea_id, prioridades$tarea_id)]
  d$Prioridad[d$Prioridad %in% "ALTA"] <- "IMPORTANTE"
  d$Prioridad[!d$Prioridad %in% names(.colores_prioridad_crm)] <- "MEDIA"
  creacion <- .gestion_filas(tablas, "tarea_creacion")
  indice <- match(d$tarea_id, creacion$tarea_id)
  d$Creador <- creacion$autor[indice]
  d$Creador[is.na(indice)] <- d$responsable[is.na(indice)]
  d$Creada <- as.Date(creacion$fecha[indice], tz = "America/Bogota")
  d$Creada[is.na(indice)] <- d$fecha[is.na(indice)]
  d$Tipo <- ifelse(d$tipo == "NOTA", "text", "list")
  d$Cumplido <- d$Tipo == "list" & d$estado == "REALIZADA"
  d$Archivado <- d$tarea_id %in% archivo$tarea_id | d$estado == "CANCELADA"
  d$Vencida <- d$Tipo == "list" & !d$Cumplido & !d$Archivado &
    !is.na(d$seguimiento) & d$seguimiento < .HoyProductos()
  d[order(d$seguimiento, -as.numeric(d$fecha), na.last = TRUE), , drop = FALSE]
}

.tablero_notas_actuar <- function(raiz, id, accion, usuario, grupos = character(), sesion_local = FALSE) {
  if (!.crm_usuario_autenticado(usuario)) {
    stop("La cuenta no tiene permiso para modificar gestión.", call. = FALSE)
  }
  if (!accion %in% c("archivar", "desarchivar", "cumplir")) stop("Acción no permitida.", call. = FALSE)
  if (accion == "cumplir" && (sesion_local || !.crm_es_admin(usuario, grupos))) {
    stop("Solo un administrador puede marcar la tarea como cumplida.", call. = FALSE)
  }
  .gestion_actualizar(raiz, function(t) {
    d <- .tablero_notas_datos(t, usuario, grupos, sesion_local)
    fila <- d[d$tarea_id %in% id, , drop = FALSE]
    if (nrow(fila) != 1L) stop("El registro ya no está disponible.", call. = FALSE)
    posicion <- match(id, t$tareas$tarea_id)
    if (accion == "archivar" && !fila$Archivado) {
      t$tarea_archivo <- .gestion_upsert(t$tarea_archivo, data.frame(tarea_id = id, estado = fila$estado), "tarea_id")
    }
    if (accion == "desarchivar" && fila$Archivado) {
      t$tarea_archivo <- t$tarea_archivo[t$tarea_archivo$tarea_id != id, , drop = FALSE]
      if (fila$estado == "CANCELADA") t$tareas$estado[posicion] <- if (fila$tipo == "NOTA") "REALIZADA" else "PENDIENTE"
    }
    if (accion == "cumplir" && fila$Tipo == "list" && !fila$Cumplido && !fila$Archivado) {
      t$tareas$estado[posicion] <- "REALIZADA"
    }
    t$tareas$actualizada[posicion] <- Sys.time()
    t
  }, usuario, paste("Tablero de notas y tareas", accion), id)
}

.actividades_nota_lista <- function(datos, id, fechas = .gestion_filas(NULL, "tarea_actividad_cumplimiento")) {
  filas <- datos[datos$tarea_id %in% id, , drop = FALSE]
  filas <- filas[order(filas$orden), , drop = FALSE]
  fechas <- fechas[fechas$tarea_id %in% id, , drop = FALSE]
  list(textos = filas$actividad, cumplidas = filas$cumplida, fechas = fechas$fecha[match(filas$orden, fechas$orden)])
}

.detalle_nota_ui <- function(fila, actividades, etiquetas, comentarios) {
  fecha <- format(fila$Creada, "%d/%m/%y")
  tagList(
    racafeForms::racafeInfoItem("Creado por", fila$Creador),
    racafeForms::racafeInfoItem("El", fecha, empty = "Fecha sin registrar"),
    racafeForms::racafeInfoItem("Asignado a", fila$responsable),
    if (!is.na(fila$resultado) && nzchar(fila$resultado)) racafeForms::racafeInfoItem("Descripción", fila$resultado),
    racafeForms::racafeInfoItem("Cronología de comentarios", .comentarios_nota_ui(comentarios, fila$tarea_id),
                               empty = "Sin comentarios"),
    racafeForms::racafeInfoItem("Etiquetas", .etiquetas_crm_ui(etiquetas), empty = "Sin etiquetas"),
    if (fila$Tipo == "list") racafeForms::racafeInfoItem(
      "Actividades",
      if (length(actividades$textos)) tags$ul(class = "list-unstyled mb-0",
        lapply(seq_along(actividades$textos), function(i) {
          texto <- actividades$textos[i]
          if (!actividades$cumplidas[i]) return(tags$li(icon("circle"), " ", texto))
          fecha_cumplida <- .formato_fecha_crm(actividades$fechas[i])
          tags$li(icon("circle-check", class = "text-success"), " ", texto,
                  tags$small(class = "d-block text-muted",
                             if (is.na(fecha_cumplida)) "Fecha de cumplimiento sin registrar" else fecha_cumplida))
        })),
      empty = "Sin actividades"
    )
  )
}

.resumen_tarjeta_nota <- function(fila, actividades) {
  total <- length(actividades$textos)
  avance <- sum(actividades$cumplidas)
  color <- if (fila$Vencida) "text-danger" else if (fila$Cumplido) "text-success" else "text-muted"
  tagList(
    if (fila$Archivado) tags$span(title = "Archivada", class = "text-muted mr-2", icon("archive"), " Archivada"),
    if (fila$Cumplido) tags$span(title = "Cumplida", class = "text-success", icon("circle-check"), " Cumplida"),
    if (!is.na(fila$seguimiento)) tags$div(class = color,
      icon("calendar"), " ", .formato_fecha_crm(fila$seguimiento), if (fila$Vencida) " · Vencida"),
    if (total) div(
      tags$small(sprintf("%d de %d actividades", avance, total)),
      div(class = "progress", tags$div(class = "progress-bar bg-success", role = "progressbar",
          `aria-label` = "Actividades cumplidas", `aria-valuenow` = avance, `aria-valuemin` = 0,
          `aria-valuemax` = total, style = sprintf("width: %.2f%%;", 100 * avance / total)))
    )
  )
}

TableroTareas <- function(id, publicaciones, raiz, refrescar, editar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    pendiente <- reactiveVal(NULL)
    solo_vencidas <- reactiveVal(character())
    identidad <- .crm_identidad_notas(session)
    usuario <- identidad$usuario %||% character()
    grupos <- identidad$grupos
    admin <- !identidad$local && .crm_es_admin(usuario, grupos)
    tablas <- reactive(publicaciones()$crm$tablas)
    tarjetas <- new.env(parent = emptyenv())
    secuencia <- 0L
    corporativo <- .tablero_notas_corporativo()
    for (prioridad in names(.colores_prioridad_crm)) local({
      columna <- prioridad
      output[[paste0("filtro_", columna)]] <- renderUI({
        activa <- columna %in% solo_vencidas()
        tagAppendAttributes(
          racafeForms::racafeButton(paste0("vencidas_", columna), NULL, icon = "filter", size = "xxs",
                                    title = "Filtrar vencidas", variant = if (activa) "primary" else "secondary",
                                    ns = ns),
          `aria-label` = paste("Filtrar vencidas", columna), `aria-pressed` = tolower(as.character(activa))
        )
      })
      output[[paste0("cantidad_", columna)]] <- renderText(sum(seleccion()$Prioridad == columna))
      observeEvent(input[[paste0("vencidas_", columna)]], {
        activas <- solo_vencidas()
        solo_vencidas(if (columna %in% activas) setdiff(activas, columna) else c(activas, columna))
      })
    })
    notas <- reactive(.tablero_notas_datos(tablas(), usuario, grupos, identidad$local))
    etiquetas <- reactive(.gestion_filas(tablas(), "tarea_etiquetas"))
    lista <- reactive({
      d <- .gestion_filas(tablas(), "tarea_actividades")
      split(d, d$tarea_id)
    })
    cumplimiento <- reactive({
      d <- .gestion_filas(tablas(), "tarea_actividad_cumplimiento")
      split(d, d$tarea_id)
    })
    actividades <- function(id) {
      d <- lista()[[id]] %||% .gestion_filas(NULL, "tarea_actividades")
      fechas <- cumplimiento()[[id]] %||% .gestion_filas(NULL, "tarea_actividad_cumplimiento")
      .actividades_nota_lista(d, id, fechas)
    }
    comentarios <- reactive({
      d <- .gestion_filas(tablas(), "tarea_comentarios")
      split(d, d$tarea_id)
    })
    etiquetas_tarjeta <- reactive(split(etiquetas()$etiqueta, etiquetas()$tarea_id))
    output$filtro_etiquetas <- renderUI({
      opciones <- sort(unique(etiquetas()$etiqueta[etiquetas()$tarea_id %in% notas()$tarea_id]))
      racafeForms::racafeInputSelect("etiquetas", "Etiquetas", c("Sin etiqueta" = "__sin__", opciones),
                                     selected = isolate(input$etiquetas), multiple = TRUE, empty_label = "Todas",
                                     ns = ns)
    })
    seleccion <- reactive({
      d <- notas()
      if (!isTRUE(input$archivadas)) d <- d[!d$Archivado, , drop = FALSE]
      if (!isTRUE(input$cumplidas)) d <- d[!d$Cumplido, , drop = FALSE]
      if (length(input$prioridad)) d <- d[d$Prioridad %in% input$prioridad, , drop = FALSE]
      if (nzchar(input$tipo %||% "")) d <- d[d$Tipo %in% input$tipo, , drop = FALSE]
      d <- d[!d$Prioridad %in% solo_vencidas() | d$Vencida, , drop = FALSE]
      if (length(input$etiquetas)) {
        e <- etiquetas()
        sin_etiqueta <- if ("__sin__" %in% input$etiquetas) setdiff(d$tarea_id, e$tarea_id) else character()
        d <- d[d$tarea_id %in% c(sin_etiqueta, e$tarea_id[e$etiqueta %in% input$etiquetas]), , drop = FALSE]
      }
      buscar <- trimws(input$buscar %||% "")
      if (nzchar(buscar)) {
        texto <- vapply(d$tarea_id, function(id) paste(actividades(id)$textos, collapse = " "), character(1))
        d <- d[grepl(tolower(buscar), tolower(paste(d$asunto, d$resultado, texto)), fixed = TRUE), , drop = FALSE]
      }
      d
    })
    acciones <- function(fila) {
      if (!.crm_usuario_autenticado(usuario)) return(character())
      c(editar = "Editar",
        if (fila$Archivado) c(desarchivar = "Desarchivar") else c(archivar = "Archivar"),
        if (admin && fila$Tipo == "list" && !fila$Cumplido && !fila$Archivado) c(cumplir = "Marcar cumplida"))
    }
    tarjeta_ui <- function(fila, lista, etiquetas, comentarios) {
      resumen <- .resumen_tarjeta_nota(fila, lista)
      detalle <- .detalle_nota_ui(fila, lista, etiquetas, comentarios)
      if (!corporativo) return(div(class = "mb-2", tags$details(tags$summary(fila$asunto), detalle), resumen))
      iconos <- c(editar = "pen", archivar = "archive", desarchivar = "box-open", cumplir = "check")
      config <- lapply(iconos, function(icono) list(icono = icono, color = "#495057"))
      opciones <- acciones(fila)
      menu <- if (length(opciones)) racafeModulos::MenuAccionesUI(fila$tarea_id, opciones, config = config, ns = ns,
                                            id_evento = "AccionTarea",
                                            id_grupo = ns(paste0("acciones_tarea_", fila$tarea_id)))
      responsable <- fila$responsable
      if (is.na(responsable) || !nzchar(responsable)) responsable <- "Sin responsable"
      tarjeta <- racafeModulos::TarjetaKanbanUI(
        fila$asunto, responsables = racafeForms::racafeStatusBadge(responsable, size = "xs"),
        contexto = if (fila$Tipo == "list") "TAREA" else "NOTA",
        cuerpo = detalle, menu = menu, color = .colores_prioridad_crm[[fila$Prioridad]]
      )
      htmltools::tagAppendChild(tarjeta, resumen)
    }
    montar_tarjeta <- function(clave, prioridad, ancla) {
      secuencia <<- secuencia + 1L
      id <- paste0("tarjeta_", secuencia)
      contenido <- reactiveVal(NULL)
      output[[id]] <- renderUI({
        datos <- contenido()
        req(datos)
        tarjeta_ui(datos$fila, datos$lista, datos$etiquetas, datos$comentarios)
      })
      insertUI(ancla, "beforeBegin", uiOutput(ns(id)), immediate = TRUE)
      tarjetas[[clave]] <- list(id = id, contenido = contenido, prioridad = prioridad)
    }
    observe({
      d <- seleccion()
      if (!corporativo) {
        updateSelectInput(session, "registro", choices = setNames(d$tarea_id, d$asunto),
                          selected = intersect(isolate(input$registro), d$tarea_id))
      }
      for (clave in ls(tarjetas, all.names = TRUE)) {
        tarjeta <- tarjetas[[clave]]
        if (identical(d$Prioridad[match(clave, d$tarea_id)], tarjeta$prioridad)) next
        removeUI(paste0("#", ns(tarjeta$id)), immediate = TRUE)
        output[[tarjeta$id]] <- NULL
        rm(list = clave, envir = tarjetas)
      }
      for (prioridad in names(.colores_prioridad_crm)) {
        filas <- d[d$Prioridad == prioridad, , drop = FALSE]
        ancla <- paste0("#", ns(paste0("fin_", prioridad)))
        for (i in rev(seq_len(nrow(filas)))) {
          fila <- filas[i, c("tarea_id", "asunto", "resultado", "responsable", "seguimiento",
                             "Creador", "Creada", "Tipo", "Prioridad", "Cumplido", "Archivado", "Vencida"), drop = FALSE]
          rownames(fila) <- NULL
          clave <- fila$tarea_id
          if (is.null(tarjetas[[clave]])) montar_tarjeta(clave, prioridad, ancla)
          tarjeta <- tarjetas[[clave]]
          historia <- comentarios()[[clave]] %||% .gestion_filas(NULL, "tarea_comentarios")
          rownames(historia) <- NULL
          tarjeta$contenido(list(fila = fila, lista = actividades(clave), etiquetas = etiquetas_tarjeta()[[clave]],
                                 comentarios = historia))
          ancla <- paste0("#", ns(tarjeta$id))
        }
      }
    })
    output$operaciones <- renderUI({
      d <- seleccion()
      fila <- d[d$tarea_id %in% input$registro, , drop = FALSE]
      if (!nrow(fila)) return(NULL)
      opciones <- acciones(fila)
      if (!length(opciones)) return(NULL)
      tagList(selectInput(ns("operacion"), "Acción", setNames(names(opciones), opciones)),
              .boton(ns("ejecutar"), "Abrir acción", "arrow-right"))
    })
    actuar <- function(evento) {
      d <- seleccion()
      fila <- d[d$tarea_id %in% evento$codigo, , drop = FALSE]
      req(nrow(fila) == 1L, evento$accion %in% names(acciones(fila)))
      if (evento$accion == "editar") return(editar(fila))
      pendiente(list(id = fila$tarea_id, accion = evento$accion))
      titulo <- switch(evento$accion, archivar = "¿Archivar este registro?",
                       desarchivar = "¿Desarchivar este registro?", cumplir = "¿Marcar la tarea como cumplida?")
      racafeForms::racafeConfirm(session, "confirmar", titulo, fila$asunto, ns = ns)
    }
    observeEvent(input$AccionTarea, actuar(input$AccionTarea))
    observeEvent(input$ejecutar, actuar(list(codigo = input$registro, accion = input$operacion)))
    observeEvent(input$confirmar, {
      evento <- pendiente()
      pendiente(NULL)
      req(isTRUE(input$confirmar), !is.null(evento))
      tryCatch({
        .tablero_notas_actuar(raiz, evento$id, evento$accion, usuario, grupos, identidad$local)
        refrescar()
        mensaje <- switch(evento$accion, archivar = "Registro archivado", desarchivar = "Registro desarchivado",
                          cumplir = "Tarea marcada como cumplida")
        racafeForms::racafeNotify(session, mensaje, type = "success")
      }, error = function(e) {
        racafeForms::racafeNotify(session, "No se pudo actualizar el registro", conditionMessage(e), type = "error")
      })
    })
  })
}
