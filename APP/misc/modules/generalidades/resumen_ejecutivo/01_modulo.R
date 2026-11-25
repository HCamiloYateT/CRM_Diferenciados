# Resumen ejecutivo ----

.resumen_caja <- function(titulo, icono, detalle, destino, formato = "entero") {
  list(titulo = titulo, icono = icono, detalle = detalle, destino = destino, formato = formato)
}

.resumen_secciones <- list(
  "Generalidades" = list(icono = "clipboard-list", cajas = list(
    pedidos = .resumen_caja("Pedidos creados", "file-circle-check", "Pedidos creados en el período.", "pedidos"),
    sacos = .resumen_caja("Sacos pedidos", "weight-hanging", "Volumen de los pedidos del período.", "pedidos")
  )),
  "Gestión comercial" = list(icono = "briefcase", cajas = list(
    presupuesto_sacos = .resumen_caja("Cumplimiento de sacos", "weight-hanging",
      "La meta de sacos está en cero. No hay un porcentaje de cumplimiento.", "presupuesto_cero", "porcentaje"),
    presupuesto_margen = .resumen_caja("Cumplimiento de margen", "money-bill-trend-up",
      "La meta de margen está en cero. No hay un porcentaje de cumplimiento.", "presupuesto_cero", "porcentaje"),
    oportunidades = .resumen_caja("Oportunidades activas", "bullseye",
      "Oportunidades abiertas para los clientes seleccionados.", "oportunidades"),
    tareas = .resumen_caja("Tareas pendientes", "tasks",
      "Tareas abiertas y sin archivar visibles para la sesión.", "notas")
  )),
  "Prospección" = list(icono = "filter", cajas = list(
    contactos = .resumen_caja("Contactos", "address-book", "Contactos de los clientes seleccionados.", "contactos"),
    leads = .resumen_caja("Leads", "bullseye", "Registros en etapa Lead.", "leads"),
    clientes = .resumen_caja("Clientes", "users", "Clientes con al menos un pedido en el historial.",
                             "prospectos_clientes"),
    descartados = .resumen_caja("Descartados", "ban", "Registros descartados en prospección.", "descartados")
  )),
  "Cohortes de clientes" = list(icono = "chart-gantt", cajas = list(
    nuevos = .resumen_caja("Clientes nuevos en el período", "people-arrows",
      "Clientes cuyo primer pedido observado se creó en el período.", "cohortes")
  )),
  "Muestras" = list(icono = "flask", cajas = list(
    muestras = .resumen_caja("Muestras aprobadas sin pedido", "flask",
      "Muestras aprobadas pendientes de vincular a un pedido.", "seguimiento")
  ))
)

.ResumenEjecutivoUI <- function(id) {
  ns <- NS(id)
  bloques <- lapply(names(.resumen_secciones), function(titulo) {
    seccion <- .resumen_secciones[[titulo]]
    tagList(
      div(class = "re-seccion d-flex align-items-center border-bottom mt-3 mb-2 pb-1",
          icon(seccion$icono, class = "text-danger mr-2"),
          tags$span(class = "small font-weight-bold text-uppercase text-secondary", titulo)),
      fluidRow(lapply(names(seccion$cajas), function(clave) {
        caja <- seccion$cajas[[clave]]
        contenido <- if (.disponible_api("racafeModulos", "cajaValor_ui")) {
          racafeModulos::cajaValor_ui(ns(clave), texto = caja$titulo, icono = caja$icono, footer = caja$detalle,
            compact = TRUE, mostrar_boton = TRUE, label_boton = NULL, tooltip = paste("Abrir", caja$titulo),
            button_size = "icon", footer_position = "outside")
        } else {
          tagList(div(class = "crm-panel border-left border-danger shadow-sm p-2 mb-1",
                      icon(caja$icono, class = "text-danger mr-1"), tags$span(caja$titulo),
                      tags$strong(class = "d-block", textOutput(ns(clave), inline = TRUE)),
                      div(class = "text-right",
                          racafeForms::racafeButton(paste0("ir_", clave), NULL, icon = "arrow-right",
                                                    title = paste("Abrir", caja$titulo), size = "xxs", ns = ns))),
                  tags$small(caja$detalle))
        }
        column(3, div(class = "re-columna mb-3", contenido))
      }))
    )
  })
  div(class = "resumen-ejecutivo", bloques)
}

.ResumenEjecutivo <- function(id, publicaciones, operativo, filtros, navegar) {
  moduleServer(id, function(input, output, session) {
    identidad <- .crm_identidad_notas(session)
    valores <- reactive({
      pub <- publicaciones()
      operacion <- operativo()
      seleccion <- filtros()
      tabla <- function(nombre) .vista_datos(pub, operacion, nombre, seleccion)
      contar <- function(d) if (is.null(d)) NA_integer_ else nrow(d)
      tablas <- pub$crm$tablas
      oportunidades <- tabla("oportunidades")
      empresas <- tabla("empresas")
      clientes <- tabla("clientes_comerciales")
      muestras <- tabla("muestras")
      pendientes <- .notas_header_registros(tablas, identidad)
      c(pedidos = contar(operacion$pedidos),
        sacos = if (!is.null(operacion$pedidos)) .dp_suma(operacion$pedidos$sacos) else NA_real_,
        presupuesto_sacos = NA_real_, presupuesto_margen = NA_real_,
        oportunidades = if (is.null(oportunidades)) NA_integer_ else sum(oportunidades$estado == "ABIERTA", na.rm = TRUE),
        tareas = if (is.null(tablas)) NA_integer_ else sum(pendientes$Tipo == "list", na.rm = TRUE),
        contactos = contar(tabla("contactos")),
        leads = if (!"etapa" %in% names(empresas)) NA_integer_ else sum(empresas$etapa == "LEAD", na.rm = TRUE),
        clientes = if (!"tiene_pedidos" %in% names(clientes)) NA_integer_ else sum(clientes$tiene_pedidos, na.rm = TRUE),
        descartados = contar(tabla("descartados")),
        nuevos = contar(tabla("primer_pedido_cliente")),
        muestras = if (is.null(muestras)) NA_integer_ else sum(muestras$estado == "APROBADA" &
                                                                 is.na(muestras$pedido_id), na.rm = TRUE))
    })
    for (seccion in .resumen_secciones) for (id_caja in names(seccion$cajas)) local({
      clave <- id_caja
      caja <- seccion$cajas[[clave]]
      valor <- reactive(valores()[[clave]])
      if (.disponible_api("racafeModulos", "cajaValor_server")) {
        tarjeta <- racafeModulos::cajaValor_server(clave, valor_r = valor, formato = caja$formato,
                                                   empty = "—", prop = FALSE)
        observeEvent(tarjeta$click_detalle(), navegar(caja$destino), ignoreInit = TRUE)
      } else {
        output[[clave]] <- renderText(if (is.na(valor())) "—" else .formato_cantidad(valor()))
        observeEvent(input[[paste0("ir_", clave)]], navegar(caja$destino))
      }
    })
  })
}
