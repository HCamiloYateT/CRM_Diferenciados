# Catálogo de asesores ----

.asesor_usuario <- function(x) toupper(trimws(as.character(x)))

.asesor_correo <- function(usuario) paste0(tolower(usuario), "@racafe.com")

.asesores_catalogo <- function(tablas) .gestion_filas(tablas, "asesores")

.asesor_nombre <- function(usuario, catalogo) {
  nombre <- catalogo$nombre[match(.asesor_usuario(usuario), catalogo$usuario)]
  nombre[is.na(nombre)] <- usuario[is.na(nombre)]
  nombre
}

.asesor_asignado <- function(tablas, creador, seleccionado = NULL) {
  if (is.null(seleccionado)) return(creador)
  elegido <- .asesor_usuario(.gestion_texto(seleccionado))
  catalogo <- .asesores_catalogo(tablas)
  encontrado <- match(elegido, catalogo$usuario)
  if (!is.na(encontrado) && isTRUE(catalogo$vigente[encontrado])) return(elegido)
  if (is.na(encontrado) && identical(elegido, .asesor_usuario(creador))) return(elegido)
  stop("Seleccione un responsable vigente.", call. = FALSE)
}

.asesores_guardar <- function(raiz, clave, nombre, correo, vigente, identidad, origen) {
  if (!.crm_es_admin(identidad$usuario, identidad$grupos)) {
    stop("Solo un administrador puede editar el catálogo.", call. = FALSE)
  }
  clave <- .asesor_usuario(.gestion_campo(clave, "usuario"))
  nombre <- .gestion_campo(nombre, "nombre del asesor")
  correo <- tolower(.gestion_campo(correo, "correo"))
  if (!grepl("^[A-Z0-9._-]+$", clave) || !grepl("^[a-z0-9._%+-]+@racafe[.]com$", correo)) {
    stop("Indique un usuario válido y un correo del dominio @racafe.com.", call. = FALSE)
  }
  .gestion_actualizar(raiz, function(tablas) {
    a <- tablas$asesores
    actual <- a[a$usuario == clave, , drop = FALSE]
    rownames(actual) <- NULL
    rownames(origen) <- NULL
    if (!identical(actual, origen)) stop("El asesor cambió en otra sesión. Vuelva a seleccionarlo.", call. = FALSE)
    fila <- data.frame(usuario = clave, nombre = nombre, correo = correo, vigente = isTRUE(vigente))
    tablas$asesores <- .gestion_upsert(a, fila, "usuario")
    tablas
  }, identidad$usuario, "Actualizar catálogo de asesores", clave)
}

.AsesoresUI <- function(id) {
  ns <- NS(id)
  racafeForms::racafePanel(title = "Asesores", uiOutput(ns("formulario")), tableOutput(ns("asesores")))
}

.AsesoresServer <- function(id, publicaciones, raiz, refrescar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    identidad <- .crm_identidad_notas(session)
    autorizado <- .crm_es_admin(identidad$usuario, identidad$grupos)
    origen <- reactiveVal(NULL)
    catalogo <- reactive(.asesores_catalogo(publicaciones()$crm$tablas))
    usuarios <- reactive({
      pedidos <- .tabla_fuente(publicaciones(), "pedidos")
      claves <- .asesor_usuario(c(pedidos$PdcUsu, catalogo()$usuario))
      sort(unique(claves[!is.na(claves) & grepl("^[A-Z0-9._-]+$", claves)]))
    })
    output$formulario <- renderUI({
      if (!autorizado) return(NULL)
      claves <- usuarios()
      racafeForms::racafeForm(
        racafeForms::racafeInputSelect("usuario", "Usuario que creó el pedido", claves,
                                       selected = intersect(isolate(input$usuario), claves), ns = ns),
        uiOutput(ns("edicion")), variant = "plain"
      )
    })
    output$edicion <- renderUI({
      clave <- .gestion_texto(input$usuario)
      req(autorizado, !is.na(clave), clave %in% usuarios())
      d <- catalogo()
      fila <- d[d$usuario == clave, , drop = FALSE]
      origen(list(clave = clave, fila = fila))
      racafeForms::racafeFormRow(
        racafeForms::racafeInputText("nombre", "Nombre", value = if (nrow(fila)) fila$nombre else clave, ns = ns),
        racafeForms::racafeInputText("correo", "Correo",
                                     value = if (nrow(fila)) fila$correo else .asesor_correo(clave), ns = ns),
        racafeForms::racafeInputSwitch("vigente", "Vigente", value = !nrow(fila) || fila$vigente, ns = ns),
        racafeForms::racafeButton("guardar", "Guardar asesor", icon = "check", ns = ns),
        columns = 4L, responsive = "auto"
      )
    })
    observeEvent(input$guardar, {
      registro <- origen()
      req(autorizado, identical(input$usuario, registro$clave), input$usuario %in% usuarios())
      tryCatch({
        .asesores_guardar(raiz, input$usuario, input$nombre, input$correo, input$vigente, identidad, registro$fila)
        refrescar()
        racafeForms::racafeNotify(session, "Asesor guardado", type = "success")
      }, error = function(e) racafeForms::racafeNotify(session, "No se pudo guardar", conditionMessage(e), type = "error"))
    })
    output$asesores <- renderTable({
      req(autorizado)
      d <- catalogo()
      data.frame(Usuario = d$usuario, Nombre = d$nombre, Correo = d$correo,
                 Vigencia = ifelse(d$vigente, "Vigente", "Inactivo"))
    })
  })
}
