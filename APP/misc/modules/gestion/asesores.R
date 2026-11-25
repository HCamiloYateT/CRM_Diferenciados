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

.asesores_filas <- function(pedidos, catalogo) {
  creadores <- .asesor_usuario(pedidos$PdcUsu)
  claves <- unique(c(creadores, catalogo$usuario))
  claves <- claves[!is.na(claves) & grepl("^[A-Z0-9._-]+$", claves)]
  i <- match(claves, catalogo$usuario)
  fechas <- pedidos$fecha_creacion %||% rep(as.Date(NA), length(creadores))
  recientes <- order(fechas, decreasing = TRUE, na.last = NA)
  ultima <- fechas[recientes[match(claves, creadores[recientes])]]
  filas <- data.frame(usuario = claves, nombre = .asesor_nombre(claves, catalogo),
                      correo = ifelse(is.na(i), .asesor_correo(claves), catalogo$correo[i]),
                      vigente = is.na(i) | catalogo$vigente[i], ultima_creacion = ultima)
  filas[order(-as.numeric(filas$ultima_creacion), filas$usuario, na.last = TRUE), , drop = FALSE]
}

.AsesoresUI <- function(id) {
  racafeForms::racafePanel(title = "Asesores",
                          racafeModulos::TablaVancouverUI(NS(id)("asesores"), descargable = FALSE))
}

.AsesoresServer <- function(id, publicaciones, raiz, refrescar) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    identidad <- .crm_identidad_notas(session)
    autorizado <- .crm_es_admin(identidad$usuario, identidad$grupos)
    origen <- reactiveVal(NULL)
    catalogo <- reactive(.asesores_catalogo(publicaciones()$crm$tablas))
    filas <- reactive({
      req(autorizado)
      .asesores_filas(.tabla_fuente(publicaciones(), "pedidos"), catalogo())
    })
    tabla <- racafeModulos::TablaVancouverServer(
      "asesores", data = filas, id_col = "usuario", modo_seleccion = "fila", descargable = FALSE,
      col_specs = list(
        usuario = list(name = "Usuario"),
        nombre = list(name = "Nombre de la persona"),
        correo = list(name = "Correo"),
        vigente = list(name = "Activo", cell = function(valor) {
          racafeForms::racafeStatusBadge(if (valor) "Sí" else "No",
                                        variant = if (valor) "success" else "neutral")
        }),
        ultima_creacion = list(name = "Última creación de pedido", cell = function(fecha) {
          if (is.na(fecha)) "—" else .formato_fecha_crm(fecha)
        })
      )
    )
    observeEvent(tabla$seleccion(), {
      req(autorizado)
      fila <- tabla$seleccion()$fila
      anterior <- catalogo()
      origen(list(clave = fila$usuario, fila = anterior[anterior$usuario == fila$usuario, , drop = FALSE]))
      tabla$limpiar()
      showModal(racafeForms::racafeModalDetail(
        title = paste("Editar asesor", fila$usuario), size = "subventana3",
        racafeForms::racafeForm(
          racafeForms::racafeInputText("nombre", "Nombre de la persona", value = fila$nombre, ns = ns),
          racafeForms::racafeInputText("correo", "Correo", value = fila$correo, ns = ns),
          racafeForms::racafeInputSwitch("vigente", "Activo", value = fila$vigente, ns = ns),
          variant = "plain"
        ),
        footer = racafeForms::racafeModalFooter(
          modalButton("Cancelar"),
          racafeForms::racafeButton("guardar", "Guardar", icon = "check", ns = ns)
        )
      ))
    })
    observeEvent(input$guardar, {
      registro <- origen()
      req(autorizado, !is.null(registro))
      tryCatch({
        .asesores_guardar(raiz, registro$clave, input$nombre, input$correo, input$vigente, identidad, registro$fila)
        origen(NULL)
        removeModal()
        refrescar()
        racafeForms::racafeNotify(session, "Asesor guardado", type = "success")
      }, error = function(e) racafeForms::racafeNotify(session, "No se pudo guardar", conditionMessage(e), type = "error"))
    })
  })
}
