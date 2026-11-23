# Pruebas sintéticas de persistencia ----

source("APP/misc/persistencia/rds.R")
source("DataPrep.R")
probar <- function() {
  raiz <- tempfile("crm-rds-")
  dir.create(raiz)
  on.exit(unlink(raiz, recursive = TRUE))
  falla <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))
  contratos <- list(
    empresas = list(version = "v1", prototipo = data.frame(id = integer(), nombre = character()), clave = "id"),
    historial = list(
      version = "v1",
      prototipo = data.frame(id = integer(), empresa = integer(), etapa = character()),
      clave = "id", requeridos = c("empresa", "etapa"), solo_anexar = TRUE,
      referencias = list(list(tabla = "empresas", columnas = "empresa"))
    )
  )
  almacen <- AbrirPersistenciaRDS(raiz, list(crm = contratos), ruta = "data")
  falla(AbrirPersistenciaRDS(raiz, list(crm = contratos), ruta = "../afuera"))
  stopifnot(nrow(LeerRDS(almacen, "empresas", "crm")) == 0L)
  cambiar <- function(funcion) {
    ActualizarCorteRDS(almacen, "crm", funcion, usuario = "prueba", motivo = "fixture", referencia = "caso")
  }
  primero <- cambiar(function(tablas) {
    tablas$empresas <- data.frame(id = 1L, nombre = "Ejemplo")
    tablas$historial <- data.frame(id = 1L, empresa = 1L, etapa = "CONTACTO")
    tablas
  })
  falla(cambiar(function(tablas) {
    tablas$empresas <- tablas$empresas[FALSE, ]
    tablas
  }))
  falla(cambiar(function(tablas) {
    tablas$historial$etapa <- "LEAD"
    tablas
  }))
  falla(cambiar(function(tablas) {
    tablas$empresas <- rbind(tablas$empresas, tablas$empresas)
    tablas
  }))
  falla(ActualizarCorteRDS(almacen, "crm", identity))
  stopifnot(identical(LeerCorteRDS(almacen, "crm")$manifiesto$id, primero$id))
  api_falla <- new.env()
  sys.source("APP/misc/persistencia/rds.R", envir = api_falla)
  api_falla$file.rename <- function(from, to) FALSE
  generaciones <- function() list.files(file.path(raiz, "data", "crm"), pattern = "^gen-")
  antes_falla <- generaciones()
  falla(api_falla$ActualizarCorteRDS(almacen, "crm", identity, "prueba", "fallo de publicación", "caso"))
  stopifnot(identical(generaciones(), antes_falla),
            identical(LeerCorteRDS(almacen, "crm")$manifiesto$id, primero$id))
  segundo <- cambiar(function(tablas) {
    tablas$empresas$nombre <- "Ejemplo actualizado"
    tablas
  })
  corte <- LeerCorteRDS(almacen, "crm")
  stopifnot(length(corte$auditoria) == 3L, identical(segundo$anterior, primero$id),
            identical(corte$auditoria[[3L]]$anterior$nombre, "Ejemplo"),
            identical(corte$auditoria[[3L]]$nuevo$nombre, "Ejemplo actualizado"))
  dir.create(file.path(raiz, "APP/misc/persistencia"), recursive = TRUE)
  file.copy("APP/misc/persistencia/rds.R", file.path(raiz, "APP/misc/persistencia/rds.R"))
  prueba_operacional <- list(clientes = contratos$empresas)
  PublicarOperacionalRDS(raiz, list(clientes = data.frame(id = 5L, nombre = "Operacional")),
                        prueba_operacional, list(fuentes = "sintética", corte = as.Date("2026-11-23")))
  stopifnot(identical(LeerCorteRDS(almacen, "crm")$manifiesto$id, segundo$id))
  if (.Platform$OS.type == "unix") {
    resultados <- parallel::mclapply(2:5, function(id) {
      cambiar(function(tablas) {
        tablas$empresas <- rbind(tablas$empresas, data.frame(id = id, nombre = "Paralelo"))
        tablas
      })
    }, mc.cores = 2L)
    stopifnot(all(vapply(resultados, is.list, logical(1))),
              setequal(LeerRDS(almacen, "empresas", "crm")$id, 1:5))
  }
  vigente <- LeerCorteRDS(almacen, "crm")
  writeLines("corrupto", file.path(raiz, "data", "crm", vigente$manifiesto$id, "empresas.rds"))
  falla(LeerCorteRDS(almacen, "crm"))
  RecuperarCorteRDS(almacen, "crm", segundo$id, "prueba", "recuperación", "caso")
  recuperado <- LeerCorteRDS(almacen, "crm")
  stopifnot(identical(recuperado$tablas, corte$tablas),
            identical(tail(recuperado$auditoria, 1L)[[1L]]$operacion, "recuperacion"))
  invisible(TRUE)
}
probar()
