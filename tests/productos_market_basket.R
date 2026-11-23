# Datos sintéticos. Prueba la publicación, conciliación, cestas y aceptación autorizada.
source("APP/global.R")
source("DataPrep.R")
probar <- function() {
  raiz <- tempfile("productos-")
  dir.create(raiz)
  on.exit(unlink(raiz, recursive = TRUE))
  file.copy("APP", raiz, recursive = TRUE)
  escribir <- function(id, tabla) utils::write.csv(tabla, file.path(raiz, paste0(id, ".csv")), row.names = FALSE)

  clientes <- data.frame(PerCod = c("001", "002", "003"))
  pedidos <- data.frame(CiaCod = c("01", "02", "01", "01"), PdcCod = c("A", "A", "B", "C"),
                        CliNit = c("001", "003", "001", "002"), UMeCod = "S",
                        PdcFecCre = c("2026-01-10", "2026-01-15", "2026-02-10", "2026-02-11"))
  lineas <- data.frame(CiaCod = c("01", "01", "02", "01", "01", "01"), PdcCod = c("A", "A", "A", "B", "C", "C"),
                        PdcLin = c("1", "2", "1", "1", "1", "2"), LinNegCod = c(20000, 10000, 20000, 20000, 20000, 20000),
                        PdcCan = c(10, 9999, 3, 4, 3, 3), MrcCod = c("01", "0", "01", "01", "01", "02"),
                        MCCod = c("0001", "0", "0003", "0001", "0001", "0002"),
                        MrcNom = c("Marca A", "X", "Marca C", "Marca A", "Marca A", "Marca B"),
                        MCNom = c("Nombre A", "X", "Nombre C", "Nombre A", "Nombre A", "Nombre B"), PaiCodDtn = "CO")
  lotes <- data.frame(CiaCod = "01", CLPdcCod = "A", CLPdcLin = "1", CLLotCod = c("L1", "L2", "L3"),
                       CLLinNegCo = 20000, SacosAsignados = c(2, 3, 4), CLMrcCod = c("01", "01", "02"),
                       CLMCCod = c("0001", "0002", "0001"), CLMrcNom = c("Marca A", "Marca A", "Marca B"),
                       CLMCNom = c("Nombre A", "Nombre B", "Nombre A"), CLPaiCodDt = c("CO", "CO", "DE"),
                       SacosEmbarcados = c(1, 2, 3), FechaEmbarqueEfectivo = "2026-03-01",
                       PesoFCCKg = c(100, 200, 300), MNFCCCOPKg = c(3, 6, 9))
  escribir("clientes", clientes)
  escribir("pedidos", pedidos)
  escribir("lineas", lineas)
  escribir("unidades", data.frame(UMeCod = "S", SacosPorUnidad = 1))
  escribir("lotes_productos", lotes)
  EjecutarDataPrep(raiz, "2026-11-23", raiz)
  pub <- LeerPublicacionRDS(raiz, "operacional")
  hechos <- .prod_hechos(pub)
  marcas <- .prod_dimension(hechos, "marca")
  nombres <- .prod_dimension(hechos, "nombre")
  stopifnot(sum(hechos$sacos) == sum(pub$tablas$lineas_pedido$sacos),
            sum(hechos$sacos[hechos$fuente == "saldo sin atribución"]) == 1,
            length(unique(marcas$item[!is.na(marcas$item)])) == 3L,
            identical(sort(unique(marcas$codigo[!is.na(marcas$codigo)])), c("01", "02")),
            identical(sort(unique(nombres$codigo[!is.na(nombres$codigo)])), c("0001", "0002", "0003")),
            sum(hechos$embarcado, na.rm = TRUE) == 6,
            .prod_resumen(hechos, character())$MNFCC == 7,
            .prod_resumen(hechos, character())$Clientes == 3L,
            .prod_resumen(hechos, character())$Pedidos == 4L)
  d <- .prod_filtrar(marcas, list(destino = "DE", percod = ""), as.Date(c("2026-01-01", "2026-12-31")))
  stopifnot(sum(d$sacos) == 4)
  cestas <- .prod_cestas(marcas, "pedido")
  cestas_mes <- .prod_cestas(marcas, "mes")
  cestas_periodo <- .prod_cestas(marcas, "periodo")
  stopifnot(length(cestas$ids) == 4L, length(cestas_mes$ids) == 4L, length(cestas_periodo$ids) == 3L,
            nrow(cestas$membresia) == 6L, !anyDuplicated(cestas$membresia))
  analisis <- .prod_analizar_cestas(cestas, 0, 1, 3, 0, 0, FALSE, TRUE)
  regla <- analisis$reglas[analisis$reglas$Compra == "2:0101" & analisis$reglas$Asociado == "2:0102", ]
  stopifnot(nrow(regla) == 1L, isTRUE(all.equal(regla$Soporte, .5)),
            isTRUE(all.equal(regla$Confianza, 2 / 3)), isTRUE(all.equal(regla$Lift, 4 / 3)),
            regla$CestasAmbos == 2L)
  temporal <- .prod_analizar_cestas(cestas, 0, 1, 3, 0, 0, TRUE, TRUE)
  stopifnot(temporal$cestas_validacion > 0L, "LiftValidacion" %in% names(temporal$reglas))
  h <- marcas[marcas$fecha > as.Date("2026-02-01"), , drop = FALSE]
  sugerencias <- .prod_recomendar(analisis, h, "001", "marca", as.Date(c("2026-02-01", "2026-03-01")))
  stopifnot(nrow(sugerencias) == 1L, is.null(LeerPublicacionRDS(raiz, "crm")))
  previo <- Sys.getenv("CRM_CROSS_SELLING_USUARIOS", unset = NA_character_)
  on.exit(if (is.na(previo)) Sys.unsetenv("CRM_CROSS_SELLING_USUARIOS") else
    Sys.setenv(CRM_CROSS_SELLING_USUARIOS = previo), add = TRUE)
  Sys.setenv(CRM_CROSS_SELLING_USUARIOS = "persona_autorizada")
  falla <- function(expr) inherits(try(force(expr), silent = TRUE), "try-error")
  stopifnot(falla(.prod_aceptar(raiz, sugerencias, "otra_persona", "prueba")))
  .prod_aceptar(raiz, sugerencias, "persona_autorizada", "Confirmación explícita de prueba")
  crm <- LeerPublicacionRDS(raiz, "crm")
  stopifnot(crm$tablas$oportunidades$tipo == "CROSS_SELLING",
            crm$tablas$oportunidades$responsable == "persona_autorizada",
            falla(.prod_aceptar(raiz, sugerencias, "persona_autorizada", "Duplicada")))
  vigente <- pub$manifiesto$id
  lotes$SacosAsignados[1L] <- 20
  escribir("lotes_productos", lotes)
  stopifnot(falla(EjecutarDataPrep(raiz, "2026-11-23", raiz)),
            LeerPublicacionRDS(raiz, "operacional")$manifiesto$id == vigente)
}
probar()
