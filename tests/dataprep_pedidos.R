# Fixture sintético. Los identificadores y resultados no corresponden a operaciones reales.
source("DataPrep.R")

probar <- function() {
  raiz <- tempfile("dataprep-")
  dir.create(raiz)
  on.exit(unlink(raiz, recursive = TRUE))
  file.copy("APP", raiz, recursive = TRUE)
  escribir <- function(nombre, datos) {
    utils::write.csv(datos, file.path(raiz, paste0(nombre, ".csv")), row.names = FALSE, na = "")
  }
  escribir("clientes", data.frame(PerCod = c("001", "002", "003", "004"), PaiCod = c("CO", "DE", "PE", "CO")))
  cabeceras <- data.frame(
    CiaCod = c("01", "02", "01", "01"), PdcCod = c("A1", "A1", "B2", "C3"),
    CliNit = c("001", "002", "001", "003"),
    PdcFecCre = c("2024-01-15", "2024-02-02", "2024-03-01", "1753-01-01"),
    UMeCod = "S", PdcAnoEmb = "2024", PdcMesEmb = "03", PdcEst = c("CANCELADO", "VIGENTE", "VIGENTE", "VIGENTE")
  )
  lineas <- data.frame(
    CiaCod = c("01", "01", "01", "02", "01", "01"), PdcCod = c("A1", "A1", "A1", "A1", "B2", "C3"),
    PdcLin = c("01", "02", "03", "01", "01", "01"), LinNegCod = c(20000L, 21000L, 20000L, 20000L, 10000L, 20000L),
    PdcCan = c(1.25, 99, 2.5, 8, 99, 5), UMeCod = c("S", "S", "S", "X", "S", "S"),
    PdcFecEmb = c("2024-04-01", "", "1753-01-01", "", "", ""), LinProCod = "0001",
    ValorLinea = c(125, 999, 250, 8, 999, 50), MonedaLinea = "USD"
  )
  escribir("pedidos", cabeceras)
  escribir("lineas", lineas)
  escribir("unidades", data.frame(UMeCod = "S", KgPorUnidad = 60, SacosPorUnidad = 1))
  resultado <- EjecutarDataPrep(raiz, "2024-12-31", raiz)
  vigente <- file.path(raiz, "data", "operacional")
  generar <- function(nombre) readRDS(file.path(vigente, readRDS(file.path(vigente, "vigente.rds")), nombre))
  pedidos <- generar("pedidos.rds")
  detalle <- generar("lineas_pedido.rds")
  primeros <- generar("primer_pedido_cliente.rds")
  stopifnot(nrow(pedidos) == 3L, nrow(detalle) == 4L, all(detalle$LinNegCod == "20000"),
            all(detalle$LinProCod == "0001"), all(is.na(detalle$producto_homologado)),
            pedidos$sacos[pedidos$PerCod == "001"] == 3.75, pedidos$kg[pedidos$PerCod == "001"] == 225,
            pedidos$valor[pedidos$PerCod == "001"] == 375, is.na(pedidos$sacos[pedidos$PerCod == "002"]),
            primeros$fecha_creacion[primeros$PerCod == "001"] == as.Date("2024-01-15"),
            !("003" %in% primeros$PerCod), nrow(generar("jerarquia_cuentas.rds")) == 0L,
            pedidos$periodo_cabecera[1L] == "2024-03", detalle$fecha_embarque_linea[1L] == as.Date("2024-04-01"),
            is.na(detalle$fecha_embarque_linea[2L]), file.exists(resultado$informe))
  EjecutarDataPrep(raiz, "2024-12-31", raiz)
  stopifnot(identical(pedidos, generar("pedidos.rds")))
  id_valido <- readRDS(file.path(vigente, "vigente.rds"))
  escribir("lineas", rbind(lineas, lineas[1L, ]))
  fallo <- try(EjecutarDataPrep(raiz, "2024-12-31", raiz), silent = TRUE)
  stopifnot(inherits(fallo, "try-error"), identical(readRDS(file.path(vigente, "vigente.rds")), id_valido))
  escribir("lineas", lineas[FALSE, ])
  EjecutarDataPrep(raiz, "2024-12-31", raiz)
  stopifnot(nrow(generar("pedidos.rds")) == 0L)
}
probar()
