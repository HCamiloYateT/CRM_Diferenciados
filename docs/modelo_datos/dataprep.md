# DataPrep.R | Primer bloque de pedidos y clientes

## Ejecución

Desde la raíz del repositorio, con R y `filelock` instalados previamente:

```sh
Rscript DataPrep.R /ruta/a/exportaciones 2026-11-23
Rscript tests/dataprep_pedidos.R
Rscript tests/persistencia_rds.R
```

La carpeta de exportaciones se mantiene fuera del control de versiones. El ETL abre archivos locales, valida el corte, registra la huella de cada exportación, prepara las tres capas y publica en `data/operacional` por medio del publicador RDS existente. Los informes CSV se conservan en `data/auditoria_etl`, también para cortes rechazados. Las aplicaciones deben usar el manifiesto y leer la generación publicada mediante la API de persistencia. La reingesta de los mismos insumos reconstruye iguales hechos sin anexar filas, con nueva trazabilidad de ejecución.

Se revisaron la validación de claves, el contrato y la publicación temporal del DataPrep de Nacional. Se aplica el patrón al dominio exportador y se reutiliza `racafeCore::SumaEstricta` cuando está disponible. El respaldo base R conserva la misma política de faltantes. `racafeBD` es una referencia para fuentes institucionales, pero la extracción de este bloque se hace desde exportaciones. Los otros repositorios corporativos aportan componentes a la futura aplicación Shiny.

## Contratos de entrada

CSV UTF-8, coma como separador, punto decimal, encabezado único. Los campos se leen como texto para preservar códigos y valores originales. Los campos vacíos se tratan como ausentes. El proveedor debe acreditar que las exportaciones corresponden al mismo corte y que las cabeceras y las líneas cubren el historial necesario y todas las líneas de las cabeceras seleccionadas. Mantener cancelaciones, compañía, ceros iniciales, máscaras de fecha y claves de origen. La primera compra derivada representa la primera observada en la cobertura entregada.

| Archivo y grano candidato | Campos requeridos | Campos opcionales y semántica |
|---|---|---|
| `clientes.csv`, fila por PerCod | `PerCod`, texto institucional | `PerRazSoc`, nombre. `PaiCod`, código fuente de país del cliente. Incluye clientes sin pedidos. |
| `pedidos.csv`, compañía × pedido | `CiaCod`, `PdcCod`, `CliNit` (mismo valor institucional que PerCod), `PdcFecCre` | `PdcEst` original y cancelaciones. `PdcAnoEmb/PdcMesEmb` para referencia mensual. `UMeCod` de cabecera solo cuando su aplicación a líneas sin unidad esté documentada por el proveedor. Otros campos quedan en la capa de origen, incluidos totales de cabecera. |
| `lineas.csv`, compañía × pedido × línea | `CiaCod`, `PdcCod`, `PdcLin`, `LinNegCod` original, `PdcCan` | `UMeCod`, `PdcFecEmb`, `LinProCod`, `MCCod`, `MrcCod`, `PaiCodDtn/PaiCodOri`, `PueCodDtn/PueCodOri`. Si existe valor original verificado, exponerlo mediante alias `ValorLinea` y `MonedaLinea` (moneda de tres letras), conservando los campos originales. |
| `unidades.csv`, fila por unidad en el corte | `UMeCod` si se suministra | `KgPorUnidad` y `SacosPorUnidad`: alias del adaptador suministrado, numéricos positivos y validados por el proveedor. Mantener también nombres originales de origen. Una conversión puede estar ausente. |

El corte es YYYY-MM-DD. Fechas de creación/embarque aceptan día ISO y hora local ISO con segundos cuando la exportación la tenga. El día analítico se extrae sin cambiar la zona, y el valor íntegro queda disponible. Se trata `1753-01-01` como centinela candidato: se preserva el valor y se excluye del uso comercial hasta validación del equipo de datos. Fechas en otros campos técnicos quedan en origen. Las referencias de cabecera y de línea se conservan separadas, sin precedencia operativa.

## Salidas y grano

| Salida RDS | Clave y campos principales |
|---|---|
| `raw_clientes`, `raw_pedidos`, `raw_lineas`, `raw_unidades` | Un contenedor por fuente, con columnas y códigos originales. Pedidos y líneas retienen solo las filas del universo 20000. Los archivos originales quedan como evidencia externa y el manifiesto conserva sus huellas. Catálogo de unidades ausente queda vacío. |
| `clientes`, `unidades` | PerCod con razón social y país fuente. Código de unidad con factores verificados de la exportación. |
| `pedidos` | `pedido_id` reversible por compañía y pedido, PerCod, fecha de creación y texto original, estado, período candidato, sacos, kg, valor y moneda. Cantidades y valores provienen de las líneas 20000. |
| `lineas_pedido` | `pedido_id × PdcLin`; código original, cantidad textual y numérica, unidad y procedencia, factores, kg, sacos, valor y moneda; fecha original y fecha interpretada; producto/marca y puertos/países de origen/destino. Producto y categoría homologados quedan en NA. |
| `primer_pedido_cliente`, `cohortes_clientes`, `clientes_comerciales` | PerCod; pedido inicial observado, fecha, cohorte mensual y antigüedad al corte. Los clientes sin pedido conservan su fila. Si hay pedidos de fecha desconocida para un PerCod, la primera fecha queda sin resolver. Los pedidos cancelados conservados en el histórico no revierten la conversión. |
| `pedidos_por_periodo`, `volumen_cliente` | Mes de creación con pedido distinto y sacos verificables; PerCod con número de pedidos, sacos y kg. Suma estricta por magnitud. |
| `referencias_embarque` | Pedido × referencia de cabecera o línea; año-mes o fecha según precisión. No produce cumplimiento. |
| `jerarquia_cuentas`, `calidad` | Plantilla vacía PerCod × inicio de vigencia; incidencias por ejecución con identificador secuencial, clave y código. |

La conciliación estructural bloquea duplicados, líneas 20000 sin cabecera, clientes sin coincidencia institucional, códigos de línea inválidos, factores no positivos y creaciones posteriores al corte. Los faltantes de cantidades, factores o fechas se conservan como NA, con incidencia cuando aplicable. La agregación usa el detalle previo a cualquier unión con lotes. No se usa cantidad o valor de cabecera como sustituto de cantidades o valores de línea. Los valores con moneda mixta o ausente quedan sin total monetario.

## Conciliación ejecutada y exportaciones pendientes

La prueba sintética cubre dos compañías con el mismo pedido, pedido mixto, sacos fraccionarios, cancelación, centinela, discrepancia entre referencia mensual y día de línea, factor ausente, reingesta, corte vacío y rechazo de duplicados con preservación del corte previo. Los fixtures no representan operaciones de Racafé. El ZIP histórico de Cliente Final aporta consultas y un Excel de lotes que carece de identificador de pedido. No se recibió la exportación auditada del pedido 30342, por lo que su caso y las fechas técnicas 2020/2024 siguen pendientes de conciliación real.

Para cerrar el primer bloque se necesitan cabecera, detalle completo, cliente y catálogo de unidades del pedido 30342, sus llaves/compañía, tipos, semántica de campos de fechas y filas de contraste con cancelación y mezcla de líneas. Luego se requieren la relación línea–lote con código y sucursal, movimientos de embarque efectivo y sus cantidades, países y equivalencias ISO con evidencia, y márgenes con grano y unidad confirmados. El formato de futuras homologaciones de productos debe incluir códigos de origen, categoría/producto aprobados, vigencia, versión, responsable, fuente y estado. Véase el [inventario de exportaciones](exportaciones.md). Lotes, embarques efectivos, ventas por país, FCC y comparaciones de ejecución quedan para los siguientes bloques.
