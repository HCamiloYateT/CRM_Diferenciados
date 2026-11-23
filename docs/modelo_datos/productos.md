# Productos y Market Basket

La vista **Productos** incluye ejecución, cestas, relaciones observadas y calidad. La unidad de identificación de un ítem es compañía y código original, con catálogos independientes para marca y nombre comercial. Las descripciones se conservan para presentación y control de calidad. Sin código, el nombre queda en la traza y el volumen se mantiene sin atribución. Los conflictos de descripciones se muestran como tales. La relación marca–nombre se observa al grano de operación y aparece también resumida por compañía. El identificador técnico se puede recuperar a partir de la longitud y el valor de la compañía y del código.

## Fuente y conciliación

DataPrep conserva el detalle original en la publicación operacional, incluyendo `MrcCod/MCCod`, `MrcNom/MCNom` o los campos originales de lote equivalentes cuando se entregan en el detalle. La cabecera aporta PerCod, compañía, creación y estado. `Comercial` en la cabecera original es opcional y requiere una asignación verificable. Los filtros de país de empresa, país de destino, cliente, PerCod y estado se comparten con el CRM. La fecha global conserva su significado de creación; el período del tablero usa la fecha efectiva para sacos embarcados. Los municipios o destinos sin código quedan visibles en calidad o como «Sin código».

La exportación opcional `lotes_productos.csv` debe contener:

| Campo | Contrato |
|---|---|
| `CiaCod, CLPdcCod, CLPdcLin, CLLotCod` | Llave operación–lote. El código de lote debe ser único físicamente dentro de la compañía; si la fuente exige sucursal, el proveedor debe aportar una clave estable que la incorpore. |
| `CLLinNegCo` | Código original. Se conserva exclusivamente 20000. |
| `SacosAsignados` | Sacos pedidos asignados a la línea–lote por un proceso certificado. `CLLotCan` se conserva en origen cuando exista; su unidad o equivalencia debe verificarse antes de generar el alias. |
| `CLMrcCod, CLMrcNom, CLMCCod, CLMCNom, CLPaiCodDt` | Atributos originales opcionales. El lote conserva sus valores aunque difieran de la línea. |
| `SacosEmbarcados, FechaEmbarqueEfectivo` | Ejecución opcional verificable por asignación. Si se entregan varios embarques por un mismo lote y línea, el corte requiere una granularidad adicional de movimiento antes de representarlos en períodos separados. |
| `PesoFCCKg, MNFCCCOPKg` | Peso físico asignado y margen unitario COP/kg para el mismo tramo; el margen total surge del producto de ambos y el MNFCC agregado se pondera por kg con FCC. |
| `ReferenciaAsignacion` | Evidencia única por asignación cuando un lote participa en varias líneas. Verificar fuera del ETL que la masa y ejecución acumuladas coinciden con el lote fuente. |

El ETL rechaza duplicados, lotes sin línea elegible y sobreasignaciones. El saldo de una línea con lotes permanece sin atribución. Cuando falta el puente, el tablero usa los atributos de línea y conserva como no disponibles la ejecución, los lotes y FCC. Los sacos sin cantidad verificable quedan como NA. Los resultados con ejecución o FCC reflejan únicamente asignaciones verificadas. El ranking de volumen usa como denominador el volumen atribuible de la dimensión; un pedido o cliente puede aparecer en más de un ítem, por lo que sus conteos agrupados no son aditivos. Los cambios entre mitades del período son indicadores descriptivos, sin umbrales automáticos de inactividad ni recomendación de up-selling.

## Market Basket

Instalar previamente `racafeForecast` 1.0.1 o compatible. El módulo utiliza sus funciones `ConjuntosFrecuentes`, `ReglasAsociacion` y `ValidarReglasAsociacion`, como Nacional. Las cestas deduplican presencia por ítem y por pedido compuesto, cliente–mes de creación o cliente–período. Los volúmenes se consultan en hechos separados. El modelo muestra cobertura de pedidos y clientes, frecuencia, tamaños, reglas, pares y una matriz de coocurrencia de los ítems principales. Las cestas incluyen una dimensión por ejecución. Las métricas se expresan como proporciones; lift compara confianza y prevalencia. La validación usa un bloque cronológico posterior para pedido y para cliente–mes. Fisher y el ajuste BH son opcionales; la evidencia pequeña requiere revisión comercial. Las reglas se calculan bajo demanda y quedan en memoria de la sesión. Cambiar los filtros o parámetros del modelo exige recalcular.

El filtro individual de cliente se aplica al tablero, pero las reglas se estiman sobre el universo comparable definido por período, destino, país de empresa, comercial y estado. Una recomendación requiere antecedente observado y excluye ítems ya presentes para ese PerCod dentro del período seleccionado, incluso fuera de los filtros del universo comparable. La validación temporal, cuando se solicita, exige evidencia de la asociación en el tramo de prueba. Los clientes pueden compartir patrones sin que ello implique probabilidad garantizada de compra.

## Aceptación y verificación

Configurar `CRM_CROSS_SELLING_USUARIOS` con identidades institucionales separadas por comas. La acción exige `session$user` en esa lista, una confirmación manual y un motivo. El usuario que confirma queda también como responsable inicial. Se guarda una oportunidad `CROSS_SELLING` pendiente con evidencia y auditoría, y se abre Gestión Comercial para revisarla. La misma combinación de cliente, dimensión e ítem se registra una sola vez. El contrato de escritura inicial admite el dominio CRM vacío o compatible con la tabla de oportunidades. Una publicación CRM con otro contrato requiere integración de esquemas antes de habilitar esta escritura; el guardado se rechaza y conserva la gestión existente.

`Rscript tests/productos_market_basket.R` usa datos sintéticos para probar asignaciones de lotes, claves por compañía, saldo sin atribución, FCC ponderado, geografía, soporte/confianza/lift, validación temporal, autorización, duplicados y preservación del corte. Las exportaciones reales con el puente línea–lote, embarques y FCC aún deben cotejarse para habilitar valores reales en esas vistas.

## Navegación y contexto comercial

Inicio describe las secciones y limitaciones. Generalidades agrupa Resumen ejecutivo y Comparación de Indicadores. Prospección sigue el orden de Nacional para las vistas compartidas. Cohortes de clientes permite elegir el intervalo exacto de primera compra observada y muestra los PerCod nuevos y la distribución mensual. Gestión Comercial incluye Presupuesto con `PptoL = 0` provisional y ejecución por creación de pedido. El cumplimiento queda ausente mientras la meta sea cero y los meses posteriores al corte no se muestran como ejecución nula. Las vistas reutilizan los estilos compartidos de Nacional, con objetos operacionales de la línea 20000.
