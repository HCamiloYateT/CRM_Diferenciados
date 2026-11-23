# Aplicación Shiny | UI inicial

Desde la raíz del proyecto, ejecutar `shiny::runApp("APP")`. Instalar `shiny`, `bs4Dash` y las dependencias de los componentes corporativos **antes** del arranque. Para la presentación corporativa completa instalar `racafeForms`, `racafeShiny`, `racafeModulos`, `racafeGraph` y `plotly`. La aplicación tiene sustituciones funcionales en Shiny y bs4Dash para una revisión local cuando esos componentes no están instalados. Los logos y estilos de Compartido usan un commit fijado; la estación de revisión debe poder resolver ese origen. Cuando el navegador no pueda acceder a él, configurar `CRM_COMPARTIDO_DIR` con un checkout del mismo commit. Se sirven sus recursos sin copiarlos al repositorio.

## Arquitectura y funciones

| Ruta | Responsabilidad |
|---|---|
| `APP/global.R`, `APP/ui.R`, `APP/server.R` | Carga ordenada, ensamblado bs4Dash, estado de sesión, actualización controlada, filtros y descargas. |
| `APP/misc/ui/` | Encabezado, navegación, controlbar, cuerpo, pie, preloader y controles compartidos. Los modales usan una definición común y se abren desde el encabezado, controlbar o vista correspondiente. |
| `APP/misc/config/vistas.R` | 44 vistas en diez grupos y registro de las fuentes esperadas. Declara dominio, columnas, clave y dependencia inicial. Los estados dinámicos incluyen fecha de corte, versión, calidad y vistas consumidoras. |
| `APP/misc/modules/vista.R` | `VistaUI()` y `VistaServer()`: módulo namespaced registrado por cada vista. Usa tablas Vancouver corporativas, KPI, notas, selectores, gráficos operacionales, ficha independiente por fuente y estados informativos. |
| `APP/misc/datos/publicacion_ui.R` | Lectura de dominios, registro de disponibilidad, validación de la estructura consumida, filtro operacional y datos filtrados por vista. |
| `APP/misc/persistencia/rds.R` | `LeerPublicacionRDS()` lee una generación publicada y verifica manifiesto y huellas. El lector no crea directorios ni inicia el ETL. |

El patrón de piezas y navegación sigue CRM Nacional y PlantillaBs4Dash. Cuando están instalados, los componentes corporativos se usan para botones, formularios, notas, precargador, tarjetas Kanban, KPIs y tablas Vancouver. El filtro distingue país de empresa de destino por línea. Al filtrar destino, se muestran solo líneas de ese destino y se recalculan sacos y valores comparables a partir de ellas. Un pedido con varios destinos puede aparecer en cada destino pertinente en un ranking; el total único por pedido se obtiene de la vista de pedidos. La descarga usa el universo de clientes filtrado.

## Alcance operativo y dependencias

- La publicación actual habilita clientes, pedidos, detalle de líneas, primeros pedidos, cohortes, referencias de embarque, catálogos disponibles y estado de datos. Países se muestran como códigos fuente y los faltantes figuran como «Sin código» en el ranking. Los mapas requieren homologación ISO.
- Los formularios de contacto, gestión, oportunidad, muestra y tareas/notas son visibles con captura y guardado deshabilitados. Su activación depende de contratos de gestión, identidad autorizada y servicios auditados. Las acciones por registro siguen el mismo criterio. Las transiciones manuales no sustituyen la conversión.
- Los resultados de cuentas padre, producto/categoría homologados, tier, presupuesto, potencial, cartera, embarque efectivo y márgenes se habilitan después de incorporar sus fuentes y reglas. El estado de cada dependencia se expone por separado. Los gráficos requieren datos y agregados compatibles. No se conectan a consultas corporativas.
- Para usar datos reales, publicar detrás del acceso institucional y verificar persistencia y permisos del alojamiento. Esta entrega se ejecutó localmente.

## Evidencia de validación

- `Rscript tests/ui.R` valida ensamblado, correspondencia de navegación, IDs únicos, contratos, estados, bloqueo del formulario y filtrado de sacos y valor por destino con datos sintéticos.
- `Rscript tests/dataprep_pedidos.R` y `Rscript tests/persistencia_rds.R` cubren la publicación y sus invariantes.
- Chromium local: arranque sin publicación, apertura de 44 vistas, controlbar, formularios desde dos accesos, botón Clientes, mensajes de carga y viewport de 420 px. En un segundo proyecto local con un corte sintético publicado por DataPrep se verificaron la lectura RDS, el filtro por destino y la descarga filtrada.

La validación local utilizó bs4Dash, Shiny, racafeForms y los recursos de Compartido. Se verificaron los componentes de respaldo de tablas, KPI y gráficos. La revisión visual con racafeModulos, racafeShiny, racafeGraph y Plotly instalados queda pendiente en el entorno corporativo.

## Indicadores de mercado

El encabezado **Indicadores** y **Generalidades → Comparación de Indicadores** comparten los 16 códigos, la fórmula y las consultas de CRM Nacional en la revisión `00ccd4a`. La comparación consulta `CRMINDICADORES` en Analitica para las fechas históricas. Cuando la fecha final es hoy en Bogotá, usa la consulta vigente de FNC y las fuentes operacionales de mercado. Las fechas históricas se resuelven en una misma lectura por solicitud. La variación usa el valor inicial absoluto como denominador y queda ausente cuando ese valor es cero o falta.

El botón Actualizar del menú solicita indicadores vigentes. La consulta de mercado es de solo lectura y la aplicación reutiliza en memoria de sesión el último resultado vigente del día; las actualizaciones explícitas solicitan valores nuevos. La configuración de conexión corresponde a `racafeBD`. Los formatos conservan COP/USD, ¢USD/lb, COP/carga, USD/lb y COP/kg. La tabla de comparación usa el componente Vancouver cuando está instalado y los colores identifican la dirección de la diferencia.

Ejecutar `Rscript tests/indicadores_mercado.R` para verificar cálculos, fechas y unidades con fuentes sintéticas. La comparación de resultados en vivo requiere el entorno con acceso autorizado a las fuentes.
