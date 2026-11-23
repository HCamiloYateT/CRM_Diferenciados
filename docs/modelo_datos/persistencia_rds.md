# Persistencia RDS

CRM Diferenciados guarda sus estados en RDS dentro del proyecto. El ETL prepara cortes operacionales y Shiny accede a ellos mediante una única capa en `APP/misc/persistencia/rds.R`. Se pueden reutilizar funciones, componentes y patrones de CRM Nacional después de retirar sus dependencias de datos. Quedan excluidas de las fuentes `Analitica.CRMNALCLIENTE` y las tablas propias de Nacional de prospección, contactos, etapas, oportunidades, notas, tareas, jerarquías y homologaciones. Las fuentes operacionales se verifican con las exportaciones autorizadas.

## Ubicación y propiedad

| Dominio | Contenido y escritor |
|---|---|
| `data/operacional/` | Cortes preparados por ETL: clientes institucionales, cabeceras, líneas elegibles, lotes, embarques, márgenes y catálogos de origen cuando estén disponibles. |
| `data/crm/` | Gestión de la aplicación, contactos, eventos, oportunidades, tareas, muestras, asignaciones, homologaciones aprobadas y catálogos administrados en el CRM. El ETL no escribe aquí. |
| `gen-.../` por dominio | Generación inmutable con un RDS por contrato, auditoría y manifiesto. Las generaciones previas son respaldos locales. |

`CRM_DATOS_DIR` configura una ruta relativa interna. El directorio predeterminado es `data`, ignorado por Git. Para rutas alternativas, la exclusión del directorio completo debe formar parte de la configuración local antes de generar datos. El directorio debe quedar fuera de las rutas públicas de Shiny y contar con permisos de servicio acordes con el dominio. ETL y aplicación deben utilizar permisos de sistema de archivos que impidan al ETL escribir gestión y limiten a Shiny a leer los cortes operacionales. La API toma la raíz del proyecto explícitamente y valida rutas. Los contratos se mantienen en código, con prototipos vacíos y las validaciones aprobadas para cada conjunto. No se crean RDS corporativos en el repositorio.

## API de acceso

1. Cargar `APP/misc/persistencia/rds.R` en el arranque de la aplicación y en el punto de entrada del ETL. Instalar `filelock` antes del arranque. Cada escritor recibe los contratos del dominio que administra.
2. `AbrirPersistenciaRDS(raiz, contratos)` registra una lista por dominio y tabla. Cada contrato tiene `version`, `prototipo` (data frame vacío de columnas tipadas), `clave`, `requeridos` opcionales, `referencias` del mismo dominio mediante `tabla` y `columnas` locales en el orden de la clave referenciada, `validar` opcional y `solo_anexar` para eventos históricos. El diccionario relacional sirve de antecedente. Los contratos operacionales concretos deben usar datos y claves verificados.
3. `LeerCorteRDS()` devuelve tablas, manifiesto y auditoría de una generación. Cargar un corte una vez y compartirlo entre módulos. `LeerRDS()` permite leer un conjunto singular y mantiene las garantías del corte completo.
4. `ActualizarCorteRDS()` modifica una o varias tablas en una sola publicación. `ActualizarRDS()` transforma una tabla y `GuardarRDS()` permite reemplazarla desde un objeto completo. Para edición de usuarios, usar transformaciones sobre el estado leído **dentro del bloqueo** y comprobar revisiones esperadas cuando un formulario puede estar desactualizado. Los callbacks devuelven datos y carecen de efectos externos. Para gestión se exige identidad autenticada, motivo y referencia autorizados por el servidor.
5. El punto de entrada `PublicarOperacionalRDS()` en `DataPrep.R` usa el dominio `operacional` y publica el conjunto completo tras validación. Recibe objetos ya preparados y procedencia con fuentes y corte y nunca guarda sobre gestión. Un vínculo comercial hacia un pedido se valida con una clave operacional estable al momento de escribir y se reconcilia por corte. La referencia conserva su evidencia si el pedido desaparece de un corte posterior. Las FK dentro de cada dominio se validan en cada publicación.
6. `LeerBackupRDS()` verifica una generación previa. `RecuperarCorteRDS()` publica un respaldo comprobado cuando el vigente no se puede leer, con autor, motivo, referencia y generación elegida. La administración debe escoger una generación previamente confirmada y conservar los archivos afectados para diagnóstico.

## Garantías y límites

- El bloqueo exclusivo por dominio cubre lectura, transformación, validación y publicación. Cada conjunto valida tipos, claves, columnas obligatorias y referencias internas. Los identificadores de operaciones son claves estables asignadas por la aplicación, con detección de duplicados. Para tareas reintentables, los productores emplean claves idempotentes y comprueban el estado vigente antes de volver a aplicar la operación.
- Se escribe una generación apartada, se valida su relectura y se publica un solo puntero mediante renombrado en el mismo directorio. Los lectores resuelven un puntero una vez y los archivos referenciados permanecen inmutables. Una operación entre dominios no se ofrece como transacción; los cambios que deban confirmarse juntos deben pertenecer al mismo dominio. La implementación requiere un host Unix y un sistema de archivos cuyo bloqueo y renombrado se hayan verificado en ese entorno.
- Antes de confirmarse, una escritura fallida conserva el puntero anterior. Los errores ordinarios retiran archivos temporales; una terminación abrupta puede dejar generaciones huérfanas. Una generación presente sin puntero requiere inspección y recuperación explícita. La verificación de huellas detecta corrupción accidental. La atomicidad de visibilidad no equivale a durabilidad frente a pérdida del sistema de archivos. Mantener copias institucionales y probar restauración.
- La auditoría por clave guarda operación, instante, usuario, motivo, referencia y datos anteriores/nuevos. El historial de etapas debe tener contrato `solo_anexar` y actualizarse junto con la etapa vigente. Publicar datos y auditoría bajo el mismo puntero. La auditoría puede contener información sensible y hereda los permisos del dominio.
- El modelo conserva copias completas y auditoría acumulada. Las generaciones previas permanecen mientras puedan estar en uso. Definir retención, volúmenes, respaldo externo y eventual archivado con el alojamiento antes de gestionar datos reales.

## Validación antes de desplegar

Verificar persistencia de la carpeta interna durante reinicios y reemplazo del despliegue. En Posit Connect, probar con la topología real si las instancias comparten la misma ruta y si bloqueo y renombrado son efectivos entre procesos. Si el directorio es efímero o las instancias tienen visiones independientes, resolver el almacenamiento persistente compartido dentro de la ruta del proyecto antes de habilitar escrituras. Hacer una prueba de temporal fallido, concurrencia, restauración y actualización del despliegue. La arquitectura queda sin despliegue ni datos reales.

La prueba sintética se ejecuta desde la raíz con `Rscript tests/persistencia_rds.R`. Cubre esquema y referencias, historial de solo anexado, auditoría, publicación fallida, concurrencia local, aislamiento operacional y recuperación. La prueba de alojamiento compartido se realiza aparte.
