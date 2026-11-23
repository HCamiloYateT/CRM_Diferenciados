# DDL de Fase 2

Dialectado provisionalmente para MySQL 8.0.16+ con InnoDB, base vacía con UTF-8 y modo estricto, por afinidad con la persistencia MySQL/MariaDB documentada en CRM Nacional. Confirmar motor, versión, colación, convenciones de auditoría y permisos antes de preparar el procedimiento de aplicación. Los contratos físicos son internos al CRM y mantienen separados origen, snapshot y gestión.

Orden: `schema/001_crm.sql` y `schema/002_hechos.sql`. Estos archivos versionados describen un esquema inicial y requieren revisión antes de convertirlos en migraciones. Al aplicarlos en el futuro se necesitará control de versiones y procedimiento de recuperación, dado que el DDL del motor puede confirmar operaciones implícitamente. Los scripts no cargan datos, consultan orígenes, crean bases ni ejecutan cambios en producción.

Las restricciones del motor cubren integridad local. La autorización, los intervalos no solapados históricos, la elegibilidad de pedidos, la conciliación de asignaciones y la aprobación de modelos deben verificarse en los servicios y el publicador de cortes. El [modelo](../docs/modelo_datos/modelo.md) y el [diccionario](../docs/modelo_datos/diccionario.md) especifican esos contratos.
