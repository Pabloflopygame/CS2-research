# Steam CS2 Research (R)

## Requisitos

-   R \>= 4.5
-   Paquetes R: `jsonlite`, `data.table`

## Notas

La base de datos no tiene valores de demanda para fechas anteriores a 2023/01/25, por lo que todas las dichas demandas tendran un 0 como valor.

## Funcionamiento

Los ficheros "`read_...`" leen los json del repertorio <https://github.com/atalantus/buff-price-history-archive> que se encuentra en data y lo transforma en dataframes que usaremos en el trabajo.

El main se encargará del trabajo de estadística principal a su vez que avisar al usuario de cualquier problema.

El RMarkdown se encarga de generar un informe legible en pdf basado en los chunks de main.

## Configuración

Primero de todo evidentemente tienes que instalar las librerías, main te preguntara si las quieres instalar y te forzará a tenerlas antes de continuar, por lo que las puedes descargar por tu cuenta o dejar que el programa lo haga por ti.

Si la base de datos original a cambiado, ahora se te plantean las 2 siguientes opciones (re-generarlos o continuar con la antigua). Re-generarlos puede tomar desde 12 horas hasta días, por lo que recomendamos mantener los actuales.

### Si quieres re-generar los datos:

Necesitas descargar los datos de: <https://github.com/atalantus/buff-price-history-archive>, descomprimirlo (el repositorio tiene una guia de como) y colocarlo en Data.

Si existen datos en out borralos y ejecuta el main y deja que se complete (ten en cuenta que esto va a tardar bastante si la base de datos crecio mucho desde la última versión a día 9/11/2025).

### Si NO quieres re-generar los datos: (recomendado)

Simplemente revisa que out tenga los archivos del repertorio y usa esos.

Aparte de estas 2 opciones no se necesita nada más para que funcione, en principio main debería avisarte y ayudarte a configurar cualquier cosa si hiciera falta.
