# REPROSA RETC - App Shiny

Aplicación Shiny para dos pasos del flujo RETC:

1. **Clasificación previa** de insumos con y sin sustancias RETC.
2. **Generación de matriz RETC** con kg/año y validación contra umbral MPU de la NOM-165-SEMARNAT-2013.

## Estructura mínima esperada

### Catálogo RETC (`catalogo_retc_nom165.csv`)
Columnas mínimas:
- `cas`
- `sustancia_retc`
- `umbral_mpu_kg_anual`

### Base de insumos para Paso 1
Debe contener al menos:
- una columna del producto: `comercial`, `nombre_insumo`, `insumo`, `producto` o `nombre_producto`
- una columna CAS: `cas`, `no_cas`, `n_cas`, `numero_cas`

### Base de insumos para Paso 2
Columnas mínimas:
- `cas`
- `cantidad_anual`
- `unidad_codigo`
- `factor_kg_por_unidad`

Opcionales:
- `nombre_compuesto`
- `nombre_insumo`
- `cliente`
- `observaciones`

## Correr localmente

```r
install.packages(c("shiny","readr","readxl","dplyr","stringr","janitor","openxlsx","DT"))
shiny::runApp()
```

## Publicar en shinyapps.io

1. Instala `rsconnect`
2. Configura tu cuenta
3. Publica

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "TU_USUARIO", token = "TU_TOKEN", secret = "TU_SECRET")
rsconnect::deployApp()
```

También puedes publicar desde el botón **Publish** de Posit/RStudio.
