* ============================================================
* ECONOMETRÍA
* EFECTO DEL USO DE BILLETERA DIGITAL SOBRE EL ACCESO A
* CRÉDITO FORMAL
* ============================================================


* ============================================================
* A.1. DISEÑO MUESTRAL
* ============================================================

* Declarar el diseño complejo de la ENAHO

svyset conglomerado [pw=factor_expansion], strata(estrato)

* Para las regresiones se utilizará la alternativa:
*
* [pw=factor_expansion], vce(cluster conglomerado)
*
* De esta manera se incorporan los factores de expansión
* y se agrupan los errores estándar a nivel de conglomerado.


* ============================================================
* A.2. PREPARACIÓN DE VARIABLES
* ============================================================

* ------------------------------------------------------------
* Mujer = 1 si la persona es mujer
* ------------------------------------------------------------

capture drop mujer
gen byte mujer = (sexo == 2) if !missing(sexo)


* ------------------------------------------------------------
* Urbano = 1 si el estrato corresponde a zona urbana
* ------------------------------------------------------------

capture drop urbano
gen byte urbano = (estrato <= 6) if !missing(estrato)


* ------------------------------------------------------------
* Edad al cuadrado
* ------------------------------------------------------------

capture drop edad_cuadrado
gen edad_cuadrado = edad^2


* ------------------------------------------------------------
* Dummies de dominio
* ------------------------------------------------------------

capture drop dom1-dom8
tab dominio, gen(dom)


* ------------------------------------------------------------
* Macro de controles
* ------------------------------------------------------------
*
* Se mantiene la misma estructura de controles en las
* estimaciones para facilitar la comparación entre modelos.
*
* Variable explicativa principal:
*       usa_billetera
*
* Variable de heterogeneidad:
*       trabajador_informal
* ------------------------------------------------------------

global X "edad edad_cuadrado mujer nivel_educativo urbano miembros_hogar dom2-dom8"


* ============================================================
* A.2. DESCRIPTIVOS
* ============================================================

* ------------------------------------------------------------
* Comparación de medias entre usuarios y no usuarios
* de billetera digital
* ------------------------------------------------------------

table () (usa_billetera), ///
    stat(mean nuevo_credito_formal trabajador_informal edad ///
         mujer nivel_educativo urbano miembros_hogar) ///
    stat(sd nuevo_credito_formal trabajador_informal edad ///
         mujer nivel_educativo urbano miembros_hogar)


* ------------------------------------------------------------
* Diferencia de medias de edad
* ------------------------------------------------------------

ttest edad, by(usa_billetera)


* ------------------------------------------------------------
* Diferencias estandarizadas
* ------------------------------------------------------------

* Si no está instalado:
* ssc install stddiff
ssc install stddiff
stddiff edad nivel_educativo, by(usa_billetera)


* ------------------------------------------------------------
* Probabilidad de nuevo crédito formal según:
* uso de billetera x informalidad
* ------------------------------------------------------------

table trabajador_informal usa_billetera, ///
    stat(mean nuevo_credito_formal) ///
    stat(freq)


* ============================================================
* A.3. MODELO PRINCIPAL Y CONTRASTES DE H1 Y H2
* ============================================================

* ------------------------------------------------------------
* MODELO PRINCIPAL: PROBIT
* ------------------------------------------------------------
*
* Variable dependiente:
*       nuevo_credito_formal
*
* Variable explicativa/tratamiento:
*       usa_billetera
*
* Heterogeneidad:
*       trabajador_informal
*
* La interacción permite que el efecto del uso de billetera
* sea diferente entre trabajadores formales e informales.
*
* Los controles son:
*       edad
*       edad^2
*       mujer
*       nivel educativo
*       urbano
*       miembros del hogar
*       dominio
*
* Se utilizan ponderadores y errores agrupados.
* ------------------------------------------------------------

probit nuevo_credito_formal ///
    i.usa_billetera##i.trabajador_informal ///
    $X ///
    [pw=factor_expansion], ///
    vce(cluster conglomerado)


* ------------------------------------------------------------
* H1 y H2:
* Efecto marginal del USO de billetera según informalidad
* ------------------------------------------------------------

margins, dydx(usa_billetera) ///
    over(trabajador_informal)


* ------------------------------------------------------------
* H2:
* Contraste formal entre los dos efectos marginales
* ------------------------------------------------------------

margins, dydx(usa_billetera) ///
    over(trabajador_informal) ///
    pwcompare(effects)


* ------------------------------------------------------------
* Probabilidades predichas
* ------------------------------------------------------------

margins trabajador_informal#usa_billetera


* ------------------------------------------------------------
* Figura 1
* ------------------------------------------------------------

marginsplot, ///
    recast(scatter) ///
    ciopts(recast(rcap))


* ------------------------------------------------------------
* TOST:
* equivalencia del efecto del USO de billetera para formales
*
* Umbral de equivalencia:
* +/- 2 puntos porcentuales
* ------------------------------------------------------------

margins, dydx(usa_billetera) ///
    over(trabajador_informal) post

test _b[1.usa_billetera:0.trabajador_informal] = 0.02

test _b[1.usa_billetera:0.trabajador_informal] = -0.02


* ============================================================
* A.4. H3 Y EVIDENCIA COMPLEMENTARIA
* ============================================================

* ------------------------------------------------------------
* NOTA:
*
* La homologación proporcionada no identifica todavía los
* nombres actuales de las variables:
*
*       creditoinformal
*       ambos
*
* Por lo tanto NO se reemplazan arbitrariamente.
*
* La lógica de H3 será:
*
*       resultado = crédito informal
*       tratamiento = usa_billetera
*
* restringido a trabajadores informales.
*
* El código se incorpora una vez identificados los nombres
* actuales de esas variables.
* ------------------------------------------------------------


* ============================================================
* A.5. ROBUSTEZ R1 A R7
* ============================================================


* ============================================================
* R1. MODELO DE PROBABILIDAD LINEAL
* ============================================================

* ------------------------------------------------------------
* Objetivo:
*
* Verificar que el resultado principal no dependa de la
* forma funcional del Probit.
*
* Se mantiene exactamente la misma especificación, pero se
* estima mediante MCO.
*
* La variable explicativa de interés sigue siendo:
*
*       usa_billetera
*
* La interacción mantiene la heterogeneidad según:
*
*       trabajador_informal
*
* El documento considera que la coincidencia sustantiva
* entre Probit y MPL es suficiente para cerrar la robustez
* de forma funcional.
* ------------------------------------------------------------

reg nuevo_credito_formal ///
    i.usa_billetera##i.trabajador_informal ///
    $X ///
    [pw=factor_expansion], ///
    vce(cluster conglomerado)


* Efectos marginales del uso de billetera
margins, dydx(usa_billetera) ///
    over(trabajador_informal)


* ============================================================
* R2. DEFINICIONES ALTERNATIVAS DEL RESULTADO
* ============================================================

* ------------------------------------------------------------
* Objetivo:
*
* Verificar que el resultado no dependa exclusivamente de
* una determinada definición de acceso al crédito formal.
*
* El documento propone:
*
*       nuevoprestamo
*       nuevatarjeta
*       nuevocredito
*
* En la base actual tenemos identificado:
*
*       nuevo_credito_formal
*
* pero todavía no tenemos en la homologación los nombres
* actuales de nuevoprestamo y nuevatarjeta.
*
* Por eso no se inventan nombres.
* ------------------------------------------------------------

* Resultado principal, ya homologado:

probit nuevo_credito_formal ///
    i.usa_billetera##i.trabajador_informal ///
    $X ///
    [pw=factor_expansion], ///
    vce(cluster conglomerado)

margins, dydx(usa_billetera) ///
    over(trabajador_informal)


* ------------------------------------------------------------
* Cuando se identifiquen los nombres actuales de:
*
*       nuevo_prestamo
*       nueva_tarjeta
*
* se podrán incorporar:
*
* probit nuevo_prestamo ///
*     i.usa_billetera##i.trabajador_informal $X ///
*     [pw=factor_expansion], vce(cluster conglomerado)
*
* probit nueva_tarjeta ///
*     i.usa_billetera##i.trabajador_informal $X ///
*     [pw=factor_expansion], vce(cluster conglomerado)
* ------------------------------------------------------------


* ============================================================
* R3. INFORMALIDAD MEDIDA EN 2025
* ============================================================

* ------------------------------------------------------------
* Objetivo:
*
* Evaluar si el efecto del USO de billetera se mantiene cuando
* la condición de informalidad se mide en 2025 en lugar de 2024.
*
* IMPORTANTE:
*
* trabajador_informal
*       = condición laboral de informalidad
*
* credito_informal_2025
*       = tenencia/uso de crédito informal en 2025
*
* NO son la misma variable.
*
* Por lo tanto NO utilizaremos credito_informal_2025 como
* reemplazo de una medida de informalidad laboral.
*
* La variable informal2025 del documento todavía requiere
* homologación con la base actual.
* ------------------------------------------------------------


* ============================================================
* R4. COTAS DE OSTER (2019)
* ============================================================

* ------------------------------------------------------------
* Objetivo:
*
* Evaluar la sensibilidad del efecto del USO de billetera
* ante posibles variables omitidas.
*
* La estimación se realiza mediante una especificación lineal.
*
* Variable de interés:
*
*       usa_billetera
* ------------------------------------------------------------

* Si no está instalado:
* ssc install psacalc

ssc install psacalc
* Especificación completa
reg nuevo_credito_formal ///
    usa_billetera ///
    $X


* Definir Rmax como 1.3 veces el R2
local rmax = 1.3 * e(r2)


* Cota de Oster
psacalc delta usa_billetera, rmax(`rmax')


* ------------------------------------------------------------
* Interpretación:
*
* Se reporta delta.
*
* El documento utiliza delta >= 1 como referencia informal
* para evaluar la robustez frente a variables omitidas.
*
* Opcionalmente:
*
* psacalc beta usa_billetera, ///
*     rmax(`rmax') delta(1)
* ------------------------------------------------------------


* ============================================================
* R5. PONDERADAS FRENTE A NO PONDERADAS
* ============================================================

* ------------------------------------------------------------
* Objetivo:
*
* Comprobar si el resultado depende de utilizar el factor
* de expansión.
*
* Modelo principal:
*
*       [pw=factor_expansion]
*
* R5:
*
*       SIN ponderador
*
* Se mantiene todo lo demás constante.
* ------------------------------------------------------------

probit nuevo_credito_formal ///
    i.usa_billetera##i.trabajador_informal ///
    $X, ///
    vce(cluster conglomerado)


* Efectos marginales sin ponderadores
margins, dydx(usa_billetera) ///
    over(trabajador_informal)


* ============================================================
* R6. ESPECIFICACIÓN EN NIVELES / STOCK
*     SOBRE LA MUESTRA COMPLETA
* ============================================================

* ------------------------------------------------------------
* Objetivo:
*
* El modelo principal estudia nuevo acceso a crédito formal.
*
* R6 utiliza:
*
*       credito_formal_2025
*
* como medida de tenencia de crédito formal en 2025 sobre la
* muestra completa.
*
* La variable explicativa de interés sigue siendo:
*
*       usa_billetera
*
* y la heterogeneidad:
*
*       trabajador_informal
* ------------------------------------------------------------

probit credito_formal_2025 ///
    i.usa_billetera##i.trabajador_informal ///
    $X ///
    [pw=factor_expansion], ///
    vce(cluster conglomerado)


* Efectos marginales
margins, dydx(usa_billetera) ///
    over(trabajador_informal)


	
	
	* ============================================================
* R7. HISTORIAL DE USO DE LA BILLETERA — PENDIENTE
* ============================================================

* ------------------------------------------------------------
* El documento propone como variante de robustez utilizar
* un indicador de historial/recurrencia de uso de billetera:
*
*       recurr_aj
*
* incorporando además:
*
*       patron
*       nrondas
*
* Estas variables todavía no han sido identificadas en la
* homologación disponible para BASE_REGRESIONES.csv.
*
* Por lo tanto, esta robustez queda PENDIENTE y no se
* reemplazarán arbitrariamente estas variables por
* tiene_billetera.
*
* Esto es especialmente importante porque la variable
* explicativa principal del trabajo es el USO de billetera
* digital, y no simplemente su tenencia.
*
* Una vez identificadas las variables actuales equivalentes,
* la especificación propuesta será:
*
* probit nuevo_credito_formal ///
*     c.recurr_aj##i.trabajador_informal ///
*     $X i.patron i.nrondas ///
*     [pw=factor_expansion], ///
*     vce(cluster conglomerado)
*
* margins, dydx(recurr_aj) over(trabajador_informal)
*
* NO ejecutar hasta identificar las variables equivalentes
* en la base actual.


* ============================================================
* VALIDACIÓN DE LA PROXY DE USO — PENDIENTE
* ============================================================

* ------------------------------------------------------------
* El documento propone además validar la proxy de uso mediante
* las variables:
*
*       proxy2023
*       billetera2024
*
* utilizando medidas de:
*
*       sensibilidad
*       especificidad
*       kappa
*
* Estas variables tampoco han sido identificadas todavía en
* la homologación actual.
*
* Por lo tanto, esta validación queda PENDIENTE.
*
* No se utilizará tiene_billetera como sustituto, ya que
* tenencia de billetera y uso de billetera son conceptos
* diferentes para este trabajo.
* ------------------------------------------------------------


* ============================================================
* FIN DEL ANÁLISIS DE ROBUSTEZ
* ============================================================

* R8 — Variables instrumentales / MC2E
* queda fuera por ahora.