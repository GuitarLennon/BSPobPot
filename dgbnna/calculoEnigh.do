#delimit ;
cls;
capture log close results;

local outputlog = subinstr("output`c(current_date)'`c(current_time)'.smcl", ":", "_", 99);

log using "`outputlog'", replace text name(results);
di "Iniciando do `c(current_date)' `c(current_time)'";
cd;

/***
 *       _____                    _             _        
 *      / ____|                  | |           (_)       
 *     | (___   ___  ___ _ __ ___| |_ __ _ _ __ _  __ _  
 *      \___ \ / _ \/ __| '__/ _ \ __/ _` | '__| |/ _` | 
 *      ____) |  __/ (__| | |  __/ || (_| | |  | | (_| | 
 *     |_____/ \___|\___|_|  \___|\__\__,_|_|  |_|\__,_| 
 *                 _                                     
 *                | |                                    
 *              __| | ___                                
 *             / _` |/ _ \                               
 *            | (_| |  __/                               
 *             \__,_|\___|                               
 *       ____  _                      _                  
 *      |  _ \(_)                    | |                 
 *      | |_) |_  ___ _ __   ___  ___| |_ __ _ _ __      
 *      |  _ <| |/ _ \ '_ \ / _ \/ __| __/ _` | '__|     
 *      | |_) | |  __/ | | |  __/\__ \ || (_| | |        
 *      |____/|_|\___|_| |_|\___||___/\__\__,_|_|        
 *                                                       
====================================================================
Secretaría de Bienestar
Dirección de Sistemas de Información y Padrones
SCRIPT: Cálculo de población potencial
DESCRIPCIÓN: Calcula la población potencial del programa DGBNNA
AUTOR: Dr. Arturo Juárez Flores, Lic. Gerardo Jurado Pliego
FECHA: 28 de julio del 2022
FUENTES: ENIGH 2020
====================================================================
*/

/* =================================================================
    Preparación de la base de datos
    Sitio de descarga: https://en.www.inegi.org.mx/programas/enigh/nc/2020/#Microdata
    Documentación: https://en.www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/889463901242.pdf
*/
use viviendas.dta, clear;
rename *  v_=;
rename (v_folioviv v_factor v_est_dis v_upm) (folioviv factor est_dis upm);

preserve;
    use hogares.dta, clear;
    rename *  h_=;
    rename (h_folioviv h_foliohog) (folioviv foliohog);
    save hogares_temp.dta, replace;
restore;

merge 1:m folioviv using hogares_temp.dta, nogenerate;

preserve;
    use poblacion.dta, clear;
    rename *  p_=;
    rename (p_folioviv p_foliohog p_numren p_padre_id p_madre_id) (folioviv foliohog numren padre_id madre_id);
    save poblacion_temp.dta, replace;
restore;

merge 1:m folioviv foliohog using poblacion_temp.dta, nogenerate;

/* =================================================================
    Generar base de padres, madres o tutores 
    Para cada persona en la base principal, se atraen datos de 
    padre o madre o jefe de familia 
*/
preserve;
    /* Sumar los montos por ingreso de este programa, por hogar */
    use ingresos.dta, clear;

    gen h_apoyoDisc = clave == "P105";
    gen h_apoyoPrograma1 = clave == "P106";
    gen h_apoyoPrograma2 = clave == "P107";
    collapse 
        (max) h_apoyoDisc 
        (max) h_apoyoPrograma1 
        (max) h_apoyoPrograma2, by(folioviv foliohog);

    save ingresos_temp, replace;
restore;

merge m:1 folioviv foliohog using ingresos_temp.dta, nogenerate keep(1 3);
replace h_apoyoPrograma2 = 0 if missing(h_apoyoPrograma2);

preserve;
    /* Determinar el número de trabajos por persona y si alguno tiene servicio de cuidado */
    use trabajos.dta, clear;
    gen tieneServicioDeCuidado = pres_6 == "06";
    gen trabajos = 1;
    collapse (sum) tieneServicioDeCuidado (sum) trabajos, by(folioviv foliohog numren);

    merge 1:1 folioviv foliohog numren using poblacion.dta, nogenerate;

    destring numren, replace;
    rename numren pmt;
    destring act_pnea1, replace force;
    destring act_pnea2, replace force;

    tab act_pnea1, miss;
    tab act_pnea2, miss;

    foreach var of varlist disc* {
        /* Convertir a entero y reemplazar por 0 los valores desconocidos */;
        destring `var', replace force;
        replace `var' = 0 if `var' == .;
        gen es`var' = `var' < 4; 
    }
    ;

    egen disc = rowmin(disc*);
    gen tieneDiscapacidad = disc < 4; 

    rename (tieneServicioDeCuidado trabajos act_pnea1 act_pnea2 etnia tieneDiscapacidad edo_conyug usotiempo4 hor_4 min_4 nivelaprob hablaind lenguaind comprenind) t_=;

    keep folioviv foliohog pmt t_*;

    save trabajos_temp, replace;
restore;

destring padre_id, replace force;
destring madre_id, replace force;
/* Seleccionar padre, madre o jefe de familia de acuerdo a su existencia */
gen pmt = cond(missing(padre_id), cond(missing(madre_id), 1, madre_id), padre_id);

count;
merge m:1 folioviv foliohog pmt using trabajos_temp, nogenerate keep(1 3);

replace t_act_pnea1 = 0 if missing(t_act_pnea1);
replace t_act_pnea2 = 0 if missing(t_act_pnea2);
replace t_trabajos = 0 if missing(t_trabajos);
replace t_tieneServicioDeCuidado = 0 if missing(t_tieneServicioDeCuidado);

count;
replace h_apoyoDisc = 0 if missing(h_apoyoDisc);
replace h_apoyoPrograma1 = 0 if missing(h_apoyoPrograma1);
replace h_apoyoPrograma1 = 0 if missing(h_apoyoPrograma2);
/* =================================================================
    Establecer metodología de encuesta
    Unidad primaria de muestreo: upm
    Peso estadístico: factor (sampling weight)
    Estrato muestral: est_dis (diseño de una fase)
    FPC: No necesaria (tiende a 1)
*/
gen n = 1;                              /* Variable para conteo */
svyset upm [weight=factor], strata(est_dis) singleunit(certainty);
/* Representatividad poblacional */
svy:total n;
/* Representatividad por hogar */
svy:total n if numren == "01";

/* =================================================================
    Generación de variables
*/
/* Edad < 24 */
gen menorA24 = p_edad < 24;               
/* Ausencia de padre */
gen aus_padre = padre_id == .;          
/* Ausencia de madre */
gen aus_madre = madre_id == .;          
/* Escala de ausencia */
gen aus = aus_padre + 2 * aus_madre;    

/* 
    Calcular discapacidad 
    * Se considera persona con discapacidad a aquella que 
    en alguna pregunta de disc. tenga un valor menor a 4 (No tiene discapacidad)

    Para esta población el valor fue menor a 1 % y no parece tener sesgo por la edad a la que se adquieren habilidades como "caminar, hablar, aprender y vestirse".
    Revisar la documentación de la encuesta en: https://en.www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/889463901242.pdf#page=71
*/

foreach var of varlist p_disc* {
    /* Convertir a entero y reemplazar por 0 los valores desconocidos */;
    destring `var', replace force;
    replace `var' = 0 if `var' == .;
    gen es`var' = `var' < 4; 
}
/* Se considera a los valores desconocidos como presonas con discapacidad ya que
    de acuerdo a la encuesta, se sabe que son personas con discapacidad pero no se conoce
    el grado de esta. Además, para la población menor a 8 años usualmente este valor es 0%
*/;
egen disc = rowmin(p_disc*);
gen tieneDiscapacidad = disc < 4; 

/* =================================================================
    Etiquetado de variables
*/

label define sino 
    0 "No"
    1 "Sí";

label define aus
    0 "Sin ausencia"
    1 "Ausencia paterna"
    2 "Ausencia materna"
    3 "Ausencia biparental";

label values menorA24 aus_padre aus_madre tieneDiscapacidad h_apoyoPrograma* sino;
label values aus aus;

/* ====================================
    Cálculo de población potencial
=======================================
https://www.dof.gob.mx/nota_detalle.php?codigo=5639617&fecha=29/12/2021#gsc.tab=0
3. Lineamientos
    3.1. Cobertura
        El Programa para el Bienestar de las Niñas y Niños Hijos de Madres Trabajadoras, operará a nivel nacional, en sus dos modalidades.
        El Programa tiene como prioridad para ser beneficiarias del programa a las personas que habiten en municipios indígenas o con población afromexicana, de alto y muy alto grado de rezago social, zonas con alto y muy alto grado de marginación o con altos índices de violencia, la zona fronteriza, así como las zonas turísticas y aquellas que generen estrategias integrales de desarrollo.
    
    3.2. Población Objetivo
        La población objetivo del Programa son las niñas, niños, adolescentes y jóvenes, de 0 a 23 años de edad que están en situación de vulnerabilidad por la ausencia de uno o de ambos padres.
        
        -- > Se calcula con las variables menor a 24 y aus

        De acuerdo a la población objetivo, el programa opera en dos modalidades:

*/

/* Cálculo de la población potencial */
gen poblPotencial = menorA24 & aus;
svy:total n, over(menorA24 aus) level(95) cformat(%9.5f);
svy:tab menorA24 aus, percent;

/*
    Población potencial:
    No aus   : 31'962,532 [31'424,005 - 32'501,059]
    aus padre: 11'629,182 [11'352,933 - 11'905,431]
    aus madre:  1'459,896 [ 1'368,907 -  1'550,885]
    aus ambos:  5'142,306 [ 4'987,359 -  5'297,252]
    Total: 50,193,916
*/

/* ====================================
    Cálculo de población objetivo
=======================================
    A) Apoyo para el bienestar de las niñas y niños, hijos de madres    trabajadoras.
        Bajo esta modalidad, se apoyarán a las niñas y niños desde recién nacidos hasta un día antes de cumplir los cuatro años de edad, o hasta un día antes de cumplir los 6 años de edad en el caso de las personas con discapacidad, que están en situación de vulnerabilidad por la ausencia temporal o permanente de uno o ambos padres, debido a que no reside(n) en la misma vivienda o no está(n) presente(s) por causas como el abandono y la búsqueda de mejores condiciones socioeconómicas y una mejor calidad de vida.

        --- > Edad (ok)
        --- > Discapacidad (ok)
        ----> ausencia de uno o ambos padres. (ok)
    
*/

gen esPoblacionObjetivoA = p_edad < cond(tieneDiscapacidad, 6, 4) & aus > 0;
label values esPoblacionObjetivoA sino;

svy:total n, over(esPoblacionObjetivoA) level(95) cformat(%9.5f);

/* ============================================
    Pobl obj: 1'830,705 [1'750,001 - 1'911,408]
*/



/* ====================================
    Cálculo de población objetivo
    que cumple criterios
=======================================
    1. Que la madre, padre solo o tutor esté trabajando, buscando empleo o estudiando y no cuenten con el servicio de cuidado y atención infantil, a través de instituciones públicas de seguridad social u otros medios.
    ---> Trabajos > #
    ---> Trabajos >  pres_6 == "06"
    ---> Población (PMT) >  act_pnea1 | act_pnea2

    2. Ser madre, padre solo o tutor de una niña o niño de recién nacido hasta un día antes de cumplir los 4 años de edad, o hasta un día antes de cumplir 6 años de edad de una niña o niño con discapacidad.
    ---> Cumplir criterios A
*/

gen estaTrabajando = t_trabajos > 0;
gen estaBuscandoEmpleo = t_act_pnea1 == 1;
gen estaEstudiando = t_act_pnea1 == 4 | t_act_pnea2 == 4;
gen cuentaConServicioDeCuidado = t_tieneServicioDeCuidado > 0;

gen cumpleCriteriosA = esPoblacionObjetivoA & (estaTrabajando | estaEstudiando | estaBuscandoEmpleo) & !cuentaConServicioDeCuidado;

label values estaTrabajando estaEstudiando estaBuscandoEmpleo cuentaConServicioDeCuidado cumpleCriteriosA sino;

svy:total n, over(cumpleCriteriosA) level(95) cformat(%9.5f);
matrix list r(table);

/* ============================================
    Pobl obj: 943,150 [887,840 - 887,840]
*/

/* ====================================
    Cálculo de población objetivo
    que cumple criterios cubierta
=======================================

    Se utilizó la variable específica en la ENIGH sobre montos recibidos del programa "P106 Apoyo para el Bienestar de los Hijos de Madres Trabajadoras"
    recibido por cualquier miembro del hogar
*/
svy:total n, over(cumpleCriteriosA h_apoyoPrograma1) level(95) cformat(%9.5f);
svy:tab h_apoyoPrograma1 cumpleCriteriosA, percent;




/* ====================================
    Cálculo de población objetivo B
=======================================
    B) Apoyo para el bienestar de las niñas, niños, adolescentes y jóvenes en orfandad materna.
        Bajo esta modalidad, se apoyarán a las niñas, niños, adolescentes y jóvenes, de recién nacidos y hasta los 23 años de edad, en situación de vulnerabilidad por la ausencia permanente de la madre, causada por su fallecimiento (orfandad materna).
        Así mismo, se incluye a las hijas e hijos de las jefas de familia que se encontraban afiliada al Programa Seguro de Vida para Jefas de Familia, vigente hasta el ejercicio fiscal 2020.
*/

gen esPoblacionObjetivoB = p_edad <= 23 & aus_madre;
label values esPoblacionObjetivoB sino;

svy:total n, over(esPoblacionObjetivoB) level(95) cformat(%9.5f);


/* ====================================
    Cálculo de población objetivo
    que cumple criterios con acceso al programa
=======================================

    Se utilizó la variable específica en la ENIGH sobre montos recibidos del programa "P106 Apoyo para el Bienestar de los Hijos de Madres Trabajadoras"
    recibido por cualquier miembro del hogar
*/

gen tipoPobl = esPoblacionObjetivoA + cumpleCriteriosA + esPoblacionObjetivoB * 10;

label define tp 
    0 "No pobl. obj"
    1 "A-"
    2 "A+"
    10 "B"
    11 "A-B"
    12 "A+B";

label values tipoPobl tp;

svy:total n, over(tipoPobl h_apoyoPrograma1) level(95) cformat(%9.5f);
svy:tab tipoPobl h_apoyoPrograma1, percent ci;

capture log close results;