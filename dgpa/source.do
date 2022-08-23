gl bases="./sources"
gl data="./data"
gl temp="./temp"
gl log="./log"
local outputlog = subinstr("$log/source - `c(current_date)'`c(current_time)'.txt", ":", "_", 99)

capture log close
log using "`outputlog'", replace text


*1)Población Potencial
/* ****ESTIMACION POBLACION POTENCIAL PABNNHMT****
                    MODALIDAD A Y B
CON BASE EN LOS DATOS DE:
1) CENSO 2020 DE PERSONAS Y VIVIENDA (INEGI)
INEGI 2020:https://www.inegi.org.mx/programas/ccpv/2020/?msclkid=68cdffc9b5bf11ec950dcb02fd64dd35#Microdatos

*/ 

*------------------------------------------------------
//Cargar base de datos del censo 2020 cuestionario ampliado
use "$bases\poblacion2020.dta", clear

*-------------------------------------------------------
//Filtrar campos para calculo
    keep ent sexo edad ident_madre ident_padre factor

*-------------------------------------------------------
    *Definición de población con respecto a la Edad
    gen mar_edad=0
    replace mar_edad=1 if edad<24 
tab mar_edad [w=factor]
keep if mar_edad==1

*-------------------------------------------------------
//Definición de situación de ausencia con respecto a los padres 
    *Ausencia materna
    gen huer_madre=0
    replace huer_madre=1 if (ident_madre==96 | ident_madre==97) & (ident_padre!=96 & ident_padre!=97)
    recode huer_madre (.=0)
    tab huer_madre [w=factor]
    
    *Ausencia paterna
    gen huer_padre=0
    replace huer_padre=1 if (ident_padre==96 | ident_padre==97) & (ident_madre!=96 & ident_madre!=97)
    recode huer_padre (.=0)
    tab huer_padre [w=factor]
    
    *Ausencia ambos
    gen huer_ambos=0
    replace huer_ambos=1 if (ident_padre==96 | ident_padre==97) & (ident_madre==96 | ident_madre==97)
    recode huer_ambos (.=0)
    tab huer_ambos [w=factor]

    *Tabla combinada
    gen huer = huer_padre | huer_madre | huer_ambos
    total huer huer_madre huer_padre huer_ambos [w=factor], cformat(%9.0f)

**CALCULO DE POBLACIONES POTENCIALES**

*1) TOTAL DE POBLACION DE 0 A 23 AÑOS CON AUSENCIA DE MADRE O PADRE A NIVEL ENTIDAD FEDERATIVA
    display in red "Población de 0 a 23 años con orfandad materna o parterna por entidades federativas"
    tabstat huer_madre huer_padre huer_ambos[w=factor],by(ent) stat(sum) format(%10.0fc) 

*2) TOTAL DE POBLACIÓN MASCULINA DE 0 A 23 AÑOS CON AUSENCIA DE MADRE O PADRE POR EDAD
    display in red "Población masculina de 0 a 23 años con orfandad materna o partena"
    tabstat huer_madre huer_padre huer_ambos if sexo==1 [w=factor],by(edad) stat(sum) format(%10.0fc) 

*3) TOTAL DE POBLACION FEMENINA DE 0 A 23 AÑOS CON AUSENCIA DE MADRE O PADRE POR EDAD
    display in red "Población femenina de 0 a 23 años con orfandad materna o partena"
    tabstat huer_madre huer_padre huer_ambos if sexo==3 [w=factor],by(edad) stat(sum) format(%10.0fc) 
    save "$bases\cuadros_potencial.dta",replace

    

/*
 Corregido 
*/
    use "$bases\viviendas.dta", clear
    merge 1:m folioviv using "$bases\hogares.dta", nogenerate
    merge 1:m folioviv foliohog using "$bases\poblacion.dta", nogenerate

*Generar base padres, madres o tutores, se atraen datos de padre o madre
preserve 
use "$bases\ingresos.dta", clear
gen h_apoyoprograma = clave =="P106"
collapse (max) h_apoyoprograma, by(foliov foliohog)
save "$bases\ingresos_temp.dta", replace

restore, preserve
use "$bases\trabajos.dta", clear 

gen tieneserviciodecuidado = pres_6 == "06"
gen trabajos=1
collapse (sum) tieneserviciodecuidado (sum) trabajos, by(folioviv foliohog numren)
merge 1:1 folioviv foliohog numren using "$bases\poblacion.dta", nogenerate 
merge m:1 folioviv foliohog using "$bases\ingresos_temp.dta", nogenerate

recode trabajos (.=0)
recode tieneserviciodecuidado (.=0)

destring numren, replace
rename numren pmt
destring act_pnea1, replace force
destring act_pnea2, replace force
rename (tieneserviciodecuidado trabajos act_pnea1 act_pnea2) t_=
keep folioviv foliohog pmt t_* h_*
save "$bases\trabajos_temp.dta", replace
restore 

destring padre_id, replace force
destring madre_id, replace force
gen pmt = cond(missing(padre_id), cond(missing(madre_id), 1 , madre_id), padre_id)
merge m:1 folioviv foliohog pmt using "$bases\trabajos_temp.dta", nogenerate keep(1 3)
gen n=1
svyset up [weight=factor], strata(est_dis) singleunit(certainty)
svy:total n
svy:total n if numren=="01"
gen menorA24=edad<24
gen aus_padre=padre_id==.
gen aus_madre=madre_id==.
gen aus=aus_padre +2*aus_madre

foreach var of varlist disc* {
destring `var', replace force
replace `var' = 0 if `var'==.
}
egen disc=rowmin(disc*)
gen tienendiscapacidad=disc<4
label define sino 0 "No" 1 "Sí" 
label define aus 0 "sin ausencia" 1 "ausencia paterna" 2 "ausencia materna" 3 "ausencia biparental"
label values menorA24 aus_padre aus_madre tienendiscapacidad h_apoyoprograma sino
label values aus aus

*Se comienza a filtrar para obtener la Población objetivO: Modalidad A
gen esPoblacionObjetivoA=edad<cond(tienendiscapacidad, 6,4) & aus>0
label values esPoblacionObjetivoA sino
svy:total n, over(esPoblacionObjetivoA)level(95) cformat(%9.0f)

*Población objetivo que cumple con todas las caracteristicas
gen estatrabajando=t_trabajos>0
gen estabuscandoempleo=t_act_pnea1==1
gen estaestudiando=t_act_pnea1==4 | t_act_pnea2==4
gen cuentaconserviciodecuidado=t_tieneserviciodecuidado>0
//Los missing se cuentan como "mayor que"

gen cumplecriteriosA=esPoblacionObjetivoA & (estatrabajando | estaestudiando | estabuscandoempleo) & !cuentaconserviciodecuidado
label values estatrabajando  estaestudiando  estabuscandoempleo cuentaconserviciodecuidado cumplecriteriosA sino
svy:total n, over(cumplecriteriosA) level(95) cformat(%9.5f)
matrix list r(table)
svy: total n, over(cumplecriteriosA h_apoyoprograma) level(95) cformat(%9.5f)
svy:tab h_apoyoprograma cumplecriteriosA, percent
svy:total esPoblacionObjetivoA cumplecriteriosA, nolstretch cformat(%9.0f)

*Caracterizamos por sexo 
tab cumplecriteriosA sex [weight=factor]

*Caracterizamos por entidad
gen ent=substr(ubica_geo, 1, 2)
tab cumplecriteriosA [weight=factor]
tab ent cumplecriteriosA  [weight=factor]

*Categorización por mercado laboral
gen cumplecriteriosAPEA=esPoblacionObjetivoA & (estatrabajando) & !cuentaconserviciodecuidado
svy:total n, over(cumplecriteriosAPEA) level(95) cformat(%9.0f)

gen cumplecriteriosAPNEA=esPoblacionObjetivoA & (estaestudiando) & !cuentaconserviciodecuidado
svy:total n, over(cumplecriteriosAPNEA) level(95) cformat(%9.0f)

gen cumplecriteriosAPEA1=esPoblacionObjetivoA & (estabuscandoempleo) & !cuentaconserviciodecuidado
svy:total n, over(cumplecriteriosAPEA1) level(95) cformat(%9.0f)

total cumplecriteriosA cumplecriteriosAPEA cumplecriteriosAPEA1 cumplecriteriosAPNEA [w=factor], cformat(%9.0f)

save "$bases\poblacionA.dta", replace
clear all

*Se realiza la caracterización para conocer a los grupos prioritarios 

*Esto se realiza para la base consolidada de la modalidad A

*Se toma el ITER para conocer los municipios mayoritariamente indigenas o afromexiacanos
use "$bases\poblacionA.dta"
rename cve_munc clave_mun
sort clave_mun
save "$bases\poblacionA.dta",replace
merge clave_mun using "$bases\muni_ind2.dta"
keep if _merge==3
sort ent
tab cumplecriteriosAPEA  sex if mun_ind_afr==1 [w=factor]
save "$bases\poblacionA_mun_ind_adro.dta",replace

*Grado de marginación (CONAPO)
use "$bases\poblacionA.dta"
merge cve_munc using "$bases\grado_marginacion_2020.dta"
keep if _merge==3
drop if _merge==2
gen huerfa_gm=0
replace huerfa_gm=1 if gm_2020== "Alto" | gm_2020=="Muy Alto"
drop _merge
sort cve_munc
tab cumplecriteriosAPEA  sex if huerfa_gm==1 [w=factor]
tab huerfa_gm [w=factor]
save "$bases\poblacionA_mun_gm",replace

*Grado de rezago social (CONEVAL)
use "$bases\poblacionA.dta"
sort cve_munc
merge cve_munc using "$bases\base_rezago2020.dta"
gen rezago=0
replace rezago=1 if grs== "Alto" | grs=="Muy alto"
tab cumplecriteriosAPEA  sex if rezago==1 [w=factor]
save "$bases\poblacionA_grs",replace

*Zonas de alta o muy alta violencia (ZAP 2022) 
use "$bases\poblacionA.dta"
sort cve_munc
drop _merge
merge cve_munc using "$bases\zap_22.dta"
drop if _merge==2
drop _merge
replace huerfa_violencia=0 if huerfa_violencia==.
tab cumplecriteriosAPEA  sex if huerfa_violencia==1 [w=factor]

*3)Estimación de la modalidad B 

*CON BASE EN LOS DATOS DE:

*1) CENSO AMPLIADO DE POBLACION Y VIVIENDA 2020 (INEGI)

*2) PRINCIPALES RESULTADOS POR LOCALIDAD (ITER) DEL CENSO INEGI

*AMBAS DISPONIBLES EN https://www.inegi.org.mx/programas/ccpv/2020/#Microdatos

*3) GRADO DE MARGINACION MUNICIPAL (CONAPO) REALIZADO CON BASE EN EL CENSO 

*INEGI 2020: https://www.gob.mx/conapo/documentos/indices-de-marginacion-2020-284372

*4) ZONAS DE ATENCION PRIORITARIA RURAL (DOF 2022): https://www.dof.gob.mx/nota_detalle.php?codigo=5636711&fecha=29/11/2021 

*------------------------------------------------------
//Cargar base de datos del censo 2020 cuestionario ampliado
use "$bases\poblacion2020.dta"
keep ent mun sexo edad ident_madre ident_padre asisten factor

*--------------------------------------------------------
//Crear claves unívocas para municipios
gen primer_cero=""
replace primer_cero="0" if mun>=1 & mun<=9
gen segundo_cero=""
replace segundo_cero="0" if mun>=1 & mun<=99
gen muni=mun
tostring muni, replace
gen entidad=ent
gen tercer_cero=""
replace tercer_cero="0" if entidad<=9
egen cve_ent= concat (tercer_cero entidad)
tostring cve_ent, replace
egen cve_munc = concat ( cve_ent primer_cero segundo_cero muni)
tostring cve_munc, replace

*-------------------------------------------------------
//Definición de situacion de orfandad 

*Orfandad materna
gen huer_madre=0
replace huer_madre=1 if ident_madre==97
tab sexo huer_madre [w=factor] //Resultado

*Orfandad paterna
gen huer_padre=0
replace huer_padre=1 if ident_padre==97
tab huer_padre [w=factor]
tab sexo huer_padre [w=factor] //Resultado

*Orfandad por ambos padres
gen huer_ambos=0
replace huer_ambos=1 if ident_madre==97 & ident_padre==97
tab huer_ambos [w=factor]
tab sexo huer_ambos [w=factor] //Resultado

*-------------------------------------------------------
//Definicion de situacion de orfandad de 0 a 23 anios

*Definicion de rango de edad de 0 a 23 años
gen edad0a23=0
replace edad0a23=1 if (edad>=0 & edad<=23)  

*Orfandad materna de 0 a 23 anios (**Poblacion Objetivo**)
gen huer_madre23=0
replace huer_madre23=1 if huer_madre==1 & edad0a23==1
tab huer_madre23 [w=factor] 
tab sexo huer_madre23 [w=factor] 
tab ent huer_madre23 [w=factor] 

*Orfandad paterna de 0 a 23 anios
gen huer_padre23=0
replace huer_padre23=1 if huer_padre==1 & edad0a23==1
tab huer_padre23 [w=factor] 
tab sexo huer_padre23 [w=factor] 
tab ent huer_padre23 [w=factor] 

*Orfandad por ambos padres de 0 a 23 anios
gen huer_ambos23=0
replace huer_ambos23=1 if huer_ambos==1 & edad0a23==1
tab huer_ambos23 [w=factor] //Resultado: 
tab sexo huer_ambos23 [w=factor]
tab ent huer_ambos23 [w=factor]

*--------------------------------------------------------
//Caracterización de la poblacion objetivo por:

*Rangos de edad:
tab huer_madre if (edad>=0 & edad<=17) [w=factor]
tab huer_madre if (edad>=0 & edad<=2) [w=factor]
tab huer_madre if (edad>=3 & edad<=5) [w=factor]
tab huer_madre if (edad>=6 & edad<=8) [w=factor]
tab huer_madre if (edad>=9 & edad<=11) [w=factor]
tab huer_madre if (edad>=12 & edad<=14) [w=factor]
tab huer_madre if (edad>=15 & edad<=17) [w=factor]
tab huer_madre if (edad>=18 & edad<=23) [w=factor]
tab huer_madre if (edad>=18 & edad<=20) [w=factor]
tab huer_madre if (edad>=21 & edad<=23) [w=factor]

*Asistencia escolar
gen hm_23es=0
replace hm_23es=1 if huer_madre23==1 & asisten==1
tab sex hm_23es [w=factor]
tab entidad hm_23es [w=factor]

*Asistencia escolar por rangos de edad
gen hm_es=0
replace hm_es=1 if huer_madre==1 & asisten==1
tab hm_es if (edad>=0 & edad<=17) [w=factor]
tab hm_es if (edad>=0 & edad<=2) [w=factor]
tab hm_es if (edad>=3 & edad<=5) [w=factor]
tab hm_es if (edad>=6 & edad<=8) [w=factor]
tab hm_es if (edad>=9 & edad<=11) [w=factor]
tab hm_es if (edad>=12 & edad<=14) [w=factor]
tab hm_es if (edad>=15 & edad<=17) [w=factor]
tab hm_es if (edad>=18 & edad<=23) [w=factor]
tab hm_es if (edad>=18 & edad<=20) [w=factor]
tab hm_es if (edad>=21 & edad<=23) [w=factor]

*No asistencia escolar
gen hm_23nes=0
replace hm_23nes=1 if huer_madre23==1 & asisten==3
tab sex hm_23nes [w=factor]
tab entidad hm_23nes [w=factor]

*No asistencia escolar por rangos de edad
gen hm_nes=0
replace hm_nes=1 if huer_madre==1 & asisten==3
tab hm_nes if (edad>=0 & edad<=17) [w=factor]
tab hm_nes if (edad>=0 & edad<=2) [w=factor]
tab hm_nes if (edad>=3 & edad<=5) [w=factor]
tab hm_nes if (edad>=6 & edad<=8) [w=factor]
tab hm_nes if (edad>=9 & edad<=11) [w=factor]
tab hm_nes if (edad>=12 & edad<=14) [w=factor]
tab hm_nes if (edad>=15 & edad<=17) [w=factor]
tab hm_nes if (edad>=18 & edad<=23) [w=factor]
tab hm_nes if (edad>=18 & edad<=20) [w=factor]
tab hm_nes if (edad>=21 & edad<=23) [w=factor]                  
//Guardar la base de datos 
sort cve_munc
save "$data\huer23_2022.dta", replace

*Caracterización de la Población Objetivo

*Municipios indígenas y afromexicanos (ITER, CENSO 2020)
use "$bases\iter_nac_2020.dta"
keep entidad nom_ent mun nom_mun loc phog_ind pobtot pob_afro
drop if mun==0
keep if loc==0
destring, replace ignore(* N/D)

*Generamos la clave para la base que contine un identificador de municipios para la población indigena 
gen primer_cero=""
replace primer_cero="0" if mun>=1 & mun<=9
gen segundo_cero=""
replace segundo_cero="0" if mun>=1 & mun<=99
gen muni=mun
tostring muni, replace
gen tercer_cero=""
replace tercer_cero="0" if entidad<=9
egen ent= concat (tercer_cero entidad)
tostring ent, replace
egen cve_munc = concat (ent primer_cero segundo_cero muni)
tostring cve_munc, replace

*Porcentaje de población indigena
gen porcentaje_ind = phog_ind/pobtot 
gen mun_ind=1 if porcentaje_ind>=0.4
tab mun_ind

*Porcentaje de población afromexicana
gen porcentaje_afro = pob_afro/pobtot 
gen mun_afro=1 if porcentaje_afro>=0.4
tab mun_afro

*Porcentaje de población indígenas o afromexicana
gen mun_ind_afro=1 if mun_ind==1 | mun_afro==1
tab mun_ind_afro 
drop if mun_ind_afro==.
sort cve_munc
save "$data\pob_indafro.dta", replace

*Hacer el cruce de variables: entre la base "data\huer23_2022" y "pob_indafro"
use "$data\huer23_2022.dta"
merge cve_munc using "$data\pob_indafro.dta"

*Muncipios indígenas y afromexicanos donde viven los niños de 0 a 23 años en orfandad materna
tab huer_madre23 mun_ind_afro [w=factor] 
keep if _merge==3
drop _merge
tab mun_ind_afro sexo [w=factor] //identificacion por sexo de municipios indigenas o afromexicanos
save "$bases\huer23_2022_indigenasoafros.dta", replace

*Rezago social (CONEVAL 2020)
use "$\bases\base_rezago2020.dta"
tostring cve_munc,replace
gen rezago=0
replace rezago=1 if grs== "Muy alto" | grs=="Alto"
drop if rezago==0
tab rezago
sort cve_munc
save "$data\base_rezago.dta", replace
use "$data\huer23_2022.dta"
sort cve_munc
merge cve_munc using "$data\base_rezago.dta"
tab _merge [w=factor]
keep if _merge==3
drop _merge
tab huer_madre23 rezago [w=factor] 
sort cve_munc
tab rezago sexo [w=factor]
save "$data\huer23_2022_rezagosocial.dta", replace

*Grado de marginación (CONAPO, 2020)
use "$bases\grado_marginacion_2020.dta"
keep cve_ent nom_ent cve_mun nom_mun gm_2020
rename cve_mun cve_munc
tostring cve_munc,replace
gen marg=0
replace marg=1 if gm_2020== "Muy alto" | gm_2020=="Alto"
tab marg
keep if marg==1
sort cve_munc
save "$data\base_marginacion.dta", replace
use "$data\huer23_2022.dta"
sort cve_munc
merge cve_munc using "$data\base_marginacion.dta"
tab huer_madre23 marg [w=factor] 
tab _merge [w=factor]
keep if _merge==3
sort cve_munc
drop _merge
tab mar sexo [w=factor]
save "$data\huer23_2022_marginacion.dta", replace

*Alto nivel delictivo (ZAP rurales 2022)
use "$data\zap_2022.dta"
keep ClaveEntidad ClaveMunicipio NombreEntidad NombreMunicipio NivelDelictivo20202021 cve_munc
rename ClaveEntidad claveentidad
rename ClaveMunicipio clavemunicipio
rename NombreEntidad nombreentidad
rename NombreMunicipio nombremunicipio
rename NivelDelictivo20202021 niveldelictivo
tostring cve_munc,replace

*Idenficar municipios con alto nivel delictivo
tab niveldelictivo
gen alto_del=0
replace alto_del=1 if niveldelictivo=="ALTO"
tab alto_del
keep if alto_del==1
sort cve_munc
save "$data\base_niveldelictivo.dta", replace

**Asociamos huerfanos que vivan en municipios con alto nivel delictivo
use "$data\huer23_2022.dta"
sort cve_munc
duplicates report cve_munc
merge cve_munc using "$data\base_niveldelictivo.dta"
tab huer_madre23 alto_del [w=factor] 
tab _merge [w=factor]
keep if _merge==3
sort cve_munc
drop _merge
tab alto_del sexo [w=factor]
save "$data\huer23_2022_niveldelictivo.dta",replace


