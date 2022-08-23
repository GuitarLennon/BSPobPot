#delimit ;
cls;
capture log close results;

local outputlog = "./temp/" + subinstr("output`c(current_date)'`c(current_time)'.smcl", ":", "_", 99);
local dofraw = "./temp/taberu.xlsx";
local dofdta = "./temp/dof.dta";
local dofdta2 = "./temp/dof2.dta";

local marginacionraw = "./temp/IMM_2020.xls";
local marginaciondta = "./temp/IMM_2020.dta";

local originalDataset = "./sources/PoblacionA.dta";
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




/* Preparar archivo de análisis */
if(!fileexists("`marginaciondta'")  | ustrregexm("`*'", "marg", 1)) {
    /* No existe archivo dta de marginación */;

    di "`c(current_date)' `c(current_time)' Creando archivo de marginación";
    if(!fileexists("`marginacionraw'")) {
        /* Si no existe base csv */;
        di as error "No existe el archivo IML_2020 de marginación";
        exit;
    };

    import excel using "`marginacionraw'", clear sheet("IMM_2020") firstrow case(lower);

    label define gm_2020
        1 "Muy bajo"
        2 "Bajo"
        3 "Medio"
        4 "Alto"
        5 "Muy alto";

    encode gm_2020, gen(_gm_2020) label(gm_2020);
    rename (gm_2020 _gm_2020) (_gm_2020 gm_2020);
    duplicates drop;
    destring cve_ent, generate(ent);
    destring cve_mun, generate(mun);

    replace mun = mod(mun,1000);

    sort ent mun;
    save "`marginaciondta'", replace;    

};

if(!fileexists("`dofdta'")  | ustrregexm("`*'", "dof", 1)) {
    /* No existe archivo dta de DOF */
    di "`c(current_date)' `c(current_time)' Creando archivo de DOF";
    if(!fileexists("`dofraw'")) {
        /* Si no existe base csv */;
        di as error "No existe el archivo DOF de marginación";
        exit;
    };

    import excel "`dofraw'", sheet("Rurales") cellrange(A1:L1390) firstrow case(lower) clear;
    destring clavedeentidadfederativa, replace;
    destring clavedemunicipio , replace;

    rename clavedeentidadfederativa ent;
    gen mun = mod(clavedemunicipio, 1000);

    label define rezago
        0 "SIN DATO"
        1 "MUY BAJO"
        2 "BAJO"
        3 "MEDIO"
        4 "ALTO"
        5 "MUY ALTO";

    label define nivelDelictivo
        0 "SIN DATO"
        1 "BAJO"
        2 "MEDIO"
        3 "ALTO";

    label define municipiosIndígenasoAfr
        0 "SIN POBLACIÓN INDÍGENA"
        1 "MUNICIPIOS AFROMEXICANOS"
        2 "PRESENCIA INDÍGENA DISPERSA" 
        3 "PRESENCIA INDÍGENA"
        4 "MUNICIPIOS INDÍGENAS POR LEYES, DECRETOS O SENTENCIAS"
        5 "MUNICIPIOS INDÍGENAS"
        ;

    label define estrategiasIntegrales
        0 ""
        1 "AIFA"
        2 "TREN MAYA"
        3 "TREN TRANSÍTSMICO"
        4 "DOS BOCAS"
        5 "TREN INTERURBANO"
        ;

    encode gradoderezagosocial2020, gen(GradoDeRezagoSocial) label(rezago);

    encode niveldelictivo20202021, gen(nivelDelicitivo) label(nivelDelictivo);

    encode municipiosindígenasoafromexic, gen(municipiosIndígenasoAfr) label(municipiosIndígenasoAfr);

    encode estrategiasintegralesdedesarr, gen(estrategiaIntegral) label(estrategiasIntegrales);

    gen zonaAtenciónPrioritaria = 1;

    tab estrategiaIntegral;

    sort ent mun;

    gen municipioFronterizo = zonasfronterizas == "SI";

    gen municipioTurístico = zonasturísticas == "SI";

    keep ent mun GradoDeRezagoSocial nivelDelicitivo municipiosIndígenasoAfr municipioFronterizo municipioTurístico estrategiaIntegral;

    save "`dofdta'", replace;    

    import excel "`dofraw'", sheet("Urbanas") cellrange(A1:L1618) firstrow case(lower) clear;

    gen zonaAtenciónPrioritaria = 1;

    gen ent = int(clavedelocalidad / 10000000);

    gen mun = mod(int(clavedelocalidad / 10000), 1000);

    gen loc = mod(clavedelocalidad, 10000);

    label define estrategiasIntegrales
        0 ""
        1 "AIFA"
        2 "TREN MAYA"
        3 "TREN TRANSÍTSMICO"
        4 "DOS BOCAS"
        5 "TREN INTERURBANO"
        ;

    encode estrategiasintegralesdedesarr, gen(estrategiaIntegral) label(estrategiasIntegrales);

    tab estrategiaIntegral;

    gen municipioFronterizo = zonasfronterizas == "SI";

    gen municipioTurístico = zonasturísticas == "SI";
    
    keep ent mun loc zonaAtenciónPrioritaria municipioFronterizo municipioTurístico estrategiaIntegral;

    duplicates drop;
    
    save "`dofdta2'", replace;    
    
};


use "`marginaciondta'", clear;

keep ent mun gm_2020;

merge 1:1 ent mun using "`dofdta'", nogenerate update;

recode GradoDeRezagoSocial (.=0);
recode nivelDelicitivo (.=0);
recode municipiosIndígenasoAfr (.=0);
recode estrategiaIntegral (.=0);
recode municipioFronterizo (.=0);
recode municipioTurístico (.=0);

save "_temp.dta", replace;

use "`originalDataset'", clear;

gen mun = substr(ubica_geo, 3, 3);

destring ent, replace;
destring mun, replace;

merge m:1 ent mun using "_temp.dta", nogenerate keep(1 3);

gen mun_afromex = municipiosIndígenasoAfr == 1; 
label variable mun_afromex "Vive a municipios afromexicanos";

gen mun_indigena = municipiosIndígenasoAfr == 5;
label variable mun_indigena "Vive en municipios indígenas";

gen gm_alto = gm_2020 == 4; 
label variable gm_alto "Grado de marginación alto";

gen gm_malto = gm_2020 == 5; 
label variable gm_malto "Grado de marginación muy alto";

gen re_alto = GradoDeRezagoSocial == 4; 
label variable re_alto "Grado de rezago alto";

gen re_malto = GradoDeRezagoSocial == 5; 
label variable re_malto "Grado de rezago muy alto";

gen nd_alto = nivelDelicitivo == 3; 
label variable nd_alto "Nivel delictivo alto";

gen alguno = mun_afromex | mun_indigena | gm_alto | gm_malto | re_alto | re_malto | nd_alto;

destring sexo, replace;

total mun_afromex mun_indigena gm_alto gm_malto re_alto re_malto nd_alto alguno n [w=factor] if cumplecriteriosA, cformat(%9.0f) ;

total mun_afromex mun_indigena gm_alto gm_malto re_alto re_malto nd_alto alguno n [w=factor] if cumplecriteriosA, over(sexo) cformat(%9.0f) ;

di as err "No se muestrearon municipios afromexicanos";