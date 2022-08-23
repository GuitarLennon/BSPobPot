#delimit ;
cls;
capture log close results;

local marginacionraw = "IMM_2020.xls";
local marginaciondta = "IMM_2020.dta";
local dofraw = "taberu.xlsx";
local dofdta = "dof.dta";
local dofdta2 = "dof2.dta";

local csvFileName = "Personas00.csv";
local rawFileName = "Personas00.raw";
local dtaFileName = "Personas00.dta";
local tempBase = "tempBase.dta"; 
local outputFileName = "output2.xlsx";
local outputlog = subinstr("output`c(current_date)'`c(current_time)'.smcl", ":", "_", 99);

log using "`outputlog'", replace text name(results);
di "Iniciando do `c(current_date)' `c(current_time)' Starting do at";

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
AUTOR: Dr. Arturo Juárez Flores
FECHA: 25 de junio del 2022
====================================================================
*/

/* Preparar archivo de análisis */
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


if(!fileexists("`dtaFileName'") | ustrregexm("`*'", "file", 1)){
    /* Si no existe base dta */;

    di "`c(current_date)' `c(current_time)' Creando archivo de datos";
    if(!fileexists("`csvFileName'"))
    { /* Si no existe base csv */;
        di as error "No existe el archivo de origen";
        exit;
    };

    /* Importar datos */
    if(!fileexists("`rawFileName'")){
        /* Si no existe el archivo */;
        import delimited using "`csvFileName'", clear;
        save "`rawFileName'";
    };
    else {
        /* Cargar el archivo */;
        use "`rawFileName'", clear;
    };

    rename loc50k loc;
    /* Preservar las variables de reelevancia */
    keep ent mun loc id_viv id_persona  cobertura estrato upm factor clavivp numper sexo edad parentesco ident_madre ident_padre nacionalidad afrodes regis_nac dhsersal1 dhsersal2 dis_ver dis_oir dis_caminar dis_recordar dis_banarse dis_hablar dis_mental hlengua perte_indigena asisten nivacad ident_pareja conact sittra servicio_medico;

    /* Etiquetar datos */
    label variable afrodes "¿Es afrodescendiente?";

    label define sinone 1 "Sí" 3 "No" 9 "No especificado";

    label define sino 0 "No" 1 "Sí";
    
    label define sexo 1 "Hombre" 3 "Mujer";

    label define disc
        1 "Sin dificultad" 
        2 "Poca dificultad"
        3 "Mucha dificultad"
        4 "No puede hacerlo"
        8 "Desconocido"
        9 "No especificado";

    label define nivacad
		00	"Ninguno"
		01	"Preescolar"
		02	"Primaria"
		03	"Secundaria"
		04	"Preparatoria o bachillerato general"
		05	"Bachillerato tecnológico"
		06	"Estudios técnicos o comerciales con primaria terminada"
		07	"Estudios técnicos o comerciales con secundaria terminada"
		08	"Estudios técnicos o comerciales con preparatoria terminada"
		09	"Normal con primaria o secundaria terminada"
		10	"Normal de licenciatura"
		11	"Licenciatura"
		12	"Especialidad"
		13	"Maestría"
		14	"Doctorado"
		99	"No especificado";

    label define conact
		10	"Trabajó"
		13	"Busca que trabaja"
		14	"Jubilado que trabaja"
		15	"Estudiante que trabaja"
		16	"Hogar que trabaja"
		17	"Incapacitado que trabaja"
		18	"Otra actividad y que trabaja"
		19	"Sin información que trabaja"
		20	"Con trabajo, no trabajó"
		30	"Buscó trabajo"
		40	"Pensionado"
		50	"Estudiante"
		60	"Hogar"
		70	"Incapacitado"
		80	"No trabaja"
		99	"No especificado";

    label define cobertura 
        1 "Municipio censado"
        2 "Municipio muestreado"
        3 "Municipio con muestra insuficiente";

    label define clavivp 
        01	"Casa única "
        02	"Casa que comparte terreno"
        03	"Casa dúplex"
        04	"Departamento en edificio"
        05	"Vecindad o cuartería"
        06	"Cuarto de azotea"
        07	"Local no habitable"
        08	"Vivienda móvil"
        09	"Refugio"
        99	"No especificado";

    label define parentesco 
		101 "Jefa(e)"
		201 "Esposa(o)"
		202 "Concubina(o) o unión libre"
		203 "Amante o querida(o)"
		301 "Hija(o)"
		302 "Hija(o) adoptiva(o)"
		303 "Hijastra(o)"
		304 "Hija(o) de crianza"
		401 "Madre o padre"
		402 "Madrastra o padrastro"
		403 "Hermana(o)"
		404 "Media(o) hermana(o)"
		405 "Hermanastra(o)"
		406 "Abuela(o)"
		407 "Bisabuela(o) o tatarabuela(o)"
		408 "Nieta(o)"
		409 "Nietastra(o)"
		410 "Esposa(o) de nieto(a)"
		411 "Bisnieta(o) o tataranieta(o)"
		412 "Tía(o)"
		413 "Sobrina(o)"
		414 "Sobrina(o) nieta(o)"
		415 "Prima(o)"
		416 "Suegra(o)"
		417 "Consuegra(o)"
		418 "Nuera o yerno"
		419 "Cuñada(o)"
		420 "Concuña(o)"
		421 "Madrina o padrino"
		422 "Ahijada(o)"
		423 "Comadre o compadre"
		424 "Otros familiares"
		501 "Sin parentesco"
		502 "Tutor(a)"
		503 "Tutelada(o)"
		601 "Trabajador(a) doméstico(a)"
		611 "Esposa(o) de trabajador(a) doméstico(a)"
		612 "Hija(o) de trabajador(a) doméstico(a)"
		613 "Otro pariente de trabajador(a) doméstico(a)"
		701 "Huésped"
		999 "Parentesco no especificado";

    label define ident 
        96	"vive en otra vivienda?"
        97	"ya falleció?"
        98	"No sabe"
        99	"No especificado";

    label define regis_nac
        1 "Sí, en México"
        2 "Sí, otro país"
        3 "No"
        9 "No especificado";

    label define dhsersal
		01 "el Seguro Social (IMSS)"
		02 "el ISSSTE"
		03 "el ISSSTE estatal"
		04 "PEMEX, Defensa o Marina"
		05 "el Seguro Popular o para una Nueva Generación (Siglo XXI) o Instituto de Salud para el Bienestar"
		06 "el IMSS-PROSPERA o IMSS-BIENESTAR"
		07 "un seguro privado"
		08 "otra institución"
		09 "No está afiliada(o) ni tiene derecho a servicios médicos"
		99 "No especificado";

    label define sittra
		1 "empleada(o) u obrera(o)"
		2 "jornalera(o) o peón(a)"
		3 "ayudante con pago"
		4 "patrón(a) o empleador(a)"
		5 "trabajador(a) por cuenta propia"
		6 "trabajador(a) sin pago"
		9 "No especificado";

    label define servicio_medico
        5 "Sí"
        6 "No"
        7 "No especificado";    

    label define dis_mental 
        5 "Sí" 6 "No" 9 "No especificado";
    
    label values dis_mental dis_mental;
    label values servicio_medico servicio_medico;
    label values sittra sittra;
    label values dhsersal1 dhsersal2 dhsersal;
    label values regis_nac regis_nac;
    label values ident_madre ident_padre ident;
    label values parentesco parentesco;
    label values sexo sexo;
    label values clavivp clavivp;
    label values cobertura cobertura;
    label values afrodes asisten nacionalidad hlengua perte_indigena sinone;
    label values conact conact;
    label values nivacad nivacad;
    label values dis_ver dis_oir dis_caminar dis_recordar dis_banarse dis_hablar disc;    
    label values dis_mental dis_mental;
    
    gen disc = inrange(dis_ver, 2, 4) | inrange(dis_oir, 2, 4) | inrange(dis_caminar, 2, 4) | inrange(dis_recordar, 2, 4) | inrange(dis_banarse, 2, 4) | inrange(dis_hablar, 2, 4) |dis_mental == 5;

    /* Guardar provisionalmente */
    preserve;
    
    /* Crear microbase de madres y padres o tutores */    
    keep id_viv numper afrodes disc hlengua perte_indigena asisten nivacad ident_pareja conact sittra dhsersal1 dhsersal2 servicio_medico;

    /* Renombrar las variables con el prefijo p_ para saber que pertenecen a padre, madre o tutor*/
    rename (afrodes-disc) p_=;

    /* Generar llaves de unión */
    gen ident_madre = numper;
    gen ident_padre = numper;
    gen ident_tutor = numper;
    
    sort id_viv numper;
    drop numper;
    save "`tempBase'", replace;

    /* Recuperar base original */
    restore;

    /*
        Pegar los datos de madres, padres y tutores con los niños
    */
    /* Verificación de duplicidad */
    count;

    /* Pegar madres */
    merge m:1 id_viv ident_madre using "`tempBase'", nogenerate keepusing(id_viv ident_madre p_*) keep(1 3) nolabel;

    rename p_* m_*;

    /* Verificación de duplicidad */
    count;

    /* Pegar padres */
    gen ident_tutor = 1;
    merge m:1 id_viv ident_tutor using "`tempBase'", nogenerate keepusing(id_viv ident_tutor p_*) keep(1 3) nolabel;

    rename p_* t_*;

    /* Verificación de duplicidad */
    count;

    /* Pegar tutores */
    merge m:1 id_viv ident_padre using "`tempBase'", nogenerate keepusing(id_viv ident_padre p_*) keep(1 3) nolabel;

    /* Verificación de duplicidad */
    count;

    /* pegar grado de marginación */
    merge m:1 ent mun using "`marginaciondta'", nogenerate keepusing(ent mun gm_2020) keep(1 3);
    
    /* Verificación de duplicidad */
    count;

    save _test.dta, replace;

    /* Pegar declaratoria DOF */
    gen zonaAtenciónPrioritaria = 0;
    gen municipioTurístico = 0;
    gen municipioFronterizo = 0;
    gen estrategiaIntegral = 0; 

    merge m:1 ent mun using "`dofdta'", nogenerate update keep(1 3);

    merge m:1 ent mun loc using "`dofdta2'", nogenerate update nolabel keep(1 3);


    /* Verificación de duplicidad */
    count;

    sort id_viv numper;

    /* Establecer metodología de encuesta. */
    /*
        La variable estrato es única dentro de upm por lo que se omite
        A recomendación de INEGI, no se corrige población finita.
    */
    svyset upm [weight=factor], strata(estrato)  singleunit(certainty)
    
    /* Crear variables de trabajo */
        gen n = 1;
        gen menorA24 = edad <= 23 if !missing(edad);


        /*
            Generar los subconjuntos de ausencia
                Vive en otra vivienda?	96
                Ya falleció?		    97
                No sabe		            98
                No especificado		    99
        */

        gen orf_madre = ident_madre==97;
        gen orf_padre = ident_padre==97;
        gen orf = orf_madre * 2 + orf_padre;
        gen aus_madre = ident_madre==96 | ident_madre==97;
        gen aus_padre = ident_padre==96 | ident_padre==97;
        gen aus = aus_madre * 2 + aus_padre;
        gen aus_madre_tc = ident_madre >= 96;
        gen aus_padre_tc = ident_padre >= 96;
        gen aus_tc = aus_madre_tc * 2 + aus_padre_tc;

        /*
            Marcar la población objetivo del programa
        */
            gen poblObjProgr = inrange(edad, 0, 23) & aus_tc;


            /* Niños con ausencia por todas las cáusas
                Población objetivo modalidad A)
                3.2 a)
                ... Bajo esta modalidad, se apoyarán a las niñas y niños desde recién nacidos hasta un día antes de cumplir los cuatro años de edad, o hasta un día antes de cumplir los 6 años de edad en el caso de las personas con discapacidad, que están en situación de vulnerabilidad por la ausencia temporal o permanente de uno o ambos padres, debido a que no reside(n) en la misma vivienda o no está(n) presente(s) por causas como el abandono y la búsqueda de mejores condiciones socioeconómicas y una mejor calidad de vida.

             */
            gen A_rangoEdad = inrange(edad, 0, cond(disc, 5, 3));

            gen A_ppot = A_rangoEdad & aus_tc;

            /*
                Criterio de elegibilidad 1.
                Que la madre, padre solo o tutor esté trabajando, buscando empleo o estudiando y  [...]
            */

            /* Padre, madre o tutor que trabaja */
            gen c_trabaja = inrange(cond(aus_madre_tc & aus_padre_tc, t_conact,
                cond(aus_padre_tc, m_conact, p_conact
                )
            ), 10, 19);

            /* Padre, madre o tutor que busca trabajo */
            gen c_busca = cond(aus_madre_tc & aus_padre_tc, t_conact,
                cond(aus_padre_tc, m_conact, p_conact
                )
            ) == 30;

            /* Padre, madre o tutor que se considera estudiante */
            gen c_estudia = cond(aus_madre_tc & aus_padre_tc, t_conact,
                cond(aus_padre_tc, m_conact, p_conact)
            ) == 50;

            /* Padre, madre o tutor que asiste a la escuela */
            gen c_asisteEscuela = cond(aus_madre_tc & aus_padre_tc, t_asisten,
                cond(aus_padre_tc, m_asisten, p_asisten)
            ) == 1;

            /* 
                Criterio de elebigilidad 1 (pte 2)

                y no cuenten con el servicio de cuidado y atención infantil, a través de instituciones públicas de seguridad social u otros medios.
                
                Padre, madre o tutor que tiene prestación de IMSS o ISSSTE 
            */
            gen c_prestacion = cond(aus_madre_tc & aus_padre_tc, t_servicio_medico,
                cond(aus_padre_tc, m_servicio_medico, p_servicio_medico
                )
            ) == 5;


            /* Marcar población objetivo modalidad A */
            gen A_pobj = A_rangoEdad & aus_tc & !c_prestacion &
                (c_trabaja | c_estudia | c_busca | c_asisteEscuela);

            /* 
                Prioritaria

                Municipios indígenas o con población afromexicana de alto y muy alto grado de rezago social y marginación o alto índice de violencia. Zonas fronterizas. 
                --> Zonas fronterizas (César).
                --> Zonas turísticas (Gerardo).
                --> Estrategias integrales de desarrollo (César). AIFA, Tren maya, Transítsmico, Dos bocas y Tren interurbano

                Criterio 3 -- Pertenece a ?
            */


            gen altaMarginacion = inrange(gm_2020, 4, 5);

            gen indiceDelictivoAlto = nivelDelicitivo == 3;

            gen rezagoSocialAltoYMuyAlto = inrange(GradoDeRezagoSocial, 4, 5);

            gen municipioIndígena = municipiosIndígenasoAfr >= 4;

            gen municipioAfromexicanos = municipiosIndígenasoAfr == 1;



            gen municipioEstrategiaIntegral = estrategiaIntegral != 0;


            gen A_ppri = A_pobj & (altaMarginacion | indiceDelictivoAlto | rezagoSocialAltoYMuyAlto | municipioIndígena | municipioAfromexicanos | municipioFronterizo | municipioTurístico | municipioEstrategiaIntegral);
            
        /*
            Marcar la población objetivo modalidad B

            Niñas, niños, adolescentes y jóvenes en orfandad materna, que se encuentran preferentemente en zonas con población mayoritariamente indígena, afromexicanos, zonas con mayor grado de marginación o zonas con altos índices de violencia, en donde la madre ha fallecido, así como aquellas hijas e hijos en donde la jefa de familia se encontraba afiliada al Programa Seguro de Vida para Jefas de Familia, vigente hasta el ejercicio fiscal 2020.

            Variables identificadas: 
            * orfandad materna. <-- orf_madre == 1
            * población mayoritariamente indígena. <-- perte_indigena, hlengua
            * afromexicanos. <-- afromexicanos
            * Niños niñas adolescentes y jóvenes. <-- edad

        */
            gen B_ppot = menorA24 & orf_madre;

            gen B_pobj = menorA24 & orf_madre;

            gen B_ppri = B_pobj & (municipioAfromexicanos | municipioIndígena) /* & (c_afrodesc | c_indig) */;

        /* Etiquetar */
        label variable orf "Categoría de orfandad";
        label variable orf_madre "Orfandad materna";
        label variable orf_padre "Orfandad paterna";
        label variable aus "Categoría de orfandad";
        label variable aus_madre "Ausencia materna";
        label variable aus_padre "Ausencia paterna";
        label variable aus_tc "Ausencia por todas las causas";
        label variable aus_madre_tc "Ausencia materna por todas las causas";
        label variable aus_padre_tc "Ausencia paterna por todas las causas";
        
        label define categs 0 "Ninguno" 1 "Padre" 2 "Madre" 3 "Ambos";
        label values aus aus_tc orf categs;
        
        label values orf_madre orf_padre aus_madre aus_padre aus_madre_tc aus_padre_tc  A_rangoEdad c_asisteEscuela c_busca c_estudia c_trabaja menorA24 disc municipioAfromexicanos municipioIndígena altaMarginacion A_pobj A_ppot A_ppri B_ppot B_pobj B_ppri sino;

    save "`dtaFileName'", replace;
};
else {
    /* Ya existe archivo */;
    use "`dtaFileName'", clear;
};


/* Población potencial A por entidad*/
di "`c(current_date)' `c(current_time)' Analisis (1/13)";
svy:tab ent, subpop(A_ppot) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("1") replace;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población objetivo A por entidad */
di "`c(current_date)' `c(current_time)' Analisis (2/13)";
svy:tab ent, subpop(A_pobj) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("2") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población objetivo A por edad y sexo */
di "`c(current_date)' `c(current_time)' Analisis (3/13)";
svy:tab edad sexo, subpop(A_pobj) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;
    
    putexcel set "`outputFileName'", sheet("3") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población objetivo A por edad y discapacidad */
di "`c(current_date)' `c(current_time)' Analisis (4/13)";
svy:tab edad disc, subpop(A_pobj) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("4") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Criterios de priorización A */
di "`c(current_date)' `c(current_time)' Analisis (5/13)";
svy:mean altaMarginacion indiceDelictivoAlto rezagoSocialAltoYMuyAlto municipioIndígena municipioAfromexicanos municipioFronterizo municipioTurístico municipioEstrategiaIntegral if A_pobj;

    putexcel set "`outputFileName'", sheet("5") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Criterios de priorización A */
di "`c(current_date)' `c(current_time)' Analisis (5/13)";
svy:mean altaMarginacion indiceDelictivoAlto rezagoSocialAltoYMuyAlto municipioIndígena municipioAfromexicanos municipioFronterizo municipioTurístico municipioEstrategiaIntegral if A_pobj, over(A_ppri);

    putexcel set "`outputFileName'", sheet("5") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;


/* Población prioritaria A por entidad */
di "`c(current_date)' `c(current_time)' Analisis (6/13)";
svy:tab ent, subpop(A_ppri) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("6") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población prioritaria A por edad y sexo */
di "`c(current_date)' `c(current_time)' Analisis (7/13)";
svy:tab edad sexo, subpop(A_ppri) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("7") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población potencial B por entidad*/
di "`c(current_date)' `c(current_time)' Analisis (8/13)";
svy:tab ent, subpop(B_ppot) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("8") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población objetivo B por entidad */
di "`c(current_date)' `c(current_time)' Analisis (9/13)";
svy:tab ent, subpop(B_pobj) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("9") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población objetivo B por edad y sexo */
di "`c(current_date)' `c(current_time)' Analisis (10/13)";
svy:tab edad sexo, subpop(B_pobj) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("10") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Criterios de priorización B */
di "`c(current_date)' `c(current_time)' Analisis (11/13)";
/*
svy:mean c_afrodesc c_indig, over(B_pobj);

    putexcel set "`outputFileName'", sheet("11") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;
*/

/* Población objetivo B por entidad */
di "`c(current_date)' `c(current_time)' Analisis (12/13)";
svy:tab ent, subpop(B_ppri) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("12") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

/* Población objetivo B por edad y sexo */
di "`c(current_date)' `c(current_time)' Analisis (13/13)";
svy:tab edad sexo, subpop(B_ppri) count cellwidth(20) format(%9.0g);
svy:tab , ci cellwidth(20) format(%9.0g) percent;

    putexcel set "`outputFileName'", sheet("13") modify;
    display as text "    Escribiendo datos en excel";
    matrix b = r(table)';
    putexcel A1 = matrix(b), names;
    putexcel save;

di "Terminado en: " c(current_date) c(current_time);
log close results;

/* Solo válido si está instalado visual studio code */
capture shell code "`outputlog'";

