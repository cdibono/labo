# realiza Feature Engineering sobre el dataset original
# Este script con seguridad va a ser modificado por los alumnos
# para agregar su propio FE, al menos en la funcion AgregarVariables

#Necesita para correr en Google Cloud
#256 GB de memoria RAM
#300 GB de espacio en el disco local
#8 vCPU


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("Rcpp")
require("rlist")
require("yaml")

require("lightgbm")

source( "~/labo/src/lib/exp_lib.r" )

#------------------------------------------------------------------------------

ReportarCampos  <- function( dataset )
{
  cat( "La cantidad de campos es ", ncol(dataset) , "\n" )
}
#------------------------------------------------------------------------------
#Agrega al dataset una variable que va de 1 a 12, el mes, para que el modelo aprenda estacionalidad

AgregarMes  <- function( dataset )
{
  gc()
  dataset[  , mes := foto_mes %% 100 ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Elimina las variables que uno supone hace Data Drifting

DriftEliminar  <- function( dataset, variables )
{
  gc()
  dataset[  , c(variables) := NULL ]
  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Autor:  Santiago Dellachiesa, UAustral 2021
#A las variables que tienen nulos, les agrega una nueva variable el dummy de is es nulo o no {0, 1}

DummiesNA  <- function( dataset )
{
  gc()
  nulos  <- colSums( is.na(dataset[ foto_mes %in% PARAM$const$futuro ]) )  #cuento la cantidad de nulos por columna
  colsconNA  <- names( which(  nulos > 0 ) )

  dataset[ , paste0( colsconNA, "_isNA") :=  lapply( .SD,  is.na ),
             .SDcols= colsconNA]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Corrige poniendo a NA las variables que en ese mes estan da√±adas

Corregir  <- function( dataset )
{
  gc()
  #acomodo los errores del dataset

  dataset[ foto_mes==201801,  internet   := NA ]
  dataset[ foto_mes==201801,  thomebanking   := NA ]
  dataset[ foto_mes==201801,  chomebanking_trx   := NA ]
  dataset[ foto_mes==201801,  tcallcenter   := NA ]
  dataset[ foto_mes==201801,  ccallcenter_trx   := NA ]
  dataset[ foto_mes==201801,  cprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_personales   := NA ]
  dataset[ foto_mes==201801,  mprestamos_hipotecarios  := NA ]
  dataset[ foto_mes==201801,  ccajas_trx   := NA ]
  dataset[ foto_mes==201801,  ccajas_consultas   := NA ]
  dataset[ foto_mes==201801,  ccajas_depositos   := NA ]
  dataset[ foto_mes==201801,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==201801,  ccajas_otras   := NA ]

  dataset[ foto_mes==201806,  tcallcenter   :=  NA ]
  dataset[ foto_mes==201806,  ccallcenter_trx   :=  NA ]

  dataset[ foto_mes==201904,  ctarjeta_visa_debitos_automaticos  :=  NA ]
  dataset[ foto_mes==201904,  mtarjeta_visa_debitos_automaticos := NA ]
  dataset[ foto_mes==201904,  Visa_mfinanciacion_limite := NA ]

  dataset[ foto_mes==201905,  mrentabilidad     := NA ]
  dataset[ foto_mes==201905,  mrentabilidad_annual     := NA ]
  dataset[ foto_mes==201905,  mcomisiones      := NA ]
  dataset[ foto_mes==201905,  mpasivos_margen  := NA ]
  dataset[ foto_mes==201905,  mactivos_margen  := NA ]
  dataset[ foto_mes==201905,  ctarjeta_visa_debitos_automaticos  := NA ]
  dataset[ foto_mes==201905,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201905,  mcomisiones_otras := NA ]

  dataset[ foto_mes==201910,  mpasivos_margen   := NA ]
  dataset[ foto_mes==201910,  mactivos_margen   := NA ]
  dataset[ foto_mes==201910,  ccomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones_otras := NA ]
  dataset[ foto_mes==201910,  mcomisiones       := NA ]
  dataset[ foto_mes==201910,  mrentabilidad     := NA ]
  dataset[ foto_mes==201910,  mrentabilidad_annual        := NA ]
  dataset[ foto_mes==201910,  chomebanking_trx  := NA ]
  dataset[ foto_mes==201910,  ctarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  ctarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  mtarjeta_visa_descuentos    := NA ]
  dataset[ foto_mes==201910,  mtarjeta_master_descuentos  := NA ]
  dataset[ foto_mes==201910,  ccajeros_propios_descuentos := NA ]
  dataset[ foto_mes==201910,  mcajeros_propios_descuentos := NA ]

  dataset[ foto_mes==202001,  cliente_vip   := NA ]

  dataset[ foto_mes==202006,  active_quarter   := NA ]
  dataset[ foto_mes==202006,  internet   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad   := NA ]
  dataset[ foto_mes==202006,  mrentabilidad_annual   := NA ]
  dataset[ foto_mes==202006,  mcomisiones   := NA ]
  dataset[ foto_mes==202006,  mactivos_margen   := NA ]
  dataset[ foto_mes==202006,  mpasivos_margen   := NA ]
  dataset[ foto_mes==202006,  mcuentas_saldo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_debito_trx   := NA ]
  dataset[ foto_mes==202006,  mautoservicio   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_visa_trx   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_visa_consumo   := NA ]
  dataset[ foto_mes==202006,  ctarjeta_master_trx   := NA ]
  dataset[ foto_mes==202006,  mtarjeta_master_consumo   := NA ]
  dataset[ foto_mes==202006,  ccomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  mcomisiones_otras   := NA ]
  dataset[ foto_mes==202006,  cextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  mextraccion_autoservicio   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos   := NA ]
  dataset[ foto_mes==202006,  ccheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_depositados_rechazados   := NA ]
  dataset[ foto_mes==202006,  ccheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  mcheques_emitidos_rechazados   := NA ]
  dataset[ foto_mes==202006,  tcallcenter   := NA ]
  dataset[ foto_mes==202006,  ccallcenter_trx   := NA ]
  dataset[ foto_mes==202006,  thomebanking   := NA ]
  dataset[ foto_mes==202006,  chomebanking_trx   := NA ]
  dataset[ foto_mes==202006,  ccajas_trx   := NA ]
  dataset[ foto_mes==202006,  ccajas_consultas   := NA ]
  dataset[ foto_mes==202006,  ccajas_depositos   := NA ]
  dataset[ foto_mes==202006,  ccajas_extracciones   := NA ]
  dataset[ foto_mes==202006,  ccajas_otras   := NA ]
  dataset[ foto_mes==202006,  catm_trx   := NA ]
  dataset[ foto_mes==202006,  matm   := NA ]
  dataset[ foto_mes==202006,  catm_trx_other   := NA ]
  dataset[ foto_mes==202006,  matm_other   := NA ]
  dataset[ foto_mes==202006,  ctrx_quarter   := NA ]
  dataset[ foto_mes==202006,  tmobile_app   := NA ]
  dataset[ foto_mes==202006,  cmobile_app_trx   := NA ]


  dataset[ foto_mes==202010,  internet  := NA ]
  dataset[ foto_mes==202011,  internet  := NA ]
  dataset[ foto_mes==202012,  internet  := NA ]
  dataset[ foto_mes==202101,  internet  := NA ]

  dataset[ foto_mes==202009,  tmobile_app  := NA ]
  dataset[ foto_mes==202010,  tmobile_app  := NA ]
  dataset[ foto_mes==202011,  tmobile_app  := NA ]
  dataset[ foto_mes==202012,  tmobile_app  := NA ]
  dataset[ foto_mes==202101,  tmobile_app  := NA ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#Esta es la parte que los alumnos deben desplegar todo su ingenio

AgregarVariables  <- function( dataset )
{
  gc()
  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ]
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ]
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status
  dataset[ , mv_status01       := pmax( Master_status,  Visa_status, na.rm = TRUE) ]
  dataset[ , mv_status02       := Master_status +  Visa_status ]
  dataset[ , mv_status03       := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ]
  dataset[ , mv_status04       := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ]
  dataset[ , mv_status05       := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ]

  dataset[ , mv_status06       := ifelse( is.na(Visa_status), 
                                          ifelse( is.na(Master_status), 10, Master_status), 
                                          Visa_status)  ]

  dataset[ , mv_status07       := ifelse( is.na(Master_status), 
                                          ifelse( is.na(Visa_status), 10, Visa_status), 
                                          Master_status)  ]


  #combino MasterCard y Visa
  dataset[ , mv_mfinanciacion_limite := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ]

  dataset[ , mv_Fvencimiento         := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ]
  dataset[ , mv_Finiciomora          := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
  dataset[ , mv_msaldototal          := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ]
  dataset[ , mv_msaldopesos          := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ]
  dataset[ , mv_msaldodolares        := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumospesos       := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mconsumosdolares     := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_mlimitecompra        := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ]
  dataset[ , mv_madelantopesos       := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ]
  dataset[ , mv_madelantodolares     := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ]
  dataset[ , mv_fultimo_cierre       := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ]
  dataset[ , mv_mpagado              := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ]
  dataset[ , mv_mpagospesos          := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ]
  dataset[ , mv_mpagosdolares        := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ]
  dataset[ , mv_fechaalta            := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ]
  dataset[ , mv_mconsumototal        := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ]
  dataset[ , mv_cconsumos            := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ]
  dataset[ , mv_cadelantosefectivo   := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ]
  dataset[ , mv_mpagominimo          := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ]

  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra:= Master_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_Visa_mlimitecompra  := Visa_mlimitecompra / mv_mlimitecompra ]
  dataset[ , mvr_msaldototal         := mv_msaldototal / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos         := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2        := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares       := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2      := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos      := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares    := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos      := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares    := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado             := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos         := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares       := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal       := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo         := mv_mpagominimo  / mv_mlimitecompra ]

  #Aqui debe usted agregar sus propias nuevas variables
  #TEAM ACV
  # polinÛmica
  dataset[ , varnueva_36 := mcuentas_saldo * 0.11 + cliente_edad * 0.04 + Master_fechaalta * 0.04 + mactivos_margen * 0.09 +
             mrentabilidad_annual * 0.06 + mpayroll * 0.15 + ctrx_quarter * 0.63 + cpayroll_trx * 0.23 + mpasivos_margen * 0.19+
             mtarjeta_visa_consumo * 0.12]
  dataset[ , varnueva_11 := ctrx_quarter * mcaja_ahorro ]
  dataset[ , varnueva_27 := mtarjeta_visa_consumo * mcaja_ahorro ]
  dataset[ , varnueva_13 := ctrx_quarter * mtarjeta_visa_consumo ]
  dataset[ , varnueva_77 := cpayroll_trx   / mcomisiones_mantenimiento  ]
  dataset[ , varnueva_58 := mactivos_margen  / mprestamos_personales  ]
  dataset[ , varnueva_02 := cliente_edad * mcuentas_saldo ]
  dataset[ , varnueva_58 := mactivos_margen  / mprestamos_personales  ]
  
  
  ### Disclaimer a partir de ac· es un HURTO literal a los compaÒeros DRAGO y LARRETA 
  ### que muy amablemente compartieron su cÛdigo en GITHUB, los invito a HURTAR con toda libertad
  ### mi cÛdigo.
  
  #LIQUIDEZ DEL CLIENTE (disp. inmediata de efectivo por parte de el cliente) (BRAIAN)##
  dataset[ , liquid_cc := rowSums( cbind( mcuenta_corriente_adicional,  mcuenta_corriente) , na.rm=TRUE ) ]
  dataset[ , liquid_ca := rowSums( cbind( mcaja_ahorro_adicional,mcaja_ahorro ) , na.rm=TRUE ) ]
  dataset[ , liquid_tot := rowSums( cbind( liquid_cc,  liquid_ca) , na.rm=TRUE ) ]
  dataset[ , liquid_tot2 := rowSums( cbind( liquid_tot/mcuentas_saldo) , na.rm=TRUE ) ]
  
  ### Disclaimer a partir de ac· es un HURTO literal al compaÒero MFER https://github.com/mjferh 
  ### que muy amablemente compartiÛ su cÛdigo en GITHUB, los invito a HURTAR con toda libertad
  ### mi cÛdigo.  
  # Nivel tecnolÛgico [0,1,2,3,4]
  dataset[, mjf_nivel_tec := internet + tcallcenter + thomebanking + tmobile_app]
  
  # Nivel tecnolÛgico por edad
  dataset[, mjf_nivel_tec_edad := mjf_nivel_tec * cliente_edad]
  
  # Cantidad de productos - para validar cproductos
  dataset[, mjf_cproductos := tpaquete1 + tpaquete2 + tpaquete7 + tpaquete9 + 
            tcuentas +  ccuenta_corriente + ccaja_ahorro + 
            ctarjeta_debito + ctarjeta_visa + ctarjeta_master + 
            cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios + 
            cplazo_fijo + cinversion1 + cinversion2 + ccaja_seguridad + 
            cseguro_vida + cseguro_auto + cseguro_vivienda + cseguro_accidentes_personales]
  # Cantidad operaciones/mes - similar a ctrx_quater
  dataset[, mjf_coper_mes := ctarjeta_debito_trx + ctarjeta_visa_trx + ctarjeta_master_trx + 
            cpayroll_trx + 
            ccuenta_debitos_automaticos + 
            ctarjeta_visa_debitos_automaticos + ctarjeta_master_debitos_automaticos +
            cpagodeservicios + cpagomiscuentas + cforex +
            ctransferencias_recibidas + ctransferencias_emitidas + 
            cextraccion_autoservicio + ccheques_depositados + ccheques_emitidos +
            ccajas_depositos + ccajas_extracciones]
  # Cantidad de descuentos
  dataset[, mjf_cdescuentos := ccajeros_propios_descuentos + ctarjeta_visa_descuentos + 
            ctarjeta_master_descuentos]
  
  
  # Ratio operaciones at. personal / autoservicio
  dataset[, mjf_rautoservicio := (ccajas_trx / 
                                    (ccallcenter_trx + chomebanking_trx + cmobile_app_trx + 
                                       catm_trx + catm_trx_other))]
  # Ratio operaciones cajeros propios / otros bancos
  dataset[, mjf_rcajeros := catm_trx / catm_trx_other]

  ### Disclaimer a partir de ac· es un HURTO literal a los compaÒeros Molfino-Mac 
  ### que muy amablemente compartieron su cÛdigo en GITHUB, los invito a HURTAR con toda libertad
  ### mi cÛdigo.   

  dataset[ , trx_tarjetas_credito          := rowSums( cbind( ctarjeta_visa_trx,  ctarjeta_master_trx) , na.rm=TRUE ) ]
  dataset[ , actividad_en_caja          := rowSums( cbind( ccajas_trx, ccajas_consultas, ccajas_depositos, ccajas_otras, ccajas_extracciones) , na.rm=TRUE ) ]
  dataset[ , monto_total_cajeros          := rowSums( cbind( matm_other, matm) , na.rm=TRUE ) ]
  dataset[ , trx_total_cajeros          := rowSums( cbind( catm_trx_other, catm_trx) , na.rm=TRUE ) ]
  dataset[ , monto_prom_cajerosbanco       := matm / catm_trx ]
  dataset[ , monto_prom_cajerosotrobanco       := matm_other / catm_trx_other ]
  dataset[ , monto_prom_totalcajeros         := monto_total_cajeros  / trx_total_cajeros ]
  dataset[ , transacciones_en_canales          := rowSums( cbind( ccallcenter_trx , cmobile_app_trx , chomebanking_trx ) , na.rm=TRUE ) ]
  dataset[ , call_hb_mobile          := rowSums( cbind( tcallcenter , thomebanking , tmobile_app ) , na.rm=TRUE ) ]
  dataset[ , transacciones_en_canales          := rowSums( cbind( tcallcenter , thomebanking ) , na.rm=TRUE ) ]
  dataset[ , transacciones_en_canales          := rowSums( cbind( thomebanking , tmobile_app ) , na.rm=TRUE ) ]
  dataset[ , transacciones_en_canales          := rowSums( cbind( tcallcenter , tmobile_app ) , na.rm=TRUE ) ]
  
  dataset[ , prom_ch_dep := mcheques_depositados / ccheques_depositados ]
  dataset[ , prom_ch_em := mcheques_emitidos / ccheques_emitidos ]
  dataset[ , prom_ch_dep_rec := mcheques_depositados_rechazados / ccheques_depositados_rechazados ]
  dataset[ , prom_ch_em_rec := mcheques_emitidos_rechazados / ccheques_emitidos_rechazados ]
  dataset[ , ratio_ch_dep_rech := mcheques_depositados_rechazados / mcheques_depositados ]
  dataset[ , ratio_ch_em_rech := mcheques_emitidos_rechazados / mcheques_emitidos ]
  
  dataset[ , prom_compra_ext := mforex_buy / cforex_buy ]
  dataset[ , prom_venta_ext := mforex_sell / cforex_sell ]
  dataset[ , prom_transf_rec := mtransferencias_recibidas / ctransferencias_recibidas ]
  dataset[ , prom_transf_em := mtransferencias_emitidas / ctransferencias_emitidas ]
  dataset[ , prom_extraccion := mextraccion_autoservicio / cextraccion_autoservicio ]
  
  dataset[ , visa_consumo_vs_limite := Visa_mconsumototal / Visa_mlimitecompra ]
  dataset[ , visa_pago_vs_saldo := Visa_mpagado / Visa_msaldototal ]
  dataset[ , visa_pago_vs_saldo_pesos := Visa_mpagospesos / Visa_msaldopesos ]
  dataset[ , visa_pago_vs_saldo_usd := Visa_mpagosdolares / Visa_msaldodolares ]
  dataset[ , visa_prom_consumo := Visa_mconsumototal / Visa_cconsumos ]
  
  
  dataset[ , master_consumo_vs_limite := Master_mconsumototal / Master_mlimitecompra ]
  dataset[ , master_pago_vs_saldo := Master_mpagado / Master_msaldototal ]
  dataset[ , master_pago_vs_saldo_pesos := Master_mpagospesos / Master_msaldopesos ]
  dataset[ , master_pago_vs_saldo_usd := Master_mpagosdolares / Master_msaldodolares ]
  dataset[ , master_prom_consumo := Master_mconsumototal / Master_cconsumos ]
  
  dataset[ , prom_credpers := mprestamos_personales / cprestamos_personales ]
  dataset[ , prom_credpren := mprestamos_prendarios / cprestamos_prendarios ]
  dataset[ , prom_credhip := mprestamos_hipotecarios / cprestamos_hipotecarios ]
  dataset[ , cant_cred          := rowSums( cbind( cprestamos_personales , cprestamos_prendarios , cprestamos_hipotecarios ) , na.rm=TRUE ) ]
  dataset[ , monto_cred          := rowSums( cbind( mprestamos_personales , mprestamos_prendarios , mprestamos_hipotecarios ) , na.rm=TRUE ) ]
  dataset[ , prom_cred := monto_cred / cant_cred ]
  
  dataset[ , cant_seg          := rowSums( cbind( cseguro_vida , cseguro_auto , cseguro_vivienda , cseguro_accidentes_personales ) , na.rm=TRUE ) ]
  
  dataset[ , prom_payroll := mpayroll / cpayroll_trx ]
  dataset[ , prom_payroll2 := mpayroll2 / cpayroll2_trx ]
  dataset[ , suma_mpayroll          := rowSums( cbind( mpayroll , mpayroll2 ) , na.rm=TRUE ) ]
  dataset[ , suma_cpayroll          := rowSums( cbind( cpayroll_trx , cpayroll2_trx ) , na.rm=TRUE ) ]
  dataset[ , prom_totalpayroll := suma_mpayroll / suma_cpayroll ]
  
  dataset[ , prom_pagoserv := mpagodeservicios / cpagodeservicios ]
  dataset[ , prom_pagomisc := mpagomiscuentas / cpagomiscuentas ]
  dataset[ , monto_pagoservtot          := rowSums( cbind( mpagodeservicios , mpagomiscuentas ) , na.rm=TRUE ) ]
  dataset[ , cant_pagoservtot          := rowSums( cbind( cpagodeservicios , cpagomiscuentas ) , na.rm=TRUE ) ]
  dataset[ , prom_pagoservtot := monto_pagoservtot / cant_pagoservtot ]
  
  dataset[ , prom_com_mant := mcomisiones_mantenimiento / ccomisiones_mantenimiento ]
  dataset[ , prom_com_otras := mcomisiones_otras / ccomisiones_otras ]
  dataset[ , monto_comtot          := rowSums( cbind( mcomisiones_mantenimiento , mcomisiones_otras ) , na.rm=TRUE ) ]
  dataset[ , cant_comtot          := rowSums( cbind( ccomisiones_mantenimiento , ccomisiones_otras ) , na.rm=TRUE ) ]
  dataset[ , prom_comtot := monto_comtot / cant_comtot ]
  
  
  dataset[ , Master_finan_quartile := smart_cut(dataset$Master_mfinanciacion_limite, 4, "g", output = "numeric") ]
  dataset[ , Visa_finan_quartile  := smart_cut(dataset$Visa_mfinanciacion_limite, 4, "g", output = "numeric") ]
  dataset[ , Master_limitcompra_quartile := smart_cut(dataset$Master_mlimitecompra, 4, "g", output = "numeric") ]
  dataset[ , Visa_limitcompra_quartile := smart_cut(dataset$Visa_mlimitecompra, 4, "g", output = "numeric") ]
  
  
  dataset[ , cliente_edad_quartile   := smart_cut(dataset$cliente_edad, 4, "g", output = "numeric") ]
  dataset[ , cliente_antig_quartile  := smart_cut(dataset$cliente_antiguedad, 4, "g", output = "numeric") ]
  
  dataset[ , cliente_edad_1 := ifelse( dataset$cliente_edad < 21, 1,
                                       ifelse(dataset$cliente_edad < 31, 2,
                                              ifelse(dataset$cliente_edad < 41, 3,
                                                     ifelse(dataset$cliente_edad < 51, 4,
                                                            ifelse(dataset$cliente_edad < 61, 5,
                                                                   ifelse(dataset$cliente_edad < 71, 6,
                                                                          ifelse(dataset$cliente_edad < 81, 7,
                                                                                 ifelse(dataset$cliente_edad < 91, 8,
                                                                                        9)))))))) ]
  
  dataset[ , cliente_edad_2 := ifelse( dataset$cliente_edad < 21, 1,
                                       ifelse(dataset$cliente_edad < 36, 2,
                                              ifelse(dataset$cliente_edad < 51, 3,
                                                     ifelse(dataset$cliente_edad < 66, 4,
                                                            ifelse(dataset$cliente_edad < 81, 5,
                                                                   ifelse(dataset$cliente_edad < 96, 6,
                                                                          7)))))) ]
  
  dataset[ , cliente_edad_3 := ifelse( dataset$cliente_edad < 21, 1,
                                       ifelse(dataset$cliente_edad < 41, 2,
                                              ifelse(dataset$cliente_edad < 61, 3,
                                                     ifelse(dataset$cliente_edad < 81, 4,
                                                            5))))]
  
  dataset[ , cliente_edad_4 := ifelse( dataset$cliente_edad < 26, 1,
                                       ifelse(dataset$cliente_edad < 51, 2,
                                              ifelse(dataset$cliente_edad < 76, 3,
                                                     4)))]
  
  dataset[ , cliente_antig_1 := ifelse( dataset$cliente_antiguedad < 51, 1,
                                        ifelse(dataset$cliente_antiguedad < 101, 2,
                                               ifelse(dataset$cliente_antiguedad < 151, 3,
                                                      ifelse(dataset$cliente_antiguedad < 201, 4,
                                                             ifelse(dataset$cliente_antiguedad < 251, 5,
                                                                    ifelse(dataset$cliente_antiguedad < 301, 6,
                                                                           ifelse(dataset$cliente_antiguedad < 351, 7,
                                                                                  8)))))))]
  
  dataset[ , cliente_antig_2 := ifelse( dataset$cliente_antiguedad < 101, 1,
                                        ifelse(dataset$cliente_antiguedad < 201, 2,
                                               ifelse(dataset$cliente_antiguedad < 301, 3,
                                                      4)))]  
  
  dataset[ , cliente_antig_3 := ifelse( dataset$cliente_antiguedad < 151, 1,
                                        ifelse(dataset$cliente_antiguedad < 301, 2,
                                               3))]
  
  dataset[ , com_vs_rent := mcomisiones / mrentabilidad]
  dataset[ , com_vs_rentanual := mcomisiones / mrentabilidad_annual]
  dataset[ , act_vs_pas := mactivos_margen / mpasivos_margen]
  dataset[ , rent_vs_prod := mrentabilidad / cproductos]
  dataset[ , rentanual_vs_prod := mrentabilidad_annual / cproductos]
  dataset[ , com_vs_prod := mcomisiones / cproductos]
  dataset[ , suma_paq := tpaquete1 + tpaquete2 + tpaquete7 + tpaquete9]
  dataset[ , mult_paq := tpaquete1 * tpaquete2 * tpaquete7 * tpaquete9]
  dataset[ , suma_paq_1_2 := tpaquete1 + tpaquete2]
  dataset[ , suma_paq_1_7 := tpaquete1 + tpaquete7]
  dataset[ , suma_paq_1_9 := tpaquete1 + tpaquete9]
  dataset[ , suma_paq_2_7 := tpaquete2 + tpaquete7]
  dataset[ , suma_paq_2_9 := tpaquete2 + tpaquete9]
  dataset[ , suma_paq_7_9 := tpaquete7 + tpaquete9]
  dataset[ , mult_paq_1_2 := tpaquete1 * tpaquete2]
  dataset[ , mult_paq_1_7 := tpaquete1 * tpaquete7]
  dataset[ , mult_paq_1_9 := tpaquete1 * tpaquete9]
  dataset[ , mult_paq_2_7 := tpaquete2 * tpaquete7]
  dataset[ , mult_paq_2_9 := tpaquete2 * tpaquete9]
  dataset[ , mult_paq_7_9 := tpaquete7 * tpaquete9]
  
  dataset[ , montos_ctacorr := mcuenta_corriente + mcuenta_corriente_adicional]
  dataset[ , montos_cajaho := mcaja_ahorro + mcaja_ahorro_adicional + mcaja_ahorro_dolares]
  dataset[ , prom_ctacorr := montos_ctacorr / ccuenta_corriente]
  dataset[ , prom_cajaho := montos_cajaho / ccaja_ahorro]
  dataset[ , mctacorr_vs_mcajaho := montos_ctacorr / montos_cajaho]
  
  dataset[ , debtrx_prom := ctarjeta_debito_trx / ctarjeta_debito]
  dataset[ , auto_trx := mautoservicio / ctarjeta_debito_trx]
  dataset[ , auto_tarj := mautoservicio / ctarjeta_debito]
  
  dataset[ , debtrx_prom_visa := ctarjeta_visa_trx / ctarjeta_visa]
  dataset[ , auto_trx_visa := mtarjeta_visa_consumo / ctarjeta_visa_trx]
  dataset[ , auto_tarj_visa := mtarjeta_visa_consumo / ctarjeta_visa]
  
  dataset[ , debtrx_prom_master := ctarjeta_master_trx / ctarjeta_master]
  dataset[ , auto_trx_master := mtarjeta_master_consumo / ctarjeta_master_trx]
  dataset[ , auto_tarj_master := mtarjeta_master_consumo / ctarjeta_master]
  
  dataset[ , debtrx_prom_visa_master := (ctarjeta_visa_trx + ctarjeta_master_trx) / (ctarjeta_visa + ctarjeta_master)]
  dataset[ , auto_trx_visa_master := (mtarjeta_visa_consumo + mtarjeta_master_consumo) / (ctarjeta_visa_trx + ctarjeta_master_trx)]
  dataset[ , auto_tarj_visa_master := (mtarjeta_visa_consumo + mtarjeta_master_consumo) / (ctarjeta_visa + ctarjeta_master)]
  
  dataset[ , debtrx_prom_all := (ctarjeta_debito_trx+ ctarjeta_visa_trx + ctarjeta_master_trx) / (ctarjeta_debito + ctarjeta_visa + ctarjeta_master)]
  dataset[ , auto_trx_all := (mautoservicio + mtarjeta_visa_consumo + mtarjeta_master_consumo) / (ctarjeta_debito_trx + ctarjeta_visa_trx + ctarjeta_master_trx)]
  dataset[ , auto_tarj_all := (mautoservicio + mtarjeta_visa_consumo + mtarjeta_master_consumo) / (ctarjeta_debito + ctarjeta_visa + ctarjeta_master)]
  
  dataset[ , montos_plazofijo := mplazo_fijo_dolares + mplazo_fijo_pesos]
  dataset[ , prom_plazofijo := montos_plazofijo / cplazo_fijo]
  dataset[ , montos_inv1 := minversion1_pesos + minversion1_dolares]
  dataset[ , prom_inv1 := montos_inv1 / cinversion1]
  dataset[ , prom_inv2 := minversion2 / cinversion2]
  dataset[ , montos_inv := montos_inv1 + minversion2]
  dataset[ , montos_inv_y_plazofijo := montos_inv + montos_plazofijo]
  
  dataset[ , prom_inv_y_plazofijo := montos_inv_y_plazofijo / (cplazo_fijo + cinversion1 + cinversion2)]
  dataset[ , cant_inv_y_plazofijo := (cplazo_fijo + cinversion1 + cinversion2)]
  
  dataset[ , nrocliente_edad := numero_de_cliente / cliente_edad]
  dataset[ , nrocliente_antig := numero_de_cliente / cliente_antiguedad]
  dataset[ , rent_edad := mrentabilidad / cliente_edad ]
  dataset[ , rentanual_edad := mrentabilidad_annual / cliente_edad]
  dataset[ , rent_antig := mrentabilidad / cliente_antiguedad]
  dataset[ , rentanual_antig := mrentabilidad_annual / cliente_antiguedad]
  dataset[ , edad_antig := cliente_edad / cliente_antiguedad]
  dataset[ , margen_edad := (mactivos_margen + mpasivos_margen) / cliente_edad]
  dataset[ , nargen_antig := (mactivos_margen + mpasivos_margen) / cliente_antiguedad]
  dataset[ , margen_edad_prod := (mactivos_margen + mpasivos_margen) / cliente_edad * cproductos]
  dataset[ , nargen_antig_prod := (mactivos_margen + mpasivos_margen) / cliente_antiguedad * cproductos]
  dataset[ , edad_vs_cprod := cliente_edad / cproductos]
  dataset[ , antig_vs_cprod := cliente_antiguedad / cproductos]
  dataset[ , montoinv_vs_edad := montos_inv_y_plazofijo / cliente_edad]
  dataset[ , payroll_vs_edad := mpayroll / cliente_edad]
  dataset[ , payroll2_vs_edad := mpayroll2 / cliente_edad]
  dataset[ , payrolltotal_vs_edad := (mpayroll + mpayroll2) / cliente_edad]
  dataset[ , pagoserv_vs_payrolltotal := (mpagodeservicios) / (mpayroll + mpayroll2)]
  dataset[ , pagomisc_vs_payrolltotal := (mpagomiscuentas) / (mpayroll + mpayroll2)]
  dataset[ , pagostotales_vs_payrolltotal := (mpagodeservicios + mpagomiscuentas) / (mpayroll + mpayroll2)]
  dataset[ , pagostotales_vs_payrolltotal := (mpagodeservicios + mpagomiscuentas) / (mpayroll + mpayroll2)]
  dataset[ , desc_vs_edad := (mcajeros_propios_descuentos + mtarjeta_visa_descuentos + mtarjeta_master_descuentos) / cliente_edad]
  dataset[ , comis_vs_acred := (mcomisiones_mantenimiento + mcomisiones_otras) / (mpayroll + mpayroll2)]
  dataset[ , inv_vs_acred := montos_inv_y_plazofijo / (mpayroll + mpayroll2)]
  dataset[ , transfrec_vs_acred := mtransferencias_recibidas / (mpayroll + mpayroll2)]
  dataset[ , cheqemi_por_trxcallcenter := ccheques_emitidos * ccallcenter_trx]
  dataset[ , limcompra_vs_edad := (Visa_mlimitecompra + Master_mlimitecompra) / cliente_edad]
  dataset[ , limcompra_vs_antig := (Visa_mlimitecompra + Master_mlimitecompra) / cliente_antiguedad]
  dataset[ , visafecha_vs_antig := Visa_fechaalta / cliente_antiguedad]
  dataset[ , masterfecha_vs_antig := Master_fechaalta / cliente_antiguedad]
  dataset[ , chedepo_por_edad := ccheques_depositados / cliente_edad]
  dataset[ , chedepo_por_antig := ccheques_depositados / cliente_antiguedad]
  dataset[ , trxhomeb_vs_acred := chomebanking_trx / (mpayroll + mpayroll2)]
  dataset[ , trxhomeb_vs_edad := chomebanking_trx / cliente_edad]
  dataset[ , trxhomeb_vs_antig := chomebanking_trx / cliente_antiguedad]
  dataset[ , trxhomeb_vs_trxcajas := chomebanking_trx / ccajas_trx]
  dataset[ , trxhomeb_vs_trxcall := chomebanking_trx / ccallcenter_trx]
  dataset[ , cheqemi_vs_cheqrech := ccheques_emitidos / ccheques_emitidos_rechazados]
  
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply(names(dataset),function(.name) dataset[ , sum(is.infinite(get(.name)))])
  infinitos_qty  <- sum( unlist( infinitos) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <<- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply(names(dataset),function(.name) dataset[ , sum(is.nan(get(.name)))])
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el programa!\n\n")
    dataset[mapply(is.nan, dataset)] <<- 0
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#esta funcion supone que dataset esta ordenado por   <numero_de_cliente, foto_mes>
#calcula el lag y el delta lag

Lags  <- function( cols, nlag, deltas )
{
  gc()
  sufijo  <- paste0( "_lag", nlag )

  dataset[ , paste0( cols, sufijo) := shift(.SD, nlag, NA, "lag"), 
             by= numero_de_cliente, 
             .SDcols= cols]

  #agrego los deltas de los lags, con un "for" nada elegante
  if( deltas )
  {
    sufijodelta  <- paste0( "_delta", nlag )

    for( vcol in cols )
    {
     dataset[,  paste0(vcol, sufijodelta) := get( vcol)  - get(paste0( vcol, sufijo))]
    }
  }

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#se calculan para los 6 meses previos el minimo, maximo y tendencia calculada con cuadrados minimos
#la formula de calculo de la tendencia puede verse en https://stats.libretexts.org/Bookshelves/Introductory_Statistics/Book%3A_Introductory_Statistics_(Shafer_and_Zhang)/10%3A_Correlation_and_Regression/10.04%3A_The_Least_Squares_Regression_Line
#para la max√≠ma velocidad esta funcion esta escrita en lenguaje C, y no en la porqueria de R o Python

cppFunction('NumericVector fhistC(NumericVector pcolumna, IntegerVector pdesde ) 
{
  /* Aqui se cargan los valores para la regresion */
  double  x[100] ;
  double  y[100] ;

  int n = pcolumna.size();
  NumericVector out( 5*n );

  for(int i = 0; i < n; i++)
  {
    //lag
    if( pdesde[i]-1 < i )  out[ i + 4*n ]  =  pcolumna[i-1] ;
    else                   out[ i + 4*n ]  =  NA_REAL ;


    int  libre    = 0 ;
    int  xvalor   = 1 ;

    for( int j= pdesde[i]-1;  j<=i; j++ )
    {
       double a = pcolumna[j] ;

       if( !R_IsNA( a ) ) 
       {
          y[ libre ]= a ;
          x[ libre ]= xvalor ;
          libre++ ;
       }

       xvalor++ ;
    }

    /* Si hay al menos dos valores */
    if( libre > 1 )
    {
      double  xsum  = x[0] ;
      double  ysum  = y[0] ;
      double  xysum = xsum * ysum ;
      double  xxsum = xsum * xsum ;
      double  vmin  = y[0] ;
      double  vmax  = y[0] ;

      for( int h=1; h<libre; h++)
      { 
        xsum  += x[h] ;
        ysum  += y[h] ; 
        xysum += x[h]*y[h] ;
        xxsum += x[h]*x[h] ;

        if( y[h] < vmin )  vmin = y[h] ;
        if( y[h] > vmax )  vmax = y[h] ;
      }

      out[ i ]  =  (libre*xysum - xsum*ysum)/(libre*xxsum -xsum*xsum) ;
      out[ i + n ]    =  vmin ;
      out[ i + 2*n ]  =  vmax ;
      out[ i + 3*n ]  =  ysum / libre ;
    }
    else
    {
      out[ i       ]  =  NA_REAL ; 
      out[ i + n   ]  =  NA_REAL ;
      out[ i + 2*n ]  =  NA_REAL ;
      out[ i + 3*n ]  =  NA_REAL ;
    }
  }

  return  out;
}')

#------------------------------------------------------------------------------
#calcula la tendencia de las variables cols de los ultimos 6 meses
#la tendencia es la pendiente de la recta que ajusta por cuadrados minimos
#La funcionalidad de ratioavg es autoria de  Daiana Sparta,  UAustral  2021

TendenciaYmuchomas  <- function( dataset, cols, ventana=6, tendencia=TRUE, minimo=TRUE, maximo=TRUE, promedio=TRUE, 
                                 ratioavg=FALSE, ratiomax=FALSE)
{
  gc()
  #Esta es la cantidad de meses que utilizo para la historia
  ventana_regresion  <- ventana

  last  <- nrow( dataset )

  #creo el vector_desde que indica cada ventana
  #de esta forma se acelera el procesamiento ya que lo hago una sola vez
  vector_ids   <- dataset$numero_de_cliente

  vector_desde  <- seq( -ventana_regresion+2,  nrow(dataset)-ventana_regresion+1 )
  vector_desde[ 1:ventana_regresion ]  <-  1

  for( i in 2:last )  if( vector_ids[ i-1 ] !=  vector_ids[ i ] ) {  vector_desde[i] <-  i }
  for( i in 2:last )  if( vector_desde[i] < vector_desde[i-1] )  {  vector_desde[i] <-  vector_desde[i-1] }

  for(  campo  in   cols )
  {
    nueva_col     <- fhistC( dataset[ , get(campo) ], vector_desde ) 

    if(tendencia)  dataset[ , paste0( campo, "_tend", ventana) := nueva_col[ (0*last +1):(1*last) ]  ]
    if(minimo)     dataset[ , paste0( campo, "_min", ventana)  := nueva_col[ (1*last +1):(2*last) ]  ]
    if(maximo)     dataset[ , paste0( campo, "_max", ventana)  := nueva_col[ (2*last +1):(3*last) ]  ]
    if(promedio)   dataset[ , paste0( campo, "_avg", ventana)  := nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratioavg)   dataset[ , paste0( campo, "_ratioavg", ventana)  := get(campo) /nueva_col[ (3*last +1):(4*last) ]  ]
    if(ratiomax)   dataset[ , paste0( campo, "_ratiomax", ventana)  := get(campo) /nueva_col[ (2*last +1):(3*last) ]  ]
  }

}
#------------------------------------------------------------------------------
#Autor: Antonio Velazquez Bustamente,  UBA 2021

Tony  <- function( cols )
{

  sufijo  <- paste0( "_tony")

  dataset[ , paste0( cols, sufijo) := lapply( .SD,  function(x){ x/mean(x, na.rm=TRUE)} ), 
             by= foto_mes, 
             .SDcols= cols]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------

VPOS_CORTE  <- c()

fganancia_lgbm_meseta  <- function(probs, datos) 
{
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")

  tbl  <- as.data.table( list( "prob"=probs, "gan"= ifelse( vlabels==1 & vpesos > 1, 60000, -1000 ) ) )

  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  setorder( tbl, -gan_acum )   #voy por la meseta

  gan  <- mean( tbl[ 1:500,  gan_acum] )  #meseta de tama√±o 500

  pos_meseta  <- tbl[ 1:500,  median(posicion)]
  VPOS_CORTE  <<- c( VPOS_CORTE, pos_meseta )

  return( list( "name"= "ganancia", 
                "value"=  gan,
                "higher_better"= TRUE ) )
}
#------------------------------------------------------------------------------
#Elimina del dataset las variables que estan por debajo de la capa geologica de canaritos
#se llama varias veces, luego de agregar muchas variables nuevas, para ir reduciendo la cantidad de variables
# y as√≠ hacer lugar a nuevas variables importantes

GVEZ <- 1 

CanaritosImportancia  <- function( canaritos_ratio=0.2 )
{
  gc()
  ReportarCampos( dataset )
  dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

  for( i  in 1:(ncol(dataset)*canaritos_ratio))  dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset))]

  campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01" ) )

  azar  <- runif( nrow(dataset) )
  dataset[ , entrenamiento := foto_mes>= 202001 &  foto_mes<= 202010 &  foto_mes!=202006 & ( clase01==1 | azar < 0.10 ) ]

  dtrain  <- lgb.Dataset( data=    data.matrix(  dataset[ entrenamiento==TRUE, campos_buenos, with=FALSE]),
                          label=   dataset[ entrenamiento==TRUE, clase01],
                          weight=  dataset[ entrenamiento==TRUE, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                        )

  dvalid  <- lgb.Dataset( data=    data.matrix(  dataset[ foto_mes==202011, campos_buenos, with=FALSE]),
                          label=   dataset[ foto_mes==202011, clase01],
                          weight=  dataset[ foto_mes==202011, ifelse(clase_ternaria=="BAJA+2", 1.0000001, 1.0)],
                          free_raw_data= FALSE
                          )


  param <- list( objective= "binary",
                 metric= "custom",
                 first_metric_only= TRUE,
                 boost_from_average= TRUE,
                 feature_pre_filter= FALSE,
                 verbosity= -100,
                 seed= 999983,
                 max_depth=  -1,         # -1 significa no limitar,  por ahora lo dejo fijo
                 min_gain_to_split= 0.0, #por ahora, lo dejo fijo
                 lambda_l1= 0.0,         #por ahora, lo dejo fijo
                 lambda_l2= 0.0,         #por ahora, lo dejo fijo
                 max_bin= 31,            #por ahora, lo dejo fijo
                 num_iterations= 9999,   #un numero muy grande, lo limita early_stopping_rounds
                 force_row_wise= TRUE,    #para que los alumnos no se atemoricen con tantos warning
                 learning_rate= 0.065, 
                 feature_fraction= 1.0,   #lo seteo en 1 para que las primeras variables del dataset no se vean opacadas
                 min_data_in_leaf= 260,
                 num_leaves= 60,
               # num_threads= 8,
                 early_stopping_rounds= 200 )

  modelo  <- lgb.train( data= dtrain,
                        valids= list( valid= dvalid ),
                        eval= fganancia_lgbm_meseta,
                        param= param,
                        verbose= -100 )

  tb_importancia  <- lgb.importance( model= modelo )
  tb_importancia[  , pos := .I ]

  fwrite( tb_importancia, 
          file= paste0( "impo_", GVEZ ,".txt"),
          sep= "\t" )

  GVEZ  <<- GVEZ + 1

  umbral  <- tb_importancia[ Feature %like% "canarito", median(pos) + sd(pos) ]  #Atencion corto en la mediana !!

  col_utiles  <- tb_importancia[ pos < umbral & !( Feature %like% "canarito"),  Feature ]
  col_utiles  <-  unique( c( col_utiles,  c("numero_de_cliente","foto_mes","clase_ternaria","mes") ) )
  col_inutiles  <- setdiff( colnames(dataset), col_utiles )

  dataset[  ,  (col_inutiles) := NULL ]

  ReportarCampos( dataset )
}
#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
#Aqui empieza el programa

exp_iniciar( )

#cargo el dataset
nom_arch  <- exp_nombre_archivo( PARAM$files$input$dentrada )
dataset   <- fread( nom_arch )

#ordeno el dataset por <numero_de_cliente, foto_mes> para poder hacer lags
setorderv( dataset, PARAM$const$campos_sort )

AgregarMes( dataset )  #agrego el mes del a√±o

if( PARAM$dummiesNA )  DummiesNA( dataset )  #esta linea debe ir ANTES de Corregir  !!

if( PARAM$corregir )  Corregir( dataset )  #esta linea debe ir DESPUES de  DummiesNA

if( PARAM$variablesmanuales )  AgregarVariables( dataset )

if( PARAM$magiacristian )  CanaritosImportancia( canaritos_ratio = PARAM$ratiocristian )


#--------------------------------------
#Esta primera parte es muuuy  artesanal  y discutible  ya que hay multiples formas de hacerlo

cols_lagueables  <- copy( setdiff( colnames(dataset), PARAM$const$campos_fijos ) )

if( PARAM$tendenciaYmuchomas$correr ) 
{
  p  <- PARAM$tendenciaYmuchomas

  TendenciaYmuchomas( dataset, 
                      cols= cols_lagueables,
                      ventana=   p$ventana,
                      tendencia= p$tendencia,
                      minimo=    p$minimo,
                      maximo=    p$maximo,
                      promedio=  p$promedio,
                      ratioavg=  p$ratioavg,
                      ratiomax=  p$ratiomax
                    )

}


for( i in 1:length( PARAM$lag ) )
{
  if( PARAM$lag[i] )
  {
    #veo si tengo que ir agregando variables
    if( PARAM$acumulavars )  cols_lagueables  <- setdiff( colnames(dataset), PARAM$const$campos_fijos )

    Lags( cols_lagueables, i, PARAM$delta[ i ] )   #calculo los lags de orden  i

    #elimino las variables poco importantes, para hacer lugar a las importantes
    if( PARAM$canaritosratio[ i ] > 0 )  CanaritosImportancia( canaritos_ratio= unlist(PARAM$canaritosratio[ i ]) )
  }
}



#dejo la clase como ultimo campo
nuevo_orden  <- c( setdiff( colnames( dataset ) , PARAM$const$clase ) , PARAM$const$clase )
setcolorder( dataset, nuevo_orden )


#Grabo el dataset    https://www.youtube.com/watch?v=66CP-pq7Cx0
fwrite( dataset,
        paste0( PARAM$files$output ),
        logical01= TRUE,
        sep= "," )



# grabo catalogo   ------------------------------------------------------------
# es lo ultimo que hago, indica que llegue a generar la salida
#no todos los archivos generados pasan al catalogo

exp_catalog_add( action= "FE",
                 type=   "file",
                 key=    "dataset",
                 value = PARAM$files$output )

#finalizo el experimento
#HouseKeeping
exp_finalizar( )
