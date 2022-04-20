#Feature Engineering
#creo nuevas variables dentro del mismo mes

#este script se muestra como esqueleto para que los alumnos agreguen sus propias variables
#ya sea basados en la teoria economica  o en el salvaje empiricismo
# "No es que la nueva variable creada, que funciona, no tenga sentido, lo que sucede es que yo no estoy siendo capaz de encontrarselo en este momento"

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")



EnriquecerDataset  <- function( dataset , arch_destino )
{

  #INICIO de la seccion donde se deben hacer cambios con variables nuevas

  #creo un ctr_quarter que tenga en cuenta cuando los clientes hace 3 menos meses que estan
  dataset[  , ctrx_quarter_normalizado := ctrx_quarter ]
  dataset[ cliente_antiguedad==1 , ctrx_quarter_normalizado := ctrx_quarter * 5 ] # Si tiene 1 mes de antig = Mov del mes x 5
  dataset[ cliente_antiguedad==2 , ctrx_quarter_normalizado := ctrx_quarter * 2 ] # Si tiene 2 meses = Mov del mes x 2
  dataset[ cliente_antiguedad==3 , ctrx_quarter_normalizado := ctrx_quarter * 1.2 ]#Si tiene 3 meses = Mov del mes x 1,2

  #variable extraida de una tesis de maestria de Irlanda
  dataset[  , mpayroll_sobre_edad  := mpayroll / cliente_edad ]

  #se crean los nuevos campos para MasterCard  y Visa, teniendo en cuenta los NA's
  #varias formas de combinar Visa_status y Master_status

  dataset[ , mv_status01 := pmax( Master_status,  Visa_status, na.rm = TRUE) ] # Tomar el máximo status entre ambas
  dataset[ , mv_status02 := Master_status +  Visa_status ] # Qué intenta hacer esta variable?
  dataset[ , mv_status03 := pmax( ifelse( is.na(Master_status), 10, Master_status) , ifelse( is.na(Visa_status), 10, Visa_status) ) ] # Me quedo con el máximo entre Master Status y Visa Status colocando 10 a los N/A
  dataset[ , mv_status04 := ifelse( is.na(Master_status), 10, Master_status)  +  ifelse( is.na(Visa_status), 10, Visa_status)  ] # Máximo de Master + Máximo de Visa considetando 10 para los N/A
  dataset[ , mv_status05 := ifelse( is.na(Master_status), 10, Master_status)  +  100*ifelse( is.na(Visa_status), 10, Visa_status)  ] # Máximo de Master + 100 x Máximo de Visa considetando 10 para los N/A en ambos casos

  dataset[ , mv_status06 := ifelse( is.na(Visa_status), 
                                    ifelse( is.na(Master_status), 10, Master_status), 
                                    Visa_status) ] # Si los dos status son N/A = 10, si Master no es N/A entonces tomá Master_status y sino Visa_status

  dataset[ , mv_status07 := ifelse( is.na(Master_status), 
                                    ifelse( is.na(Visa_status), 10, Visa_status), 
                                    Master_status) ] # Si los dos status son N/A = 10, si Visa no es N/A entonces tomá Visa_status y sino Master_status


  #combino MasterCard y Visa , teniendo en cuenta los NA
  dataset[ , mv_mfinanciacion_limite  := rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ) ] # Suma del límite de las 2 tarjetas

  dataset[ , mv_Fvencimiento          := pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ] # Día mínimo de vto de ambas tarjetas
  dataset[ , mv_Finiciomora           := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ] # Día mínimo siguiente a la fecha de vto 
  dataset[ , mv_msaldototal           := rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ) ] # Saldo total entre ambas tarjetas
  dataset[ , mv_msaldopesos           := rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ) ] # Saldo total entre ambas tarjetas en ARS 
  dataset[ , mv_msaldodolares         := rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ) ] # Saldo total entre ambas tarjetas en USD 
  dataset[ , mv_mconsumospesos        := rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ) ] # Consumo del mes entre ambas tarjetas en ARS
  dataset[ , mv_mconsumosdolares      := rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ) ] # Consumo del mes entre ambas tarjetas en USD
  dataset[ , mv_mlimitecompra         := rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ) ] # Límite de pago en 1 cuota en TC a checkear
  dataset[ , mv_madelantopesos        := rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ) ] # Extracciones en ARS sumadas
  dataset[ , mv_madelantodolares      := rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ) ] # Extracciones en USD sumadas
  dataset[ , mv_fultimo_cierre        := pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE) ] # Máximo entre ambas tarjetas en días de cierre anterior a la fecha de la foto
  dataset[ , mv_mpagado               := rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ) ] # Suma de monto total de los pagos (checkear si los USD acá están convertidos a ARS)
  dataset[ , mv_mpagospesos           := rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ) ] # Suma de monto total de los pagos en ARS
  dataset[ , mv_mpagosdolares         := rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ) ] # Suma de monto total de los pagos en USD
  dataset[ , mv_fechaalta             := pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE) ] # Máximo en días de alta a la fecha de la foto
  dataset[ , mv_mconsumototal         := rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ) ] # Consumos totales expresados en ARS entre ambas tarjetas
  dataset[ , mv_cconsumos             := rowSums( cbind( Master_cconsumos,  Visa_cconsumos) , na.rm=TRUE ) ] # Suma de la cantidad de "consumos" (operaciones) entre ambas tarjetas
  dataset[ , mv_cadelantosefectivo    := rowSums( cbind( Master_cadelantosefectivo,  Visa_cadelantosefectivo) , na.rm=TRUE ) ] # Cantidad de Extracciones sumadas
  dataset[ , mv_mpagominimo           := rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ] # Suma de los montos de pagos mínimos para no ser moroso


  #a partir de aqui juego con la suma de Mastercard y Visa
  dataset[ , mvr_Master_mlimitecompra := Master_mlimitecompra / mv_mlimitecompra ] #Índice entre el límite de Master y Visa
  dataset[ , mvr_Visa_mlimitecompra   := Visa_mlimitecompra / mv_mlimitecompra ] #Índice entre el límite de Visa y Master
  dataset[ , mvr_msaldototal          := mv_msaldototal / mv_mlimitecompra ] 
  dataset[ , mvr_msaldopesos          := mv_msaldopesos / mv_mlimitecompra ]
  dataset[ , mvr_msaldopesos2         := mv_msaldopesos / mv_msaldototal ]
  dataset[ , mvr_msaldodolares        := mv_msaldodolares / mv_mlimitecompra ]
  dataset[ , mvr_msaldodolares2       := mv_msaldodolares / mv_msaldototal ]
  dataset[ , mvr_mconsumospesos       := mv_mconsumospesos / mv_mlimitecompra ]
  dataset[ , mvr_mconsumosdolares     := mv_mconsumosdolares / mv_mlimitecompra ]
  dataset[ , mvr_madelantopesos       := mv_madelantopesos / mv_mlimitecompra ]
  dataset[ , mvr_madelantodolares     := mv_madelantodolares / mv_mlimitecompra ]
  dataset[ , mvr_mpagado              := mv_mpagado / mv_mlimitecompra ]
  dataset[ , mvr_mpagospesos          := mv_mpagospesos / mv_mlimitecompra ]
  dataset[ , mvr_mpagosdolares        := mv_mpagosdolares / mv_mlimitecompra ]
  dataset[ , mvr_mconsumototal        := mv_mconsumototal  / mv_mlimitecompra ]
  dataset[ , mvr_mpagominimo          := mv_mpagominimo  / mv_mlimitecompra ]
  
  # Nuevas variables ACV
  
  # cliente_edad
  dataset[ , varnueva_01 := cliente_edad * cpayroll_trx ]
  dataset[ , varnueva_02 := cliente_edad * mcuentas_saldo ]
  dataset[ , varnueva_03 := cliente_edad * mcaja_ahorro ]
  dataset[ , varnueva_04 := cliente_edad * ctarjeta_visa_trx ]
  dataset[ , varnueva_05 := cliente_edad * mtarjeta_visa_consumo ]
  dataset[ , varnueva_06 := cliente_edad * mprestamos_personales ]
  dataset[ , varnueva_07 := cliente_edad * mrentabilidad_annual ]
  dataset[ , varnueva_08 := cliente_edad * mactivos_margen ]
  
  # ctrx_quarter
  dataset[ , varnueva_09 := ctrx_quarter * cpayroll_trx ]
  dataset[ , varnueva_10 := ctrx_quarter * mcuentas_saldo ]
  dataset[ , varnueva_11 := ctrx_quarter * mcaja_ahorro ]
  dataset[ , varnueva_12 := ctrx_quarter * ctarjeta_visa_trx ]
  dataset[ , varnueva_13 := ctrx_quarter * mtarjeta_visa_consumo ]
  dataset[ , varnueva_14 := ctrx_quarter * mprestamos_personales ]
  dataset[ , varnueva_15 := ctrx_quarter * mrentabilidad_annual ]
  dataset[ , varnueva_16 := ctrx_quarter * mactivos_margen ]
  dataset[ , varnueva_17 := ctrx_quarter * cliente_edad ]
  
  # cpayroll_trx
  dataset[ , varnueva_18 := cpayroll_trx * mcuentas_saldo ]
  dataset[ , varnueva_19 := cpayroll_trx * mcaja_ahorro ]
  dataset[ , varnueva_20 := cpayroll_trx * ctarjeta_visa_trx ]
  dataset[ , varnueva_21 := cpayroll_trx * mtarjeta_visa_consumo ]
  dataset[ , varnueva_22 := cpayroll_trx * mprestamos_personales ]
  dataset[ , varnueva_23 := cpayroll_trx * mrentabilidad_annual ]
  dataset[ , varnueva_24 := cpayroll_trx * mactivos_margen ]
  
  # mtarjeta_visa_consumo
  dataset[ , varnueva_25 := mtarjeta_visa_consumo * cpayroll_trx ]
  dataset[ , varnueva_26 := mtarjeta_visa_consumo * mcuentas_saldo ]
  dataset[ , varnueva_27 := mtarjeta_visa_consumo * mcaja_ahorro ]
  dataset[ , varnueva_28 := mtarjeta_visa_consumo * ctarjeta_visa_trx ]
  dataset[ , varnueva_29 := mtarjeta_visa_consumo * mtarjeta_visa_consumo ]
  dataset[ , varnueva_30 := mtarjeta_visa_consumo * mprestamos_personales ]
  dataset[ , varnueva_31 := mtarjeta_visa_consumo * mrentabilidad_annual ]
  dataset[ , varnueva_32 := mtarjeta_visa_consumo * mactivos_margen ]
  
  # prestamos 
  dataset[ , varnueva_33 := mprestamos_personales + mprestamos_prendarios + mprestamos_hipotecarios ]
  dataset[ , varnueva_34 := cprestamos_personales + cprestamos_prendarios + cprestamos_hipotecarios ]
  dataset[ , varnueva_35 := varnueva_33/varnueva_34 ]
  
  # polinómica
  dataset[ , varnueva_36 := mcuentas_saldo * 0.11 + cliente_edad * 0.04 + Master_fechaalta * 0.04 + mactivos_margen * 0.09 +
             mrentabilidad_annual * 0.06 + mpayroll * 0.15 + ctrx_quarter * 0.63 + cpayroll_trx * 0.23 + mpasivos_margen * 0.19+
             mtarjeta_visa_consumo * 0.12]
  
  # Coeficientes
  dataset[ , varnueva_37 := (Master_mlimitecompra + Visa_mlimitecompra)/cliente_edad/1000 ]
  dataset[ , varnueva_38 := (Master_mlimitecompra + Visa_mlimitecompra)/ctrx_quarter/1000 ]

  # Pagado - Pago mín = Dif entre lo pagado que excede el mínimo
  dataset[ , varnueva_39 := mv_mpagado - mv_mpagominimo ]

  # Ratios
  dataset[ , varnueva_40         := mactivos_margen / ctrx_quarter ]
  dataset[ , varnueva_41         := cpayroll_trx / mactivos_margen ]
  dataset[ , varnueva_42         := ctrx_quarter  / mtarjeta_visa_consumo  ]
  dataset[ , varnueva_43         := ctrx_quarter  / mcaja_ahorro  ]
  dataset[ , varnueva_44         := ctrx_quarter  / varnueva_33  ]
  dataset[ , varnueva_45         := ctrx_quarter  / cpayroll_trx  ]
  dataset[ , varnueva_46         := ctrx_quarter  / mvr_msaldopesos  ]
  dataset[ , varnueva_47         := ctrx_quarter  / mcuentas_saldo  ]
  dataset[ , varnueva_48         := ctrx_quarter  / cpayroll_trx  ]
  dataset[ , varnueva_49         := ctrx_quarter  / mpayroll  ]
  dataset[ , varnueva_50         := ctrx_quarter  / mcuenta_corriente  ]
  dataset[ , varnueva_51         := ctrx_quarter  / ctarjeta_visa_trx  ]
  dataset[ , varnueva_52         := ctrx_quarter  / mprestamos_personales  ]
  dataset[ , varnueva_53         := ctrx_quarter  / mactivos_margen  ]
  dataset[ , varnueva_54         := ctrx_quarter  / mv_status01  ]
  dataset[ , varnueva_55         := ctrx_quarter  / mcomisiones_mantenimiento  ]
  dataset[ , varnueva_56         := ctrx_quarter  / ctarjeta_visa_trx  ]
  dataset[ , varnueva_57         := ctrx_quarter  / mrentabilidad_annual  ]  
  dataset[ , varnueva_58         := mactivos_margen  / mprestamos_personales  ]
  dataset[ , varnueva_59         := mprestamos_personales  / mv_status01  ]
  dataset[ , varnueva_60         := mactivos_margen  / mv_status01  ]
  dataset[ , varnueva_61         := mactivos_margen  / mcomisiones_mantenimiento  ]
  dataset[ , varnueva_62         := mactivos_margen  / ctarjeta_visa_trx  ]
  dataset[ , varnueva_63         := mactivos_margen  / mrentabilidad_annual  ]
  dataset[ , varnueva_64         := mtarjeta_visa_consumo  / mcaja_ahorro  ]
  dataset[ , varnueva_65         := mtarjeta_visa_consumo  / varnueva_33  ]
  dataset[ , varnueva_66         := mtarjeta_visa_consumo  / mvr_msaldopesos  ]
  dataset[ , varnueva_67         := mtarjeta_visa_consumo  / mcuentas_saldo  ]
  dataset[ , varnueva_68         := mtarjeta_visa_consumo  / cpayroll_trx  ]
  dataset[ , varnueva_69         := mtarjeta_visa_consumo  / mpayroll  ]
  dataset[ , varnueva_70         := mtarjeta_visa_consumo  / mv_msaldopesos  ]
  dataset[ , varnueva_71         := mtarjeta_visa_consumo   / mcomisiones_mantenimiento  ]
  dataset[ , varnueva_72         := mtarjeta_visa_consumo   / ctarjeta_visa_trx  ]
  dataset[ , varnueva_73         := mtarjeta_visa_consumo   / mrentabilidad_annual  ]
  dataset[ , varnueva_74         := cpayroll_trx  / mvr_msaldopesos  ]
  dataset[ , varnueva_75         := cpayroll_trx  / mcuentas_saldo  ]
  dataset[ , varnueva_76         := cpayroll_trx  / mpayroll  ]
  dataset[ , varnueva_77         := cpayroll_trx   / mcomisiones_mantenimiento  ]
  dataset[ , varnueva_78         := cpayroll_trx   / ctarjeta_visa_trx  ]
  dataset[ , varnueva_79         := cpayroll_trx   / mrentabilidad_annual  ]  
  
  
  
    
  
  #valvula de seguridad para evitar valores infinitos
  #paso los infinitos a NULOS
  infinitos      <- lapply( names(dataset),
                            function(.name) dataset[ , sum(is.infinite( get(.name) )) ]  )
  
  infinitos_qty  <- sum( unlist( infinitos ) )
  if( infinitos_qty > 0 )
  {
    cat( "ATENCION, hay", infinitos_qty, "valores infinitos en tu dataset. Seran pasados a NA\n" )
    dataset[mapply(is.infinite, dataset)] <- NA
  }


  #valvula de seguridad para evitar valores NaN  que es 0/0
  #paso los NaN a 0 , decision polemica si las hay
  #se invita a asignar un valor razonable segun la semantica del campo creado
  nans      <- lapply( names(dataset),
                       function(.name) dataset[ , sum( is.nan( get(.name) )) ] )
  
  nans_qty  <- sum( unlist( nans) )
  if( nans_qty > 0 )
  {
    cat( "ATENCION, hay", nans_qty, "valores NaN 0/0 en tu dataset. Seran pasados arbitrariamente a 0\n" )
    cat( "Si no te gusta la decision, modifica a gusto el script!\n\n")
    dataset[mapply(is.nan, dataset)] <- 0
  }

  #FIN de la seccion donde se deben hacer cambios con variables nuevas

  #grabo con nombre extendido
  fwrite( dataset,
          file= arch_destino,
          sep= "," )

}
#------------------------------------------------------------------------------

#aqui comienza el programa

#Establezco el Working Directory
setwd("C:\\Users\\cdibono\\OneDrive - TERNIUM\\Austral\\Materias\\11. Laboratorios de implementación\\") #Establezco el Working Directory #establezco la carpeta donde voy a trabajar


#lectura de los datasets
dataset1  <- fread("./datasets/paquete_premium_202011.csv")
dataset2  <- fread("./datasets/paquete_premium_202101.csv")


#creo la carpeta donde va el experimento
# FE  representa  Feature Engineering
dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/FE4020/", showWarnings = FALSE )
setwd("C:\\Users\\cdibono\\OneDrive - TERNIUM\\Austral\\Materias\\11. Laboratorios de implementación\\labo\\exp\\FE4020\\")   #Establezco el Working Directory DEL EXPERIMENTO

EnriquecerDataset( dataset1, "paquete_premium_202011_ext.csv" )
EnriquecerDataset( dataset2, "paquete_premium_202101_ext.csv" )

