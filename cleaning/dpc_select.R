dpc_select <- function(dpc){
  dpc_selected <<- dpc %>% 
    select(sex,
           birth_date,
           adm_date,
           disc_date,
           adm_style,
           adm_ambul,
           disc_to,
           disc_outcome,
           death_24h,
           adm_before,
           dmainnm,
           dmain,
           dadmnm,
           dadm,
           dres1nm,
           dres1,
           dres2nm,
           dres2,
           dcom1nm,
           dcom1,
           dcom2nm,
           dcom2,
           dcom3nm,
           dcom3,
           dcom4nm,
           dcom4,
           ddev1nm,
           ddev1,
           ddev2nm,
           ddev2,
           ddev3nm,
           ddev3,
           ddev4nm,
           ddev4,
           height,
           weight,
           smokingidx,
           adm_jcs,
           disc_jcs,
           adm_adl,
           disc_adl,
           hughjones,
           openm1,
           opek1,
           opedt1,
           openm2,
           opek2,
           opedt2,
           openm3,
           opek3,
           opedt3,
           openm4,
           opek4,
           opedt4,
           openm5,
           opek5,
           opedt5)
}
