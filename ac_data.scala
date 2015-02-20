case class ACD_POS(
  var data: LL,
  var status: STATUS
)
case class ACD_GPWS(
  var palt_or_gpsalt: DATA_S32,
  var palt_is_gps: BOOL,
  var msl_alt: DATA_S32,
  var ra_in: DATA_S32,
  var ra_avg2: DATA_S32,
  var ra_avg6: DATA_S32,
  var ra_fps: DATA_S32,
  var ra_fpm: DATA_S32,
  var bsr_fpm: DATA_S32,
  var bvs_fpm: DATA_S32,
  var gs_kts: DATA_S32,
  var ils: DATA_ILS,
  var hat: DATA_S32,
  var cell_elev: DATA_S32,
  var bc: DATA_U32,
  var gear: DATA_U32
)
case class ACD_ACC(
  var lateral: DATA_S32,
  var vertical: DATA_S32
)
case class ACD_TA(
  var pos: DATA_POS,
  var cosine_pos_lat: DATA_POS,
  var trk: DATA_ANG,
  var trk_rate: DATA_S32,
  var gs_kts: DATA_S32,
  var gs_fps: DATA_S32,
  var acc: ACD_ACC,
  var vs: DATA_S32,
  var best_msl_alt: DATA_S32,
  var hatc: DATA_S32,
  var ap: DATA_U32
)
case class ACD_POF(
  var pos: DATA_POS,
  var alt: DATA_S32,
  var palt: DATA_S32,
  var ra_avg: DATA_S32,
  var gs_kts: DATA_S32,
  var hat: DATA_S32,
  var cell_elev: DATA_S32
)
