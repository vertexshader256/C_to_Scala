typedef struct {
    LL data;
    STATUS status;
} ACD_POS;
typedef struct {
    DATA_S32 palt_or_gpsalt;
    BOOL palt_is_gps;
    DATA_S32 msl_alt;
    DATA_S32 ra_in;
    DATA_S32 ra_avg2;
    DATA_S32 ra_avg6;
    DATA_S32 ra_fps;
    DATA_S32 ra_fpm;
    DATA_S32 bsr_fpm;
    DATA_S32 bvs_fpm;
    DATA_S32 gs_kts;
    DATA_ILS ils;
    DATA_S32 hat;
    DATA_S32 cell_elev;
    DATA_U32 bc;
    DATA_U32 gear;
} ACD_GPWS;
typedef struct {
    DATA_S32 lateral;
    DATA_S32 vertical;
} ACD_ACC;
typedef struct {
    DATA_POS pos;
    double cosine_pos_lat;
    DATA_ANG trk;
    DATA_S32 trk_rate;
    DATA_S32 gs_kts;
    DATA_S32 gs_fps;
    ACD_ACC acc;
    DATA_S32 vs;
    DATA_S32 best_msl_alt;
    DATA_S32 hatc;
    DATA_U32 ap;
} ACD_TA;
typedef struct {
    DATA_POS pos;
    DATA_S32 alt;
    DATA_S32 palt;
    DATA_S32 ra_avg;
    DATA_S32 gs_kts;
    DATA_S32 hat;
    DATA_S32 cell_elev;
} ACD_POF;
void acd_get_gpws (ACD_GPWS *item);
ACD_TA* acd_get_taws_ptr(void);
ACD_TA* acd_get_taws (void);
ACD_POF* acd_get_pof_ptr (void);
void acd_taws_init(void);
void acd_setup (void);
void acd_taws_sec (void);
void acd_100ms (void *pTawsIn);
void acd_50ms (void);
