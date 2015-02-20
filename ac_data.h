//  File:  ac_data.h -- Aircraft Data processing module header
// -----------------------------------------------------------------------------------
//
//  SANDEL AVIONICS proprietary rights are included in the information 
//  disclosed herein.  Recipient by accepting this document agrees that  
//  neither this document nor the information disclosed herein nor any part  
//  thereof shall be reproduced or transferred to other documents or used or  
//  disclosed to others for manufacturing or for any other purpose except as  
//  specifically authorized in writing by SANDEL AVIONICS.
// 
//  Copyright (C) 1998-2014 by Sandel Avionics, Vista, CA, U.S.A.; all rights 
//  reserved. Confidential, unpublished property of Sandel Avionics.
//
// -----------------------------------------------------------------------------------
// Abstract:
//      ac_data.h defines the public parameters, variables and functions used
//      for obtaining aircraft data such as current location, altitude, speed,  
//      heading, change rates, ground track, flap staus, landing gear 
//      position, and auto pilot status.
// -----------------------------------------------------------------------------------
//
//  Date of last commit       $Date:: 2014-12-08 13:35:50 #$: 
//  Revision of last commit    $Rev:: 52896                $: 
//  User at last commit     $Author:: SANDEL0\lcorn        $: 
//  $URL: http://svn.sandel.local/svn/Projects/TAWS%20API/RC_BRS/SW/Code/api_lib/ac_data.h $
//
// History:
//  (01/06/2011,LC) API Baseline
// 

#ifndef AC_DATA_H_
#define AC_DATA_H_


#if defined( __cplusplus )                        
extern "C"{     // To use "C" code in "C++" code  
#endif


//
//  DEFINES
//
    
// special values
#define RANGE_UNKNOWN       (999999)  // value used when range field has unknown data
#define ALERT_INVALID_DATA  (999999)

#ifndef API_COMMON_H_ 
  #define API_STAT  STATUS
  #define API_BOOL  BOOL
#endif

//
//  TYPEDEFS
//

// position 
typedef struct {
    LL       data;
    API_STAT status;
} ACD_POS;

//  GPWS calculations
typedef struct {
    DATA_S32  palt_or_gpsalt;       // feet, prioritized source selection, see ac_data_proc.c
    API_BOOL  palt_is_gps;          // for field above; F=palt; T=GPS (fixed wing only)
    DATA_S32  msl_alt;              // msl feet, prioritized source selection
    DATA_S32  ra_in;                // feet, unfiltered
    DATA_S32  ra_avg2;              // feet, avg 2 second
    DATA_S32  ra_avg6;              // feet, avg 6 second
    DATA_S32  ra_fps;               // ra feet per second +descending
    DATA_S32  ra_fpm;               // ra feet per minute +descending 
    DATA_S32  bsr_fpm;              // baro fpm           +descending
    DATA_S32  bvs_fpm;              // baro fpm           +ascending
    DATA_S32  gs_kts;               // groundspeed kts
    DATA_ILS  ils;                  // microamps
    DATA_S32  hat;                  // height above terrain, best source
    DATA_S32  cell_elev;            // current cell elevation
    DATA_U32  bc;                   // T = BC active
#ifdef FIXED_WING_TAWS
    DATA_U32  flaps;                // T = flaps down
#endif
    DATA_U32  gear;                 // T = gear down
#ifdef HELICOPTER_TAWS
    DATA_U32  m3_cancel;            // cancel request
#endif
} ACD_GPWS;

//  Accuracy data
typedef struct {
    DATA_S32  lateral;              // lateral error in feet
    DATA_S32  vertical;             // lateral error in feet
} ACD_ACC;

//  data required by the alert calculations
typedef struct {
    DATA_POS  pos;                  // LATLON position, with
    double    cosine_pos_lat;       // calculated cos(latitude) for current position
    DATA_ANG  trk;                  // fms track, true
    DATA_S32  trk_rate;             // track rate (+ is turning right)
    DATA_S32  gs_kts;               // best source groundspeed
    DATA_S32  gs_fps;               // groundspeed feet per second
    ACD_ACC   acc;                  // accuracy
    DATA_S32  vs;                   // best source vertical speed +ascending
    DATA_S32  best_msl_alt;         // best source altitude
    DATA_S32  hatc;                 // height above current terrain cell
    DATA_U32  ap;                   // T = autopilot engaged
} ACD_TA;

// data required by the POF calculations
typedef struct {
    DATA_POS  pos;                  // position lat & lon
    DATA_S32  alt;                  // best msl altitude
    DATA_S32  palt;                 // pressure altitude
    DATA_S32  ra_avg;               // averaged radar alt
    DATA_S32  gs_kts;               // averaged groundspeed (from GPS/FMS)
    DATA_S32  hat;                  // ra or if n/a height above current terrain cell
    DATA_S32  cell_elev;            // current terrain cell elevation
} ACD_POF;


//
//  PROTOTYPES
//

void     acd_get_gpws    (ACD_GPWS *item);
ACD_TA*  acd_get_taws_ptr(void);
ACD_TA*  acd_get_taws    (void);
ACD_POF* acd_get_pof_ptr (void);

void acd_taws_init(void);
void acd_setup    (void);
void acd_taws_sec (void);
void acd_100ms    (void *pTawsIn);
void acd_50ms     (void);


#if defined( __cplusplus )                        
}   // extern "C"   .. use "C" code in "C++" code 
#endif

#endif // def  AC_DATA_H_
