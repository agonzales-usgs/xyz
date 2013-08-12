c
c-------------------------------------------------------------------
c
      subroutine WSACH (KNAME, YARRAY, NLEN, XARRAY, NERR,
     +          nsta, nchn, ntyp, iy, id, ih, im, ss, dt,
     +          rla, rlo, ela, elo, edep)

C     The parameters defined in the continuation line above are defined in
C   the calling program and relate to the Harvard CD ROM reader data files.
C
C	This version of wsach created: 24-May-91 R. Gauthier & S. Mangino
C
*=====================================================================
* PURPOSE: To write a SAC file with user defined header fields.
*=====================================================================
* INPUT ARGUMENTS:
*     KNAME:  Name of disk file to write.
*             Name must contain at least one trailing blank.
*    YARRAY:  Array containing the dependent variable.
*      NLEN:  Length of YARRAY.
*    XARRAY:  Array containing the independent variable if data is
*             unevenly spaced; not used if data is evenly spaced.
*=====================================================================
* OUTPUT ARGUMENTS:
*      NERR:  Error return flag.
*=====================================================================
* MODULE/LEVEL: DFM/4
*=====================================================================
* GLOBAL OUTPUT:
*    MACH:
*    HDR:     Potentially all header fields.
*=====================================================================
* SUBROUTINES CALLED:
*    SACLIB: INIHDR, NEWHDR, WSAC0
*=====================================================================

      INCLUDE '/opt/local/sac/inc/mach'
      INCLUDE '/opt/local/sac/inc/dfm'
      INCLUDE '/opt/local/sac/inc/hdr'
      INCLUDE '/opt/local/sac/inc/mem'
C
      CHARACTER*(*) KNAME
   	real*4          ss,dt
        integer*4       iy, id, ih, im
        character*4     nsta, nchn, ntyp
C
      DIMENSION XARRAY(NLEN), YARRAY(NLEN)

* - USER ACTION: Include any common blocks needed to
*             interface to your data here.

* PROCEDURE:

* - Data load HDR common blocks if not already done.

      IF(FUNDEF.NE.-12345.)CALL INIHDR

* - Initialize all header fields to their default values.

      CALL NEWHDR

* - USER ACTION: Include any calls to your functions or
*             subroutines here used to calculate header fields.

* - USER ACTION: Listed below are statements used to define each
*             of the SAC header fields.  With the exception of
*             the first one, no values are given.  Use your
*             own variables to define the fields you want in
*             the SAC file.  Delete the lines containing fields
*             you wish to remain undefined. Then include
*             this subroutine in your source program and
*             compile it.  Load using the SACIOL library.

*             Fields which begin with 'L' or logical fields and
*             must be set to either .TRUE. or .FALSE.  Fields
*             which begin with N are integer fields.  Fields
*             beginning with 'I' are also integer but can only
*             be set to a limited number of values, each corre-
*             sponding to a specific condition.  Fields beginning
*             with 'K' are character variables of length 8 unless
*             otherwise noted.  All other fields are floating point.

* - The following fields are required:

      NPTS	= NLEN
      B		= 0.0
      E		= B+(DT*NLEN)
      DELTA	= DT
      LEVEN	= .TRUE.

* - The following define the GMT/UT corresponding to zero seconds
*   in the file.  This the time corresponding to zero which is NOT
*   necessarily the time of the beginning of the file.  That is B.

      NZYEAR	= IY
      NZJDAY	= ID
      NZHOUR	= IH
      NZMIN	= IM
      NZSEC	= INT(SS)
      NZMSEC	= 1000.0*(SS-NZSEC)
      IZTYPE	= IB

* - The next set of fields are time picks.  These are all in seconds
*   and are also relative to the zero of the file not the beginning.

      O=0.0
*      A=
*      T0=
*      T1=
*      T2=
*      T3=
*      T4=
*      T5=
*      T6=
*      T7=
*      T8=
*      T9=
*      F=

* - The next set of fields are ids for the last 13 time picks.

*      KO=
*      KA=
*      KT0=
*      KT1=
*      KT2=
*      KT3=
*      KT4=
*      KT5=
*      KT6=
*      KT7=
*      KT8=
*      KT9=
*      KF=

* - The next set of fields define the station properities:

      KSTNM	= NSTA 
      STLA	= rla
      STLO	= rlo
*      STDP	= 
*      STEL	= 
*      ISTREG	=
*      CMPAZ	= 
*      CMPINC	= 
      KCMPNM	= NCHN
*      LPSPOL	= 
      KNETWK	= NTYP


* - The next set of fields define the event properties:

* - (KEVNM can be up to 16 characters long.)
      KEVNM	= NCHN
      EVLA	= ela
      EVLO	= elo
      EVDP	= edep
*      EVEL	= 
*      IEVREG	=
      IEVTYP	= IUNKN

* - A few miscellaneous fields here:

      SCALE	= 1.0
      IFTYPE	= ITIME
      IDEP	= IUNKN

      LOVROK	= .FALSE.

* - The following fields are not used by SAC at all.  They are in the
*   header so users can pass data to their own programs if needed:

*      USER0=
*      USER1=
*      USER2=
*      DIST=
*      GCARC	=
*      AZ	= 
*      BAZ	= 
*      IINST	=
*      USER8=
*      USER9=
*      KUSER1=
*      KUSER2=
*      KUSER3=

* - The remaining header fields are not currently being used by SAC
*   but commands are planned to make use of them:

*      ISYNTH=
*      IQUAL=
*      IINST=
*      RESP0=
*      RESP1=
*      RESP2=
*      RESP3=
*      RESP4=
*      RESP5=
*      RESP6=
*      RESP7=
*      RESP8=
*      RESP9=

* - USER ACTION:  Relax and have a beer.  You are almost done.
*                 Just put this subroutine in your code and compile.

* - Write the file to disk.

      CALL WSAC0 (KNAME, XARRAY, YARRAY, NERR)

 8888 RETURN

*=====================================================================
* MODIFICATION HISTORY:
*    800827:  Original version [Prime].
*=====================================================================

      END
