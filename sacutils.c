/*
   R access to SAC routines.  All must be dynamically loadable.

   Each routine requires some interfacing code to make it able to call the
   underlying SAC routine, or to emulate the underlying SAC functionality.

   Major pieces are:
   1 - declare names of accessible routines and parameter types
   2 - convert real-valued parameters to appropriate type for routine
       call
   3 - write code called at library load time to present accessible
       routines callable by R (R_registerRoutines).

   The name of the initialization routine for the library is based on the
   file name of the library minus any suffix used to indicate it is dynamically
   loadable.  This is somewhat inflexible because it means that a change in
   file name requires a change to the source code.  Thus a dynamically
   loaded object file called XXX.so gets initialized by calling R_init_XXX.

   At present, there two routines provided for R use:
      - read SAC file headers and, optionally data;
      - the xapiir filtering routines.

   Should implement functions of:
        SAC facility      | R routine
      --------------------+------------
      read header         |   rsac
      read header & data  |   rsac
      write header & data |  (not yet implemented)

   G. Helffrich/U. Bristol
      17 June 2012
      Revised 8 Aug. 2013, 7 Mar. 2016

   Copyright (c) 2013-2016 by G. Helffrich.
   All rights reserved.
 
   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the distribution.  
    * The names of the authors may not be used to endorse or promote products
      derived from this software without specific prior written permission.

   THERE IS NO WARRANTY EXPRESSED OR IMPLIED, NOR LIABILITY FOR DAMAGES ASSUMED,
   IN PROVIDING THIS SOFTWARE.

*/
#include <unistd.h>
#include <sys/stat.h>
#include <stdio.h>

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <R_ext/Print.h>

#define DEBUG 0

static void xapiir_intf(
   double *data, int *ndat, char **class, double *Dtrbndw, double *Datten,
   int *nord, char **type, double *Dflo, double *Dfhi, double *Ddt, int *npass
) {

   void dxapiir_();
   float
      trbndw = (float)*Dtrbndw,
      atten = (float)*Datten,
      flo = (float)*Dflo,
      fhi = (float)*Dfhi,
      dt = (float)*Ddt;

   dxapiir_(data, ndat, *class, &trbndw, &atten, nord,
      *type, &flo, &fhi, &dt, npass);
}

/* Length in words of parts of SAC file header, except N_KBUF, in chars. */
#define N_FBUF 70
#define N_NBUF 15
#define N_IBUF 20
#define N_LBUF 5
#define N_KBUF 24*8

/* Description of a field in the SAC header:
   name - field name
   stype - R SEXP type for internal value
   foff - offset in file header (words, except for character items)
   type - SAC file header type
*/
struct sacfld {
   char *name;
   short stype, foff;
   enum {fhdr, nhdr, khdr, lhdr, ihdr} type;
};

/* Description of an I-type (enumerated) SAC file header value:
   name - upper case name
   code - integer code for value (stored in file header)
*/
struct ifld {
   char *name;
   unsigned char code;
};

struct sacfld flds[] = {

   {"delta", REALSXP, 1, fhdr},
   {"depmin", REALSXP, 2, fhdr},
   {"depmax", REALSXP, 3, fhdr},
   {"scale", REALSXP, 4, fhdr},
   {"odelta", REALSXP, 5, fhdr},
   {"b", REALSXP, 6, fhdr},
   {"e", REALSXP, 7, fhdr},
   {"o", REALSXP, 8, fhdr},
   {"a", REALSXP, 9, fhdr},
   {"fmt", REALSXP, 10, fhdr},
   {"t0", REALSXP, 11, fhdr},
   {"t1", REALSXP, 12, fhdr},
   {"t2", REALSXP, 13, fhdr},
   {"t3", REALSXP, 14, fhdr},
   {"t4", REALSXP, 15, fhdr},
   {"t5", REALSXP, 16, fhdr},
   {"t6", REALSXP, 17, fhdr},
   {"t7", REALSXP, 18, fhdr},
   {"t8", REALSXP, 19, fhdr},
   {"t9", REALSXP, 20, fhdr},
   {"f", REALSXP, 21, fhdr},
   {"resp0", REALSXP, 22, fhdr},
   {"resp1", REALSXP, 23, fhdr},
   {"resp2", REALSXP, 24, fhdr},
   {"resp3", REALSXP, 25, fhdr},
   {"resp4", REALSXP, 26, fhdr},
   {"resp5", REALSXP, 27, fhdr},
   {"resp6", REALSXP, 28, fhdr},
   {"resp7", REALSXP, 29, fhdr},
   {"resp8", REALSXP, 30, fhdr},
   {"resp9", REALSXP, 31, fhdr},
   {"stla", REALSXP, 32, fhdr},
   {"stlo", REALSXP, 33, fhdr},
   {"stel", REALSXP, 34, fhdr},
   {"stdp", REALSXP, 35, fhdr},
   {"evla", REALSXP, 36, fhdr},
   {"evlo", REALSXP, 37, fhdr},
   {"evel", REALSXP, 38, fhdr},
   {"evdp", REALSXP, 39, fhdr},
   {"mag", REALSXP, 40, fhdr},
   {"user0", REALSXP, 41, fhdr},
   {"user1", REALSXP, 42, fhdr},
   {"user2", REALSXP, 43, fhdr},
   {"user3", REALSXP, 44, fhdr},
   {"user4", REALSXP, 45, fhdr},
   {"user5", REALSXP, 46, fhdr},
   {"user6", REALSXP, 47, fhdr},
   {"user7", REALSXP, 48, fhdr},
   {"user8", REALSXP, 49, fhdr},
   {"user9", REALSXP, 50, fhdr},
   {"dist", REALSXP, 51, fhdr},
   {"az", REALSXP, 52, fhdr},
   {"baz", REALSXP, 53, fhdr},
   {"gcarc", REALSXP, 54, fhdr},
   {"sb", REALSXP, 55, fhdr},
   {"sdelta", REALSXP, 56, fhdr},
   {"depmen", REALSXP, 57, fhdr},
   {"cmpaz", REALSXP, 58, fhdr},
   {"cmpinc", REALSXP, 59, fhdr},
   {"xminimum", REALSXP, 60, fhdr},
   {"xmaximum", REALSXP, 61, fhdr},
   {"yminimum", REALSXP, 62, fhdr},
   {"ymaximum", REALSXP, 63, fhdr},

   {"nzyear", INTSXP, 1, nhdr},
   {"nzjday", INTSXP, 2, nhdr},
   {"nzhour", INTSXP, 3, nhdr},
   {"nzmin", INTSXP, 4, nhdr},
   {"nzsec", INTSXP, 5, nhdr},
   {"nzmsec", INTSXP, 6, nhdr},
   {"norid", INTSXP, 8, nhdr},
   {"nevid", INTSXP, 9, nhdr},
   {"npts", INTSXP, 10, nhdr},
   {"nsnpts", INTSXP, 11, nhdr},
   {"nsn", INTSXP, 12, nhdr},
   {"nxsize", INTSXP, 13, nhdr},
   {"nysize", INTSXP, 14, nhdr},

   {"iftype", STRSXP, 1, ihdr},
   {"idep", STRSXP, 2, ihdr},
   {"iztype", STRSXP, 3, ihdr},
   {"iinst", STRSXP, 5, ihdr},
   {"istreg", STRSXP, 6, ihdr},
   {"ievreg", STRSXP, 7, ihdr},
   {"ievtyp", STRSXP, 8, ihdr},
   {"iqual", STRSXP, 9, ihdr},
   {"isynth", STRSXP, 10, ihdr},
   {"imgtyp", STRSXP, 11, ihdr},
   {"imgsrc", STRSXP, 12, ihdr},

   {"leven", LGLSXP, 1, lhdr},
   {"lspol", LGLSXP, 2, lhdr},
   {"lovrok", LGLSXP, 3, lhdr},
   {"lcalda", LGLSXP, 4, lhdr},

   {"kstnm", STRSXP, 1, khdr},
   {"kevnm", STRSXP, 2, khdr},
   {"khole", STRSXP, 4, khdr},
   {"ko", STRSXP, 5, khdr},
   {"ka", STRSXP, 6, khdr},
   {"kt0", STRSXP, 7, khdr},
   {"kt1", STRSXP, 8, khdr},
   {"kt2", STRSXP, 9, khdr},
   {"kt3", STRSXP, 10, khdr},
   {"kt4", STRSXP, 11, khdr},
   {"kt5", STRSXP, 12, khdr},
   {"kt6", STRSXP, 13, khdr},
   {"kt7", STRSXP, 14, khdr},
   {"kt8", STRSXP, 15, khdr},
   {"kt9", STRSXP, 16, khdr},
   {"kf", STRSXP, 17, khdr},
   {"kuser0", STRSXP, 18, khdr},
   {"kuser1", STRSXP, 19, khdr},
   {"kuser2", STRSXP, 20, khdr},
   {"kcmpnm", STRSXP, 21, khdr},
   {"knetwk", STRSXP, 22, khdr},
   {"kdatrd", STRSXP, 23, khdr},
   {"kinst", STRSXP, 24, khdr},
};

#define kiv(n,str) {str, n}

struct ifld icodes[] = {
      kiv(1,"ITIME"),
      kiv(2,"IRLIM"),
      kiv(3,"IAMPH"),
      kiv(4,"IXY"),
      kiv(5,"IUNKN"),
      kiv(6,"IDISP"),
      kiv(7,"IVEL"),
      kiv(8,"IACC"),
      kiv(9,"IB"),
      kiv(10,"IDAY"),
      kiv(11,"IO"),
      kiv(12,"IA"),
      kiv(13,"IT0"),
      kiv(14,"IT1"),
      kiv(15,"IT"),
      kiv(16,"IT3"),
      kiv(17,"IT4"),
      kiv(18,"IT5"),
      kiv(19,"IT6"),
      kiv(20,"IT7"),
      kiv(21,"IT8"),
      kiv(22,"IT9"),
      kiv(23,"IRADNV"),
      kiv(24,"ITANNV"),
      kiv(25,"IRADEV"),
      kiv(26,"ITANEV"),
      kiv(27,"INORTH"),
      kiv(28,"IEAST"),
      kiv(29,"IHORZA"),
      kiv(30,"IDOWN"),
      kiv(31,"IUP"),
      kiv(32,"ILLLBB"),
      kiv(33,"IWWSN1"),
      kiv(34,"IWWSN2"),
      kiv(35,"IHGLP"),
      kiv(36,"ISRO"),
      kiv(37,"INUCL"),
      kiv(38,"IPREN"),
      kiv(39,"IPOSTN"),
      kiv(40,"IQUAKE"),
      kiv(41,"IPREQ"),
      kiv(42,"IPOSTQ"),
      kiv(43,"ICHEM"),
      kiv(44,"IOTHER"),
      kiv(45,"IGOOD"),
      kiv(46,"IGLCH"),
      kiv(47,"IDROP"),
      kiv(48,"ILOWSN"),
      kiv(49,"IRLDTA"),
      kiv(50,"IVOLTS"),
      kiv(51,"IXYZ"),
      kiv(52,"IMB"),
      kiv(53,"IMS"),
      kiv(54,"IML"),
      kiv(55,"IMW"),
      kiv(56,"IMD"),
      kiv(57,"IMX"),
      kiv(58,"INEIC"),
      kiv(59,"IPDEQ"),
      kiv(60,"IPDEW"),
      kiv(61,"IPDE"),
      kiv(62,"IISC"),
      kiv(63,"IREB"),
      kiv(64,"IUSGS"),
      kiv(65,"IBRK"),
      kiv(66,"ICALTECH"),
      kiv(67,"ILLNL"),
      kiv(68,"IEVLOC"),
      kiv(69,"IJSOP"),
      kiv(70,"IUSER"),
      kiv(71,"IUNKNOWN"),
      kiv(72,"IQB"),
      kiv(73,"IQB1"),
      kiv(74,"IQB2"),
      kiv(75,"IQBX"),
      kiv(76,"IQMT"),
      kiv(77,"IEQ"),
      kiv(78,"IEQ1"),
      kiv(79,"IEQ2"),
      kiv(80,"IME"),
      kiv(81,"IEX"),
      kiv(82,"INU"),
      kiv(83,"INC"),
      kiv(84,"IO_"),
      kiv(85,"IL"),
      kiv(86,"IR"),
      kiv(87,"IT"),
      kiv(88,"IU"),
      kiv(89,"IEQ3"),
      kiv(90,"IEQ0"),
      kiv(91,"IEX0"),
      kiv(92,"IQC"),
      kiv(93,"IQB0"),
      kiv(94,"IGEY"),
      kiv(95,"ILIT"),
      kiv(96,"IMET"),
      kiv(97,"IODOR"),
      kiv(98,NULL),
};

/* Helper macro to shorten code to retrieve items from the file header */
#define A(arr,off) (unsigned char *)(arr+off)

/* Byte-swapping routines for header, various types:
   fhdr, nhdr, ihdr, lhdr
*/
#define swapl swapi
#define swapn swapi

static int swapi(unsigned char v[]){
   union {int v; unsigned char c[4];} u;
   
   u.c[0] = v[3]; u.c[1] = v[2]; u.c[2] = v[1]; u.c[3] = v[0];
   return u.v;
}

static float swapf(unsigned char v[]){
   union {float v; unsigned char c[4];} u;
   
   u.c[0] = v[3]; u.c[1] = v[2]; u.c[2] = v[1]; u.c[3] = v[0];
   return u.v;
}

static char *iencode(int code){
   /* Binary search in table for i-type code; translate into character */
   int nfld = sizeof(icodes)/sizeof(struct ifld);
   int going, lo=0, hi=nfld-1, ix;

   do {
      ix = (lo+hi)/2;
      if (code == icodes[ix].code) return icodes[ix].name;
      if (code < icodes[ix].code)
         hi = ix-1;
      else
         lo = ix+1;
      going = hi != lo;
   } while(going);
   return (code == icodes[hi].code) ? icodes[hi].name : NULL;
}

static SEXP getLE(SEXP list, const char *str) {
   /* Get the list element named str or return NULL */
   SEXP elmt = R_NilValue, names = getAttrib(list, R_NamesSymbol);
   R_len_t i;

   for (i=0; i<length(list); i++) {
      if (strcmp(CHAR(STRING_ELT(names, i)), str) == 0) {
         elmt = VECTOR_ELT(list, i);
	 break;
      }
   }
   return elmt;
}

static SEXP newRSAC(SEXP nm){
   /* Create a new RSAC object for return, and initially fill with
      undefined values.  Also sets name.
   */

   SEXP res, fld;
   R_len_t nfld = sizeof(flds)/sizeof(struct sacfld), i;

   /* Allocate list object to hold result */
   PROTECT(res = allocVector(VECSXP, 3+nfld));

   /* Allocate name vector and set up unusual names */
   PROTECT(fld = allocVector(STRSXP, nfld+3));
   SET_STRING_ELT(fld, 0, mkChar("name"));
   SET_STRING_ELT(fld, 1, mkChar("ydata"));
   SET_STRING_ELT(fld, 2, mkChar("xdata"));

   /* Allocate rest of field names in header, then associate with
      resulting object */
   for(i=0; i<nfld; i++)
      SET_STRING_ELT(fld, 3+i, mkChar(flds[i].name));
   setAttrib(res, R_NamesSymbol, fld);
   UNPROTECT(1);

   /* Set name */
   PROTECT(fld = allocVector(STRSXP,1));
   STRING_PTR(fld)[0] = STRING_ELT(nm, 0);
   SET_VECTOR_ELT(res, 0, fld);
   UNPROTECT(1);

   /* Stuff in undefined values of each type */
   for(i=0; i<nfld; i++) {
      SEXP fld;
      switch (flds[i].stype) {
      case REALSXP:
         PROTECT(fld = allocVector(REALSXP,1));
         REAL(fld)[0] = NA_REAL;
	 break;
      case INTSXP:
         PROTECT(fld = allocVector(INTSXP,1));
         INTEGER(fld)[0] = NA_INTEGER;
	 break;
      case LGLSXP:
         PROTECT(fld = allocVector(LGLSXP,1));
         LOGICAL(fld)[0] = NA_LOGICAL;
	 break;
      case STRSXP:
         PROTECT(fld = allocVector(STRSXP,1));
         SET_STRING_ELT(fld, 0, NA_STRING);
	 break;
      default:
	 error("**RSAC internal error:  Unexpected R SEXP type (object"
	       " setup)");
      }
      SET_VECTOR_ELT(res, 3+i, fld);
      UNPROTECT(1);
   }

   return res;
}

static SEXP rsac(SEXP nm, SEXP hdr){

   /* Allocate and possibly read data for and fill an RSAC object
      Called via:

         .Call("rsac", nm, hdr)

      Assumes:
         nm - name of a SAC file to read or null string.  If null string,
	    only a header object returned with all values undefined.
	 hdr - TRUE or FALSE depending on whether the header only or
	    header and data are to be read.

      Returns:
         Function result - list (RSAC object) with elements the header
	    variables, plus the following names:
	    $name - file name
	    $ydata - dependent variable values
	    $xdata - independent variable values (if XY-type file)

      
      Any undefined values in the header are returned as NAs.

      G. Helffrich/U. Bristol
         19 June 2012
   
   */

   SEXP res;
   int hdronly = asLogical(hdr);
   int def;
   FILE *fd = NULL;

   /* Check parameters */
   if(!isString(nm)) return R_NilValue;
   if(hdronly == NA_LOGICAL)
      error("rsac second argument must be TRUE or FALSE");

   /* Create object for return */
   res = newRSAC(nm);

   /* Check if file to be read, and, if so, whether it is readable.
      If not, set up default values. */
   def = 0 == strlen(CHAR(STRING_ELT(nm,0)));
   if (!def)
      def = 0 != access(CHAR(STRING_ELT(nm, 0)), R_OK);
   while (!def) {
      struct stat info;
      int swp;
      R_len_t nfld = sizeof(flds)/sizeof(struct sacfld), i;
      float fbuf[N_FBUF];
      int nbuf[N_NBUF];
      int ibuf[N_IBUF];
      int lbuf[N_LBUF];
      char kbuf[N_KBUF];

      if ((def = (0 != stat(CHAR(STRING_ELT(nm, 0)), &info)))) break;
      if ((def = (info.st_size < 158*4))) break;
      fd = fopen(CHAR(STRING_ELT(nm, 0)), "r");
      if ((def = (fd == NULL))) break;
      if ((def = N_FBUF != fread(fbuf, sizeof(float), N_FBUF, fd))) break;
      if ((def = N_NBUF != fread(nbuf, sizeof(int),   N_NBUF, fd))) break;
      if ((def = N_IBUF != fread(ibuf, sizeof(int),   N_IBUF, fd))) break;
      if ((def = N_LBUF != fread(lbuf, sizeof(int),   N_LBUF, fd))) break;
      if ((def = N_KBUF != fread(kbuf, sizeof(char),  N_KBUF, fd))) break;

      /* Have file header.  Make checks as to validity. */
      swp = nbuf[6] != 6;
      if (swp && (def = swapn(A(nbuf,6)) != 6)) break;

      /* Have recognizable file header.  Decode values and set into R object */
      for(i=0; i<nfld; i++) {
	 float fval;
	 int j, k, nval, lval, iraw, ix=flds[i].foff-1;
         char *nm = flds[i].name, *ival, *kval;
	 switch (flds[i].type) {
	 case fhdr:
	    fval = swp ? swapf(A(fbuf,ix)) : fbuf[ix];
            if (fval != -12345.0) REAL(getLE(res, nm))[0] = fval;
	    break;
	 case lhdr:
	    lval = 0 != swp ? swapl(A(ibuf,ix)) : lbuf[ix];
            if (lval != -12345) LOGICAL(getLE(res, nm))[0] = lval;
	    break;
	 case nhdr:
	    nval = swp ? swapn(A(nbuf,ix)) : nbuf[ix];
            if (nval != -12345) INTEGER(getLE(res, nm))[0] = nval;
	    break;
	 case ihdr:
	    iraw = swp ? swapn(A(ibuf,ix)) : ibuf[ix];
	    if (iraw != -12345) {
	       ival = iencode(iraw);
	       if (ival) SET_STRING_ELT(getLE(res, nm), 0, mkChar(ival));
	    }
	    break;
	 case khdr:
	    kval = kbuf+ix*8;
	    j = (ix == 1) ? 16:8;
	    for(k = j-1; k>=0; k--)
	       if (kval[k] != '\0' && kval[k] != ' ') break;
	    if (k>=0 && (k != 5 || 0 != strncmp("-12345",kval,6)))
	       SET_STRING_ELT(getLE(res, nm), 0, mkCharLen(kval,k+1));
	    break;
	 default:
            fclose(fd);
	    error("**RSAC internal error:  Unexpected R SEXP type (file"
	          " header translation)");
	 }
      }

      /* Want to keep data? */
      if (hdronly) break;

      /* Read in data into buffer and decode */
#if DEBUG
      Rprintf("**rsac:  Reading %d data points...\n",
         INTEGER(getLE(res, "npts"))[0]);
#endif
      {
         int mxbuf = BUFSIZ/sizeof(float);
	 int cnt, want;
	 const char *iftype = CHAR(STRING_ELT(getLE(res,"iftype"),0));
         R_len_t n, nleft;
	 SEXP data;
	 double *rdat;
	 float inbuf[BUFSIZ/sizeof(float)];

	 nleft = INTEGER(getLE(res, "npts"))[0];
	 n = 0; 
         PROTECT(data = allocVector(REALSXP, nleft));
         SET_VECTOR_ELT(res, 1, data);
	 rdat = REAL(data);

	 while (nleft>0) {
	    want = nleft < mxbuf ? nleft : mxbuf;
	    cnt = fread(inbuf, sizeof(float), want, fd);
	    if (cnt>0) {
	       for(i=0; i<cnt; i++)
	          rdat[n+i] = swp ? swapf(A(inbuf,i)) : inbuf[i];
	       n+=cnt; nleft-=cnt;
	    } else if (cnt<=0)
	       break;
	 }
	 UNPROTECT(1);
	 if (nleft > 0) {
	    warning("rsac: Incomplete Y data read (%d points omitted) for"
	       " %f\n", nleft, CHAR(STRING_ELT(nm,0)));
	    break;
	 }
	 if (0 == strcmp(iftype,"IXY") ||
	     0 == strcmp(iftype,"IRLIM") ||
	     0 == strcmp(iftype,"IAMPH")) {

	    nleft = INTEGER(getLE(res, "npts"))[0];
	    n = 0; 
	    PROTECT(data = allocVector(REALSXP, nleft));
	    SET_VECTOR_ELT(res, 2, data);
	    rdat = REAL(data);
	    while (nleft>0) {
	       want = nleft < mxbuf ? nleft : mxbuf;
	       cnt = fread(inbuf, sizeof(float), want, fd);
	       if (cnt>0) {
		  for(i=0; i<cnt; i++)
		     rdat[n+i] = swp ? swapf(A(inbuf,i)) : inbuf[i];
		  n+=cnt; nleft-=cnt;
	       } else if (cnt<=0)
		  break;
	    }
	    UNPROTECT(1);
	    if (nleft > 0) {
	       warning("rsac: Incomplete X data read (%d points omitted) for"
		  " %f\n", nleft, CHAR(STRING_ELT(nm,0)));
	       break;
	    }
	 }
      }
      break;

   }

   if (fd) fclose(fd), fd = NULL;
   if (def) {
      LOGICAL(getLE(res, "leven"))[0] = 1;
      LOGICAL(getLE(res, "lovrok"))[0] = 1;
      LOGICAL(getLE(res, "lcalda"))[0] = 1;
      STRING_PTR(getLE(res,"iftype"))[0] = mkChar("ITIME");
   }

   /* Unprotect return object and return it. */
   UNPROTECT(1); 
   return res;
}

R_NativeArgStyle xapiir_styles[] = 
      { R_ARG_IN_OUT, // DATA,
        R_ARG_IN,     // NDAT,
	R_ARG_IN,     // CLASS,
	R_ARG_IN,     // TRBNDW,
	R_ARG_IN,     // ATTEN,
        R_ARG_IN,     // NORD,
	R_ARG_IN,     // TYPE,
	R_ARG_IN,     // FLO,
	R_ARG_IN,     // FHI,
	R_ARG_IN,     // DT,
        R_ARG_IN,     // NPASS
      };

R_NativePrimitiveArgType xapiir_types[] =
      { REALSXP,   // DATA,
        INTSXP,    // NDAT,
	STRSXP,    // CLASS,
	REALSXP,   // TRBNDW,
	REALSXP,   // ATTEN,
        INTSXP,    // NORD,
	STRSXP,    // TYPE,
	REALSXP,   // FLO,
	REALSXP,   // FHI,
	REALSXP,   // DT,
        INTSXP,    // NPASS
      };

R_CMethodDef Crtns[] = {

   {"xapiir", (DL_FUNC) &xapiir_intf, 11, xapiir_types, xapiir_styles},
   {NULL, NULL, 0, NULL, NULL}  /* end-of-list sentinel */

};

R_CallMethodDef Rrtns[] = {
   {"readsac", (DL_FUNC) &rsac, 2},
   {NULL, NULL, 0}  /* end-of-list sentinel */
};

void R_init_sacutils(DllInfo *info){

   R_registerRoutines(info, Crtns, Rrtns, NULL, NULL);
#if DEBUG
   Rprintf("**Loaded SACUTILS\n");
#endif

}

void R_unload_sacutils(DllInfo *info){
#if DEBUG
   Rprintf("**Unloaded SACUTILS\n");
#endif

}
