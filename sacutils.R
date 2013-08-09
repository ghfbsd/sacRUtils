## Utility functions to access SAC files and access the filtering functionality
## of SAC from R.
##
## Usage:
##   The load library sacutils.so must be available in R's library search path.
##   See R documentation on dyn.load() for information.
##
##
##   o<-rsac(file, hdronly=F) - Read the data in SAC file.  Omit reading data if
##      hdronly is T.  Return value is an R object o with fields
##      o$ydata - y data values
##      o$xdata - x data values (if X-Y file, otherwise nothing)
##      o$xxxx - value of header variable xxxx
##
##   sacdemo() - Simple demo that reads a SAC file and makes a plot.
##
##   sacdemo('filter') - Simple demo that shows the use of the filtering
##      abilities in SAC available to auxiliary programs.
##
##   saclp(dat, dt, f, poles=1, passes=1, type='BUTTER', tran=c(0,0))
##       Low-pass filter of data given by dat.  Sample interval is dt.
##       Frequency cutoff is f.  Type of filter is BUTTER, BESSEL, C1 or C2
##       (see SAC documentation for meaning; default is BUTTER).  For
##       Chebychev filters (C1, C2), tran=c(lo,hi) are the attenuation limits as
##       fractions of the signal amplitude.
##
##   sachp(dat, dt, f, poles=1, passes=1, type='BUTTER', tran=c(0,0))
##       As for saclp() but a high-pass filter.
##
##   sacbp(dat, dt, fl, fh, poles=1, passes=1, type='BUTTER', tran=c(0,0))
##       Bandpass filter of data given by dat.  Sample interval is dt.
##       Low frequency cutoff is fl, high frequency cutoff is fh.
##       Type of filter is BUTTER, BESSEL, C1 or C2, and for
##       Chebychev filters (C1, C2), tran=c(lo,hi) are the attenuation limits as
##       fractions of the signal amplitude.
##
##   sacbr(dat, dt, fl, fh, poles=1, passes=1, type='BUTTER', tran=c(0,0))
##       As for sacbp() but a band reject filter.

rsac<-function(fn,hdronly=F){
   if (!exists('readsac')) dyn.load('sacutils.so')
   .Call('readsac', fn, hdronly, PACKAGE='sacutils')
}

sacdemo<-function(type='basic'){
   ## Create a temporary data file using SAC
   if ('no' == readLines(pipe(
      'test -f /tmp/seismogram.sac && echo yes || echo no'))
   ) system('echo "funcgen seismogram; write /tmp/seismogram.sac; quit" | sac')
   obj <- rsac('/tmp/seismogram.sac',F)
   t <- obj$b + obj$delta*(0:(obj$npts-1))

   dat<-obj$ydata - mean(obj$ydata)

   chk <- any(type == c('basic','filter'))
   if (!chk) type<-'basic'

   if (type == 'basic') {
      plot(t,dat,
	 type='l',
	 xlab='Time (s)',
	 ylab=obj$idep,
	 main="/tmp/seismogram.sac"
      )
   } else if (type == 'filter') {
      plot(t, dat,
	 type='l',
	 xlab='Time (s)',
	 ylab=obj$idep,
	 main="/tmp/seismogram.sac - raw and LP 1 Hz data"
      )
      flt<-saclp(dat, obj$delta, 1.0, passes=2)
      lines(t, flt, lty=3)
   } else {
      cat('**Unknown demo type "',type,'"\n')
   }
}

saclp<-function(dat,dt,f,poles=1,passes=1,type='BUTTER',tran=c(0,0)){
   if (!exists('xapiir')) dyn.load('sacutils.so')
   .C('xapiir',
      dat, length(dat), type, tran[1], tran[2], as.integer(poles), 'LP',
      0,f,dt,as.integer(passes), PACKAGE='sacutils')[[1]]
}

sachp<-function(dat,dt,f,poles=1,passes=1,type='BUTTER',tran=c(0,0)){
   if (!exists('xapiir')) dyn.load('sacutils.so')
   .C('xapiir',
      dat, length(dat), type, tran[1], tran[2], as.integer(poles), 'HP',
      0,f,dt,as.integer(passes), PACKAGE='sacutils')[[1]]
}

sacbp<-function(dat,dt,flo,fhi,poles=1,passes=1,type='BUTTER',tran=c(0,0)){
   if (!exists('xapiir')) dyn.load('sacutils.so')
   .C('xapiir',
      dat, length(dat), type, tran[1], tran[2], as.integer(poles), 'BP',
      flo,fhi,dt,as.integer(passes), PACKAGE='sacutils')[[1]]
}

sacbr<-function(dat,dt,flo,fhi,poles=1,passes=1,type='BUTTER',tran=c(0,0)){
   if (!exists('xapiir')) dyn.load('sacutils.so')
   .C('xapiir',
      dat, length(dat), type, tran[1], tran[2], as.integer(poles), 'BR',
      flo,fhi,dt,as.integer(passes), PACKAGE='sacutils')[[1]]
}
