      subroutine post_commons
c
      implicit none
c
      include 'postcommons.h'
c
      namelist/post/post_type
     .             ,nfiles,incr,begin,ngrid,ncropx,ncropy
c
      namelist/avs/flddir,corflg,corfil
     .            ,nfields,cfield
c
      namelist/ncarg/level,ncplots,ncfields,ncfield,cint
c_______________________________________________________________________________
c
c *** Read POST namelist.
c
      open(1,file='POSTIN',form='formatted',status='old',err=900)
      read(1,post,end=901)
      call upcase(post_type,post_type)
      if (post_type .eq. 'AVS') then
         read(1,avs,end=902)
      elseif (post_type .eq. 'NCARG') then
         read(1,ncarg,end=903)
      else
         print *,'Unrecognized POST_TYPE - ',post_type
         print *,'Abort.'
         close(1)
         stop
      endif
      close(1)
c
      return
c
c *** Error trapping.
c
900   print *,'POSTIN namelist not found.'
      stop
c
901   print *,'Error reading namelist - post'
      stop
c
902   print *,'Error reading namelist - avs'
      stop
c
903   print *,'Error reading namelist - ncarg'
      stop
c
      end
