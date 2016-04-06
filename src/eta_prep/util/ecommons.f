      subroutine eta_commons
c
      implicit none
c
      include 'ecommons.h'
c
      namelist/model_grids/tlm0d,tph0d,im,jm,lm,ptinp,dlmd,dphd
Cmp     .                    ,dt,w,idtad,imonth,idate,iyear,istrtim
     .                    ,dt,idtad,imonth,idate,iyear,istrtim
     .                    ,nsoil
     .                    ,ninit,init_in,init_gdsdir,init_out
     .                    ,tboco,nhour
c
Cmp      namelist/surface/topo_in,topo_out
C     .                ,soili,soil_in,soil_out
C     .                ,vegi,veg_in,veg_out
Cmp     .                ,surfi,climsst,surft,surfw
      namelist/surface/topo_in,topo_out,GRIBSOIL,seares
c
Cmp      namelist/plot_diag/plot,plot_out
c
      namelist/init_diag/sigma,hires
c_______________________________________________________________________________
c
c *** Read ETA namelist.
c
      open(1,file='ETAIN',form='formatted',status='old',err=900)
      read(1,model_grids,end=901)
      read(1,surface,end=902)
Cmp      read(1,plot_diag,end=903)
      read(1,init_diag,end=904)
      close(1)
c
c *** Fill model grid constants.
c
Cmp      im=-wbd/dlmd+1.5
Cmp      jm=-2*sbd/dphd+1.5

	WBD=-(IM-1)*DLMD
	SBD=-((JM-1)/2.)*DPHD
Cmp
      imt=2*im-1
      jmt=jm/2+1
      imjm=im*jm-jm/2
      imjm1=imjm-1
      im1=im-1
      im2=im-2
      jm1=jm-1
      jm2=jm-2
      jm3=jm-3
      jm4=jm-4
      jm5=jm-5
      imm1=imt-1
      imm3=imt-3
      imm5=imt-5
      jmm1=jmt-1
      jmm2=jmt-2
      kb=im*2+jm-3
      lb=jmt*2+imt-3
      lmp1=lm+1
      jam=6+2*(jm-10)
c
      ctph0=cosd(tph0d)
      stph0=sind(tph0d)
c
      print *,' '
      print *,'ETA grid parameters:'
      print *,'   im   =',im   ,' jm  =',jm
      print *,'   imt  =',imt  ,' jmt =',jmt
      print *,'   imjm =',imjm
      print *,' '
	print*, 'soil levels  in ecommons ', nsoil
Cmp	
	print*,' centered at ',TPH0D,TLM0D
C	print*, TPH0D, TLM0D
C	print*, 'cos/sin vals...', ctph0,stph0
Cmp
c

	write(6,*) 'seares in eta_commons: ', seares
      return
c
c *** Error trapping.
c
900   print *,'ETAIN namelist not found.'
      stop
c
901   print *,'Error reading namelist - model_grids'
      stop
c
902   print *,'Error reading namelist - surface'
      stop
c
903   print *,'Error reading namelist - plot_diag'
      stop
c
904   print *,'Error reading namelist - init_diag'
      stop
c
      end
