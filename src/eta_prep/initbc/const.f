      subroutine eta_const
c
c *** Original code received from U. of Athens and modified at FSL.
c
c *** Two-dimensional horizontal indexing into one-dimensional,     
c     defines dummy initial and boundary moisture values        
c     and calculates constants needed for the one-dimensional  
c     version of the ub/nmc model.                            

C
C
C	revisions adapted from Z. Janjic version of this code.  Now 
C	stays in im,jm indexing, and seems to produce better results

c
      implicit none
c
      include 'ecommons.h'
c
      real*4 pt,aeta(lm),eta(lmp1),dfl(lmp1),detac(lm)

	REAL,ALLOCATABLE:: RES(:,:),WET(:,:),SNO(:,:)
	REAL,ALLOCATABLE:: SST(:,:),SI(:,:),CMC(:,:),ALBEDO(:,:)

	REAL,ALLOCATABLE:: HBM2(:,:),VBM2(:,:),VBM3(:,:)
	REAL,ALLOCATABLE:: SM(:,:),SICE(:,:),HTM(:,:,:),VTM(:,:,:)
c
      real*4 pdb(kb,2)
     .      ,tb(kb,lm,2),qb(kb,lm,2)
     .      ,ub(kb,lm,2),vb(kb,lm,2)
     .      ,pd(im,jm)
     .      ,pdt(im,jm),fist(im,jm)  

	REAL,ALLOCATABLE:: TT(:,:,:),UT(:,:,:),VT(:,:,:),QT(:,:,:)
	REAL,ALLOCATABLE:: SFCGRID(:,:,:),SMC(:,:,:),STC(:,:,:)

       real dum2(im,jm)

c
C	real smc,stc,tg
     	real	tg(im,jm)
      integer*4 idat(3),ihrst,ntsd,i,j,k,l,len,II,JJ
	common /mytime/idat
	real*4 pdmin
c
      logical run
c_______________________________________________________________________________
c
      print*,'im,jm=',im,jm

c
      len=index(init_out//' ',' ')-1
      if (init_out(len:len) .ne. '/') then
         len=len+1
         init_out(len:len)='/'
      endif
      open(1,file=init_out(1:len)//'preproc.init'
     .    ,status='old',form='unformatted')

	ALLOCATE(UT(IM,JM,LM),VT(IM,JM,LM),TT(IM,JM,LM),QT(IM,JM,LM))
	ALLOCATE(SM(IM,JM),RES(IM,JM))
	write(6,*) 'to preproc.init read'
C
C	       L   I3    I    
      read(1) run,idat,ihrst,ntsd,ut,vt
	write(6,*) 'middle of preproc.init read'
      read(1) tt,qt,pdt,fist,sm,res,eta,pt,detac,aeta,dfl            
	write(6,*) 'past preproc.init read'

	write(6,*) 'some res values: ', (res(i,jm/2),i=1,im)
	

C        do J=jm,1,-1
C        write(6,666) (pdt(i,j)/100.,i=im-18,im)
C        enddo
  666   format(20(f5.0,1x))

	ALLOCATE(SFCGRID(IM,JM,12))
	read(1) sfcgrid
	
	ALLOCATE(STC(IM,JM,NSOIL),SMC(IM,JM,NSOIL))
	if (GRIBSOIL) then

Cmp     pull out the smc and stc values from sfcgrid
	do K=1,4
        do j=1,jm
        do i=1,im
        stc(i,j,K)=sfcgrid(i,j,2*K+3)
        smc(i,j,K)=sfcgrid(i,j,2*K+4)
        enddo
        enddo
	enddo
	endif

      close(1)
c
	ALLOCATE(HBM2(IM,JM),VBM2(IM,JM),VBM3(IM,JM),HTM(IM,JM,LM))
	ALLOCATE(VTM(IM,JM,LM),SICE(IM,JM),WET(IM,JM),SNO(IM,JM))
	ALLOCATE(SST(IM,JM),SI(IM,JM),ALBEDO(IM,JM))
      call cnsts(pt,aeta,eta,dfl,detac
     .          ,res,fist,wet,sno,sst,si,albedo
     .          ,hbm2,vbm2,vbm3,sm,sice,htm,vtm,smc,stc,tg)
c
	do j=1,jm
	do i=1,im
         res(i,j)=1./res(i,j)                                                  
        enddo
	enddo
C
      print *,' '
      print *,'Creating eta initial condition file...'
      print *,'Write to file : ',init_out(1:len)//'init.file'
      open(1,file=init_out(1:len)//'init.file'
     .    ,status='unknown',form='unformatted')
      write(1) run,idat,ihrst,ntsd

Cmp----------------------------
	write(6,*) 'in CONST: pd values'
	pdmin=9999999.
	do JJ=jm,1,-1
	do II=1,IM
	if (pdt(II,JJ) .lt. pdmin) pdmin=pd(II,JJ)
	enddo
	enddo
	write(6,*) 'pdmin in CONST is: ', pdmin
      write(1) pdt
      write(6,*) 'pdt: ',pdt(1,1),pdt(21,21),pdt(im,jm)
Cmp---------------------------
      write(1) res
C-----------------------------
	write(6,*) 'fist: ', fist(1,1),fist(21,21),fist(im,jm)
      write(1) fist
C-----------------------------
      do l=1,lm
	do j=1,jm
	do i=1,im
	dum2(i,j)=ut(i,j,l)
	enddo
	enddo
         write(1) dum2
        print *,'u: ',L,dum2(1,1),dum2(50,70),dum2(im,jm)
      enddo

	DEALLOCATE(UT)
C-----------------------------
      do l=1,lm
	do j=1,jm
        do i=1,im
        dum2(i,j)=vt(i,j,l)
        enddo
        enddo
         write(1) dum2
        if (l.eq.20) print *,'v:',dum2(1,1),dum2(21,21),dum2(im,jm)
      enddo
	DEALLOCATE(VT)
C----------------------------
      do l=1,lm
	do J=1,jm
	do I=1,im
	dum2(i,j)=tt(i,j,l)
	enddo
	enddo
         write(1) dum2
        if (l.eq.20) print *,'t:',dum2(1,1),dum2(21,21),dum2(im,jm)
      enddo
	DEALLOCATE(TT)
C------------------------------
      do l=1,lm
        do J=1,jm
        do I=1,im
        dum2(i,j)=qt(i,j,l)
        enddo
        enddo
         write(1) dum2
        if (l.eq.20) print *,'q:',dum2(1,1),dum2(21,21),dum2(im,jm)
      enddo
	DEALLOCATE(QT)
C------------------------------
      write(1) wet
      write(1) sno
      write(1) smc

	DEALLOCATE(WET,SNO,SMC)
C-----------------------------------
C  SET VEGETATION CANOPY WATER CONTENT EQUAL TO ZERO EVERYWHERE FOR NOW
	ALLOCATE(CMC(IM,JM))
	cmc=0.
      write(1) cmc
	DEALLOCATE(CMC)
C-----------------------------------
      write(1) stc
	write(6,*) ' soil tmps (layer 1) '
	do JJ=JM,1,-JM/30
	write(6,922) (stc(II,JJ,1),II=1,IM,IM/12)
  922	format(25(f4.0,1x))
	enddo

	DEALLOCATE(STC)
C-------------------------------------
      close(1)
      end                                        
c
c===============================================================================
c
      subroutine cnsts(pt,aeta,eta,dfl,detac
     .                ,res,fis,wet,sno,sst,si,albedo
     .                ,hbm2,vbm2,vbm3,sm,sice,htm,vtm
     .		      ,smc,stc,tg)
c
c *** Routine for initialization of constants and variables         
c
      implicit none
c
      include 'ecommons.h'
      include 'econstants.h'
c
      integer*4 itb,jtb,itbq,jtbq,JJ
      parameter (itb=76,jtb=134
     .          ,itbq=152,jtbq=440)
c
c     common/pteta/                       
      real*4 pt,aeta(lm),eta(lmp1),dfl(lmp1),detac(lm)
     .      ,res(im,jm),fis(im,jm),wet(im,jm),sno(im,jm)              
     .      ,sst(im,jm),si(im,jm),cmc(im,jm)
c
c     common/masks/                        
      real*4 hbm2(im,jm),vbm2(im,jm),vbm3(im,jm)
     .      ,sm(im,jm),sice(im,jm)  
     .      ,htm(im,jm,lm),vtm(im,jm,lm)
c
      real*4 dxj(jm),wpdarj(jm),cpgfuj(jm),curvj(jm),fcpj(jm)      
     .      ,fdivj(jm),emj(jm),emtj(jm),fadj(jm)                  
     .      ,ddmpuj(jm),ddmpvj(jm),hdacj(jm)                              
     .      ,qsold(jtb),pold(jtb),qsnew(jtb),pnew(jtb)                  
     .      ,y2p(jtb),app(jtb),aqp(jtb)                              
     .      ,theold(jtb),told(jtb),thenew(jtb),tnew(jtb)                  
     .      ,y2t(jtb),apt(jtb),aqt(jtb)                              

Cmp
C	real smc,stc,tg
	real smc(im,jm,nsoil),stc(im,jm,nsoil),
     +			tg(im,jm)
Cmp
c
      integer*4 khla(jam),khha(jam),kvla(jam),kvha(jam)                  
     .         ,khl2(jm),khh2(jm),kvl2(jm),kvh2(jm),kvl                  
Cmp     .	       ,ihw(jm),ihe(jm),ivw(jm),ive(jm),ihl,ihh
     .	       ,ihl,ihh,ihe,ihw
c
      integer lmh(im,jm),lmv(im,jm)                                        
c
      real*4 rdeta(lm),f4q2(lm),diff
     .      ,em(jam),emt(jam)                                          
     .      ,dx(im,jm),wpdar(im,jm),cpgfu(im,jm),curv(im,jm),fcp(im,jm) 
     .      ,fdiv(im,jm),fad(im,jm),f(im,jm),ddmpu(im,jm),ddmpv(im,jm) 
     .      ,glat(im,jm),glon(im,jm),glatr(im,jm),glonr(im,jm)        
     .      ,deta2(lm),aeta2(lm),eta2(lmp1),dfrlg(lmp1)                         
     .      ,qs0(jtb),sqs(jtb),the0(itb),sthe(itb)                  
     .      ,epsr(im,jm),gffc(im,jm),wfk(im,jm)             
     .      ,hdac(im,jm),hdacv(im,jm)                                        
     .      ,ptbl(itb,jtb),ttbl(jtb,itb)                                  
cds new convection
     .      ,ttblq(jtbq,itbq),the0q(itbq),stheq(itbq)
c
      integer*4 idum2(im,jm),idat(3),ierr
c
      real*4 deta1(lm),aeta1(lm),eta1(lmp1)                                
     .      ,dum2(im,jm),dum2b(im,jm),dum2a(im,jm)
     .      ,dum2sm(im,jm)
c
      integer*4 kpm,kpm1,kthm,kthm1,kvh,khl,khh
     .         ,nddamp
     .         ,list,len,i,j,k,l,ja,nfcst,nbc
c
      real*4 vegfrc(im,jm),sldpth(nsoil),rtdpth(nsoil)
     .      ,f4d,f4q,ctlm,ef4t,stlm,aph,stph,ctph,tlm
     .      ,sinphi,coslam,fp,fact,thl,en,ent,dy,cpgfv
     .      ,dtad,tsph,acdt,dxp,wbi
     .      ,rdph,rdlm,sbi,anbi,ebi
     .      ,rdq,rdth,rdthe,rdtheq,rdp,rdpq
     .      ,pl,pt1,pt2,r1,ph,tph0,tph,tdph,tdlm,cddamp
     .      ,thh,ti0,wb,dph,dlm,sb
c
	INTEGER*4 I1D(360,180),IONETA(im,jm), JULM(13),JULD,
     .  ILON1,ILON2,ILAT1,ILAT2
     .  
       REAL*4 ALBC1(361,180),ALBC2 (361,180),ALBC3(361,180),
     .	  ALBC4(361,180),ALBC  (361,180),ALBEDO(IM,JM)
     .   ,S1,S2,W1,W2,AR1,AR2,AR3,AR4,H1,D5,D01,HM1,ELON,ELAT,
     .    H90,H360,WGT1,WGT2,D00,DIF,alb1d(IMJM),radfac

	parameter (H1=1.0,D5=5.E-1,D01=1.00E-2,HM1=-1.0,
     .	H90=90.0,H360=360.0,D00=0.0,radfac=57.295777951)
    
Cmp
	common /mytime/idat
	DATA JULM/0,31,59,90,120,151,181,212,243,273,304,334,365/
c
c *** Universal constants.
c
      real*4 a,twom
      data a/6376000./,twom/.00014584/
c
c *** Dissipation & turbulence.
c
      real*4 coac,codamp,tddamp,dfc,ddfc
	data codamp/0150.00/,w/0.25/
     .    ,tddamp/48.00/,dfc/01./,ddfc/8.0/
c
c
c *** Surface data.
c
Cmp	these should be same as operational now.

      real*4 ros,cs,ds,aks,dzg,tg0,tga,roi,ci,di,aki,dzi,elwv,plq
      data ros/1500./,cs/1339.2/,ds/.100/,aks/.0000005  /,dzg/02.50/       
     .,tg0/258.16/,tga/30./                                             
     .,roi/916.6/,ci/2060.0/,di/.100/,aki/.000001075/,dzi/2.0/          
     .,ti0/271.16/                                                      
     .,elwv/2.50e6/                                                     
     .,plq/70000./
c
c
      logical noz,lusaf
c_______________________________________________________________________________
c
Cmp
Cmp
	tg=-99.
Cmp
Cmp
      noz=.true.
      list=3
	nfcst=13
	nbc=16

	write(6,*) 'executing cnsts....GRIBSOIL is: ', GRIBSOIL

	write(6,*) 'const believes there are ', nsoil, ' soil layers'

Cmp
Cmp	compute coac based on resolution!
Cmp	
Cmp	coac=.04*(.53846154/DPHD)**2.


C	removed squaring per t. black recommendation

	coac=.04*(.53846154/DPHD)

C	cap at 0.2
	coac=amin1(coac,0.2)

	write(6,*) 'computed coac based on grid resolution...'
	write(6,*) 'will use coac= ', coac
Cmp

c *** Derived vertical constants.
c
      do l=1,lmp1                             
         dfrlg(l)=dfl(l)                            
         dfl(l)=dfl(l)*g                            
      enddo
c
c *** Dummy constants.
c
      do l=1,lmp1                             
         eta1(l)=eta(l)                                                    
         eta2(l)=eta(l)                                                    
      enddo
      do l=1,lm                              
         deta1(l)=detac(l)                                                  
         aeta1(l)=aeta(l)                                                  
         deta2(l)=detac(l)                                                  
         aeta2(l)=aeta(l)                                                  
      enddo
c
c *** Derived geometrical constants.
c
      tph0=tph0d*dtr                                                    
      wb=wbd*dtr                                                        
      sb=sbd*dtr                                                        
      dlm=dlmd*dtr                                                      
      dph=dphd*dtr                                                      
      tdlm=dlm+dlm                                                      
      tdph=dph+dph                                                      
      rdlm=1./dlm                                                       
      rdph=1./dph                                                       
c                                                                       
      wbi=wb+tdlm                                                       
      sbi=sb+tdph                                                       
      ebi=wb+im2*tdlm                                                   
      anbi=sb+jm3*dph                                                   
c                                                                       
c     stph0=sin(tph0)                                                   
c     ctph0=cos(tph0)                                                   
c
c *** Time stepping related constants.
c
      tsph=3600./dt                                                     
      nddamp=tddamp*tsph+.5                                             
c                                                                       
      dtad=idtad                                                        
c
c *** Derived horizontal grid constants.
c
      dy=a*dph                                                          
      cpgfv=-dt/(48.*dy)                                                
      en= dt/( 4.*dy)*dtad                                              
      ent=dt/(16.*dy)*dtad                                              
c                                                                       
      tph=sb-dph                                                        
      do j=1,jm                                            
C      ihw(j)=-mod(j,2)
C      ihe(j)=ihw(j)+1
C      ivw(j)=-mod(j+1,2)
C      ive(j)=ivw(j)+1
c                                                                       
         tph=tph+dph                                               
         dxp=a*dlm*cos(tph)                                        
         dxj(j)=dxp                                                
cvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
csd      wpdarj(j)=-dt*w/(32.*dxp*dy)                      
csd      wpdarj(j)=-dt*w*100000./(32.*dxp*dy)                      
         wpdarj(j)=-w*((a*dlm*amin1(cos(anbi),cos(sbi)))**2+dy**2)
     .            /(dt*32.*dxp*dy)*.88
caaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
         cpgfuj(j)=-dt/(48.*dxp)                                   
         curvj(j)=.5*dt*tan(tph)/a                                 
         fcpj(j)=dt/(cp*192.*dxp*dy)                               
         fdivj(j)=1./(12.*dxp*dy)                                  
         emj(j)= dt/( 4.*dxp)*dtad                                 
         emtj(j)=dt/(16.*dxp)*dtad                                 
         fadj(j)=-dt/(48.*dxp*dy)*dtad                             
Cmp         acdt=coac*dt                                              
Cmp     .       *sqrt((a*dlm*amin1(cos(anbi),cos(sbi)))**2+dy**2)     
        ACDT=DT
     2            *SQRT((A*DLM*AMIN1(COS(ANBI),COS(SBI)))**2+DY**2)
Cmp         cddamp=codamp*acdt                                        
         cddamp=.04*codamp*acdt                                        
Cmp         hdacj(j)=acdt/(4.*dxp*dy)                                 
         hdacj(j)=coac*acdt/(4.*dxp*dy)                                 
         ddmpuj(j)=cddamp/dxp                                      
         ddmpvj(j)=cddamp/dy                                       
      enddo
c
c *** Spreading of upstream height-point advection factor.
c
      ja=0                                                              
      do j=3,5                                              
         ja=ja+1                                                       
      khla(ja)=2
      khha(ja)=im-1-mod(j+1,2)
         emt(ja)=emtj(j)                                               
      enddo

      do j=jm4,jm2                                          
         ja=ja+1                                                       
         khla(ja)=2                                 
         khha(ja)=im-1-mod(j+1,2)                                            
         emt(ja)=emtj(j)                                               
      enddo

      do j=6,jm5                                            
         ja=ja+1                                                       
	khla(ja)=2
        khha(ja)=2+mod(j,2)
        emt(ja)=emtj(j)                                               
      enddo

      do j=6,jm5                                            
         ja=ja+1                                                       
      khla(ja)=im-2
      khha(ja)=im-1-mod(j+1,2)
      emt(ja)=emtj(j)
      enddo
      print*,'ja=',ja,' jam=',jam
c
c *** Spreading of upstream velocity-point advection factor.
c
      ja=0                                                              
      do j=3,5                                              
         ja=ja+1                                                       
	 kvla(ja)=2
         kvha(ja)=im-1-mod(j,2)
         em(ja)=emj(j)                                                 
      enddo

      do j=jm4,jm2                                          
         ja=ja+1                                                       
         kvla(ja)=2     
         kvha(ja)=im-1-mod(j,2)                                           
         em(ja)=emj(j)                                                 
      enddo

      do j=6,jm5                                            
         ja=ja+1                                                       
         kvla(ja)=2                                          
         kvha(ja)=2+mod(j+1,2)                                              
         em(ja)=emj(j)                                                 
      enddo

      do j=6,jm5                                            
         ja=ja+1                                                       
         kvla(ja)=im-2
         kvha(ja)=im-1-mod(j,2)
         em(ja)=emj(j)                                                 
      enddo
c
c *** Coriolis parameter in tll system & related constants.
c
      tph=sb-dph                                                
      do j=1,jm                                            
c                                                                       
         tlm=wb-tdlm+mod(j,2)*dlm                                  
         tph=tph+dph                                               
         stph=sin(tph)                                             
         ctph=cos(tph)                                             
c                                                                       
         do i=1,im                                             
            tlm=tlm+tdlm                                                      
            fp=twom*(ctph0*stph+stph0*ctph*cos(tlm))                          
            f(i,j)=0.5*dt*fp                                                    
         enddo
      enddo
c
c *** Geographic lat and long of tll grid points.
c
      tph=sb-dph                                                
      do j=1,jm                                            
         tlm=wb-tdlm+mod(j+1,2)*dlm                                
         tph=tph+dph                                               
         stph=sin(tph)                                             
         ctph=cos(tph)                                             
c                                                                       
	do i=1,im
            tlm=tlm+tdlm                                                      
            sinphi=ctph0*stph+stph0*ctph*cos(tlm)                             
            glatr(i,j)=asin(sinphi)
            coslam=ctph*cos(tlm)/(cos(glatr(i,j))*ctph0)
     .            -tan(glatr(i,j))*tan(tph0) 
            coslam=min(coslam,1.)
            fact=1.                                                           
            if (tlm .gt. 0.0) fact=-1.
            glonr(i,j)=-tlm0d*dtr+fact*acos(coslam)                            
         enddo
      enddo
c
c *** Derived vertical grid constants.
c
      ef4t=.5*dt/cp                                                     
      f4q =   -dt*dtad                                                  
      f4d =-.5*dt*dtad                                                  
      do l=1,lm                          
         rdeta(l)=1./detac(l)                        
         f4q2(l)=-.25*dt*dtad/detac(l)               
      enddo
c
c *** Boundary masks.
c
      do j=1,jm                                          
	do i=1,im
         hbm2(i,j)=0.                                                        
         vbm2(i,j)=0.                                                        
         vbm3(i,j)=0.                                                        
	enddo
      enddo
	
      do j=3,jm2                                            
         do i=2,im-1-mod(j+1,2)                                      
            hbm2(i,j)=1.                                                        
         enddo
	 do i=2,im-1-mod(j,2)
            vbm2(i,j)=1.
         enddo
      enddo

      do j=4,jm3                                            
          do i=2+mod(j+1,2),im-2
            vbm3(i,j)=1.                                                        
          enddo
      enddo
c
c *** Topography masks & maximum vertical indices.
c
	do j=1,jm
         do i=1,im                                          
         lmh(i,j)=lm                                                         
         lmv(i,j)=lm                                                         
         enddo
	enddo
      do l=1,lm                              
      do j=1,jm                                          
	do i=1,im
         htm(i,j,l)=1.                                                       
         vtm(i,j,l)=1.                                                       
        enddo
      enddo
      enddo
c
c *** Topography masks & maximum vertical indices.
c
      if (.not. sigma) then                                            
c
      DO L=1,LM
       DO J=1,JM
        DO I=1,IM
         IF(LMH(I,J).EQ.LM.AND.ETA(L+1).GE.RES(I,J)) LMH(I,J)=L
        ENDDO
       ENDDO
      ENDDO

c                                                                       
         do l=1,lm                              
         do j=1,jm                                          
	 do i=1,im
            if (eta(l+1) .gt. res(i,j)) htm(i,j,l)=0.
              IF(I.EQ.33.AND.J.EQ.33) THEN
               DIFF=ETA(L+1)-RES(I,J)
               WRITE(6,28000)I,J,L,ETA(L+1),RES(I,J),DIFF
28000          FORMAT(1X,3I6,3(1X,E18.11))
              ENDIF
         enddo
         enddo
	 enddo
c                                                                       
      DO L=1,LM
       DO J=2,JM1
        IHE=MOD(J+1,2)
        IHW=IHE-1
        KHL=1+MOD(J,2)
        KHH=IM-1
        DO I=KHL,KHH
           IF(ETA(L+1).GT.RES(I,J)) THEN
                  VTM(I+IHE,J,L)=0.
                  VTM(I+IHW,J,L)=0.
                  VTM(I,J-1,L)=0.
                  VTM(I,J+1,L)=0.
           ENDIF
        ENDDO
       ENDDO
      ENDDO

c                                                                       

      DO L=1,LM
       DO J=2,JM1
       KVL=2-MOD(J,2)
         DO I=KVL,IM-1
           IF(LMV(I,J).EQ.LM.AND.VTM(I,J,L).LT.0.1) LMV(I,J)=L-1
         ENDDO
       ENDDO
      ENDDO

c
      endif
c

C next 80 lines or so from operational CNSTS.f
C--------------SPREADING OF LATITUDE DEPENDENT CONSTANTS----------------

      DO J=1,JM
        DO I=1,IM
c       KHH=IM-1+MOD(J,2)
c       DO I=1,KHH
          DX(I,J)=DXJ(J)
Cmp          WPDAR(I,J)=WPDARJ(J)*HBM2(I,J)
	if (.not. SIGMA) then
          WPDAR(I,J)=WPDARJ(J)*HBM2(I,J)
	else
	  if (I .eq. 1 .and. J .eq. 1) then
	 	write(6,*) 'REDUCING WPDAR BECAUSE RUNNING IN SIGMA!!!'
	  endif
	WPDAR(I,J)=WPDARJ(J)*HBM2(I,J)*0.5
	endif
          FCP(I,J)=FCPJ(J)
          FDIV(I,J)=FDIVJ(J)
          FAD(I,J)=FADJ(J)
          HDAC(I,J)=HDACJ(J)*1.25*HBM2(I,J)
c       ENDDO
c       KVH=IM-MOD(J,2)
c       DO I=1,KVH
          HDACV(I,J)=HDACJ(J)
          CPGFU(I,J)=CPGFUJ(J)
          CURV(I,J)=CURVJ(J)
        ENDDO
      ENDDO
C--------------INCREASING DIFFUSION ALONG THE BOUNDARIES----------------
      DO J=3,JM2
        IF (J.LE.5.OR.J.GE.JM4) THEN
          KHH=IM-2+MOD(J,2)
          DO I=2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
        ELSE
          KHH=2+MOD(J,2)
          DO I=2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
          KHH=IM-2+MOD(J,2)
          DO I=IM-2,KHH
           HDAC(I,J)=HDAC(I,J)* DFC
          ENDDO
        ENDIF
      ENDDO
CMEB          KHL2(J)=IM*(J-1)-(J-1)/2+2 ! 2 if j odd or j even
CMEB          KHL3(J)=IM*(J-1)-J/2+3     ! 3 if j odd, 2 if j even
CMEB          KHH2(J)=IM*J-J/2-1         ! IM-1 if j odd, IM-2 if j even
CMEB          KHH3(J)=IM*J-(J+1)/2-1     ! IM-2 if j odd, or even

C-----------------------------------------------------------------------
      DO J=1,JM
CMEB          KVL0(J)=IM*(J-1)-J/2+1  1 if j odd, 1 if J even
CMEB          KVH0(J)=IM*J-(J+1)/2    IM-1 if j odd, IM if J even
        KVH=IM-MOD(J,2)
        DO I=1,KVH
         DDMPU(I,J)=DDMPUJ(J)*VBM2(I,J)
         DDMPV(I,J)=DDMPVJ(J)*VBM2(I,J)
         HDACV(I,J)=HDACV(I,J)*VBM2(I,J)
        ENDDO
      ENDDO
C--------------INCREASING DIFFUSION ALONG THE BOUNDARIES----------------
      DO J=3,JM2
CMEB          KVL3(J)=IM*(J-1)-(J-1)/2+2 2 if j odd, 3 if j even
CMEB          KVL2(J)=IM*(J-1)-J/2+2     2 if j odd or even
CMEB          KVH2(J)=IM*J-(J+1)/2-1     IM-2 if j odd, IM-1 if j even
CMEB          KVH3(J)=IM*J-J/2-2         IM-2 if j odd or even
        IF (J.LE.5.OR.J.GE.JM4) THEN
          KVH=IM-1-MOD(J,2)
          DO I=2,KVH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
        ELSE
          KVH=3-MOD(J,2)
          DO I=2,KVH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
          KVH=IM-1-MOD(J,2)
          DO I=IM-2,KHH
            DDMPU(I,J)=DDMPU(I,J)*DDFC
            DDMPV(I,J)=DDMPV(I,J)*DDFC
            HDACV(I,J)=HDACV(I,J)* DFC
          ENDDO
        ENDIF
      ENDDO

c
c *** Surface parameters.
c
Cmp	added 25 Feb 99
Cmp
C
C--------------READ ALBEDO AND MOISTURE AVAILABILTY FILES---------------
C
C NOTE:  FOLLOWING ANNUAL MEAN GROUND SFC MOISTURE AVAIL NOT NEEDED
C        AFTER 31 JAN 96 JIF OF NEW SOIL/VEG/SNOW PHYSICS, BUT WE
C        RETAIN FOR TIME BEING TO ALLOW EASIER IMPACT STUDIES
C
      READ(21)ALBC1
      READ(22)ALBC2
      READ(23)ALBC3
      READ(24)ALBC4

C fIND jULIAN DAY OF YEAR TO DO THE TEMPORAL INTERPOLATION
      JULD = JULM(IDAT(1)) + IDAT(2)
      IF(JULD.LE.32) THEN
        S1 = 32 - JULD
        S2 = JULD + 30
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC1(I,J) + WGT2 * ALBC4(I,J)
        END DO
        END DO
      ELSE IF(JULD.LE.121.AND.JULD.GT.32) THEN
        S1 = 121 - JULD
        S2 = JULD - 32
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC2(I,J) + WGT2 * ALBC1(I,J)
        END DO
        END DO
      ELSE IF(JULD.LE.213.AND.JULD.GT.121) THEN
        S1 = 213 - JULD
        S2 = JULD - 121
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC3(I,J) + WGT2 * ALBC2(I,J)
        END DO
        END DO
      ELSE IF(JULD.LE.305.AND.JULD.GT.213) then
        S1 = 305 - JULD
        S2 = JULD - 213
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC4(I,J) + WGT2 * ALBC3(I,J)
        END DO
        END DO
      ELSE
        S1 = 365 - JULD + 32
        S2 = JULD - 305
        WGT1 = S2/(S1+S2)
        WGT2 = S1/(S1+S2)
        DO J = 1,180
        DO I = 1,361
        ALBC(I,J) = WGT1 * ALBC1(I,J) + WGT2 * ALBC4(I,J)
        END DO
        END DO
      END IF
C--------INTERPOLATE ALBEDO AND MOISTURE AVAILABILTY TO E GRID-------
          DO J=1,JM
          DO I=1,IM
      ELAT=90.+glatr(I,J)/DTR
      ELON=360.-glonr(I,J)/DTR
      IF(ELON.GT.360.)ELON=ELON-360.
      ILON1=INT(ELON)
      DIF=ELON-ILON1
      IF(DIF.GT.D5)ILON1=ILON1+1
      IF(ILON1.EQ.D00)ILON1=360
      ILON2=ILON1+1
      ILAT1=INT(ELAT)
      DIF=ELAT-ILAT1
      IF(DIF.GT.D5)ILAT1=MIN(ILAT1+1,179)
      ILAT2=ILAT1+1
      W1=ELON-ILON1+D5
      IF(W1.LT.D00)W1=W1+360.
      W2=ELAT-ILAT1+D5
      AR1=W1*W2
      AR2=W1*(H1-W2)
      AR3=(H1-W1)*(H1-W2)
      AR4=(H1-W1)*W2
      ALBEDO(I,J)=AR1*ALBC(ILON2,ILAT2)+AR2*ALBC(ILON2,ILAT1)+
     1 AR3*ALBC(ILON1,ILAT1)+AR4*ALBC(ILON1,ILAT2)
      ALBEDO(I,J)=ALBEDO(I,J)*D01
C      AVAILM(I,J)=AR1*AVLM(ILON2,ILAT2)+AR2*AVLM(ILON2,ILAT1)+
C     1 AR3*AVLM(ILON1,ILAT1)+AR4*AVLM(ILON1,ILAT2)
CVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV
C     AVAILM(I,J)=AVAILM(I,J)/.1125
CAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
C      IF(AVAILM(I,J).LT.1.E-8)  AVAILM(I,J)=1.E-8
      ENDDO
      ENDDO
Cmp    END ALBEDO SECTION **************************************************

      tph=sb-dph                                                
      print *,' '

        IF ( HIRES ) THEN
           print *, "CALLING HIRES SST ROUTINE"
           print *, " "
           CALL SSTHIRES(sst,sm,glatr,glonr,IDAT,6,DTR)
        ELSE
           print *, "CALLING LORES SST ROUTINE"
           print *, " "
           CALL SSTLORES(sst,sm,glatr,glonr,IDAT,6,DTR)
        ENDIF
        write(6,*) 'back from SST RES call'

	lusaf=.true.
	CALL SNOHIRES(si,sm,glatr,glonr,6,LUSAF,DTR)
	write(6,*) 'back from SNOHIRES call'

      do j=1,jm                                            
         tlm=wb-tdlm+mod(j+1,2)*dlm                                
         tph=tph+dph                                               
         stph=sin(tph)                                             
         ctph=cos(tph)                                             
	do i=1,im
            tlm=tlm+tdlm                                                      
            ctlm=cos(tlm)                                                     
            stlm=sin(tlm)                                                     
            aph=asin(stph0*ctph*ctlm+ctph0*stph) 
	    wet(i,j)=(sin(aph-30.*dtr))**2
            if (aph .le. 30.*dtr) wet(i,j)=0.
Cmp	operational line only used if dont have snowdepth data
        if (.NOT.lusaf.and.sm(i,j).lt.0.9) then
        write(6,*) 'NOT LUSAF!'
	si(i,j)=si(i,j)*sin(aph)
	endif

        enddo
      enddo


C------------------------------------------------
c

      DO J=1,JM
        DO I=1,IM


          SICE(I,J)=0.
          SNO (I,J)=0.
          IF(ALBEDO(I,J).GT.0.99999999)    ALBEDO(I,J)=0.99999999

        IF(SM(I,J).GT.0.9) THEN
C  SEA
          EPSR(I,J)=.97
          GFFC(I,J)=0.
          WET(I,J)=1.
          WFK(I,J)=1.
          ALBEDO(I,J)=.06

          IF(SI (I,J).GT.0.    ) THEN
C  SEA-ICE
            SM(I,J)=0.
            SICE(I,J)=1.
C             TG(I,J)=TI0
            GFFC(I,J)=ROI*CI*AKI/DZI
            ALBEDO(I,J)=.60
          END IF

        ELSE
C  LAND
          EPSR(I,J)=1.0
C           NEXT TWO LINES NOT NEEDED BUT KEPT FOR IMPACT STUDIES
          WFK(I,J)=.1125
C          WET(I,J)=AVAILM(I,J)*WFK(I,J)
          GFFC(I,J)=ROS*CS*AKS/DZG
          SICE(I,J)=0.

          IF(SI(I,J).GT.0.    ) THEN
C  SNOW      (change snowdepth in meters to water equiv in meters)
            SNO(I,J)=SI(I,J)*.20
C            WET(I,J)=WFK(I,J)
C      .....ALBEDO(I,J)=.65
C      (now set in model using snowdepth, capped at .55)
          END IF
        END IF
        ENDDO
      ENDDO
Cmp
C	convert lat/lon from rad to deg
	do J=1,JM
	do I=1,IM
	glon(I,J)=360.-glonr(I,J)*radfac
	glat(I,J)=glatr(I,J)*radfac
	if (glon(i,j) .ge. 360.) glon(I,J)=glon(I,J)-360.
	enddo
	enddo

	if (.NOT. GRIBSOIL) then
	CALL READSFC(glat,glon,sm,sice,smc,stc,tg)
	endif

      do j=1,jm
	do i=1,im
Cmp         sst(k)=sst(k)*sm(k)
         gffc(i,j)=gffc(i,j)*hbm2(i,j) 
         epsr(i,j)=epsr(i,j)*hbm2(i,j)
      enddo
	enddo
c
c *** Coarse look-up table for saturation point.
c
      kthm=jtb                                                          
      kpm=itb                                                           
      kthm1=kthm-1                                                      
      kpm1=kpm-1                                                        
c                                                                       
      thl=210.                                                          
      thh=350.                                                          
      pl=pt                                                             
      ph=105000.                                                        
      r1=r
      pt1=pt
c-----------------------------------------------------------------------
csd new convection
      call table(ptbl,ttbl,pt
     .          ,rdq,rdth,rdp,rdthe,pl,thl,qs0,sqs,sthe,the0)
      call tableq(ttblq,rdpq,rdtheq,plq,thl,stheq,the0q)
c-----------------------------------------------------------------------
      len=index(init_out//' ',' ')-1

      open(1,file=init_out(1:len)//'cnst.file'
     .    ,status='unknown',form='unformatted')
c
      write(1)                                                      
     . nfcst,nbc,list
     .,dt,idtad,sigma                                                   
     .,khla,khha,kvla,kvha,khl2,khh2,kvl2,kvh2
c
      write(1) lmh
      write(1) lmv
C        write(6,*) 'lmv values: '
        do J=JM,1,-2
C        write(6,633) (lmv(i,j),i=1,im,3)
        enddo
C        write(6,*) 'lmh values: '
        do J=JM,1,-2
C        write(6,633) (lmh(i,j),i=1,im,3)
        enddo
  633   format(30(I2,1x))

      write(1) hbm2 
      write(1) vbm2 
      write(1) vbm3 
      write(1) sm
      write(1) sice
      do l=1,lm
         write(1) ((htm(i,j,l),i=1,im),j=1,jm)
      enddo
      do l=1,lm
         write(1) ((vtm(i,j,l),i=1,im),j=1,jm)
      enddo
      write(1) dy,cpgfv,en,ent,r,pt,tddamp
     .        ,f4d,f4q,ef4t,detac,rdeta,aeta,f4q2,eta,dfl
     .        ,em,emt
c
      write(1) dx
      write(1) wpdar
      write(1) cpgfu
      write(1) curv
      write(1) fcp
      write(1) fdiv
      write(1) fad
      write(1) f
      write(1) ddmpu
      write(1) ddmpv
      pt2=pt
      write(1) pt2,glatr
      write(1) glonr
      write(1) plq,rdpq,rdtheq,stheq,the0q
      write(1) ros,cs,ds,roi,ci,di
     .        ,pl,thl,rdq,rdth,rdp,rdthe
     .        ,deta2,aeta2,dfrlg
     .        ,qs0,sqs,sthe,the0
      write(1) wfk
      write(1) epsr
Cmp
Cmp
	do J=1,jm
	do I=1,im
	tg(I,J)=stc(I,J,4)
	enddo
	enddo
Cmp
      write(1) tg
      write(1) gffc 
      write(1) sst
C
	write(6,*) 'sample SST values '
	do J=jm,1,-JM/30
	write(6,369) (sst(I,J),I=1,im,IM/12)
  369	format(31(f4.0,x))
	enddo
C
      write(1) albedo
      write(1) hdac
      write(1) hdacv
      write(1) ttblq
      write(1) ptbl,ttbl
     .        ,r1,pt1,tsph
     .        ,wbd,sbd,tlm0d,tph0d,dlmd,dphd,tlm0d,dp30
     .        ,x1p,y1p,ixm,iym
     .        ,deta1,aeta1,eta1

C NEW CONTENT *********************************************

C    TIME TO WRITE THE SOIL MODEL STUFF TO NHIBU (1)
C

C	if (I .eq. IM/2 .and. J .eq. JM/2) then 
	write(6,*) 'center hlat/hlon', glat(im/2,jm/2),
     +					glon(im/2,Jm/2)
C	endif
	
C **   FIRST THE VEGETATION TYPES ************************

       REWIND 30
       READ (30) I1D
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       IONETA=7
              CALL PUTEM(glat,glon,I1D,sm,si,IONETA)
	write(6,*) 'writing vegetation type'
       WRITE(1) IONETA

C	write(6,*) 'sample veg type values '
	do J=jm,1,-2
C	write(6,429) (IONETA(I,J),I=1,im,3)
  429	format(31(I2,x))
	enddo

C **   SECOND THE SOIL TYPES TYPES **********************

       REWIND 31
       READ (31) I1D
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       IONETA=2
              CALL PUTEM(glat,glon,I1D,sm,si,IONETA)
       DO J = 1,JM
         DO I = 1,IM
         IF(IONETA(I,J).EQ.13) IONETA(I,J) = 9
         END DO
       END DO
	write(6,*) 'writing soil type'
       WRITE(1) IONETA

C **   THIRD THE SURFACE SLOPE TYPES ***********************

       REWIND 32
       READ (32) I1D
C        SET DEFAULT FOR ETA LAND FAR FROM LAND IN GLOBAL
       IONETA=1
              CALL PUTEM(glat,glon,I1D,sm,si,IONETA)
	write(6,*) 'writing slope type'
       WRITE(1) IONETA

Cmp END NEW CONTENT ***************************************

	write(6,*) 'calling VFRAC'
	CALL VFRAC(im,jm,glatr,glonr,TLM0D,TPH0D,DLMD,DPHD,sm,si,
     +	vegfrc)

	write(6,*) 'sample VEGFRC values (* 1000) '
	do J=jm,1,-jm/30
	write(6,396) (vegfrc(I,J)*1000,I=1,im,im/12)
  396	format(31(f4.0,x))
	enddo

      write(1) vegfrc

	sldpth(1)=0.1
	sldpth(2)=0.3
	sldpth(3)=0.6
	sldpth(4)=1.0
	rtdpth(1)=0.1
	rtdpth(2)=0.3
	rtdpth(3)=0.6
	rtdpth(4)=0.0
Cmp

C	write(6,*) 'writing sldpth ... ', sldpth
      write(1) sldpth  
C	write(6,*) 'writing rtdpth... ', rtdpth
      write(1) rtdpth   
      close(1)
c
      return                                     
      end
c
c===============================================================================
c
      subroutine table(ptbl,ttbl,pt
     .                ,rdq,rdth,rdp,rdthe,pl,thl,qs0,sqs,sthe,the0)
c
c     implicit none
c
c *** Generate values for look-up tables used in convection.
c
      integer*4 itb,jtb
      parameter (itb=76,jtb=134)
      real*4 thh,ph,pq0,a1,a2,a3,a4,r,cp,eliwv,eps
Cmp      parameter (thh=350.,ph=105000.
C	changed 3/8/99 to make more like operation grdeta stuff
C	appeared to have no change on underflow problem
      parameter (thh=365.,ph=105000.
     .          ,pq0=379.90516
     .          ,a1=610.78,a2=17.2693882,a3=273.16,a4=35.86
     .          ,r=287.04,cp=1004.6,eliwv=2.683e6,eps=1.e-10)
c
      real*4 ptbl(itb,jtb),ttbl(jtb,itb),qsold (jtb),pold(jtb)
     .      ,qs0 (jtb),sqs   (jtb),qsnew(jtb)
     .      ,y2p (jtb),app   (jtb),aqp  (jtb),pnew(jtb)
     .      ,told(jtb),theold(jtb),the0 (itb),sthe(itb)
     .      ,y2t (jtb),thenew(jtb),apt  (jtb),aqt (jtb),tnew(jtb)
c_______________________________________________________________________________
c
c *** Coarse look-up table for saturation point.
c
      kthm=jtb
      kpm=itb
      kthm1=kthm-1
      kpm1=kpm-1
c
      pl=pt
c
      dth=(thh-thl)/real(kthm-1)
      dp =(ph -pl )/real(kpm -1)
c
      rdth=1./dth
      rdp=1./dp
      rdq=kpm-1
c
      th=thl-dth
c
      do kth=1,kthm
         th=th+dth
         p=pl-dp
         do kp=1,kpm
            p=p+dp
            ape=(100000./p)**(r/cp)
            qsold(kp)=pq0/p*exp(a2*(th-a3*ape)/(th-a4*ape))
            pold(kp)=p
         enddo
c
         qs0k=qsold(1)
         sqsk=qsold(kpm)-qsold(1)
         qsold(1  )=0.
         qsold(kpm)=1.
c
         do kp=2,kpm1
            qsold(kp)=(qsold(kp)-qs0k)/sqsk
c
c ********* Fix due to cyber half prec. limitation.
c
            if (qsold(kp)-qsold(kp-1) .lt. eps) 
     .         qsold(kp)=qsold(kp-1)+eps
c
c ********* End fix.
c
         enddo
c
         qs0(kth)=qs0k
         sqs(kth)=sqsk
         qsnew(1  )=0.
         qsnew(kpm)=1.
         dqs=1./real(kpm-1)
c
         do kp=2,kpm1
            qsnew(kp)=qsnew(kp-1)+dqs
         enddo
c
         y2p(1   )=0.
         y2p(kpm )=0.
c
         call spline(jtb,kpm,qsold,pold,y2p,kpm,qsnew,pnew,app,aqp)
c
         do kp=1,kpm
            ptbl(kp,kth)=pnew(kp)
         enddo
c
      enddo
c
c *** Coarse look-up table for t(p) from constant the.
c
      p=pl-dp
      do kp=1,kpm
         p=p+dp
         th=thl-dth
         do kth=1,kthm
            th=th+dth
            ape=(100000./p)**(r/cp)
            qs=pq0/p*exp(a2*(th-a3*ape)/(th-a4*ape))
            told(kth)=th/ape
            theold(kth)=th*exp(eliwv*qs/(cp*told(kth)))
         enddo
c
         the0k=theold(1)
         sthek=theold(kthm)-theold(1)
         theold(1   )=0.
         theold(kthm)=1.
c
         do kth=2,kthm1
            theold(kth)=(theold(kth)-the0k)/sthek
c
            if (theold(kth)-theold(kth-1) .lt. eps)
     .         theold(kth)=theold(kth-1)+eps
c
         enddo
c
         the0(kp)=the0k
         sthe(kp)=sthek
         thenew(1  )=0.
         thenew(kthm)=1.
         dthe=1./real(kthm-1)
         rdthe=1./dthe
c
         do kth=2,kthm1
            thenew(kth)=thenew(kth-1)+dthe
         enddo
c
         y2t(1   )=0.
         y2t(kthm)=0.
c
         call spline(jtb,kthm,theold,told,y2t,kthm,thenew,tnew,apt,aqt)
c
         do kth=1,kthm
            ttbl(kth,kp)=tnew(kth)
         enddo
c
      enddo
c
      return
      end
c
c===============================================================================
c
      subroutine tableq(ttblq,rdp,rdthe,pl,thl,sthe,the0)
c
c *** Generate values for finer look-up tables used in convection.
c
      parameter (itb=152,jtb=440)
      parameter (thh=325.,ph=105000.
     .          ,pq0=379.90516
     .          ,a1=610.78,a2=17.2693882,a3=273.16,a4=35.86
     .          ,r=287.04,cp=1004.6,eliwv=2.683e6,eps=1.e-9)
Cmp	change eps from 1.e-10 to 1.e-9 (make more like operational)
c
      real*4 ttblq(jtb,itb)
     .      ,told (jtb),theold(jtb),the0(itb),sthe(itb)
     .      ,y2t  (jtb),thenew(jtb),apt (jtb),aqt (jtb),tnew(jtb)
c_______________________________________________________________________________
c
c *** Coarse look-up table for saturation point.
c
      kthm=jtb
      kpm=itb
      kthm1=kthm-1
      kpm1=kpm-1
c
      dth=(thh-thl)/real(kthm-1)
      dp =(ph -pl )/real(kpm -1)
c
      rdp=1./dp
      th=thl-dth
c
c *** Coarse look-up table for t(p) from constant the.
c
      p=pl-dp
      do kp=1,kpm
         p=p+dp
         th=thl-dth
         do kth=1,kthm
            th=th+dth
            ape=(100000./p)**(r/cp)
            qs=pq0/p*exp(a2*(th-a3*ape)/(th-a4*ape))
            told(kth)=th/ape
            theold(kth)=th*exp(eliwv*qs/(cp*told(kth)))
         enddo
c
         the0k=theold(1)
         sthek=theold(kthm)-theold(1)
         theold(1   )=0.
         theold(kthm)=1.
c
         do kth=2,kthm1
            theold(kth)=(theold(kth)-the0k)/sthek
            if (theold(kth)-theold(kth-1) .lt. eps)
     .         theold(kth)=theold(kth-1)+eps
c
         enddo
c
         the0(kp)=the0k
         sthe(kp)=sthek
c
         thenew(1  )=0.
         thenew(kthm)=1.
         dthe=1./real(kthm-1)
         rdthe=1./dthe
c
         do kth=2,kthm1
            thenew(kth)=thenew(kth-1)+dthe
         enddo
c
         y2t(1   )=0.
         y2t(kthm)=0.
c
         call spline(jtb,kthm,theold,told,y2t,kthm,thenew,tnew,apt,aqt)
c
         do kth=1,kthm
            ttblq(kth,kp)=tnew(kth)
         enddo
c
      enddo
c
      return
      end
c
c===============================================================================
c
      subroutine spline(jtb,nold,xold,yold,y2,nnew,xnew,ynew,p,q)           
c
c *** This is a one-dimensional cubic spline fitting routine 
c        programed for a small scalar machine.                       
c
c *** Programer: Z. Janjic, Yugoslav Fed. Hydromet. Inst., Beograd 
c
c *** nold - number of given values of the function.  must be ge 3. 
c     xold - locations of the points at which the values of the     
c            function are given.  must be in ascending order.       
c     yold - the given values of the function at the points xold.   
c     y2   - the second derivatives at the points xold.  if natural 
c            spline is fitted y2(1)=0. and y2(nold)=0. must be      
c            specified.                                             
c     nnew - number of values of the function to be calculated.     
c     xnew - locations of the points at which the values of the     
c            function are calculated.  xnew(k) must be ge xold(1)   
c            and le xold(nold).                                     
c     ynew - the values of the function to be calculated.           
c     p, q - auxiliary vectors of the length nold-2.                
c                                                                   
      real*4 xold(jtb),yold(jtb),y2(jtb),p(jtb),q(jtb)                        
     .      ,xnew(jtb),ynew(jtb)                                              
c_______________________________________________________________________________
c
      noldm1=nold-1                                                     
c                                                                       
      dxl=xold(2)-xold(1)                                               
      dxr=xold(3)-xold(2)                                               
      dydxl=(yold(2)-yold(1))/dxl                                       
      dydxr=(yold(3)-yold(2))/dxr                                       
      rtdxc=.5/(dxl+dxr)                                                
c                                                                       
      p(1)= rtdxc*(6.*(dydxr-dydxl)-dxl*y2(1))                          
      q(1)=-rtdxc*dxr                                                   
c                                                                       
      if(nold.eq.3) go to 700                                           
c-----------------------------------------------------------------------
      k=3                                                               
c                                                                       
 100  dxl=dxr                                                           
      dydxl=dydxr                                                       
      dxr=xold(k+1)-xold(k)                                             
      dydxr=(yold(k+1)-yold(k))/dxr                                     
      dxc=dxl+dxr                                                       
      den=1./(dxl*q(k-2)+dxc+dxc)                                       
c                                                                       
      p(k-1)= den*(6.*(dydxr-dydxl)-dxl*p(k-2))                         
      q(k-1)=-den*dxr                                                   
c                                                                       
      k=k+1                                                             
      if(k.lt.nold) go to 100                                           
c-----------------------------------------------------------------------
 700  k=noldm1                                                          
c                                                                       
 200  y2(k)=p(k-1)+q(k-1)*y2(k+1)                                       
c                                                                       
      k=k-1                                                             
      if(k.gt.1) go to 200                                              
c-----------------------------------------------------------------------
      k1=1                                                              
c                                                                       
 300  xk=xnew(k1)                                                       
c                                                                       
      do 400 k2=2,nold                                                  
      if(xold(k2).le.xk) go to 400                                      
      kold=k2-1                                                         
      go to 450                                                         
 400  continue                                                          
      ynew(k1)=yold(nold)                                               
      go to 600                                                         
c                                                                       
 450  if(k1.eq.1)   go to 500                                           
      if(k.eq.kold) go to 550                                           
c                                                                       
 500  k=kold                                                            
c                                                                       
      y2k=y2(k)                                                         
      y2kp1=y2(k+1)                                                     
      dx=xold(k+1)-xold(k)                                              
      rdx=1./dx                                                         
c                                                                       
      ak=.1666667*rdx*(y2kp1-y2k)                                       
      bk=.5*y2k                                                         
      ck=rdx*(yold(k+1)-yold(k))-.1666667*dx*(y2kp1+y2k+y2k)            
c                                                                       
 550  x=xk-xold(k)                                                      
      xsq=x*x                                                           
c                                                                       
      ynew(k1)=ak*xsq*x+bk*xsq+ck*x+yold(k)                             
c                                                                       
 600  k1=k1+1                                                           
      if(k1.le.nnew) go to 300                                          
c
      return                                    
      end                                       
c
c===============================================================================
c
      subroutine albedo_vege(ivegetype,albedo)
c
c *** ssib vegetation types (dorman and sellers, 1989; jam)
c
c      1:   broadleaf-evergreen trees  (tropical forest)
c      2:   broadleaf-deciduous tress
c      3:   broadleaf and needleleaf tress (mixed forest)
c      4:   needleleaf-evergreen trees
c      5:   needleleaf-deciduous tress (larch)
c      6:   broadleaf tress with groundcover (savanna)
c      7:   groundcover only (perennial)
c      8:   broadleaf shrubs with perennial groundcover
c      9:   broadleaf shrubs with bare soil
c     10:   dwarf trees and shrubs with groundcover (tundra)
c     11:   bare soil
c     12:   cultivations (the same parameters for the type 7)
c     13:   glacial
c     14:   water, according to eta sm
c
      dimension albedo_veg(14)
c
      data albedo_veg/0.11, 0.19, 0.16, 0.13, 0.19, 0.19, 0.19
     .               ,0.29, 0.29, 0.14, 0.15, 0.19, 0.15, 0.10/
c_______________________________________________________________________________
c
      albedo=albedo_veg(ivegetype)
      return
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C	BLOCK DATA GSOIL

C	logical GRIBSOIL
C	DATA GRIBSOIL /.FALSE./

C	END


