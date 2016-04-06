      program degrib2model_driver
c
      implicit none
c
      integer nx,ny,nz,kpds(200),datsav(9),n         !Degrib grid dimensions
c
      character(LEN=255):: gribfile,outdir
c
      real esat,es
c
      common /estab/esat(15000:45000),es(15000:45000)
c_______________________________________________________________________________
c
c *** Initialize tables.
c
      call es_ini
c
c *** Read file names.
c
C      read(5,'(a)') degrib_dir
C      read(5,'(a)') degrib_file
      read(5,'(a)') gribfile
      read(5,'(a)') outdir
c

	n=index(gribfile,' ')-1
	CALL GET_GDS(gribfile(1:n),datsav,kpds)
      nx=datsav(1)
      ny=datsav(2)
      nz=26
c
Cold      call degrib2model(degrib_dir,degrib_file,outdir,nx,ny,nz)
      call degrib2model(gribfile,outdir,nx,ny,nz)

c
      end
c
c===============================================================================
c
Cold      subroutine degrib2model(degrib_dir,degrib_file,outdir,nx,ny,nz)
      subroutine degrib2model(gribfile,outdir,nx,ny,nz)
c
      implicit none
c
      real    cp,kappa,dlat,dlon,sum,avg
      parameter(cp=1004.,kappa=287./1004.)
c
      integer nx,ny,nz,i,j,k,l,n,it,ip,jp,nsfcfld
     .         ,iyear,imonth,iday,ijulday,julday_laps,ifcsthr
     .	       ,count,kgds(200)
c
      real ht(nx,ny,nz)              !Isobaric heights (m)
     .      ,tp(nx,ny,nz)              !Isobaric temps (K)
     .      ,th(nx,ny,nz)              !Isobaric theta (K)
     .      ,uw(nx,ny,nz)              !Isobaric u-wind (m/s)
     .      ,vw(nx,ny,nz)              !Isobaric v-wind (m/s)
     .      ,rh(nx,ny,nz)              !Isobaric rh,mr (%,kg/kg)
     .      ,pr(nz),pri(nz)            !Isobaric pressures (mb)
     .      ,lat1,lat2,lon0,sw(2),ne(2)
     .      ,xe,mrsat,esat,es
     .	    ,avnlevs(26)
     .	    ,slp(nx,ny,12) 	  !slp(i,j,1)=EMSL;slp(i,j,2)=PMSL
     .      ,temp(nx,ny)
				  !slp(i,j,3)=PSFC;slp(i,j,4)=ZSFC
				  !slp(i,j,5 & 6) are 0-10 cm STC and SMC
				  !slp(i,j,7 & 8) are 10-200 cm STC and SMC
c
      character(LEN=255):: gribfile,outdir,outfile,gdsfile
      character(LEN=2)::   gproj
      character(LEN=11)::   atime
      character(LEN=7)::   model

       data AVNLEVS/1000,975,950,925,900,850,800,750,700,650,
     +		     600,550,500,450,400,350,300,250,200,150,
     +		     100, 70, 50, 30, 20, 10/
c
      common /estab/esat(15000:45000),es(15000:45000)
c_______________________________________________________________________________
c
c *** Fill pressure levels.
c
Cmp      nsfcfld will be 12 although 4 of these are set to missing for AVN...
      nsfcfld=12
Cmp
	write(6,*) 'using hardwired stuff! ', nx,ny,nz
      do k=1,nz
Cmp         pr(k)=1025.-float(k*25)
	pr(k)=AVNLEVS(k)
C		write(6,*) 'pressure value of ', pr(k)
	      enddo
c 
c *** Read in degrib data.
c
      n=index(gribfile,' ')-1
	write(6,*) 'calling read_degrib'
      call read_degrib(gribfile(1:n)
     .                ,nx*ny,nz,pr,ht,tp,rh,uw,vw,slp,atime)
	write(6,*) 'return read_degrib'
c
c *** Check for any missing data.
c
      do k=1,nz
	  if (ht(1,1,k) .eq. -99999.) then
	     print *,'Height data missing at level: ',pr(k)
	     stop
	  elseif (tp(1,1,k) .eq. -99999.) then
	     print *,'Temperature data missing at level: ',pr(k)
	     stop
	  elseif (rh(1,1,k) .eq. -99999.) then
	     print *,'RH data missing at level: ',pr(k)
	     print *,'Calling RH patch.'
	     call rh_fix(nx,ny,nz,rh,pr)
	  elseif (uw(1,1,k) .eq. -99999.) then
	     print *,'U-wind data missing at level: ',pr(k)
	     stop
	  elseif (vw(1,1,k) .eq. -99999.) then
	     print *,'V-wind data missing at level: ',pr(k)
	     stop
	  endif
      enddo

Cmp
Cmp     Handle missing surface data....
Cmp
	write(6,*) 'going to use nsfcfld= ', nsfcfld
	do k=5,nsfcfld

Cmp	if (slp(1,1,k) .eq. 0.0) then
	if (slp(1,1,k) .eq. -99999.) then
	write(6,*) 'SURFACE DATA MISSING... ', K       
	if (k.eq.9.or.k.eq.11.) then
	 write(6,*) 'filling soil temp data...'
	 do j=1,ny
	 do i=1,nx
	   slp(i,j,k)=slp(i,j,7)
	 enddo
	 enddo

      elseif(k.eq.10.or.k.eq.12.) then

	write(6,*) 'filling soil moisture data...'

	do j=1,ny
	do i=1,nx
	 slp(i,j,k)=slp(i,j,8)
	enddo
	enddo
      endif
      endif
Cmp
	if (mod(k,2) .eq. 0) then
	do j=1,ny
	do i=1,nx
	if (slp(i,j,k) .eq. 0) then
	slp(i,j,k)=0.14
	endif
	enddo
	enddo

	else

	do j=1,ny
        do i=1,nx
        if (slp(i,j,k) .eq. 0) then
        slp(i,j,k)=273.15
        endif
        enddo
        enddo

	endif
Cmp
	enddo

c
c *** Convert 3d temp to theta.
c *** Compute Exner function.
c *** Convert 3d rh to mr.
c
      do k=1,nz
	 pri(k)=1./pr(k)
      enddo
c
      do k=1,nz
      do j=1,ny
      do i=1,nx
	 th(i,j,k)=tp(i,j,k)*(1000.*pri(k))**kappa
C	 ex(i,j,k)=cp*tp(i,j,k)/th(i,j,k)
	 it=tp(i,j,k)*100
	 it=min(45000,max(15000,it))
	 xe=esat(it)
	 mrsat=0.00622*xe/(pr(k)-xe)
	 rh(i,j,k)=rh(i,j,k)*mrsat
      enddo
      enddo
      enddo


Cmp
Cmp for some reason AVN data appears to be "upside down" in the N-S
Cmp sense.  Flip the arrays of data.
Cmp

      do k=1,nz

      do j=1,ny
      do i=1,nx
	temp(i,j)=th(i,j,k)
      enddo
      enddo

      do J=1,ny
      do i=1,nx
	th(i,j,k)=temp(i,ny-J+1)
      enddo
      enddo

C======================================

	      do j=1,ny
	      do i=1,nx
		temp(i,j)=uw(i,j,k)
	      enddo
	      enddo

	      do J=1,ny
	      do i=1,nx
		uw(i,j,k)=temp(i,ny-J+1)
	      enddo
	      enddo

C======================================

	      do j=1,ny
	      do i=1,nx
		temp(i,j)=vw(i,j,k)
	      enddo
	      enddo

	      do J=1,ny
	      do i=1,nx
		vw(i,j,k)=temp(i,ny-J+1)
	      enddo
	      enddo

C======================================

	      do j=1,ny
	      do i=1,nx
		temp(i,j)=ht(i,j,k)
	      enddo
	      enddo

		if (k.eq.1) write(6,*) 'flipping isobaric data - '
	      do J=1,ny
	      do i=1,nx
		ht(i,j,k)=temp(i,ny-J+1)
	      enddo
	      enddo

C======================================

	      do j=1,ny
	      do i=1,nx
		temp(i,j)=rh(i,j,k)
	      enddo
	      enddo

	      do J=1,ny
	      do i=1,nx
		rh(i,j,k)=temp(i,ny-J+1)
	      enddo
	      enddo

C======================================

	      do j=1,ny
	      do i=1,nx
C		temp(i,j)=ex(i,j,k)
	      enddo
	      enddo

	      do J=1,ny
	      do i=1,nx
C		ex(i,j,k)=temp(i,ny-J+1)
	      enddo
	      enddo
C-----------------------------------------
      enddo

Cmp	generalize eventually!
	do k=1,nsfcfld

C	write(6,*) 'flipping surface fields... ' 
	
      do j=1,ny
      do i=1,nx
	temp(i,j)=slp(i,j,k)
      enddo
      enddo

      do J=1,ny
      do i=1,nx
	slp(i,j,k)=temp(i,ny-J+1)
      enddo
      enddo

	enddo
c
c
c *** Create output file name.
c
      n=index(outdir,' ')-1

Cmp	make ETA_avn to differentiate from files based on eta grib input
      model='ETA_avn'
      outfile=outdir(1:n)//'/'//atime//'.'//model
      n=index(outfile,' ')-1
      print *,model,' data ---> ',outfile(1:n)
      open(1,file=outfile(1:n),status='unknown',form='unformatted')
c
c *** Write header stuff.

c
      ip=1
      jp=1
Cmp      nsfcfld will be 12 although 4 of these are set to missing for AVN...
      nsfcfld=12
      gproj='LL'
      write(1) nx,ny,nz,nx,ny,ip,jp,nsfcfld,gproj
      lat1=-90.0
      lat2=0.0
      lon0=0.0
      sw(1)=-90
      sw(2)=0.
      ne(1)=90.
      ne(2)=359.
	dlat=1.
	dlon=1.
Cmp      write(1) nx,ny,nz,lat1,lat2,lon0,sw,ne
	write(1) nx,ny,nz,lat1,lon0,dlat,dlon

C new stuff for GDS file
C
        n=index(gribfile,' ')-1
        write(6,*) 'calling get_fullgds with ',
     +    gribfile(1:n)
        call get_fullgds(gribfile(1:n),nx,ny,kgds)
        write(6,*) 'back from get_fullgds'


        n=index(outdir,' ')-1
        gdsfile=outdir(1:n)//'/'//'gdsinfo.'//model

        n=index(gdsfile,' ')-1
        write(6,*) 'GDS written to ', gdsfile(1:n)
        open(unit=14,file=gdsfile(1:n),form='unformatted',
     +  access='sequential')

C
C	MODIFY GDS TO REFLECT FLIPPING OF DATA IN N-S SENSE
C

	kgds(4)= -kgds(4)
	kgds(7)= -kgds(7)
        write(6,*) 'writing kgds ', (kgds(I),i=1,14)
        write(14) kgds

        close(14)
C
C end new stuff

c
c *** Write isobaric upper air data.
c
      write(1) uw
      write(1) vw
C      write(1) th
      write(1) ht
      write(1) rh
C      write(1) ex
	write(1) pr

	write(6,*) 'ht(25,25,20) ', ht(25,25,20)
	write(6,*) 'pr(20) ', pr(20)
      write(1) slp
Cmp	
	write(6,*) 'writing these sfc values to the ETA_avn file...'
	write(6,*) 'aproximately 100W,40N'
	do k=1,nsfcfld
	write(6,*) 'field, value ', k, slp(260,130,k)
	enddo
	write(6,*) 'approximately 40W,40N'
	do k=1,nsfcfld
	write(6,*) 'field, value ', k, slp(320,130,k)
	enddo

c
      close(1)
c
      return
      end
c
c===============================================================================
c
      subroutine read_degrib(etafile,nxny,nz,pr
     .                      ,ht,tp,rh,uw,vw,slp,atime)
c
      implicit none
c
      integer nxny,nz,i
c
      real pr(nz),ht(nxny,nz),tp(nxny,nz)
     .      ,rh(nxny,nz),uw(nxny,nz),vw(nxny,nz)
     .      ,dummy,slp(nxny,12)
c
      integer kgds(200),kpds(50),len,kerr
     .         ,lenpds,lenkgds,nwords,kpdsl
     .         ,ijulday,julday_laps,j,k,datsav(9)
     .	       ,IRETO,IRET1,JPDS(200),JGDS(200),KNUM

	logical BITMAP(nxny)
c
      character*(*) etafile
      character(LEN=1)::  pds(50)
      character(LEN=11)::   atime
c_______________________________________________________________________________
c
      len=index(etafile//' ',' ')-1
c
c
c *** Fill time stamp (assume all records are for same time).
c

	call get_gds(etafile(1:len),datsav,kpds)
	write(6,*) 'back from get_gds'

Cmp      ijulday=julday_laps(kpds(10),kpds(9),kpds(8))
	if (kpds(8) .eq. 100) kpds(8)=0
      write(atime,'(i2.2,i2.2,i2.2,i2.2,i3.3)') 
     .   kpds(8),kpds(9),kpds(10),kpds(11),kpds(14)
c
c *** Fill a missing value flag into first space of each variable.
c
      do k=1,nz
	 ht(1,k)=-99999.
 	 tp(1,k)=-99999.
	 rh(1,k)=-99999.
	 uw(1,k)=-99999.
	 vw(1,k)=-99999.
      enddo
	write(6,*) 'here1'

Cmp initialize surface fields to -99999.  so the interp code can handle
Cmp     appropriately

	write(6,*) 'nwords, nxny: ', nwords, nxny
	do k=1,12
	do j=1,datsav(1)*datsav(2)
	slp(j,k)=-99999.
	enddo
	enddo

Cmp

c
c *** Now put the data into the corresponding arrays.
c

	write(6,*) 'here2'
	call baopen(11,etafile,IRETO)
	write(6,*) 'here3'
	if (IRETO .ne. 0) write(6,*) 'BAOPEN TROUBLE!!!! ', IRETO

	jpds=-1
	jgds=-1

	jpds(5)=81
	jpds(6)=1
	jpds(7)=0

	call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,1),IRET1)

	write(6,*) 'first GETGB, IRET1= ', IRET1
	write(6,*) 'LAND/SEA READ!!!!! '
	write(6,*) (slp(j,1),j=nwords/2,nwords/2+5)

	jpds(5)=2
	jpds(6)=102
	jpds(7)=0

       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,2),IRET1)

	write(6,*) 'PMSL READ!!!!! '
	write(6,*) (slp(j,2),j=nwords/2,nwords/2+8)

	jpds(5)=1
	jpds(6)=1
	jpds(7)=0

       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,3),IRET1)

	write(6,*) 'PSFC READ!!!!! '
	write(6,*) (slp(j,3),j=nwords/2,nwords/2+8)

	jpds(5)=7
	jpds(6)=1
	jpds(7)=0

       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,4),IRET1)

	write(6,*) 'ZSFC READ!!!!! '
	write(6,*) (slp(j,4),j=nwords/2,nwords/2+8)

	
Cmp     SOIL FIELDS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C

C	AVN stores soil T in 11, ETA in 85
	jpds(5)=11
	jpds(6)=112
	jpds(7)=10


       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,5),IRET1)

	if (IRET1.eq.0) then
	 write(6,*) 'found soil temp at ',jpds(7)
	write(6,*) (slp(j,5),j=nwords/4,nwords/4+8)
	write(6,*) (slp(j,5),j=nwords/2,nwords/2+8)
	endif

	jpds(7)=2600
       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,7),IRET1)
	if (IRET1.eq.0)  then
	write(6,*) 'found soil temp at ',jpds(7)
	write(6,*) (slp(j,7),j=nwords/2,nwords/2+8)
	write(6,*) (slp(j,7),j=nwords/2,nwords/2+8)
	endif

	jpds(7)=10340
	       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,9),IRET1)
	if (IRET1.eq.0)  then
	write(6,*) 'found soil temp at ',jpds(7)
	write(6,*) (slp(j,9),j=nwords/2,nwords/2+5)
	endif

	jpds(7)=25800
       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,11),IRET1)
	if (IRET1.eq.0) then
	write(6,*) 'found soil temp at ',jpds(7)
	write(6,*) (slp(j,11),j=nwords/4,nwords/4+5)
	endif


C	10-200 layer

	jpds(7)=2760
	call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,7),IRET1)
	if (IRET1.eq.0) then
	write(6,*) 'found soil temp at ',jpds(7)
	write(6,*) (slp(j,7),j=nwords/4,nwords/4+8)
	endif

		
		
C       MOISTURE

	jpds(5)=144
	jpds(7)=10

       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,6),IRET1)
	if (IRET1.eq.0) then
	 write(6,*) 'found soil wet at ',jpds(7)
        write(6,*) (slp(j,6),j=nwords/4,nwords/4+8)
	endif

        jpds(7)=2600
       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,8),IRET1)
        if (IRET1.eq.0) then
	 write(6,*) 'found soil wet at ',jpds(7)
        write(6,*) (slp(j,8),j=nwords/2,nwords/2+8)
	endif

        jpds(7)=10340
       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,10),IRET1)
        if (IRET1.eq.0) then
	 write(6,*) 'found soil wet at ',jpds(7)
        write(6,*) (slp(j,10),j=nwords/2,nwords/2+5)
	endif

        jpds(7)=25800
       call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,12),IRET1)
        if (IRET1.eq.0) then
	 write(6,*) 'found soil wet at ',jpds(7)
        write(6,*) (slp(j,12),j=nwords/2,nwords/2+5)
	endif

C       10-200 layer

        jpds(7)=2760
        call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,slp(1,8),IRET1)
        if (IRET1.eq.0) then
	 write(6,*) 'found soil wet at ',jpds(7)
        write(6,*) (slp(j,8),j=nwords/4,nwords/4+5)
	endif


	
C 	ISOBARIC DATA

        jpds(6)=100

        do k=1,nz
        jpds(7)=nint(pr(k))

        jpds(5)=7
      call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,ht(1,k),IRET1)
        if (IRET1 .ne. 0) write(6,*) ' AT LEVEL ', jpds(7) , jpds(5)

        jpds(5)=11
      call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,tp(1,k),IRET1)
        if (IRET1 .ne. 0) write(6,*) ' AT LEVEL ', jpds(7) , jpds(5)

        jpds(5)=52
      call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,rh(1,k),IRET1)
        if (IRET1 .ne. 0) write(6,*) ' AT LEVEL ', jpds(7) , jpds(5)

        jpds(5)=33
      call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,uw(1,k),IRET1)
        if (IRET1 .ne. 0) write(6,*) ' AT LEVEL ', jpds(7) , jpds(5)

        jpds(5)=34
      call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,vw(1,k),IRET1)
        if (IRET1 .ne. 0) write(6,*) ' AT LEVEL ', jpds(7) , jpds(5)

        write(6,*) 'Z,T,Q,U,V ', ht(nxny/2,k),tp(nxny/2,k),
     +  rh(nxny/2,k),uw(nxny/2,k),vw(nxny/2,k)

       enddo
c
c *** Normal finish.
c
1000  continue
      return
c
c *** Premature end of file.
c
1100  continue
      print *,'Premature end of file.'
      print *,'Abort...'
      stop
c
      end
c
c===============================================================================
c
      subroutine rh_fix(nx,ny,nz,rh,pr)
c
      implicit none
c
      integer nx,ny,nz,i,j,k,kk
c
      real rh(nx,ny,nz),pr(nz)
c_______________________________________________________________________________
c
c *** Fix bottom levels if necessary.
c
      if (rh(1,1,1) .eq. -99999.) then
	 do k=2,nz
	    if (rh(1,1,k) .ne. -99999.) then
	       do kk=k-1,1,-1
	       print *,'Copying',nint(pr(kk+1)),' mb to'
     .                , nint(pr(kk)),' mb.'
	       do j=1,ny
	       do i=1,nx
		  rh(i,j,kk)=rh(i,j,kk+1)
               enddo
               enddo
               enddo
	       goto 10
            endif
         enddo
	 print *,'RH patch did not work.'
	 stop
      endif
c
c *** Fix upper levels if necessary.
c
10    continue
      if (rh(1,1,nz) .eq. -99999.) then
	 do k=nz-1,1,-1
	    if (rh(1,1,k) .ne. -99999.) then
	       do kk=k+1,nz
	       print *,'Copying',nint(pr(kk-1)),' mb to'
     .                , nint(pr(kk)),' mb.'
	       do j=1,ny
	       do i=1,nx
		  rh(i,j,kk)=rh(i,j,kk-1)
               enddo
               enddo
               enddo
	       goto 20
            endif
         enddo      
      endif
c
20    continue
      do k=1,nz
	 if (rh(1,1,k) .eq. -99999.) then
	    print *,'RH patch did not work.'
	    stop
	 endif
      enddo
c
      return
      end
c
c===============================================================================
c
      subroutine es_ini
c
      common /estab/esat(15000:45000),es(15000:45000)
c
c *** Create tables of the saturation vapour pressure with up to
c        two decimal figures of accuraccy:
c
      do it=15000,45000
         t=it*0.01
         p1 = 11.344-0.0303998*t
         p2 = 3.49149-1302.8844/t
         c1 = 23.832241-5.02808*alog10(t)
         esat(it) = 10.**(c1-1.3816E-7*10.**p1+
     .               8.1328E-3*10.**p2-2949.076/t)
         es(it) = 610.78*exp(17.269*(t-273.16)/(t-35.86))
      enddo
c
      return
      end
c
c===============================================================================
c
      function julday_laps(day,month,year)
c
      implicit	none
c      
      integer	julday_laps
     .         ,day,month,year
     .         ,ndays(12),i
c
      data ndays/31,28,31,30,31,30,31,31,30,31,30,31/
c_______________________________________________________________________________
c
      julday_laps=0
      do i=1,month-1
         julday_laps=julday_laps+ndays(i)
      enddo
      julday_laps=julday_laps+day
      if (mod(year,4) .eq. 0 .and. month .gt. 2) 
     .   julday_laps=julday_laps+1
      return
c      
      end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

       subroutine get_gds(etafile,gdsinfo,kpds)

        character*(*) etafile
        character(LEN=1):: pds(50)

Ctst        integer*4 kgds(200),kpds(50),len,kerr
        integer kgds(200),kpds(200),len,kerr
     .         ,lenpds,lenkgds,nwords,kpdsl
     .         ,ijulday,julday_laps,j,k,gdsinfo(9)
     .         ,gdsav,IRETO,JGDS(200),JPDS(200)
	real tmp(70000)

	LOGICAL BITMAP(70000)

        nxny=360*181

        JPDS=-1
        JGDS=-1

        len=index(etafile//' ',' ')-1

        call baopen(11,etafile(1:len),IRETO)
        write(6,*) 'BAOPEN in get_gds: ', IRETO

        if (IRETO .ne. 0) then
         print *,'Error opening unit=11, file name = ',etafile(1:len)
     .          ,' iostat = ',kerr
         stop
        endif

        jpds(5)=7
        jpds(6)=100
        jpds(7)=500
	write(6,*) 'calling getgb'
        call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,tmp,IRET1)
	write(6,*) 'return from getgb ', IRET1


       gdsinfo(1)=KGDS(2)
       gdsinfo(2)=KGDS(3)
       gdsinfo(3)=KGDS(4)
       gdsinfo(4)=KGDS(5)
       gdsinfo(5)=KGDS(7)
       gdsinfo(6)=KGDS(8)
       gdsinfo(7)=KGDS(9)
       gdsinfo(8)=KGDS(12)
       gdsinfo(9)=KGDS(13)
       write(6,*) gdsinfo

        return
        print *,'GETGDS PROBLEM'
        stop

        end

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

                subroutine get_fullgds(etafile,nx,ny,kgds)

        character*(*) etafile
        character(LEN=1) pds(50)

        integer kgds(200),kpds(200),len,kerr,jpds(200)
     .         ,lenpds,lenkgds,nwords,kpdsl,jgds(200)
     .         ,j,k,KNUM,nx,ny

        logical bitmap(nx*ny)
        real tmp(nx*ny)

        write(6,*) 'inside get_fullgds...', etafile


        nxny=nx*ny

        len=index(etafile//' ',' ')-1

        jpds=-1
        jgds=-1

        jpds(5)=11
        jpds(6)=100
        jpds(7)=500

        write(6,*) 'calling getgb '
        write(6,*) 'jpds =  ', jpds
        write(6,*) 'jgds =  ', jgds

        call getgb(11,0,nxny,0,JPDS,JGDS,nwords,KNUM,KPDS,KGDS,
     &     BITMAP,tmp,IRET1)
        write(6,*) 'return from getgb ', IRET1

        if (IRET1 .ne. 0) then
         print *,'Error  getting GDS in get_fullgds ', IRET1
         stop
        endif

        write(6,*) 'leaving get_fullgds'
        return
        end

