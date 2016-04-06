	program degribtiles_driver
C----------------------------------------------------------------------

        INCLUDE	        'params.inc'

	REAL*4		esat, es

	INTEGER*4	ntile, iun, i, j, k, l, istart, jstart

        INTEGER*4       datsav(IDAT), tileone(IDAT), kpds(IPDS)

	CHARACTER*72	degrib_dir, outdir, tname, ahold, filename
	CHARACTER*72	grib_files(IFILEMAX),g_files(IFILEMAX)

	COMMON		/estab/ esat(15000:45000),es(15000:45000)

	DATA	        IO /10/

C----------------------------------------------------------------------

C
C       INITIALIZE THE TABLES
C
 	call es_ini

	grib_files = "NONE"
C
C	READ FILENAMES FROM USER PROVIDED INPUT FILE
C
C	READ ( 5, '(A)' ) filename

C       IF ( filename .eq. ' ' ) THEN
C          print *,"program <filename>"
C          print *, "I'm out of here..."
C          print *
C          STOP
C       END IF

C	OPEN( IO,FILE=filename,STATUS='old',iostat=ierr )

        READ (5,'(A)',END=9000) outdir

        DO I = 1, IFILEMAX
           READ (5,'(A)',END=3000) g_files(I)
           IF ( g_files(I).eq.' ' ) goto 3000
           NTILES = I
        END DO

 3000   CONTINUE 

C
C	REARRANGE GRIB FILES
C 
        DO I = 1, NTILES
           N = 1
           DO J = 1, NTILES
              IF ( g_files(I).GT.g_files(J) ) N = N + 1
           END DO
           grib_files(N) = g_files(I)
        END DO

        ITOT = 0
        JTOT = 0

        DO I = 1, NTILES

           ion = io + I

           filename = grib_files(I)

           CALL GET_GDS( filename, datsav, kpds, ion )
           
           IF ( I.EQ.1 ) THEN
              DO J = 1, IDAT
                 tileone(J)=datsav(J)
              END DO
           END IF

           CALL GET_INDEX( tileone, istart, jstart, datsav )

           ITOT = max (istart-1,itot )
           JTOT = max (jstart-1,jtot )

           NGX = ITOT + datsav(1)
           NGY = JTOT + datsav(2)

        END DO

        CALL DEGRIB2MODEL( grib_files, outdir, ntiles, ngx, ngy )
           
        STOP 'TILES TO GRIB'

 9000   WRITE(6,*)"Premature file termination - Bummer!"
        

        END

C--------------------- SUBROUTINE GET_GDS --------------------------------

       subroutine GET_GDS( etafile, gdsinfo, kpds, io ) 

       INCLUDE 		'params.inc'

       REAL*4		temp1(nxny)

       INTEGER*4	l, iret, nwords, knum, io
       
       INTEGER*4 	kpds(ipds), kgds(ipds), jgds(ipds), jpds(200)
     .                 ,gdsinfo(idat)

       CHARACTER*72	etafile

       LOGICAL  	bitmap(nxny)


C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

       jpds = -1
       jgds = -1

C      l=index(etafile//' ',' ')-1
       l=index(etafile,' ')-1

       CALL baopen( io, etafile, iret )
       IF ( iret .NE. 0 ) then
          WRITE(6,*)"Error in GET_GDS: "
          WRITE(6,*)"Error opening unit=",io," file name = ",
     .               etafile(1:l)," status =",iret
          STOP
       END IF

       jpds(5)=7
       jpds(6)=100
       jpds(7)=500

       CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum, 
     .            kpds, kgds, bitmap, temp1, iret ) 

       gdsinfo(1)  = KGDS(2)
       gdsinfo(2)  = KGDS(3)
       gdsinfo(3)  = KGDS(4)
       gdsinfo(4)  = KGDS(5)
       gdsinfo(5)  = KGDS(7)
       gdsinfo(6)  = KGDS(8)
       gdsinfo(7)  = KGDS(9)
       gdsinfo(8)  = KGDS(12)
       gdsinfo(9)  = KGDS(13)
       write(6,*) 'gds in get_gds: ', (gdsinfo(II),II=1,4)

       CALL baclose( io , iret ) 
       IF ( iret .NE. 0 ) then
          WRITE(6,*)"Error in GET_GDS: "
          WRITE(6,*)"Error closing unit=",io," file name = ",
     .               etafile(1:l)," status =",iret
          STOP
       END IF


       RETURN
       END
C--------------------- END SUBROUTINE GET_GDS --------------------------------


C---------------------  SUBROUTINE GET_INDEX ------------------------------------

        subroutine GET_INDEX( tileone, outi, outj, datsav )

	
C
C       Subroutine written 11 March 1999 by M. Pyle to support tiled 221 input.
C       Code adapted from GEMPAK routine gblamb.c.
C       Purpose is to do a K. Brill worthy piece of tiling code that will
C       figure out where to put the data in the output data set (get the
C       I and J indices)
C

        integer*4 latin1,latin2,nx,ny,la1,lo1,lov,tonelat,tonelon
        integer*4 dx,dy,datsav(9),outi,outj,tileone(9)

        real*8 earth_rad, const_cone, xll,yll,xur,yur,lat1,lon1,loncnt,
     +  angle1,angle2,x1,x2,y1,y2,alpha,rtemp,latfst,lonfst
        real*4 gdslatur,gdslonur
     +  ,gdslatll,gdslonll

        parameter(rpi=3.141592654)
        parameter(d2r=rpi/180.)
        parameter(r2d=180./rpi)
        parameter(radius=6370000.)

C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C
C       latin values are the true "cutting" latitudes of the projection
C       la1, lo1 are the lat/lon of the LL point of the GRIB data
C       dx,dy is a measure of grid spacing
C       nx,ny are the I and J dimensions of the GRIB data
C       lov is the center longitude of the projection
C

        latin1=datsav(8)
        latin2=datsav(9)
        la1=datsav(3)
        lo1=datsav(4)
        tonelat=tileone(3)
        tonelon=tileone(4)
        dx=datsav(6)
        dy=datsav(7)
        nx=datsav(1)
        ny=datsav(2)
        lov=datsav(5)

        latfst=(tonelat/1000.0)*d2r
        lat1= (la1/1000.0)*d2r
        if (lo1 .ge.  180000) lo1=lo1-360000
        if (lo1 .lt. -180000) lo1=lo1+360000
        if (tonelon .ge.  180000) tonelon=tonelon-360000
        if (tonelon .lt. -180000) tonelon=tonelon+360000
        lon1= (lo1/1000.0)*d2r
        lonfst= (tonelon/1000.0)*d2r

Cmp     now have LL corner in radians, W is negative

        if (lov .ge.  180000) lov=lov-360000
        if (lov .lt. -180000) lov=lov+360000
        loncnt= (lov/1000.0)*d2r

        angle1= (rpi/2.) - ( abs(latin1/1000.0) * d2r )
        angle2= (rpi/2.) - ( abs(latin2/1000.0) * d2r )

        if (latin1 .eq. latin2) then
        const_cone=cos(angle1)
        else
        const_cone= ( log ( sin(angle2) ) - log ( sin ( angle1 ) ) )/
     +  ( log ( tan ( angle2/2 ) ) - log ( tan ( angle1/2 ) ) )
        endif

        earth_rad=radius/const_cone

cmp     assuming NH

        x1 = earth_rad * tan( (rpi/2.-latfst) / 2 )**(const_cone)*
     +  sin(const_cone * ( lonfst - loncnt ) )

        x2 = earth_rad * tan( (rpi/2. - lat1) / 2 )**(const_cone)*
     +  sin(const_cone * ( lon1  -  loncnt ) )

        y1=- earth_rad * tan( (rpi/2.-latfst) / 2 )**(const_cone)*
     +  cos(const_cone * ( lonfst - loncnt ) )

        y2 =-earth_rad * tan( (rpi/2. - lat1) / 2 )**(const_cone)*
     +  cos(const_cone * ( lon1  -  loncnt ) )

        alpha= (tan(angle1 / 2 )**const_cone)/sin (angle1)

        xll=min(x1,x2)
        xur=max(x1,x2)
        yll=min(y1,y2)
        yur=max(y1,y2)

        if ( (xur - xll) .lt. 100.) then
        outi=1
        else
        outi= (xur-xll)/(alpha*dx) + 2
        endif

        if ( (yur - yll ) .lt. 100.) then
        outj=1
        else
        outj= (yur-yll)/(alpha*dy) + 2
        endif

        return

        END


C--------------------- END SUBROUTINE GET_INDEX -------------------------------

C---------------------  SUBROUTINE DEGRIB2MODEL -------------------------------

	subroutine DEGRIB2MODEL( filenames, outdir, ntiles, ngx, ngy )

        INCLUDE	 	'params.inc'

	REAL*4	CP, KAPPA
        PARAMETER ( CP = 1004.0, KAPPA = 287./1004. )

	INTEGER*4	kgds(IPDS),    kpds(IPDS)
        INTEGER*4	gdsinfo(IDAT), datsav(IDAT), tileone(IDAT)
	INTEGER*4       i, j, k, l, ntile, numtile, nsfcfld, ip, jp,
     .                  ntiles, io, kio, ngx, ngy

 	REAL*4		 ht(ngx,ngy,nz),htin(nx,ny,nz) !Isobaric heights (m)
     .  		,tp(ngx,ngy,nz),tpin(nx,ny,nz) !Isobaric temps (K)
     .  		,th(ngx,ngy,nz),thin(nx,ny,nz) !Isobaric theta (K)
     .  		,uw(ngx,ngy,nz),uwin(nx,ny,nz) !Isobaric u-wind (m/s)
     .  		,vw(ngx,ngy,nz),vwin(nx,ny,nz) !Isobaric v-wind (m/s)
     .  		,rh(ngx,ngy,nz),rhin(nx,ny,nz) !Isobaric rh,mr (%,kg/kg)
     .      		,ex(ngx,ngy,nz)                !Isobaric Exner
     .      		,pr(nz),pri(nz)              !Isobaric pressures (mb)
     .      		,lat1,lat2,lon0,sw(2),ne(2)
     .      		,xe,mrsat,esat,es
     . 			,slp(ngx,ngy,12),slpin(nx,ny,12),llcorner(2,30)
     . 			,tmp(ngx*ngy,nz)

	CHARACTER*72	filenames(IFILEMAX), outdir, outfile, degrib_file, 
     .                  gdsfile
        CHARACTER*11	atime
        CHARACTER*2     gproj

	common /estab/esat(15000:45000),es(15000:45000)

	DATA	KIO /20/

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\

        DO NTILE = 1, NTILES

           IO = KIO + NTILE

           NUMTILE = ntile
           degrib_file = filenames(NTILE)

           print *,NTILE," ",degrib_file(1:25)

           DO K = 1, NZ
 	      pr(k) = 1000.-float(k*25)
 	   END DO

           CALL GET_GDS( degrib_file, datsav, kpds, io )

   	   CALL READ_DEGRIB( degrib_file, pr, htin, tpin, rhin,
     .                       uwin, vwin, slpin, atime )

           IF ( NTILE .EQ. 1) THEN
              DO I = 1, IDAT
                 tileone(I) = datsav(I)
              END DO
           END IF

  	   llcorner(1,ntile)=datsav(3)
           if (datsav(4) .ge. 180000) datsav(4)=datsav(4)-360000
           llcorner(2,ntile)=datsav(4)


           CALL GET_INDEX( tileone, istart, jstart, datsav )

	   DO K = 1, NZ
  	      DO I = istart, istart+datsav(1)-1
              DO J = jstart, jstart+datsav(2)-1

                 ival=(I-istart)+1
                 jval=(J-jstart)+1

                 ht(I,J,K)=htin(ival,jval,K)
                 tp(I,J,K)=tpin(ival,jval,K)
                 rh(I,J,K)=rhin(ival,jval,K)
                 uw(I,J,K)=uwin(ival,jval,K)
                 vw(I,J,K)=vwin(ival,jval,K)

              END DO
              END DO
      	   END DO

           DO K = 1, 12
              DO I = istart, istart+datsav(1)-1
              DO J = jstart, jstart+datsav(2)-1

                 ival=(I-istart)+1
                 jval=(J-jstart)+1

                 slp(I,J,K) = slpin(ival,jval,K)
              ENDDO
              ENDDO
           ENDDO
        
           DO K = 1, 12
              if (slp(1,1,k) .eq. -99999.) then

                 write(6,*) 'SURFACE DATA MISSING... ', K

                 if (k.eq.9.or.k.eq.11.) then
                     write(6,*) 'filling soil temp data...'
                     do j=1,ngy
                     do i=1,ngx
                        slp(i,j,k)=slp(i,j,7)
                     enddo
                     enddo
                  else if (k.eq.10.or.k.eq.12.) then
                     do j=1,ngy
                     do i=1,ngx
                        slp(i,j,k)=slp(i,j,8)
                     enddo
                     enddo
                 endif

               endif
           END DO

Cmp
Cmp     Only do this stuff when the grid is completely filled!
Cmp
	   IF ( ntile .EQ. ntiles ) THEN 
c
c       Check for any missing data.

              DO I = 1, NZ
                 if (ht(1,1,k) .eq. -99999.) then
                    print *,'Height data missing at level: ',pr(k)
                    stop
                 else if (tp(1,1,k) .eq. -99999.) then
                    print *,'Temperature data missing at level: ',pr(k)
                    stop
                 else if (rh(1,1,k) .eq. -99999.) then
                    print *,'RH data missing at level: ',pr(k)
                    print *,'Calling RH patch.'
                    call rh_fix(ngx,ngy,nz,rh,pr)
                 else if (uw(1,1,k) .eq. -99999.) then
                    print *,'U-wind data missing at level: ',pr(k)
                    stop
                 else if (vw(1,1,k) .eq. -99999.) then
                    print *,'V-wind data missing at level: ',pr(k)
                    stop
                 endif

                 DO K = 1, NZ
                    pri(k)=1./pr(k)
                 END DO

              END DO 

              DO K = 1, NZ
                 DO J = 1, NGY
                 DO I = 1, NGX
                    th(i,j,k)=tp(i,j,k)*(1000.*pri(k))**kappa
                    it=tp(i,j,k)*100
                    it=min(45000,max(15000,it))
                    xe=esat(it)
                    mrsat=0.00622*xe/(pr(k)-xe)
                    rh(i,j,k)=rh(i,j,k)*mrsat
                 END DO
                 END DO
              END DO
C
C          CREATE OUTPUT FILENAME
C

               
              write(6,*) " "
              write(6,*) 'WRITING THE DATA TO FILE'

              l=index(outdir,' ')-1
              outfile = outdir(1:l)//atime//'.'//'ETA_tile'

              open(1,file=outfile,status='unknown',form='unformatted')
C
C          WRITE HEADER INFORMATION
C
              ip=1
              jp=1
              nsfcfld=12
              gproj='LC'
              write(1) ngx,ngy,nz,ngx,ngy,ip,jp,nsfcfld,gproj

              lat1=datsav(8)/1000.
              lat2=datsav(9)/1000.
              lon0=datsav(5)/1000.
Cmp
Cmp     The sw(1) and sw(2) values are passed from the degribbing, use the
Cmp     lambert subroutine to calculate the ne values (assumes that the
Cmp     most NE'ly tile was processed last and provides the GDS info in
Cmp     datsav)
Cmp
Cmp     Could this obtaining of NE corner point be made more general so
Cmp     as to prevent needing to process that grid last?

              write(6,*) 'gds based sw values ', llcorner(1,1)/1000.,
     +                                           llcorner(2,1)/1000.
              call lambert (urlat,urlon,datsav)
              write(6,*) 'gds based ne corner ', urlat,urlon

              sw(1)=llcorner(1,1)/1000.
              sw(2)=llcorner(2,1)/1000.
              ne(1)=urlat
              ne(2)=urlon

C             call get_fullgds(degrib_file,datsav(1),datsav(2),kgds)
              call get_fullgds(degrib_file,kgds)

C       overwrite grid dims and sw corner coordinates

              kgds(2)=ngx
              kgds(3)=ngy
              kgds(4)=sw(1)*1000.
              kgds(5)=sw(2)*1000.
C
C       NEW INFORMATION FOR GDS FILE 
C
              l=index(outdir,' ')-1
              gdsfile=outdir(1:l)//'/'//'gdsinfo.'//'ETA_tile'
              l=index(gdsfile,' ')-1
              open(unit=14,file=gdsfile(1:l),form='unformatted',
     +             access='sequential')

              write(6,*) 'writing kgds ', (kgds(I),i=1,14)
              write(14) kgds
              close(14)

              WRITE(1) ngx,ngy,nz,lat1,lat2,lon0,sw,ne
C
C          Write isobaric upper air data.
C
              write(6,*) 'level 20', ngx,ngy
              write(6,*) 'mixr * 1000.'
              do J=ngy,1,-(ngy/15)
                 write(6,744) (rh(I,J,20)*1000.,I=1,ngx,ngx/9)
              enddo

              write(6,*) 'HT'
              do J=ngy,1,-(ngy/15)
                 write(6,744) (ht(I,J,20),I=1,ngx,ngx/9)
              enddo

              write(6,*) 'UW'
                 do J=ngy,1,-(ngy/15)
                 write(6,744) (uw(I,J,20),I=1,ngx,ngx/9)
              enddo
   
              write(6,*) 'VW'
              do J=ngy,1,-(ngy/15)
                 write(6,744) (vw(I,J,20),I=1,ngx,ngx/9)
              enddo

              write(6,*) 'TH'
              do J=ngy,1,-(ngy/15)
                 write(6,744) (th(I,J,20),I=1,ngx,ngx/9)
              enddo

  743         format(30(e9.3,x))
  744         format(30(f7.1,x))

              WRITE(1) uw
              WRITE(1) vw
              WRITE(1) ht
              WRITE(1) rh
              WRITE(1) pr
              WRITE(1) slp

              do k=1,nsfcfld
                 write(6,*) 'field, value ', k, slp(ngx/2,ngy/2,k)
              enddo

              CLOSE(1)
         

           ENDIF

       END DO

       RETURN
       END

C-------------------  END SUBROUTINE DEGRIB2MODEL -------------------------------

C-------------------   SUBROUTINE READ_DEGRIB -------------------------------

	subroutine READ_DEGRIB( filename, pr, htout, tpout, rhout, 
     .                          uwout, vwout, slpout, atime )

	INCLUDE         'params.inc'

	REAL*4		pr(nz),      ht(nxny,nz), tp(nxny,nz),
     .                  rh(nxny,nz), uw(nxny,nz), vw(nxny,nz),
     .                  slp(nxny,12)


	REAL*4		htout(nx,ny,nz),tpout(nx,ny,nz),rhout(nx,ny,nz),
     .                  uwout(nx,ny,nz),vwout(nx,ny,nz),slpout(nx,ny,12)

	REAL*4		crot(nxny), srot(nxny)

	INTEGER*4	kgds(IPDS), kpds(IPDS), jpds(IPDS), jgds(IPDS)
	INTEGER*4	datsav(IDAT)
	INTEGER*4	nwords, iret, knum, i, k, l

	CHARACTER*72    filename
	CHARACTER*11    atime

	LOGICAL		bitmap(nxny)

	DATA    IO /10/

C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

        DO K = 1, NZ
           ht(1,k) = -99999.
           tp(1,k) = -99999.
           rh(1,k) = -99999.
           uw(1,k) = -99999.
           vw(1,k) = -99999.
        END DO

        DO K = 1, 12
        DO J = 1, NXNY
           slp(j,k) = -99999.
        ENDDO
        ENDDO

	JPDS = -1
	JGDS = -1


	CALL GET_GDS( filename, datsav, kpds, io )

	IF (kpds(8) .ge. 100) kpds(8) = kpds(8) - 100

	WRITE( atime,'(i2.2,i2.2,i2.2,i2.2,i3.3)' )
     .         kpds(8),kpds(9),kpds(10),kpds(11),kpds(14) 


        CALL baopen( io, filename, iret )
        IF ( iret .NE. 0 ) then
           WRITE(6,*)"Error in GET_GDS: "
           WRITE(6,*)"Error opening unit=",io," file name = ",
     .               filename," status =",iret
           STOP
        END IF

        jpds(5)=81
        jpds(6)=1
        jpds(7)=0
        CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,1), iret )

C       write(6,*) 'LAND/SEA READ!!!!! '
C       write(6,*) (slp(j,1),j=nwords/2,nwords/2+5)

        jpds(5)=2
        jpds(6)=102
        jpds(7)=0
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,2), iret )

C       write(6,*) 'PMSL READ!!!!! '
C       write(6,*) (slp(j,2),j=nwords/2,nwords/2+8)

	jpds(5)=1
        jpds(6)=1
        jpds(7)=0
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,3), iret )

C       write(6,*) 'PSFC READ!!!!! '
C       write(6,*) (slp(j,3),j=nwords/2,nwords/2+8)

	jpds(5)=7
        jpds(6)=1
        jpds(7)=0
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,4), iret ) 

C       write(6,*) 'ZSFC READ!!!!! '
C       write(6,*) (slp(j,4),j=nwords/2,nwords/2+8)


	jpds(5)=85
        jpds(6)=112
        jpds(7)=10
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,5), iret )

	jpds(7)=2600
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,7), iret )

	jpds(7)=10340
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,9), iret )

	jpds(7)=25800
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,11), iret )

C       write(6,*) 'found soil temp over ',jpds(7), 'layer!!',IRET1
C       write(6,*) (slp(j,11),j=nwords/2,nwords/2+5)

C
C	MOISTURE
C
        
C       print *, "DOING MOISTURE"

	jpds(5)=144
        jpds(7)=10
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,6), iret )

	jpds(7)=2600
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,8), iret )

	jpds(7)=10340
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,10), iret )

	jpds(7)=25800
	CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, slp(1,12), iret )

C       write(6,*) 'found soil wet over ',jpds(7), 'layer!!',IRET1
C       write(6,*) (slp(j,12),j=nwords/2,nwords/2+5)


	jpds(6)=100
	DO K = NZ, 1, -1
	   jpds(7) = nint(pr(k))
	   jpds(5) = 7 
	   CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .                kpds, kgds, bitmap, ht(1,k), iret )

	   jpds(5)=11
	   CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .                kpds, kgds, bitmap, tp(1,k), iret )

	   jpds(5)=52
	   CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .                kpds, kgds, bitmap, rh(1,k), iret )

	   jpds(5)=33
	   CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .                kpds, kgds, bitmap, uw(1,k), iret )

	   jpds(5)=34
	   CALL GETGB(io, 0, nxny, 0, jpds, jgds, nwords, knum,
     .                kpds, kgds, bitmap, vw(1,k), iret )

	END DO

        CALL baclose( io, iret )

 1000	CONTINUE

 	CALL ROTATE_LCC( kgds, crot, srot )

 	DO K = 1, NZ
 	DO I = 1, NXNY
 	   rmagb = (uw(I,K)**2. + vw(I,K)**2.)**(0.5)
 	   ubef  = uw(I,K)
 	   vbef  = vw(I,K)

           uw(I,K) = crot(I)*ubef+srot(I)*vbef
           vw(I,K) = crot(I)*vbef-srot(I)*ubef
           rmagaft =(uw(I,K)**2. + vw(I,K)**2.)**(0.5)

           if (abs(rmagaft-rmagb).gt.3.) then
              write(6,*) 'MAG, I,K,old,new==> ',I,K,rmagb,rmagaft
              write(6,*) 'original components..', ubef,vbef
              write(6,*) 'new components..', uw(I,k),vw(I,K)
              write(6,*) 'rotation cosines ', crot(I),srot(I)
              write(6,*) '.................................'
           endif
        ENDDO
        ENDDO

        call resort_1d( ht, htout, nz, datsav)
        call resort_1d( uw, uwout, nz, datsav)
        call resort_1d( vw, vwout, nz, datsav)
        call resort_1d( tp, tpout, nz, datsav)
        call resort_1d( rh, rhout, nz, datsav)
        call resort_1d( slp,slpout,12, datsav)

        RETURN
c
c ***   Premature end of file.
c
1100  continue
      print *,'Premature end of file.'
      print *,'Abort...'
      STOP
c
      END
c

C-------------------  END SUBROUTINE READ_DEGRIB -------------------------------

C-------------------   SUBROUTINE ROTATE_LCC  -------------------------------

	subroutine ROTATE_LCC( kgds, crot, srot )

        INCLUDE	'params.inc'

Cmp     stolen/adapted from iplib code gdswiz03
C
C SUBPROGRAM:  GDSWIZ03   GDS WIZARD FOR LAMBERT CONFORMAL CONICAL
C   PRGMMR: IREDELL       ORG: W/NMC23       DATE: 96-04-10
C
C ABSTRACT: THIS SUBPROGRAM DECODES THE GRIB GRID DESCRIPTION SECTION
C           (PASSED IN INTEGER FORM AS DECODED BY SUBPROGRAM W3FI63)
C           AND RETURNS ONE OF THE FOLLOWING:
C             (IOPT=+1) EARTH COORDINATES OF SELECTED GRID COORDINATES
C             (IOPT=-1) GRID COORDINATES OF SELECTED EARTH COORDINATES
C           FOR LAMBERT CONFORMAL CONICAL PROJECTIONS.
C           IF THE SELECTED COORDINATES ARE MORE THAN ONE GRIDPOINT
C           BEYOND THE THE EDGES OF THE GRID DOMAIN, THEN THE RELEVANT
C           OUTPUT ELEMENTS ARE SET TO FILL VALUES.
C           THE ACTUAL NUMBER OF VALID POINTS COMPUTED IS RETURNED TOO.

C       LAMBERT CONFORMAL GRIDS
C          (2)   - NX NR POINTS ALONG X-AXIS
C          (3)   - NY NR POINTS ALONG Y-AXIS
C          (4)   - LA1 LAT OF ORIGIN (LOWER LEFT)
C          (5)   - LO1 LON OF ORIGIN (LOWER LEFT)
C          (6)   - RESOLUTION (RIGHT ADJ COPY OF OCTET 17)
C          (7)   - LOV - ORIENTATION OF GRID
C          (8)   - DX - X-DIR INCREMENT
C          (9)   - DY - Y-DIR INCREMENT
C          (10)  - PROJECTION CENTER FLAG
C          (11)  - SCANNING MODE FLAG (RIGHT ADJ COPY OF OCTET 28)
C          (12)  - LATIN 1 - FIRST LAT FROM POLE OF SECANT CONE INTER
C          (13)  - LATIN 2 - SECOND LAT FROM POLE OF SECANT CONE INTER

      INTEGER KGDS(IPDS)

      REAL RLON(NXNY),RLAT(NXNY)
      REAL CROT(NXNY),SROT(NXNY)
        real DLON,AN
        real  DE,DR
        REAL PI,DPR
      PARAMETER(RERTH=6.3712E6)
      PARAMETER(PI=3.14159265,DPR=180./PI)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

        FILL=-999.
        LROT=1
        IROT=1
        IM=KGDS(2)
        JM=KGDS(3)
        RLAT1=KGDS(4)*1.E-3
        RLON1=KGDS(5)*1.E-3
        IROT=MOD(KGDS(6)/8,2)
        ORIENT=KGDS(7)*1.E-3
        DX=KGDS(8)
        DY=KGDS(9)
        IPROJ=MOD(KGDS(10)/128,2)
        ISCAN=MOD(KGDS(11)/128,2)
        JSCAN=MOD(KGDS(11)/64,2)
        NSCAN=MOD(KGDS(11)/32,2)
        RLATI1=KGDS(12)*1.E-3
        RLATI2=KGDS(13)*1.E-3
        H=(-1.)**IPROJ
        HI=(-1.)**ISCAN
        HJ=(-1.)**(1-JSCAN)
        DXS=DX*HI
        DYS=DY*HJ

        IF(RLATI1.EQ.RLATI2) THEN
          AN=SIN(H*RLATI1/DPR)
        ELSE
          AN=LOG(COS(RLATI1/DPR)/COS(RLATI2/DPR))/
     &       LOG(TAN((H*RLATI1+90)/2/DPR)/TAN((H*RLATI2+90)/2/DPR))
        ENDIF      
        DE=RERTH*COS(RLATI1/DPR)*TAN((H*RLATI1+90)/2/DPR)**AN/AN
        IF(H*RLAT1.EQ.90) THEN
          XP=1
          YP=1
        ELSE
          DR=DE/TAN((H*RLAT1+90)/2/DPR)**AN
          DLON1=MOD(RLON1-ORIENT+180+3600,360.)-180
C       atmp=RLON1-ORIENT+180.+3600.
C       DLON1= atmp - INT (atmp/360.)*360. - 180.
          XP=1-H*SIN(AN*DLON1/DPR)*DR/DXS
          YP=1+COS(AN*DLON1/DPR)*DR/DYS
        ENDIF
        ANTR=1/(2*AN)
        DE2=DE**2
        XMIN=0
        XMAX=IM+1
        YMIN=0
        YMAX=JM+1
        NRET=0

C
C  TRANSLATE GRID COORDINATES TO EARTH COORDINATES
C       XP=1
C       YP=1
        DO N=1,IM*JM
        J=INT((N-1)/IM)+1
        I=N-(J-1)*IM
            IF(I.GE.XMIN.AND.I.LE.XMAX.AND.
     &         J.GE.YMIN.AND.J.LE.YMAX) THEN
              DI=(I-XP)*DXS
              DJ=(J-YP)*DYS
              DR2=DI**2+DJ**2
              IF(DR2.LT.DE2*1.E-6) THEN
                RLON(N)=0.
                RLAT(N)=H*90.
              ELSE
                RLON(N)=MOD(ORIENT+H/AN*DPR*ATAN2(DI,-DJ)+3600,360.)
C       atmp=ORIENT+H/AN*DPR*ATAN2(DI,-DJ)+3600
C       RLON(N)= atmp - INT(atmp/360.) * 360.
                RLAT(N)=H*(2*DPR*ATAN((DE2/DR2)**ANTR)-90)
              ENDIF
              NRET=NRET+1
              IF(LROT.EQ.1) THEN
                IF(IROT.EQ.1) THEN
C       atmp=RLON(N)-ORIENT+180.+3600.
C       DLON=atmp - INT(atmp/360.)*360.-180.
                  DLON=MOD(RLON(N)-ORIENT+180+3600,360.)-180
                  CROT(N)=H*COS(AN*DLON/DPR)
                  SROT(N)=SIN(AN*DLON/DPR)
                ELSE
                  CROT(N)=1
                 SROT(N)=0
                ENDIF
              ENDIF
            ELSE
              RLON(N)=FILL
              RLAT(N)=FILL
            ENDIF
          ENDDO

        RETURN
      END

C------------------   END SUBROUTINE ROTATE_LCC  -------------------------------

C--------------------   SUBROUTINE RESORT_1D  -------------------------------

	subroutine RESORT_1D( arrayin, arrayout, nk, datsav )

        INCLUDE		'params.inc'

	REAL*4	 arrayout(NX,NY,NK)
	REAL*4	 arrayin (NX*NY,NK)

	INTEGER*4	I, J, K, datsav(IDAT)
	
        arrayout = -9999.

        do 90 K=1, NK

           do 89 J=1,datsav(2)
           do 89 I=1,datsav(1)
              indval=(J-1)*datsav(1)+I
              arrayout(I,J,K)=arrayin(indval,K)
   89      continue
   90   continue

        RETURN
        END

C------------------   END SUBROUTINE RESORT_1D  -------------------------------

C--------------------  SUBROUTINE GET_FULLGDS  -------------------------------

        subroutine GET_FULLGDS( etafile, kgds )

	INCLUDE 'params.inc'

        INTEGER*4       kgds(IPDS), kpds(IPDS), jpds(IPDS), jgds(IPDS)
        INTEGER*4       datsav(IDAT)
	INTEGER*4       nwords, iret, knum, i, k, l

	REAL*4		temp1(NXNY)
        LOGICAL 	bitmap(NXNY)

	CHARACTER*72	etafile

        DATA            IO /60/

         l=index(etafile,' ')-1
        CALL baopen( io, etafile, iret )
        IF ( iret .NE. 0 ) then
           WRITE(6,*)"Error in GET_FULLGDS: "
           WRITE(6,*)"Error opening unit=",io," file name = ",
     .               etafile," status =",iret
          STOP
        END IF

         jpds=-1
         jgds=-1

        jpds(5)=7
        jpds(6)=100
        jpds(7)=500

        CALL GETGB(IO, 0, NXNY, 0, jpds, jgds, nwords, knum,
     .             kpds, kgds, bitmap, temp1, iret )

        IF (iret .NE. 0) then
           print *,'Error  getting GDS in get_fullgds ', IRET1
           stop
        ENDIF

        write(6,*) 'leaving get_fullgds'
        RETURN
        END


C---------------   END SUBROUTINE GET_FULLGDS  -------------------------

C--------------------  SUBROUTINE RH_FIX  -------------------------------

      subroutine RH_FIX( rh, pr )

      INCLUDE 'params.inc'

      integer*4 i, j, k, kk
 
      REAL*4 rh(NX,NY,NZ),pr(NZ)

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

C---------------   END SUBROUTINE RH_FIX  -------------------------

C--------------------  SUBROUTINE RH_FIX  -------------------------------

        subroutine LAMBERT(gdslatur,gdslonur,datsav)

C
C       Subroutine written 9 March 1999 by M. Pyle to support tiled 221 input.
C       Code adapted from GEMPAK routine gblamb.c.  Whole purpose is to get the
C       UR corner lat and lon for use in workstation Eta.

        integer latin1,latin2,nx,ny,la1,lo1,lov
        integer dx,dy,datsav(9)

        real*8 earth_rad, const_cone, xll,yll,xur,yur,lat1,lon1,loncnt,
     +  angle1,angle2,x1,x2,y1,y2,alpha,rtemp
        real*4 gdslatur,gdslonur
     +  ,gdslatll,gdslonll

        parameter(rpi=3.141592654)
        parameter(d2r=rpi/180.)
        parameter(r2d=180./rpi)
        parameter(radius=6370000.)

        latin1=datsav(8)
        latin2=datsav(9)
        la1=datsav(3)
        lo1=datsav(4)
        dx=datsav(6)
        dy=datsav(7)
        nx=datsav(1)
        ny=datsav(2)
        lov=datsav(5)


        lat1= (la1/1000.0)*d2r
        if (lo1 .eq.  180000) lo1=lo1-360000
        if (lo1 .lt. -180000) lo1=lo1+360000
        lon1= (lo1/1000.0)*d2r

Cmp     now have LL corner in radians, W is negative

        if (lov .eq.  180000) lov=lov-360000
        if (lov .lt. -180000) lov=lov+360000
        loncnt= (lov/1000.0)*d2r

        angle1= (rpi/2.) - ( abs(latin1/1000.0) * d2r )
        angle2= (rpi/2.) - ( abs(latin2/1000.0) * d2r )

        if (latin1 .eq. latin2) then
        const_cone=cos(angle1)
        else
        const_cone= ( log ( sin(angle2) ) - log ( sin ( angle1 ) ) )/
     +  ( log ( tan ( angle2/2 ) ) - log ( tan ( angle1/2 ) ) )
        endif

C        write(6,*) 'const_cone= ', const_cone

        earth_rad=radius/const_cone

cmp     assuming NH

        x1 = earth_rad * tan( (rpi/2.-lat1) / 2 )**(const_cone)*
     +  sin (const_cone * ( lon1 - loncnt ) )
        y1 = -earth_rad * tan( (rpi/2.-lat1) / 2 )**(const_cone)*
     +  cos (const_cone * ( lon1 - loncnt ) )

         alpha= (tan(angle1 / 2 )**const_cone)/sin (angle1)

        x2=x1 + ( nx - 1 ) * alpha * dx
        y2=y1 + ( ny - 1 ) * alpha * dy

        xll=min(x1,x2)
        xur=max(x1,x2)
        yll=min(y1,y2)
        yur=max(y1,y2)

        xlltmp=abs(xll)
        ylltmp=abs(yll)
        gdslatll= ( rpi/2. - 2 *
     + atan ( ( (xlltmp**2.+ylltmp**2.)**(0.5)/earth_rad)**
     + (1/const_cone) ) ) * r2d


        rtemp= atan2 ( xll, -yll ) * ( 1 / const_cone ) + loncnt

        if ( rtemp .gt. rpi ) then
        gdslonll = ( rtemp - 2.*rpi ) * r2d
        else if ( rtemp .lt. -rpi ) then
        gdslonll = ( rtemp + 2.*rpi ) * r2d
        else
        gdslonll = rtemp * r2d
        endif

        xurtmp=abs(xur)
        yurtmp=abs(yur)

        gdslatur= ( rpi/2. - 2 *
     + atan ( ( (xurtmp**2.+yurtmp**2.)**(0.5)/earth_rad)**
     + (1/const_cone) ) ) * r2d

        rtemp= atan2 ( xur, -yur ) * ( 1 / const_cone ) + loncnt
        if ( rtemp .gt. rpi ) then
        gdslonur = ( rtemp - 2.*rpi ) * r2d
        else if ( rtemp .lt. -rpi ) then
        gdslonur = ( rtemp + 2.*rpi ) * r2d
        else
        gdslonur = rtemp * r2d
        endif

C        write(6,*) 'output==> '
C        write(6,*) 'LL points ', gdslatll,gdslonll
C        write(6,*) 'UR points ', gdslatur,gdslonur

        return

        END

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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC



