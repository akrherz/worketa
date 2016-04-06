      program data_prep
c
c *** Program to prepare ETA initialization fields from any gridded
c        data set.
c
c *** Original code obtained from U. of Athens and modified at FSL.
c
      implicit none
c
      include 'ecommons.h'
c
      integer*4 n
c_______________________________________________________________________________
c 
c *** Fill eta model common blocks.
c
      call eta_commons
c
c *** Interpolate data to eta grid from each input init gridded dataset.
c
      do n=1,ninit
c
c ****** Get the native grid dimensions of the input data.
c           (Dims are filled into named common block - sectorsize).
c
         call get_sector_size(init_in(n))
c
         call eta_interp(n)
c
      enddo
c
c *** Create model init and constants files.
c
      call eta_const
c
c *** Create model boundary condition files.
c
      call eta_boco
      write(*,*) 'STOP_PREP'
c
      end
