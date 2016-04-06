/*
 * initdate ( gdate )
 *
 * Computes 00hr forecast date from YYYYMMDDHH FH
 *
 */

#include <stdlib.h>

main(int argc, char *argv[])
{
 
        char  fh[4] = {'\0'},
	      dd[3] = {'\0'},
              mm[3] = {'\0'}, 
              yr[5] = {'\0'},
              hr[3] = {'\0'};

        unsigned long int jdate ;

        unsigned int ndpm[12] = { 31, 28, 31, 30, 31, 30,
                                  31, 31, 30, 31, 30, 31 };
        unsigned int fhour, year, month, day, jday, i, hour=0 ;

        strncpy(yr,&argv[1][0],4);
        strncpy(mm,&argv[1][4],2);
        strncpy(dd,&argv[1][6],2);
        strncpy(hr,&argv[1][8],2);
        strncpy(fh,&argv[2][0],3);

        month = strtol(mm,(char **)NULL,10);
        day   = strtol(dd,(char **)NULL,10);
        year  = strtol(yr,(char **)NULL,10);
        hour  = strtol(hr,(char **)NULL,10);

        fhour = strtol(fh,(char **)NULL,10);

        if ( year % 4 == 0 ) ndpm[1] = 29;

        hour = hour + fhour;

        while ( hour > 23 ) {
           hour = hour - 24;
           day  = day + 1;
           if ( day > ndpm[month-1] ) {
              month = month + 1;
              day = 1;
              if ( month > 12 ) {
                 month = 1;
                 year = year + 1;
              }
           }
        }

        printf("%d\n",year*1000000+month*10000+day*100+hour);

        return ;
}

/*
 *
 * Main function
 *
 */


