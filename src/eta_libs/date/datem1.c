/*
 * datem1 ( gdate )
 *
 * finds yesterdays date
 *
 *
 */

#include <stdlib.h>

main(int argc, char *argv[])
{
 
	char  dd[3] = {'\0'},
              mm[3] = {'\0'}, 
              yr[5] = {'\0'},
              hr[3] = {'\0'};

        unsigned long int jdate ;

        unsigned int ndpm[12] = { 31, 28, 31, 30, 31, 30,
                                  31, 31, 30, 31, 30, 31 };
        int fhour, year, month, day, jday, i, hour=0 ;

        strncpy(yr,&argv[1][0],4);
        strncpy(mm,&argv[1][4],2);
        strncpy(dd,&argv[1][6],2);
        strncpy(hr,&argv[1][8],2);

        month = strtol(mm,(char **)NULL,10);
        day   = strtol(dd,(char **)NULL,10);
        year  = strtol(yr,(char **)NULL,10);
        hour  = strtol(hr,(char **)NULL,10);

        fhour = 24;

        if ( year % 4 == 0 ) ndpm[1] = 29;

        hour = hour - fhour;

        while ( hour < - 23 ) {
           hour = hour + 24;
           day  = day - 1;
           if ( day < 1 ) {
              month = month - 1;
              day = ndpm[month-1];
              if ( month < 1 ) {
                 month = 12;
                 day   = 31;
                 year = year - 1;
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
