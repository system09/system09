/*
 * Motorola S1 format to Intel hex format
 * Usage
 * mot2hex <file_name>
 */

#include <stdio.h>
#include <string.h>


int gethex( FILE *fp_in )
{
    int hex;

    hex = fgetc( fp_in );
    if( (hex >= '0') && (hex <= '9' ))
	hex -= '0';
    else if( (hex >= 'A') && (hex <= 'F'))
	hex = hex - 'A' + 10;
    else
	hex = -1;
    return hex;
}

int get2hex( FILE *fp_in )
{
    int hexhi, hexlo, byte;

    hexhi = gethex( fp_in );
    if( hexhi != -1 )
    {
	hexlo = gethex( fp_in );
	if( hexlo != -1 )
	{
	    byte = hexhi * 16 + hexlo;
	    return byte;
	}
    }
    return -1;
}

int get4hex( FILE *fp_in )
{
    int bytehi, bytelo, addr;

    bytehi = get2hex( fp_in );
    if( bytehi != -1 )
    {
	bytelo = get2hex( fp_in );
	if( bytelo != -1 )
	{
	    addr = (bytehi * 256) + bytelo;
	    return addr;
	}
    }
    return -1;
}    


main( int argc, char *argv[] )
{
	FILE *fp_in, *fp_out;
	char fname_in[32];
	char fname_out[32];
	int byte, addr, start, i;
        int motorola_check, intel_check;
	int motorola_count, intel_count;

	if( argc != 2 )
	{
	    printf( "\n usage: mot2hex <file_name> \n" );
	    exit(0);
	}
 	sprintf( fname_in, "%s.s19", argv[1] );
	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
 	    sprintf( fname_in, "%s.s1", argv[1] );
            fp_in = fopen( fname_in, "r" );
	    if( !fp_in )
	    {
	        printf( "Can't open %s", fname_in );
	        exit(0);
            }
	}
 	sprintf( fname_out, "%s.hex", argv[1] );
	fp_out = fopen( fname_out, "w" );
	if( !fp_out )
	{
	    printf( "Can't open %s", fname_out );
	    exit(0);
	}
	byte = 0;
	addr = 0;
        start = -1;

	while( byte != -1 )
	{
	    /*
	     * Motorola 8 bit record starts with "S1"
	     */
	    do {
		byte = fgetc( fp_in);
	    } while( (byte != 'S') && (byte != -1) );

	    byte = fgetc( fp_in );
	    if( byte == '1' )
	    {
		/*
		 * get byte count from Motorola record
		 */
		motorola_count = get2hex( fp_in );
	        motorola_check = motorola_count;
		/*
		 * Intel byte count is for data field only
		 * round up to even byte boundary
		 */
		intel_count = motorola_count - 3;
		intel_check = intel_count;
		/*
		 * Get two byte motorola address field
		 */
		addr = get4hex( fp_in );
	        motorola_check += (addr & 0xff); 
		motorola_check += (addr & 0xff00) >> 8;
		/*
		 * output intel start of record
		 * ":" <data count> <address> <record type>
		 */
		intel_check += (addr & 0xff);
		intel_check += (addr & 0xff00) >> 8;
	        fprintf( fp_out, ":%02x%04x00", intel_count, addr );
		/*
		 * Input Motorola data field
		 */
	        for( i=0; i<intel_count; i++ )
	        {
		    byte = get2hex( fp_in );
	            motorola_check += byte;
		    /*
		     * Output Intel data field
		     */
		    intel_check += byte;
		    fprintf( fp_out, "%02x", byte );
		    /*
		     * Get the start of record from the reset vector
		     */
	            if( addr == 0xfffe )
		        start = byte * 256;
	            if( addr == 0xffff )
		        start += byte;
		    addr ++;
		}
		/*
		 * Display Motorola cehcksum for current record
		 * should be 0xff
		 */	
	        byte = get2hex( fp_in) & 0xff;
	        motorola_check += byte;
		motorola_check &= 0xff;
	        printf( " Motorola checksum = %02x   ", motorola_check );
		/*
		 * Display Intel checksum
		 */
		intel_check &= 0xff;
		intel_check = 0x100 - intel_check;
		intel_check &= 0xff;
	        printf( "Intel checksum = %02x \n", intel_check );
		/*
		 * Output Intel checksum
		 */
	        fprintf( fp_out, "%02x\n", intel_check);
	        if( addr > 0xffff )
		   byte = -1;
	     }
	}
	/*
	 * output Intel trasfer address
	 */
	if( start != -1 )
	{
	    fprintf( fp_out, ":00%04x01", start );
	}
	/*
	 * Close input and output files and exit
	 */
	close( fp_in );
	close( fp_out );
}
