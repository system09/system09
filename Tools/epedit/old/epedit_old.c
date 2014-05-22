/*
 * epedit
 *
 * binary file editer program
 */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
/*
 * equates
 */
#define EPROM_MAX (1<<16)
#define CMD_LINE_MAX 80
#define FALSE 0
#define TRUE !FALSE
#define BINARY 0
#define MOTOROLA 1
#define INTEL 2
#define SMAL32 3
#define VHDL_BIN 4
#define VHDL_BYTE 5
#define VHDL_WORD 6

/*
 * global variables
 */
FILE *cmdfp;				/* command input pointer */
char cmdbuff[CMD_LINE_MAX];
unsigned char eprom_buff[EPROM_MAX];	/* eprom buffer */
int eprom_top;				/* top of EPROM buffer */
int mod_flag;				/* buffer has been modified */
int auxflag;				/* Auxillary input file specified */
int count;
int checksum;
int offset;				/* Eprom Buffer memory offset */
int format_type;			/* load / save format type */
char *hex_str = "0123456789ABCDEF";

/*
 * Read a command line into the command buffer
 */
void read_command( char *cb )
{
	int ch;			/* character temp */
	int ci;			/* command index */

	ci = 0;
	printf( "\n>>> " );

	fflush( stdout );
	while( (ch = getc( cmdfp )) != EOF )
	{
		if( (ch == '\n') || (ch == 0x0d) )
		{
			cb[ci] = '\0';
			return;
		} else if( ch == 0x8 )
		{
			if( ci > 0 )
				ci--;
		} else
		{
			cb[ci] = ch;
			if( ci < CMD_LINE_MAX )
				ci++;
		}
		fflush( stdout );
	}
	if(( ch == EOF) && auxflag )
	{
		/* revert back to stdin if using an auxillay file
		 * We can assume that the auxillary command file
		 * has not been terminated with "quit" command
		 */
		fclose( cmdfp );
		cmdfp = stdin;
		auxflag = FALSE;
	}
}

/*
 * compare a string of specified length
 * return TRUE if a match
 * return FALSE if no match
 * ignore case
 */
int str_equal( char *s1, char *s2, int len )
{
	int i;

	i = 0;
	while( i<len )
	{
		if( toupper( s1[i] ) == toupper( s2[i] ) )
			i++;
		else return FALSE;
	}
	return TRUE;
}	

/*
 * extract a file name from the command line
 */
int get_filename( char *cb, char *fn )
{
	int i;

	i = 0;
	while( !isspace( cb[i] ) && (cb[i] !='\n') && (cb[i] != '\0'))
	{
		fn[i] = cb[i];
		i++;
	}
	fn[i] = '\0';
	if( i == 0 )
		return i;
	while( isspace( cb[i] ))
		i++;
	return i;
}

int to_hexadecimal( char c )
{
	int k;
	
	for( k=0; k<16; k++ )
	{
		if( toupper(c) == hex_str[k] )
			return k;
	}
	return -1;
}			

/*
 * extract an address from the command line
 * returns an offset to the end of the argument.
 */
int get_address( char *cb, int *addr )
{
	int i, j, k;

	j = 0;
	i = 0;

	while((k = to_hexadecimal(cb[i])) != -1)
	{
		i++;
		j = j *16 + k;
	}
	*addr = j;
	if( i == 0 )
		return i;
	while( isspace( cb[i]) )
		i++;
	return i;
}


/*
 * Motorola S1 format to Intel hex format
 * Usage
 * mot2hex <file_name>
 */

int gethex( FILE *fp_in )
{
    int hex;

    hex = fgetc( fp_in );
    return( to_hexadecimal( hex ) );
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
	    checksum = (checksum + byte) & 0xff;
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

int get6hex( FILE *fp_in )
{
    int bytehi, bytemid, bytelow, addr;

    bytehi = get2hex( fp_in );
    if( bytehi != -1 )
    {
	bytemid = get2hex( fp_in );
	if( bytemid != -1 )
	{
	    bytelow = get2hex( fp_in );
	    if( bytelow != -1 )
	    {
	        addr = (bytehi << 16) + (bytemid << 8) + bytelow;
	        return addr;
	    }
	}
    }
    return -1;
}    

long get8hex( FILE *fp_in )
{
    int wordhi, wordlow;
    long addr;

    wordhi = get4hex( fp_in );
    if( wordhi != -1 )
    {
            wordlow = get4hex( fp_in );
	    if( wordlow != -1 )
	    {
	        addr = ((long)wordhi << 16) + (long)wordlow;
	        return addr;
	    }
    }
    return -1;
}    


/*
 * Load Raw binary file
 */

void load_binary( char *fname_in, int load_addr )
{
	FILE *fp_in;
	int byte_count, addr;
	int data_byte;

	if( (fp_in = fopen( fname_in, "rb" )) == NULL )
	{
		printf( "\ncan't open %s for read", fname_in );
		return;
	} else
	{
		byte_count = 0;
		addr = load_addr - offset;
		printf( "\nReading file '%s' offset by  %x", fname_in, offset );
		while( (data_byte = fgetc( fp_in )) != EOF )
		{
			eprom_buff[(addr + byte_count) % EPROM_MAX ] = (unsigned char) data_byte;
			byte_count++;
		}

		fclose( fp_in );
		printf( "\nbuffer loaded from %x to %x", addr, addr+byte_count-1 );
		if( (addr + byte_count) > eprom_top )
			eprom_top = addr + byte_count;
	}
}


/*
 * load motorola formatted file
 */

void load_mot( char *fname_in )
{
	FILE *fp_in;
	int byte, addr, i;

	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
	    printf( "\nCan't open %s", fname_in );
	    return; 
	}

	byte = 0;
	addr = 0;

	while( byte != -1 )
	{
	    do {
		byte = fgetc( fp_in);
	    } while( (byte != 'S') && (byte != -1) );

	    byte = fgetc( fp_in );
	    checksum = 0;
	    if( (byte == '1') || (byte == '2') )
	    {
		count = get2hex( fp_in );
		if( byte == '1' )
		{
		    addr = get4hex( fp_in );
		    count -= 3;
		}
		else
		{
		    addr = get6hex( fp_in );
		    count -= 4;
		}
	        for( i=0; i<count; i++ )
	        {
		    byte = get2hex( fp_in );
		    eprom_buff[( addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
		}
	        byte = get2hex( fp_in);
		checksum = (~checksum) & 0xff;
		if( checksum != 0 )
	           printf( "\nchecksum error - read check = %02x", byte );
	     }
	}
	close( fp_in );
}


/*
 * load smal32 .o formatted file
 */

void load_smal32( char *fname_in )
{
	FILE *fp_in;
	int byte;
        long addr;
        int state;

	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
	    printf( "\nCan't open %s", fname_in );
	    return; 
	}

	byte = 0;
	addr = 0;
        state = 0;

	while( byte != -1 )
	{
		byte = fgetc( fp_in);
                switch( state )
                {
                case 0:
                  switch( byte )
                  {
                   case '.':
                     state = 1;
		     break;
                   case 'B':
		     state = 4;
		     break;
                   case 'H':
		     state = 5;
		     break;
                   case 'W':
		     state = 6;
		     break;
                   default:
		     state = 0;
		  }
                  break;
                case 1:
                  if( byte == '=' )
                     state = 2;
                  else
                     state = 0;
                  break;
		case 2:
                  if( byte == '#' )
                  {
		     addr = get8hex( fp_in );
	             printf( "\nload address = %08x", addr );
                  }
                  state = 0;
                  break;
                case 3:
                  break;
                case 4:
                  if( byte == '#' )
                  {
		    byte = get2hex( fp_in );
		    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
                  }
                  state = 0;
                  break;
                case 5:
                  if( byte == '#' )
                  {
		    byte = get2hex( fp_in );
		    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
		    byte = get2hex( fp_in );
		    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
                  }
                  state = 0;
                  break;
                case 6:
                  if( byte == '#' )
                  {
		    byte = get2hex( fp_in );
		    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
		    byte = get2hex( fp_in );
		    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
		    byte = get2hex( fp_in );
		    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
		    byte = get2hex( fp_in );
		    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
		    addr++;
		  }
                  state = 0;
                  break;
               default :
                 state = 0;
                 break;

	       }

	    if( addr >= EPROM_MAX  )
               byte = -1;
        }
	printf( "\nlast address = %08x", addr );
	close( fp_in );
}

/*
 * Load Intel hex file
 */

void load_intel( char *fname_in )
{
	FILE *fp_in;
	int byte, addr, i;

	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
	    printf( "\nCan't open %s for input", fname_in );
	    return;
	}

	byte = 0;
	while( byte != -1)
	{
		do {
			byte = fgetc( fp_in);
		} while( (byte != ':') && (byte != -1) );

		checksum = 0;
		count = get2hex( fp_in );	/* read byte count */
		addr = get4hex( fp_in );	/* read address */
		i = get2hex( fp_in );		/* read 00 to load a record */
		if( i == 0 )			/* read 01 to end record */
		{
	        	for( i=0; i<count; i++ )
	        	{
			    byte = get2hex( fp_in );
			    eprom_buff[ (addr - offset) % EPROM_MAX ] = (unsigned char)byte;
			    addr++;
			}
		        byte = get2hex( fp_in);
			if( checksum != 0 )
			{
				printf( "\nchecksum read %02x, expected %02x", byte, (checksum - byte)&0xff );
				byte = -1;
			} else if( addr >= EPROM_MAX )
			   byte = -1;
		} else if( i == 1 )
			byte = -1;
	}
	close( fp_in );
}


/*
 * load VHDL Binary file
 */

void load_vhdl_bin( char *fname_in, int start_addr )
{
	FILE *fp_in;
	int addr;
        int i;
        int byte, data;
        int state;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\nCan't open '%s' for read ", fname_in );
		return;
	}
	byte = 0;
	state = 0;
	addr = start_addr;
	while( byte != -1)
	{
		byte = fgetc( fp_in);
		switch( state )
		{
		case 0:
			if( byte == '"' )
			{
				data = 0;
				state = 1;
				i = 0;
			}
			break;
		case 1:
			data <<= 1;
			if( byte == '1' )
				data += 1;
			else
			{
				if( byte != '0' )
				{
					state = 0;
					break;
				}
			}
			i++;
			if( i == 8 )
				state = 2;
			break;
		case 2:
			if( byte == '"' )
			{
				eprom_buff[ addr % EPROM_MAX ] = (unsigned char)data;
				addr++;
			}
			state = 0;
			break;
		default:
			break;
		}
	}
	fclose( fp_in );
	printf( "\nLoaded " );
}

/*
 * load VHDL Hex file
 */

void load_vhdl_byte( char *fname_in, int start_addr )
{
	FILE *fp_in;
	int addr, i;
        int byte, data;
        int state;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\nCan't open '%s' for read ", fname_in );
		return;
	}
	byte = 0;
	state = 0;
	addr = start_addr;
	while( byte != -1)
	{
		byte = fgetc( fp_in);
		switch( state )
		{
		case 0:
			if( byte == 'x' )
			{
				state = 1;
			}
			break;
		case 1:
			if( byte == '"' )
			{
				data = 0;
				state = 2;
				i = 31;
			}
			break;
		case 2:
		        data = to_hexadecimal( (char)byte );
			if( data != -1 )
			{
				data <<= 4;
				state = 3;
			} else
				state = 0;
			break;
		case 3:
			if( to_hexadecimal( byte ) != -1 )
			{
		        	data += to_hexadecimal( (char)byte );
				eprom_buff[ (addr + i ) % EPROM_MAX ] = (unsigned char)data;
				if( i == 0 )
				{
					addr += 32;
					state = 0;
				} else
				{
					i--;
					state = 2;
				}
			} else
				state = 0;

			break;
		default:
			break;
		}
	}
	fclose( fp_in );
	printf( "\nLoaded " );
}


/*
 * load VHDL Word file
 */

void load_vhdl_word( char *fname_in, int start_addr )
{
	FILE *fp_in;
	int addr;
        int i;
        int byte, data;
        int state;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\nCan't open '%s' for read ", fname_in );
		return;
	}
	byte = 0;
	state = 0;
	addr = start_addr;
	while( byte != -1)
	{
		byte = fgetc( fp_in);
		switch( state )
		{
		case 0:
			if( byte == 'x' )
			{
				state = 1;
			}
			break;
		case 1:
			if( byte == '"' )
			{
				data = 0;
				state = 2;
				i = 30;
			}
			break;
		case 2:
		        data = to_hexadecimal( (char)byte );
			if( data != -1 )
			{
				data <<= 4;
				state = 3;
			} else
				state = 0;
			break;
		case 3:
			if( to_hexadecimal( (char)byte ) != -1 )
			{
		        	data += to_hexadecimal( (char)byte );
				eprom_buff[ (addr + i) % EPROM_MAX ] = (unsigned char)data;
				state = 4;
			} else
				state = 0;

			break;
		case 4:
		        data = to_hexadecimal( (char)byte );
			if( data != -1 )
			{
				data <<= 4;
				state = 5;
			} else
				state = 0;
			break;
		case 5:
			if( to_hexadecimal( (char)byte ) != -1 )
			{
		        	data += to_hexadecimal( (char)byte );
				eprom_buff[ (addr + i + 1) % EPROM_MAX ] = (unsigned char)data;
				if( i == 0 )
				{
					addr += 32;
					state = 0;
				} else
				{
					i -= 2;
					state = 2;
				}
			} else
				state = 0;

			break;
		default:
			break;
		}
	}
	fclose( fp_in );
	printf( "\nLoaded " );
}



int put2hex( FILE *fp, int h )
{
	int i, hex;
	
	hex = (h & 0xf0)>>4;
	i = fputc( (int)hex_str[hex], fp );
	hex = (h & 0xf);
	i = fputc( (int)hex_str[hex], fp );
	checksum = (checksum + h) & 0xff;
	return i;
}

int put4hex( FILE * fp, int h )
{
	int i;
	
	i = put2hex( fp, (h & 0xff00 )>>8 );
	i = put2hex( fp, (h & 0xff) );
	return i;
}



/*
 * save VHDL Binary file
 */

void save_vhdl_bin( char *fname_out, int start_addr, int end_addr )
{
	FILE *fp_out;
	int addr;
        int i;
        int byte;

	if( (fp_out = fopen( fname_out, "w" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}
	for( addr=start_addr; addr<=end_addr; addr++ )
	{
		putc( '"', fp_out );
		for(i=7; i>=0; i-- )
		{
			byte = (int)eprom_buff[(addr - offset) % EPROM_MAX];
                        if(( byte & (1<<i)) == 0 )
				putc( '0', fp_out );
			else
				putc( '1', fp_out );
		}
		putc( '"', fp_out );
		putc( ',', fp_out );
		putc( '\n', fp_out );
	}
	fclose( fp_out );
	printf( "\nwritten successfully" );
}


/*
 * save VHDL hexadecimal file
 */

void save_vhdl_byte( char *fname_out, int start_addr, int end_addr )
{
	FILE *fp_out;
	int addr;
        int i,j;
        int byte;

	if( (fp_out = fopen( fname_out, "w" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}
        j=0;
	for( addr=start_addr; addr<=end_addr; addr+=32 )
	{
 		fprintf( fp_out, "    INIT_%02x => x\"", j );
		for(i=31; i>=0; i-- )
		{
			byte = (int)eprom_buff[(addr - offset + i) % EPROM_MAX];
                        putc( hex_str[(byte >>4) & 0xf], fp_out ); 
                        putc( hex_str[byte & 0xf], fp_out ); 
		}
 		fprintf( fp_out, "\",\n" );
		j++;
	}
	fclose( fp_out );
	printf( "\nwritten successfully" );
}


/*
 * save VHDL hexadecimal Word file
 */

void save_vhdl_word( char *fname_out, int start_addr, int end_addr )
{
	FILE *fp_out;
	int addr;
        int i,j;
        int byte;

	if( (fp_out = fopen( fname_out, "w" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}
        j=0;
	for( addr=start_addr; addr<=end_addr; addr+=32 )
	{
                fprintf( fp_out, "  INIT_%02x => x\"" );
		for(i=30; i>=0; i-=2 )
		{
			/* MSByte first */
			byte = (int)eprom_buff[(addr - offset + i) % EPROM_MAX];
                        putc( hex_str[(byte >>4) & 0xf], fp_out ); 
                        putc( hex_str[byte & 0xf], fp_out ); 
			/* LSByte second */
			byte = (int)eprom_buff[(addr - offset + i + 1) % EPROM_MAX];
                        putc( hex_str[(byte >>4) & 0xf], fp_out ); 
                        putc( hex_str[byte & 0xf], fp_out ); 
		}
                fprintf( fp_out, "\",\n" );
		j++;
	}
	fclose( fp_out );
	printf( "\nwritten successfully" );
}


/*
 * save raw binary file
 */

void save_binary( char *fname_out, int start_addr, int end_addr )
{
	FILE *fp_out;
	int addr;

	if( (fp_out = fopen( fname_out, "wb" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}
	for( addr=start_addr; addr<=end_addr; addr++ )
	{
		putc( eprom_buff[(addr - offset) % EPROM_MAX ], fp_out );
	}
	fclose( fp_out );
	printf( "\nwritten successfully" );
}


/*
 * Save Motorola S1 file
 */

void save_mot( char *fname_out, int start_addr, int end_addr )
{
	FILE *fp_out;
	int addr, start;
	int i;
	int E_total;
	int data_byte;

	fp_out = fopen( fname_out, "w" );
	if( !fp_out )
	{
	    printf( "\nCan't open %s for output", fname_out );
	    return;
	}

	addr = start_addr;
	while( addr <= end_addr)
	{
		if( (end_addr - addr + 1) > 0 )
			E_total = 16;
		else
			E_total = end_addr - addr + 1;
		fputc( 'S', fp_out);  	/* record header preamble */
		fputc( '1', fp_out);  	/* record header preamble */
		checksum = 0;
		put2hex( fp_out, E_total+3);	/* byte count + 3 */
		put4hex( fp_out, addr);		/* Program Counter */
		for(i=0; i<E_total; i++)
		{
			data_byte = (int)eprom_buff[(addr - offset + i) % EPROM_MAX];
			put2hex( fp_out, data_byte);
		}
		checksum = ~checksum;	/* one's complement */
		put2hex( fp_out, (checksum & 0xff));
		fputc( '\n', fp_out);  	/* record header preamble */
		addr += E_total;
	}
	checksum = 0;
	fputc( 'S', fp_out);  		/* record header preamble */
	fputc( '9', fp_out);  		/* record header preamble */
	put2hex( fp_out, 3);		/* byte count + 3 */
	put4hex( fp_out, 0);		/* Program Counter */
	put2hex( fp_out, 0xfc );	/* byte count + 3 */
	fclose( fp_out );
}

/*
 * save Intelhex file
 */

void save_intel( char *fname_out, int start_addr, int end_addr )
{
	FILE *fp_out;
	int byte, addr, start, check, i;

	fp_out = fopen( fname_out, "w" );
	if( fp_out == NULL )
	{
		printf( "\nCan't open %s for output", fname_out );
		return;
	}

	addr = start_addr;
	start = addr;
	while( addr <= end_addr )
	{
		check = 0;
		count = 16;
	        fputc( ':', fp_out );
		put2hex( fp_out, count );
                check += count;
		put4hex( fp_out, addr );
                check += (addr & 0xff);
                check += ((addr>>8) & 0xff);
		put2hex( fp_out, 0 );
                check += 0;
	        for( i=0; i<count; i++ )
	        {
		    byte = (int)eprom_buff[ (addr - offset) % EPROM_MAX ];
		    if( addr == 0xfffe )
		        start = byte * 256 ;
	            if( addr == 0xffff )
		        start += byte;
		    put2hex( fp_out, byte );
		    check += byte;
		    addr ++;
		}
		check = (0-check) & 0xff;
	        put2hex( fp_out, check);
		fputc( '\n', fp_out );
	}
	fputc( ':', fp_out );
	put2hex( fp_out, 0 );
	put4hex( fp_out, start );
        put2hex( fp_out, 1 );
	close( fp_out );
}


/*
 * Compare Raw binary file
 */

void compare_binary( char *fname_in, int load_addr, int start_addr, int end_addr)
{
	FILE *fp_in;
	int addr;
	int data_byte;
	int diff_count;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\ncant open %s for read", fname_in );
		return;
	} else
	{
		diff_count = 0;
		addr = load_addr;
		while( (data_byte = fgetc( fp_in )) != EOF )
		{
			if( (addr >= start_addr) && (addr <= end_addr) )
			{
				if( (unsigned char)data_byte != eprom_buff[(addr -offset) % EPROM_MAX] )
				{
					printf( "\n%04x %02x %02x", addr, eprom_buff[(addr-offset) % EPROM_MAX ], data_byte );
					diff_count++;
				}
			}
			addr++;
		}
		printf( "\nLast compared address %x ", addr-1 );
		printf( "\nThere were %d differences ", diff_count );
		fclose( fp_in );
	}
}


/*
 * Compare motorola formatted file
 */

void compare_mot( char *fname_in, int start_addr, int end_addr )
{
	FILE *fp_in;
	int byte, addr, start, check, i;
	int diff_count;

	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
	    printf( "\nCan't open %s", fname_in );
	    return; 
	}

	byte = 0;
	addr = start;
	diff_count = 0;

	while( byte != -1 )
	{
	    do {
		byte = fgetc( fp_in);
	    } while( (byte != 'S') && (byte != -1) );

	    byte = fgetc( fp_in );
	    checksum = 0;
	    if( (byte == '1') || (byte == '2') )
	    {
		count = get2hex( fp_in );
		if( byte == '1' )
		    addr = get4hex( fp_in );
		else
		    addr = get6hex( fp_in );
		count -= 3;
	        for( i=0; i<count; i++ )
	        {
		    	byte = get2hex( fp_in );
			if( (addr >= start_addr) && (addr <= end_addr ))
			{
				if( (unsigned char)byte != eprom_buff[(addr - offset) % EPROM_MAX ] )
				{
					printf( "\n%04x %02x %02x", addr, eprom_buff[(addr-offset) % EPROM_MAX ], byte );
					diff_count++;
				}
			}
			addr++;
		}
	        byte = get2hex( fp_in);
	        if( addr >= EPROM_MAX )
		   byte = -1;
	     }
	}
	close( fp_in );
	printf( "\nLast compared address %x ", addr - 1 );
	printf( "\nThere were %d differences ", diff_count );
}

/*
 * Load Intel hex file
 */

void compare_intel( char *fname_in, int start_addr, int end_addr )
{
	FILE *fp_in;
	int byte, addr, start, i;
	int diff_count;

	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
	    printf( "\nCan't open %s for input", fname_in );
	    return;
	}

	byte = 0;
	diff_count = 0;
	while( byte != -1)
	{
		do {
			byte = fgetc( fp_in);
		} while( (byte != ':') && (byte != -1) );

		checksum = 0;
		count = get2hex( fp_in );
		addr = get4hex( fp_in );
		i = get2hex( fp_in );		/* read 00 to load a record */
		if( i == 0 )			/* read 01 to end record */
		{
	        	for( i=0; i<count; i++ )
		        {
			    	byte = get2hex( fp_in );
				if( (addr >= start_addr) && (addr <= end_addr ))
				{
					if( (unsigned char)byte != eprom_buff[(addr - offset) % EPROM_MAX] )
					{
						printf( "\n%04x %02x %02x", addr, eprom_buff[(addr-offset) % EPROM_MAX], byte );
						diff_count++;
					}
				}
			    	addr++;
			}
		        byte = get2hex( fp_in);
			if( checksum != 0 )
			{
				printf( "\nchecksum error detected, read %02x, expected %02x", byte, ((checksum - byte) & 0xff) );
				byte = -1;
			} else if( addr >= EPROM_MAX )
			   byte = -1;
		} else if( i == 1 )		/* test for end of record */
			byte = -1;
	}
	close( fp_in );
	printf( "\nLast compared address %x ", addr - 1 );
	printf( "\nThere were %d differences ", diff_count );
}


/*
 * help menu
 */
void display_help()
{
	printf( "\n*** Help Menu ***\n\n");
	printf( "All addresses are in hexadecimal\n" );
	printf( "H/?/help                             - This help menu\n" );
	printf( "T/type - Set Load & Save format\n" );
        printf( "     <I/intel>\n");
        printf( "     <M/motorola>\n");
        printf( "     <B/binary>\n" );
        printf( "     <O/Smal32>\n" );
        printf( "     <V/vhdl> VHDL Binary String \n" );
        printf( "     <H/hex> VHDL Hexadecimal Byte String\n" );
        printf( "     <W/word> VHDL Hexadecimal Word String\n" );
	printf( "O/offset <address>                   - Set Eprom buffer offset\n" );
	printf( "L/load <filename> [<load_addr>]      - Loads file into buffer\n" );
	printf( "S/save <filename> <start-address> <end-address> - Saves buffer to file\n" );
	printf( "C/compare <filename> [<load_addr>]   - compare file to buffer\n" );
/*
	printf( "R/read <address>                     - reads buffer data\n" );
*/
	printf( "W/write <address> <data> .... <data> - Write data to buffer\n" );
	printf( "F/fill <start-address> <end-address> <data> - fill buffer with data\n");
	printf( "M/move <start-address> <end-address> <dest-address> - block move\n" );
	printf( "D/dump <start_address> <end-address> - Hex buffer dump\n");
	printf( "E/echo <string>                      - echo string to console\n" );
	printf( "X/excecute <command-file>            - execute a command script\n");
	printf( "Q/quit                               - quit program\n" );
}

/*
 * Parse command
 * Return FALSE to exit
 */
int parse_command( char *cmdline )
{
	int lineptr;
	char *cmdptr;
	int arglen;
	FILE *fp;
	char filename[64];
	int start_addr;
	int end_addr;
	int dest_addr;
	int load_addr;
	int byte_count;
	int data_byte;
	int ch;
	int len;

	lineptr = 0;
	start_addr = 0;
	end_addr = 0;

	/* skip leading white spaces */
	while( isspace( cmdline[lineptr] ))
		 lineptr++;
	/* point to the start of the command argument & extract command */
	cmdptr = &cmdline[lineptr];
	len = 0;
	while( isalpha( cmdline[lineptr] ) ||
	      (cmdline[lineptr] == '?'))
	{
		lineptr++;
		len++;
	}
	/* skip trailing white spaces */
	while( isspace( cmdline[lineptr] ) )
		lineptr++;
    if( len > 0 )
    {

	if( str_equal( cmdptr, "T", len ) ||
	    str_equal( cmdptr, "type", len ) )
	{
		/***********************************************************
		 *
		 * Specify file I/O format type
		 */
		cmdptr = &cmdline[lineptr];
		len = 0;
		while( isalpha( cmdline[lineptr] ))
		{
			lineptr++;
			len++;
		}
		if( len != 0 )
		{
			if( str_equal( cmdptr, "B", len ) ) 
				format_type = BINARY; 
			else if( str_equal( cmdptr, "binary", len ) )
				format_type = BINARY; 
			else if( str_equal( cmdptr, "M", len ) ) 
				format_type = MOTOROLA; 
			else if( str_equal( cmdptr, "motorola", len ) ) 
				format_type = MOTOROLA; 
			else if( str_equal( cmdptr, "I", len ) )
				format_type = INTEL; 
			else if( str_equal( cmdptr, "intel", len ) ) 
				format_type = INTEL;
			else if( str_equal( cmdptr, "O", len ) )
				format_type = SMAL32; 
			else if( str_equal( cmdptr, "smal", len ) ) 
				format_type = SMAL32;
			else if( str_equal( cmdptr, "V", len ) )
				format_type = VHDL_BIN; 
			else if( str_equal( cmdptr, "vhdl", len ) ) 
				format_type = VHDL_BIN;
			else if( str_equal( cmdptr, "H", len ) )
				format_type = VHDL_BYTE; 
			else if( str_equal( cmdptr, "hex", len ) ) 
				format_type = VHDL_BYTE;
			else if( str_equal( cmdptr, "W", len ) )
				format_type = VHDL_WORD; 
			else if( str_equal( cmdptr, "word", len ) ) 
				format_type = VHDL_WORD;
			else printf( "\nUnrecognised file format" );
		}
		printf( "\nFile format type is : " );
		switch( format_type ) {
		case BINARY:
			printf( "Raw Binary" );
			break;
		case MOTOROLA:
			printf( "Motorola S1" );
			break;
		case INTEL:
			printf( "Intel Hex" );
			break;
		case SMAL32:
			printf( "Smal32 .o" );
			break;
		case VHDL_BIN:
			printf( "vhdl binary" );
			break;
		case VHDL_BYTE:
			printf( "vhdl hexadecimal byte" );
			break;
		case VHDL_WORD:
			printf( "vhdl hexadecimal word" );
			break;
		default:
			printf( "Bad format - should not get here" );
			break;
		}
		return TRUE;
	}
	else if( str_equal( cmdptr, "L", len ) ||
	         str_equal( cmdptr, "load", len ) )
	{
		/***********************************************************
		 *
		 * Load file into buffer
		 */
		arglen = get_filename( &cmdline[lineptr], &filename[0] );
		if( arglen == 0 )
		{
			printf( "\nFile name expected " );
			return TRUE;
		}
		lineptr += arglen;
		arglen = get_address( &cmdline[lineptr], &load_addr );
		if( arglen == 0 )
		{
			/* default binary load address = start of eprom */
			load_addr =  offset;

		}
		if( (load_addr < offset) || (load_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nLoad address out of range" );
			return TRUE;
		}
		switch( format_type ) {
		case BINARY:
			printf( "\nLoading binary file" );
			load_binary( filename, load_addr );
			break;
		case MOTOROLA:
			printf( "\nLoading Motorola S1 file" );
			load_mot( filename );
			break;
		case INTEL:
			printf( "\nLoading intel hex file" );
			load_intel( filename );
			break;
		case SMAL32:
			printf( "\nLoading Smal32 file" );
			load_smal32( filename );
			break;
		case VHDL_BIN:
			printf( "\nLoading VHDL binary format" );
			load_vhdl_bin( filename, load_addr );
			break;
		case VHDL_BYTE:
			printf( "\nLoading VHDL hexadecimal byte format" );
			load_vhdl_byte( filename, load_addr );
			break;
		case VHDL_WORD:
			printf( "\nLoading VHDL hexadecimal word format" );
			load_vhdl_word( filename, load_addr );
			break;
		default:
			printf( "\nBad format - no load" );
			break;
		}
		mod_flag = TRUE;
		return TRUE;
	} else if( str_equal( cmdptr, "S", len ) ||
		   str_equal( cmdptr, "save", len ))
	{
		/***********************************************************
		 *
		 * Save buffer to file
		 *
		 ***********************************************************/
		arglen = get_filename( &cmdline[lineptr], &filename[0] );
		if( arglen == 0 )
		{
			printf( "\nFile name expected " );
			return TRUE;
		}
		lineptr += arglen;
		arglen = get_address( &cmdline[lineptr], &start_addr );
		if( arglen == 0 )
		{
			/* this could default to 0 */
			printf( "\nStart address expected " );
			return TRUE;
		}
		lineptr += arglen;
		if( (arglen = get_address( &cmdline[lineptr], &end_addr )) == 0)
		{
			/* this could default to eprom_top */
			printf( "\nEnd address expected " );
			return TRUE;
		}
		if( (start_addr < offset) || (start_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nStart address out of range" );
			return TRUE;
		}
		if( (end_addr < offset) || (end_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nEnd address out of range" );
			return TRUE;
		}
		printf( "\nSaving buffer %x to %x to file %s ", start_addr, end_addr, filename );
		switch( format_type ) {
		case BINARY:
			printf( "\nBinary format" );
			save_binary( filename, start_addr, end_addr );
			break;
		case MOTOROLA:
			printf( "\nMotorola S1 format" );
			save_mot( filename, start_addr, end_addr );
			break;
		case INTEL:
			printf( "\nIntel hex format" );
			save_intel( filename, start_addr, end_addr );
			break;
		case VHDL_BIN:
			printf( "\nVHDL binary format" );
			save_vhdl_bin( filename, start_addr, end_addr );
			break;
		case VHDL_BYTE:
			printf( "\nVHDL hexadecimal byte format" );
			save_vhdl_byte( filename, start_addr, end_addr );
			break;
		case VHDL_WORD:
			printf( "\nVHDL hexadecimal word format" );
			save_vhdl_word( filename, start_addr, end_addr );
			break;
		default:
			printf( "\nBad format - no save performed" );
			break;
		}
		return TRUE;
	} else if( str_equal( cmdptr, "D", len ) ||
		   str_equal( cmdptr, "dump", len ))
	{
		/***********************************************************
		 *
		 * dump buffer to the display
		 */
		if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
		{
			printf( "\nStart address expected " );
			return TRUE;
		}
		lineptr += arglen;
		if( (arglen = get_address( &cmdline[lineptr], &end_addr )) == 0)
		{
			printf( "\nEnd address expected " );
			return TRUE;
		}
		if( (start_addr < offset) || (start_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nStart address out of range" );
			return TRUE;
		}
		if( (end_addr < offset) || (end_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nEnd address out of range" );
			return TRUE;
		}
		load_addr = 0;
		printf( "\n Memory Dump form %x to %x ", start_addr, end_addr );
		while( (start_addr + load_addr) <= end_addr )
		{
			printf( "\n  %04x - ", start_addr + load_addr );
			for( dest_addr = 0; dest_addr < 16; dest_addr++ )
			{
				if( (start_addr + load_addr + dest_addr) <= end_addr )
					printf( "%02x ", eprom_buff[(start_addr+load_addr+dest_addr) % EPROM_MAX] );
				else
					printf( "   " );
			}
			printf( "  " );
			for( dest_addr = 0; dest_addr < 16; dest_addr++ )
			{
				if( (start_addr + load_addr + dest_addr) <= end_addr )
				{
					ch = eprom_buff[(start_addr+load_addr+dest_addr) % EPROM_MAX];
					if( (ch > 0x20) && (ch < 0x7f) )
						printf( "%c", ch );
					else
						printf( "." );
				} else
					printf( " " );
			}
			load_addr += dest_addr;
		}
		return TRUE;
	} else if( str_equal( cmdptr, "C", len ) ||
		   str_equal( cmdptr, "compare", len ))
	{
		/***********************************************************
		 *
		 * compare file with buffer
		 *
		 ***********************************************************/
		if( (arglen = get_filename( &cmdline[lineptr], &filename[0] )) == 0 )
		{
			printf( "\nFile name expected " );
			return TRUE;
		}
		lineptr += arglen;
		/* start/load address is optional - default to start of eprom */
		if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
		{
			start_addr = offset;
		}
		lineptr += arglen;

		/* the end/start address is optional */
		if( (arglen = get_address( &cmdline[lineptr], &end_addr )) == 0 )
		{
			end_addr = EPROM_MAX + offset - 1;
		}
		lineptr += arglen;

		/* the end/start address is optional */
		if( (arglen = get_address( &cmdline[lineptr], &load_addr )) == 0 )
		{
			load_addr = EPROM_MAX + offset - 1;
		}

		/* check for valid address range */
		if( (start_addr < offset) || (start_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nStart address out of range" );
			return TRUE;
		}
		if( (end_addr < offset) || (end_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nEnd address out of range" );
			return TRUE;
		}
		if( (load_addr < offset) || (load_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nLoad address out of range" );
			return TRUE;
		}
		printf( "\nComparing buffer to file %s", filename );
		switch( format_type ) {
		case BINARY:
			printf( "\nBinary format" );
			dest_addr = load_addr;
			load_addr = start_addr;
			start_addr = end_addr;
			end_addr = dest_addr;
			if( start_addr == (EPROM_MAX + offset - 1) )
				start_addr = offset;
			compare_binary( filename, load_addr, start_addr, end_addr );
			break;
		case MOTOROLA:
			printf( "\nMotorola S1 format" );
			compare_mot( filename, start_addr, end_addr );
			break;
		case INTEL:
			printf( "\nIntel hex format" );
			compare_intel( filename, start_addr, end_addr );
			break;
		default:
			printf( "\nBad format - no save performed" );
			break;
		}
		return TRUE;
	} else if( str_equal( cmdptr, "M", len ) ||
		   str_equal( cmdptr, "move", len ))
	{
		/***********************************************************
		 *
		 * memory block move
		 */
		if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
		{
			printf( "\nStart address expected " );
			return TRUE;
		}
		lineptr += arglen;
		if( (arglen = get_address( &cmdline[lineptr], &end_addr )) == 0)
		{
			printf( "\nEnd address expected " );
			return TRUE;
		}
		lineptr += arglen;
		if( (arglen = get_address( &cmdline[lineptr], &dest_addr )) == 0)
		{
			printf( "\nDestination address expected " );
			return TRUE;
		}
		/* check that the range is around the right way */
		if( end_addr < start_addr )
		{
			load_addr = start_addr;
			start_addr = end_addr;
			end_addr = load_addr;
		}
		/* check for valid address range */
		if( (start_addr < offset) || (start_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nStart address out of range" );
			return TRUE;
		}
		if( (end_addr < offset) || (end_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nEnd address out of range" );
			return TRUE;
		}
		if( (dest_addr < offset) || (dest_addr >= (offset + EPROM_MAX)))

		{
			printf( "\nDestination address out of range" );
			return TRUE;
		}
		byte_count = end_addr - start_addr;
		printf( "\nTransfering memory block %04x thru %04x to %04x", start_addr, end_addr, dest_addr );
		if( start_addr > dest_addr  )
		{
			for( load_addr=0; load_addr<=byte_count; load_addr++ )
				eprom_buff[(dest_addr-offset+load_addr) % EPROM_MAX] = eprom_buff[(start_addr-offset+load_addr) % EPROM_MAX];
		} else
		{
			for( load_addr=byte_count; load_addr>=0; load_addr-- )
				eprom_buff[(dest_addr-offset+load_addr) % EPROM_MAX] = eprom_buff[(start_addr-offset+load_addr) % EPROM_MAX];
		}	
		printf( "\nDone" );
		mod_flag = TRUE;
		return TRUE;
	} else if( str_equal( cmdptr, "F", len ) ||
		   str_equal( cmdptr, "fill", len ))
	{
		/***********************************************************
		 *
		 * fill memory with data
		 */
		if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
		{
			printf( "\nStart address expected " );
			return TRUE;
		}
		lineptr += arglen;
		if( (arglen = get_address( &cmdline[lineptr], &end_addr )) == 0)
		{
			printf( "\nEnd address expected " );
			return TRUE;
		}
		lineptr += arglen;
		if( (arglen = get_address( &cmdline[lineptr], &data_byte )) == 0)
		{
			printf( "\nFill data byte expected " );
			return TRUE;
		}
		if( (start_addr < offset) || (start_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nStart address out of range" );
			return TRUE;
		}
		if( (end_addr < offset) || (end_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nEnd address out of range" );
			return TRUE;
		}
		printf( "\nFilling address %x thru %x with %x ", start_addr, end_addr, data_byte );
		for( dest_addr=start_addr; dest_addr<=end_addr; dest_addr++ )
			eprom_buff[(dest_addr-offset) % EPROM_MAX] = (unsigned char)data_byte;
		printf( "\nDone" );
		mod_flag = TRUE;
		return TRUE;
	} else if( str_equal( cmdptr, "W", len ) ||
		   str_equal( cmdptr, "write", len ))

	{
		/***********************************************************
		 *
		 * Write to buffer location
		 */
		if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
		{
			printf( "\nStart address expected " );
			return TRUE;
		}
		lineptr += arglen;
		/* check for valid address range */
		if( (start_addr < offset) || (start_addr >= (offset + EPROM_MAX)))
		{
			printf( "\nStart address out of range" );
			return TRUE;
		}
		printf( "\nSetting buffer loaction %x", start_addr );
		while( (arglen = get_address( &cmdline[lineptr], &data_byte )) != 0)
		{
			eprom_buff[(start_addr-offset) % EPROM_MAX] = (unsigned char)data_byte;
			printf( "\n %04x = %02x ", start_addr, data_byte );
			start_addr++;
			lineptr += arglen;
		}
		mod_flag = TRUE;
		return TRUE;
	} else if( str_equal( cmdptr, "O", len ) ||
		   str_equal( cmdptr, "offset", len ))
	{
		/***********************************************************
		 *
		 * Set Eprom base address offset
		 */
		if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
		{
			/* No argument, display offset */
			printf( "\nBase address offset = %04x", offset );
			return TRUE;
		}
		offset = start_addr;
		mod_flag = TRUE;
		return TRUE;
	} else if( str_equal( cmdptr, "H", len ) ||
		   str_equal( cmdptr, "?", len ) ||
		   str_equal( cmdptr, "help", len ))
	{
		/***********************************************************
		 *
		 * Display help menu
		 */
		display_help();
		return TRUE;
	} else if( str_equal( cmdptr, "Q", len ) ||
		   str_equal( cmdptr, "quit", len ))
	{
		/***********************************************************
		 *
		 * Quit program - return FALSE
		 */
		printf( "\nQuitting program" );
		return FALSE;
	} else if( str_equal( cmdptr, "E", len ) ||
		   str_equal( cmdptr, "echo", len ))
	{
		/***********************************************************
		 *
		 * Echo string to the console
		 */
		printf( "\n" );
		while( (cmdline[lineptr] != '\0') && (cmdline[lineptr] != '\n'))
			printf( "%c", cmdline[lineptr++] );
		printf( "\n" );
		return TRUE;

	} else if( str_equal( cmdptr, "X", len ) ||
		   str_equal( cmdptr, "execute", len ))
	{
		/***********************************************************
		 *
		 * Execute command script
		 * We can onle do this if we are using stdin
		 */
		if( cmdfp != stdin )
		{
			printf( "\nWe cannot nest redirected input " );
			return TRUE;
		}

		arglen = get_filename( &cmdline[lineptr], &filename[0] );
		if( arglen == 0 )
		{
			printf( "\nFile name expected " );
			return TRUE;
		}

		if( (cmdfp = fopen( &filename[0], "r" )) == NULL )
		{
			printf( "\ncan't open auxillary input file '%s'", &filename[0] );
			/* we had better revert to the original command file */
			cmdfp = stdin;	
			return TRUE;
		}
		/* every thing is ok, input is re-directed */
		auxflag = TRUE;
		return TRUE;
	}
    }
	/***********************************************************
	 *
	 * Command not understood
	 */
	printf( "\nUnrecognised command" );
	return TRUE;
}

/*
 * epedit main program
 */
main(int argc, char **argv)
{
	
	auxflag = FALSE;
	offset = 0;
	format_type = BINARY;

	if(argc >= 2)
	{
		/* check for auxillary command file */
		if((cmdfp = fopen( argv[1], "r" )) == NULL)
		{
			printf(" can't open input on %s\n", argv[1] );
			printf("Usage: %s [command-file]\n",argv[0]);
			exit(1);
		}
		auxflag = TRUE;
	} else 
	{
		/* no auxillary command file specified, use stdin */
		cmdfp = stdin;
	}

	eprom_top = 0;
	printf( "*** EPROM Editor ***\n" );
	printf( "type H/?/help for commands\n" );
	do {
		read_command( cmdbuff );
	} while( parse_command( cmdbuff ) );
	printf( "\n*** exit epedit ***\n" );
	exit( 0 );
}

