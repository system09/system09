/*
 * epedit.c
 *
 * Binary file editer program
 *
 * Author - John Kent
 * 
 */
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <malloc.h>

extern main( int argc, char *argv[]);

/*
 * equates
 */
#define FALSE 0
#define TRUE !FALSE

/*
 * Commnad line buffer
 */
#define CMD_LINE_MAX 80
char cmdbuff[CMD_LINE_MAX];

/*
 * Load / Save format types 
 */
#define BINARY 0
#define MOTOROLA 1
#define INTEL 2
#define SMAL32 3
#define VHDL_BIN 4
#define VHDL_BYTE 5
#define VHDL_WORD 6

int format_type;			/* load / save format type */

/*
 * Motorola Module record
 */
#define MNAME_SIZE 21
#define MVER_SIZE 3
#define MREV_SIZE 3
#define MDESC_SIZE 37

char module_name[MNAME_SIZE];
char module_ver[MVER_SIZE];
char module_rev[MREV_SIZE];
char module_desc[MDESC_SIZE];

/*
 * global variables
 */
FILE *cmdfp;				/* command input pointer */
int auxflag;				/* Auxillary input file specified */

/* minumum / maximum buffer size */
#define EPROM_MIN 0x00000800
#define EPROM_MAX 0x01000000
unsigned char *eprom_buff;	/* EPROM Buffer Pointer */
unsigned char *new_buff;	/* New Buffer Pointer */
unsigned long eprom_size;	/* EPROM Buffer Size */
unsigned long eprom_top;	/* top of EPROM buffer */
unsigned long eprom_bot;	/* bottom of EPROM buffer */
int mod_flag;				/* buffer has been modified */

unsigned long count;		/* Record Byte Count */
unsigned long addr;			/* buffer load address */
unsigned long offset;		/* buffer load offset */
unsigned long checksum;		/* Record Checksum */
unsigned long rec_count;	/* Record Count */
unsigned long trans_addr;	/* Transfer Address */

char *hex_str = "0123456789ABCDEF";
char str_buff[40];

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
		} 
		else if( ch == 0x8 )
		{
			if( ci > 0 )
			{
				ci--;
			}
		} 
		else
		{
			cb[ci] = ch;
			if( ci < CMD_LINE_MAX )
			{
				ci++;
			}
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
	int equal;

	equal = TRUE;
	for( i=0; i<len; i++ )
	{
		if( toupper( s1[i] ) != toupper( s2[i] ) )
		{
			equal = FALSE;
		}
	}
	return equal;
}	

/*
 * extract a file name from the command line
 */
int get_string( char *cb, char *st )
{
	int i;

	i = 0;
	while( !isspace( cb[i] ) && (cb[i] !='\n') && (cb[i] != '\0'))
	{
		st[i] = cb[i];
		i++;
	}
	st[i] = '\0';
	if( i != 0 )
	{
		while( isspace( cb[i] ))
		{
			i++;
		}
	}
	return i;
}

/*
 * convert a hexadecimal character to an integer
 */
int hexadec2int( char c )
{
	int k;

	for( k=0; k<16; k++)
	{
		if (toupper(c) == hex_str[k])
		{
			return k;
		}
	}
	return -1;
}			

/*
 * extract an address from the command line
 * returns an offset to the end of the argument.
 */
int get_address( char *cb, unsigned long *a )
{
	int i, nybble;

	*a = 0;
	i = 0;
	while((nybble = hexadec2int(cb[i])) != -1)
	{
		i++;
		*a = ((*a)<<4) + (unsigned long)nybble;
	}
	if( i != 0 )
	{
		while( isspace( cb[i]) )
		{
			i++;
		}
	}
	return i;
}


/*
 * Motorola S1 format to Intel hex format
 * Usage
 * mot2hex <file_name>
 */

int get1hex( FILE *fp_in, unsigned long *nybble )
{
	int hex;

	*nybble = 0;
	hex = fgetc( fp_in );
	if( hex != -1 )
	{
		*nybble = hexadec2int( (char)hex );
	}
	return hex;
}

int get2hex( FILE *fp_in, unsigned long *b )
{
	int err;
    unsigned long nybble;

	*b = 0;
    err = get1hex( fp_in, &nybble );
    if( err != -1 )
    {
		*b = nybble;
		err = get1hex( fp_in, &nybble );
		if( err != -1 )
		{
			*b = ((*b)<<4) + nybble;
			checksum = (checksum + *b) & 0x000000ff;
			count--;
		}
    }
    return err;
}

int get4hex( FILE *fp_in, unsigned long *word16 )
{
	int err;
	unsigned long byte;

	*word16 = 0;
    err = get2hex( fp_in, &byte );
    if( err != -1 )
    {
		*word16 = byte;
		err = get2hex( fp_in, &byte );
		if( err != -1 )
		{
			*word16 = ((*word16)<<8) + byte;
		}
    }
    return err;
}    

int get6hex( FILE *fp_in, unsigned long *word24 )
{
	int err;
	unsigned long byte;

	*word24 = 0;
    err = get2hex( fp_in, &byte );
    if( err != -1 )
    {
		*word24 = byte;
		err = get2hex( fp_in, &byte );
		if( err != -1 )
		{
			*word24 = ((*word24)<<8) + byte;
			err = get2hex( fp_in, &byte );
			if( err != -1 )
			{
				*word24 = ((*word24)<<8) + byte;
			}
	    }
    }
    return err;
}    

int get8hex( FILE *fp_in, unsigned long *word32 )
{
    int err;
    unsigned long word;

	*word32 = 0;
    err = get4hex( fp_in, &word );
    if( err != -1 )
    {
		*word32 = word;
		err = get4hex( fp_in, &word );
	    if( err != -1 )
	    {
	        *word32 = ((*word32) << 16) + word;
	    }
    }
    return err;
}    

/*
 * read byte into eprom buffer
 * return -1 if error
 */

int read_byte( FILE *fp_in )
{
	int err;
	unsigned long byte;

	if( (err = get2hex( fp_in, &byte )) != -1)
	{
		if( (addr - offset) < eprom_size  )
		{
			if( (addr - offset) > eprom_top )
			{
				eprom_top = addr - offset;
			}
			eprom_buff[ (addr - offset) % eprom_size ] = (unsigned char)byte;
			addr++;
		}
		else
		{
			err = -1;
		}
	}
	return err;
}

int read_addr16( FILE *fp_in )
{
	int err;

	if( (err = get4hex( fp_in, &addr )) != -1 )
	{
		if( (addr - offset) < eprom_size )
		{
			if( (addr - offset) < eprom_bot )
			{
				eprom_bot = addr-offset;
			}
		}
		else
		{
			err = -1;
		}
	}
	return err;
}


int read_addr24( FILE *fp_in )
{
	int err;

	if( (err = get6hex( fp_in, &addr )) != -1 )
	{
		if( (addr - offset) < eprom_size )
		{
			if( (addr - offset) < eprom_bot )
			{
				eprom_bot = addr-offset;
			}
		}
		else
		{
			err = -1;
		}
	}
	return err;
}


int read_addr32( FILE *fp_in )
{
	int err;

	if( (err = get8hex( fp_in, &addr )) != -1 )
	{
		if( (addr - offset) < eprom_size )
		{
			if( (addr - offset) < eprom_bot )
			{
				eprom_bot = addr-offset;
			}
		}
		else
		{
			err = -1;
		}
	}
	return err;
}

/*
 * Load Raw binary file
 */

void load_binary( char *fname_in, unsigned long load_addr )
{
	FILE *fp_in;
	int err;
	int byte;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\ncan't open %s for read", fname_in );
		return;
	} 
	else
	{
		err = 0;
		addr = load_addr;
		if( (addr - offset) < eprom_bot )
		{
			eprom_bot = (addr - offset);
		}
		printf( "\nReading file '%s' offset by  %x", fname_in, offset );
		while( ((byte = fgetc( fp_in )) != EOF) && (err != -1) )
		{
			if( (addr - offset) < eprom_size  )
			{
				if( (addr - offset) > eprom_top )
				{
					eprom_top = addr - offset;
				}
				eprom_buff[ (addr - offset) % eprom_size ] = (unsigned char)byte;
				addr++;
			}
			else
			{
				err = -1;
			}
		}

		printf( "\nbuffer loaded from %x to %x", addr, addr+count-1 );
	}
	fclose( fp_in );
}


/*
 * load motorola formatted file
 */

void load_mot( char *fname_in )
{
	FILE *fp_in;
	int err;
	int sor;
	int type;
	int i;
	unsigned long byte;

	if( (fp_in = fopen( fname_in, "r")) == NULL )
	{
	    printf( "\nCan't open %s", fname_in );
	    return; 
	}

	err = 0;
	rec_count = 0;
	byte = 0;
	addr = 0;

	while( err != -1 )
	{
		/* Find start of record "S" */
	    do {
			sor = fgetc( fp_in);
	    } while( (sor != 'S') && (sor != -1) );

		/* Read Record Type */
	    type = fgetc( fp_in );
	    checksum = 0;

		/* Decode Record Type */
	    if( type != -1 )
	    {
			err = get2hex( fp_in, &count );
			switch( type )
			{
			case '0':
				/* Header information */
				for(i=0; i<MNAME_SIZE; i++)
				{
					module_name[i]='\n';
				}
				for(i=0; i<MVER_SIZE; i++)
				{
					module_ver[i]='\n';
				}
				for(i=0; i<MREV_SIZE; i++)
				{
					module_rev[i]='\n';
				}
				for(i=0; i<MDESC_SIZE; i++)
				{
					module_desc[i]='\n';
				}
				/* address = $0000 */
				if( (err = read_addr16( fp_in )) == -1)
				{
					break;
				}
				i=0;
				for( i=0; (i<60) && (err != -1) && (count>0); i++ )
				{
					/* module name char[20] */
					if( i < 20)
					{
						if( (err =get2hex( fp_in, &byte)) == -1)
						{
							break;
						}
						module_name[i] = (char)byte;
					}
					else
					{
						/* module version char[2] */
						if( i < 22 )
						{
							if( (err =get2hex( fp_in, &byte)) == -1)
							{
								break;
							}
							module_ver[i-20] = (char)byte;
						}
						else
						{
							/* module revison char[2] */
							if( i < 24 )
							{
								if( (err =get2hex( fp_in, &byte)) == -1)
								{
									break;
								}
								module_rev[i-22] = (char)byte;
							}
							else
							{
								/* module description char[0-36] */
								if( (err =get2hex( fp_in, &byte)) == -1)
								{
									break;
								}
								module_desc[i-24] = (char)byte;
							}
						}
					}
				}
				break;

			case '1':
				/* 16 bit address record */
				err = read_addr16( fp_in );
				while( (count > 0) && (err != -1) )
				{
					err = read_byte( fp_in );
				}
				break;

			case '2':
				/* 24 bit address record */
				err = read_addr24( fp_in );
				while( (count > 0) && (err != -1) )
				{
					err = read_byte( fp_in );
				}
				break;

			case '3':
				/* 32 bit address record */
				err = read_addr32( fp_in );
				while( (count > 0) && (err != -1) )
				{
					err = read_byte( fp_in );
				}
				break;

			case '5':
				/* 2 byte sum of records (S1, S2, S3)*/
				err = get4hex( fp_in, &rec_count );
				break;

			case '7':
				/* Transfer address (S3) */
				err = get8hex( fp_in, &trans_addr );
				break;

			case '8':
				/* Transfer address (S2) */
				err = get6hex( fp_in, &trans_addr );
				break;

			case '9':
				err = get8hex( fp_in, &trans_addr );
				break;
				/* Transfer address (S1) */

			}

			err = get2hex( fp_in, &byte);
			checksum = (~checksum) & 0x000000ff;
			if( checksum != 0 )
			{
				printf( "\nchecksum error - read check = %02x", byte );
				err = -1;
			}
			else
			{
				rec_count++;
			}
		}
	}
	fclose( fp_in );
}

/*
 * Load Intel hex file
 */

void load_intel( char *fname_in )
{
	FILE *fp_in;
	int err;
	int sor;
	unsigned long type;
	unsigned long addr;
	unsigned long byte;

	if( (fp_in = fopen( fname_in, "r")) == NULL )
	{
	    printf( "\nCan't open %s for input", fname_in );
	    return;
	}

	err = 0;
	while( err != -1 )
	{
		/* locate start of record */
		do {
			sor = fgetc( fp_in);
		} while( (sor != ':') && (sor != -1) );
		checksum = 0;

		/* read data byte count */
		if( (err = get2hex( fp_in, &count )) == -1)
		{
			break;
		}

		/* read address */
		if( (err = read_addr16( fp_in )) == -1 )
		{
			break;
		}

		/* read record type */
		if( (err = get2hex( fp_in, &type  )) == -1)
		{
			break;
		}

		/* adjust byte count */
		count += 4;	

		switch( type)
		{
		case 0:
			/* read 00 to load a record */
			while( (count > 0) && (err != -1) )
	        {
				err = read_byte( fp_in );
			}
			err = get2hex( fp_in, &byte );
			if( checksum != 0 )
			{
				printf( "\nchecksum read %02x, expected %02x", byte, (checksum - byte)&0xff );
				err = -1;
			}
			break;

		case 1:
			/* read 01 to end record */
			break;

		default:
			break;
		}
	}
	fclose( fp_in );
}


/*
 * load smal32 .o formatted file
 */

void load_smal32( char *fname_in )
{
	FILE *fp_in;
	int state;
	int err;
	int sor;
	unsigned long byte;

	if( (fp_in = fopen( fname_in, "r")) == NULL )
	{
	    printf( "\nCan't open %s", fname_in );
	    return; 
	}

	state = 0;
	addr  = 0;
	byte  = 0;

	while( err != -1 )
	{
		sor = fgetc( fp_in);
		switch( state )
		{
		case 0:
			switch( sor )
			{
			case '.':
				state = 1;
				break;
			case 'B':
				state = 3;
				break;
			case 'H':
				state = 4;
				break;
			case 'W':
				state = 5;
				break;
			default:
				state = 0;
			}
			break;

		case 1:
			if( sor == '=' )
			{
				state = 2;
			}
			else
			{
				state = 0;
			}
			break;

		case 2:
			/* ".=#" => 32 bit address */
			state = 0;
			if( sor == '#' )
			{
				if( (err = read_addr32( fp_in )) == -1)
				{
					break;
				}
				printf( "\nload address = %08x", addr );
			}
			break;

		case 3:
			/* "B#" => 8 bit data byte */
			state = 0;
			if( sor == '#' )
			{
				if( (err = read_byte( fp_in )) == -1)
				{
					break;
				}
			}
			break;

		case 4:
			/* "H#" => 16 bit data half word */
			state = 0;
			if( sor == '#' )
			{
				if( (err = read_byte( fp_in )) == -1)
				{
					break;
				}
				if( (err = read_byte( fp_in )) == -1)
				{
					break;
				}
			}
			break;

		case 5:
			/* "W#" => 32 bit data long word */
			state = 0;
			if( sor == '#' )
			{
				if( (err = read_byte( fp_in )) == -1)
				{
					break;
				}
				if( (err = read_byte( fp_in )) == -1)
				{
					break;
				}
				if( (err = read_byte( fp_in )) == -1)
				{
					break;
				}
				if( (err = read_byte( fp_in )) == -1)
				{
					break;
				}
			}
			break;

		default :
			state = 0;
			break;
		}
	}
	printf( "\nlast address = %08x", addr );
	fclose( fp_in );
}

/*
 * load VHDL Binary file
 */

void load_vhdl_bin( char *fname_in, unsigned long load_addr )
{
	FILE *fp_in;
	int i;
	int err;
	int state;
	int byte;
	int data;
	unsigned long addr;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\nCan't open '%s' for read ", fname_in );
		return;
	}
	state = 0;
	addr = load_addr;
	err = 0;
	while( err != -1)
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
			else
			{
				state = 0;
			}
			break;
		case 1:
			data <<= 1;
			if( byte == '1' )
			{
				data += 1;
			}
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
			{
				state = 2;
			}
			break;
		case 2:
			state = 0;
			if( byte == '"' )
			{
				if( (addr - offset) < eprom_size )
				{
					if( (addr - offset) > eprom_top)
					{
						eprom_top = (addr - offset);
					}
					eprom_buff[ (addr - offset) % eprom_size ] = (unsigned char)data;
					addr++;
				}
			}
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

void load_vhdl_byte( char *fname_in, unsigned long start_addr )
{
	FILE *fp_in;
	int i;
	int byte;
	int data;
	int state;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\nCan't open '%s' for read ", fname_in );
		return;
	}
	state = 0;
	addr = start_addr;
	if( (addr - offset) < eprom_bot )
	{
		eprom_bot = addr - offset;
	}
	byte = 0;
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
			else
			{
				state = 0;
			}
			break;

		case 1:
			if( byte == '"' )
			{
				data = 0;
				i = 31;
				if( (addr - offset + i) >= eprom_size )
				{
					state = 0;
					break;
				}
				if( (addr - offset + i) > eprom_top )
				{
					eprom_top = addr - offset + i;
				}
				state = 2;
			}
			else
			{
				state = 0;
			}
			break;

		case 2:
			data = hexadec2int( (char)byte );
			if( data != -1 )
			{
				data <<= 4;
				state = 3;
			} 
			else
			{
				state = 0;
			}
			break;

		case 3:
			if( hexadec2int( (char)byte ) != -1 )
			{
				data += hexadec2int( (char)byte );
				eprom_buff[ (addr - offset + i ) % eprom_size ] = (unsigned char)data;
				if( i == 0 )
				{
					addr += 32;
					state = 0;
				} 
				else
				{
					i--;
					state = 2;
				}
			} 
			else
			{
				state = 0;
			}
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

void load_vhdl_word( char *fname_in, unsigned long start_addr )
{
	FILE *fp_in;
	unsigned long addr;
	int i;
	int byte;
	int data;
	int state;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\nCan't open '%s' for read ", fname_in );
		return;
	}
	state = 0;
	addr = start_addr;
	byte = 0;
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
			else
			{
				state = 0;
			}
			break;

		case 1:
			if( byte == '"' )
			{
				data = 0;
				i = 30;
				if( (addr - offset + i) >= eprom_size )
				{
					state = 0;
					break;
				}
				if( (addr - offset + i) > eprom_top )
				{
					eprom_top = addr - offset + i;
				}
				state = 2;
			}
			else
			{
				state = 0;
			}
			break;

		case 2:
			data = hexadec2int( (char)byte );
			if( data != -1 )
			{
				data <<= 4;
				state = 3;
			} 
			else
			{
				state = 0;
			}
			break;

		case 3:
			if( hexadec2int( (char)byte ) != -1 )
			{
				data += hexadec2int( (char)byte );
				eprom_buff[ (addr + i) % eprom_size ] = (unsigned char)data;
				state = 4;
			} 
			else
			{
				state = 0;
			}
			break;

		case 4:
			data = hexadec2int( (char)byte );
			if( data != -1 )
			{
				data <<= 4;
				state = 5;
			} 
			else
			{
				state = 0;
			}
			break;

		case 5:
			if( hexadec2int( (char)byte ) != -1 )
			{
				data += hexadec2int( (char)byte );
				eprom_buff[ (addr + i + 1) % eprom_size ] = (unsigned char)data;
				if( i == 0 )
				{
					addr += 32;
					state = 0;
				} 
				else
				{
					i -= 2;
					state = 2;
				}
			} 
			else
			{
				state = 0;
			}
			break;

		default:
			break;
		}
	}
	fclose( fp_in );
	printf( "\nLoaded " );
}



int put2hex( FILE *fp_out, unsigned long h )
{
	int i;
	unsigned long hex;
	
	hex      = (h & 0x000000f0)>>4;
	i        = fputc( (int)hex_str[hex], fp_out );
	hex      = (h & 0x0000000f);
	i        = fputc( (int)hex_str[hex], fp_out );
	checksum = (checksum + h) & 0x000000ff;
	count--;
	return i;
}

int put4hex( FILE *fp_out, unsigned long h )
{
	unsigned int i;
	
	i = put2hex( fp_out, (h & 0x000000ff00)>>8 );
	i = put2hex( fp_out, (h & 0x00000000ff) );
	return i;
}

int put6hex( FILE *fp_out, unsigned long h )
{
	int i;
	
	i = put2hex( fp_out, (h & 0x00ff0000)>>16 );
	i = put2hex( fp_out, (h & 0x0000ff00)>>8 );
	i = put2hex( fp_out, (h & 0x000000ff) );
	return i;
}

int put8hex( FILE *fp_out, unsigned long h )
{
	int i;
	
	i = put2hex( fp_out, (h & 0xff000000)>>24 );
	i = put2hex( fp_out, (h & 0x00ff0000)>>16 );
	i = put2hex( fp_out, (h & 0x0000ff00)>>8 );
	i = put2hex( fp_out, (h & 0x000000ff) );
	return i;
}

int write_byte( FILE *fp_out )
{
	unsigned long byte;
	int err = 0;

	if( (addr - offset) >= eprom_size )
	{
		err = -1;
	}
	else
	{
		byte = (unsigned long)eprom_buff[(addr - offset) % eprom_size];
		err = put2hex( fp_out, byte );
		addr++;
	}
	return err;
}

/*
 * generate binary string of specified length
 */
char *bin_str( unsigned long byte, unsigned long len )
{
	long i;
	char *cp;

	cp = &str_buff[0];
	*cp++ = '\"';
	for( i = len-1; i >= 0; i-- )
	{
		if(( byte & (1<<i)) == 0 )
		{
			*cp++ = '0';;
		}
		else
		{
			*cp++ = '1';;
		}
	}
	*cp++ = '\"';
	*cp++ = '\0';
	return &str_buff[0];
}

/*
 * save raw binary file
 */

void save_binary( char *fname_out, unsigned long start_addr, unsigned long end_addr )
{
	FILE *fp_out;
	unsigned long addr;

	if( (fp_out = fopen( fname_out, "w" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}
	for( addr = start_addr; addr <= end_addr; addr++ )
	{
		putc( eprom_buff[(addr - offset) % eprom_size ], fp_out );
	}
	fclose( fp_out );
	printf( "\nwritten successfully" );
}


/*
 * Save Motorola S1 file
 */
void save_mot( char *fname_out, unsigned long start_addr, unsigned long end_addr )
{
	FILE *fp_out;
	int err;
	char type;
	unsigned long addr;
	unsigned long byte;

	if( (fp_out = fopen( fname_out, "w")) == NULL )
	{
	    printf( "\nCan't open %s for output", fname_out );
	    return;
	}

	if( end_addr < 0x00010000 )
	{
		type = '1';
	}
	else if (end_addr < 0x01000000 )
	{
		type = '2';
	}
	else
	{
		type = '3';
	}

	addr = start_addr;
	while( addr <= end_addr )
	{
		if( (end_addr - addr + 1) > 16 )
		{
			count = 16;
		}
		else
		{
			count = end_addr - addr + 1;
		}
		fputc( 'S', fp_out);  	/* record header preamble */
		fputc( type, fp_out);  	/* record header preamble */
		checksum = 0;
		switch( type )
		{
		case '1':
			err = put2hex( fp_out, count+3);		/* byte count */
			err = put4hex( fp_out, addr);			/* Program Counter */
			break;

		case '2':
			err = put2hex( fp_out, count+4);		/* byte count + 4 */
			err = put6hex( fp_out, addr);			/* Program Counter */
			break;

		default:
			err = put2hex( fp_out, count+5);		/* byte count + 5 */
			err = put8hex( fp_out, addr);			/* Program Counter */
			break;
		}

		while( (count > 0) && (err != -1) )
		{
			err = write_byte( fp_out );
		}
		checksum = (~checksum) & 0x000000ff;		/* one's complement */
		err = put2hex( fp_out, checksum );
		fputc( '\n', fp_out);  						/* record header preamble */
	}
	checksum = 0;
	fputc( 'S', fp_out);  							/* record header preamble */
	switch( type )
	{
	case '1':
		fputc( '9', fp_out);						/* record header preamble */
		count = 3;
		put2hex( fp_out, count );					/* byte count */
		put4hex( fp_out, trans_addr );				/* transfer address */
		break;

	case '2':
		fputc( '8', fp_out);  						/* record header preamble */
		count = 4;
		put2hex( fp_out, count );					/* byte count */
		put6hex( fp_out, trans_addr );				/* transfer address */
		break;

	case '3':
		fputc( '7', fp_out);  						/* record header preamble */
		count = 5;
		put2hex( fp_out, count );					/* byte count */
		put8hex( fp_out, trans_addr );				/* transfer address */
		break;

	default:
		fputc( '9', fp_out);  						/* record header preamble */
		count = 0;
		put2hex( fp_out, count );					/* byte count */
		break;
	}
	checksum = (~checksum) & 0x000000ff;			/* one's complement */
	put2hex( fp_out, checksum );					/* checksum */
	fputc( '\n', fp_out);
	fclose( fp_out );
}

/*
 * save Intel hex file
 */

void save_intel( char *fname_out, unsigned long start_addr, unsigned long end_addr )
{
	FILE *fp_out;
	int i;
	int err = 0;
	int rec_len;
	unsigned long byte;
	unsigned long addr;
	unsigned long check;

	fp_out = fopen( fname_out, "w" );
	if( fp_out == NULL )
	{
		printf( "\nCan't open %s for output", fname_out );
		return;
	}

	addr = start_addr;
	while( addr <= end_addr )
	{
		check = 0;
		count = 16;
		checksum = 0;
		fputc( ':', fp_out );
		put2hex( fp_out, count );
		put4hex( fp_out, addr );
		put2hex( fp_out, 0 );
		count += 4;
		while( ( count > 0) && ( err != -1) )
		{
			err = write_byte( fp_out );
		}
		checksum = (0 - checksum) & 0x000000ff;
		put2hex( fp_out, checksum);
		fputc( '\n', fp_out );
	}
	fputc( ':', fp_out );
	put2hex( fp_out, (unsigned long)0 );
	put4hex( fp_out, (trans_addr & 0x0000ffff) );
	put2hex( fp_out, (unsigned long)1 );
	fputc( '\n', fp_out );
	fclose( fp_out );
}

/*
 * save VHDL Binary file
 */

void save_vhdl_bin( char *fname_out, unsigned long start_addr, unsigned long end_addr )
{
	FILE *fp_out;
	int i;
	unsigned long addr;
	int byte;

	if( (fp_out = fopen( fname_out, "w" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}
	for( addr = start_addr; addr <= end_addr; addr++ )
	{
		byte = (int)eprom_buff[(addr - offset) % eprom_size];
		fputs( bin_str( byte, 8 ), fp_out );
		if (addr < end_addr-1)
		{
			putc( ',', fp_out );
		}
		putc( '\n', fp_out );
	}
	fclose( fp_out );
	printf( "\nwritten successfully" );
}

/*
 * save VHDL hexadecimal file
 */
void save_vhdl_byte( char *fname_out, unsigned long start_addr, unsigned long end_addr, char *entity_name, unsigned long bit_count, unsigned long data_width )
{
	FILE *fp_out;
	int i;
	int j;
	unsigned long addr;
	int byte;
	int byte_count;
	int addr_count;
	int block_count;
	int byte_width;
	int addr_width;
	int block_width;
	int parity_width;
	int block_index;


	if( (fp_out = fopen( fname_out, "w" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}


	byte_count   = (int)(bit_count / 8);
	byte_width   = (int)ceil( log( (double)byte_count  ) / log( 2.0 ) );
	addr_count   = (int)(bit_count / data_width);
	addr_width   = (int)ceil( log( (double)addr_count  ) / log( 2.0 ) );
	block_count  = (int)(((end_addr - start_addr)/byte_count) + 1);
	block_width  = (int)ceil( log( (double)block_count ) / log( 2.0 ) );
	parity_width = (int)(data_width / 8);

	printf( "    file name %s \n", fname_out );
	printf( "start address %d \n", start_addr );
	printf( "  end address %d \n", end_addr );
	printf( "  entity name %s \n", entity_name );
	printf( "    bit count %d \n", bit_count );
	printf( "   data width %d \n", data_width );
	printf( "   byte count %d \n", byte_count );
	printf( "   byte width %d \n", byte_width );
	printf( "address count %d \n", addr_count );
	printf( "address width %d \n", addr_width );
	printf( "  block count %d \n", block_count );
	printf( "  block width %d \n", block_width );
	printf( " parity width %d \n", parity_width );

	fprintf(fp_out, "library IEEE;\n");
	fprintf(fp_out, "   use IEEE.std_logic_1164.all;\n");
	fprintf(fp_out, "   use IEEE.std_logic_arith.all;\n");
	fprintf(fp_out, "library unisim;\n");
	fprintf(fp_out, "   use unisim.vcomponents.all;\n");
	fprintf(fp_out, "\n");
	fprintf(fp_out, "entity %s is\n", entity_name);
	fprintf(fp_out, "   port(\n");
	fprintf(fp_out, "      clk      : in  std_logic;\n");
	fprintf(fp_out, "      rst      : in  std_logic;\n");
	fprintf(fp_out, "      cs       : in  std_logic;\n");
	fprintf(fp_out, "      rw       : in  std_logic;\n");
	fprintf(fp_out, "      addr     : in  std_logic_vector(%d downto 0);\n", addr_width + block_width - 1);
	fprintf(fp_out, "      data_in  : in  std_logic_vector(%d downto 0);\n", data_width - 1);
	fprintf(fp_out, "      data_out : out std_logic_vector(%d downto 0)\n",  data_width - 1);
	fprintf(fp_out, "   );\n");
	fprintf(fp_out, "end %s;\n", entity_name);
	fprintf(fp_out, "\n");
	fprintf(fp_out, "architecture rtl of %s is\n", entity_name);
	fprintf(fp_out, "   type d_type is array( 0 to %d ) of std_logic_vector(%d downto 0);\n", block_count-1, data_width-1);
	fprintf(fp_out, "   type p_type is array( 0 to %d ) of std_logic_vector(%d downto 0);\n", block_count-1, parity_width-1);
	fprintf(fp_out, "   signal do : d_type;\n");
	fprintf(fp_out, "   signal dp : p_type;\n");
	fprintf(fp_out, "   signal en : std_logic_vector(%d downto 0);\n", block_count-1);
	fprintf(fp_out, "   signal we : std_logic;\n");
	fprintf(fp_out, "begin\n");

	addr = start_addr;
	for( block_index = 0; block_index < block_count; block_index++ )
	{
		fprintf(fp_out, "   ROM%d: RAMB16_S%d\n", block_index, data_width+parity_width );
		fprintf(fp_out, "      generic map (\n");

		for( j = 0; j < byte_count/32; j++ )
		{
 			fprintf( fp_out, "    INIT_%02x => x\"", j );
			for( i = 31; i >= 0; i-- )
			{
				byte = (int)eprom_buff[(addr - offset + i) % eprom_size];
				fputc( hex_str[(byte >>4) & 0x0f], fp_out ); 
				fputc( hex_str[byte & 0x0f], fp_out ); 
			}
 			if ( addr > (end_addr - 32) )
			{
				fprintf( fp_out, "\"\n" );
			}
			else
			{
				fprintf( fp_out, "\",\n" );
			}
			addr += 32;
		}
		fprintf(fp_out, "      )\n");
		fprintf(fp_out, "      port map (\n");
		fprintf(fp_out, "         do      => do(%d),\n", block_index);
		fprintf(fp_out, "         dop     => dp(%d),\n", block_index);
		fprintf(fp_out, "         addr    => addr(%d downto 0),\n", addr_width-1 );
		fprintf(fp_out, "         clk     => clk,\n");
		fprintf(fp_out, "         di      => data_in,\n");
		fprintf(fp_out, "         dip     => dp(%d),\n", block_index);
		fprintf(fp_out, "         en      => en(%d),\n", block_index);
		fprintf(fp_out, "         ssr     => rst,\n");
		fprintf(fp_out, "         we      => we\n");
		fprintf(fp_out, "      );\n");
	}

	fprintf(fp_out, "   rom_decode: process(rw, addr, do)\n");
	fprintf(fp_out, "   begin\n");
	fprintf(fp_out, "      we <= not rw;\n");
	if( block_count > 1 )
	{
		fprintf(fp_out, "      en <= (others => '0');\n");
		fprintf(fp_out, "      case( addr(%d downto %d)) is\n", addr_width + block_width - 1, addr_width);
		for( block_index = 0; block_index < block_count; block_index++ )
		{
			fprintf(fp_out, "      when %s =>\n", bin_str( block_index, block_width ));
			fprintf(fp_out, "          en(%d)   <= cs;\n", block_index);
			fprintf(fp_out, "          data_out <= do(%d);\n", block_index);
		}
		fprintf(fp_out, "      when others =>\n");
		fprintf(fp_out, "          data_out <= (others=>'0');\n");
		fprintf(fp_out, "      end case;\n");
	}
	else
	{
		fprintf(fp_out, "      en(%d)   <= cs;\n", 0 );
		fprintf(fp_out, "      data_out <= do(%d);\n", 0);
	}

	fprintf(fp_out, "   end process;\n");
	fprintf(fp_out, "end architecture rtl;\n\n");

	fclose( fp_out );
	printf( "\nwritten successfully" );
}

/*
 * save VHDL hexadecimal Word file
 */

void save_vhdl_word( char *fname_out, unsigned long start_addr, unsigned long end_addr )
{
	FILE *fp_out;
	unsigned long addr;
	int i;
	int j;
	int byte;

	if( (fp_out = fopen( fname_out, "w" )) == NULL )
	{
		printf( "\nCan't open '%s' for write ", fname_out );
		return;
	}

	j=0;
	for( addr = start_addr; addr <= end_addr; addr += 32 )
	{
		fprintf( fp_out, "  INIT_%02x => x\"", j );
		for( i = 30; i >= 0; i -= 2 )
		{
			/* MSByte first */
			byte = (int)eprom_buff[(addr - offset + i) % eprom_size];
			putc( hex_str[(byte >>4) & 0x0f], fp_out ); 
			putc( hex_str[byte & 0x0f], fp_out ); 
			/* LSByte second */
			byte = (int)eprom_buff[(addr - offset + i + 1) % eprom_size];
			putc( hex_str[(byte >>4) & 0x0f], fp_out ); 
			putc( hex_str[byte & 0x0f], fp_out ); 
		}
  		if ( addr > (end_addr - 32) )
		{
			fprintf( fp_out, "\"\n" );
		}
		else
		{
			fprintf( fp_out, "\",\n" );
		}
		j++;
	}
	fclose( fp_out );
	printf( "\nwritten successfully" );
}

/*
 * Compare Raw binary file
 */

void compare_binary( char *fname_in, unsigned long load_addr, unsigned long start_addr, unsigned long end_addr)
{
	FILE *fp_in;
	int addr;
	int data_byte;
	unsigned long diff_count;

	if( (fp_in = fopen( fname_in, "r" )) == NULL )
	{
		printf( "\ncant open %s for read", fname_in );
		return;
	} 
	diff_count = 0;
	addr = load_addr;
	while( (data_byte = fgetc( fp_in )) != EOF )
	{
		if( (addr >= start_addr) && (addr <= end_addr) )
		{
			if( (unsigned char)data_byte != eprom_buff[(addr - offset) % eprom_size] )
			{
				printf( "\n%08x %02x %02x", addr, eprom_buff[(addr - offset) % eprom_size ], data_byte );
				diff_count++;
			}
		}
		addr++;
	}
	fclose( fp_in );
	printf( "\nLast compared address %x ", addr-1 );
	printf( "\nThere were %d differences ", diff_count );
}


/*
 * Compare motorola formatted file
 */

void compare_mot( char *fname_in, unsigned long start_addr, unsigned long end_addr )
{
	FILE *fp_in;
	int i;
	int type;
	int err;
	int sor;
	unsigned long addr;
	unsigned long byte;
	unsigned long diff_count;

	fp_in = fopen( fname_in, "r" );
	if( !fp_in )
	{
	    printf( "\nCan't open %s", fname_in );
	    return; 
	}

	err = 0;
	diff_count = 0;

	while( err != -1 )
	{
		do {
			sor = fgetc( fp_in);
		} while( (sor != 'S') && (sor != -1) );

		type = fgetc( fp_in );
		checksum = 0;
		if( err != -1 )
		{
			err = get2hex( fp_in, &count );
			switch( type )
			{
			case '1':
				err = get4hex( fp_in, &addr );
				break;
			case '2':
				err = get6hex( fp_in, &addr );
				break;
			case '3':
				err = get8hex( fp_in, &addr );
				break;
			default:
				break;
			}
			while( (count > 0) && (err != -1) )
			{
				err = get2hex( fp_in, &byte );
				if( (addr >= start_addr) && (addr <= end_addr ))
				{
					if( (unsigned char)byte != eprom_buff[(addr - offset) % eprom_size ] )
					{
						printf( "\n%08x %02x %02x", addr, eprom_buff[(addr-offset) % eprom_size ], byte );
						diff_count++;
					}
				}
				addr++;
				if( addr >= eprom_size )
				{
					printf( "\nAddress out of range ");
					err = -1;
				}
			}
			err = get2hex( fp_in, &byte );
		}
	}
	fclose( fp_in );
	printf( "\nLast compared address %08x ", addr - 1 );
	printf( "\nThere were %d differences ", diff_count );
}

/*
 * Compare Intel hex file
 */

void compare_intel( char *fname_in, unsigned long start_addr, unsigned long end_addr )
{
	FILE *fp_in;
	int sor;
	int err;
	unsigned long type;
	unsigned long byte;
	unsigned long addr;
	unsigned long diff_count;
	int byte_count;

	fp_in = fopen( fname_in, "r" );
	if( fp_in == 0 )
	{
	    printf( "\nCan't open %s for input", fname_in );
	    return;
	}

	err = 0;
	byte_count = 0;
	diff_count = 0;
	while( err != -1)
	{
		do {
			sor = fgetc( fp_in);
		} while( (sor != ':') && (sor != -1) );

		checksum = 0;
		err = get2hex( fp_in, &count );
		err = get4hex( fp_in, &addr );
		err = get2hex( fp_in, &type );
		count = count+4;
		switch( type )						
		{
		case 0:
			/* read 00 to load a record */
			while( (count > 0) && (err != -1) )
			{
				err = get2hex( fp_in, &byte );
				if( (addr >= start_addr) && (addr <= end_addr ))
				{
					if( (unsigned char)byte != eprom_buff[(addr - offset) % eprom_size] )
					{
						printf( "\n%08x %02x %02x", addr, eprom_buff[(addr-offset) % eprom_size], byte );
						diff_count++;
					}
				}
				addr++;
				if( addr >= eprom_size )
				{
					err = -1;
				}
			}
			/* read checksum */
			err = get2hex( fp_in, &byte );
			if( checksum != 0 )
			{
				printf( "\nchecksum error detected, read %02x, expected %02x", byte, ((checksum - byte) & 0xff) );
				err = -1;
			}
			break;
		default:
			/* test for end of record type == 1*/
			err = -1;
		}
	}
	fclose( fp_in );
	printf( "\nLast compared address %08x ", addr - 1 );
	printf( "\nThere were %d differences ", diff_count );
}


/*
 * help menu
 */
void display_help()
{
	printf( "\n               *** Epedit Help Menu ***\n");
	printf( "\n");
	printf( "All addresses are in hexadecimal\n" );
	printf( "H/?/help                                             - This help menu\n" );
	printf( "T/type     <format>                                  - Set Load & Save format\n" );
	printf( "where      <format> is:\n");
	printf( "           'B'/'binary'                              - raw Binary\n" );
	printf( "           'M'/'motorola'                            - Motorola s19\n");
	printf( "           'I'/'intel'                               - Intel Hex\n");
	printf( "           'O'/'smal32'                              - smal macro assemble Object\n" );
	printf( "           'V'/'vhdl'                                - xilinx Vhdl binary string\n" );
	printf( "           'H'/'hex'                                 - xilinx vhdl block ram Hexadecimal byte string\n" );
	printf( "           'W'/'word'                                - xilinx vhdl block ram hexadecimal Word string\n" );
	printf( "O/offset   <address>                                 - set eprom buffer Offset\n" );
	printf( "L/load     <filename> [<load_addr>]                  - Loads file into buffer\n" );
	printf( "S/save     <filename> <start_addr> <end_addr>        - Saves buffer to file\n" ); 
	printf( "           [<entity_name> <block_size> <data_width>] \n");
	printf( "C/compare  <filename> [<load_addr>]                  - Compare file with buffer\n" );
	printf( "W/write    <address> <data> .... <data>              - Write data to buffer\n" );
	printf( "F/fill     <start_addr> <end_addr> <data>            - Fill buffer with data byte\n");
	printf( "M/move     <start_addr> <end_addr> <dest_addr>       - block Move memory\n" );
	printf( "D/dump     <start_addr> <end_addr>                   - Dump buffer\n");
	printf( "E/echo     <string>                                  - Echo string to console\n" );
	printf( "B/buffer   <size>                                    - set Buffer size\n" );
	printf( "A/address  <address>                                 - set transfer Address\n");
	printf( "X/excecute <command-file>                            - eXecute a command script\n");
	printf( "Q/quit                                               - Quit program\n" );
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
	char filename[64];
	char entity_name[64];		/* name of xilinx block RAM entity */
	unsigned long ram_size;		/* size of xilinx block RAM in bits */
	unsigned long data_width;			/* data width of xilinx block RAM in bits */
	unsigned long i;
	unsigned long start_addr;
	unsigned long end_addr;
	unsigned long dest_addr;
	unsigned long load_addr;
	unsigned long byte_count;
	unsigned long data_byte;
	int ch;
	int len;

	lineptr    = 0;
	start_addr = 0;
	end_addr   = 0;

	/* skip leading white spaces */
	while( isspace( cmdline[lineptr] ))
	{
		 lineptr++;
	}
	/* point to the start of the command argument & extract command */
	cmdptr = &cmdline[lineptr];
	len = 0;
	while( isalpha( cmdline[lineptr] ) || (cmdline[lineptr] == '?'))
	{
		lineptr++;
		len++;
	}
	/* skip trailing white spaces */
	while( isspace( cmdline[lineptr] ) )
	{
		lineptr++;
	}
    if( len > 0 )
    {
		if( str_equal( cmdptr, "T", len ) || str_equal( cmdptr, "type", len ) )
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
			switch( format_type ) 
			{
			case BINARY:
				printf( "Raw Binary" );
				break;
			case MOTOROLA:
				printf( "Motorola S" );
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
		else if( str_equal( cmdptr, "L", len ) || str_equal( cmdptr, "load", len ) )
		{
			/***********************************************************
			 *
			 * Load file into buffer
			 */
			arglen = get_string( &cmdline[lineptr], &filename[0] );
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
			if( (load_addr < offset) || (load_addr >= (offset + eprom_size)))
			{
				printf( "\nLoad address out of range" );
				return TRUE;
			}
			switch( format_type ) 
			{
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
		} 
		else if( str_equal( cmdptr, "S", len ) || str_equal( cmdptr, "save", len ))
		{
			/*
			 * Save buffer to file
			 */
			if( (arglen = get_string( &cmdline[lineptr], &filename[0] )) == 0 )
			{
				printf( "\nFile name expected " );
				return TRUE;
			}
			lineptr += arglen;
			if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
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
			lineptr += arglen;
			if( (start_addr < offset) || (start_addr >= (offset + eprom_size)))
			{
				printf( "\nStart address out of range" );
				return TRUE;
			}
			if( (end_addr < offset) || (end_addr >= (offset + eprom_size)))
			{
				printf( "\nEnd address out of range" );
				return TRUE;
			}
			printf( "\nSaving buffer %08x to %08x to file %s ", start_addr, end_addr, filename );
			switch( format_type ) 
			{
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
				if( (arglen = get_string( &cmdline[lineptr], &entity_name[0] )) == 0)
				{
					printf( "\nEntity name expected " );
					return TRUE;
				}
				lineptr += arglen;
				if( (arglen = get_address( &cmdline[lineptr], &ram_size )) == 0)
				{
					printf( "\nBlock RAM size expected " );
					return TRUE;
				}
				lineptr += arglen;
				if( (arglen = get_address( &cmdline[lineptr], &data_width )) == 0)
				{
					printf( "\nBlock RAM data width expected " );
					return TRUE;
				}
				lineptr += arglen;
				printf( "\nVHDL hexadecimal byte format" );
				save_vhdl_byte( filename, start_addr, end_addr, &entity_name[0], ram_size, data_width );
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
		} 
		else if( str_equal( cmdptr, "D", len ) || str_equal( cmdptr, "dump", len ))
		{
			/*
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
			if( (start_addr < offset) || (start_addr >= (offset + eprom_size)))
			{
				printf( "\nStart address out of range" );
				return TRUE;
			}
			if( (end_addr < offset) || (end_addr >= (offset + eprom_size)))
			{
				printf( "\nEnd address out of range" );
				return TRUE;
			}
			load_addr = 0;
			printf( "\n Memory Dump form %08x to %08x ", start_addr, end_addr );
			while( (start_addr + load_addr) <= end_addr )
			{
				printf( "\n  %08x - ", start_addr + load_addr );
				for( dest_addr = 0; dest_addr < 16; dest_addr++ )
				{
					if( (start_addr + load_addr + dest_addr) <= end_addr )
					{
						printf( "%02x ", eprom_buff[( start_addr + load_addr + dest_addr) % eprom_size] );
					}
					else
					{
						printf( "   " );
					}
				}
				printf( "  " );
				for( dest_addr = 0; dest_addr < 16; dest_addr++ )
				{
					if( (start_addr + load_addr + dest_addr) <= end_addr )
					{
						ch = eprom_buff[(start_addr+load_addr+dest_addr) % eprom_size];
						if( (ch > 0x20) && (ch < 0x7f) )
						{
							printf( "%c", ch );
						}
						else
						{
							printf( "." );
						}
					} 
					else
					{
						printf( " " );
					}
				}
				load_addr += dest_addr;
			}
			return TRUE;
		} 
		else if( str_equal( cmdptr, "C", len ) || str_equal( cmdptr, "compare", len ))
		{
			/*
			 * compare file with buffer
			 */
			if( (arglen = get_string( &cmdline[lineptr], &filename[0] )) == 0 )
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
				end_addr = eprom_size + offset - 1;
			}
			lineptr += arglen;

			/* the end/start address is optional */
			if( (arglen = get_address( &cmdline[lineptr], &load_addr )) == 0 )
			{
				load_addr = eprom_size + offset - 1;
			}

			/* check for valid address range */
			if( (start_addr < offset) || (start_addr >= (offset + eprom_size)))
			{
				printf( "\nStart address out of range" );
				return TRUE;
			}
			if( (end_addr < offset) || (end_addr >= (offset + eprom_size)))
			{
				printf( "\nEnd address out of range" );
				return TRUE;
			}
			if( (load_addr < offset) || (load_addr >= (offset + eprom_size)))
			{
				printf( "\nLoad address out of range" );
				return TRUE;
			}
			printf( "\nComparing buffer to file %s", filename );
			switch( format_type ) 
			{
			case BINARY:
				printf( "\nBinary format" );
				dest_addr = load_addr;
				load_addr = start_addr;
				start_addr = end_addr;
				end_addr = dest_addr;
				if( start_addr == (eprom_size + offset - 1) )
				{
					start_addr = offset;
				}
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
		} 
		else if( str_equal( cmdptr, "M", len ) || str_equal( cmdptr, "move", len ))
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
			if( (start_addr < offset) || (start_addr >= (offset + eprom_size)))
			{
				printf( "\nStart address out of range" );
				return TRUE;
			}
			if( (end_addr < offset) || (end_addr >= (offset + eprom_size)))
			{
				printf( "\nEnd address out of range" );
				return TRUE;
			}
			if( (dest_addr < offset) || (dest_addr >= (offset + eprom_size)))	
			{
				printf( "\nDestination address out of range" );
				return TRUE;
			}
			byte_count = end_addr - start_addr;
			printf( "\nTransfering memory block %08x thru %08x to %08x", start_addr, end_addr, dest_addr );
			if( start_addr > dest_addr  )
			{
				for( load_addr=0; load_addr<=byte_count; load_addr++ )
				{
					eprom_buff[(dest_addr-offset+load_addr) % eprom_size] = eprom_buff[(start_addr-offset+load_addr) % eprom_size];
				}
			} 
			else
			{
				for( load_addr=byte_count; load_addr>=0; load_addr-- )
				{
					eprom_buff[(dest_addr-offset+load_addr) % eprom_size] = eprom_buff[(start_addr-offset+load_addr) % eprom_size];
				}
			}	
			printf( "\nDone" );
			mod_flag = TRUE;
			return TRUE;
		} 
		else if( str_equal( cmdptr, "F", len ) || str_equal( cmdptr, "fill", len ))
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
			if( (start_addr < offset) || (start_addr >= (offset + eprom_size)))
			{
				printf( "\nStart address out of range" );
				return TRUE;
			}
			if( (end_addr < offset) || (end_addr >= (offset + eprom_size)))
			{
				printf( "\nEnd address out of range" );
				return TRUE;
			}
			printf( "\nFilling address %08x thru %08x with %02x ", start_addr, end_addr, data_byte );
			for( dest_addr=start_addr; dest_addr<=end_addr; dest_addr++ )
			{
				eprom_buff[(dest_addr-offset) % eprom_size] = (unsigned char)data_byte;
			}
			printf( "\nDone" );
			mod_flag = TRUE;
			return TRUE;
		} 
		else if( str_equal( cmdptr, "W", len ) || str_equal( cmdptr, "write", len ))
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
			if( (start_addr < offset) || (start_addr >= (offset + eprom_size)))
			{
				printf( "\nStart address out of range" );
				return TRUE;
			}
			printf( "\nSetting buffer loaction %08x", start_addr );
			while( (arglen = get_address( &cmdline[lineptr], &data_byte )) != 0)
			{
				eprom_buff[(start_addr-offset) % eprom_size] = (unsigned char)data_byte;
				printf( "\n %08x = %02x ", start_addr, data_byte );
				start_addr++;
				lineptr += arglen;
			}
			mod_flag = TRUE;
			return TRUE;
		} 
		else if( str_equal( cmdptr, "O", len ) || str_equal( cmdptr, "offset", len ))
		{
			/***********************************************************
			 *
			 * Set Eprom base address offset
			 */
			if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
			{
				/* No argument, display offset */
				printf( "\nBase address offset = %08x", offset );
				return TRUE;
			}
			offset = start_addr;
			mod_flag = TRUE;
			return TRUE;
		} 
		else if( str_equal( cmdptr, "H", len ) || str_equal( cmdptr, "?", len ) || str_equal( cmdptr, "help", len ))
		{
			/***********************************************************
			 *
			 * Display help menu
			 */
			display_help();
			return TRUE;
		} 
		else if( str_equal( cmdptr, "Q", len ) || str_equal( cmdptr, "quit", len ))
		{
			/***********************************************************
			 *
			 * Quit program - return FALSE
			 */
			printf( "\nQuitting program" );
			return FALSE;
		} 
		else if( str_equal( cmdptr, "E", len ) || str_equal( cmdptr, "echo", len ))
		{
			/***********************************************************
			 *
			 * Echo string to the console
			 */
			printf( "\n" );
			while( (cmdline[lineptr] != '\0') && (cmdline[lineptr] != '\n'))
			{
				printf( "%c", cmdline[lineptr++] );
			}
			printf( "\n" );
			return TRUE;
		} 
		else if( str_equal( cmdptr, "B", len ) || str_equal( cmdptr, "buffer", len ))
		{
			/*
			 * set buffer size
			 */
			if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
			{
				/* No change, display buffer size */
				printf( "\nBuffer size = %08x", eprom_size );
				return TRUE;
			}
			if( start_addr > EPROM_MAX )
			{
				/* No change, display buffer size */
				printf( "\nRequested buffer size %08x too large", start_addr );
				printf( "\nBuffer size = %08x", eprom_size );
				return TRUE;
			}
			if( start_addr < EPROM_MIN )
			{
				/* No change, display buffer size */
				printf( "\nRequested buffer size %08x too small", start_addr );
				printf( "\nBuffer size = %08x", eprom_size );
				return TRUE;
			}
			/* allocate new buffer */
			new_buff = (unsigned char *)realloc( eprom_buff, start_addr );
			if( new_buff == NULL )
			{
				printf( "\nFailed to allocate new buffer" );
				printf( "\nBuffer size = %08x", eprom_size );
				return TRUE;
			}

			/* assign the new buffer */
			eprom_size = start_addr;
			eprom_buff = new_buff;

			/* flag the buffer has changed */
			mod_flag = TRUE;
			return TRUE;
		}
		else if( str_equal( cmdptr, "A", len ) || str_equal( cmdptr, "address", len ))
		{
			/*
			 * set / display transfer Address
			 */
			if( (arglen = get_address( &cmdline[lineptr], &start_addr )) == 0 )
			{
				/* No argument, display transfer address */
				printf( "\nTransfer address = %08x", trans_addr );
				return TRUE;
			}
			trans_addr = start_addr;
			mod_flag = TRUE;
			return TRUE;
		} 
		else if( str_equal( cmdptr, "X", len ) || str_equal( cmdptr, "execute", len ))
		{
			/*
			 * Execute command script
			 * We can onle do this if we are using stdin
			 */
			if( cmdfp != stdin )
			{
				printf( "\nWe cannot nest redirected input " );
				return TRUE;
			}

			arglen = get_string( &cmdline[lineptr], &filename[0] );
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
int main(int argc, char *argv[])
{	
	printf( "*** EPROM Editor ***\n" );
	auxflag = FALSE;
	format_type = BINARY;
	addr   = 0;
	offset = 0;

	if(argc >= 2)
	{
		/* check for auxillary command file */
		if((cmdfp = fopen( argv[1], "r" )) == NULL)
		{
			printf("can't open input on %s\n", argv[1] );
			printf("Usage: %s [command-file]\n", argv[0] );
			return 1;
		}
		auxflag = TRUE;
	} else 
	{
		/* no auxillary command file specified, use stdin */
		cmdfp = stdin;
	}

	eprom_top = 0;
	eprom_size = 0x00010000;
	eprom_buff = (unsigned char *)malloc( eprom_size );
	if( eprom_buff == NULL )
	{
		printf( "\nFailed to allocate buffer\n" );
		printf( "*** exit epedit ***\n" );
		return 1;
	}

	printf( "Buffer min address = %08x ", offset );
	printf( "Buffer max address = %08x ", offset + eprom_size - 1 );
	printf( "Buffer size = %08x \n", eprom_size );
	printf( "Enter 'H', '?' or 'help' to list commands\n" );

	do {
		read_command( cmdbuff );
	} while( parse_command( cmdbuff ) );
	free( eprom_buff );
	printf( "\n*** exit epedit ***\n" );
	return 0;
}

