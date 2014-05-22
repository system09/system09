// S19toVHD.cpp : Defines the entry point for the console application.
//

/*
* epedit
*
* binary file editer program
*/
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

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
	fclose( fp_in );
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

char *bin_string( int num, int num_len )
{
        static char retbuf[33];
        char *p;
        int i;

        p = &retbuf[sizeof(retbuf)-1];
        *p = '\0';
        for (i=0; i<num_len; i++ )
        {
            *--p = "01"[num % 2];
            num >>= 1;
        } 
        return p;
}

/*
* save VHDL hexadecimal file 4KBit Block RAM (Spartan 2)
*/

void save_vhdl_b4( FILE *fp_out, char *entity_name, int start_addr, int end_addr )
{
   int addr;
   int i,j;
   int byte;
   int rom_num, rom_max, rom_len;
   int addr_len;

   rom_max = (end_addr+1-start_addr)/512;
   rom_len = 8;
   addr_len = (int)(log((double)(end_addr-start_addr))/log(2.0));

   fprintf(fp_out, "library IEEE;\n");
   fprintf(fp_out, "   use IEEE.std_logic_1164.all;\n");
   fprintf(fp_out, "   use IEEE.std_logic_arith.all;\n");
   fprintf(fp_out, "library unisim;\n");
   fprintf(fp_out, "   use unisim.vcomponents.all;\n");
   fprintf(fp_out, "\n");
   fprintf(fp_out, "entity %s is\n", entity_name);
   fprintf(fp_out, "   port(\n");
   fprintf(fp_out, "      clk    : in  std_logic;\n");
   fprintf(fp_out, "      rst    : in  std_logic;\n");
   fprintf(fp_out, "      cs     : in  std_logic;\n");
   fprintf(fp_out, "      rw     : in  std_logic;\n");
   fprintf(fp_out, "      addr   : in  std_logic_vector(%d downto 0);\n", addr_len);
   fprintf(fp_out, "      rdata  : out std_logic_vector(7 downto 0);\n");
   fprintf(fp_out, "      wdata  : in  std_logic_vector(7 downto 0)\n");
   fprintf(fp_out, "   );\n");
   fprintf(fp_out, "end %s;\n", entity_name);
   fprintf(fp_out, "\n");
   fprintf(fp_out, "architecture rtl of %s is\n", entity_name);
   fprintf(fp_out, "\n");
   fprintf(fp_out, "   type data_array is array(0 to %d) of std_logic_vector(7 downto 0);\n",rom_max-1);
   fprintf(fp_out, "   signal xdata : data_array;\n");
   fprintf(fp_out, "   signal en : std_logic_vector(%d downto 0);\n", rom_max-1);
   fprintf(fp_out, "   signal we : std_logic;\n");
   fprintf(fp_out, "\n");
   fprintf(fp_out, "component RAMB4_S8\n");
   fprintf(fp_out, "generic (\n");
   fprintf(fp_out, "   INIT_00, INIT_01, INIT_02, INIT_03,\n");
   fprintf(fp_out, "   INIT_04, INIT_05, INIT_06, INIT_07,\n");
   fprintf(fp_out, "   INIT_08, INIT_09, INIT_0A, INIT_0B,\n");
   fprintf(fp_out, "   INIT_0C, INIT_0D, INIT_0E, INIT_0F : bit_vector (255 downto 0)\n");
   fprintf(fp_out, "    );\n");
   fprintf(fp_out,"   port (\n");
   fprintf(fp_out,"      clk, we, en, rst : in std_logic;\n");
   fprintf(fp_out,"      addr : in std_logic_vector(8 downto 0);\n");
   fprintf(fp_out,"      di   : in std_logic_vector(7 downto 0);\n");
   fprintf(fp_out,"      do   : out std_logic_vector(7 downto 0)\n");
   fprintf(fp_out,"      );\n");
   fprintf(fp_out,"     end component RAMB4_S8;\n");
   fprintf(fp_out, "\n");
   fprintf(fp_out,"   begin\n");
   fprintf(fp_out, "\n");

   addr=start_addr;
   for( rom_num=0; rom_num<rom_max; rom_num++)
   {
      fprintf(fp_out, "   ROM%02x: RAMB4_S8\n", rom_num );
      fprintf(fp_out, "      generic map (\n");
      for( j=0; j<16; j++ )
      {
         fprintf( fp_out, "         INIT_%02x => x\"", j );
         for(i=31; i>=0; i-- )
         {
            byte = (int)eprom_buff[(addr - offset + i) % EPROM_MAX];
            putc( hex_str[(byte >>4) & 0xf], fp_out ); 
            putc( hex_str[byte & 0xf], fp_out ); 
         }
         if (j < 15) 
         {
            fprintf( fp_out, "\",\n" );
         } else 
         {
            fprintf( fp_out, "\"\n" );
         }
         addr+=32;
      }
      fprintf(fp_out, "      )\n");
      fprintf(fp_out, "      port map (\n");
      fprintf(fp_out, "         clk     => clk,\n");
      fprintf(fp_out, "         en      => en(%d),\n", rom_num );
      fprintf(fp_out, "         we      => we,\n");
      fprintf(fp_out, "         rst     => rst,\n");
      fprintf(fp_out, "         addr    => addr(8 downto 0),\n");
      fprintf(fp_out, "         di      => wdata,\n");
      fprintf(fp_out, "         do      => xdata(%d)\n", rom_num );
      fprintf(fp_out, "      );\n");
      fprintf(fp_out, "\n");
   }

   fprintf(fp_out, "   rom_glue: process (cs, rw, addr, xdata)\n");
   fprintf(fp_out, "   begin\n");
   if( addr_len > rom_len )
   {
      fprintf(fp_out, "      en <= (others=>'0');\n");
      fprintf(fp_out, "      case addr(%d downto %d) is\n", addr_len, rom_len+1 );

      for( rom_num=0; rom_num<rom_max; rom_num++ )
      {
         fprintf(fp_out, "      when \"%s\" =>\n", bin_string(rom_num, addr_len-rom_len) );
         fprintf(fp_out, "         en(%d)  <= cs;\n", rom_num );
         fprintf(fp_out, "         rdata  <= xdata(%d);\n", rom_num);
      }

      fprintf(fp_out, "      when others =>\n");
      fprintf(fp_out, "         null;\n");
      fprintf(fp_out, "      end case;\n");
   }
   else
   {
      fprintf(fp_out, "      en(0)  <= cs;\n" );
      fprintf(fp_out, "      rdata  <= xdata(0);\n" );
   }
   fprintf(fp_out, "      we <= not rw;\n");
   fprintf(fp_out, "   end process;\n");
   fprintf(fp_out, "end architecture rtl;\n\n");
}


/*
* save VHDL hexadecimal file 16 KBit Block RAM (Spartan 3)
*/

void save_vhdl_b16( FILE *fp_out, char *entity_name, int start_addr, int end_addr )
{
   int addr;
   int i,j;
   int byte;
   int rom_num, rom_max, rom_len;
   int addr_len;

   rom_max = (end_addr+1-start_addr)/2048;
   rom_len = 10;
   addr_len = (int)(log((double)(end_addr-start_addr))/log(2.0));

   fprintf(fp_out, "library IEEE;\n");
   fprintf(fp_out, "   use IEEE.std_logic_1164.all;\n");
   fprintf(fp_out, "   use IEEE.std_logic_arith.all;\n");
   fprintf(fp_out, "library unisim;\n");
   fprintf(fp_out, "   use unisim.vcomponents.all;\n");
   fprintf(fp_out, "\n");
   fprintf(fp_out, "entity %s is\n", entity_name);
   fprintf(fp_out, "   port(\n");
   fprintf(fp_out, "      clk    : in  std_logic;\n");
   fprintf(fp_out, "      rst    : in  std_logic;\n");
   fprintf(fp_out, "      cs     : in  std_logic;\n");
   fprintf(fp_out, "      rw     : in  std_logic;\n");
   fprintf(fp_out, "      addr   : in  std_logic_vector(%d downto 0);\n", addr_len);
   fprintf(fp_out, "      rdata  : out std_logic_vector(7 downto 0);\n");
   fprintf(fp_out, "      wdata  : in  std_logic_vector(7 downto 0)\n");
   fprintf(fp_out, "   );\n");
   fprintf(fp_out, "end %s;\n", entity_name);
   fprintf(fp_out, "\n");
   fprintf(fp_out, "architecture rtl of %s is\n", entity_name);
   fprintf(fp_out, "\n");
   fprintf(fp_out, "   type data_array is array(0 to %d) of std_logic_vector(7 downto 0);\n",rom_max-1);
   fprintf(fp_out, "   signal xdata : data_array;\n");
   fprintf(fp_out, "   signal en : std_logic_vector(%d downto 0);\n", rom_max-1);
   fprintf(fp_out, "   signal dp : std_logic_vector(%d downto 0);\n", rom_max-1);
   fprintf(fp_out, "   signal we : std_logic;\n");
   fprintf(fp_out, "\n");
   fprintf(fp_out, "component RAMB16_S9\n");
   fprintf(fp_out, "generic (\n");
   fprintf(fp_out, "   INIT_00, INIT_01, INIT_02, INIT_03,\n");
   fprintf(fp_out, "   INIT_04, INIT_05, INIT_06, INIT_07,\n");
   fprintf(fp_out, "   INIT_08, INIT_09, INIT_0A, INIT_0B,\n");
   fprintf(fp_out, "   INIT_0C, INIT_0D, INIT_0E, INIT_0F,\n");
   fprintf(fp_out, "   INIT_10, INIT_11, INIT_12, INIT_13,\n");
   fprintf(fp_out, "   INIT_14, INIT_15, INIT_16, INIT_17,\n");
   fprintf(fp_out, "   INIT_18, INIT_19, INIT_1A, INIT_1B,\n");
   fprintf(fp_out, "   INIT_1C, INIT_1D, INIT_1E, INIT_1F,\n");
   fprintf(fp_out, "   INIT_20, INIT_21, INIT_22, INIT_23,\n");
   fprintf(fp_out, "   INIT_24, INIT_25, INIT_26, INIT_27,\n");
   fprintf(fp_out, "   INIT_28, INIT_29, INIT_2A, INIT_2B,\n");
   fprintf(fp_out, "   INIT_2C, INIT_2D, INIT_2E, INIT_2F,\n");
   fprintf(fp_out, "   INIT_30, INIT_31, INIT_32, INIT_33,\n");
   fprintf(fp_out, "   INIT_34, INIT_35, INIT_36, INIT_37,\n");
   fprintf(fp_out, "   INIT_38, INIT_39, INIT_3A, INIT_3B,\n");
   fprintf(fp_out, "   INIT_3C, INIT_3D, INIT_3E, INIT_3F : bit_vector (255 downto 0)\n");
   fprintf(fp_out, "   );\n");
   fprintf(fp_out, "\n");
   fprintf(fp_out, "   port (\n");
   fprintf(fp_out, "      clk  : in  std_logic;\n");
   fprintf(fp_out, "      ssr  : in  std_logic;\n");
   fprintf(fp_out, "      en   : in  std_logic;\n");
   fprintf(fp_out, "      we   : in  std_logic;\n");
   fprintf(fp_out, "      addr : in  std_logic_vector(10 downto 0);\n");
   fprintf(fp_out, "      di   : in  std_logic_vector( 7 downto 0);\n");
   fprintf(fp_out, "      dip  : in  std_logic_vector( 0 downto 0);\n");
   fprintf(fp_out, "      do   : out std_logic_vector( 7 downto 0);\n");
   fprintf(fp_out, "      dop  : out std_logic_vector( 0 downto 0)\n");
   fprintf(fp_out, "      );\n");
   fprintf(fp_out, "     end component RAMB16_S9;\n");
   fprintf(fp_out, "\n");
   fprintf(fp_out, "   begin\n");
   fprintf(fp_out, "\n");
        
   addr=start_addr;
   for( rom_num=0; rom_num<rom_max; rom_num++)
   {
      fprintf(fp_out, "   ROM%02x: RAMB16_S9\n", rom_num );
	  fprintf(fp_out, "      generic map (\n");
	  for( j=0; j<64; j++ )
   	  {
         fprintf( fp_out, "         INIT_%02x => x\"", j );
         for(i=31; i>=0; i-- )
		 {
            byte = (int)eprom_buff[(addr - offset + i) % EPROM_MAX];
            putc( hex_str[(byte >>4) & 0xf], fp_out ); 
            putc( hex_str[byte & 0xf], fp_out ); 
         }
         if (j < 63) 
         {
            fprintf( fp_out, "\",\n" );
         } else 
         {
            fprintf( fp_out, "\"\n" );
         }
         addr+=32;
      }
      fprintf(fp_out, "      )\n");
      fprintf(fp_out, "      port map (\n");
      fprintf(fp_out, "         clk     => clk,\n");
      fprintf(fp_out, "         ssr     => rst,\n");
      fprintf(fp_out, "         en      => en(%d),\n", rom_num );
      fprintf(fp_out, "         we      => we,\n");
      fprintf(fp_out, "         addr    => addr(10 downto 0),\n");
      fprintf(fp_out, "         di      => wdata,\n");
      fprintf(fp_out, "         dip(0)  => dp(%d),\n", rom_num );
      fprintf(fp_out, "         do      => xdata(%d),\n", rom_num );
      fprintf(fp_out, "         dop(0)  => dp(%d)\n", rom_num );
      fprintf(fp_out, "      );\n");
   }

   fprintf(fp_out, "   rom_glue: process (cs, rw, addr, xdata)\n");
   fprintf(fp_out, "   begin\n");
   if( addr_len > rom_len )
   {
      fprintf(fp_out, "      en <= (others=>'0');\n");
      fprintf(fp_out, "      case addr(%d downto %d) is\n", addr_len, rom_len+1 );

      for( rom_num=0; rom_num<rom_max; rom_num++ )
      {
         fprintf(fp_out, "      when \"%s\" =>\n", bin_string(rom_num, addr_len-rom_len) );
         fprintf(fp_out, "         en(%d)  <= cs;\n", rom_num );
         fprintf(fp_out, "         rdata  <= xdata(%d);\n", rom_num);
      }

      fprintf(fp_out, "      when others =>\n");
      fprintf(fp_out, "         null;\n");
      fprintf(fp_out, "      end case;\n");
   }
   else
   {
      fprintf(fp_out, "      en(0)  <= cs;\n");
      fprintf(fp_out, "      rdata  <= xdata(0);\n");
   }
   fprintf(fp_out, "      we <= not rw;\n");
   fprintf(fp_out, "   end process;\n");
   fprintf(fp_out, "end architecture rtl;\n\n");
}


/*
* epedit main program
*/
int main(int argc, char* argv[])
{
	int start_addr;
	int end_addr;
	int arglen;
	char entity_name_buf[512];
        char file_name_buf[512];
	char hdl_file_buf[1024];
	char buf[1024];
	char *curpos;
	FILE *fp_out;

	if (argc < 5)
    {
		printf("Usage: s19tovhd <s19 file> <base vhd file> <vhdl base entity name> <addr> [<addr> ...]\n");
		return(-1);
	}
	printf("Reading Motorola S19 from file '%s'\n", argv[1]);
	printf("VHDL file name '%s'\n", argv[2]);
	printf("Base RAM/ROM entity name is '%s'\n", argv[3]);
	load_mot( argv[1] );

	sprintf(file_name_buf, "%s_b16.vhd", argv[2]);
	if( (fp_out = fopen( &file_name_buf[0], "w" )) == NULL ) 
	{
		printf( "\nCan't open '%s' for write ", argv[2] );
		return(-1);
	}

	for (int cnt=4; cnt<argc; cnt++) 
	{
		if( (arglen = get_address( argv[cnt], &start_addr )) == 0 ) 
		{
			printf("Expected hex start address, got %s\n", argv[cnt]);
			continue;
		}
		end_addr = start_addr + 2047;
		sprintf(entity_name_buf, "%s_%4X", argv[3], start_addr);

		printf("Entity '%s' (address range '0x%4X'-'0x%4X') written to file '%s'\n", 
			entity_name_buf, start_addr, end_addr, argv[2]);
		save_vhdl_b16( fp_out, entity_name_buf, start_addr, end_addr );
	}
	if (fp_out) fclose(fp_out);

	sprintf(file_name_buf, "%s_b4.vhd", argv[2]);
	if( (fp_out = fopen( &file_name_buf[0], "w" )) == NULL ) 
	{
		printf( "\nCan't open '%s' for write ", argv[2] );
		return(-1);
	}

	for (int cnt=4; cnt<argc; cnt++) 
	{
		if( (arglen = get_address( argv[cnt], &start_addr )) == 0 ) 
		{
			printf("Expected hex start address, got %s\n", argv[cnt]);
			continue;
		}
		end_addr = start_addr + 2047;
		sprintf(entity_name_buf, "%s_%4X", argv[3], start_addr);

		printf("Entity '%s' (address range '0x%4X'-'0x%4X') written to file '%s'\n", 
			entity_name_buf, start_addr, end_addr, argv[2]);
		save_vhdl_b4( fp_out, entity_name_buf, start_addr, end_addr );
	}
	if (fp_out) fclose(fp_out);
	return(0);
}

