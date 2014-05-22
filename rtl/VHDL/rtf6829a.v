`timescale 1ns / 1ps
//=============================================================================
//        __
//   \\__/ o\    (C) 2012,2013  Robert Finch
//    \  __ /    All rights reserved.
//     \/_//     robfinch<remove>@opencores.org
//       ||
//  
//	rtf6829a.v
//  - memory mapper
//
//  
// This source file is free software: you can redistribute it and/or modify 
// it under the terms of the GNU Lesser General Public License as published 
// by the Free Software Foundation, either version 3 of the License, or     
// (at your option) any later version.                                      
//                                                                          
// This source file is distributed in the hope that it will be useful,      
// but WITHOUT ANY WARRANTY; without even the implied warranty of           
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the            
// GNU General Public License for more details.                             
//                                                                          
// You should have received a copy of the GNU General Public License        
// along with this program.  If not, see <http://www.gnu.org/licenses/>.    
//                              
//                                            
// Webpack 14.4 xc6slx45-3csg324
// 620 LUTs / 33 FF's / 270 MHz
//=============================================================================
//
module rtf6829a(reset_n, num, clk, dma, rw_n, dbi, dbo, adr, padr_o, wp_o, inta_i);
parameter pIOAddress = 16'hF800;
parameter pInterruptWrites = 4'd7;
input reset_n;			// active low reset
input [2:0] num;		// mmu number (hard code 0-7)
input clk;				// '02' clock
input dma;
input rw_n;				// 1=read,0=write
input [7:0] dbi;		// data bus input
output [7:0] dbo;		// data bus output
reg [7:0] dbo;
input [15:0] adr;		// address
output [23:0] padr_o;	// mapped physical address
reg [23:0] padr_o;
output wp_o;			// write protect
reg wp_o;
input inta_i;

wire cs = adr[15:7]==pIOAddress[15:7];	// circuit select
reg [2:0] kvmmu;		// key value for mmu
reg [12:0] map [1023:0];
reg [1023:0] wp;

reg [3:0] fuse;
reg ofuse3;				// detect undeflow for a single cycle
reg [7:0] accessKey;
reg [7:0] operateKey;
reg su;					// 1=system task#0 active
reg forceHi;
wire forceHi2 = cs;
reg [3:0] icnt;			// interrupt cycle counter

wire IsInta = icnt>=pInterruptWrites;

wire [7:0] oKey =
	dma ? 8'd1 :
	su ? 8'd0 :
	operateKey
	;

wire [9:0] mwa = {accessKey[4:0],adr[5:1]};
wire [9:0] mrad = {accessKey[4:0],adr[5:1]};
wire [9:0] mra = {oKey[4:0],adr[15:11]};
wire [12:0] mo = map[mra];
wire wpo = wp[mra];
wire thisMMU = kvmmu==accessKey[7:5];

// Register write logic

always @(negedge clk)
if (reset_n==1'b0) begin
	accessKey <= 8'd0;
	operateKey <= 8'd0;
	kvmmu <= 3'd0;
	su <= 1'b1;
	fuse <= 4'hF;
	ofuse3 <= 1'b1;
	forceHi <= 1'b1;
end
else begin
	ofuse3 <= fuse[3];
	if (!fuse[3] && !dma)
		fuse <= fuse - 4'd1;
	if (fuse[3]&!ofuse3) begin
		$display("clearing s bit");
		su <= 1'b0;
	end
	if (IsInta|inta_i) begin
		$display("setting s bit");
		su <= 1'b1;
	end

	if (cs && rw_n==1'b0 && su) begin
		casex(adr[6:0])
		7'b0xxxxx0:
			if (thisMMU) begin
				map[mwa][12:8] <= dbi[4:0];
				wp[mwa] <= dbi[7];
			end
		7'b0xxxxx1:
			if (thisMMU)
				map[mwa][7:0] <= dbi;
		7'b1000xxx:
			if ((adr[2:0]==num) && oKey==8'd0) begin
				kvmmu <= dbi[2:0];
				forceHi <= 1'b0;
			end
		7'h49:	fuse <= {1'b0,dbi[2:0]};
		7'h4A:	accessKey <= dbi;
		7'h4B:	operateKey <= dbi;
		default:	;
		endcase
	end
end



// Register read logic
//
always @(cs or adr or oKey or thisMMU or kvmmu or su or fuse or accessKey or operateKey or num)
begin
	if ((adr[2:0]==num) && oKey==8'd0 && adr[6:3]==4'b1000 && cs)
		dbo <= {5'd0,kvmmu};
	else if (cs && thisMMU) begin
		casex(adr[6:0])
		7'b0xxxxx0:
			dbo <= {wp[mrad],2'b0,map[mrad][12:8]};
		7'b0xxxxx1:
			dbo <= map[mrad][7:0];
		7'b1000xxx:
			if ((adr[2:0]==num) && oKey==8'd0)
				dbo <= kvmmu;
			else
				dbo <= 8'd0;
		7'h48:	dbo <= su;
		7'h49:	dbo <= fuse;
		7'h4A:	dbo <= accessKey;
		7'h4B:	dbo <= operateKey;
		default:	dbo <= 8'h00;
		endcase
	end
	else
		dbo <= 8'h00;
end

// Counter to detect interrupt acknowledge as three or more consecutive write accesses
//
always @(negedge clk)
if (reset_n==1'b0)
	icnt <= 4'd0;
else begin
	if (rw_n==1'b0)
		icnt <= icnt + 4'd1;
	else
		icnt <= 4'd0;
end

always @(forceHi or forceHi2 or kvmmu or mo or adr)
	if (forceHi|forceHi2) begin
		padr_o[23:11] <= 13'h1FFF;
		padr_o[10:0] <= adr[10:0];
		wp_o <= 1'b1;
	end
	else if (oKey[7:5]==kvmmu) begin
		padr_o[23:11] <= mo;
		padr_o[10:0] <= adr[10:0];
		wp_o <= wpo;
	end
	// Allow wire-or'ing of outputs
	else begin
		padr_o[23:11] <= 13'd0;
		padr_o[10:0] <= adr[10:0];
		wp_o <= 1'b0;
	end

endmodule
