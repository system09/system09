`timescale 1ns / 1ps
//=============================================================================
//        __
//   \\__/ o\    (C) 2012,2013  Robert Finch
//    \  __ /    All rights reserved.
//     \/_//     robfinch<remove>@opencores.org
//       ||
//  
//	rtf6829a_tb.v
//  - memory mapper test bench
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
module rtf6829a_tb();

reg clk;
reg rst;
reg rw;
reg [15:0] adr;
wire [23:0] padr;
reg [7:0] dat;
reg [7:0] cnt;
integer n;

initial begin
	clk = 1'b0;
	rst = 1'b0;
	rst = #100 1'b1;
	rst = #100 1'b0;
end

always #20 clk = ~clk;

always @(posedge clk)
if (rst) begin
	cnt <= 8'h00;
	adr <= 16'h0000;
end
else begin
cnt <= cnt + 8'd1;
casex(cnt)
8'b0011xxx0:
		begin
		adr <= 16'hF800 + cnt;
		dat <= 8'h1F;
		rw <= 1'b0;
		end
// setup a linear map
8'b00xxxxx0:
		begin
		adr <= 16'hF800 + cnt;
		dat <= 8'h00;
		rw <= 1'b0;
		end
8'b00xxxxx1:
		begin
		adr <= 16'hF800 + cnt;
		dat <= cnt[5:1];
		rw <= 1'b0;
		end
// This should cause the mmu to start outputing address translations
8'b01000000:
		begin
		adr <= 16'hF840;
		dat <= 8'h00;
		rw <= 1'b0;
		end
8'b01000001:
		begin
		adr <= 16'h1000;
		rw <= 1'b1;
		end
8'b01000010:
		begin
		adr <= 16'hF000;
		rw <= 1'b1;
		end
8'h43,8'h44,8'h45:
		begin
		adr <= 16'h0100-cnt[3:0];
		rw <= 1'b0;
		end
8'h46:
		begin
		adr <= 16'h0100-cnt[3:0];
		rw <= 1'b1;
		end
8'h47:
		begin
		$display("Setting fuse");
		adr <= 16'hF849;
		rw <= 1'b0;
		dat <= 8'h4;
		end
8'h48:
		begin
		adr <= 16'h0000;
		rw <= 1'b1;
		end
8'h50:	begin
		for (n = 0; n < 64; n = n + 1)
			$display("map[%d]=%h",n,u1.map[n]);
		$stop;
		end
endcase
end
always @(negedge clk)
$display("cnt%h,adr=%h,padr=%h", cnt, adr, padr);

rtf6829a #(.pInterruptWrites(3)) u1
(
	.reset_n(~rst),
	.num(0),
	.clk(clk),
	.dma(1'b0),			// no DMA
	.rw_n(rw),
	.dbi(dat),
	.dbo(),
	.adr(adr),
	.padr_o(padr),
	.wp_o()				// not used
);

endmodule

