module vesp_risc_tb();

    reg rst;
    wire clk;
    parameter word_size = 16;
    parameter address_size = 12;
    reg [8:0] k;

    Clock_Unit M1 (clk);
    vesp_risc M2 (clk,rst);

    wire[word_size-1:0] word0,word1,word2,word3, word4, word5, word6, word7, word8,
                        word9, word10, word11, word12, word13, word14, word15, word16,
                        word17, word18;
    wire[word_size-1:0] word128,word129,word130,word131,word132,word133,word134,word135,word136,word137,word138,word139,word140,word255;

    assign word0 = M2.M1.memory[0];
    assign word1 = M2.M1.memory[1];
    assign word2 = M2.M1.memory[2];
    assign word3 = M2.M1.memory[3];
    assign word4 = M2.M1.memory[4];
    assign word5 = M2.M1.memory[5];
    assign word6 = M2.M1.memory[6];
    assign word7 = M2.M1.memory[7];
    assign word8 = M2.M1.memory[8];
    assign word9 = M2.M1.memory[9];
    assign word10 = M2.M1.memory[10];
    assign word11 = M2.M1.memory[11];
    assign word12 = M2.M1.memory[12];
    assign word13 = M2.M1.memory[13];
    assign word14 = M2.M1.memory[14];
    assign word15 = M2.M1.memory[15];
    assign word16 = M2.M1.memory[16];
    assign word17 = M2.M1.memory[17];
    assign word18 = M2.M1.memory[18];

    assign word128 = M2.M1.memory[128];
    assign word129 = M2.M1.memory[129];
    assign word130 = M2.M1.memory[130];
    assign word131 = M2.M1.memory[131];
    assign word132 = M2.M1.memory[132];
    assign word133 = M2.M1.memory[133];
    assign word134 = M2.M1.memory[134];
    assign word135 = M2.M1.memory[135];
    assign word136 = M2.M1.memory[136];
    assign word137 = M2.M1.memory[137];
    assign word138 = M2.M1.memory[138];
    assign word139 = M2.M1.memory[139];
    assign word140 = M2.M1.memory[140];
    assign word255 = M2.M1.memory[255];

    initial #2800 $finish;

    //flush memory
    initial begin
    #2 rst = 0; for (k=0; k<=255; k=k+1) M2.M1.memory[k] = 0; #10 rst=1;
    end

    initial begin: Load_program
    #5

    M2.M1.memory[0] =  16'h2000;
    M2.M1.memory[1] =  16'h1458;
    M2.M1.memory[2] =  16'h8000;
    M2.M1.memory[3] =  16'hD000;
    M2.M1.memory[4] =  16'h7000; 
    end

    initial begin
        $dumpfile("vesprisc.vcd");
        $dumpvars(0);
    end

endmodule

module Clock_Unit (output reg clock);
    parameter delay = 0;
    parameter half_cycle = 10;
    initial begin #delay clock = 0; forever #half_cycle clock = ~clock; 
    end
endmodule

