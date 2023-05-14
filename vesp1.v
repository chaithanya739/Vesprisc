module vesp1(clk,rst);
    parameter address_size = 12;
    parameter word_size = 16;
    parameter Sel1_size = 3;
    parameter Sel2_size = 2;
    input clk,rst;
    wire [Sel1_size-1:0] Sel_bus1;
    wire [Sel2_size-1:0] Sel_bus2;
    wire [address_size-1:0] address;
    wire [word_size-1:0] instruction;
    wire [word_size-1:0] mem_word;
    wire Load_IR,Load_MAR,Load_MDR,Load_PC,Inc_PC,Load_zero,Load_sign,read,write;
    wire [word_size-1:0] Bus1;
    wire [word_size-1:0] Bus2;
    wire [word_size-1:0] A;
    wire [word_size-1:0] B;

    processor P1 (address,instruction,sign,zero,Bus1,Load_PC,Inc_PC,Load_IR,Load_MAR,Load_MDR,mem_word,A,B,Load_sign,Load_zero,Sel_bus1,Sel_bus2,clk,rst);
    Control C1 (write,read,Load_IR,Load_PC,Inc_PC,Load_sign,Load_zero,Load_MAR,Load_MDR,Sel_bus1,Sel_bus2,instruction,zero,sign,clk,rst);
    Memory M1 (.data_out(mem_word),.A(A),.B(B),.data_in(Bus1),.address(address),.clk(clk),.read(read),.write(write));

endmodule

module processor (address,instruction,sign,zero,Bus1,Load_PC,Inc_PC,Load_IR,Load_MAR,Load_MDR,mem_word,A,B,Load_sign,Load_zero,Sel_bus1,Sel_bus2,clk,rst);
    parameter word_size = 16;
    parameter address_size = 12;
    parameter Sel1_size = 3;
    parameter Sel2_size = 2;
    parameter opcode_size = 4;
    output [address_size-1:0] address;
    output [word_size-1:0] instruction;
    output sign;
    output zero;
    output [word_size-1:0] Bus1;
    input Load_PC,Inc_PC,Load_MAR,Load_MDR,Load_IR;
    input [word_size-1:0] mem_word;
    input [Sel1_size-1:0] Sel_bus1;
    input [Sel2_size-1:0] Sel_bus2;
    input [word_size-1:0] A;
    input [word_size-1:0] B;
    input clk,rst;
    input Load_sign;
    input Load_zero;
    wire [word_size-1:0] Bus2;
    wire [opcode_size-1:0] opcode = instruction[word_size-1 : word_size-opcode_size];
    wire [word_size-opcode_size-1:0] IRoperand = instruction[word_size-opcode_size-1:0];
    wire [word_size-opcode_size-1:0] MDR_operand = MDR_out[word_size-opcode_size-1:0];
    wire [word_size-1:0] MDR_out;
    wire [word_size-1:0] alu_out;
    wire [word_size-1:0] IR_out;
    wire [address_size-1:0] PC_out;


    Program_counter PC1 (PC_out,Bus2,Load_PC,Inc_PC,clk,rst);
    Intruction_register IR1 (instruction,Bus2,Load_IR,clk,rst);
    MAR_Unit MAR1 (address,Bus2,Load_MAR,clk,rst);
    MDR_unit MDR1 (MDR_out,mem_word,Load_MDR,clk,rst);
    Multiplexer_4ch A1 (Bus1,alu_out,PC_out,instruction,IRoperand,MDR_out,Sel_bus1);
    Multiplexer_1ch A2 (Bus2,Bus1,MDR_out,MDR_operand,Sel_bus2);
    ALU ALU1 (alu_out,A,B,opcode);
    Zero_Unit Z1 (zero,A,Load_zero,clk,rst);
    signed_unit S1 (sign,A,Load_sign,clk,rst);

endmodule
    



module Multiplexer_4ch (mux_out,data_a,data_b,data_c,data_d,data_e,sel);

    parameter word_size = 16;
    parameter address_size = 12;
    output [word_size-1:0] mux_out;
    input [word_size-1:0] data_a;
    input [word_size-1:0] data_b;
    input [word_size-1:0] data_c;
    input [word_size-1:0] data_d;
    input [word_size-1:0] data_e;
    input [2:0]sel;

    assign mux_out = (sel == 0) ? data_a: (sel == 1)
                                ? data_b: (sel == 2)
                                ? data_c: (sel == 3)
                                ? data_d: (sel == 4)
                                ? data_e: 'bx;

endmodule

module Multiplexer_1ch (mux_out,data_a,data_b,data_c,sel);

    parameter word_size = 16;
    parameter address_size = 12;
    output [word_size-1:0] mux_out;
    input [word_size-1:0] data_a;
    input [word_size-1:0] data_b;
    input [word_size-1:0] data_c;
    input [1:0]sel;

    assign mux_out = (sel == 0) ? data_a: (sel == 1)
                                ? data_b: (sel == 2)
                                ? data_c: 'bx;

endmodule

module Zero_Unit (data_out,data_in,load,clk,rst);
    parameter word_size = 16;
    output  data_out;
    input [word_size-1:0] data_in;
    input load;
    input clk,rst;

    reg data_out;
    always @(posedge clk or negedge rst) begin
        if(rst==0) data_out <= 0; else if(load) data_out <= ~|data_in;
    end
endmodule

module signed_unit (data_out,data_in,load,clk,rst);
    parameter word_size = 16;
    output data_out;
    input signed [word_size-1:0] data_in;
    input load;
    input clk,rst;

    reg data_out;
    always @(posedge clk or negedge rst) begin
        if (rst==0) data_out <=0; 
        else if (load) begin
            if(data_in >> 15) begin
                data_out = 1;
            end
            else begin
                data_out = 0;
            end
        end
    end
endmodule

module MDR_unit (data_out,data_in,load,clk,rst);
    parameter word_size = 16;
    output [word_size-1:0] data_out;
    input [word_size-1:0] data_in;
    input load;
    input clk,rst;

    reg data_out;

    always @(posedge clk or negedge rst) begin
        if (rst==0) data_out <= 0; else if (load) data_out <= data_in;
    end

endmodule

module MAR_Unit (data_out,data_in,load,clk,rst);
    parameter address_size = 12;
    output [address_size-1:0] data_out;
    input [address_size-1:0] data_in;
    input load;
    input clk,rst;

    reg data_out;

    always @(posedge clk or negedge rst) begin
        if (rst==0) data_out <= 0; else if (load) data_out <= data_in[address_size-1:0];
    end

endmodule


module Intruction_register (data_out,data_in,load,clk,rst);
    parameter word_size = 16;
    output [word_size-1:0] data_out;
    input [word_size-1:0] data_in;
    input load;
    input clk,rst;

    reg data_out;

    always @(posedge clk or negedge rst) begin
        if (rst==0) data_out <= 0; else if (load) data_out <= data_in;
    end

endmodule


module Program_counter (data_out,data_in,Load_PC,Inc_PC,clk,rst);
    parameter address_size = 12;
    output [address_size-1:0] data_out;
    input [address_size-1:0] data_in;
    input Load_PC;
    input Inc_PC;
    input clk,rst;

    reg data_out;

    always @(posedge clk or negedge rst) begin
        if(rst == 0) data_out <= 12'h002; else if (Load_PC) data_out <= data_in; else if (Inc_PC)
        data_out <= data_out+1;
    end

endmodule


module ALU (alu_out,data_1,data_2,opcode);

    parameter word_size = 16;
    parameter opcode_size = 4;
    //opcodes
    parameter ADD = 4'b0000;
    parameter CMP = 4'b0001;
    parameter LDA = 4'b0010;
    parameter MOV = 4'b0011;
    parameter JMP = 4'b0100;
    parameter JEZ = 4'b0101;
    parameter JPS = 4'b0110;
    parameter HLT = 4'b0111;
    output [word_size-1:0] alu_out;
    input [word_size-1:0] data_1;
    input [word_size-1:0] data_2;
    input [opcode_size-1:0] opcode;
    reg alu_out;

    always @(opcode or data_1 or data_2) begin 
        case(opcode)
        ADD: alu_out = data_1 + data_2;
        CMP: alu_out = ~data_1;
        default alu_out = 0;
        endcase
    end

endmodule


module Control (write,read,Load_IR,Load_PC,Inc_PC,Load_sign,Load_zero,Load_MAR,Load_MDR,Sel_bus1,Sel_bus2,instruction,zero,sign,clk,rst);

    parameter word_size = 16;
    parameter address_size = 12;
    parameter opcode_size = 4;
    parameter Sel1_size = 3;
    parameter Sel2_size = 2;
    parameter state_size = 5;

    parameter S_idle = 0, S_fet1 = 1, S_fet2 = 2, S_fet3 = 3, S_fet4 = 4;
    parameter S_dec = 5, S_exc1=6, S_rd1 = 7, S_rd2 = 8, S_rd3 = 9, S_rd4 = 10;
    parameter S_mv1 = 11, S_mv2 = 12, S_mv3 = 13, S_mv4 = 14, S_mv5 = 15, S_mv6 = 16,S_mv7=17,S_mv8=18,S_halt=19;

    parameter ADD = 0, CMP = 1, LDA = 2, MOV = 3, JMP = 4, JEZ = 5, JPS = 6, HLT = 7;

    output write, read, Load_IR,Load_PC,Inc_PC,Load_MAR,Load_MDR,Load_sign,Load_zero;
    output [Sel1_size-1:0] Sel_bus1;
    output [Sel2_size-1:0] Sel_bus2;
    input [word_size-1:0] instruction;
    input zero;
    input sign; 
    input clk;
    input rst;

    reg[state_size-1:0]state, next_state;
    reg Load_IR,Load_MAR,Load_MDR,Load_PC,Inc_PC,write,read,Load_sign,Load_zero;
    reg err_flag;
    reg Sel_alu,Sel_PC,Sel_IR,Sel_IRoperand,Sel_MDR,Sel_MDRoperand,Sel_MDR1,Sel_Bus1;

    wire [opcode_size-1:0] opcode = instruction[word_size-1:word_size-opcode_size];
    wire [word_size-opcode_size-1:0] operand = instruction [word_size-opcode_size-1:0];

    assign Sel_bus1[Sel1_size-1:0] = Sel_alu ? 0:
                                     Sel_PC ? 1:
                                     Sel_IR ? 2:
                                     Sel_IRoperand ? 3:
                                     Sel_MDR1 ? 4: 3'bx;

    assign Sel_bus2[Sel2_size-1:0] = Sel_Bus1 ? 0:
                                     Sel_MDR ? 1:
                                     Sel_MDRoperand? 2: 2'bx;
    
    always @(posedge clk or negedge rst) begin
        if (rst == 0) state <= S_idle; else state <= next_state; end

    always @(state or opcode or zero or sign ) begin
        Sel_alu = 0; Sel_PC = 0; Sel_IR = 0; Sel_IRoperand = 0; Sel_MDR = 0; Sel_MDRoperand = 0; Sel_Bus1=0; Sel_MDR1 = 0;
        Load_IR = 0; Load_MAR=0; Load_MDR=0; Load_PC = 0; write =0; read=0; Inc_PC =0; Load_sign = 0; Load_zero = 0;
        err_flag =0;
        next_state =  state;
        case(state)
            S_idle: next_state = S_fet1;
            S_fet1: begin
                        next_state = S_fet2;
                        Sel_PC = 1;
                        Sel_Bus1 = 1;
                        Load_MAR = 1;
                    end
            S_fet2: begin
                        next_state = S_fet3;
                        Load_MAR = 1;
                        read = 1;
                        Load_MDR = 1;
                    end
            S_fet3: begin
                        next_state = S_fet4;
                        Load_MDR = 1;
                    end
            S_fet4: begin
                        next_state = S_dec;
                        Sel_MDR = 1;
                        Load_IR = 1;
                        Inc_PC = 1;
            end

            S_dec: begin 
                    case(opcode)
                    ADD,CMP: begin
                        next_state = S_exc1;
                        Load_MAR = 1;
                        Sel_IRoperand = 1;
                        Sel_Bus1 = 1;
                    end
                    LDA: begin
                        next_state = S_rd1;
                        Sel_PC = 1;
                        Load_MAR = 1;
                        Sel_Bus1 = 1;
                    end
                    MOV: begin
                        next_state = S_mv1;
                        Sel_PC = 1;
                        Load_MAR = 1;
                        Sel_Bus1 = 1;
                    end
                    JMP: begin
                        next_state = S_fet1;
                        Load_PC = 1;
                        Sel_IRoperand = 1;
                        Sel_Bus1 = 1;
                    end
                    JEZ: begin
                         next_state = S_fet1;
                         Load_sign = 1;
                         if(zero) begin
                            Load_PC =1;
                            Sel_IRoperand = 1;
                            Sel_Bus1 = 1;
                         end
                         else begin
                            Inc_PC = 1;
                         end
                    end
                    JPS: begin
                         next_state = S_fet1;
                         Load_sign = 1;
                         if (sign) begin
                            Load_PC = 1;
                            Sel_IRoperand = 1;
                            Sel_Bus1 = 1;
                         end
                         else begin
                            Inc_PC = 1;
                         end
                    end
                    endcase
            end
            

            S_exc1: begin
                     next_state = S_fet1;
                     Sel_alu = 1;
                     write = 1;
            end
            S_rd1: begin
                     next_state = S_rd2;
                     Load_MAR = 1;
                     read = 1;
                     Load_MDR = 1;
                  end
            S_rd2: begin
                    next_state = S_rd3;
                    Load_MDR = 1;
                    end

            S_mv1: begin
                     next_state = S_mv2;
                     Load_MAR = 1;
                     read = 1;
                   end
            S_mv2: begin
                     next_state = S_mv3;
                     Load_MDR = 1;
                    end

            S_rd3: begin
                     next_state = S_rd4;
                     Sel_IRoperand = 1;
                     Load_MAR = 1;
                     Sel_Bus1 = 1;
                   end

            S_mv3: begin
                     next_state = S_mv4;
                     Sel_MDRoperand = 1;
                     Load_MAR = 1;
                   end

            S_mv4: begin
                     next_state = S_mv5;
                     Load_MAR = 1;
                     read = 1;
                   end
            S_mv5: begin
                    next_state = S_mv6;
                    Load_MDR = 1;
            end

            S_rd4: begin
                     next_state = S_fet1;
                     Sel_MDR1 = 1;
                     write = 1;
                   end
                     
            S_mv6: begin
                     next_state = S_mv7;
                     Sel_IRoperand = 1;
                     Load_MAR = 1;
                     Sel_Bus1 = 1;
                   end
            S_mv7: begin
                     next_state = S_fet1;
                     Sel_MDR1 = 1;
                     write = 1;
                     Inc_PC = 1;
                   end
                   
            S_halt: next_state = S_halt;
            default: next_state = S_idle;
        endcase
    end
endmodule

module Memory (data_out,A,B,data_in,address,clk,write,read);
        parameter address_size = 12;
        parameter memory_size = 256;
        parameter word_size = 16;

        output [word_size-1:0] data_out;
        output [word_size-1:0] A;
        output [word_size-1:0] B;
        input [word_size-1:0] data_in;
        input [address_size-1:0] address;
        input clk,write,read;
        reg A,B;
        reg data_out;

        reg [word_size-1:0] memory[memory_size-1:0];


        always @(posedge clk) begin
            if (read) begin
                data_out <= memory[address];
                A <= memory[0];
                B <= memory[1];
            end
            else if (write) begin 
                memory[address] <= data_in;
                A <= memory[0];
                B <= memory[1];
        end
        end         
endmodule