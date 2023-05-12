module vesp_risc(clk,rst);
    parameter word_size = 16;
    parameter Sel_size1 = 2;
    parameter Sel_size2 = 2;
    parameter address_size = 12;
    input clk,rst;
    wire [Sel_size1-1:0] Select_BBus_Mux;
    wire [Sel_size2-1:0] Select_OBus_Mux;

    wire [word_size-1:0]instruction,memory_word,Bus1;
    wire [address_size-1:0] address;
    wire zero;
    wire Load_A,Load_B,Load_IX,Load_MAR,Load_MDR,Inc_PC,Load_PC,Load_IR,Load_IXread,Inc_IX,write;

    Processor P0(instruction,zero,sign,address,Bus1,memory_word,Load_A,Load_B,Load_IR,Load_IX,Load_IXread,
    Load_MAR,Load_MDR,Load_PC,Inc_PC,Inc_IX,Load_zero,Load_signed,Select_BBus_Mux,Select_OBus_Mux,clk,rst);

    Control_Unit C0(Load_A,Load_B,Load_IR,Load_IX,Load_zero,Load_signed,Load_IXread,Load_MAR,Load_MDR,Load_PC,Inc_PC,Select_BBus_Mux,Select_OBus_Mux,
    Inc_IX,write,instruction,zero,sign,clk,rst);

    Memory M1(.data_out(memory_word),.data_in(Bus1),.address(address),.clk(clk),.write(write));

endmodule

module Processor(instruction,Zflag,sign_flag,address,Bus1,memory_word,Load_A,Load_B,Load_IR,Load_IX,Load_IXread,
    Load_MAR,Load_MDR,Load_PC,Inc_PC,Inc_IX,Load_zero,Load_signed,Select_BBus_Mux,Select_OBus_Mux,clk,rst);

    parameter word_size = 16;
    parameter Sel_size1 = 2;
    parameter Sel_size2 = 2;
    parameter opcode_size = 4;
    parameter operand_size = 12;
    parameter inst_address = 12;

    output [word_size-1:0] instruction,Bus1;
    output Zflag;
    output sign_flag;
    output [inst_address-1:0] address;

    input Load_A,Load_B,Load_IX,Load_MAR,Load_MDR,Load_PC,Load_IXread,Load_IR,Load_signed;
    input Inc_PC,Inc_IX;
    input [word_size-1:0] memory_word;
    input clk,rst;
    input [Sel_size1-1:0] Select_BBus_Mux;
    input [Sel_size2-1:0] Select_OBus_Mux;
    input Load_zero;

    wire Load_A,Load_B,Load_IX,Load_IXread;
    wire [word_size-1:0] A_out, B_out, IX_out,IX_read_out,alu_out,Bus2,MDR_out;
    wire [inst_address-1:0] PC_out;
    wire [opcode_size-1:0] opcode = instruction[word_size-1 : word_size-opcode_size];
    wire [operand_size-1:0] operand = instruction[word_size-opcode_size-1:0];
    wire [word_size-1:opcode_size] MDR_operand = MDR_out[word_size-1:opcode_size];
    wire A_zero_flag;

    Register_Unit A (A_out,Bus2,Load_A,clk,rst);
    Register_Unit B (B_out,Bus2,Load_B,clk,rst);
    Register_Unit IX_read (IX_read_out,Bus2,Load_IXread,clk,rst);
    signed_unit Signed (sign_flag,A_out,Load_signed,clk,rst);
    Zero_Unit zero (Zflag,A_out,Load_zero,clk,rst);
    IX_Unit IX (IX_out,Bus2,Load_IXread,Inc_IX,clk,rst);
    Program_counter PC (PC_out,Bus2,Load_PC,Inc_PC,clk,rst);
    Intruction_register IR (instruction,Bus2,Load_IR,clk,rst);
    MDR_unit MDR (MDR_out,memory_word,Load_MDR,clk,rst);
    MAR_Unit MAR (address,Bus2,Load_MAR,clk,rst);
    ALU A1 (alu_out,A_out,B_out,opcode);
    Multiplexer_5ch MUX1 (Bus1,IX_out,PC_out,operand,IX_read_out,Select_BBus_Mux);
    Multiplexer_4ch MUX2 (Bus2,alu_out,Bus1,MDR_out,MDR_operand,Select_OBus_Mux);
endmodule

module Register_Unit (data_out,data_in,load,clk,rst);
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

module IX_Unit (data_out,data_in,Load_IX,Inc_IX,clk,rst);
    parameter word_size = 16;
    output [word_size-1:0] data_out;
    input [word_size-1:0] data_in;
    input Load_IX;
    input Inc_IX;
    input clk,rst;

    reg data_out;

    always @(posedge clk or negedge rst) begin
        if(rst == 0) data_out <= data_in; else if (Load_IX) data_out <= data_in; else if (Inc_IX)
        data_out <= data_out+1;
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
        if(rst == 0) data_out <= data_in; else if (Load_PC) data_out <= data_in; else if (Inc_PC)
        data_out <= data_out+1;
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


module Multiplexer_5ch (mux_out,data_a,data_b,data_c,data_d,sel);

    parameter word_size = 16;
    parameter address_size = 12;
    output [word_size-1:0] mux_out;
    input [word_size-1:0] data_a, data_d;
    input [word_size-1:0] data_b, data_c;
    input [1:0] sel;

    assign mux_out = (sel == 0) ? data_a: (sel == 1)
                                ? data_b: (sel == 2)
                                ? data_c: (sel == 3)
                                ? data_d : 'bx;
endmodule

module Multiplexer_4ch (mux_out,data_a,data_b,data_c,data_d,sel);

    parameter word_size = 16;
    parameter address_size = 12;
    output [word_size-1:0] mux_out;
    input [word_size-1:0] data_a;
    input [word_size-1:0] data_b;
    input [word_size-1:0] data_c;
    input [word_size-1:0] data_d;
    input [1:0]sel;

    assign mux_out = (sel == 0) ? data_a: (sel == 1)
                                ? data_b: (sel == 2)
                                ? data_c: (sel == 3)
                                ? data_d : 'bx;

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
    parameter INC = 4'b1000;
    parameter DEC = 4'b1001;
    parameter AND = 4'b1010;
    parameter IOR = 4'b1011;
    parameter SHL = 4'b1100;
    parameter SHR = 4'b1101;
    parameter MXF = 4'b1110;
    parameter MXT = 4'b1111;
    output [word_size-1:0] alu_out;
    input [word_size-1:0] data_1;
    input [word_size-1:0] data_2;
    input [opcode_size-1:0] opcode;
    reg alu_out;

    always @(opcode or data_1 or data_2) begin 
        case(opcode)
        ADD: alu_out = data_1 + data_2;
        CMP: alu_out = ~data_1;
        INC: alu_out = data_1 + 1;
        DEC: alu_out = data_1 -1;
        AND: alu_out = data_1 & data_2;
        IOR: alu_out = data_1 | data_2;
        SHL: alu_out = data_1 << 1;
        SHR: alu_out = data_1 >> 1;
        default alu_out = 0;
        endcase
    end

endmodule

module Control_Unit(Load_A,Load_B,Load_IR,Load_IX,Load_zero,Load_signed,Load_IXread,Load_MAR,Load_MDR,Load_PC,Inc_PC,Select_BBus_Mux,Select_OBus_Mux,
    Inc_IX,write,instruction,zero,sign,clk,rst);

    parameter word_size = 16;
    parameter opcode_size = 4;
    parameter operand_size = 12;
    parameter address_size = 12;
    parameter Sel_size1 = 2;
    parameter Sel_size2 = 2;
    parameter state_size = 5;
    //State Codes
    parameter S_idle = 0, S_fet1=1, S_fet2 = 2, S_fet3 = 3, S_dec = 4;
    parameter S_rd1 = 5, S_rd2 = 6;
    parameter S_mv1 = 7,S_mv2 = 8,S_mv3 = 9, S_mv4 =10;
    parameter S_mxf1 = 11,S_mxf2 = 12, S_mxf3 = 13, S_mxf4= 14;
    parameter S_mxt1 = 15,S_mxt2 = 16,S_halt=17;
    //Opcodes
    parameter ADD = 0, CMP = 1, LDA = 2, MOV = 3, JMP = 4, JEZ = 5, JPS = 6;
    parameter HLT = 7, INC = 8, DEC = 9, AND = 10, IOR = 11, SHL = 12, SHR = 13;
    parameter MXF = 14, MXT = 15;

    //registers
    parameter A = 0, B = 1, IX = 3;

    output Load_A,Load_B,Load_IR,Load_IX,Load_zero,Load_signed,Load_IXread,Load_MAR,Load_PC;
    output Load_MDR,Inc_IX,Inc_PC;
    output write;
    output [Sel_size1-1:0] Select_BBus_Mux;
    output [Sel_size2-1:0] Select_OBus_Mux;
    input [word_size-1:0] instruction;
    input zero,sign;
    input clk,rst;

    reg [state_size-1:0] state, next_state;
    reg Load_A,Load_B,Load_IR,Load_IX,Inc_PC,Load_IXread,Load_MAR,Load_MDR,Load_signed,Load_zero,Load_PC,Inc_IX;
    reg Sel_A,Sel_B,Sel_IX,Sel_MDR,Sel_operand,Sel_PC,Sel_Bus_1,Sel_MDRoperand,Sel_IXread,write,Sel_ALU;
    reg err_flag;

    wire [opcode_size-1:0] opcode = instruction[word_size-1:word_size-opcode_size];
    wire [operand_size-1:0] operand = instruction[word_size-opcode_size-1:0];

    //Mux Selectors
    assign Select_BBus_Mux[Sel_size1-1:0] = Sel_IX ? 0:
                                            Sel_PC ? 1:
                                            Sel_operand ? 2:
                                            Sel_IXread;

    assign Select_OBus_Mux[Sel_size2-1:0] = Sel_ALU ? 0:
                                            Sel_Bus_1 ? 1:
                                            Sel_MDR ? 2:
                                            Sel_MDRoperand;


    always @(posedge clk or negedge rst) begin: State_transistions
        if(rst==0) state <= S_idle; else state <= next_state; end

    always @(state or operand or zero or sign) begin:Output_and_next_state

        Sel_A = 0; Sel_B = 0; Sel_IX = 0; Sel_MDR = 0; Sel_operand = 0; Sel_PC = 0; Sel_Bus_1 = 0; Sel_MDRoperand = 0; Sel_IXread = 0; Sel_ALU=0;
        Load_A = 0; Load_B = 0; Load_IR = 0; Load_IX = 0; Load_IXread = 0; Load_MAR = 0; Load_MDR = 0; Load_PC =0;
        Load_signed = 0; Load_zero = 0;Inc_PC = 0; Inc_IX =0; 
        next_state = state;
        write = 0;
        err_flag = 0;

        case(state) S_idle: next_state = S_fet1;
                    S_fet1: begin
                                next_state = S_fet2;
                                Sel_PC = 1;
                                Sel_Bus_1 = 1;
                                Load_MAR = 1;
                    end
                    S_fet2: begin
                                next_state = S_fet3;
                                Load_MAR = 1;
                                Load_MDR = 1;
                    end
                    S_fet3: begin
                                next_state = S_dec;
                                Sel_MDR = 1;
                                Load_IR = 1;
                                Inc_PC = 1;
                    end
                    S_dec: begin
                                case(opcode)
                                ADD,INC,DEC,AND,IOR,SHL,SHR: begin
                                    next_state = S_fet1;
                                    Sel_ALU = 1;
                                    Load_A = 1;
                                end

                                LDA: begin
                                    next_state = S_rd1;
                                    Sel_PC = 1;
                                    Sel_Bus_1 = 1;
                                    Load_MAR = 1;
                                end

                                MOV: begin
                                    next_state = S_mv1;
                                    Sel_PC = 1;
                                    Sel_Bus_1 = 1;
                                    Load_MAR = 1;
                                end

                                MXF: begin
                                    next_state = S_mxf1;
                                    Sel_IX = 1;
                                    Sel_Bus_1 = 1;
                                    Load_MAR = 1;
                                end

                                MXT: begin
                                    next_state = S_mxt1;
                                    Sel_operand = 1;
                                    Sel_Bus_1 = 1;
                                    Load_MAR = 1;
                                end

                                JMP: begin
                                    next_state = S_fet1;
                                    Sel_operand = 1;
                                    Sel_Bus_1 = 1;
                                    Load_PC = 1;
                                end

                                JEZ: begin
                                    Load_zero = 1;
                                    if (zero == 1) begin
                                        next_state = S_fet1;
                                        Sel_operand = 1;
                                        Sel_Bus_1 = 1;
                                        Load_PC = 1;
                                    end
                                    else begin
                                        next_state = S_fet1;
                                end
                                end

                                JPS: begin
                                    Load_signed = 1;
                                    if (sign == 1) begin
                                        next_state = S_fet1;
                                        Sel_operand = 1;
                                        Sel_Bus_1 = 1;
                                        Load_PC = 1;
                                    end
                                    else begin
                                        next_state = S_fet1;
                                    end
                                end
                                default: next_state = S_halt;
                                endcase
                    end

                    S_rd1: begin
                                next_state = S_rd2;
                                Load_MDR = 1;
                                Load_MAR = 1;
                                Inc_PC = 1;
                            end

                    S_mv1: begin
                                next_state = S_mv2;
                                Load_MDR = 1;
                                Load_MAR = 1;
                                Inc_PC = 1;
                            end

                    S_mxf1: begin
                                next_state = S_mxf2;
                                Load_MAR = 1;
                                Load_MDR = 1;
                            end

                    S_mxt1: begin
                                next_state = S_mxt2;
                                Load_MAR = 1;
                                Load_MDR = 1;
                            end

                    S_rd2: begin
                                next_state = S_fet1;
                                Sel_MDR = 1;
                                case(operand)
                                    A : Load_A = 1;
                                    B : Load_B = 1;
                                    IX : Load_IX = 1;
                                    default : err_flag = 1;
                                endcase
                            end

                    S_mv2: begin
                                next_state = S_mv3;
                                Sel_MDRoperand = 1;
                                Load_MAR = 1;
                            end

                    S_mxf2: begin
                                next_state = S_mxf3;
                                Sel_MDR = 1;
                                Load_IXread = 1;
                            end

                    S_mxt2: begin
                                next_state = S_fet1;
                                Sel_MDR = 1;
                                Load_IX = 1;
                                Inc_IX = 1;
                            end

                    S_mv3: begin
                                next_state = S_mv4;
                                Load_MAR = 1;
                                Load_MDR = 1;
                            end

                    S_mxf3: begin
                                next_state = S_mxf4;
                                Sel_operand = 1;
                                Sel_Bus_1 = 1;
                                Load_MAR = 1;
                            end

                    S_mv4: begin
                                next_state = S_fet1;
                                Sel_MDR = 1;
                                case(operand)
                                    A : Load_A = 1;
                                    B : Load_B = 1;
                                    default : err_flag = 1;
                                endcase
                            end
                    S_mxf4: begin
                                next_state = S_fet1;
                                Inc_IX=1;
                                Sel_IXread = 1;
                                write = 1;
                            end

                    S_halt: next_state = S_halt;
                    default next_state = S_idle;
                    endcase
    end
    endmodule

module Memory (data_out,data_in,address,clk,write);
        parameter address_size = 12;
        parameter memory_size = 256;
        parameter word_size = 16;

        output [word_size-1:0] data_out;
        input [word_size-1:0] data_in;
        input [address_size-1:0] address;
        input clk,write;

        reg [word_size-1:0] memory[memory_size-1:0];

        assign data_out = memory[address];

        always @(posedge clk)
            if(write) memory[address] <= data_in;

endmodule



                    





















