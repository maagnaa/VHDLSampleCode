
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use std.textio.all;

entity MIPS_CPU is
  port (
    clk         : in  std_logic;
    reset       : in  std_logic;
    WaitRequest : in  std_logic;
    D_write_en  : out std_logic;
    D_read_en   : out std_logic;
    I_ADR       : out std_logic_vector (31 downto 0);
    I_DATA      : in  std_logic_vector (31 downto 0);
    D_ADR       : out std_logic_vector (31 downto 0);
    D_W_DATA    : out std_logic_vector (31 downto 0);
    D_R_DATA    : in  std_logic_vector (31 downto 0));
end MIPS_CPU;

architecture a_MIPS_CPU of MIPS_CPU is

  component BWMUL is
    port (
        A       : in    std_logic_vector(31 downto 0);  -- Multiplicand
        B       : in    std_logic_vector(31 downto 0);  -- Multiplier
        MUL_out : out   std_logic_vector(63 downto 0)
    );
end component BWMUL;

  type   memtype is array (31 downto 0) of std_logic_vector(31 downto 0);
  signal RegFile : memtype := (others => (others => '0'));

  type Instruction_type_type is (undefined, Rt_1, Rt_2, It_SW, It_BNE, It_ADDIU, It_SLTI, It_LUI,
                                 It_BEQ, It_BLTZ, It_ORI, Jt_J, Jt_JAL, It_LW);
  -- for example:
  --  type   Instruction_type_type is (Undefined, R_type_special, R_type_special_2, RI_type,
  --                                   load, store, branch, jump, jal, slti,sltiu);

  type Funct_type_type is (ADDU, MUL, SwLL, JR, NotR);

  -- Predefined Signals
  signal Instruction_type        	: Instruction_type_type;
  signal Function_Type            : Funct_type_type;
  signal W_ADR1                   : std_logic_vector(4 downto 0);
  signal W_ADR                   	: std_logic_vector(4 downto 0);
  signal PC, nextPC, PC4          : std_logic_vector(31 downto 0);
  signal instr                   	: std_logic_vector(5 downto 0);
  signal funct                   	: std_logic_vector(5 downto 0);
  signal ALU_out                 	: std_logic_vector(31 downto 0):= (others => '0'); 	-- ALU output
  signal iData                    : std_logic_vector(31 downto 0):= (others => '0');
  signal s_MUL_out                : std_logic_vector(63 downto 0):= (others => '0');
  -- Control Signals from Decoder
  signal s_ctrl						: std_logic_vector(7 downto 0);

  alias SLL_Ena           : std_logic is s_ctrl(7); -- Set to '1' for SLL instruction, else '0'
  alias MemReadEna 				: std_logic is s_ctrl(6); -- Enable reading from memory
  alias RegDst 						: std_logic is s_ctrl(5); -- Register Destination: W_ADR is iData(20:16) when '0', iData(15:11) when '1'.
  alias RegWriteEna				: std_logic is s_ctrl(4); -- Enable writing to registers.
  alias ALUSrc						: std_logic is s_ctrl(3); -- Input SrcB to ALU is rDATA2 when '0', output from Immediate Multiplexer when '1'.
  alias ImmSel						: std_logic is s_ctrl(2); -- Immediate Select: Pass sign extended immediate to ALU Input Multiplexer when '0', pass resized immediate when '1'.
  alias MemWriteEna				: std_logic is s_ctrl(1); -- Enable Writing to Memory.
  alias MemtoReg					: std_logic is s_ctrl(0); -- Pass ALU_out to wData when '0', D_R_DATA when '1'.

  -- Input Signals to Register File
  alias a_rs              : std_logic_vector(4 downto 0) is iData(25 downto 21);
  alias a_rt              : std_logic_vector(4 downto 0) is iData(20 downto 16);
  alias a_rd              : std_logic_vector(4 downto 0) is iData(15 downto 11);
  alias a_sa              : std_logic_vector(4 downto 0) is iData(10 downto 6);
  signal s_shamt          : std_logic_vector(31 downto 0);
  --signal wADDR            : std_logic_vector(4 downto 0)   := (others => '0');
  signal wData1           : std_logic_vector(31 downto 0)  := (others => '0');
  signal wData            : std_logic_vector(31 downto 0)  := (others => '0');
  -- Output Signals from Register File
  signal R_DATA_A           : std_logic_vector(31 downto 0)  := (others => '0');
  signal R_DATA_B           : std_logic_vector(31 downto 0)  := (others => '0');

  -- Input Signals to ALU
  signal s_SrcA           : std_logic_vector(31 downto 0)  := (others => '0');
  signal s_SrcB           : std_logic_vector(31 downto 0)  := (others => '0');

  -- Signals for Immediate Multiplexer
  signal s_ImmOut0        : std_logic_vector(31 downto 0)  := (others => '0');
  signal s_ImmOut1        : std_logic_vector(31 downto 0)  := (others => '0');
  signal s_ImmOut         : std_logic_vector(31 downto 0)  := (others => '0');

  -- Control Signals from ALU
  signal Branch					: std_logic := '0'; -- Don't branch when 0, branch when 1.
  signal BranchR        : std_logic := '0'; -- Don't branch when 0, branch when 1.

  -- Signals for Branching
  signal BranchPC         : std_logic_vector(31 downto 0)  := (others => '0');
  signal BranchPCR        : std_logic_vector(31 downto 0)  := (others => '0');
  signal s_ImmOutLS2		  : std_logic_vector(31 downto 0)  := (others => '0');

  -- Signals for Jumping
  alias target            : std_logic_vector(25 downto 0) is iData(25 downto 0);
  constant c_Reg31        : std_logic_vector := "11111";

  signal Jump             : std_logic := '0'; -- Jump when 1, dont Jump when 0.
  signal JumpR            : std_logic := '0'; -- Jump when 1, dont Jump when 0.
  signal Link             : std_logic := '0';
  signal s_Target32       : std_logic_vector(31 downto 0)  := (others => '0');

begin

------------ Register File
  p_write : process (clk)
  begin
    if rising_edge(clk) then
      if WaitRequest /= '1' then
        if RegWriteEna = '1' and (W_ADR /= (W_ADR'range => '0')) and reset /= '1' then
          RegFile(to_integer(unsigned(W_ADR))) <= wData;
        end if;  -- RegFileEnable
     end if;  --Waitrequest
    end if;  --clk
  end process;

  -- instruction decoder
  -- implement the missing functionality
  instr <= iData(31 downto 26);
  funct <= iData(5 downto 0);

  p_INS_DECODER : process (instr, funct)
  begin
	  -- Opcode is Instruction(31:26) [26.01]
  -- Decode OpCode
     Jump <= '0';
     Link <= '0';
    case instr is
		when "000000" =>
			Instruction_type 	<= Rt_1; 		-- R-type 1
      case funct is
          when "100001" =>
            Function_type 	<= ADDU;		-- Add Unsigned Word Rt1
            s_ctrl			<= "00110000";
          when "000000" =>
            Function_type 	<= SwLL;		-- Shift Word Left Logical Rt1
            s_ctrl			<= "10110000";
          when "001000" =>
            Function_type 	<= JR;   -- Jump Register R
            s_ctrl			<= "00000000";
          when others =>
             Null;
          --report " +++ unimplemented instruction type !! ";
        end case;
		when "011100" =>
			Instruction_type 	<= Rt_2; 		-- R-type 2
         case funct is
          when "000010" =>
            Function_type 	<= MUL;			-- Multiply Word to GPR Rt2
            s_ctrl			<= "00110000";
          when others =>
             Null;
          --report " +++ unimplemented instruction type !! ";
        end case;
		when "101011" =>
			Instruction_type 	<= It_SW; 		-- I-type, Store Word
			s_ctrl				<= "00001010";	-- Control Vector
      Function_Type <= NotR;
		when "000101" =>
			Instruction_type 	<= It_BNE; 		-- I-type, Branch not Equal
			s_ctrl				<= "00000000";	-- Control Vector
      Function_Type <= NotR;
		when "001001" =>
			Instruction_type 	<= It_ADDIU;	-- I-type, Add Immediate Unsigned
			s_ctrl				<= "00011000";	-- Control Vector
      Function_Type <= NotR;
		when "001010" =>
			Instruction_type 	<= It_SLTI;		-- I-type, Set on Less than Immediate
			s_ctrl				<= "00011000";	-- Control Vector
      Function_Type <= NotR;
		when "001111" =>
			Instruction_type 	<= It_LUI;		-- I_type, Load Upper Immediate
			s_ctrl				<= "00011100";	  -- Control Vector
      Function_Type <= NotR;
     when "000100" =>
      Instruction_type <= It_BEQ;     -- I_type, Unconditional Branch
      s_ctrl				<= "00000000";
      Function_Type <= NotR;
     when "000001" =>
      Instruction_type 	<= It_BLTZ;   -- I_type, Branch on less than zero
      s_ctrl				<= "00000000";
      Function_Type <= NotR;
     when "001101" =>
      Instruction_type 	<= It_ORI;    -- I-type, Or Immediate
      s_ctrl				<= "00011100";
      Function_Type <= NotR;
     when "000010" =>
      Instruction_type 	<= Jt_J;      -- I-type, Jump
      s_ctrl				<= "00000000";
      Jump <= '1';
      Function_Type <= NotR;
     when "000011" =>
      Instruction_type 	<= Jt_JAL;    -- I-type, Jump and Link
      s_ctrl				<= "00010000";
      Jump <= '1';
      Link <= '1';
      Function_Type <= NotR;
     when "100011" =>
      Instruction_type 	<= It_LW;     -- I-type, Load Word
      s_ctrl				<= "01011101";
      Function_Type <= NotR;
      when others =>
			NULL;
 --       report " +++ unimplemented instruction type !! ";
    end case;
  end process;

  -- ALU
  -- implement the missing functionality
  p_ALU: process (Instruction_type, Function_Type, s_SrcA, s_SrcB)
  variable ALU_out64 : std_logic_vector(63 downto 0);
  begin
   -- if reset = '1' then
   --   ALU_out <= (others => '0');
   -- else
      Branch <= '0';
      case Instruction_type is
  		when Rt_1     =>
          if Function_type = ADDU then
  --          ALU_out   <= std_logic_vector(unsigned(s_SrcA) + unsigned(s_SrcB));
            ALU_out   <= std_logic_vector(signed(s_SrcA) + signed(s_SrcB));
          elsif Function_type = SwLL then
            ALU_out   <= std_logic_vector(unsigned(s_SrcB) sll to_integer(unsigned(s_SrcA)));
          elsif Function_type = JR then
            ALU_out   <= s_SrcA;
          end if;
  		when Rt_2     =>
          if Function_Type = MUL then
            --ALU_out64 := std_logic_vector(unsigned(s_SrcA) * unsigned(s_SrcB));
            ALU_out   <= s_MUL_out(31 downto 0);
          end if;
  		when It_SW    =>
          ALU_out <= std_logic_vector(signed(s_SrcA) + signed(s_SrcB));
  		when It_BNE   =>
          if s_SrcA /= s_SrcB then
            Branch <= '1';  -- Branch when Branch = 1
          else
            Branch <= '0';
        end if;
          ALU_out <= (others=> '0');
  		when It_ADDIU =>
          ALU_out <= (std_logic_vector(signed(s_SrcA) + signed(s_SrcB)));
  		when It_SLTI  =>
  			if (to_integer(signed(s_SrcA)) < to_integer(signed(s_SrcB))) then
  		    ALU_out <=(0 => '1', others => '0');
        else
          ALU_out <=(others => '0');
        end if;
  		when It_LUI =>
          ALU_out <= s_SrcB(15 downto 0) & x"0000";
      when It_BEQ =>
          if (s_SrcA = s_SrcB) then
            Branch <= '1';
          else
            Branch <= '0';
          end if;
          ALU_out <= (others=> '0');
      when It_BLTZ =>
          if to_integer(signed(s_SrcA)) < to_integer(signed(s_SrcB)) then
           Branch <= '1';
          else
            Branch <= '0';
        end if;
          ALU_out <= (others=> '0');
      when It_ORI =>
          ALU_out <= s_SrcA OR s_SrcB;
      when Jt_J =>
          ALU_out <= (others=> '0');
      when Jt_JAL =>
          ALU_out <= (others=> '0');
      when It_LW =>
          ALU_out <= (std_logic_vector(signed(s_SrcA) + signed(s_SrcB)));
  		when others =>
  		    ALU_out <= (others => '0');
      end case;
  --  end if;
  end process;

D_ADR <= ALU_out;


   p_PC : process (clk)
    begin
        if rising_edge(clk) then
            if reset = '1' then
              PC 		    <= x"BFC0_0000"; 	-- mips reset vector
              nextPC  	<= x"BFC0_0004"; 	-- mips reset vector
            elsif WaitRequest /= '1' then
              PC <= nextPC;
              BranchR <= Branch;
              JumpR <= Jump;
              if Branch ='1' then
                nextPC <= BranchPC;
              elsif Jump = '1' then
                nextPC <= s_Target32;
              elsif Function_type = JR then
                nextPC <= ALU_out;
              else
                nextPC <= PC4;
              end if;
            end if;
        end if;
    end process;

MUL1 : BWMUL
  port map(
      A => s_SrcA,
      B => s_SrcB,
      MUL_out => s_MUL_out
  );


  --RegFile(to_integer(unsigned(W_ADR)));
  iData <= (others => '0') when reset = '1' else I_DATA;
  -- Combinational Logic
  D_ADR        <= ALU_out;
  D_W_DATA    <= R_DATA_B;
  D_read_en   <= MemReadEna;
  D_write_en  <= MemWriteEna;

  -- PC increment
  PC4 <= std_logic_vector(unsigned(nextPC) + 4);
  -- Branching
  s_ImmOutLS2 <= s_ImmOut(29 downto 0) & "00";
  BranchPC  <= std_logic_vector(signed(s_ImmOutLS2)+ signed(nextPC)) when (BranchR = '0' and Branch = '1') else BranchPC;


  -- Jumping
  s_Target32 <= (PC4(31 downto 28) & target & "00") when (JumpR = '0' and Jump = '1') else s_Target32;

  I_ADR <= PC;

  -- Multiplexer Logic
  -- Register Destiny Multiplexer
  W_ADR1 <= a_rt when RegDst = '0' else a_rd;
  W_ADR  <= c_Reg31 when Link = '1' else W_ADR1;

  -- Immediate Multiplexer
  s_ImmOut0 <= std_logic_vector(resize(signed(iData(15 downto 0)),s_ImmOut'length));
  s_ImmOut1 <= std_logic_vector(resize(unsigned(iData(15 downto 0)),s_ImmOut'length));   -- Resized Immediate (not sign extended)
  s_ImmOut  <= s_ImmOut0 when ImmSel = '0' else s_ImmOut1;

  -- ALU Source Selection
  R_DATA_A  <= RegFile(to_integer(unsigned(a_rs)));
  R_DATA_B  <= RegFile(to_integer(unsigned(a_rt)));  -- Output from register when reading address a_rt

  s_SrcA    <= s_shamt when SLL_Ena = '1' else R_DATA_A;
  s_SrcB    <= R_DATA_B when ALUSrc = '0' else s_ImmOut;

  -- Memory Bypass Multiplexer
  wData1    <=  ALU_out when MemtoReg = '0' else D_R_DATA;
  wData     <= PC4 when Link = '1' else wData1;

  -- SLL input Multiplexer
  s_shamt   <= std_logic_vector(resize(unsigned(a_sa),s_shamt'length));




end;
