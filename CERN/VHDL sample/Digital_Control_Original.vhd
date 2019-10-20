----------------------------------------------------------------------------------
-- Company: CERN - TE EPC FPC 
-- 
-- Create Date:    09:19:24 02/06/2013 
-- Design Name: 	MAXIDISCAP DIGITAL CONTROL BOARD 
-- Module Name:    Digital Algorithm - Behavioral 
-- Project Name:	MAXIDISCAP 
-- Target Devices: 	SIRAMATRIX
--
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

----------------------------------------------------------------------------------
--	Entity Definition
----------------------------------------------------------------------------------

entity Digital_Control is
	Port(RESET_i           : in  STD_LOGIC;
		 IREF_i             : in  STD_LOGIC_VECTOR(13 downto 0);
		 IACQ_i             : in  STD_LOGIC_VECTOR(13 downto 0);
		 IFILT_EN_i			  : in 	STD_LOGIC;
		 VC_Measurement_i   : in  STD_LOGIC_VECTOR(13 downto 0);
		 VC_Reference16_i   : in  STD_LOGIC_VECTOR(15 downto 0);
		 VC_Measurement16_i : in  STD_LOGIC_VECTOR(15 downto 0);
		 MULT1_i            : in  STD_LOGIC_VECTOR(15 downto 0);
		 MULT2_i            : in  STD_LOGIC_VECTOR(15 downto 0);
		 SYNC_i             : in  STD_LOGIC;
		 FAST_CLK_i         : in  STD_LOGIC;
		 Saturate_IGBT_i    : in  STD_LOGIC;
		 Blocking_IGBT_i    : in  STD_LOGIC;
		 ACTUATION_o        : out STD_LOGIC_VECTOR(13 downto 0);
		 UC_OK_o            : out STD_LOGIC;
		 TEST1_o            : out STD_LOGIC_VECTOR(15 downto 0);
		 TEST2_o            : out STD_LOGIC_VECTOR(15 downto 0);
		 Init_Cond_Iref_Mult_i   : in  STD_LOGIC_VECTOR(15 downto 0);
		 Initial_offset_i   : in  STD_LOGIC_VECTOR(15 downto 0)
	);
end Digital_Control;

----------------------------------------------------------------------------------
--	Architecture
----------------------------------------------------------------------------------

architecture Behavioral of Digital_Control is

	----------------------------------------------------------------------------------
	-- Components
	----------------------------------------------------------------------------------
	component Imeas_FILTER
	 generic ( 	g_F1  	  : integer range 0 to 6500 := 20;
					g_F2		  : integer range 0 to 6500 := 41;
					g_F3  	  : integer range 0 to 6500 := 1605;
					g_F4  	  : integer range 0 to 6500 := 662;
					g_Length   : integer range 1 to 32   := 14);
    port (  	Rst_i 	: in  std_logic;
					Clk_i 	: in  std_logic; -- 40 MHz clock
					Sync_i	: in  std_logic;
					Sync_o	: out std_logic;
					I_i   	: in  std_logic_vector((g_Length-1) downto 0);  -- Input Current, Unfiltered
					I_o   	: out std_logic_vector((g_Length-1) downto 0)); -- Output Current, Filtered
	end component Imeas_FILTER;

	----------------------------------------------------------------------------------
	-- Signals
	----------------------------------------------------------------------------------

	signal s_SyncRRRR : Std_Logic;
	signal s_IACQ_FILT : STD_LOGIC_VECTOR(13 downto 0); 

	signal s_Error   : Std_Logic_vector(13 downto 0);
	signal s_Esign   : Std_Logic;
	signal s_Error_1 : Std_Logic_vector(13 downto 0);
	signal s_Esign_1 : Std_Logic;

	signal s_Temp_Actuation  : Std_Logic_vector(13 downto 0);
	signal s_Temp_Actuation2 : Std_Logic_vector(13 downto 0);
	signal s_Actuation       : Std_Logic_vector(13 downto 0);
	signal s_Actuation_1     : Std_Logic_vector(13 downto 0);

	signal s_Delay1 : Std_Logic;
	signal s_Delay2 : Std_Logic;
	signal s_Delay3 : Std_Logic;

	signal s_MULT1 : Std_Logic_vector(29 downto 0);
	signal s_MULT2 : Std_Logic_vector(29 downto 0);

	signal c_Mult1 : Std_Logic_vector(15 downto 0);
	signal c_Mult2 : Std_Logic_vector(15 downto 0);

	signal s_MULT1_14 : Std_Logic_vector(13 downto 0);
	signal s_MULT2_14 : Std_Logic_vector(13 downto 0);

	signal s_ADD_B       : Std_Logic_vector(14 downto 0);
	signal s_ADD_D       : Std_Logic_vector(15 downto 0);
	signal s_ADD_B_sign  : Std_Logic;
	signal s_ADD_D_sign  : Std_Logic;
	signal s_ADD_D_sign1 : Std_Logic;

	signal s_DigActuation    : Std_Logic_vector(14 downto 0);
	signal s_DAC_sign        : std_logic;
	signal s_Test_actuation  : std_logic_vector(14 downto 0);
	signal s_Test_actuation2 : Std_Logic_vector(15 downto 0);
	signal s_Test_actuation3 : Std_Logic_vector(15 downto 0);
	signal s_Test_actuation4 : Std_Logic_vector(13 downto 0);
	signal s_Test_actuation5 : Std_Logic_vector(13 downto 0);

	signal s_TempVoltage     : Std_Logic_vector(13 downto 0);
	signal s_TempVoltage2    : Std_Logic_vector(13 downto 0);
	signal s_TempVoltage3    : Std_Logic_vector(13 downto 0);

	signal s_Test_add  : Std_Logic_vector(14 downto 0);
	signal s_DAC_sign1 : Std_Logic;

	-- Ki gain
	signal s_Mult_Saturated : Std_Logic_vector(13 downto 0);
	signal s_DigActuation1  : Std_Logic_vector(14 downto 0);

	-- UC Checking (15%)
	constant c_UC10percent   : Std_Logic_vector(7 downto 0) := x"73";
	signal s_Mult_UC_Value   : Std_Logic_vector(23 downto 0);
	signal s_UC_Final_Window : Std_Logic_vector(15 downto 0);
	signal s_NewREF16_UC     : Std_Logic_vector(15 downto 0);

	--Initial Conditions calculation
	signal s_Initial_Mult     : Std_Logic_vector(29 downto 0);
	signal s_Initial_Mult2    : Std_Logic_vector(23 downto 0);
	signal s_Initial_Addition : Std_Logic_vector(15 downto 0);
----------------------------------------------------------------------------------
----------------------------------------------------------------------------------

begin

	-- Component Instantiation
	Imeas_Filtering: Imeas_FILTER
	  generic map( g_Length => 14)
		port map	(  Rst_i 	=> not(IFILT_EN_i),
						Clk_i 	=> FAST_CLK_i,
						Sync_i	=> SYNC_i,
						Sync_o	=> s_SyncRRRR,
						I_i   	=> IACQ_i,
						I_o   	=> s_IACQ_FILT); 

	c_Mult1 <= MULT1_i;
	c_Mult2 <= MULT2_i;
	
	
	-- Latching delayed signals and calculating Error.

	p_ErrorCalc : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if RESET_i = '1' then
				s_Error <= (others => '0');
				s_Esign <= '0';
			elsif (s_SyncRRRR = '1') then
				if IREF_i < s_IACQ_FILT then
					s_Error <= s_IACQ_FILT - IREF_i;
					s_Esign <= '1';
				else
					s_Error <= IREF_i - s_IACQ_FILT;
					s_Esign <= '0';
				end if;
			end if;
		end if;
	end process p_ErrorCalc;

	p_LatchErrors : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if RESET_i = '1' then
				s_Esign_1 <= '0';
				s_Error_1 <= (others => '0');
			elsif (s_SyncRRRR = '1') then
				s_Error_1 <= s_Error;
				s_Esign_1 <= s_Esign;
			end if;
		end if;
	end process p_LatchErrors;

	p_LatchActuation : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if RESET_i = '1' then
				s_Actuation_1 <= (others => '0');
				s_ADD_D_sign1 <= '0';
			elsif (s_SyncRRRR = '1') then
				s_Actuation_1 <= s_Actuation;
				s_ADD_D_sign1 <= s_ADD_D_sign;
			end if;
		end if;
	end process p_LatchActuation;

	--Creating Sync Delays for calculation time

	p_Delaying : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if (RESET_i = '1') then 
				s_Delay1 <= '0';
				s_Delay2 <= '0';
				s_Delay3 <= '0';
			else
				s_Delay1 <= s_SyncRRRR;
				s_Delay2 <= s_Delay1;
				s_Delay3 <= s_Delay2;
			end if;
		end if;
	end process p_Delaying;

	-------------------------------------------------------------------------------		
	--CODING REGULATION ALGORITHM--------------------------------------------------
	--   y(k)=y(k-1) + 10.8826 E(k) - 10.8639 E(k-1)
	--	  FirstAdd=10.8826 E(k) - 10.8639 E(k-1)
	-------------------------------------------------------------------------------		

	s_MULT1 <= c_Mult1 * s_Error;
	s_MULT2 <= c_Mult2 * s_Error_1;

	s_MULT1_14 <= s_MULT1(22 downto 9) when s_MULT1 < x"7FFFFF" else (others => '1'); --Dividing by 128
	s_MULT2_14 <= s_MULT2(22 downto 9) when s_MULT2 < x"7FFFFF" else (others => '1'); --Dividing by 128  --'0' & s_Actuation_2(13 downto 1);		


	p_B_Adder : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if RESET_i = '1' then
				s_ADD_B      <= (others => '0');
				s_ADD_B_sign <= '0';
			elsif (s_Delay2 = '1') then
				if s_Esign = '0' then
					if s_Esign_1 = '0' then
						if s_MULT1_14 > s_MULT2_14 then
							s_ADD_B      <= ('0' & s_MULT1_14) - ('0' & s_MULT2_14);
							s_ADD_B_sign <= '0';
						else
							s_ADD_B      <= ('0' & s_MULT2_14) - ('0' & s_MULT1_14);
							s_ADD_B_sign <= '1';
						end if;
					else
						s_ADD_B      <= ('0' & s_MULT1_14) + ('0' & s_MULT2_14);
						s_ADD_B_sign <= '0';
					end if;
				else
					if s_Esign_1 = '0' then
						s_ADD_B      <= ('0' & s_MULT2_14) + ('0' & s_MULT1_14);
						s_ADD_B_sign <= '1';
					else
						if s_MULT2_14 > s_MULT1_14 then
							s_ADD_B      <= ('0' & s_MULT2_14) - ('0' & s_MULT1_14);
							s_ADD_B_sign <= '0';
						else
							s_ADD_B      <= ('0' & s_MULT1_14) - ('0' & s_MULT2_14);
							s_ADD_B_sign <= '1';
						end if;
					end if;
				end if;
			end if;
		end if;
	end process p_B_Adder;

	p_D_Adder : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if RESET_i = '1' then
				s_ADD_D      <= s_Initial_Addition;
				s_ADD_D_sign <= '0';
			elsif (s_Delay3 = '1') then
				if s_ADD_B_sign = '0' then
					if s_ADD_D_sign1 = '0' then
						s_ADD_D      <= ('0' & s_ADD_B) + ('0' & s_Actuation_1);
						s_ADD_D_sign <= '0';
					else
						if s_ADD_B > s_Actuation_1 then
							s_ADD_D      <= ('0' & s_ADD_B) - ('0' & s_Actuation_1);
							s_ADD_D_sign <= '0';
						else
							s_ADD_D      <= ('0' & s_Actuation_1) - ('0' & s_ADD_B);
							s_ADD_D_sign <= '1';
						end if;
					end if;
				else
					if s_ADD_D_sign1 = '0' then
						if s_Actuation_1 > s_ADD_B then
							s_ADD_D      <= ('0' & s_Actuation_1) - ('0' & s_ADD_B);
							s_ADD_D_sign <= '0';
						else
							s_ADD_D      <= ('0' & s_ADD_B) - ('0' & s_Actuation_1);
							s_ADD_D_sign <= '1';
						end if;
					else
						s_ADD_D      <= ('0' & s_ADD_B) + ('0' & s_Actuation_1);
						s_ADD_D_sign <= '1';
					end if;
				end if;
			end if;
		end if;
	end process p_D_Adder;

	s_Temp_Actuation <= s_ADD_D(13 downto 0) when s_ADD_D < x"3FFF" else (others => '1');

	--s_Temp_Actuation2 <= ('0' & not (s_Temp_Actuation(13 downto 1))) when s_ADD_D_sign = '0' else ('1' & s_Temp_Actuation(13 downto 1)); --JMC

	s_Actuation <= s_Temp_Actuation;

	s_DigActuation <= s_ADD_D_sign & ('0' & s_Temp_Actuation(13 downto 1));

	----------------------

	p_Actuation : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if s_DigActuation(14) = '0' then
				if VC_Measurement_i > s_DigActuation(13 downto 0) then
					s_Test_actuation <= ('0' & VC_Measurement_i) - ('0' & s_DigActuation(13 downto 0));
					s_DAC_sign       <= '0';
				else
					s_Test_actuation <= ('0' & s_DigActuation(13 downto 0)) - ('0' & VC_Measurement_i);
					s_DAC_sign       <= '1';
				end if;
			else
				s_Test_actuation <= ('0' & VC_Measurement_i) + ('0' & s_DigActuation(13 downto 0));
				s_DAC_sign       <= '0';
			end if;
		end if;
	end process p_Actuation;

	s_Test_actuation2 <= ( B"0001_1111_1111_1111" + ('0' & s_Test_actuation) ) when s_Test_actuation < B"010_0000_0000_0000" else (others => '1'); -- if Actuation is positive
	s_Test_actuation3 <= ( B"0001_1111_1111_1111" - ('0' & s_Test_actuation) ) when s_Test_actuation < B"001_1111_1111_1111" else (others => '0'); -- if Actuation is negative

	s_Test_actuation4 <= s_Test_actuation2(13 downto 0) when s_DAC_sign = '0' else s_Test_actuation3(13 downto 0);
	s_Test_actuation5 <= s_Test_actuation4; --when s_Test_actuation4<"10001100110010" else "10001100110010"; --limitation to 1V

	s_TempVoltage3 <= s_Test_actuation5;
	s_TempVoltage  <= (others => '0') when Saturate_IGBT_i = '1' else s_TempVoltage3;
	ACTUATION_o    <= (others => '1') when Blocking_IGBT_i = '1' else s_TempVoltage;

	s_Mult_Saturated <= s_IACQ_FILT;

	p_Actuation2 : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if s_DigActuation(14) = '1' then
				if s_Mult_Saturated > s_DigActuation(13 downto 0) then
					s_Test_add  <= ('0' & s_Mult_Saturated) - ('0' & s_DigActuation(13 downto 0));
					s_DAC_sign1 <= '0';
				else
					s_Test_add  <= ('0' & s_DigActuation(13 downto 0)) - ('0' & s_Mult_Saturated);
					s_DAC_sign1 <= '1';
				end if;
			else
				s_Test_add  <= ('0' & s_Mult_Saturated) + ('0' & s_DigActuation(13 downto 0));
				s_DAC_sign1 <= '0';
			end if;
		end if;
	end process p_Actuation2;

	s_DigActuation1(14)          <= s_DAC_sign1;
	s_DigActuation1(13 downto 0) <= (s_Test_add(13 downto 0)) when s_Test_add < x"3FFF" else ("11111111111111");

	-------------------------------------------------------------------------------
	-- UC Checking
	-------------------------------------------------------------------------------
	s_NewREF16_UC <= ('0' & VC_Reference16_i(14 downto 0)) when (VC_Reference16_i(15) = '1') else (others => '0');

	s_Mult_UC_Value <= c_UC10percent * s_NewREF16_UC;

	s_UC_Final_Window <= s_Mult_UC_Value(22 downto 7) when s_Mult_UC_Value < x"7FFFFF" else (others => '1');

	UC_OK_o <= '1' when VC_Measurement16_i > ('0' & s_UC_Final_Window(15 downto 1)) else '0';

	TEST1_o <= VC_Measurement16_i;
	TEST2_o <= s_UC_Final_Window;

	-------------------------------------------------------------------------------
	-- Initial Conditions calculation
	-------------------------------------------------------------------------------
	p_initial_multy : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			s_Initial_Mult <= IREF_i * Init_Cond_Iref_Mult_i;
		end if;
	end process p_initial_multy;

	s_Initial_Mult2 <= s_Initial_Mult(23 downto 0) when s_Initial_Mult < x"FFFFFF" else (others => '1');

	p_initial_adder : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			s_Initial_Addition <= ("000" & Initial_offset_i(12 downto 0)) + ("000" & s_Initial_Mult2(23 downto 11));
		end if;
	end process p_initial_adder;

end Behavioral;