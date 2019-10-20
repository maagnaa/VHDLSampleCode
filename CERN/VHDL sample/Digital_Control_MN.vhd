library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.Numeric_std.all;

entity Digital_Control is
	Port(RESET_i           		: in  std_logic;
		 IREF_i             	: in  std_logic_vector(13 downto 0);
		 IACQ_i             	: in  std_logic_vector(13 downto 0);
		 IFILT_EN_i			  	: in  std_logic;
		 IFILT_COEF_1_i	  		: in  std_logic_vector(15 downto 0);	
		 IFILT_COEF_2_i	  		: in  std_logic_vector(15 downto 0);
		 IFILT_COEF_3_i			: in  std_logic_vector(15 downto 0);
		 IFILT_COEF_4_i			: in  std_logic_vector(15 downto 0);
		 IFILT_COEF_5_i			: in  std_logic_vector(15 downto 0);
		 VC_Measurement_i   	: in  std_logic_vector(13 downto 0);
		 VC_Reference16_i   	: in  std_logic_vector(15 downto 0);
		 VC_Measurement16_i 	: in  std_logic_vector(15 downto 0);
		 MULT1_i            	: in  std_logic_vector(15 downto 0);
		 MULT2_i            	: in  std_logic_vector(15 downto 0);
		 SYNC_i             	: in  std_logic;
		 FAST_CLK_i         	: in  std_logic;
		 Saturate_IGBT_i    	: in  std_logic;
		 Blocking_IGBT_i    	: in  std_logic;
		 Regulation_mode_i  	: in  std_logic_vector(1 DOWNTO 0); 
		 ACTUATION_o        	: out std_logic_vector(13 downto 0);
		 UC_OK_o            	: out std_logic;
		 TEST1_o            	: out std_logic_vector(15 downto 0);
		 TEST2_o            	: out std_logic_vector(15 downto 0);
		 Initial_factor_i   	: in  std_logic_vector(15 downto 0);
		 Initial_offset_i   	: in  std_logic_vector(15 downto 0);
		 Smooth_Value_i	  		: in  std_logic_vector(15 downto 0);
		 Smoothing_Mult_i	  	: in  std_logic_vector(15 downto 0);
		 Smoothing_Offset_i 	: in  std_logic_vector(15 downto 0)
	);
end Digital_Control;

architecture rtl of Digital_Control is

alias clk : std_logic is FAST_CLK_i;
-- Components
component Imeas_FILTER
 generic (g_Length   			: integer range 1 to 32   := 14);
 port (  	Rst_i 				: in  std_logic;
				Clk_i 				: in  std_logic; -- 40 MHz clock
				Sync_i				: in  std_logic;
				Sync_o				: out std_logic;
				IFILT_COEF_1_i	  	: in  std_logic_vector(15 downto 0);	
				IFILT_COEF_2_i	  	: in  std_logic_vector(15 downto 0);
				IFILT_COEF_3_i		: in  std_logic_vector(15 downto 0);
				IFILT_COEF_4_i		: in  std_logic_vector(15 downto 0);
				IFILT_COEF_5_i		: in  std_logic_vector(15 downto 0);
				I_i   				: in  std_logic_vector(13 downto 0);  -- Input Current, Unfiltered
				I_o   				: out std_logic_vector(13 downto 0)); -- Output Current, Filtered
	end component Imeas_FILTER;
	
-- Signals 

-- For Calculation 1
signal s_Iref 		: signed (14 downto 0);
signal s_Iaq   		: signed (14 downto 0);
signal s_Err		: signed (14 downto 0);

-- For Calculation 2
signal s_ErrR	: signed 	(14 downto 0); 
signal s_Mult1	: signed 	(15 downto 0);
signal s_Mult2	: signed 	(15 downto 0);
signal s_512Y	: signed 	(30 downto 0);
signal s_512YR	: signed 	(30 downto 0);
signal s_Y		: signed 	(30 downto 0);

-- For Calculation 3
signal s_Uc			: signed (13 downto 0);
signal s_YFeedF		: signed (30 downto 0); -- Y'(k)
signal s_YFeedF14 	: signed (13 downto 0);

-- For Calculation 4
signal s_YR		: signed (30 downto 0);

-- For Output Process
signal s_CTRL	: std_logic_vector(1 downto 0);

signal s_IFILT_COEF_1 : std_logic_vector(15 downto 0);
signal s_IFILT_COEF_2 : std_logic_vector(15 downto 0);
signal s_IFILT_COEF_3 : std_logic_vector(15 downto 0);
signal s_IFILT_COEF_4 : std_logic_vector(15 downto 0);
signal s_IFILT_COEF_5 : std_logic_vector(15 downto 0);
	
signal s_SyncRRRR 	: std_logic;
signal s_IACQ_FILT 	: std_logic_vector(13 downto 0); 

signal s_Delay1 : std_logic;
signal s_Delay2 : std_logic;
signal s_Delay3 : std_logic;
signal s_Delay4 : std_logic; 
	
begin
-- Component Instantiation
Imeas_Filtering: Imeas_FILTER
	generic map( g_Length => 15)
	port map	(  Rst_i 			=> not(IFILT_EN_i),
					Clk_i 			=> FAST_CLK_i,
					Sync_i			=> SYNC_i,
					Sync_o			=> s_SyncRRRR,
					IFILT_COEF_1_i	=> s_IFILT_COEF_1, 		
					IFILT_COEF_2_i	=> s_IFILT_COEF_2,  	
					IFILT_COEF_3_i	=>	s_IFILT_COEF_3, 
					IFILT_COEF_4_i	=>	s_IFILT_COEF_4, 
					IFILT_COEF_5_i	=>	s_IFILT_COEF_5,						
					I_i   			=> IACQ_i,
					I_o   			=> s_IACQ_FILT); 
					
s_IFILT_COEF_1 <= IFILT_COEF_1_i;
s_IFILT_COEF_2 <= IFILT_COEF_2_i;
s_IFILT_COEF_3 <= IFILT_COEF_3_i;
s_IFILT_COEF_4 <= IFILT_COEF_4_i;
s_IFILT_COEF_5 <= IFILT_COEF_5_i;


--Creating Sync Delays for calculation time

	p_Delaying : process(FAST_CLK_i)
	begin
		if rising_edge(FAST_CLK_i) then
			if (RESET_i = '1') then 
				s_Delay1 <= '0';
				s_Delay2 <= '0';
				s_Delay3 <= '0';
				s_Delay4 <= '0'; 
			else
				s_Delay1 <= s_SyncRRRR;
				s_Delay2 <= s_Delay1;
				s_Delay3 <= s_Delay2;
				s_Delay4 <= s_Delay3; 
			end if;
		end if;
	end process p_Delaying;


-- Calculation 1: E(k) = Iref(k) - Iacq(k) 	

p_Calc1: process (clk)
begin
	if RESET_i='1' then
		s_Err 	<= (others => '0');
	elsif rising_edge(clk) then
		if (s_Delay1 = '1') then
			s_Iref 	<= resize(signed(IREF_i),15);
			s_Iaq		<= resize(signed(IACQ_i),15);
			s_Err 	<= s_Iref - s_Iaq;
		end if;
	end if;
end process;

-- Registering Values for Calculation 2 
p_Reg: process (clk)
begin
	if RESET_i = '1' then
		s_ErrR 	<= (others => '0');
		s_512YR	<= (others => '0');
		s_Mult1	<= (others => '0');
		s_Mult2	<= (others => '0');
	elsif rising_edge(clk) then
		if (s_Delay2 = '1') then
			s_ErrR 	<= s_Err;
			s_512YR	<= s_512Y;
			s_Mult1	<= resize(signed(MULT1_i),16);
			s_Mult2	<= resize(signed(MULT2_i),16);
		end if;
	end if;
end process;

-- Calculation 2: 
-- 512Y(k) = Mult1 x E(k) 	- Mult 2 x E(k-1) + 512 Y(k-1)
-- 512Y(k) = temp1			- temp2				+ 521 Y(K-1)

p_Calc2: process (clk)
variable v_temp1	: signed(30 downto 0) := (others => '0');
variable v_temp2	: signed(30 downto 0) := (others => '0');
variable v_Sum		: signed(30 downto 0) := (others => '0');
begin
	if (RESET_i = '1') then
		s_512YR 	<= (others => '0');
		s_Y		<= (others => '0');
	elsif rising_edge(clk) then
		v_temp1 	:= s_Mult1*s_Err;
		v_temp2 	:= s_Mult2*s_ErrR;
		v_Sum   	:= v_temp1 - v_temp2 + s_512Y;
		s_512Y 	<= v_Sum;
		s_Y		<= shift_right(s_512Y,9); -- Divide by 512
	end if;
end process;

-- Calculation 3: 
-- Y'(k) = Y(k) + 2Uc(k)
-- Where Uc(k) is s_Uc = VC_Measurement_i
  
p_Calc3:  process (clk)
begin
	if RESET_i = '1' then
		s_Uc 		<= (others => '0');
		s_YFeedF	<= (others => '0');
	elsif rising_edge(clk) then
		if (s_Delay3 = '1') then
			s_Uc		<= resize(signed(VC_Measurement_i),14);
			s_YFeedF <= s_Y + 2*s_Uc;
			s_YFeedF14 <= resize(s_YFeedF,14);
		end if;
	end if;
end process;
 
UC_OK_o <= '1' when s_Delay3='1' else '0';

-- Calculation 4
-- Y(k-1) = Iref x Initial_factor_i/2048 + Initial_Offset_i
-- temp1 = Iref x Initial_factor_i
-- temp2 = temp1/2048

p_Calc4:  process (clk)
variable v_temp1 	: signed (30 downto 0);
variable v_temp2	: signed (30 downto 0);
variable v_sum		: signed (30 downto 0);
begin
	if RESET_i = '1'  then
		s_YR 	<= (others => '0');
	elsif rising_edge(clk) then
		if (s_Delay4 = '1') then
			v_temp1 	:= s_Iref*s_Mult1;
			v_temp2 	:= shift_right(v_temp1,11);
			v_sum		:= v_temp2 + s_Mult2;
			s_YR 		<= resize(v_sum,31);
		end if;
	end if;
end process;

-- Output Process

s_CTRL <= Blocking_IGBT_i & Saturate_IGBT_i;

p_Output: process (clk) begin
	if RESET_i = '1' then
		ACTUATION_o <= (others => '0');
	elsif rising_edge(clk) then
		case s_CTRL is
			when "00" => 	ACTUATION_o 	<=  std_logic_vector(s_YFeedF14);
			when "01" => 	ACTUATION_o 	<= (others => '0');
			when "10" => 	ACTUATION_o 	<=  "11111111111111";
			when others => null;
		end case;
	end if;
end process;
			
TEST1_o <=(others => '0');
TEST2_o <=(others => '0'); 					
end rtl;