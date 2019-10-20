-- Second Order Lowpass Filter
-- Filters noise out of input current measurement
--
-- Equation:
-- R(K)= F1*u(k)+ F2*u(k-1)+ F3*u(k-2)+ F4*Y(k-1)*1024^(-1)- F5*Y(k-2)*1024^(-1)
-- Y(K)= R(K)*1024^(-1)
--
-- Where Y(K) is the output filtered current, u(k) is the input 
-- current measurement, and F1 to F3 are constant coefficients.

library IEEE;
use IEEE.Std_logic_1164.all;
use IEEE.Numeric_std.all;

entity Imeas_FILTER is
  generic(  g_Length   : integer range 1 to 32   := 14);
    port (  Rst_i 				: in  std_logic;
            Clk_i 				: in  std_logic; -- 40 MHz clock
            Sync_i				: in  std_logic;
            Sync_o				: out std_logic;
			IFILT_COEF_1_i	  	: in  std_logic_vector(15 downto 0);	
			IFILT_COEF_2_i	  	: in  std_logic_vector(15 downto 0);
			IFILT_COEF_3_i		: in  std_logic_vector(15 downto 0);
			IFILT_COEF_4_i		: in  std_logic_vector(15 downto 0);
			IFILT_COEF_5_i		: in  std_logic_vector(15 downto 0);
            I_i   				: in  std_logic_vector((g_Length-1) downto 0);  -- Input Current, Unfiltered
            I_o   				: out std_logic_vector((g_Length-1) downto 0)); -- Output Current, Filtered
end;

architecture RTL of Imeas_FILTER is

signal s_SyncR		 	: std_logic := '0';
signal s_SyncRR	 		: std_logic := '0';
signal s_SyncRRR	 	: std_logic := '0';
signal s_SyncRRRR	 	: std_logic := '0';
signal s_Ena		 	: std_logic := '0';

signal s_Iacq      		: signed((g_Length-1) downto 0) := (others => '0');
signal s_IacqR     		: signed((g_Length-1) downto 0) := (others => '0');
signal s_IacqRR    		: signed((g_Length-1) downto 0) := (others => '0');

signal s_Ifilt     		: signed((g_Length-1) downto 0) := (others => '0');

signal s_IfiltP	 		: signed(((g_Length*2)-1+4) downto 0) := (others => '0');
signal s_IfiltPR   		: signed(((g_Length*2)-1+4) downto 0) := (others => '0');
signal s_IfiltPRR  		: signed(((g_Length*2)-1+4) downto 0) := (others => '0');

signal s_F1				: signed((g_Length-1) downto 0) := (others => '0');
signal s_F2 			: signed((g_Length-1) downto 0) := (others => '0');
signal s_F3 			: signed((g_Length-1) downto 0) := (others => '0');
signal s_F4 			: signed((g_Length-1) downto 0) := (others => '0');
signal s_F5 			: signed((g_Length-1) downto 0) := (others => '0');

begin

-- Edge Detector for Sync Signal

p_EnaGen:	process (Clk_i)
begin
	if rising_edge(clk_i) then
	  if (Sync_i /= s_SyncR) then
			 s_Ena <= '1';
		else
			 s_Ena <= '0';
		end if;
		s_SyncR    <= Sync_i;
	  s_SyncRR   <= s_SyncR;
	  s_SyncRRR  <= s_SyncRR;
	  s_SyncRRRR <= s_SyncRRR;
	end if;
end process;


-- Output Sync Signal is Sync_i delayed four clock cycles
-- to allow the calculation to be completed.

Sync_o <= s_SyncRRRR;

-- Registering of Values

p_Reg: process (Clk_i)
begin
  if rising_edge(clk_i) then
    if (Rst_i = '1') then
      s_IacqR     <= (others => '0');
      s_IacqRR    <= (others => '0');
      s_IfiltPR    <= (others => '0');
      s_IfiltPRR   <= (others => '0');
    elsif (s_Ena = '1') then
      s_IacqR   <= s_Iacq;   			-- u(k-1)
      s_IacqRR  <= s_IacqR;  			-- u(k-2)
      s_IfiltPR  <= s_IfiltP;  		-- Y(k-1)
      s_IfiltPRR <= s_IfiltPR; 			-- Y(k-2)
    end if;
  end if;
end process;

p_Sync_Coef:  process (Clk_i)
begin
	if rising_edge(clk_i) then
		s_F1 <= resize(signed(IFILT_COEF_1_i),g_Length);
		s_F2 <= resize(signed(IFILT_COEF_2_i),g_Length);
		s_F3 <= resize(signed(IFILT_COEF_3_i),g_Length);
		s_F4 <= resize(signed(IFILT_COEF_4_i),g_Length);
		s_F5 <= resize(signed(IFILT_COEF_5_i),g_Length);
	end if;
end process;

--Calcultation of the filter equation
-- 4096*Y(K) = F1*u(k)  + F2*u(k-1)+ F1*u(k-2)+ F3*Y(k-1)- F4*Y(k-2)
-- s_Sum    <= temp1 	+ 	temp2   + temp3  	 + temp4 	- temp5; 		-- 4096*Y(K)
p_PipeCalc: process(Clk_i)

variable v_temp1		: signed(((g_Length*2)-1)   downto 0) := (others => '0');
variable v_temp2		: signed(((g_Length*2)-1)   downto 0) := (others => '0');	
variable v_temp3		: signed(((g_Length*2)-1)   downto 0) := (others => '0');
variable v_temp4		: signed(((g_Length*3)-1+4) downto 0) := (others => '0');    
variable v_temp5		: signed(((g_Length*3)-1+4) downto 0) := (others => '0');
variable v_Sum    	: signed(((g_Length*3)-1+6) downto 0) := (others => '0');    
begin
    if rising_edge(Clk_i) then
		 if (Rst_i = '1') then
		 	s_Iacq		<= (others => '0');
			v_temp1 		:= (others => '0');
			v_temp2 		:= (others => '0');
			v_temp3 		:= (others => '0');
			v_temp4 		:= (others => '0');
			v_temp5 		:= (others => '0');
			v_Sum	  		:= (others => '0');
			s_IfiltP 	<= (others => '0');
		 else
			if (I_i = x"1FFF") then
				s_Iacq 		<= (others => '0');
			else
				s_Iacq 		<= signed(I_i);
			end if;
			v_temp1	  	:= s_Iacq*s_F1; 	 	--F1*u(k)   Width: 2*g_Length
			v_temp2   	:= s_IacqR*s_F2; 	 	--F2*u(k-1)	Width: 2*g_Length						
			v_temp3		:= s_IacqRR*s_F3;	 	--F3*u(k-2) Width: 2*g_Length
			v_temp4 	:= s_IfiltPR*s_F4;	--F4*Y(k-1) Width: width(s_IfiltPR)+g_Length
			v_temp5   	:= s_IfiltPRR*s_F5; 	--F5*Y(k-2) Width: width(s_IfiltPR)+g_Length
			v_Sum 		:= resize(v_temp1,((g_Length*2)+4)) + resize(v_temp2,((g_Length*2)+4)) + resize(v_temp3,((g_Length*2)+4)) + resize(shift_right(v_temp4,10),((g_Length*3)+6)) - resize(shift_right(v_temp5,10),((g_Length*3)+6));
			s_IfiltP  	<= resize(v_Sum,((g_Length*2)+4));
		 end if; 
	 end if;
end process;
  
  
p_RegOutputs: process(Clk_i)
begin
    if rising_edge(Clk_i) then
		s_Ifilt 	<= resize(shift_right(s_IfiltP,10),g_Length);
		I_o 	  	<= std_logic_vector(s_Ifilt); 
	 end if;
end process;

end architecture;