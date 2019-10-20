entity synchex2 is
port (   
         clkA     : in std_logic; -- Slow Clock Domain                             
         clkB     : in std_logic; -- Fast Clock Domain                          
         sigB     : in std_logic;
         sigA     : out std_logic
      );
end synchex2;
      
-- ToDo: We want to transfer sigB, which originates 
-- at the clkB domain, to the clkA domain, and then
-- output it as sigA.       
      
architecture behaviour of synchex2 is 

-- Clock Counter Signals
signal ClkBCnt         : std_logic_vector(1 downto 0); 
-- The length of ClkBCnt must be chosen by analysing 
-- the relationship between clkA and clkB. 
-- The MSB of ClkBCnt must change at a rate that
-- satisfies the three edge requirement. 

-- Sampler 1 Alias and Signals
alias  ClkBCntMSB      : std_logic is ClkBCnt(ClkBCnt'left);
signal ClkBCntMSBR1    : std_logic; 
signal SampEn          : std_logic;
signal SampSigB        : std_logic;

-- Synchronizer Signals
signal SampSigBR       : std_logic;
signal SampSigBRR      : std_logic;

------------------ CLOCK COUNTER ------------------
pCountClkB: process (clkB) is
begin
   if (rising_edge(clkB)) then
      if resetRR = '1' then
         ClkBCnt <= ( others => '0');
      else 
         ClkBCnt <= ClkBCnt + '1';
      end if;
   end if;
end process;

-------------------- SAMPLER 1 --------------------
-- Sample SigB every 8th ClkB pulse

-- Generate Sample Enable Signal
pGenSampEn1: process (clkB) is
begin
   if (rising_edge(clkB)) then
       ClkBCntMSBR1 <= ClkBCntMSB;
      if (ClkBCntMSBR1 = '0' and ClkBCntMSB = '1') 
      then   
         SampEn <= '1';
      else
         SampEn <= '0';
      end if;
   end if;
end process;

-- Sample sigB to SampSigB
pSampleSigB:  process (clkB) is
begin
   if (rising_edge(clkB)) then
	   if(SampEn = '1') then
         SampSigB <= SigB;
		end if;
   end if;    
end process; 

-- SampSigB is SigB at the transition, stretched

------------------ Synchronizer -------------------
----------------- into clkA domain ----------------- 

pSynch: process (clkA) is
begin
   if (rising_edge(clkA)) then
      SampSigBR  <= SampSigB;
      SampSigBRR <= SampSigBR;
   end if;
end process;