library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.common_pack.all;

-- library UNISIM;
-- use UNISIM.VCOMPONENTS.ALL;
-- use UNISIM.VPKG.ALL;


entity cmdProc IS
  port(rxData:  in std_logic_vector(7 downto 0);
       rxnow:  in std_logic;
       ovErr:   in std_logic;		
       framErr: in std_logic;
       rxdone:  out std_logic;

       txnow :  out  STD_LOGIC;
       txData :   out  STD_LOGIC_VECTOR (7 downto 0);
       txdone : in  STD_LOGIC;

       start :       out std_logic;
       numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
       dataReady:    in std_logic;
       byte:         in std_logic_vector(7 downto 0);
       maxIndex:     in BCD_ARRAY_TYPE(2 downto 0);
       dataResults:  in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
       seqDone:      in std_logic;

       clk:          in std_logic;
       reset:        in std_logic
       );
end;

ARCHITECTURE myArch OF cmdProc IS
  type state_type is (INIT_A, FIRST_A, SECOND_A, THIRD_A, FORTH_A, FIFTH_A, SIXTH_A, SEVENTH_A, EIGHTH_A, NINETH_A, TENTH_A, ELEVENTH_A, TWELFTH_A);
  signal curState, nextState: STATE_TYPE;
  signal reg, reg_in: std_logic_vector(11 downto 0);
  signal count: integer:= 0;
  signal count_en, count_re: std_logic;
BEGIN
  
  regist: PROCESS(clk, reg, reg_in)
  BEGIN
    IF clk'event AND clk='1' THEN
      reg <= reg_in;
    END IF;
  END PROCESS;

  counter: PROCESS(clk, count_en,count)         --counter1
    VARIABLE Vcount: integer := 0;
  BEGIN
    IF rising_edge(clk) THEN
      IF count_en = '1' THEN
          Vcount := Vcount + 1;
      END IF;
    ELSIF count_re = '1' THEN
      Vcount := 0;
    END IF;
  count <= Vcount;
  END PROCESS;


  combi_nextState: process(curState, rxnow, txdone, seqDone, dataReady)
  BEGIN
  count_en <= '0';
  count_re <= '0';

  CASE curState IS
    
    WHEN INIT_A =>
      IF rxnow = '1' THEN 
        nextState <= FIRST_A;
      ELSE
        nextState <= INIT_A;
      END IF;


    WHEN FIRST_A =>
      IF count = 0  THEN
        IF rxData = "01100001" or rxData = "01000001" THEN
           nextState <= SECOND_A;
        ELSE
           nextState <= THIRD_A;
        END IF;

      ELSIF rxData="00110000" or rxData="00110001" or rxData="00110010" or rxData="00110011" or rxData="00110100" or rxData="00110101" or rxData="00110110" or rxData="00110111" or rxData="00111000" or rxData="00111001" THEN
        IF count = 1 THEN
          reg_in(11 downto 8) <= rxData(3 downto 0);
        ELSIF count = 2 THEN
          reg_in(7 downto 4) <= rxData(3 downto 0);
        ELSIF count = 3 THEN
          reg_in(3 downto 0) <= rxData(3 downto 0);
        END IF; 
        nextState <= SECOND_A;
      
      ELSE
        nextState <= THIRD_A;
      END IF;

    WHEN SECOND_A =>
      count_en <= '1';
      rxdone <= '1';
      nextState <= FORTH_A;

    WHEN THIRD_A =>
      count_re <= '1';
      rxdone <= '1';
      nextState <= FORTH_A;

    WHEN FORTH_A =>
      txData <= rxData;
      txnow <= '1';
      nextState <= FIFTH_A;

    WHEN FIFTH_A =>
      rxdone <= '0';
      txnow <= '0';
      IF count < 4 THEN
        nextState <= INIT_A;
      ELSE
        count_re <= '1' ;
	numWords_bcd(2) <= reg(11 downto 8);
        numWords_bcd(1) <= reg(7 downto 4);
   	numWords_bcd(0) <= reg(3 downto 0);
        nextState <= SIXTH_A;
      END IF; 
        
    WHEN SIXTH_A =>
      start <= '1';
      nextState <= SEVENTH_A;
  
    WHEN SEVENTH_A =>
      start <= '0';
      IF dataReady = '1' THEN
        IF byte(7 downto 4) < "1010" THEN
           reg_in(7 downto 4) <= "0011";
           reg_in(3 downto 0) <= byte(7 downto 4);
         ELSIF byte(7 downto 4) = "1010" THEN  --A
           reg_in(7 downto 0) <= "01000001";               --ascii code corresponde to 'A'

         ELSIF byte(7 downto 4) = "1011" THEN  --B
           reg_in(7 downto 0) <= "01000010";               --ascii code corresponde to 'B'

         ELSIF byte(7 downto 4) = "1100" THEN  --C
           reg_in(7 downto 0) <= "01000011";               --ascii code corresponde to 'C'

         ELSIF byte(7 downto 4) = "1101" THEN  --D
           reg_in(7 downto 0) <= "01000100";               --ascii code corresponde to 'D'

         ELSIF byte(7 downto 4) = "1110" THEN  --E
           reg_in(7 downto 0) <= "01000101";               --ascii code corresponde to 'E'

         ELSIF byte(7 downto 4) = "1111" THEN  --F
           reg_in(7 downto 0) <= "01000110";               --ascii code corresponde to 'F'

         END IF;
         nextState <= EIGHTH_A;
       ELSE
         nextState <= SEVENTH_A;
       END IF;

    WHEN EIGHTH_A =>
      txnow <= '1';
      txData <= reg(7 downto 0);
      IF seqDone = '1' THEN
        count_en <= '1';
      END IF;
      nextState <= NINETH_A;
      
    WHEN NINETH_A =>
      txnow <= '0';
      IF txdone = '1' THEN
        IF byte(3 downto 0) < "1010" THEN
          reg_in(7 downto 4) <= "0011";
          reg_in(3 downto 0) <= byte(3 downto 0);
        ELSIF byte(3 downto 0) = "1010" THEN  --A
          reg_in(7 downto 0) <= "01000001";               --ascii code correspond to 'A'

        ELSIF byte(3 downto 0) = "1011" THEN  --B
          reg_in(7 downto 0) <= "01000010";               --ascii code correspond to 'B'

        ELSIF byte(3 downto 0) = "1100" THEN  --C
          reg_in(7 downto 0) <= "01000011";               --ascii code correspond to 'C'

        ELSIF byte(3 downto 0) = "1101" THEN  --D
          reg_in(7 downto 0) <= "01000100";               --ascii code correspond to 'D'

        ELSIF byte(3 downto 0) = "1110" THEN  --E
          reg_in(7 downto 0) <= "01000101";               --ascii code correspond to 'E'

        ELSIF byte(3 downto 0) = "1111" THEN  --F
          reg_in(7 downto 0) <= "01000110";               --ascii code correspond to 'F'

        END IF;
      nextState <= TENTH_A;
      ELSE
        nextState <= NINETH_A;
      END IF;

    WHEN TENTH_A =>
      txnow <= '1';
      txData <= reg(7 downto 0);
      IF count = 1 THEN
        count_re <= '1';
        nextState <= INIT_A;
      ELSE
        nextState <= ELEVENTH_A;

      END IF;

    WHEN ELEVENTH_A =>
      txnow <= '0';
      IF txdone = '1' THEN
        reg_in(7 downto 0) <= "00100000";
        nextState <= TWELFTH_A;
      ELSE
        nextState <= ELEVENTH_A;
      END IF;

    WHEN TWELFTH_A => 
      txnow <= '1';
      txData <= reg(7 downto 0);
      nextState <= SIXTH_A;

    WHEN OTHERS =>
      nextState <= INIT_A;

    END CASE;
  END PROCESS;

   seq_state: PROCESS (clk, reset)   --CLK set
   BEGIN
    IF reset = '1' THEN
      curState <= INIT_A;
    ELSIF clk'EVENT AND clk='1' THEN  
      curState <= nextState;
    END IF;
  END PROCESS; -- seq
      
END;        
      	