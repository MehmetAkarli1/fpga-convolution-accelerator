-- University Of Florida
-- Mehmet Akarli and Xie Xianghui

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.user_pkg.all;

entity buffer_k3 is
    port (
        clk       : in  std_logic;    
        rst       : in  std_logic;
        wr_en     : in  std_logic;
        wr_data   : in  std_logic_vector(SIGNAL_WIDTH_RANGE);
        rd_en     : in  std_logic;
        full      : out std_logic;
        empty     : out std_logic;
        rd_data   : out window(0 to C_KERNEL_SIZE - 1)
        );
end buffer_k3;

architecture BHV of buffer_k3 is

signal full_int    : std_logic; 
signal empty_int   : std_logic;
--signal rd_data_int : std_logic_vector(C_KERNEL_SIZE*16-1  downto 0); 

signal buf : window(0 to C_KERNEL_SIZE-1);
signal count : integer range 0 to C_KERNEL_SIZE := 0;

begin
    -- Write process
    process(clk, rst)
    begin
        if (rst = '1') then
            buf     <= (others => (others => '0'));

        elsif (rising_edge(clk)) then

            if (wr_en = '1') then
                if (rd_en = '0') then

                for i in C_KERNEL_SIZE-1 downto 1 loop  
                buf(i) <= buf(i- 1);
                end loop;
                end if;

            buf(0) <= wr_data;
            end if;

        -- For read
        if (rd_en = '1') then
            
            for i in C_KERNEL_SIZE - 1 downto 1 loop
                buf(i) <= buf(i - 1);
            end loop;
            
        buf(0) <= (others => '0');
        end if;    

        end if;
    end process;

    -- Count process 
    process(clk, rst)
        variable next_count : integer range 0 to C_KERNEL_SIZE;
    begin
        if (rst = '1') then
            count <= 0;
            --full_int    <= '0';
            --empty_int   <= '1';
        elsif (rising_edge(clk)) then
            next_count := count;

            -- Write logic (only increment if not at max)
            if (wr_en = '1' and next_count < C_KERNEL_SIZE) then
            next_count := next_count + 1;
            end if;

            -- Read logic (only decrement if not at 0)
            if (rd_en = '1' and next_count > 0) then
                --if (empty_int = '1') then
                    next_count := next_count - 1; -- only read when not empty
                else
                    next_count := next_count;     -- Do nothing
                --end if;    
            end if;

        count <= next_count;     

        end if; 
    end process;
    
    -- Read process
    --process(buf)
    --   variable tmp : std_logic_vector(C_KERNEL_SIZE*16-1 downto 0);
    --   begin
    --    for i in 0 to C_KERNEL_SIZE-1 loop
    --        tmp( (i+1)*16-1 downto i*16 ) := buf(i);
    --    end loop;
    --    
    --    rd_data_int <= tmp;
    --end process;

    full_int  <= '1' when (count = C_KERNEL_SIZE and rd_en = '0') else '0';
    empty_int <= '1' when count < C_KERNEL_SIZE else '0'; 

    full  <= full_int;
    empty <= empty_int; 
    rd_data <= buf;
            
end BHV;