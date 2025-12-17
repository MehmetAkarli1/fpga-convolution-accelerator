-- Mehmet Akarli and Xie Xianghui
-- Sliding window signal buffer

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.math_custom.all;
use work.user_pkg.all;
use work.config_pkg.all;

entity signal_buffer4 is
    port (
        clk      : in  std_logic;
        rst      : in  std_logic;
        wr_data  : in  std_logic_vector(C_KERNEL_WIDTH-1 downto 0);
        rd_en    : in  std_logic;
        wr_en    : in  std_logic;
        rd_data  : out std_logic_vector(C_KERNEL_WIDTH*C_KERNEL_SIZE-1 downto 0);
        full     : out std_logic;
        empty    : out std_logic
    );
end signal_buffer4;

architecture Behavioral of signal_buffer4 is

    type word_array is array (0 to C_KERNEL_SIZE-1) of std_logic_vector(C_KERNEL_WIDTH-1 downto 0);

    signal bank, bank_next : word_array;

    signal fill_cnt, fill_cnt_next : integer range 0 to C_KERNEL_SIZE := 0;

begin

    ------------------------------------------------------------------------------
    -- SYNCHRONOUS UPDATE
    ------------------------------------------------------------------------------
    process(clk, rst)
    begin
        if rising_edge(clk) then
            if rst = '1' then
                bank      <= (others => (others => '0'));
                fill_cnt  <= 0;
            else
                bank      <= bank_next;
                fill_cnt  <= fill_cnt_next;
            end if;
        end if;
    end process;


    ------------------------------------------------------------------------------
    -- NEXT-STATE LOGIC FOR SHIFT REGISTER
    ------------------------------------------------------------------------------
    process(bank, wr_en, rd_en, wr_data)
        variable tmp : word_array;
    begin
        tmp := bank;

        ---------------------------------------------------------
        -- CASE 1: write AND read
        ---------------------------------------------------------
        if (wr_en = '1' and rd_en = '1') then
            -- shift everything upward
            for i in C_KERNEL_SIZE-1 downto 1 loop
                tmp(i) := bank(i-1);
            end loop;
            -- wr_data overrides position 0 (matches original)
            tmp(0) := wr_data;

        ---------------------------------------------------------
        -- CASE 2: write ONLY
        ---------------------------------------------------------
        elsif (wr_en = '1') then
            for i in C_KERNEL_SIZE-1 downto 1 loop
                tmp(i) := bank(i-1);
            end loop;
            tmp(0) := wr_data;

        ---------------------------------------------------------
        -- CASE 3: read ONLY
        ---------------------------------------------------------
        elsif (rd_en = '1') then
            for i in C_KERNEL_SIZE-1 downto 1 loop
                tmp(i) := bank(i-1);
            end loop;
            tmp(0) := (others => '0');   -- matches original behavior

        ---------------------------------------------------------
        -- CASE 4: idle
        ---------------------------------------------------------
        else
            tmp := bank;
        end if;

        bank_next <= tmp;
    end process;


    ------------------------------------------------------------------------------
    -- NEXT-STATE LOGIC FOR FILL-COUNT
    ------------------------------------------------------------------------------
    process(fill_cnt, wr_en, rd_en)
    begin
        fill_cnt_next <= fill_cnt;

        if (wr_en = '1' and rd_en = '1') then
            -- count stays the same
            fill_cnt_next <= fill_cnt;

        elsif (wr_en = '1') then
            if fill_cnt < C_KERNEL_SIZE then
                fill_cnt_next <= fill_cnt + 1;
            end if;

        elsif (rd_en = '1') then
            if fill_cnt > 0 then
                fill_cnt_next <= fill_cnt - 1;
            end if;

        end if;
    end process;


    ------------------------------------------------------------------------------
    -- OUTPUT PACKING (COMBINATIONAL)
    ------------------------------------------------------------------------------
    process(bank)
        variable flat : std_logic_vector(C_KERNEL_WIDTH*C_KERNEL_SIZE-1 downto 0);
    begin
        for i in 0 to C_KERNEL_SIZE-1 loop
            flat((i+1)*C_KERNEL_WIDTH-1 downto i*C_KERNEL_WIDTH) := bank(i);
        end loop;
        rd_data <= flat;
    end process;


    ------------------------------------------------------------------------------
    -- FULL / EMPTY FLAGS   (preserves your exact semantics)
    ------------------------------------------------------------------------------
    full  <= '1' when (fill_cnt = C_KERNEL_SIZE and rd_en = '0') else '0';
    empty <= '1' when (fill_cnt < C_KERNEL_SIZE) else '0';

end architecture;