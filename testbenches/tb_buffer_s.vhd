-- University Of Florida
-- Mehmet Akarli and Xie Xianghui
-- File: tb_buffer_s.vhd

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.user_pkg.all;

entity tb_buffer_s is
end tb_buffer_s;

architecture behav of tb_buffer_s is

    signal clk       : std_logic := '0';
    signal rst       : std_logic := '1';
    signal wr_en     : std_logic := '0';
    signal wr_data   : std_logic_vector(15 downto 0);
    signal rd_en     : std_logic := '0';
    signal full      : std_logic;
    signal empty     : std_logic;
    signal rd_data   : std_logic_vector(C_KERNEL_SIZE*16-1 downto 0);

    -- 100 MHz clock
    constant CLK_PERIOD : time := 10 ns;

begin

    -- Instantiate your buffer
    DUT: entity work.signal_buffer4
        port map (
            clk     => clk,
            rst     => rst,
            wr_en   => wr_en,
            wr_data => wr_data,
            rd_en   => rd_en,
            full    => full,
            empty   => empty,
            rd_data => rd_data
        );

    -- Clock generation
    clk <= not clk after CLK_PERIOD/2;

    -- Stimuli
    process
    begin
        -- Reset
        rst <= '1';
        wait for 4*CLK_PERIOD;
        rst <= '0';
        wait for CLK_PERIOD;

        -- Fill the entire buffer with 0,1,2,...,127
        report "Filling buffer...";
        for i in 0 to C_KERNEL_SIZE-1 loop
            wr_en   <= '1';
            wr_data <= std_logic_vector(to_unsigned(i, 16));
            wait for CLK_PERIOD;
        end loop;
        wr_en <= '0';
        wait for 5*CLK_PERIOD;

        -- One read → should see 0,1,2,...,126 shifted
        report "Reading once...";
        rd_en <= '1';
        wait for CLK_PERIOD;
        rd_en <= '0';
        wait for 10*CLK_PERIOD;

        -- Write one more + read one more (simultaneous)
        report "Write+Read simultaneous...";
        wr_en   <= '0';
        rd_en   <= '1';
        wr_data <= std_logic_vector(to_unsigned(999, 16));
        wait for CLK_PERIOD;
        wr_en <= '0';
        rd_en <= '0';

        wait for 20*CLK_PERIOD;
        report "Test finished - check waveform!";
        wait;
    end process;

end behav;