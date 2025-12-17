-- File: tb_buffer_s.vhd
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.user_pkg.all;
use work.config_pkg.all;

entity tb_buffer_k is
end tb_buffer_k;

architecture behav of tb_buffer_k is

    signal clk       : std_logic := '0';
    signal rst       : std_logic := '1';
    signal wr_en     : std_logic := '0';
    signal wr_data   : std_logic_vector(15 downto 0);
    signal rd_en     : std_logic := '0';
    signal full      : std_logic;
    signal empty     : std_logic;
    signal rd_data   : window(0 to C_KERNEL_SIZE - 1);

    -- 100 MHz clock
    constant CLK_PERIOD : time := 10 ns;

begin

    -- Instantiate your buffer
    DUT: entity work.buffer_k3
        port map (
            clk     => clk,
            rst     => rst,
            wr_en   => wr_en,
            wr_data => wr_data,
            rd_en   => rd_en,           -- always '0' -> no sliding
            full    => full,
            empty   => empty,
            rd_data => rd_data
        );

    -- Clock generation
    clk <= not clk after CLK_PERIOD/2;

   process
        variable expected : window(0 to C_KERNEL_SIZE - 1) := (others => (others => '0'));
    begin
        rst <= '1';
        wait for 40 ns;
        rst <= '0';
        wait for 10 ns;

        report "KERNEL BUFFER TEST: Loading kernel (rd_en = '0' forever)";

        -- Load C_KERNEL_SIZE values → they will be shifted all the way to the end
        for i in 0 to C_KERNEL_SIZE-1 loop
            wr_en   <= '1';
            wr_data <= std_logic_vector(to_unsigned(1000 + i, 16));
            -- Build expected value: first written ends up in MSB
            -- Shift like the DUT buffer
        for j in C_KERNEL_SIZE-1 downto 1 loop
            expected(j) := expected(j-1);
        end loop;

        -- Insert newest at index 0
        expected(0) := std_logic_vector(to_unsigned(1000 + i, 16));

        wait for 10 ns;
        end loop;
        wr_en <= '0';

        wait for 50 ns;

        if rd_data /= expected then
            report "KERNEL LOAD FAILED!" severity failure;
        else
            report "KERNEL LOAD CORRECT!" severity note;
        end if;

        if full /= '1' then
            report "ERROR: full flag not asserted after loading kernel!" severity failure;
        end if;

        -- Prove it NEVER changes (even after 100 cycles)
        wait for 1000 ns;
        if rd_data /= expected then
            report "KERNEL CORRUPTED OVER TIME!" severity failure;
        else
            report "KERNEL BUFFER PERFECTLY STATIC, TEST PASSED" severity note;
        end if;

        report "Kernel buffer test finished";
        wait;
    end process;

end behav;