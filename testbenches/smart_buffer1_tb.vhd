library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.user_pkg.all;

entity tb_smart_b is
end tb_smart_b;

architecture sim of tb_smart_b is

    signal clk     : std_logic := '0';
    signal rst     : std_logic := '1';
    signal wr_en   : std_logic := '0';
    signal rd_en   : std_logic := '0';
    signal wr_data : std_logic_vector(SIGNAL_WIDTH_RANGE) := (others => '0');

    signal full    : std_logic;
    signal empty   : std_logic;
    signal rd_data : std_logic_vector(C_KERNEL_SIZE * C_KERNEL_WIDTH - 1 downto 0);

    constant CLK_PERIOD : time := 10 ns;

begin

    clk <= not clk after CLK_PERIOD/2;

    DUT : entity work.signal_buffer
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

    stim_proc: process
        procedure write_sample(val : natural) is
        begin
            wr_en   <= '1';
            wr_data <= std_logic_vector(to_unsigned(val, C_KERNEL_WIDTH));
            wait until rising_edge(clk);
            wr_en   <= '0';
            wait until rising_edge(clk);
        end procedure;

        procedure consume_window is
        begin
            rd_en <= '1';
            wait until rising_edge(clk);
            rd_en <= '0';
            wait until rising_edge(clk);
        end procedure;

    begin
        rst <= '1';
        wait for 40 ns;
        rst <= '0';
        wait until rising_edge(clk);

        report "STARTING SMART BUFFER TEST";
        
        for i in 1 to C_KERNEL_SIZE loop
            write_sample(i);
        end loop;

        wait until rising_edge(clk);
        assert full = '1'  report "ERROR: not full after filling" severity error;

        consume_window;
        assert full = '1'  report "ERROR: full dropped after one read" severity error;

        for i in C_KERNEL_SIZE+1 to C_KERNEL_SIZE+15 loop
            wr_en   <= '1';
            rd_en   <= '1';
            wr_data <= std_logic_vector(to_unsigned(i, C_KERNEL_WIDTH));
            wait until rising_edge(clk);
            wr_en   <= '0';
            rd_en   <= '0';
            wait until rising_edge(clk);
        end loop;

        for i in 1 to C_KERNEL_SIZE+10 loop
            consume_window;
        end loop;

        assert empty = '1' report "ERROR: not empty at end" severity error;

        report "ALL TESTS PASSED - SLIDING WINDOW WORKS!";
        wait;
    end process;

end sim;