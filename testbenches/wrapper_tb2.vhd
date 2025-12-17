-- wrapper_tb2.vhd
-- Mehmet Akarli

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;
use work.config_pkg.all;
use work.user_pkg.all;

entity wrapper_tb is
end wrapper_tb;

architecture sim of wrapper_tb is
    constant CLK_PERIOD : time := 10 ns;

    -- Exact sizes from main.cpp
    constant SMALL_SIGNAL   : integer := 10;
    constant MEDIUM_SIGNAL  : integer := 1000;
    constant BIG_SIGNAL     : integer := 65282;
    constant SMALL_KERNEL   : integer := 4;
    constant MEDIUM_KERNEL  : integer := 40;
    constant BIG_KERNEL     : integer := 128;

    signal clk  : std_logic := '0';
    signal rst  : std_logic := '1';
    signal clks : std_logic_vector(NUM_CLKS_RANGE) := (others => '0');

    signal mmap_wr_en         : std_logic := '0';
    signal mmap_wr_addr       : std_logic_vector(MMAP_ADDR_RANGE) := (others => '0');
    signal mmap_wr_data       : std_logic_vector(MMAP_DATA_RANGE) := (others => '0');
    signal mmap_rd_en         : std_logic := '0';
    signal mmap_rd_addr       : std_logic_vector(MMAP_ADDR_RANGE) := (others => '0');
    signal mmap_rd_data       : std_logic_vector(MMAP_DATA_RANGE);
    signal mmap_rd_data_valid : std_logic;

    type u16_array is array (natural range <>) of unsigned(15 downto 0);

begin
    clk <= not clk after CLK_PERIOD/2;
    clks <= (others => clk);  -- all clocks same

    DUT: entity work.wrapper
        port map (
            clks               => clks,
            rst                => rst,
            mmap_wr_en         => mmap_wr_en,
            mmap_wr_addr       => mmap_wr_addr,
            mmap_wr_data       => mmap_wr_data,
            mmap_rd_en         => mmap_rd_en,
            mmap_rd_addr       => mmap_rd_addr,
            mmap_rd_data       => mmap_rd_data,
            mmap_rd_data_valid => mmap_rd_data_valid
        );

        process
        -- Simple write
        procedure write_reg(addr : natural; data : unsigned(31 downto 0)) is
        begin
            mmap_wr_addr <= std_logic_vector(to_unsigned(addr, mmap_wr_addr'length));
            mmap_wr_data <= std_logic_vector(data);
            mmap_wr_en   <= '1';
            wait for CLK_PERIOD;
            mmap_wr_en   <= '0';
            wait for CLK_PERIOD;
        end procedure;

        -- Simple read
        procedure read_reg(addr : natural; variable data : out unsigned(31 downto 0)) is
            variable tmp : unsigned(31 downto 0);
        begin
            mmap_rd_addr <= std_logic_vector(to_unsigned(addr, mmap_rd_addr'length));
            mmap_rd_en   <= '1';
            wait for CLK_PERIOD;
            mmap_rd_en   <= '0';
            loop
                wait for CLK_PERIOD;
                exit when mmap_rd_data_valid = '1';
            end loop;
            tmp := unsigned(mmap_rd_data);
            data := tmp;
        end procedure;

        -- Overloaded write_reg for std_logic_vector address
        procedure write_reg(addr : std_logic_vector; data : unsigned(31 downto 0)) is
        begin
            write_reg(to_integer(unsigned(addr)), data);
        end procedure;

        -- Overloaded read_reg for std_logic_vector address
        procedure read_reg(addr : std_logic_vector; variable data : out unsigned(31 downto 0)) is
        begin
            read_reg(to_integer(unsigned(addr)), data);
        end procedure;

        -- Golden reference convolution (exact match convolveSW)
        procedure golden_convolve(
            sig    : u16_array;
            kern   : u16_array;
            variable result : out u16_array
        ) is
            variable sum, prod : unsigned(31 downto 0);
            constant MAX16 : unsigned(31 downto 0) := x"0000FFFF";
        begin
            for i in 0 to sig'length + kern'length - 2 loop
                sum := (others => '0');
                for k in 0 to kern'length-1 loop
                    if i-k >= 0 and i-k < sig'length then
                        prod := resize(sig(i-k), 32) * resize(kern(k), 32);
                        sum := sum + prod;
                    end if;
                end loop;

                if sum > MAX16 then
                    result(i) := MAX16(15 downto 0);
                else
                    result(i) := sum(15 downto 0);
                end if;
            end loop;
        end procedure;

        -- Run one full test
        procedure run_test(
            name         : string;
            sig_size     : integer;
            kern_size    : integer;
            zeros_sig    : boolean;
            ones_sig     : boolean;
            rand_small   : boolean;
            rand_full    : boolean;
            weight       : real;
            variable total_score : inout real
                ) is
            variable sig, kern, padded_sig, golden, hw_out : u16_array(0 to 70000);
            variable seed1, seed2 : positive := 1234;
            variable r : real;
            variable errors : integer := 0;
            variable done_val : unsigned(31 downto 0);
            variable tmp32 : std_logic_vector(31 downto 0);
        begin

            report "=== " & name & " ===";

            -- Generate signal
            for i in 0 to sig_size-1 loop
                if zeros_sig then sig(i) := x"0000";
                elsif ones_sig then sig(i) := x"0001";
                elsif rand_small then uniform(seed1,seed2,r); sig(i) := to_unsigned(integer(r*15.0),16);
                else uniform(seed1,seed2,r); sig(i) := to_unsigned(integer(r*65536.0),16);
                end if;
            end loop;

            -- Generate kernel
            for i in 0 to kern_size-1 loop
                if zeros_sig then kern(i) := x"0000";
                elsif ones_sig then kern(i) := x"0001";
                elsif rand_small then uniform(seed1,seed2,r); kern(i) := to_unsigned(integer(r*15.0),16);
                else uniform(seed1,seed2,r); kern(i) := to_unsigned(integer(r*65536.0),16);
                end if;
            end loop;

            -- Pad signal
            for i in 0 to 126 loop padded_sig(i) := x"0000"; end loop;
            for i in 0 to sig_size-1 loop padded_sig(127 + i) := sig(i); end loop;
            for i in sig_size+127 to padded_sig'length-1 loop padded_sig(i) := x"0000"; end loop;

            -- Golden result
            golden_convolve(sig(0 to sig_size-1), kern(0 to kern_size-1), golden);

            -- HW run
            write_reg(16#1FFFF#, to_unsigned(0, 32)); -- user mode off

            for i in 0 to (sig_size + 254 + 1)/2 - 1 loop
                tmp32 := std_logic_vector(padded_sig(2*i+1)) & std_logic_vector(padded_sig(2*i));
                write_reg(i, unsigned(tmp32(31 downto 0)));
            end loop;

            write_reg(16#1FFFF#, to_unsigned(1, 32)); -- user mode on

            write_reg(C_CLEAR_ADDR, to_unsigned(1, 32));
            write_reg(C_SIGNAL_SIZE_ADDR, to_unsigned(sig_size, 32));
            for i in 0 to 127 loop
                if i < kern_size then
                    write_reg(C_KERNEL_DATA_ADDR, resize(kern(i), 32));
                else
                    write_reg(C_KERNEL_DATA_ADDR, to_unsigned(0, 32));
                end if;
            end loop;
            write_reg(C_GO_ADDR, to_unsigned(1, 32));

            -- Wait for done
            loop
                read_reg(C_DONE_ADDR, done_val);
                exit when done_val(0) = '1';
            end loop;

            -- Read output
            write_reg(16#1FFFF#, to_unsigned(0, 32)); -- user mode off

            for i in 0 to (sig_size + kern_size)/2 loop
                read_reg(i, done_val);
                if 2*i   < golden'length then hw_out(2*i)   := done_val(15 downto 0); end if;
                if 2*i+1 < golden'length then hw_out(2*i+1) := done_val(31 downto 16); end if;
            end loop;

            -- Check
            errors := 0;
            for i in 0 to golden'length-1 loop
                if hw_out(i) /= golden(i) then
                    errors := errors + 1;
                end if;
            end loop;

            report name & " => " & integer'image(errors) & " errors (" &
                   real'image(100.0 * real(golden'length - errors) / real(golden'length)) & "%)";

            total_score := total_score + weight * real(golden'length - errors) / real(golden'length);
        end procedure;

        variable score : real := 0.0;

    begin
        rst <= '1'; wait for 20 ns; rst <= '0'; wait for 100 ns;

        run_test("Zeros small",      SMALL_SIGNAL,  SMALL_KERNEL,  true,  false, false, false, 0.05, score);
        run_test("Ones small",       SMALL_SIGNAL,  SMALL_KERNEL,  false, true,  false, false, 0.10, score);
        run_test("RandNoClip small", SMALL_SIGNAL,  SMALL_KERNEL,  false, false, true,  false, 0.10, score);
        run_test("RandNoClip med",   MEDIUM_SIGNAL, MEDIUM_KERNEL, false, false, true,  false, 0.15, score);
        run_test("RandNoClip big",   BIG_SIGNAL,    BIG_KERNEL,    false, false, true,  false, 0.15, score);
        run_test("Rand small",       SMALL_SIGNAL,  SMALL_KERNEL,  false, false, false, true,  0.10, score);
        run_test("Rand med",         MEDIUM_SIGNAL, MEDIUM_KERNEL, false, false, false, true,  0.15, score);
        run_test("Rand big",         BIG_SIGNAL,    BIG_KERNEL,    false, false, false, true,  0.20, score);

    --    report "=====================================";
        report "FINAL SCORE: " & real'image(score * 100.0) & " / 100";
    --    report "=====================================";

        wait;
    end process;

end sim;