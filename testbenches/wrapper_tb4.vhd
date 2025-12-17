-- Mehmet Akarli and Xie Xianghui
-- University of Florida
-- EEL 5721/4720 Reconfigurable Computing
--
-- File: wrapper_tb.vhd
--
-- This testbench simulates the dram_test application from the wrapper entity
-- so that it can access the emulated DRAMs.
--
-- IMPORTANT: This is not provided as a thorough testbench. It is up to you
-- to extend it to test your own design.

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

use work.config_pkg.all;
use work.user_pkg.all;
--use work.user_pkg_test.all;

entity wrapper_convolve_tb is
end wrapper_convolve_tb;

architecture behavior of wrapper_convolve_tb is

    constant TEST_SIZE  : integer := 256;
    constant MAX_CYCLES : integer := TEST_SIZE*100;

    constant MAX_MMAP_READ_CYCLES : integer := 100;


    constant CLK0_HALF_PERIOD : time := 5 ns;

    signal clks : std_logic_vector(NUM_CLKS_RANGE) := (others => '0');
    signal rst  : std_logic                        := '1';

    signal mmap_wr_en   : std_logic                         := '0';
    signal mmap_wr_addr : std_logic_vector(MMAP_ADDR_RANGE) := (others => '0');
    signal mmap_wr_data : std_logic_vector(MMAP_DATA_RANGE) := (others => '0');

    signal mmap_rd_en         : std_logic                         := '0';
    signal mmap_rd_addr       : std_logic_vector(MMAP_ADDR_RANGE) := (others => '0');
    signal mmap_rd_data       : std_logic_vector(MMAP_DATA_RANGE);
    signal mmap_rd_data_valid : std_logic;

    type signal_array_t is array(0 to C_MAX_SIGNAL_SIZE-1) of std_logic_vector(15 downto 0);
    signal padded_signal : signal_array_t := (others => (others => '0'));

    type int_array_t is array (natural range <>) of integer;

    type expected_word_t is record
        w  : unsigned(C_MMAP_DATA_WIDTH-1 downto 0);
        y0 : unsigned(15 downto 0);
        y1 : unsigned(15 downto 0);
    end record;

    type kernel_array_t is array(0 to C_KERNEL_SIZE-1) of std_logic_vector(31 downto 0);
    signal kernel : kernel_array_t := (others => (others => '0'));

    signal sim_done : std_logic := '0';

    constant C_MMAP_CYCLES : positive := 1;

    -- Simple patterns for tests (similar spirit to main.cpp)
    constant P_ZEROS        : integer := 0;
    constant P_ONES         : integer := 1;
    constant P_RAND_NO_CLIP : integer := 2;
    constant P_RAND         : integer := 3;

    subtype mmap_unsigned_t is unsigned(C_MMAP_DATA_WIDTH-1 downto 0);

    -- -- function to check if the outputs is correct
    -- function checkOutput (
    --     i : integer)
    --     return integer is

    -- begin
    --     return i+1;
    -- end checkOutput;

    -- Compute expected convolution output at sample i

    -- we read padded_signal and kernel to compute output (Still needs to be verified)
    function checkOutput(
        idx : integer;
        padded_signal : signal_array_t;
        kernel        : kernel_array_t
    ) return mmap_unsigned_t is
        variable acc        : integer := 0;
        variable sig_val_i  : integer;
        variable ker_val_i  : integer;
        variable result     : mmap_unsigned_t := (others => '0');
    begin
        -- Convolution at output index idx:
        -- y[idx] = sum_k padded_signal[idx + k] * reversed_kernel[k]
        for k in 0 to C_KERNEL_SIZE-1 loop
            -- 16-bit input
            sig_val_i := to_integer(unsigned(padded_signal(idx + k)));

            -- reverse the kernel here: C_KERNEL_SIZE-1 - k
            ker_val_i := to_integer(unsigned(kernel(C_KERNEL_SIZE-1 - k)(15 downto 0)));

            acc := acc + sig_val_i * ker_val_i;
        end loop;

        if acc < 0 then
            acc := 0;
        elsif acc > 65535 then  -- 0xFFFF
            acc := 65535;
        end if;

        -- Put the 16-bit result in the low half of the MMAP word, zero-extend the rest
        result := (others => '0');
        result(15 downto 0) := to_unsigned(acc, 16);

        return result;
    end function;

    subtype u16_int is integer range 0 to 65535;
    subtype u32_int is integer range 0 to 2147483647; -- safe positive range

    -- Compute a single convolution sample using the same behavior as
    -- convolveSW() in main.cpp (with 16-bit saturation).
    function conv_sample(idx        : integer;
                         inputSize  : integer;
                         kernelSize : integer;
                         input      : int_array_t;
                         kernel     : int_array_t) return integer is

        variable temp    : u32_int;
        variable product : u32_int;
        variable sum     : u32_int := 0;
    begin
        for j in 0 to kernelSize-1 loop
            if (idx >= j) and (idx-j < inputSize) then
                temp := input(idx-j);
            else
                temp := 0;
            end if;

            product := kernel(j) * temp;

            -- saturate product to 16 bits
            if product > 65535 then
                sum := 65535;
                exit;
            end if;

            sum := sum + product;

            -- saturate running sum to 16 bits
            if sum > 65535 then
                sum := 65535;
                exit;
            end if;
        end loop;

        return sum;
    end function conv_sample;

    -- Pack y[2*i] and y[2*i+1] into a 32-bit MMAP word, matching
    -- the hardware's memory-map format for the output.
    function expected_word(word_idx   : integer;
                           inputSize  : integer;
                           kernelSize : integer;
                           input      : int_array_t;
                           kernel     : int_array_t) return expected_word_t is

        constant SAMPLE_WIDTH : integer := 16;
        constant OUTPUT_SIZE  : integer := inputSize + kernelSize - 1;

        variable idx0, idx1 : integer;
        variable y0_i, y1_i     : integer := 0;
        variable res          : expected_word_t;
    begin
        idx0 := 2*word_idx;
        idx1 := 2*word_idx + 1;

        if (idx0 >= 0) and (idx0 < OUTPUT_SIZE) then
            y0_i := conv_sample(idx0, inputSize, kernelSize, input, kernel);
        end if;

        if (idx1 >= 0) and (idx1 < OUTPUT_SIZE) then
            y1_i := conv_sample(idx1, inputSize, kernelSize, input, kernel);
        end if;

        -- Convert to unsigned 16-bit
        res.y0 := to_unsigned(y0_i, SAMPLE_WIDTH);
        res.y1 := to_unsigned(y1_i, SAMPLE_WIDTH);

        -- Pack into 32-bit word: low=y0, high=y1
        res.w := (others => '0');
        res.w(SAMPLE_WIDTH-1 downto 0)               := res.y0;
        res.w(2*SAMPLE_WIDTH-1 downto SAMPLE_WIDTH)  := res.y1;

        return res;
    end function expected_word;


    procedure clearMMAP (signal mmap_rd_en : out std_logic;
                         signal mmap_wr_en : out std_logic) is
    begin
        mmap_rd_en <= '0';
        mmap_wr_en <= '0';
    end clearMMAP;


    -- Read from the memory map and wait until the data is marked as valid.
    -- The result is available in mmap_rd_data upon completion.
    procedure mmap_read(constant addr       : in  std_logic_vector(MMAP_ADDR_RANGE);
                        constant MAX_CYCLES : in  natural;
                        signal mmap_rd_addr : out std_logic_vector(MMAP_ADDR_RANGE);
                        signal mmap_rd_en   : out std_logic
                        ) is

        variable count : integer := 0;

    begin

        mmap_rd_addr <= addr;
        mmap_rd_en   <= '1';
        wait until (rising_edge(clks(0)));
        mmap_rd_en   <= '0';

        while (mmap_rd_data_valid = '0' and count < MAX_CYCLES) loop
            count := count + 1;
            wait until (rising_edge(clks(0)));
        end loop;

        if (count = MAX_CYCLES) then
            report "ERROR: MMAP read timeout.";
        end if;

    end procedure;


    -- Run convolve
    -- size is the unpadded signal size in 16-bit elements.
    procedure run_test(constant test_name :    string;
                       constant size      : in natural;
                       constant kernelSize: in natural;
                       constant pattern   : in integer;

                       signal mmap_wr_addr : out std_logic_vector(MMAP_ADDR_RANGE);
                       signal mmap_wr_data : out std_logic_vector(MMAP_DATA_RANGE);
                       signal mmap_wr_en   : out std_logic;
                       signal mmap_rd_addr : out std_logic_vector(MMAP_ADDR_RANGE);
                       signal mmap_rd_en   : out std_logic;

                       signal padded_signal : inout signal_array_t;
                       signal kernel : inout kernel_array_t
                       ) is

        variable count  : integer   := 0;
        variable errors : integer   := 0;
        variable done   : std_logic := '0';

        constant MAX_DONE_CYCLES : integer := size * 20;

        constant PADDED_SIGNAL_SIZE : integer := size + 2*(C_KERNEL_SIZE-1);
        constant DRAM_WORDS         : integer := PADDED_SIGNAL_SIZE / 2 + PADDED_SIGNAL_SIZE mod 2;

        constant NUMBER_OF_WINDOWS : integer := size + C_KERNEL_SIZE - 1;

        -- pseudo-random generation (for P_RAND_NO_CLIP and P_RAND)
        variable rng : real;
        variable seed1 : positive := 1357;
        variable seed2 : positive := 2468;

        -- number of 32-bit words that can store NUMBER_OF_WINDOWS 16-bit outputs
        constant OUT_WORDS : integer := (NUMBER_OF_WINDOWS + 1) / 2;  -- ceil(NUMBER_OF_WINDOWS / 2)

        -- Unpadded signal and active kernel values for hardware and software inputs
        variable inputVals  : int_array_t(0 to size-1);
        variable kernelVals : int_array_t(0 to kernelSize-1);

        variable out_idx    : integer := 0;                    -- which output sample
        variable first_ans  : unsigned(15 downto 0);
        variable second_ans : unsigned(15 downto 0);
        -- variable expected   : mmap_unsigned_t;
        variable expected : expected_word_t;

    begin

        -------------------------------------------------------------------
        -- 1) Create unpadded signal and kernel according to 'pattern'
        -------------------------------------------------------------------
        if (pattern = P_ZEROS) then

            for i in 0 to size-1 loop
                inputVals(i) := 0;
            end loop;

            for i in 0 to kernelSize-1 loop
                kernelVals(i) := 0;
            end loop;

        elsif (pattern = P_ONES) then

            for i in 0 to size-1 loop
                inputVals(i) := 1;
            end loop;

            for i in 0 to kernelSize-1 loop
                kernelVals(i) := i+1;
            end loop;

        elsif (pattern = P_RAND_NO_CLIP) then

            -- small random values (0..15) so that clipping should not occur
            for i in 0 to size-1 loop
                uniform(seed1, seed2, rng);
                inputVals(i) := integer(rng*16.0); -- 0..15
            end loop;

            for i in 0 to kernelSize-1 loop
                uniform(seed1, seed2, rng);
                kernelVals(i) := integer(rng*16.0); -- 0..15
            end loop;

        elsif (pattern = P_RAND) then

            -- Full-range random values (0..32767) for 16-bit domain
            -- This WILL likely trigger clipping/saturation in SW/HW,
            -- which is fine if you're testing that behavior.
            for i in 0 to size-1 loop
                uniform(seed1, seed2, rng);
                inputVals(i) := integer(rng*5536.0); -- 0...65535
            end loop;

            for i in 0 to kernelSize-1 loop
                uniform(seed1, seed2, rng);
                kernelVals(i) := integer(rng*5536.0); -- 0...65535
            end loop;

        else
            -- fallback pattern: simple non-trivial deterministic values
            for i in 0 to size-1 loop
                inputVals(i) := (i mod 4);
            end loop;

            for i in 0 to kernelSize-1 loop
                kernelVals(i) := (i mod 5);
            end loop;
        end if;

        -------------------------------------------------------------------
        -- 2) Build the padded signal (matches Signal constructor in C++)
        -------------------------------------------------------------------
        for i in 0 to C_KERNEL_SIZE-2 loop
            padded_signal(i) <= (others => '0');
        end loop;

        for i in 0 to size-1 loop
            padded_signal(i + C_KERNEL_SIZE - 1) <=
                std_logic_vector(to_unsigned(inputVals(i), 16));
        end loop;

        for i in 0 to C_KERNEL_SIZE-2 loop
            padded_signal(i + size + C_KERNEL_SIZE - 1) <= (others => '0');
        end loop;

        -------------------------------------------------------------------
        -- 3) Kernel: first 'kernelSize' taps active, rest forced to 0,
        --    but HW always expects 128 transfers.
        -------------------------------------------------------------------
        for i in 0 to kernelSize-1 loop
            kernel(i) <= std_logic_vector(to_unsigned(kernelVals(i), 32));
        end loop;

        if (kernelSize < C_KERNEL_SIZE) then
            for i in kernelSize to C_KERNEL_SIZE-1 loop
                kernel(i) <= (others => '0');
            end loop;
        end if;

        -- Disable user mode to enable transfers to DRAM.
        mmap_wr_addr <= (others => '1');
        mmap_wr_en   <= '1';
        mmap_wr_data <= std_logic_vector(to_unsigned(0, C_MMAP_DATA_WIDTH));
        wait until rising_edge(clks(0));
        clearMMAP(mmap_rd_en, mmap_wr_en);

        -------------------------------------------------------------------
        -- 5) Send padded signal to input RAM at address 0.
        --    (write(signal.getSignal(), 0, signal.getSize()))
        --    Two 16-bit samples per 32-bit word, matching App::write().
        -------------------------------------------------------------------
        for i in 0 to DRAM_WORDS-1 loop
            mmap_wr_addr <= std_logic_vector(to_unsigned(i, C_MMAP_ADDR_WIDTH));
            mmap_wr_en   <= '1';

            -- Pack two 16-bit inputs.
            --mmap_wr_data <= std_logic_vector(to_unsigned(padded_signal(2*i+1), C_MMAP_DATA_WIDTH/2) & to_unsigned(padded_signal(2*i), C_MMAP_DATA_WIDTH/2));
            mmap_wr_data <= padded_signal(2*i+1) & padded_signal(2*i);
            wait until rising_edge(clks(0));
            clearMMAP(mmap_rd_en, mmap_wr_en);
        end loop;

        for i in 0 to 3 loop
            wait until rising_edge(clks(0));
        end loop;

        -------------------------------------------------------------------
        -- 6) Enable user mode.
        --    (write(1, MODE_ADDR))
        -------------------------------------------------------------------
        mmap_wr_addr <= (others => '1');
        mmap_wr_en   <= '1';
        mmap_wr_data <= std_logic_vector(to_unsigned(1, C_MMAP_DATA_WIDTH));
        wait until rising_edge(clks(0));
        clearMMAP(mmap_rd_en, mmap_wr_en);

        -------------------------------------------------------------------
        -- 7) Clear internal buffers.
        --    (write(1, CLEAR_ADDR))
        -------------------------------------------------------------------
        -- mmap_wr_addr <= (others => '1');
        -- mmap_wr_en   <= '1';
        -- mmap_wr_data <= std_logic_vector(to_unsigned(1, C_MMAP_DATA_WIDTH));
        -- wait until rising_edge(clks(0));
        -- clearMMAP(mmap_rd_en, mmap_wr_en);

        -------------------------------------------------------------------
        -- 8) Send unpadded signal size.
        --    (write(signal.getUnpaddedSize(), SIGNAL_SIZE_ADDR))
        -------------------------------------------------------------------
        -- mmap_wr_addr <= C_SIGNAL_SIZE_ADDR;
        -- mmap_wr_en   <= '1';
        -- mmap_wr_data <= std_logic_vector(to_unsigned(size, C_MMAP_DATA_WIDTH));
        -- wait until rising_edge(clks(0));
        -- clearMMAP(mmap_rd_en, mmap_wr_en);

        ------------------------------------------------------------------
        -- 9) Send kernel taps.
        --    (for i=0..127: write(kernel[i], KERNEL_DATA_ADDR))
        -------------------------------------------------------------------
        for i in 0 to C_KERNEL_SIZE-1 loop
            mmap_wr_addr <= C_KERNEL_DATA_ADDR;
            mmap_wr_en   <= '1';
            mmap_wr_data <= kernel(i);
            wait until rising_edge(clks(0));
            clearMMAP(mmap_rd_en, mmap_wr_en);
        end loop;

--        -- Specify the starting write address (added)
--        mmap_wr_addr <= C_RAM1_WR_ADDR_ADDR;
--        mmap_wr_en   <= '1';
--        mmap_wr_data <= std_logic_vector(to_unsigned(0, C_MMAP_DATA_WIDTH));
--        wait until rising_edge(clks(0));
--        clearMMAP(mmap_rd_en, mmap_wr_en);

--        -- Specify the starting read address  (added)
--        mmap_wr_addr <= C_RAM0_RD_ADDR_ADDR;
--        mmap_wr_en   <= '1';
--        mmap_wr_data <= std_logic_vector(to_unsigned(0, C_MMAP_DATA_WIDTH));
--        wait until rising_edge(clks(0));
--        clearMMAP(mmap_rd_en, mmap_wr_en);

        -------------------------------------------------------------------
        -- 8) Send unpadded signal size.
        --    (write(signal.getUnpaddedSize(), SIGNAL_SIZE_ADDR))
        -------------------------------------------------------------------
        mmap_wr_addr <= C_SIGNAL_SIZE_ADDR;
        mmap_wr_en   <= '1';
        mmap_wr_data <= std_logic_vector(to_unsigned(size, C_MMAP_DATA_WIDTH));
        wait until rising_edge(clks(0));
        clearMMAP(mmap_rd_en, mmap_wr_en);

        -------------------------------------------------------------------
        -- 10) Start user_app.
        --     (write(1, GO_ADDR))
        -------------------------------------------------------------------
        mmap_wr_addr <= C_GO_ADDR;
        mmap_wr_en   <= '1';
        mmap_wr_data <= std_logic_vector(to_unsigned(1, C_MMAP_DATA_WIDTH));
        wait until rising_edge(clks(0));
        clearMMAP(mmap_rd_en, mmap_wr_en);

        -- Since the user clock domain is slower, we need to give some time for
        -- done to be cleared from previous runs.
        for i in 0 to 9 loop
            wait until (rising_edge(clks(0)));
        end loop;

        done  := '0';
        count := 0;

        -------------------------------------------------------------------
        -- 11) WaitUntilDone(timeout):
        --       write(1, MODE_ADDR);
        --       while (!value && t<timeout) read(DONE_ADDR);
        -------------------------------------------------------------------
        while done = '0' and count < MAX_DONE_CYCLES loop

            mmap_read(C_DONE_ADDR, MAX_MMAP_READ_CYCLES, mmap_rd_addr, mmap_rd_en);
            done  := mmap_rd_data(0);
            count := count + 1;
        end loop;

        if (done /= '1') then
            errors := errors + 1;
            report "ERROR: Done signal not asserted before timeout.";
        end if;

        -------------------------------------------------------------------
        -- 12) getOutput():
        --       write(0, MODE_ADDR);
        --       read(output, 0, outputSize);
        --     Disable user mode to access DRAM via wrapper.
        -------------------------------------------------------------------
        mmap_wr_addr <= (others => '1');
        mmap_wr_en   <= '1';
        mmap_wr_data <= std_logic_vector(to_unsigned(0, C_MMAP_DATA_WIDTH));
        wait until rising_edge(clks(0));
        clearMMAP(mmap_rd_en, mmap_wr_en);

        -------------------------------------------------------------------
        -- 13) Read outputs from output memory and compare against SW.
        --     DRAM stores {y[2*i+1], y[2*i]} per 32-bit word.
        -------------------------------------------------------------------
        --------------------------------------------------------------------
        -- Read packed 32-bit outputs from DRAM.
        -- Each word holds two 16-bit outputs:
        --   mmap_rd_data(31 downto 16) -> "second_ans"
        --   mmap_rd_data(15 downto 0)  -> "first_ans"
        --
        -- For the FIRST word, the lower 16 bits are invalid, so we:
        --   - use only second_ans for output index 0
        --   - then from the second word onward, we use both halves.
        --------------------------------------------------------------------
        out_idx := 0;

        for word_idx in 0 to DRAM_WORDS-1 loop

            mmap_read(std_logic_vector(to_unsigned(word_idx, C_MMAP_ADDR_WIDTH)),
                      MAX_MMAP_READ_CYCLES,
                      mmap_rd_addr, mmap_rd_en);

            -- upper 16 bits always correspond to a valid candidate
            second_ans := unsigned(mmap_rd_data(31 downto 16));

            -- lower 16 bits: invalid for first word, valid after that
            first_ans := unsigned(mmap_rd_data(15 downto 0));

            expected := expected_word(word_idx, size, kernelSize, inputVals, kernelVals);

            -- report "first_ans:" & integer'image(to_integer(first_ans)) & " word_idx: " & integer'image(word_idx);
            -- report "second_ans:" & integer'image(to_integer(second_ans)) & " word_idx: " & integer'image(word_idx);

            -- report "expected.y0:" & integer'image(to_integer(expected.y0)) & " word_idx: " & integer'image(word_idx);
            -- report "expected.y1:" & integer'image(to_integer(expected.y1)) & " word_idx: " & integer'image(word_idx);
            
            -- if (unsigned(mmap_rd_data) /= expected.w) then
            --     errors := errors + 1;
            --     report "Result w for word " & integer'image(word_idx) &
            --            " is incorrect. The output is " &
            --            integer'image(to_integer(unsigned(mmap_rd_data))) &
            --            " but should be " &
            --            integer'image(to_integer(expected.w));
            -- end if;

            if (first_ans /= expected.y0) then
                errors := errors + 1;
                report "Result y0 for word " & integer'image(word_idx) &
                       " is incorrect. The output is " &
                       integer'image(to_integer(first_ans)) &
                       " but should be " &
                       integer'image(to_integer(expected.y0));
            end if;

            if (second_ans /= expected.y1) then
                errors := errors + 1;
                report "Result y1 for word " & integer'image(word_idx) &
                       " is incorrect. The output is " &
                       integer'image(to_integer(second_ans)) &
                       " but should be " &
                       integer'image(to_integer(expected.y1));
            end if;
        end loop;

        report test_name & " completed. Total Errors: " & integer'image(errors);
    end procedure;

begin

    UUT : entity work.wrapper
        port map (
            clks               => clks,
            rst                => rst,
            mmap_wr_en         => mmap_wr_en,
            mmap_wr_addr       => mmap_wr_addr,
            mmap_wr_data       => mmap_wr_data,
            mmap_rd_en         => mmap_rd_en,
            mmap_rd_addr       => mmap_rd_addr,
            mmap_rd_data       => mmap_rd_data,
            mmap_rd_data_valid => mmap_rd_data_valid);

    -- Toggle clocks
    clks(0) <= not clks(0) after 5 ns when sim_done = '0' else clks(0);
    clks(1) <= not clks(1) after 7 ns when sim_done = '0' else clks(1);

    -- Main testbench code.
    process
        -- Software "small" settings from main.cpp
        variable small_signal_v : natural := 5;
        variable small_kernel_v : natural := C_KERNEL_SIZE;
    begin
        -- Reset circuit  
        rst <= '1';
        clearMMAP(mmap_rd_en, mmap_wr_en);
        for i in 0 to 20 loop
            wait until rising_edge(clks(0));
        end loop;

        -- Clear reset
        rst <= '0';
        for i in 0 to 20 loop
            wait until rising_edge(clks(0));
        end loop;

        -- Software "small" settings from main.cpp
        
        small_kernel_v := C_KERNEL_SIZE;

        -- small_signal_v := 10;
        small_signal_v := 1000;
        -- small_signal_v := 2**(C_RAM1_ADDR_WIDTH+1);

        ----------------------------------------------------------
        ----------------------------------------------------------

        -- run_test("Zeros_medium",
        --         small_signal_v,
        --         small_kernel_v,
        --         P_ZEROS,
        --         mmap_wr_addr, mmap_wr_data, mmap_wr_en,
        --         mmap_rd_addr, mmap_rd_en,
        --         padded_signal, kernel);

        -- rst <= '1';
        -- for i in 0 to 10 loop
        --     wait until rising_edge(clks(0));
        -- end loop;
        -- rst <= '0';
        -- for i in 0 to 10 loop
        --     wait until rising_edge(clks(0));
        -- end loop;

        -- small_signal_v := 1000;

        -- run_test("Ones_medium_Test", 
        --         small_signal_v, 
        --         small_kernel_v, 
        --         P_ONES, 
        --         mmap_wr_addr, mmap_wr_data, mmap_wr_en, 
        --         mmap_rd_addr, mmap_rd_en, 
        --         padded_signal, kernel);

        rst <= '1';
        for i in 0 to 10 loop
            wait until rising_edge(clks(0));
        end loop;
        rst <= '0';
        for i in 0 to 10 loop
            wait until rising_edge(clks(0));
        end loop;

        small_signal_v := 1000;

        run_test("RandNoClip_medium", 
                small_signal_v, 
                small_kernel_v, 
                P_RAND_NO_CLIP, 
                mmap_wr_addr, mmap_wr_data, mmap_wr_en, 
                mmap_rd_addr, mmap_rd_en, 
                padded_signal, kernel);

        rst <= '1';
        for i in 0 to 10 loop
            wait until rising_edge(clks(0));
        end loop;
        rst <= '0';
        for i in 0 to 10 loop
            wait until rising_edge(clks(0));
        end loop;

        small_signal_v := 1000;

        run_test("RandWithClip_medium", 
                small_signal_v, 
                small_kernel_v, 
                P_RAND, 
                mmap_wr_addr, mmap_wr_data, mmap_wr_en, 
                mmap_rd_addr, mmap_rd_en, 
                padded_signal, kernel);
        
        report "SIMULATION FINISHED.";
        sim_done <= '1';
        wait;

    end process;
end behavior;
