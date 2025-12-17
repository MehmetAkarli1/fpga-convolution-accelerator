-- University Of Florida
-- Mehmet Akarli and Xie Xianghui

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

use work.user_pkg.all;
use work.config_pkg.all;
use work.math_custom.all;

entity user_app is
    port (
         clk                      : in  std_logic;
         rst                      : in  std_logic;

         -- Memory-map Interface
         mmap_wr_en               : in  std_logic;
         mmap_wr_addr             : in  std_logic_vector(MMAP_ADDR_RANGE);
         mmap_wr_data             : in  std_logic_vector(MMAP_DATA_RANGE);
         mmap_rd_en               : in  std_logic;
         mmap_rd_addr             : in  std_logic_vector(MMAP_ADDR_RANGE);
         mmap_rd_data             : out std_logic_vector(MMAP_DATA_RANGE);

         -- DMA read interface for RAM 0
         ram0_rd_rd_en            : out  std_logic;                                           
         ram0_rd_go               : out  std_logic;                                           
         ram0_rd_valid            : in   std_logic;                                           -- gets asserted when the address generator of DRAM_RD_RAM0 generates ‘size’ amount memory address
         ram0_rd_data             : in   std_logic_vector(RAM0_RD_DATA_RANGE);                -- Connected to sig_wr_data  
         ram0_rd_addr             : out  std_logic_vector(RAM0_ADDR_RANGE);      -- Address that dram_rd uses internally to read from RAM0
         ram0_rd_size             : out  std_logic_vector(RAM0_RD_SIZE_RANGE);   -- Total number of samples that dram_rd will read
         ram0_rd_done             : in   std_logic;                                           -- gets asserted when DRAM_RD_RAM0 finishes generating ‘size’ amount of data from DRAM

         debug_ram0_rd_count      : in std_logic_vector(RAM0_RD_SIZE_RANGE);
         debug_ram0_rd_size       : in std_logic_vector(C_RAM0_ADDR_WIDTH downto 0);          -- 15 bit
         debug_ram0_rd_addr       : in std_logic_vector(RAM0_ADDR_RANGE);
         debug_ram0_rd_start_addr : in std_logic_vector(RAM0_ADDR_RANGE);
         debug_ram0_rd_prog_full  : in std_logic;
         debug_ram0_rd_empty      : in std_logic;
         
         -- Input signals from output memory dram_wr (from wrapper) 
         ram1_wr_ready            : in std_logic;                                             -- Ready signal from output memory
         ram1_wr_done             : in std_logic;                                             -- done signal from output memory
         
         -- Output signals from user_app to wrapper
         ram1_wr_go               : out std_logic;                                            -- Connected to go_s from memory_map in user_app architecture
         ram1_wr_valid            : out std_logic;                                            -- Connected to valid_out from delay    
         ram1_wr_data             : out std_logic_vector(RAM1_WR_DATA_RANGE);          -- Pipeline output to output memory
         ram1_wr_addr             : out std_logic_vector(RAM1_ADDR_RANGE);       
         ram1_wr_size             : out std_logic_vector(RAM1_WR_SIZE_RANGE)                  -- number of output samples that dram_wr must write back to software

         );
end user_app;

architecture default of user_app is

    --signal comparison_s   : std_logic_vector()

    -- Internal Signals [MEMORY_MAP]
    signal go_s           : std_logic;
    signal clear_s        : std_logic;
    signal sw_rst_s       : std_logic;
    signal signal_size_s  : std_logic_vector(RAM0_RD_SIZE_RANGE);
    signal kernel_data_s  : std_logic_vector(KERNEL_WIDTH_RANGE);
    signal kernel_load_s  : std_logic;
    signal kernel_ready_s : std_logic;
    signal done_s         : std_logic;

    -- Internal Signals [Signal and Kernel Buffer]
    signal sig_wr_en             : std_logic;
    signal sig_wr_data           : std_logic_vector(SIGNAL_WIDTH_RANGE); -- SIGNAL_WIDTH_RANGE: 16 bit
    signal sig_rd_en             : std_logic_vector(0 downto 0);
    signal sig_full              : std_logic;
    signal sig_empty             : std_logic;
    signal sig_rd_data           : std_logic_vector(C_KERNEL_SIZE*C_KERNEL_WIDTH-1 downto 0);
    
    signal kern_wr_en            : std_logic;
    signal kern_wr_data          : std_logic_vector(C_KERNEL_WIDTH-1 downto 0);
    signal kern_rd_en            : std_logic := '0';
    signal kern_full             : std_logic;
    signal kern_empty            : std_logic;

    signal kern_rd_data          : window(0 to C_KERNEL_SIZE - 1);
    --signal kern_rd_data          : std_logic_vector(C_KERNEL_SIZE*C_KERNEL_WIDTH - 1 downto 0);
    signal in_kernel_rd_data     : std_logic_vector(C_KERNEL_SIZE*C_KERNEL_WIDTH - 1 downto 0);
    signal in_kernel_rd_data_rev : std_logic_vector(C_KERNEL_SIZE*C_KERNEL_WIDTH - 1 downto 0);

    -- Padded and Output size to ram0 and ram1
    signal padded_size_s       : std_logic_vector(RAM0_RD_SIZE_RANGE);
    signal output_size_s       : std_logic_vector(RAM1_WR_SIZE_RANGE);
    
     -- Convolution Signals
    constant C_CONV_LATENCY       : natural := clog2(C_KERNEL_SIZE) + 1;
    constant CLIPPING_IN         : positive := C_RAM0_RD_DATA_WIDTH + C_RAM1_WR_DATA_WIDTH + clog2(C_KERNEL_SIZE);

    -- Internal Signals [Delay] 
    signal valid_out      : std_logic_vector(0 downto 0);

    -- Internal Signal [Pipeline]
    signal pipeline_out   : std_logic_vector(CLIPPING_IN -1 downto 0);
    -- Internal Signal [Clipping]
    signal conv_result    : std_logic_vector(RAM1_WR_DATA_RANGE);
    -- Resetting between test cases to ensure buffers are empty
    signal rst_comb : std_logic;

begin

    -- Calculating padding size to dram_rd --
    padded_size_s   <= std_logic_vector(unsigned(signal_size_s) + to_unsigned(2*(C_KERNEL_SIZE-1), C_RAM0_ADDR_WIDTH));
    ram0_rd_size    <= padded_size_s;

    -- Caltulating padded size to dram_wr --
    --output_size_s   <= std_logic_vector(unsigned(signal_size_s) + to_unsigned(C_KERNEL_SIZE-1, signal_size_s'length));
    output_size_s   <= std_logic_vector(unsigned(signal_size_s) + to_unsigned(C_KERNEL_SIZE-1, C_RAM0_ADDR_WIDTH));
    ram1_wr_size    <= output_size_s; 

    -- Control logic [Signal Buffer] --
    sig_wr_en       <= ram0_rd_valid; --and (not sig_full);          -- independent of empty [Professor advice]                            
    sig_wr_data     <= ram0_rd_data;
    sig_rd_en(0)    <= (not sig_empty) and ram1_wr_ready;            -- independent of full  [Professor advice]

    -- Control logic [Kernel Buffer] --
    kern_wr_en      <= kernel_load_s AND (NOT kern_full);
    kern_wr_data    <= kernel_data_s;
    kern_rd_en      <= '0';                                          -- Hardcoded to '0' as Kernel Buffer do not have sliding window function
    kernel_ready_s  <= kern_full;                                    -- Telling memory_map kernel buffer is full

    -- ram0 input signals
    ram0_rd_go      <= go_s;
    ram0_rd_rd_en   <= (not sig_full) and ram1_wr_ready;
    ram0_rd_addr    <= (others => '0');                              -- Setting start address of dram0

 
    -- ram1 output signals to wrapper
    done_s          <= ram1_wr_done;
    ram1_wr_valid   <= valid_out(0) and ram1_wr_ready;              -- Valid_out from delay 
    ram1_wr_data    <= conv_result;                                 -- Clipped pipeline result
    ram1_wr_go      <= go_s;                                        -- connect go signal from ram1 to go signal from memory_map 
    ram1_wr_addr    <= (others => '0');                             -- Starting address for dram_wr in wrapper(I assume dram_wr handles the address incrementing internally)

    -- Reset function for several components to ensure resetting before new test case begin
    rst_comb <= clear_s or rst;
  

    -- Instance of memory map
    U_MEM_MAP : entity work.memory_map
      port map (
        clk           => clk,   
        rst           => rst,
        wr_en         => mmap_wr_en,   
        wr_addr       => mmap_wr_addr, 
        wr_data       => mmap_wr_data, 
        rd_en         => mmap_rd_en, 
        rd_addr       => mmap_rd_addr, 
        rd_data       => mmap_rd_data, 

        -- Circuit interface from software        
        go            => go_s,
        clear         => clear_s,  
        sw_rst        => sw_rst_s,
        signal_size   => signal_size_s,
        kernel_data   => kernel_data_s,
        kernel_load   => kernel_load_s,
        kernel_ready  => kernel_ready_s,
        done          => done_s
               );

   --Instance of Signal Buffer (WORKING)
   -- U_SIGNAL_BUFFER : entity work.signal_buffer2
   --   port map (
   --     clk         => clk,      
   --     rst         => rst_comb,
   --     wr_en       => sig_wr_en,                                         -- dram_rd_valid AND not full, -- valid signal from dram
   --     wr_data     => sig_wr_data,                                       -- dram_rd_data, data that need to be read from dram
   --     rd_en       => sig_rd_en(0),                                      -- (NOT sig_empty) and dram_wr_ready, (buffer not empty) AND (output memory ready) 
   --     full        => sig_full,
   --     empty       => sig_empty,
   --     rd_data     => sig_rd_data
   --            );
   
    -- NOT WORKING            
    --U_KERNEL_BUFFER : entity work.buffer_kernel
    --  port map (
    --    clk         => clk,      
    --    rst         => rst_comb,
    --    wr_en       => kernel_load_s,                                         -- dram_rd_valid AND not full, -- valid signal from dram
    --    wr_data     => kernel_data_s,                                       -- dram_rd_data, data that need to be read from dram
    --    full        => kern_full,
    --   rd_data     => kern_rd_data
    --            );
    
    --Instance of Signal Buffer (WORKING)
    --U_SIGNAL_BUFFER : entity work.signal_buffer3
    --  port map (
    --    clk         => clk,      
    --    rst         => rst_comb,
    --    wr_en       => sig_wr_en,                                         -- dram_rd_valid AND not full, -- valid signal from dram
    --    wr_data     => sig_wr_data,                                       -- dram_rd_data, data that need to be read from dram
    --    rd_en       => sig_rd_en(0),                                      -- (NOT sig_empty) and dram_wr_ready, (buffer not empty) AND (output memory ready) 
    --    full        => sig_full,
    --    empty       => sig_empty,
    --    rd_data     => sig_rd_data
    --            );
    
    -- Sliding Signal Buffer WORKING - signal_buffer4.vhd
    U_SIGNAL_BUFFER : entity work.signal_buffer4
      port map (
        clk         => clk,      
        rst         => rst_comb,
        wr_en       => sig_wr_en,                                         -- dram_rd_valid AND not full, -- valid signal from dram
        wr_data     => sig_wr_data,                                       -- dram_rd_data, data that need to be read from dram
        rd_en       => sig_rd_en(0),                                      -- (NOT sig_empty) and dram_wr_ready, (buffer not empty) AND (output memory ready) 
        full        => sig_full,
        empty       => sig_empty,
        rd_data     => sig_rd_data
                );            
    
    -- KERNEL BUFFER (WORKING) - kernel_buffer3.vhd
    U_KERNEL_BUFFER : entity work.buffer_k3
      port map (
        clk         => clk,      
        rst         => rst_comb,
        wr_en       => kernel_load_s,                                         
        wr_data     => kernel_data_s,                                         
        rd_en       => '0',                                                   
        full        => kern_full,
       empty        => kern_empty,
        rd_data     => kern_rd_data
                );


   -- Kernel Buffer(WORKING)
   --  U_KERNEL_BUFFER : entity work.kernel_buffer4
   --      generic map(
   --          ELEMENT_WIDTH => C_SIGNAL_WIDTH,
   --          NUM_ELEMENTS  => C_KERNEL_SIZE
   --      )
   --      port map(
   --          clk     => clk,
   --          rst     => rst_comb,
   --          wr_data => kernel_data_s,
   --          wr_en   => kernel_load_s,
   --          rd_data => kern_rd_data,
   --          rd_en   => '0',
   --          empty   => kern_empty,
   --          full    => kern_full
   --          );
      
      -- Convert array from the kernel buffer into one big vector
      U_VECTORIZE_KERNEL : for i in 0 to C_KERNEL_SIZE - 1 generate
         in_kernel_rd_data((i + 1) * C_SIGNAL_WIDTH - 1 downto i * C_SIGNAL_WIDTH) <= kern_rd_data(i);
      end generate;

      -- Reversing Kernel Buffer output
      gen_reverse: for i in 0 to C_KERNEL_SIZE-1 generate
        in_kernel_rd_data_rev((i+1)*16-1 downto 16*i) <= in_kernel_rd_data((C_KERNEL_SIZE-i)*16-1 downto (C_KERNEL_SIZE-i-1)*16);
      end generate;   

    -- Instance of delay (WORKING)
    U_DELAY : entity work.delay
      generic map (
        CYCLES => clog2(C_KERNEL_SIZE)+1,
        WIDTH  => 1)
    port map (
        clk       => clk,
        rst       => rst_comb,
        en        => ram1_wr_ready,                                                -- sig_rd_en already accounts for enable function
        input     => sig_rd_en,
        output    => valid_out
             );    
        
    -- Pipeline output processed in mult_add_tree (WORKING)
    U_MULT_ADD_TREE : entity work.mult_add_tree--(unsigned_arch)
    generic map (
        num_inputs   => C_KERNEL_SIZE,
        input1_width => C_SIGNAL_WIDTH,
        input2_width => C_KERNEL_WIDTH
                )
    port map (
        clk    => clk,
        rst    => rst_comb,
        en     => ram1_wr_ready,                                            -- ram1_wr_ready input to enable pipeline
        input1 => sig_rd_data,                                              -- signal buffer
        input2 => in_kernel_rd_data_rev,                                    -- reversed kernel buffer output
        output => pipeline_out                                              -- final pipeline output
             ); 
    
    -- Clipping (WORKING)
    U_CLIPPING : entity work.clipping3
         generic map(
             in_width  =>  CLIPPING_IN,
             out_width => C_SIGNAL_WIDTH
         )
         port map(
             input_unclipped  => pipeline_out,
             output_clipped => conv_result 
         );
         
    -- Clipping of pipeline output (WORKING)
    -- 16-bit saturate after the full convolution
    -- Clipping Detector (WORKING)
    -- U_CLIPPING_DETECTOR : entity work.clipping_detector2
    --     generic map(
    --         INPUT_WIDTH  =>  CLIPPING_IN,
    --         OUTPUT_WIDTH => C_SIGNAL_WIDTH
    --     )
    --     port map(
    --         input  => pipeline_out,
    --         output => conv_result
    --     );
    
end default;