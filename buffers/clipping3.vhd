-- Mehmet Akarli and Xie Xianghui
-- Universify of Florida

library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.user_pkg.all;
use work.config_pkg.all;


entity clipping3 is
    generic(
        in_width : positive;
        out_width : positive

    );
    port(
        input_unclipped  : in std_logic_vector(in_width - 1 downto 0);
        output_clipped   : out std_logic_vector(out_width - 1 downto 0)
    );
end entity clipping3;

architecture clipping of clipping3 is
begin
    process(input_unclipped)
        variable temp_in : unsigned(in_width-1 downto 0);
    begin
        temp_in := unsigned(input_unclipped);

        if in_width > out_width and
           temp_in(in_width-1 downto out_width) /= 0 then
            output_clipped <= (others => '1');
        else
            output_clipped <= std_logic_vector(temp_in(out_width-1 downto 0));
        end if;
    end process;
end architecture;
