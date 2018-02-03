#!/usr/bin/python3
# encoding: utf-8
# module triacode
# by Daway.Cai,guangxian
# email: caixnet@outlook.com
# triacode is a ternary fractional logic
# ©2013 Daway.Cai,guangxian; there are no restrictions on linking to this web page.
# http://rosettacode.org/wiki/Ternary_logic
"""
toism - the way
the tao produced one; one produced two; two produced three;
three produced all things. all things leave behind them the obscurity.
                                                 -laozi:42

tao  -  one  - two   - three  - all
ro   -  ri   - rori  - toi   - tori

tri-a-code a ternary code
triacode have include two string 0 and 1 code string
first code string is negative number string
second code string is positive number string
combine first code and second have a triacode

                  IOITR
               /--00011--- -3^x
triacode-map =|   |||||
               \++10101+++ +3^x

            -3^x  /0 /-3/0 /-3
triacode-string= 01'10'00'11
            +3^x  \+3\0 \0 \+3
                 I  T  O  R

triacode=(-ternary_array, + ternary_array)

    3^3 3^2 3^1 3^0
 -  0   1   0   1   --> negative string array
 +  0   0   1   1   --> positive string array
 ===================
 =  0   T   1   0

 triacode string (-, +)
 00 =  0   = O = (0, 0)
 01 =  1   = I = (0, 1)
 10 = -1   = T = (1, 0)
 11 =  0   = R = (1, 1)

"""
##############################################

#triacode ternary logic
from triacode.triacode import TRIACODE
from triacode.triacode import rid
from triacode.triacode import invert
from triacode.triacode import sand
from triacode.triacode import isand
from triacode.triacode import sor
from triacode.triacode import isor
from triacode.triacode import band
from triacode.triacode import iband
from triacode.triacode import bor
from triacode.triacode import ibor
from triacode.triacode import xor
from triacode.triacode import xand
from triacode.triacode import sum
from triacode.triacode import isum
from triacode.triacode import carry
from triacode.triacode import icarry
#
from triacode.adder import halfadder2
from triacode.adder import fulladder2
#
from triacode.triacode import test
from triacode.triacode import itest
from triacode.triacode import test2in2out
from triacode.triacode import test3in2out


