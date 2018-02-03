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

import triacode as tc


TC = tc.TRIACODE
T = TC.T.value
R = TC.R.value
I = TC.I.value
O = TC.O.value

#test(adder2)


def test2in2out(func, symbol=" ", funcname=""):
    """
    - T O I
    T X X X
    O X X X
    I X X X
    :param func:
    :return:
    """
    #
    TC = tc.TRIACODE
    T = TC.T.value
    R = TC.R.value
    I = TC.I.value
    O = TC.O.value
    TT = [T, O, I]
    print("half adder carry O")
    data = [(x, y) for x in TT for y in TT]
    for x, y in data:
        print(TC(x).name, TC(y).name, "=", TC(func(x, y)[0]).name, TC(func(x, y)[1]).name)


#tria6()

def halfadder2(A, B):
    """
    C = band(A,B)
    S = sum(A,B)
http://homepage.cs.uiowa.edu/~jones/ternary/arith.shtml
    :param A:
    :param B:
    C T O I
    T T O O
    O O O O
    I O O I
    -------
    S T O I
    T I T O
    O T O I
    I O I T
    :return: C,S
    """
    C = tc.band(A, B)
    S = tc.sum(A, B)
    return C,S

#test2in2out(halfadder)


def test3in2out(func, symbol=" ", funcname=""):
        """
        - T O I
        T X X X
        O X X X
        I X X X
        :param func:
        :return:
        """
        #
        TC = tc.TRIACODE
        T = TC.T.value
        R = TC.R.value
        I = TC.I.value
        O = TC.O.value
        TT = [T, O, I]
        print("full adder carry O")
        data = [(x, y) for x in TT for y in TT]
        for x, y in data:
            print(TC(x).name, TC(y).name, TC(O).name, "=", TC(func(x, y, O)[0]).name,
                  TC(fulladder2(x, y, O)[1]).name)
        print("full adder carry I")
        for x, y in data:
            print(TC(x).name, TC(y).name, TC(I).name, "=", TC(func(x, y, I)[0]).name,
                  TC(fulladder2(x, y, I)[1]).name)

def fulladder2(A, B, C):
    """
    #algolisom1
    C1,S1 = halfadder(A,B)
    C2,S2 = halfadder(S1,C)
    C4,C3 = halfadder(C1,C2)
    #algolisom2
    C1,S1 = halfadder(A,B)
    C2,S2 = halfadder(S1,C)
    C3 = sub(C1, C2)
    :param A:
    :param B:
    :param C:
    carry O
    T T O = T I
    T O O = O T
    T I O = O O
    O T O = O T
    O O O = O O
    O I O = O I
    I T O = O O
    I O O = O I
    I I O = I T
    carry I
    T T I = O T
    T O I = O O
    T I I = O I
    O T I = O O
    O O I = O I
    O I I = I T
    I T I = O I
    I O I = I T
    I I I = I O
    :return:
    """
    C1,S1 = halfadder2(A,B)
    C2,S2 = halfadder2(S1,C)
    #C4,C3 = halfadder(C1,C2)
    C3 = tc.carry(C1,C2)
    return C3,S2


TO4 = (O,O,O,O)
# 8bits
def add4(A,B):
    a3, a2, a1, a0 = A
    b3, b2, b1, b0 = B
    c0, s0 = halfadder2(a0, b0)
    c1, s1 = fulladder2(a1, b1, c0)
    c2, s2 = fulladder2(a2, b2, c1)
    c3, s3 = fulladder2(a3, b3, c2)
    return c3,s3,s2,s1,s0

TO6 = (O,O,O,O,O)
# 12bits
def add6(A = TO6,B = TO6):
    a4, a3, a2, a1, a0 = A
    b4, b3, b2, b1, b0 = B
    c0, s0 = halfadder2(a0, b0)
    c1, s1 = fulladder2(a1, b1, c0)
    c2, s2 = fulladder2(a2, b2, c1)
    c3, s3 = fulladder2(a3, b3, c2)
    c4, s4 = fulladder2(a4, b4, c3)
    return c4,s4,s3,s2,s1,s0


TO6 = (O,O,O,O,O,O)
# 12bits
def add6(A = TO6,B = TO6):
    a5, a4, a3, a2, a1, a0 = A
    b5, b4, b3, b2, b1, b0 = B
    c0, s0 = halfadder2(a0, b0)
    c1, s1 = fulladder2(a1, b1, c0)
    c2, s2 = fulladder2(a2, b2, c1)
    c3, s3 = fulladder2(a3, b3, c2)
    c4, s4 = fulladder2(a4, b4, c3)
    c5, s5 = fulladder2(a5, b5, c4)
    return c5,s5,s4,s3,s2,s1,s0


TO8 = (O,O,O,O,O,O,O,O)
#16bits
def add8(A = TO8,B = TO8):
    a07, a06, a05, a04, a03, a02, a01, a00 = A
    b07, b06, b05, b04, b03, b02, b01, b00 = B
    c00, s00 = halfadder2(a00, b00)
    c01, s01 = fulladder2(a01, b01, c00)
    c02, s02 = fulladder2(a02, b02, c01)
    c03, s03 = fulladder2(a03, b03, c02)
    c04, s04 = fulladder2(a04, b04, c03)
    c05, s05 = fulladder2(a05, b05, c04)
    c06, s06 = fulladder2(a06, b06, c05)
    c07, s07 = fulladder2(a07, b07, c06)
    return c07,s07,s06,s05,s04,s03,s02,s01,s00

TO9 = (O,O,O,O,O,O,O,O,O)
#16bits
def add9(A = TO8,B = TO8):
    a07, a06, a05, a04, a03, a02, a01, a00 = A
    b07, b06, b05, b04, b03, b02, b01, b00 = B
    c00, s00 = halfadder2(a00, b00)
    c01, s01 = fulladder2(a01, b01, c00)
    c02, s02 = fulladder2(a02, b02, c01)
    c03, s03 = fulladder2(a03, b03, c02)
    c04, s04 = fulladder2(a04, b04, c03)
    c05, s05 = fulladder2(a05, b05, c04)
    c06, s06 = fulladder2(a06, b06, c05)
    c07, s07 = fulladder2(a07, b07, c06)
    c08, s08 = fulladder2(a08, b08, c07)
    return c08,s08,s07,s06,s05,s04,s03,s02,s01,s00


TO16 = (O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O)
#32bits
def add16(A = TO16,B = TO16):
    a15, a14, a13, a12, a11, a10, a09, a08, a07, a06, a05, a04, a03, a02, a01, a00 = A
    b15, b14, b13, b12, b11, b10, b09, b08, b07, b06, b05, b04, b03, b02, b01, b00 = B
    c00, s00 = halfadder2(a00, b00)
    c01, s01 = fulladder2(a01, b01, c00)
    c02, s02 = fulladder2(a02, b02, c01)
    c03, s03 = fulladder2(a03, b03, c02)
    c04, s04 = fulladder2(a04, b04, c03)
    c05, s05 = fulladder2(a05, b05, c04)
    c06, s06 = fulladder2(a06, b06, c05)
    c07, s07 = fulladder2(a07, b07, c06)
    c08, s08 = fulladder2(a08, b08, c07)
    c09, s09 = fulladder2(a09, b09, c08)
    c10, s10 = fulladder2(a10, b10, c09)
    c11, s11 = fulladder2(a11, b11, c10)
    c12, s12 = fulladder2(a12, b12, c11)
    c13, s13 = fulladder2(a13, b13, c12)
    c14, s14 = fulladder2(a14, b14, c13)
    c15, s15 = fulladder2(a15, b15, c14)
    return c15,s15,s14,s13,s12,s11,s10,s09,s08,s07,s06,s05,s04,s03,s02,s01,s00


TO27 = ( O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O, O)


# 54bits
def add27(A=TO27, B=TO27):
    a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, \
    a15, a14, a13, a12, a11, a10, a09, a08, a07, a06, a05, a04, a03, a02, a01, a00 = A

    b26, b25, b24, b23, b22, b21, b20, b19, b18, b17, b16, \
    b15, b14, b13, b12, b11, b10, b09, b08, b07, b06, b05, b04, b03, b02, b01, b00 = B

    c00, s00 = halfadder2(a00, b00)
    c01, s01 = fulladder2(a01, b01, c00)
    c02, s02 = fulladder2(a02, b02, c01)
    c03, s03 = fulladder2(a03, b03, c02)
    c04, s04 = fulladder2(a04, b04, c03)
    c05, s05 = fulladder2(a05, b05, c04)
    c06, s06 = fulladder2(a06, b06, c05)
    c07, s07 = fulladder2(a07, b07, c06)
    c08, s08 = fulladder2(a08, b08, c07)
    c09, s09 = fulladder2(a09, b09, c08)
    c10, s10 = fulladder2(a10, b10, c09)
    c11, s11 = fulladder2(a11, b11, c10)
    c12, s12 = fulladder2(a12, b12, c11)
    c13, s13 = fulladder2(a13, b13, c12)
    c14, s14 = fulladder2(a14, b14, c13)
    c15, s15 = fulladder2(a15, b15, c14)
    c16, s16 = fulladder2(a16, b16, c15)
    c17, s17 = fulladder2(a17, b17, c16)
    c18, s18 = fulladder2(a18, b18, c17)
    c19, s19 = fulladder2(a19, b19, c18)
    c20, s20 = fulladder2(a20, b20, c19)
    c21, s21 = fulladder2(a21, b21, c20)
    c22, s22 = fulladder2(a22, b22, c21)
    c23, s23 = fulladder2(a23, b23, c22)
    c24, s24 = fulladder2(a24, b24, c23)
    c25, s25 = fulladder2(a25, b25, c24)
    c26, s26 = fulladder2(a26, b26, c25)
    return c26, s26, s25, s24, s23, s22, s21, s20, s19, s18, s17, s16, s15, s14, \
           s13, s12, s11, s10, s09, s08, s07, s06, s05, s04, s03, s02, s01, s00

TO32 = (O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O,O)
#64bits
def add32(A = TO32,B = TO32):

    a31, a30, a29, a28, a27, a26, a25, a24, a23, a22, a21, a20, a19, a18, a17, a16, \
    a15, a14, a13, a12, a11, a10, a09, a08, a07, a06, a05, a04, a03, a02, a01, a00 = A

    b31, b30, b29, b28, b27, b26, b25, b24, b23, b22, b21, b20, b19, b18, b17, b16, \
    b15, b14, b13, b12, b11, b10, b09, b08, b07, b06, b05, b04, b03, b02, b01, b00 = B

    c00, s00 = halfadder2(a00, b00)
    c01, s01 = fulladder2(a01, b01, c00)
    c02, s02 = fulladder2(a02, b02, c01)
    c03, s03 = fulladder2(a03, b03, c02)
    c04, s04 = fulladder2(a04, b04, c03)
    c05, s05 = fulladder2(a05, b05, c04)
    c06, s06 = fulladder2(a06, b06, c05)
    c07, s07 = fulladder2(a07, b07, c06)
    c08, s08 = fulladder2(a08, b08, c07)
    c09, s09 = fulladder2(a09, b09, c08)
    c10, s10 = fulladder2(a10, b10, c09)
    c11, s11 = fulladder2(a11, b11, c10)
    c12, s12 = fulladder2(a12, b12, c11)
    c13, s13 = fulladder2(a13, b13, c12)
    c14, s14 = fulladder2(a14, b14, c13)
    c15, s15 = fulladder2(a15, b15, c14)
    c16, s16 = fulladder2(a16, b16, c15)
    c17, s17 = fulladder2(a17, b17, c16)
    c18, s18 = fulladder2(a18, b18, c17)
    c19, s19 = fulladder2(a19, b19, c18)
    c20, s20 = fulladder2(a20, b20, c19)
    c21, s21 = fulladder2(a21, b21, c20)
    c22, s22 = fulladder2(a22, b22, c21)
    c23, s23 = fulladder2(a23, b23, c22)
    c24, s24 = fulladder2(a24, b24, c23)
    c25, s25 = fulladder2(a25, b25, c24)
    c26, s26 = fulladder2(a26, b26, c25)
    c27, s27 = fulladder2(a27, b27, c26)
    c28, s28 = fulladder2(a28, b28, c27)
    c29, s29 = fulladder2(a29, b29, c28)
    c30, s30 = fulladder2(a30, b30, c29)
    c31, s31 = fulladder2(a31, b31, c30)
    return c31, s31, s30, s29, s28, s27, s26, s25, s24, s23, s22, s21, s20, s19, s18, s17, s16, \
                s15, s14, s13, s12, s11, s10, s09, s08, s07, s06, s05, s04, s03, s02, s01, s00


A = (O,O,O,I,I,I)
B = (O,O,O,O,I,I)
#a1,a2,a3,a4,a5 = A
a5, a4, a3, a2, a1, a0 = A
#print(A,a1,a2)
# C = add6(A,B)
# for i in C:
#      print(TC(i).name, end="")

#test3in2out(fulladder2)

def trit(A, bw):
    C = [ x for x in range(bw)]
    list().append(O)

