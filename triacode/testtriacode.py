#!/usr/bin/python3
# -*- coding: UTF-8 -*-
# triacode research by Daway.Cai,guangxian
# triacode is a ternary fractional logic
#

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
 -  0   1   0   1
 +  0   0   1   1
 ===================
 =  0   T   1   0

 00 =  0   = O = (0,0)
 01 =  1   = I = (0,1)
 10 = -1   = T = (1,0)
 11 =  0   = R = (1,1)

"""
##############################################
from enum import Enum
import sys
import re
##############################################
# logos
# 无的状态不能分辨彼此
class LOGOS(Enum):
    LO = 0   # ro / tao
##############################################
# boolean 逻辑 2**(2*2) = 16种
# 逻辑以真假为基本元素,阴阳分别
class LOGIC(Enum):
    TRUE = 1    # true  one
    FALSE = LOGOS.LO.value #false

class BIT(Enum):
    I = ("%d"%(LOGIC.TRUE.value))    # 1
    O = ("%d"%(LOGIC.FALSE.value))   # 0

# get binary
# 自然数的二进制表示
# x is int, n is binary length
def bin2(x,n=8):
    bin = lambda x, n: format(x,'b').zfill(n)
    return bin(x,n)
#    return "0b"+bin(x,n)

# get bind array
# a1b2c3d4 => ['a1','b2',c3','d4']
# chunk(string)
# @param string = "a1b2c3d4"
# @return ['a1','b2',c3','d4']
def bit(string, length=2):
    return (string[0+i:length+i] for i in range(0, len(string), length))
#print( list(chunk("a1b2c3d4")))

# get bind array with re
# a1b2c3d4 => ['a1','b2',c3','d4']
def rebit(string, length=2):
  return re.findall('.{%d}' % length, string)
#print( list(rbit("a1b2c3d4")))

# get ternary array
# a1b2c3d4 => ['abcd','1234']
# bin3a(10,8)
# @x is int
# @result array 00001010=>('0011', '0000')
def bin2a(x,n=8):
    bine = lambda x, n: format(x,'b').zfill(n)
    return (bine(x,n)[::2],bine(x,n)[1::2])

# check python3
def py3is():
    if sys.version > '3':
        return True
    else:
        return False

##############################################
# riron 三元理论继承形式为逻辑
# 一种比合三进制理论是平衡三进制基础上的三进制
# ri is True, ro is False
# ry/ro=0,li/ri=1,ne=2,ca=3,ko=4,tu=5
# to=-1      fi=6,ge=7,xa=8,po=9,ru=10
##############################################

class RIRON(Enum):
    RI = LOGIC.TRUE.value   # +1
    RO = LOGIC.FALSE.value  # -+0

# tori 三元道理 3**(3*3) = 19683种
# 三元逻辑增加了三元素:(TO)=(反值-1)，(RO)=(零0)，RI=(正+1)
# TO 表示反-（负数）， RO表示合0（零），RI表示正+（正数）
# 正(RI)反(TO)合(RO)
class TORI(Enum):
    TO = -1 # none
    RO = RIRON.RO.value # false
    RI = RIRON.RI.value # true

# toi 道义
# 反合正 (- 0 +)
# 正反合的TO=T，RO=O，RI=I表示三元基本元素(T,O,I)
# T=-1,O=0,I=1, T=none, O=false, I=true
class TOI(Enum):
    T = TORI.TO.value # none
    O = TORI.RO.value # false
    I = TORI.RI.value # true
    def __str__(self):
        return self.name
#print(TOI.I)

#
#      a
# A = ---
#      a'
#
class TRIACODE(Enum):
    T = (TOI.I.value, TOI.O.value)
    O = (TOI.O.value, TOI.O.value)
    R = (TOI.I.value, TOI.I.value)
    I = (TOI.O.value, TOI.I.value)

TC = TRIACODE
T = TC.T.value
R = TC.R.value
I = TC.I.value
O = TC.O.value

#print(TRIACODE.O.name, TRIACODE.O.value)

##############################################

# class triacode():"""
#triacode ternary logic

def rid(one):
    """
    triacode ternary logic function rid(A)
    :param one: TRIACODE Enum type
    :return:
    """
    ro, ri = one
    rori = ro & ri ^ ro, ro & ri ^ ri
    return rori

#
def invert(one):
    """
    triacode ternary
    :param one:
    :return:
    """
    ro, ri = one
    return ri, ro
#
def test(func, symbol=" ", funcname=""):
    """
    - T O I
    T X X X
    O X X X
    I X X X
    :param func:
    :return:
    """
    print(funcname)
    print (symbol, TC.T.name, TC.O.name, TC.I.name)
    print(TC.T.name,TC(func(T, T)).name,TC(func(T, O)).name,TC(func(T, I)).name)
    print(TC.O.name,TC(func(O, T)).name,TC(func(O, O)).name,TC(func(O, I)).name)
    print(TC.I.name,TC(func(I, T)).name,TC(func(I, O)).name,TC(func(I, I)).name)

#
def itest(func, symbol=" ", funcname=""):
    """
    - T O I
    T X X X
    O X X X
    I X X X
    :param func:
    :return:
    """
    print(funcname)
    print (symbol, TC.T.name, TC.O.name, TC.I.name)
    print(TC.T.name,TC(invert(func(T, T))).name,TC(invert(func(T, O))).name,TC(invert(func(T, I))).name)
    print(TC.O.name,TC(invert(func(O, T))).name,TC(invert(func(O, O))).name,TC(invert(func(O, I))).name)
    print(TC.I.name,TC(invert(func(I, T))).name,TC(invert(func(I, O))).name,TC(invert(func(I, I))).name)

#
def sand(A, B):
    """
    & T O I
    T T O I
    O O O I
    I I I I
    :param A:
    :param B:
    :return:
    """
    return A[0] & B[0], A[1] | B[1]

#
def isand(A, B):
    """
   ~& T O I
    T T T T
    O T O O
    I T O I
    :param A:
    :param B:
    :return:
    """
    return sand(invert(A), invert(B))

#
def sor(A, B):
    """
    | T O I
    T T T T
    O T O O
    I T O I
    :param A:
    :param B:
    :return:
    """
    return A[0] | B[0], A[1] & B[1]

#
def isor(A, B):
    """
   ~| T O I
    T I O T
    O O O T
    I T T T
    :param A:
    :param B:
    :return:
    """
    return sor(invert(A), invert(B))

#
def band(A, B):
    """
    @ T O I
    T T O O
    O O O O
    I O O I
    :param A:
    :param B:
    :return:
    """
    return A[0] & B[0], A[1] & B[1]

#
def iband(A, B):
    """
   ~@ T O I
    T I O O
    O O O O
    I O O T
    :param A:
    :param B:
    :return:
    """
    return band(invert(A),invert((B)))

#
def bor(A, B):
    """
    # T O I
    T T T O
    O T O I
    I O I I
    :param A:
    :param B:
    :return:
    """
    return rid((A[0] | B[0], A[1] | B[1]))

#
def ibor(A, B):
    """
   ~# T O I
    T I I O
    O I O T
    I O T T
    :param A:
    :param B:
    :return:
    """
    return bor(invert(A), invert(B))

#
def xor(A, B):
    """
    ^ T O I
    T I O T
    O O O O
    I T O I
    :param A:
    :param B:
    :return:
    """
    return (A[0] | B[0]) & (A[1] | B[1]), (A[0] | B[1]) & (A[1] | B[0])

#
def xand(A, B):
    """
   ~^ T O I
    T T O I
    O O O O
    I I O T
    :param A:
    :param B:
    :return:
    """
    return invert(xor(A, B))

#
def sum(A, B):
    """
    $ T O I
    T I T O
    O T O I
    I O I T
    :param A:
    :param B:
    :return:
    a1,a2 = A; b1,b2 = B
    t1 = (a1 | b1); t2 = (a2 | b2)
    t3 = (a1 & b1); t4 = (a2 & b2)
    t5 = (t3 | t4)
    c1 = (t5 & t2) | (~t5 & t1)
    c2 = (t5 & t1) | (~t5 & t2)
    return rid((c1,c2))
    """
    return rid(((((A[0] & B[0])|(A[1] & B[1]))&(A[1] | B[1]))|(~((A[0] & B[0])|(A[1] & B[1]))&(A[0] | B[0])),\
              (((A[0] & B[0])|(A[1] & B[1]))&(A[0] | B[0]))|(~((A[0] & B[0])|(A[1] & B[1]))&(A[1] | B[1]))))


#
def isum(A, B):
    """
   ~$ T O I
    T T I O
    O I O T
    I O T I
    :param A:
    :param B:
    :return:
    """
    return invert(sum(A, B))


# test(isum,"$")
# test(sum,"^")
# #test(ibor,"#")
# itest(bor,"|")
# print(TRIA.T,TRIA((1,0)).name)


