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
from enum import Enum
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

# get ternary array
# a1b2c3d4 => ['abcd','1234']
# bin3a(10,8)
# @x is int
# @result array 00001010=>('0011', '0000')
def bin2a(x,n=8):
    bine = lambda x, n: format(x,'b').zfill(n)
    return (bine(x,n)[::2],bine(x,n)[1::2])

##############################################
# triacode riron 三元理论继承形式为逻辑
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

#LIGR
class LOG(Enum):
    """
    ----> 0011 \
    ||||  |||| = GIRL
    ++++> 1010 /
    ====> GIRL
    G = (0,1)
    I = (0,0)
    R = (1,1)
    L = (1,0)
    """
    G = (TOI.O.value, TOI.I.value)
    I = (TOI.O.value, TOI.O.value)
    R = (TOI.I.value, TOI.I.value)
    L = (TOI.I.value, TOI.O.value)
#

#
class TRIACODE(Enum):
    """
    a,a' boolean
    A trian
    A trion
          a
     A = ---
          a'
    """
    T = LOG.L.value
    O = LOG.I.value
    R = LOG.R.value
    I = LOG.G.value


#
class CHAI(Enum):
    P  = -13
    H  = -12
    M  = -11
    O  = -10
    Q  = - 9
    X  = - 8
    K  = - 7
    F  = - 6
    A  = - 5
    J  = - 4
    C  = - 3
    Z  = - 2
    L  = - 1
    R  =   0
    G  = + 1
    N  = + 2
    U  = + 3
    Y  = + 4
    T  = + 5
    E  = + 6
    V  = + 7
    W  = + 8
    I  = + 9
    D  = +10
    B  = +11
    S  = +12
    IE = +13
##############################################

# class triacode():"""
#triacode ternary logic

#code 19683
def tria9():
    T = [TOI.T.name,TOI.O.name,TOI.I.name]
    R = [(a1,a2,a3,a4,a5,a6,a7,a8,a9) \
         for a1 in T for a2 in T for a3 in T \
         for a4 in T for a5 in T for a6 in T \
         for a7 in T for a8 in T for a9 in T ]
    for N in R:
        print("".join([x for x in N]))
        print ("-" * 5 )
        for i in range(9):
            if ((i+1) % 3)==0:
                print(N[i])
            else:
                print(N[i],end=" ")
        print("=" * 5)
    print(len(R))

# code: 729
def tria6():
    count = 0
    T = [TOI.T.name,TOI.O.name,TOI.I.name]
    R = [(a1,a2,a3,a2,a5,a6,a3,a6,a4) \
         for a1 in T for a2 in T for a3 in T \
         for a4 in T for a5 in T for a6 in T ]
    for N in R:
        #print("".join([x for x in N]))

        if N[4] == 'O' and  N[1] != N[5] :
            print("-" * 5)
            print("".join([x for x in N]))
            count += 1
            for i in range(9):
                if ((i+1) % 3)==0:
                    print(N[i])
                else:
                    print(N[i],end=" ")
            print("=" * 5)
    print("count:",count)
    print(len(R))

# code: 81
def tria4():
    T = [TOI.T.name,TOI.O.name,TOI.I.name]
    #R = [(a1, a2, a3, a4, a5, a6, a7, a8, a9)
    #R = [(a1, a2, a3, a2, a5, a6, a3, a6, a4)
    R = [ (a1, a2, a3, a2, a4, a2, a3, a2, a4) \
         for a1 in T for a2 in T for a3 in T  for a4 in T  ]
    for N in R:
        print("".join([x for x in N]))
        print ("-" * 5 )
        for i in range(9):
            if ((i+1) % 3)==0:
                print(N[i])
            else:
                print(N[i],end=" ")
        print("=" * 5)
    print(len(R))

def rid(one):
    """
    a1,a2 = one
    c1 = (a1 & c2) ^ a1
    c2 = (a1 & a2) ^ a2
    c = (c1,c2)
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
    c1,c2 = one
    c = (c2,c1)
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
    #
    TC = TRIACODE
    T = TC.T.value
    R = TC.R.value
    I = TC.I.value
    O = TC.O.value
    print(funcname)
    print (symbol,            TC.T.name,          TC.O.name,          TC.I.name)
    print(TC.T.name,TC(func(T, T)).name,TC(func(T, O)).name,TC(func(T, I)).name)
    print(TC.O.name,TC(func(O, T)).name,TC(func(O, O)).name,TC(func(O, I)).name)
    print(TC.I.name,TC(func(I, T)).name,TC(func(I, O)).name,TC(func(I, I)).name)


def ltest(func, symbol=" ", funcname=""):
    """
    - L I G
    L X X X
    I X X X
    G X X X
    :param func:
    :return:
    """
    L = LOG.L.value
    O = LOG.I.value
    G = LOG.G.value
    print(funcname)
    print (symbol,            LOG.L.name,          LOG.I.name,          LOG.G.name)
    print(LOG.L.name,LOG(func(L, L)).name,LOG(func(L, O)).name,LOG(func(L, G)).name)
    print(LOG.I.name,LOG(func(O, L)).name,LOG(func(O, O)).name,LOG(func(O, G)).name)
    print(LOG.G.name,LOG(func(L, L)).name,LOG(func(G, O)).name,LOG(func(G, G)).name)
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
    #
    TC = TRIACODE
    T = TC.T.value
    R = TC.R.value
    I = TC.I.value
    O = TC.O.value
    print(funcname)
    print (symbol, TC.T.name, TC.O.name, TC.I.name)
    print(TC.T.name,TC(invert(func(T, T))).name,TC(invert(func(T, O))).name,TC(invert(func(T, I))).name)
    print(TC.O.name,TC(invert(func(O, T))).name,TC(invert(func(O, O))).name,TC(invert(func(O, I))).name)
    print(TC.I.name,TC(invert(func(I, T))).name,TC(invert(func(I, O))).name,TC(invert(func(I, I))).name)



#
def sand(A, B):
    """
    a1,a2 = A
    b1,b2 = B
    c1 = a1 & b1
    c2 = a2 | b2
    C = (c1,c2)
    :param A: T,O,I
    :param B: T,O,I
    & T O I
    T T O I
    O O O I
    I I I I
    :return:
    """
    return A[0] & B[0], A[1] | B[1]

#test(sand)
#ltest(sand)
#
def isand(A, B):
    """
    A = invert(A)
    B = invert(b)
    C = sand(A,B)
    :param A: T,O,I
    :param B: T,O,I
   ~& T O I
    T T T T
    O T O O
    I T O I
    :return:
    """
    return sand(invert(A), invert(B))

#
def sor(A, B):
    """
    a1,a2 = A
    b1,b2 = B
    c1 = a1 | b1
    c2 = a2 & b2
    C = (c1,c2)
    :param A: T,O,I
    :param B: T,O,I
    | T O I
    T T T T
    O T O O
    I T O I
    :return:
    """
    return A[0] | B[0], A[1] & B[1]

#
def isor(A, B):
    """
    A = invert(A)
    B = invert(B)
    C = sor(A,B)
    :param A: T,O,I
    :param B: T,O,I
   ~| T O I
    T I O T
    O O O T
    I T T T
    :return:
    """
    return sor(invert(A), invert(B))

#
def band(A, B):
    """
    a1,a2 = A
    b1,b2 = B
    c1 = a1 & b1
    c2 = a2 & b2
    C = (c1,c2)
    :param A: T,O,I
    :param B: T,O,I
    @ T O I
    T T O O
    O O O O
    I O O I
    :return:
    """
    return A[0] & B[0], A[1] & B[1]

#
def iband(A, B):
    """
    a1,a2 = A
    b1,b2 = B
    c1 = b2 & b2
    c2 = a1 & b1
    C = (c1,c2)
    :param A: T,O,I
    :param B: T,O,I
   ~@ T O I
    T I O O
    O O O O
    I O O T
    :return:
    """
    return band(invert(A),invert((B)))

#
def bor(A, B):
    """
    a1,a2 = A
    b1,b2 = B
    c1 = a1 | b1
    c2 = a2 | b2
    c = (c1,c2)
    c = rid(c)
    :param A: T,O,I
    :param B: T,O,I
    # T O I
    T T T O
    O T O I
    I O I I
    :return:
    """
    return rid((A[0] | B[0], A[1] | B[1]))

#
def ibor(A, B):
    """
    A = invert(A)
    B = invert(B)
    C = bor(A,B)
    :param A: T,O,I
    :param B: T,O,I
   ~# T O I
    T I I O
    O I O T
    I O T T
    :return:
    """
    return bor(invert(A), invert(B))

#
def xor(A, B):
    """
    a1,a2 = A
    b1,b2 = B
    c1 = (a1 | b1) & (a2 | b2)
    c2 = (a1 | b2) & (a2 | b1)
    C = (c1,c2)
    :param A: T,O,I
    :param B: T,O,I
    ^ T O I
    T I O T
    O O O O
    I T O I
    :return:
    """
    return (A[0] | B[0]) & (A[1] | B[1]), (A[0] | B[1]) & (A[1] | B[0])

#
def xand(A, B):
    """
    C = xor(A,B)
    C = invert(C)
    :param A: T,O,I
    :param B: T,O,I
   ~^ T O I
    T T O I
    O O O O
    I I O T
    :return:
    """
    return invert(xor(A, B))

#
def sum(A, B):
    """
    a1,a2 = A
    b1,b2 = B
    t1 = (a1 | b1); t2 = (a2 | b2)
    t3 = (a1 & b1); t4 = (a2 & b2)
    t5 = (t3 | t4)
    c1 = (t5 & t2) | (~t5 & t1)
    c2 = (t5 & t1) | (~t5 & t2)
    C = (c1,c2)
    C = rid(C)
    :param A: T,O,I
    :param B: T,O,I
    $ T O I
    T I T O
    O T O I
    I O I T
    :return:

    return rid((c1,c2))
    """
    return rid(((((A[0] & B[0])|(A[1] & B[1]))&(A[1] | B[1]))|(~((A[0] & B[0])|(A[1] & B[1]))&(A[0] | B[0])),\
              (((A[0] & B[0])|(A[1] & B[1]))&(A[0] | B[0]))|(~((A[0] & B[0])|(A[1] & B[1]))&(A[1] | B[1]))))


#
def isum(A, B):
    """
    C = sum(A,B)
    C = invert(C)
    :param A:
    :param B:
   ~$ T O I
    T T I O
    O I O T
    I O T I
    :return:
    """
    return invert(sum(A, B))


def carry(A, B):
    """
    a1,a2 = A; b1,b2 = B
    t1 = (a1 | b1); t2 = (a2 | b2)
    t3 = (a1 & b1); t4 = (a2 & b2)
    t5 = (t3 | t4)
    c1 = t5 | ~(t5 & t1)
    c2 = t5 | ~(t5 & t2)
    C = (c1,c2)
    :param A:
    :param B:
    - T O I
    T O T O
    O T O I
    I O I O
    :return:
    """
    return rid(((((A[0] & B[0]) | (A[1] & B[1]))) | (~((A[0] & B[0]) | (A[1] & B[1])) & ((A[0] | B[0]))), \
                (((A[0] & B[0]) | (A[1] & B[1]))) | (~((A[0] & B[0]) | (A[1] & B[1])) & (A[1] | B[1]))))

def icarry(A, B):
    """
    A = invert(A)
    B = invert(B)
    C = sub(A,B)
    :param A:
    :param B:
   ~- T O I
    T O I O
    O I O T
    I O T O
    :return:
    """
    return carry(invert(A), invert(B))

#test(carry)
#test(icarry)

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
    TC = TRIACODE
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

def halfadder(A, B):
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
    C = band(A, B)
    S = sum(A, B)
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
        TC = TRIACODE
        T = TC.T.value
        R = TC.R.value
        I = TC.I.value
        O = TC.O.value
        TT = [T, O, I]
        print("full adder carry O")
        data = [(x, y) for x in TT for y in TT]
        for x, y in data:
            print(TC(x).name, TC(y).name, TC(O).name, "=", TC(func(x, y, O)[0]).name,
                  TC(fulladder(x, y, O)[1]).name)
        print("full adder carry I")
        for x, y in data:
            print(TC(x).name, TC(y).name, TC(I).name, "=", TC(func(x, y, I)[0]).name,
                  TC(fulladder(x, y, I)[1]).name)

def fulladder(A, B, C):
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
    C1,S1 = halfadder(A,B)
    C2,S2 = halfadder(S1,C)
    #C4,C3 = halfadder(C1,C2)
    C3 = carry(C1, C2)
    return C3,S2



#test3in2out(fulladder)


# test(isum,"$")
# test(sum,"^")
# #test(ibor,"#")
# itest(bor,"|")
# print(TRIA.T,TRIA((1,0)).name)


