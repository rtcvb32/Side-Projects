/**
 * A fixed-sized Arbitrary Int type of struct for D2. Includes x86 & x86_64 specific extentions and assembly language and optimization.
 *
 * Authors:
 *    Era Scarecrow <rtcvb32@yahoo.com>
 */
module std.experimental.arbitraryint;

import std.traits : isIntegral, isSigned, isUnsigned;
import std.format : FormatSpec;
import std.experimental.arbitraryint.gen32;

/// Unsigned cent implimentation
alias UCent = ArbitraryInt!(128, false);
/// Signed cent implimentation
alias Cent = ArbitraryInt!(128, true);

/// Returns if the template is an ArbitraryInt type
enum isArbitraryInt(T) = is(T == ArbitraryInt!(size, flag), size_t size, bool flag);

//this test is to check for a if a binary operation where the sign matters
//if either is negative then the result is neg, but calculate them after inverting them
private enum opSignMatters(string op) = (op == "/" || op == "%" || op == "*" || op == "^^");

//checks if a integral and not larger than our smaller int type
private enum isSmallIntegral(T) = (isIntegral!T && T.sizeof <= Int.sizeof);

version(D_InlineAsm_X86_64) {
    import std.experimental.arbitraryint.amd64;
    private alias Int = ulong;
} else {
    //generic
    private alias Int = uint;
}

private enum IntBits = Int.sizeof*8;

/**
  *  Template for Arbitrary Int. Creates a simulated/emulated byte as long as it's multiple of 64 bits
  *
  */
struct ArbitraryInt(size_t NumBits, bool Signed) {
    static assert(NumBits, "Cannot have empty ArbitaryInt. 64bits minimum!");
    static assert((NumBits % 64) == 0, "Must be a multiple of 64bits");
    enum Size = NumBits / IntBits;
    enum IsSigned = Signed;
    private Int[Size] val;
    
    static if (IsSigned) {
        static immutable ArbitraryInt max = ArbitraryInt(-1) >>> 1, min = max+1;
    }else{
        static immutable ArbitraryInt max = ArbitraryInt(-1), min;
    }
    
    ///set using basic Integral type
    this(T)(T v) 
    if(isIntegral!T) {
        if(isSigned!T && v < 0)
            val[1 .. $] = -1;
        
        static if (!isSmallIntegral!T) {
            static assert(val.sizeof >= T.sizeof, "Internal type is larger than Arbitrary Int...");

            size_t count = T.sizeof / Int.sizeof;
            foreach(ref _v; val[0 .. count]) {
                _v = cast(Int) v;
                v >>= IntBits;
            }
        } else {
            val[0] = v;
        }
    }
    
    ///Takes any ArbitraryInt data, although it might not fit
    this(T)(auto ref const(T) other)
    if (isArbitraryInt!T) {
        static if (T.IsSigned) {
            if (getSign(other.val))
                this.val[] = -1;
        }
    
        static if (T.Size > Size) {
            this.val[] = other.val[0 .. Size];
        } else {
            this.val[0 .. T.Size] = other.val[];
        }
    }
    
    ///Use base10 or base16 to set the value
    this(string v) {
        import std.ascii;
        Int[Size+1] mult_tmp = void;
        Int[1] mult=10, a;
        ArbitraryInt t;
        bool isneg;

        if (v[0] == '-') {
            isneg = true;
            v = v[1 .. $];
        }
        
        if (v[0 .. 2] == "0x" || v[0 .. 2] == "0X") {
            mult[0] = 16;
            v = v[2 .. $];
        }

        foreach(ch; v) {
            a = -1;
            if (isDigit(ch)) {
                a = ch - '0';
            } else if (ch >= 'A' && ch <= 'F') {
                a = ch - 'A' + 10;
            } else if (ch >= 'a' && ch <= 'f') {
                a = ch - 'a' + 10;
            } else if (ch == '_') {
                continue;
            }
            
            assert(a[0] >= 0, "Bad input string");
            assert(a[0] < mult[0], "Exceeds selected base10/base16 type");
            mul(mult_tmp, t.val, mult);
            t.val[] = mult_tmp[0 .. Size];
            add(t.val, a);
        }
        
        if (isneg) {
            this = -t;
        } else
            this = t;
    }

    //returns the sign flags, namely the current one being '2' and the rhs being '1'. 
    private size_t AIgetSigns(T)(auto ref const(T) other) const 
    if (isIntegral!T || isArbitraryInt!T) {
        size_t signFlags;
        static if (isIntegral!T && isSigned!T) { signFlags |= other < 0 ? 1 : 0; }
        static if (isArbitraryInt!T && T.IsSigned) { signFlags |= getSign(other.val) ? 1 : 0; }
        static if (IsSigned) { signFlags |= getSign(val) ? 2 : 0; }
        return signFlags;
    }
    
    private static bool isOneSigned(size_t flags) pure @safe nothrow @nogc {
        return flags == 1 || flags == 2;
    }
    
    /**
     * Performs binary operation and pass it back as a new ArbitaryInt.
     *
     * Params:
     *    other = The other value, can be integral or an ArbitraryInt
     */
    auto opBinary(string op, T)(auto ref const(T) other) const {
        static if (isArbitraryInt!T && Size > T.Size && (op == "/" || op == "%")) {
            //lhs promotion
            return this.opBinary!op(ArbitraryInt!(Size*IntBits, T.IsSigned)(other));
            assert(false); 
        }
        static if (isArbitraryInt!T && Size < T.Size && (op == "+" || op == "-" || op == "*" || op == "&" || op == "|" || op == "^")) {
            //lhs promotion
            auto result = ArbitraryInt!(T.Size*IntBits, IsSigned || T.IsSigned)(this);
        } else {
            ArbitraryInt result = this;
        }
        
        static if ((IsSigned || (isArbitraryInt!T && T.IsSigned) || (isIntegral!T && isSigned!T))   //lhs or rhs is signed
                && (isArbitraryInt!T || isSmallIntegral!T)                                          //where rhs isn't going to be upgraded
                && opSignMatters!op) {                                                      //binaryop where inverting may be neccesary
            size_t signFlags = AIgetSigns(other);
            Int[Size] lhs = this.val;
            
            static if (isArbitraryInt!T) {
                Int[T.Size] rhs = other.val;
            } else {
                Int[1] rhs = other;
            }

            if (signFlags & 2)
                neg(lhs);
            if (signFlags & 1)
                neg(rhs);
        } else {
            enum signFlags = 0;
            alias lhs = val;
            static if (isArbitraryInt!T) {
                const Int[] rhs = other.val;
            } else static if (isSmallIntegral!T) {
                Int[1] rhs = other;
            }
        }
        
        static if (isSmallIntegral!T) {
            static if (op == "+") {
                add(result.val, rhs[]);
            } else static if (op == "-") {
                sub(result.val, rhs[]);
            } else static if (op == "*") {
                Int[Size+1] m_tmp = void;
                
                mul(m_tmp, lhs, rhs);
                result.val[] = m_tmp[0 .. Size];
            } else static if (op == "/" || op == "%") {
                Int[Size*3] buff = void;    
                
                T rem = cast(T) div_small(buff, lhs, rhs[0], result.val);
                
                static if (op == "%") {
                    result.val[] = 0;
                    result.val[0] = rem;
                }
            } else static if (op == "&" || op == "|" || op == "^") {
                mixin("result.val[0] "~op~"= other;");
            //unique int rhs only.
            } else static if (op == ">>>") {
                rshift(result.val, this.val, other);
            } else static if (op == ">>") {
                rshift(result.val, this.val, other, IsSigned ? getSign(val) : false);
            } else static if (op == "<<") {
                lshift(result.val, this.val, other);
            } else static if (op == "^^") {
                assert(other >= 0, "negative power only equals 0, no point in using it.");
                Int[Size*2] m_tmp = void;
                
                result.val[] = 0;
                result.val[0] = 1;
                
                foreach(i; 0 .. other) {
                    mul(m_tmp, result.val, lhs, false);
                    
                    assert(cmp(m_tmp[Size .. $], []) == 0, "Power result too large to be useful on this size of Arbitrary Int");
                    result.val[] = m_tmp[0 .. Size];
                }
            } else {
                static assert(false, "Operation "~op~" Not implimented");
            }
            
            //if one of the results is neg we need to give a negative result.
            static if (opSignMatters!op) {
                if (isOneSigned(signFlags))
                    neg(result.val);
            }
            
            return result;
        } else static if (isIntegral!T) {
            //convert and perform peration. As it's larger than Int/type internally we spuport
            //it's easier to just forward it.
            mixin ("return this "~op~" ArbitraryInt!(NumBits, isSigned!T)(cast(T) other);");
        } else static if (isArbitraryInt!T) {
            static if (op == "+") {
                add(result.val, other.val);
            } else static if (op == "-") {
                sub(result.val, other.val);
            } else static if (op == "*") {
                Int[result.Size+T.Size] m_tmp = void;
                mul(m_tmp, lhs, rhs);
                result.val[] = m_tmp[0 .. result.Size];
            } else static if (op == "/" || op == "%") {
                import std.algorithm : max;
                enum SIZE = max(Size, T.Size);
                Int[SIZE*4] buff = void;
                Int[SIZE] qr = void;
                
                static if (op == "/") {
                    div(buff, lhs, rhs, result.val, qr);
                } else {
                    div(buff, lhs, rhs, qr, result.val);
                }
            } else static if (op == "&" || op == "|" || op == "^") {
                import std.algorithm : min;
                enum Dollar = min(Size, T.Size);
                mixin("result.val[0 .. Dollar] = this.val[0 .. Dollar] "~op~" other.val[0 .. Dollar];");

            } else static if (op == ">>>" || op == ">>" || op == "<<" || op == "^^") {
                //none of these make sense as a larger int type, so forward to the other half.
                assert(cmp(other.val[1 .. $], null) == 0, "Value too large for type "~Int.stringof~" or is negative");
                mixin("result = this.opBinary!(op, Int)(other.val[0]);");
            } else {
                static assert(false, "Operation "~op~" Not implimented");
            }

            
            //in the case of one of the required operators, we invert if both aren't the same sign
            static if (opSignMatters!op && op!="^^") {
                if (isOneSigned(signFlags))
                    neg(result.val);
            }
            
            return result;
        } else {
            static assert(0, "Unknown type, cannot perform op " ~ op ~ " on " ~ T.stringof);
        }
    }

    /**
     * opBinary result with int on the left argument side. returns an ArbitaryInt type
     *
     * Params:
     *    other = is a Integral type.
     */    
    ArbitraryInt opBinaryRight(string op, T)(const(T) other) const
    if (isIntegral!T) {
        //reduced to forwarding and converting due to T could be too large a type to fit.
        mixin("return ArbitraryInt(cast(T) other) "~op~" this;");
    }
    
    /**
     * either invert all the bits, or is a negative (two's compliment) of the number
     */
    ArbitraryInt opUnary(string op)() const
    if (op == "-" || op == "~") {
        ArbitraryInt t;

        t.val[] = ~val[];
        
        static if (op == "-") {
            inc(t.val);
        }
        
        return t;
     }
     
    /**
     * impliments incriment/decrement operators
     */
    ref ArbitraryInt opUnary(string op)()
    if (op == "++" || op == "--") {
        static if (op == "--") {
            dec(val);
        }
        static if (op == "++") {
            inc(val);
        }
        
        return this;
     }
     
    /**
     * forwards any and all operations from opBinary to opOpAssign
     */
    ref ArbitraryInt opOpAssign(string op, T)(auto ref const(T) other) {
        mixin("this = this "~op~" other;");
        return this;
    }

    /**
     * allows implicit assignment of Integrals and ArbitaryInts without requiring opCast
     * If it's downcasting, you require opCast
     *
     * Params:
     *    other = The other value, can be integral or an ArbitraryInt
     */
    ref ArbitraryInt opAssign(T)(auto ref const(T) other) {
        static if(isArbitraryInt!T && T.Size <= Size) {
            static if (T.Size < Size) {
                this.val[other.val.length .. $] = 0;
            }
            
            this.val[0 .. other.val.length] = other.val[];
            
            //forward sign if appropriate
            static if (T.Size < Size && T.IsSigned) {
                if (getSign(other.val)) {
                    this.val[other.val.length .. $] = -1;
                }
            }
            return this;
        } else static if (isIntegral!T) {
            static assert(T.sizeof <= val.sizeof, "Integral type is larger than ArbitaryInt!?");
            this.val[] = ArbitraryInt!(NumBits, isSigned!T)(cast(T) other).val[];
            return this;
        } else {
            static assert(isArbitraryInt!T, "Implicit assignment/downcasting of ArbitraryInt is unsupported, use opCast.");
            static assert(false, "Assignment type for "~T.stringof~" is unsupported");
        }
    }
    
    /**
     * comparison of arbitrary ints, 
     *
     * Params:
     *    other = The other value, can be integral or an ArbitraryInt
     */    
    ptrdiff_t opCmp(T)(auto ref const(T) other) const
    if (isArbitraryInt!T || isIntegral!T) {
        size_t signFlags = AIgetSigns(other);
        
        if (signFlags == 2) return -1;
        if (signFlags == 1) return 1;
        
        //obvious difference not there, so, convert and use cmp
        static if (isSmallIntegral!T) {
            Int[1] rhs = other;
            return signFlags ? icmp(this.val, rhs) : cmp(this.val, rhs);
        } else static if (isIntegral!T) {
            return signFlags ? icmp(this.val, ArbitraryInt(cast(T) other).val) : cmp(this.val, ArbitraryInt(cast(T) other).val);
        } else {
            return signFlags ? icmp(this.val, other.val) : cmp(this.val, other.val);
        }
    }
    
    /**
     * simple compare if identical or not, considers signed-ness
     *
     * Params:
     *    other = The other value, can be integral or an ArbitraryInt
     */
    bool opEquals(T)(auto ref const(T) other) const
    if (isArbitraryInt!T || isIntegral!T) {
        size_t signFlags = AIgetSigns(other);
        
        if (signFlags == 2 || signFlags == 1)
            return false;
        
        //obvious difference not there, so, convert and use cmp
        static if (isIntegral!T && T.sizeof <= Int.sizeof) {
            Int[1] rhs = other;
            return (signFlags ? icmp(this.val, rhs) : cmp(this.val, rhs)) == 0;
        } else static if (isIntegral!T && T.sizeof > Int.sizeof) {
            return (signFlags ? icmp(this.val, ArbitraryInt(cast(T) other).val) : cmp(this.val, ArbitraryInt(cast(T) other).val)) == 0;
        } else {
            return (signFlags ? icmp(this.val, other.val) : cmp(this.val, other.val))== 0;
        }
    }
    
    /**
      * simple cast for bool, or if zero/non-zero
      */
    bool opCast(T)() const
    if (is(T == bool)){
        foreach(v; val) {
            if (v)
                return true;
        }
        return false;
    }

    /**
      * Forcibly converts to a smaller numberic type.
      */
    T opCast(T)() const
    if (isIntegral!T){
        static if (isSmallIntegral!T) {
            return cast(T) val[0];
        } else {
            //it's a larger type than we support, need to build the value instead.
            static assert(val.sizeof > T.sizeof, "Internal type is larger than Arbitrary Int...");

            size_t count = T.sizeof / Int.sizeof;
            T r = 0;
            foreach_reverse(v; val[0 .. count]) {
                r <<= IntBits;
                r |= v;
            }
            return r;
        }
    }
    
    /**
      * Allows up/down casting of any Arbitrary type, though you may lose information.
      */
    T opCast(T)() const
    if (isArbitraryInt!T) {
        T t;
        static if (T.Size > Size) {
            static if (IsSigned) {
                if (getSign(val))
                    t.val[Size .. $] = -1;
            }
            t.val[0 .. Size] = val[];
        } else {
            t.val[] = val[0 .. T.Size];
        }

        return t;
    }

    //for _toString
    private enum Digits = 9;    //per 4bytes
    private enum DigitsBuff = (val.sizeof/4 + 1) * Digits;
    
    /// Returns a string base10 representation of the ArbritraryInt
    string toString() const pure nothrow {
        char[DigitsBuff] str;
        return _toString(str).dup;
    }
    
    /// a non-GC variant
    void toString(scope void delegate (const(char)[]) sink) const {
        char[DigitsBuff] str;
        sink(_toString(str));
    }
    
    ///format capable.
    void toString(scope void delegate (const(char)[]) sink, FormatSpec!char fmt) const {
        char[DigitsBuff] str;
        switch(fmt.spec)
        {
            case 's':
            case 'd':
                sink(_toString(str));
                break;
            case 'x':
                sink(_toString(str[(str.length % 7) .. $], 16, 7));
                break;
            default:
                assert(false, "Format unsupported!");
        }
    }
    
    /** creates a string of base10 to represent the number
      * seeing as division is expensive, it's far faster to grab a bunch of digits and use a cheaper divides
      * every 32bit block can hold 9 digits and 64bit can hold 18 digits, makes a mighty simple algorighm then
      */
    private char[] _toString(char[] str, Int base=10, Int digits=Digits) const pure nothrow @nogc {
        static immutable string digitstring = "0123456789ABCDEF";
        Int digitsMod = base^^digits;
        Int[Size*3] buff = void;   //buffer just in case division requires it

        ArbitraryInt tmp = this;
        Int tmod;
        
        static if (IsSigned) {
            if (getSign(tmp.val)) {
                tmp = -tmp;
            }
        }
        
        foreach_reverse(i, ref ch; str) {
            if ((i+1) % digits == 0) {
                //need both division and mod at the same time, so we'll use one function call
                assert(digitsMod <= uint.max);
                tmod = div_small(buff, tmp.val, digitsMod, tmp.val);
            }
            ch = digitstring[tmod%base];
            tmod /= base;
            if (!tmod && !tmp) {
                //if signed, prepend that
                if(IsSigned && getSign(val)) {
                    --i;
                    str[i] = '-';
                }
            
                return str[i .. $];
            }
        }

        assert(false);  //shouldn't get here.
    }
}

//test Cent/UCent
unittest {
    //check basics, is more the glue than the actual process, which the above should have been confirmed.
    
    //first check that the toString is correct, and basic assignments
    Cent c;
    UCent uc;
    
    //should manage both Big & Little Endian types
    static if(is(Int == ulong)) {
        //fact34
        Int[2] f34 = [0x445DA75B00000000L, 0xDE1BC4D19EFCAC82L];
    } else {
        Int[4] f34 = [0x0, 0x445DA75B, 0x9EFCAC82, 0xDE1BC4D1];
    }
    
    uc.val[] = f34[];
    assert(uc.toString == "295232799039604140847618609643520000000");
    c = cast(Cent) uc;
    assert(c.toString == "-45049567881334322615755997788248211456");
    
    uc = UCent("295232799039604140847618609643520000000");
    assert(uc.toString == "295232799039604140847618609643520000000");
    
    c = Cent("0x688589C_C0E9505E_2F2fee55_80000000");          //fact33
    assert(c.toString == "8683317618811886495518194401280000000");
    
    c = Cent(-100);
    assert(c.toString == "-100");
    assert(Cent("-100").toString == "-100");
    assert(Cent("-0xff").toString == "-255");
    
    //test larger type than supported Int (largest we have is long/ulong), fact!20
    assert(Cent(2432902008176640000L).toString == "2432902008176640000");
    assert(Cent(-2432902008176640000L).toString == "-2432902008176640000");
    
    //test opcast to smaller simpler types
    assert((cast(byte) c) == -100);
    assert((cast(short) c) == -100);
    assert((cast(int) c) == -100);

    //test larger type than what internally we support.
    assert((cast(long) c) == -100);
    assert((cast(long) Cent(-2432902008176640000L)) == -2432902008176640000L);
    
    // & | ^
    assert((Cent("0x0123456789abcdef") & Cent("0x1122334455667788")) == Cent("0x0122014401224588"));
    assert((Cent("0x0123456789abcdef") | Cent("0x1122334455667788")) == Cent("0x11237767DDEFFFEF"));
    assert((Cent("0x0123456789abcdef") ^ Cent("0x1122334455667788")) == Cent("0x10017623DCCDBA67"));

    //basic math
    assert((Cent(100) + Cent(50)).toString == "150");
    assert((Cent(100) - Cent(50)).toString == "50");
    assert((Cent(100) * Cent(50)).toString == "5000");
    assert((Cent(100) / Cent(50)).toString == "2");
    assert((Cent(100) % Cent(51)).toString == "49");
    
    //math with integrals
    assert((Cent(100) + 50).toString == "150");
    assert((Cent(100) - 50).toString == "50");
    assert((Cent(100) * 50).toString == "5000");
    assert((Cent(100) / 50).toString == "2");
    assert((Cent(100) % 51).toString == "49");

    //check binary operator, large rhs
    assert((Cent(100) + 50L).toString == "150");
    assert((Cent(100) - 50L).toString == "50");
    assert((Cent(100) * 50L).toString == "5000");
    assert((Cent(100) / 50L).toString == "2");
    assert((Cent(100) % 51L).toString == "49");
    
    assert((Cent(100) << 2).toString == "400");
    assert((Cent(100) >> 2).toString == "25");
    assert((Cent(-100) >> 2).toString == "-25");
    assert((Cent(-100) >>> 2).toString == "85070591730234615865843651857942052839");
    assert((Cent(3) ^^ 80).toString == "147808829414345923316083210206383297601");
    assert((Cent(-3) ^^ 80).toString == "-147808829414345923316083210206383297601");
    assert((UCent(3) ^^ 80).toString == "147808829414345923316083210206383297601"); //just tests the alternate/unsigned path

    //make sure these int-only types using ArbitaryInt forces downcasting.
    assert((Cent(100) << Cent(2)).toString == "400");
    assert((Cent(100) >> Cent(2)).toString == "25");
    assert((Cent(-100) >> Cent(2)).toString == "-25");
    assert((Cent(-100) >>> Cent(2)).toString == "85070591730234615865843651857942052839");
    assert((Cent(3) ^^ Cent(80)).toString == "147808829414345923316083210206383297601");
    assert((Cent(-3) ^^ Cent(80)).toString == "-147808829414345923316083210206383297601");
    assert((UCent(3) ^^ Cent(80)).toString == "147808829414345923316083210206383297601"); //just tests the alternate/unsigned path
    
    //binaryRight
    assert((100 + Cent(50)).toString == "150");
    assert((100 - Cent(50)).toString == "50");
    assert((100 * Cent(50)).toString == "5000");
    assert((100 / Cent(50)).toString == "2");
    assert((100 % Cent(51)).toString == "49");
    
    //mixed math
    assert((Cent(100) + UCent(50)).toString == "150");
    assert((Cent(100) - UCent(50)).toString == "50");
    assert((Cent(100) * UCent(50)).toString == "5000");
    assert((Cent(100) / UCent(50)).toString == "2");
    assert((Cent(100) % UCent(51)).toString == "49");
    
    //signed div/mod
    assert((Cent(100) / Cent(50)).toString == "2");
    assert((Cent(100) % Cent(51)).toString == "49");
    assert((Cent(-100) / Cent(50)).toString == "-2");
    assert((Cent(-100) % Cent(51)).toString == "-49");
    assert((Cent(100) / Cent(-50)).toString == "-2");
    assert((Cent(100) % Cent(-51)).toString == "-49");
    assert((Cent(-100) / Cent(-50)).toString == "2");
    assert((Cent(-100) % Cent(-51)).toString == "49");

    assert((Cent(100) / 50).toString == "2");
    assert((Cent(100) % 51).toString == "49");
    assert((Cent(-100) / 50).toString == "-2");
    assert((Cent(-100) % 51).toString == "-49");
    assert((Cent(100) / -50L).toString == "-2");
    assert((Cent(100) % -51L).toString == "-49");
    assert((Cent(-100) / -50L).toString == "2");
    assert((Cent(-100) % -51L).toString == "49");

    assert((100 / Cent(50)).toString == "2");
    assert((100 % Cent(51)).toString == "49");
    assert((-100 / Cent(50)).toString == "-2");
    assert((-100 % Cent(51)).toString == "-49");
    assert((100 / Cent(-50)).toString == "-2");
    assert((100 % Cent(-51)).toString == "-49");
    assert((-100 / Cent(-50)).toString == "2");
    assert((-100 % Cent(-51)).toString == "49");
    
    //signed multiply
    assert((Cent(-100) * Cent(50)).toString == "-5000");
    assert((Cent(100) * Cent(-50)).toString == "-5000");
    assert((Cent(-100) * Cent(-50)).toString == "5000");

    assert((Cent(-100) * 50).toString == "-5000");
    assert((Cent(100) * -50).toString == "-5000");
    assert((Cent(-100) * -50).toString == "5000");

    assert((Cent(-100) * 50L).toString == "-5000");
    assert((Cent(100) * -50L).toString == "-5000");
    assert((Cent(-100) * -50L).toString == "5000");

    assert((-100 * Cent(50)).toString == "-5000");
    assert((100 * Cent(-50)).toString == "-5000");
    assert((-100 * Cent(-50)).toString == "5000");
    
    //comparisons
    assert(UCent(100) > UCent(50));
    assert(UCent(100) == UCent(100));
    assert(Cent(100) > UCent(50));
    assert(Cent(100) == UCent(100));
    assert(UCent(100) > Cent(50));
    assert(UCent(100) == Cent(100));
    assert(Cent(100) > Cent(50));
    assert(Cent(100) == Cent(100));
    
    assert(Cent(-2) < UCent(0));
    assert(Cent(-2) < Cent(0));
    assert(Cent(-2) < UCent(-1));
    assert(Cent(-2) < Cent(-1));
    assert(Cent(-2) > -3);
    assert(Cent(-2) > Cent(-3));
    
    //check identical value, but one has a sign
    c = Cent.min;
    uc = cast(UCent) c;

    assert(c != uc);
    assert(uc != c);
    
    assert(Cent(-2) > -3L);
    assert(Cent(-2) != -3L);


    //verify larger/smaller ArbitraryInts work, basic math
    //a bigger worry was division and multiplication
    alias DCent = ArbitraryInt!(256, true);
    assert((Cent(100) + DCent(50)).toString == "150");
    assert((Cent(100) - DCent(50)).toString == "50");
    assert((Cent(100) * DCent(50)).toString == "5000");
    assert((Cent(100) / DCent(50)).toString == "2");
    assert((Cent(100) % DCent(51)).toString == "49");
    
    assert((DCent(100) * Cent(50)).toString == "5000");
    assert((DCent(100) / Cent(50)).toString == "2");
    assert((DCent(100) % Cent(51)).toString == "49");

    //test different sizes with binary operators.
    assert((DCent("0x0123456789abcdef") & Cent("0x1122334455667788")) == Cent("0x0122014401224588"));
    assert((DCent("0x0123456789abcdef") | Cent("0x1122334455667788")) == Cent("0x11237767DDEFFFEF"));
    assert((DCent("0x0123456789abcdef") ^ Cent("0x1122334455667788")) == Cent("0x10017623DCCDBA67"));
    assert((Cent("0x0123456789abcdef") & DCent("0x1122334455667788")) == Cent("0x0122014401224588"));
    assert((Cent("0x0123456789abcdef") | DCent("0x1122334455667788")) == Cent("0x11237767DDEFFFEF"));
    assert((Cent("0x0123456789abcdef") ^ DCent("0x1122334455667788")) == Cent("0x10017623DCCDBA67"));
    
    //opAssign
    c = Cent(100);
    
    c += Cent(50);
    assert(c.toString == "150");
    c -= 100;
    assert(c.toString == "50");
    c *= 100;
    assert(c.toString == "5000");
    c /= 3;
    assert(c.toString == "1666");
    c %= 5;
    assert(c.toString == "1");
    c <<= 5;
    assert(c.toString == "32");
    c >>= 2;
    assert(c.toString == "8");
    c ^= 0xf;
    assert(c.toString == "7");
    c &= 0xc;
    assert(c.toString == "4");
    c |= 0x1;
    assert(c.toString == "5");
    
    //upcast Arbitrary Int
    assert((cast(DCent) c) == 5);
    c = -c;
    assert((cast(Cent) DCent(-5)) == -5);
    assert((cast(DCent) c) == -5);
    
    //test increment/decrement
    c = Cent(10);
    assert(++c == 11);
    assert(c++ == 11);
    assert(c == 12);
    assert(--c == 11);
    assert(c-- == 11);
    assert(c == 10);
    
    //check carry goes through the entire thing
    c = Cent(0);
    assert(--c == -1);
    assert(++c == 0);
    
    //test opAssign, integrals
    c = 100;
    assert(c == 100);
    c = 1000L;
    assert(c == 1000);
    c = -100;
    assert(c == -100);
    
    //same size autoconversion
    c = uc;
    
    alias Long = ArbitraryInt!(64, true);
    
    //upcasting will work fine
    Long l = 0x123456789abcdefL;
    
    c = l;
    assert(c == 0x123456789abcdefL);
    //check sign carry forward
    c = -l;
    assert(c == -0x123456789abcdefL);
    
    //check that we can't downcast
    assert(__traits(compiles, l = c) == false);

    //check ctfe
    //only * and / have asm, so they are the only ones to check

    enum ctfe_mul = (Cent(0x123456789abcdefL) * Cent(50)),
         ctfe_div = (Cent(0x123456789abcdefL) / Cent(50)),
         ctfe_mod = (Cent(0x123456789abcdefL) % Cent(50)),
         ctfe_add = (Cent(0x123456789abcdefL) + Cent(0xffff)),
         ctfe_sub = (Cent(0x123456789abcdefL) - Cent(0xffff)),
         ctfe_add2 = (Cent(0x123456789abcdefL) + 0xffff),
         ctfe_sub2 = (Cent(0x123456789abcdefL) - 0xffff),
         ctfe_lsh = (Cent(0x123456789abcdefL) << 4),
         ctfe_rsh = (Cent(-0x123456789abcdefL) >> 4),
         ctfe_rsh2 = (Cent(-0x123456789abcdefL) >>> 4),
         ctfe_pow = (Cent(3) ^^ 80);
         
    assert(ctfe_mul == Cent(0x123456789abcdefL) * Cent(50));
    assert(ctfe_div == Cent(0x123456789abcdefL) / Cent(50));
    assert(ctfe_mod == Cent(0x123456789abcdefL) % Cent(50));
    assert(ctfe_add == Cent(0x123456789abcdefL) + Cent(0xffff));
    assert(ctfe_sub == Cent(0x123456789abcdefL) - Cent(0xffff));
    assert(ctfe_add2 == Cent(0x123456789abcdefL) + 0xffff);
    assert(ctfe_sub2 == Cent(0x123456789abcdefL) - 0xffff);
    assert(ctfe_lsh == Cent(0x123456789abcdefL) << 4);
    assert(ctfe_rsh == Cent(-0x123456789abcdefL) >> 4);
    assert(ctfe_rsh2 == Cent(-0x123456789abcdefL) >>> 4);
    assert(ctfe_pow == Cent(3) ^^ 80);
}
