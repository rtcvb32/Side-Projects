/**
 * A fixed-sized Arbitrary Int type of struct for D2. Includes x86 & x86_64 specific extentions and assembly language and optimization.
 *
 * Authors:
 *    Era Scarecrow <rtcvb32@yahoo.com>
 */
module std.experimental.arbitraryint;

import std.conv;
import std.traits;

/// Unsigned cent implimentation
alias UCent = ArbitraryInt!(128, false);
/// Signed cent implimentation
alias Cent = ArbitraryInt!(128, true);

/// Returns if the template is an ArbitraryInt type
enum isArbitraryInt(T) = is(T == ArbitraryInt!(size, flag), size_t size, bool flag);

/**
  *  Template for Arbitrary Int. Creates a simulated/emulated byte as long as it's multiple of 64 bits
  *
  */
struct ArbitraryInt(size_t NumBits, bool Signed) {
    static assert((NumBits % 64) == 0, "Must be a multiple of 64bits");
    enum Size = NumBits / (Int.sizeof*8);
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
        
        static if (T.sizeof > Int.sizeof) {
            static assert(val.sizeof > T.sizeof, "Internal type is larger than Arbitrary Int...");

            int count = T.sizeof / Int.sizeof;
            T r = 0;
            foreach(ref _v; val[0 .. count]) {
                _v = cast(Int) v;
                v >>= Int.sizeof*8;
            }
        } else {
            val[0] = v;
        }
    }
    
    ///Takes any ArbitraryInt data, although it might not fit
    this(T)(ref const(T) other)
    if (isArbitraryInt!T) {
        static if (T.IsSigned) {
            if (getSign(other))
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
        Int[Size*2] mult_tmp = void;
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

    /**
     * Performs binary operation and pass it back as a new ArbitaryInt.
     *
     * Params:
     *    other = The other value, can be integral or an ArbitraryInt
     */
    ArbitraryInt opBinary(string op, T)(auto ref const(T) other) const {
        ArbitraryInt result = this;

        static if ((op == "/" || op == "%") && (IsSigned || T.IsSigned)) {
            int signFlags;
            static if (isIntegral!T && isSigned!T) { signFlags |= other < 0 ? 1 : 0; }
            static if (isArbitraryInt!T && T.IsSigned) { signFlags |= getSign(other.val) ? 1 : 0; }
            static if (IsSigned) { signFlags |= getSign(val) ? 2 : 0; }
        } else {
            enum signFlags = 0;
        }
        
        static if (isIntegral!T && T.sizeof <= Int.sizeof) {
            Int[1] rhs = other;
            
            static if (op == "+") {
                add(result.val, rhs[]);
            } else static if (op == "-") {
                sub(result.val, rhs[]);
            } else static if (op == "*") {
                Int[Size*2] m_tmp = void;
                mul(m_tmp, this.val, rhs);
                result.val[] = m_tmp[0 .. Size];
            } else static if (op == "/" || op == "%") {
                T rem;
                if (signFlags & 2) {
                    ArbitraryInt lhs = this;
                    rem = div_small(neg(lhs.val), (signFlags & 1) ? -other : other, result.val);
                } else {
                    rem = div_small(this.val, (signFlags & 1) ? -other : other, result.val);
                }
                
                static if (op == "%") {
                    result.val[] = 0;
                    result.val[0] = rem;
                }
                
                if (signFlags == 1 || signFlags == 2)
                    neg(result.val);
            } else static if (op == "&" || op == "|" || op == "^") {
                mixin("result.val[0] "~op~"= other;");
            //unique int rhs only.
            } else static if (op == ">>>") {
                rshift(result.val, this.val, other, 0);
            } else static if (op == ">>") {
                rshift(result.val, this.val, other, IsSigned ? (this.val[$-1] & (1<<(Int.sizeof*8-1))) : 0);
            } else static if (op == "<<") {
                lshift(result.val, this.val, other);
            } else static if (op == "^^") {
                Int[Size*2+1] m_tmp = void;
                result.val[] = 0;
                result.val[0] = 1;

                static if (IsSigned) {
                    bool isneg = getSign(this.val);
                    ArbitraryInt tmp_this = this;
                    if (isneg)
                        neg(tmp_this.val);
                } else {
                    enum isneg = false;
                    alias tmp_this = this;
                }
                
                foreach(i; 0 .. other) {
                    mul(m_tmp, result.val, tmp_this.val, false);
                    assert(m_tmp[$-1] == 0, "Power result too large to be useful on this size of Arbitrary Int");
                    result.val[] = m_tmp[0 .. Size];
                }
                
                if (isneg) {
                    neg(result.val);
                }
            } else {
                static assert(false, "Operation "~op~" Not implimented");
            }
            
            return result;
        } else static if (isIntegral!T && T.sizeof > Int.sizeof) {
            //convert and perform peration. As it's larger than Int/type internally we spuport
            //it's easier to just forward it.
            mixin ("return this "~op~" ArbitraryInt!(NumBits, isSigned!T)(cast(T) other);");
        } else static if (isArbitraryInt!T) {
            static if (op == "+") {
                add(result.val, other.val);
            } else static if (op == "-") {
                sub(result.val, other.val);
            } else static if (op == "*") {
                Int[Size+T.Size] m_tmp = void;
                mul(m_tmp, this.val, other.val);
                result.val[] = m_tmp[0 .. Size];
            } else static if (op == "/" || op == "%") {
                import std.algorithm : max;
                enum SIZE = max(Size, T.Size);
                Int[SIZE*3] buff = void;
                Int[T.Size] d = other.val;
                Int[Size] lhs = this.val, qr = void;

                if (signFlags & 1) neg(d);
                if (signFlags & 2) neg(lhs);
                
                static if (op == "/") {
                    div(buff, lhs, d, result.val, qr);
                } else {
                    div(buff, lhs, d, qr, result.val);
                }

                if (signFlags == 1 || signFlags == 2)
                    neg(result.val);
            } else static if (op == "&" || op == "|" || op == "^") {
                import std.algorithm : min;
                enum Dollar = min(Size, T.Size);
                mixin("result.val[0 .. Dollar] = this.val[0 .. Dollar] "~op~" other.val[0 .. Dollar];");
            } else {
                static assert(false, "Operation "~op~" Not implimented");
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
     * forwards any and all operations from opBinary to opOpAssign
     */
    ref ArbitraryInt opOpAssign(string op, T)(auto ref const(T) other) {
        mixin("this = this "~op~" other;");
        return this;
    }
    
    /**
     * comparison of arbitrary ints, 
     *
     * Params:
     *    other = The other value, can be integral or an ArbitraryInt
     */    
    ptrdiff_t opCmp(T)(auto ref const(T) other) const
    if (isArbitraryInt!T || isIntegral!T) {
        int signFlags;
        static if (isIntegral!T && isSigned!T) { signFlags |= other < 0 ? 1 : 0; }
        static if (isArbitraryInt!T) { signFlags |= getSign(other.val) ? 1 : 0; }
        static if (IsSigned) { signFlags |= getSign(val) ? 2 : 0; }
        
        if (signFlags == 2) return -1;
        if (signFlags == 1) return 1;
        
        //obvious difference not there, so, convert and use cmp

        static if (isIntegral!T && T.sizeof <= Int.sizeof) {
            Int[1] rhs;
            rhs[0] = other;
            return signFlags ? icmp(this.val, rhs) : cmp(this.val, rhs);
        } else static if (isIntegral!T && T.sizeof > Int.sizeof) {
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
        int signFlags;
        static if (isIntegral!T && isSigned!T) { signFlags |= other < 0 ? 1 : 0; }
        static if (isArbitraryInt!T && T.IsSigned) { signFlags |= getSign(other.val) ? 1 : 0; }
        static if (IsSigned) { signFlags |= getSign(val) ? 2 : 0; }
        
        if (signFlags == 2 || signFlags == 1)
            return false;
        
        //obvious difference not there, so, convert and use cmp
        static if (isIntegral!T && T.sizeof <= Int.sizeof) {
            Int[1] rhs;
            rhs[0] = other;
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
    bool opCast(T)()
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
    T opCast(T)()
    if (isIntegral!T){
        static if (T.sizeof <= Int.sizeof) {
            return cast(T) val[0];
        } else {
            //it's a larger type than we support, need to build the value instead.
            static assert(val.sizeof > T.sizeof, "Internal type is larger than Arbitrary Int...");

            int count = T.sizeof / Int.sizeof;
            T r = 0;
            foreach_reverse(v; val[0 .. count]) {
                r <<= Int.sizeof*8;
                r |= v;
            }
            return r;
        }
    }
    
    /**
      * Allows up/down casting of any Arbitrary type, though you may lose information.
      */
    T opCast(T)()
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
    private enum Digits = 9;
    
    /// Returns a string base10 representation of the ArbritraryInt
    string toString() const pure nothrow {
        char[Digits * (val.sizeof+1)] str;
        return _toString(str).dup;
    }
    
    /// a non-GC variant
    void toString(scope void delegate (const(char)[]) dg) const {
        char[Digits * (val.sizeof+1)] str;
        dg(_toString(str));
    }
    
    /** creates a string of base10 to represent the number
      * seeing as division is expensive, it's far faster to grab a bunch of digits and use a cheaper divides
      * every 32bit block can hold 9 digits and 64bit can hold 18 digits, makes a mighty simple algorighm then
      */
    private char[] _toString(char[] str) const pure nothrow @nogc {
        enum DigitsMod = 10L^^Digits;
        ArbitraryInt tmp = this;
        Int tmod;
        
        static if (IsSigned) {
            if (getSign(tmp.val)) {
                tmp = -tmp;
            }
        }
        
        foreach_reverse(i, ref ch; str) {
            if ((i+1) % Digits == 0) {
                //need both division and mod at the same time, so we'll use one function call
                tmod = div_small(tmp.val, DigitsMod, tmp.val);
            }
            ch = cast(char)('0' + (tmod%10));
            tmod /= 10;
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

/*  Internal functions, unlimited add/sub/inc/dec/rshift/lshift/cmp/icmp/div_small_div/mul on one or two arrays

    For simplicity, it's little endian in structure. So [0] is the lowest value and [$-1] has the highest value.
    All functions use array slices.
*/
private {
//    pragma(inline, true): //causing LDC to barf, "can't be always inline & not inline at the same time".
    
    //internal type, should be second largest type we can work with
    //so uint if we can work with longs, and longs if we can work with cent
    version(none) {
        alias Int = ulong;
    } else {
        alias Int = uint;
    }
    
    //forcibly turns off ASM coding.
    debug(NoAsm) {
        enum UseAsm = false;
    } else {
        enum UseAsm = true;
    }
    
    //how many bits used from lower to higher. So 0x5 would return 3, while 0xff would be 8, and 0x100 is 9. etc.
    size_t bitsUsed(Int val) pure @safe @nogc nothrow {
        Int mask = -1;
        size_t total = Int.sizeof*8, bits = Int.sizeof*8;
        
        if (!val)
            return 0;
        
        do {
            bits >>= 1;
            mask <<= bits;
            
            if (!(val & mask)) {
                total -= bits;
                val <<= bits;
            }

        } while (bits);
        
        return total;
    }

    unittest {
        assert(bitsUsed(0) == 0);
        assert(bitsUsed(1) == 1);
        assert(bitsUsed(5) == 3);
        assert(bitsUsed(100) == 7);
        assert(bitsUsed(0xffffff) == 24);
        assert(bitsUsed(0x800000) == 24);
        assert(bitsUsed(0x700000) == 23);
    }

    //like cmp, only reduces both signed flags before considering comparing.
    ptrdiff_t icmp(const(Int)[] lhs, const(Int)[] rhs) pure @safe nothrow @nogc {
        int signFlags = (getSign(lhs) ? 2 : 0) | (getSign(rhs) ? 1 : 0);

        if (signFlags == 2) return -1;
        if (signFlags == 1) return 1;
        
        //reduce excessive sign flag (0 / -1)
        if (signFlags) {
            while(lhs.length && lhs[$-1] == -1)
                lhs = lhs[0 .. $-1];
            while(rhs.length && rhs[$-1] == -1)
                rhs = rhs[0 .. $-1];
        } else {
            while(lhs.length && !lhs[$-1])
                lhs = lhs[0 .. $-1];
            while(rhs.length && !rhs[$-1])
                rhs = rhs[0 .. $-1];
        }
        
        //return length different. If both are signed then invert the answer
        if (lhs.length != rhs.length)
                return signFlags ? rhs.length - lhs.length : lhs.length - rhs.length;
        
        foreach_reverse(i, v; lhs) {
            //was v - rhs[i], however if v is sufficiently large it becomes negative
            if (v > rhs[i])
                return 1;
            if (v < rhs[i])
                return -1;
        }
        
        return 0;
    }

    //basic comparison, any two lengths you want.
    //the larger length of the two is always larger (after reduction).
    ptrdiff_t cmp(const(Int)[] lhs, const(Int)[] rhs) pure @safe nothrow @nogc {
        //reduce
        while(lhs.length && !lhs[$-1])
            lhs = lhs[0 .. $-1];
        while(rhs.length && !rhs[$-1])
            rhs = rhs[0 .. $-1];
        
        if (lhs.length != rhs.length)
            return lhs.length - rhs.length;
        
        foreach_reverse(i, v; lhs) {
            //was v - rhs[i], however if v is sufficiently large it becomes negative
            if (v > rhs[i])
                return 1;
            if (v < rhs[i])
                return -1;
        }
        
        return 0;
    }
    
    unittest {
        Int[4] buff;
        Int[] small = buff[0 .. 2], large=buff[2 .. $];
        
        //identical size, larger later elements
        small[] = [100, 100];
        large[] = [100, 101];

        assert(cmp(large, small) > 0);
        assert(cmp(small, large) < 0);
        assert(icmp(large, small) > 0);
        assert(icmp(small, large) < 0);
        
        //small is smaller, compare by length
        small[1] = 0;
        assert(cmp(large, small) > 0);
        assert(icmp(large, small) > 0);
        assert(cmp(small, large) < 0);
        assert(icmp(small, large) < 0);
        
        assert(cmp(large, small[0 .. 1]) > 0);
        assert(icmp(large, small[0 .. 1]) > 0);
        assert(cmp(small[0 .. 1], large) < 0);
        
        assert(icmp(small[0 .. 1], large) < 0);

        //identical
        assert(cmp(large, large) == 0);
        assert(icmp(large, large) == 0);
        
        //test sufficiently large comparison
        assert(cmp([0xDA444D2C],[0x2]) > 0);

        //icmp specific tests
        assert(icmp([100, 50], [100, -50]) > 0);
        assert(icmp([100, -50], [100, 50]) < 0);

        small[] = [0, -1];
        large[] = [0, 0];
        assert(icmp(small, large) < 0);
        assert(icmp(large, small) > 0);
        
        small[] = [0, -1];
        large[] = [-1, -1];
        assert(icmp(small, large) < 0);
        assert(icmp(large, small) > 0);
        
        //forced reduction, identical
        assert(icmp([-1, -1, -1, -1], [-1])  == 0);
        assert(icmp([-1], [-1, -1, -1, -1])  == 0);

        assert(icmp([-2, -1], [-1])  <= 0);
        assert(icmp([0, 0, uint.max], [-1])  < 0);
        assert(icmp([-1], [0, 0, uint.max])  > 0);
    }
    
    //add any int array to another int array. rhs is truncated to the result/left side.
    Int[] add(Int[] lhs, const (Int)[] rhs) pure @safe @nogc nothrow {
        long t;    //temporary, doubles as carry
        
        if (rhs.length > lhs.length)
            rhs = rhs[0 .. lhs.length];
        
        foreach(i, ref v; lhs[0 .. rhs.length]) {
            t += v;
            t += rhs[i];
            v = cast(Int) t;
            //reset carry
            t >>= Int.sizeof*8;
        }
        
        //carry leftover? Adjust accordingly
        if (t)
            foreach(ref v; lhs[rhs.length .. $]) {
                ++v;
                if (v)
                    break;
            }

        return lhs;
    }

    //subtract any int array to another int array. rhs is truncated to the result/left side.
    Int[] sub(Int[] lhs, const (Int)[] rhs) pure @safe @nogc nothrow {
        long t;    //temporary, doubles as carry
        
        if (rhs.length > lhs.length)
            rhs = rhs[0 .. lhs.length];

        foreach(i, ref v; lhs[0 .. rhs.length]) {
            t += v;
            t -= rhs[i];
            v = cast(Int) t;
            //reset carry
            t >>= Int.sizeof*8;
            version(GNU) {
            //gdc workaround - https://github.com/D-Programming-GDC/GDC/pull/501
                if (t)
                    t |= 0xffffffff_00000000L;
            }
        }
        
        //carry leftover? Adjust accordingly
        if (t)
            foreach(ref v; lhs[rhs.length .. $]) {
                --v;
                if (v != -1)
                    break;
            }
        return lhs;
    }
    
    unittest {
        Int[3] buff;
        Int[] lhs = buff[], rhs;
    
        lhs[] = [100,200,300];
        
        //add/sub - nocarry
        rhs = [5,10,15];
        assert(add(lhs, rhs) == [105, 210, 315]);
        assert(lhs == [105, 210, 315]); //ensures the lhs was updated and was the return
        assert(sub(lhs, rhs) == [100, 200, 300]);
        assert(lhs == [100, 200, 300]); //ensures the lhs was updated and was the return
                
        //add/sub - carry
        rhs = [-1, 0, 0];
        assert(add(lhs, rhs) == [99, 201, 300]);
        assert(sub(lhs, rhs) == [100, 200, 300]);

        rhs = [0, -1, 0];
        assert(add(lhs, rhs) == [100, 199, 301]);
        assert(sub(lhs, rhs) == [100, 200, 300]);
        
        //add/sub rhs longer - nocarry (should be identical to above)

        rhs = [5,10];
        assert(add(lhs, rhs) == [105, 210, 300]);
        assert(sub(lhs, rhs) == [100, 200, 300]);
                
        //add/sub rhs longer - carry
        rhs = [-1, 0];
        assert(add(lhs, rhs) == [99, 201, 300]);
        assert(sub(lhs, rhs) == [100, 200, 300]);

        rhs = [0, -1];
        assert(add(lhs, rhs) == [100, 199, 301]);
        assert(sub(lhs, rhs) == [100, 200, 300]);
        
        //add/sub shorter - nocarry
        lhs = buff[0 .. 2];
        
        rhs = [5,10,15];
        assert(add(lhs, rhs) == [105, 210]);
        assert(sub(lhs, rhs) == [100, 200]);
        
        //add/sub shorter - carry
        rhs = [-1, 0, 0];
        assert(add(lhs, rhs) == [99, 201]);
        assert(sub(lhs, rhs) == [100, 200]);

        rhs = [0, -1, 0];
        assert(add(lhs, rhs) == [100, 199]);
        assert(sub(lhs, rhs) == [100, 200]);
        
        //second half carry
        lhs = buff[];
        lhs[] = [-1, -1, 0];
        assert(add(lhs, [1]) == [0, 0, 1]);
        assert(sub(lhs, [1]) == [-1, -1, 0]);
        lhs[] = [-1, -1, -1];
        assert(add(lhs, [1]) == [0, 0, 0]);
        assert(sub(lhs, [1]) == [-1, -1, -1]);
    }
    
    Int[] inc(Int[] val) pure @safe @nogc nothrow {
        foreach(ref v; val) {
            ++v;
            if (v)
                break;
        }
        return val;
    }
    
    Int[] dec(Int[] val) pure @safe @nogc nothrow {
        foreach(ref v; val) {
            --v;
            if (v != -1)
                break;
        }
        return val;
    }
    
    unittest {
        Int[2] buff;
        assert(inc(buff[]) == [1, 0]);
        assert(dec(buff[]) == [0, 0]);
        assert(dec(buff[]) == [-1, -1]);
        assert(inc(buff[]) == [0, 0]);
    }
    
    //multiply. length of result has to be as big as the lhs+rhs lengths.
    //faster means be less precise, and only return the length that the lhs gives, at which point
    //the lhs has to be as big if not bigger than the rhs.
    Int[] mul(Int[] res, const(Int)[] lhs, const(Int)[] rhs, bool faster = true) pure @nogc nothrow {
        assert(isUnsigned!Int);
        assert(res.length >= lhs.length + rhs.length);

        res[0 .. (faster ? lhs.length : $)] = 0;
        
        if (!__ctfe && UseAsm) {
            version(D_InlineAsm_X86) {
                foreach(i, rhs_v; cast(Int[]) rhs) { //cast only, otherwise: integer constant expression expected instead
                    if (!rhs_v)
                        continue;
                    //s & p pointers to get proper location
                    const(Int)* s = &lhs[0];
                    Int* p = &res[i];
                    int cnt = faster ? lhs.length - i : lhs.length;
                    if (cnt <= 0)
                        break;

                    asm pure nothrow @nogc {
                            mov ESI, s;
                            mov ECX, cnt;
                            mov EDI, p;
                    start:  mov EAX, rhs_v;
                            mul dword ptr [ESI];
                    //EAX:EDX has the result
                            add dword ptr [EDI], EAX;
                            adc dword ptr [EDI + 4], EDX;
                    //manage carry, if applicable
                            jnc nocarry;
                            mov EAX, EDI;   //EAX, EBX and EDX are free now. Avoids extra memory access.
                    carry:  add EDI, 4;
                            add dword ptr [EDI + 4], 1;
                            jc carry;
                            mov EDI, EAX;
                    //advance
                    nocarry:add EDI, 4;
                            add ESI, 4;
                            loop start;
                    }
                }
                return faster ? res[0 .. lhs.length]: res;
            }

            version(none) {
                foreach(i, rhs_v; cast(ulong[]) rhs) { //cast only, otherwise: integer constant expression expected instead
                    if (!rhs_v)
                        continue;
                    //s & p pointers to get proper location
                    const(Int)* s = &lhs[0];
                    Int* p = &res[i];
                    long cnt = faster ? (lhs.length + 1 - i ) / 2 : lhs.length / 2;
                    if (cnt <= 0)
                        break;

                    asm pure nothrow @nogc {
                            mov RSI, s;
                            mov RCX, cnt;
                            mov RDI, p;
                            
                            mov R9D, dword ptr rhs_v+4;   //load multiplier
                            shl R9, 32;
                            mov R9D, dword ptr rhs_v;
                            
                    start:  mov EAX, dword ptr [RSI+4];
                            shl RAX, 32;
                            mov EAX, dword ptr [RSI];
                            mul R9;
                    //RAX:RDX has the result
                    //to ensure CF is untouched from shr we need to shift the values out first.
                    //So the value becomes EBX:EAX:R8D:EDX, no 4th entry with 32bit multiply
                            mov EBX, EAX;
                            shr RAX, 32;
                            mov R8D, EDX;
                            shr RDX, 32;
                    
                            add dword ptr [RDI], EBX;
                            adc dword ptr [RDI + 4], EAX;
                            adc dword ptr [RDI + 8], R8D;
                            adc dword ptr [RDI + 12], EDX;
                    //manage carry, if applicable
                            jnc nocarry;
                            mov RAX, RDI;   //RAX, RBX and RDX are free now. Avoids extra memory access.
                    carry:  add RDI, 4;
                            add dword ptr [RDI + 12], 1;
                            jc carry;
                            mov RDI, RAX;
                    //advance
                    nocarry:add RDI, 8;
                            add RSI, 8;
                            loop start;
                    }
                }
                return faster ? res[0 .. lhs.length]: res;
            }
        }
        
        //need some shifts
        ulong val;
        uint c;

        foreach(i, r; rhs)
        if (r) {
            val = c = 0;
            foreach(i2, ulong l; faster ? lhs[0 .. $-i] : lhs) {
                val = l * r + c + res[i + i2];
                res[i + i2] = cast(Int) val; //lower half saved
                c = val >> (Int.sizeof * 8);  //upper half becomes carry
            }
            
            //if there's a carry, we add it
            if (!faster && c)
                foreach(i2, ref l; res[i+lhs.length .. $]) {
                    val = c + l;
                    l = cast(Int) val;
                    c = val >> (Int.sizeof * 8);
                    if (!c)
                        break;
                }
        }
        
        return faster ? res[0 .. lhs.length]: res;
    }
    
    unittest {
        Int[4] m;
        
        assert(mul(m[],[0x12345678, 0],[0x12345678, 0], false) == [0x1DF4D840, 0x14B66DC, 0, 0]);
        assert(mul(m[],[0x12345678, 0],[0, 0x12345678], false) == [0, 0x1DF4D840, 0x14B66DC, 0]);
        assert(mul(m[],[0, 0x12345678],[0x12345678, 0], false) == [0, 0x1DF4D840, 0x14B66DC, 0]);
        assert(mul(m[],[0x12345678],[0, 0x12345678], false) == [0, 0x1DF4D840, 0x14B66DC, 0]);
        assert(mul(m[],[0, 0x12345678],[0x12345678], false) == [0, 0x1DF4D840, 0x14B66DC, 0]);
        assert(mul(m[],[0x12345678, 0x12345678],[0, 0x12345678], false) == [0, 0x1DF4D840, 0x1F403F1C, 0x14B66DC]);
        assert(mul(m[],[0, 0x12345678], [0x12345678, 0x12345678], false) == [0, 0x1DF4D840, 0x1F403F1C, 0x14B66DC]);
        //shorter input
        assert(mul(m[],[0x12345678, 0x12345678],[0x12345678], false) == [0x1DF4D840, 0x1F403F1C, 0x14B66DC, 0]);
        assert(mul(m[],[0x12345678], [0x12345678, 0x12345678], false) == [0x1DF4D840, 0x1F403F1C, 0x14B66DC, 0]);
        
        //test faster, same as above only truncates to the lhs/input length.
        //only the lower half (equal to the lhs length) will be accurate, the other half may contain junk.
        assert(mul(m[],[0x12345678, 0],[0x12345678, 0]) == [0x1DF4D840, 0x14B66DC]);
        assert(mul(m[],[0x12345678, 0],[0, 0x12345678]) == [0, 0x1DF4D840]);
        assert(mul(m[],[0, 0x12345678],[0x12345678, 0]) == [0, 0x1DF4D840]);
        assert(mul(m[],[0, 0x12345678],[0x12345678]) == [0, 0x1DF4D840]);
        assert(mul(m[],[0x12345678, 0x12345678],[0, 0x12345678]) == [0, 0x1DF4D840]);
        assert(mul(m[],[0x12345678, 0x12345678],[0x12345678]) == [0x1DF4D840, 0x1F403F1C]);
        
        //show truncates to left side.
        assert(mul(m[],[0x12345678],[0, 0x12345678]) == [0]);
        assert(mul(m[],[0x12345678], [0x12345678, 0x12345678]) == [0x1DF4D840]);
        
        //you only need l+r length
        assert(mul(m[0 .. 3],[0x12345678, 0x12345678],[0x12345678]) == [0x1DF4D840, 0x1F403F1C]);
        
    }

    //modular division using native types, part of the larger division code needed.
    //stores the quotient in the result, and returns the remainder
    Int div_small(const Int[] n, Int d, Int[] result) pure nothrow @nogc {
        assert(n.length <= result.length);
        result[n.length .. $] = 0; //zeroize, can't possibly be anything there.
        
        if (!__ctfe && UseAsm) {
            version(D_InlineAsm_X86){
                Int *dividend = cast(Int *) n.ptr;
                Int *quotent = result.ptr;
                Int len = n.length;
                Int r;
                
                asm pure nothrow @nogc {
                        mov ESI, dividend;
                        mov EDI, quotent;
                        mov EAX, 4;
                        mul dword ptr len;      //EDX cleared with this
                        sub EAX, 4;
                        mov EBX, d;     //get divisor
                        add ESI, EAX;   //get to the last element
                        add EDI, EAX;
                        mov ECX, len;   //counter for loop
                start:  mov EAX, dword ptr [ESI];
                        div EBX;
                        mov dword ptr [EDI], EAX;
                        sub ESI, 4;
                        sub EDI, 4;
                        loop start;
                        mov r, EDX;
                }

                return r;
            }

            //will give 64-96 bit division instead of 32-64
            version(none) {
                Int *dividend = cast(Int *) n.ptr;
                Int *quotent = result.ptr;
                Int len = cast(Int) (n.length / 2);
                Int r;
                
                //due to byte order and compatible using uint, gotta load/save 32bit at a time.
                asm pure nothrow @nogc {
                        mov RSI, dividend;
                        mov RDI, quotent;
                        mov EAX, 8;
                        mul dword ptr len;
                        sub EAX, 8;
                        
                        xor RBX, RBX;
                        mov EBX, d;     //get divisor
                        
                        add RSI, RAX;   //get to the last element
                        add RDI, RAX;
                        
                        xor RDX, RDX;   //remainder of last divide
                        xor RCX, RCX;
                        mov ECX, len;   //counter for loop
                start:;
                        mov EAX, dword ptr [RSI+4];//load high
                        shl RAX, 32;
                        mov EAX, dword ptr [RSI];  //load low
                        
                        div RBX;
                        
                        mov dword ptr [RDI], EAX;  //save low
                        shr RAX, 32;
                        mov dword ptr [RDI+4], EAX;//save high
                        
                        sub RSI, 8;
                        sub RDI, 8;
                        loop start;
                        mov r, EDX;     //save remainder.
                }

                return r;
            }
        }

        ulong val;
        foreach_reverse(i, v; n) {
            val |= v;
            Int t = cast(Int) (val / d);
            result[i] = t;
            val -= t * d;
            val <<= 32;
        }
        
        return val >> 32;
    }
    
    unittest {
        //fact34: 295 232799039 604140847 618609643 520000000
        Int[4] n = [0, 0x445DA75B, 0x9EFCAC82, 0xDE1BC4D1],
               q;

        Int[] rem = [295, 232799039, 604140847, 618609643, 520000000];
        Int[][] q_res = [
            cast(Int[])[0],
            cast(Int[])[0x127],
            cast(Int[])[0xBD3F013F, 0x44],
            cast(Int[])[0x1FE42B2F, 0x12D9A84, 0x10],
            cast(Int[])[0xFE3851EB, 0x62C1065F, 0xB9F2D8F9, 0x3],
        ];
        
        //basically do 9 digits at a time as though we were printing, easiest verification with known factorial!34
        foreach_reverse(i, r; rem) {
            assert(r == div_small(n[], 1_0000_00000, q[]));
            assert(cmp(q[], q_res[i]) == 0);
            n[] = q[];
        }
    }
    
    //left shift the value into the result. Value & result can be the same array.
    Int[] lshift(Int[] result, const Int[] value, size_t shiftby) pure @safe @nogc nothrow {
        assert(result.length == value.length);
        assert(shiftby >= 0 && shiftby < (result.length*Int.sizeof*8));
        enum uint32 = uint.sizeof*8;
        if (!__ctfe && UseAsm) {
            version(none){}
        }

        size_t skip = shiftby / uint32;    //how many whole blocks to move by
        shiftby -= skip * uint32;
        ulong t;

        if (!shiftby)
            result[skip .. $] = value[0 .. $-skip];
        else
            foreach(i, ref v; result[skip .. $]) {
                t |= (cast(ulong)value[i]) << shiftby;
                v = cast(uint) t;
                t >>= uint32;
            }

        result[0 .. skip] = 0;
        
        return result;
    }

    //right shift the value into the result.  Value & result can be the same array.
    Int[] rshift(Int[] result, const Int[] value, size_t shiftby, Int setcarry = 0) pure @safe @nogc nothrow {
        assert(value.length == result.length);
        assert(shiftby >= 0 && shiftby < (result.length*Int.sizeof*8));
        enum uint32 = uint.sizeof*8;
        if (!__ctfe && UseAsm) {
            version(none){}
        }

        size_t skip = shiftby / uint32;    //how many whole blocks to move by
        shiftby -= skip * uint32;

        if (!shiftby)
            result[0 .. $-skip] = value[skip .. $];
        else {
            size_t left = uint32 - shiftby;
            ulong t = setcarry ? -1L << (left+uint32) : 0;
            foreach_reverse(i, ref v; result[0 .. $-skip]) {
                t |= (cast(ulong)value[i+skip]) << left;
                v = t >> uint32;
                t <<= uint32;
            }
        }

        result[$-skip .. $] = setcarry ? -1 : 0;

        return result;
    }
    
    unittest {
        enum orig = [0x76543210, 0xfedcba98];
        Int[2] lhs = orig;
        
        assert(lshift(lhs, orig, 4) == [0x65432100, 0xedcba987]);
        assert(lshift(lhs, orig, 32) == [0, 0x76543210]);
        assert(lshift(lhs, orig, 36) == [0, 0x65432100]);

        assert(rshift(lhs, orig, 4) == [0x87654321, 0x0fedcba9]);
        assert(rshift(lhs, orig, 32) == [0xfedcba98, 0]);
        assert(rshift(lhs, orig, 36) == [0x0fedcba9, 0]);

        //third argument, anything positive becomes the carry
        assert(rshift(lhs, orig, 4, 1) == [0x87654321, 0xffedcba9]);
        assert(rshift(lhs, orig, 32, 1000) == [0xfedcba98, 0xffffffff]);
        assert(rshift(lhs, orig, 36, 0xffffff) == [0xffedcba9, 0xffffffff]);
        
        //test against same buffer, make sure it doesn't cause issues
        lhs = orig; assert(lshift(lhs, lhs, 4) == [0x65432100, 0xedcba987]);
        lhs = orig; assert(lshift(lhs, lhs, 32) == [0, 0x76543210]);
        lhs = orig; assert(lshift(lhs, lhs, 36) == [0, 0x65432100]);

        lhs = orig; assert(rshift(lhs, lhs, 4) == [0x87654321, 0x0fedcba9]);
        lhs = orig; assert(rshift(lhs, lhs, 32) == [0xfedcba98, 0]);
        lhs = orig; assert(rshift(lhs, lhs, 36) == [0x0fedcba9, 0]);

        lhs = orig; assert(rshift(lhs, lhs, 4, 1) == [0x87654321, 0xffedcba9]);
        lhs = orig; assert(rshift(lhs, lhs, 32, 1000) == [0xfedcba98, 0xffffffff]);
        lhs = orig; assert(rshift(lhs, lhs, 36, 0xffffff) == [0xffedcba9, 0xffffffff]);
    }

/*  Perhaps the hardest part of this whole thing is the following function. Watched a video on a simple
    arbitrary division using only the most significant digit, which then collapses easily enough with
    div_asm_small where a number of passes are done each getting closer to the goal, taking the difference
    and adjusting the next pass.
    
    One key diference is i shift the divisor and dividend to fill a full 32/64 bit block first, my tests
    showed it went from 8 passes to 2.
    
    the buff must be at least 3 times larger than the n value, for temporaries. This avoids the gc
    */
    void div(Int[] buff, const Int[] n, const Int[] d, Int[] q, Int[] r) pure @nogc nothrow {
        assert(d, "Divide by zero");
        
        foreach_reverse(i, Int divisor; d) {
            if (!divisor)
                continue;
        //find most significant to divide by.
            if (i == 0) {
                //simple division, no guesswork
                //call the simpler separated one once
                r[1 .. $] = 0;
                r[0] = div_small(n, divisor, q);
            } else {
                Int[] quotent_t = buff[0 .. n.length];
                Int[] mult_temp = buff[n.length .. $];
                bool dividend_sign = true;
                size_t reduceby = bitsUsed(divisor), Size = n.length;
                alias dividend = r;
                dividend[] = n[];
                q[] = 0;
                
                //removes custom reduceby, for speed testing
                debug(NoDivReduce) { reduceby = 0; }
                
                //new divisor, should be fully filled
                if (reduceby)
                    divisor = (rshift(mult_temp[0 .. d.length], d, reduceby)[--i]);
                    
                do {
                    if (reduceby)
                        rshift(dividend, dividend, reduceby);
                
                    //divide
                    div_small(dividend[i .. $], divisor, quotent_t); //remainder is junk

                    //add/sub to our current total
                    if (dividend_sign)
                        add(q, quotent_t);
                    else
                        sub(q, quotent_t);

                    //multiply
                    mul(mult_temp, q, d, false);
                    
                    //subtract the difference
                    sub(mult_temp, n);

                    dividend_sign = mult_temp[$-1] == -1;   //quick dirty check
                    
                    if (dividend_sign) {
                        dividend[] = ~mult_temp[0 .. Size];
                        inc(dividend);
                    } else
                        dividend[] = mult_temp[0 .. Size];

                } while(cmp(dividend, d) > 0);
                
                //off by one? Fix remainder
                //in the rare case we get a fully divisible answer, this cmp check will see if that's the case.
                if (!dividend_sign && cmp(dividend, null) > 0) {
                    dec(q);
                    r[] = d[];
                    sub(r, mult_temp[0 .. Size]);
                }
                
                break;
            }
        }
        
        //out(void) not an option, so this is an extra check confirming d*q+r = n
        debug {
            assert(cmp(r, null) >= 0);
            assert(cmp(r, d) < 0);
            //no out(void)
            mul(buff, q, d, false);
            add(buff, r);
            assert(cmp(buff, n) == 0);
        }
    }
    
    unittest {
        Int[12] buff;
        Int[4]  fact13p1=[0x7328CC01, 1, 0, 0],
                fact21 = [0xB8C40000, 0xC5077D36, 2, 0],
                fact28 = [0xAE000000, 0xAD2CD59D, 0xD925BA47, 3],
                fact34 = [0, 0x445DA75B, 0x9EFCAC82, 0xDE1BC4D1],
                q, r;
        
//        5778574175582208000 & 0
        //even result
        div(buff, fact34, fact21, q, r);
        assert(q == [0xB3D2C000, 0x50319E98, 0, 0]);
        assert(r == [0, 0, 0, 0]);
        
        //force remainder, fact21+1
        inc(fact21);
//        5778574175582207999 & 45312367996127232001
        div(buff, fact34, fact21, q, r);
        assert(q == [0xB3D2BFFF, 0x50319E98, 0, 0]);
        assert(r == [0x4F14001, 0x74D5DE9E, 2, 0]);

        //test with at least one negative interation calculation
        div(buff, fact28, fact13p1, q, r);
        assert(q == [0xF1BCE021, 0xA77C82A7, 2, 0]);
        assert(r == [0x6180D3DF, 0, 0, 0]);

        //test forward to div_small, small enough divisor
        Int[4] sm = [123456789, 0, 0, 0];
        div(buff, fact21, sm, q, r);
        assert(q == [0x5A95ECDC, 0x60, 0, 0]);
        assert(r == [0x59765F5, 0, 0, 0]);
    }

    //assumed signed, returns the highest bit of the last element
    bool getSign(const Int[] n) @safe pure nothrow @nogc {
        return cast(bool) (n[$-1] & (1 << (Int.sizeof*8-1)));
    }
    
    unittest {
        assert(getSign([0]) == false);
        assert(getSign([0x7fffffff]) == false);
        assert(getSign([0x80000000]) == true);
        assert(getSign([0xffffffff]) == true);
        assert(getSign([0, 0]) == false);
        assert(getSign([0, 0x7fffffff]) == false);
        assert(getSign([0, 0x80000000]) == true);
        assert(getSign([0, 0xffffffff]) == true);

        assert(getSign([0x7fffffff, 0]) == false);
        assert(getSign([0x80000000, 0]) == false);
        assert(getSign([0xffffffff, 0]) == false);
    }

    //return/apply 2's compliment
    Int[] neg(Int[] n) @safe pure nothrow @nogc {
        n[] = ~n[];
        return inc(n);
    }
    
    unittest {
        Int[2] val = [100, 0];
        
        assert(neg(val) == [-100, -1]);
        assert(neg(val) == [100, 0]);
        val[] = 0x55555555;
        assert(neg(val) == [0xAAAAAAAB, 0xAAAAAAAA]);
        assert(neg(val) == [0x55555555, 0x55555555]);
    }
    
}

//test Cent/UCent
unittest {
    //check basics, is more the glue than the actual process, which the above should have been confirmed.
    
    //first check that the toString is correct, and basic assignments
    Cent c;
    UCent uc;
    
    uc.val[] = [0x0, 0x445DA75B, 0x9EFCAC82, 0xDE1BC4D1];    //fact34
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
    assert((Cent(100) / -50).toString == "-2");
    assert((Cent(100) % -51).toString == "-49");
    assert((Cent(-100) / -50).toString == "2");
    assert((Cent(-100) % -51).toString == "49");

    assert((100 / Cent(50)).toString == "2");
    assert((100 % Cent(51)).toString == "49");
    assert((-100 / Cent(50)).toString == "-2");
    assert((-100 % Cent(51)).toString == "-49");
    assert((100 / Cent(-50)).toString == "-2");
    assert((100 % Cent(-51)).toString == "-49");
    assert((-100 / Cent(-50)).toString == "2");
    assert((-100 % Cent(-51)).toString == "49");
    
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
    

}