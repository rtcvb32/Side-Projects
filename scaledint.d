module scaledint;

/** Author: Era Scarecrow <rtcvb32@yahoo.com>
    Date: 11 June 2017
    License: Undecided.
    Description: A fixed-sized Arbitrary Int type of struct for D2. Includes x86 & x86_64 specific extentions and assembly language and optimization.
    
    Done: UCent defined and passes all my basic tests. Still adding more tests and final debugging. Cent untested. Compatibility & generic D version completed.
    
    Todo: Add further battery of tests and speed test, more documentation, more opBinary/opUnary, and other possible operations not yet covered by code.
    
    Usage: UScaledInt!bits, it will upscale to a multiple of either 32 or 64 bits. so UScaled!bits 90 could give you 96 or 128 bits. If not x86 it will return a multiple of 32
*/

import std.stdio;
import std.conv;
import std.traits;

alias UCent = UScaledInt!128;
alias Cent = ScaledInt!128;

version(X86) {
//    version = Intel;
    enum OpPrefix = "E";
    enum Ptr = "dword ptr";
}
version(X86_64) {
//    version = Intel;
    enum OpPrefix = "R";
    enum Ptr = "qword ptr";
}

version(Intel) {
    //largest natural type
    alias Int = UnsignedTypeOf!size_t;
} else {
    //need longs for calculations, so the storage is 32bit uints
    alias Int = uint;
}

//returns how many size_t's needed to hold that many bits. Giving 100 will give you 128, etc
int calcSizeFromBits(int bits) {
    return bits / (Int.sizeof * 8) + (bits % (Int.sizeof * 8) ? 1 : 0);
}

unittest {
    assert(calcSizeFromBits(1) == 32 / (Int.sizeof*8));
    assert(calcSizeFromBits(32) == 32 / (Int.sizeof*8));
    assert(calcSizeFromBits(64) == 64 / (Int.sizeof*8));
    assert(calcSizeFromBits(100) == 128 / (Int.sizeof*8));
    assert(calcSizeFromBits(200) == 224 / (Int.sizeof*8));
}

version(Intel) {
    //to handle 32 & 64bit code at the same time, no rewriting
    
    import std.array : replace;
    //^ = ptr type, &=multiplier value, @=reg prefix name, %=instruction
    //# = numerical multiplier in loops, separate round.
    //give a good basic translation, should allow the formula to work in 64bit and 32bit easily.
    string translate(string src, string inst="", int mult=size_t.sizeof, string pre=OpPrefix, string ptr=Ptr, string head="asm pure nothrow @nogc {", string foot="}") {
        return head ~ src.replace("^", ptr).replace("&", to!string(mult)).replace("@", pre).replace("%", inst) ~ foot;
    }

    //same as previous translate, except multiple instructions can be passed.
    //good for add followed by adc, letting me drop instructions without adding complexity
    //so ["add", "adc"] means %=add and %%=adc.
    string translate(string src, string[] instlist, int mult=size_t.sizeof, string pre=OpPrefix, string ptr=Ptr, string head="asm pure nothrow @nogc {", string foot="}") {
        string t = src;
        
        string x = "%";
        while (x.length <= instlist.length)
            x ~= x;
        
        //go largest to smallest, otherwise wouldn't work
        foreach_reverse(i, inst; instlist) {
            string y = x[0 .. i+1];
            t = t.replace(y, inst);
        }
        
        return translate(t, "", mult, pre, ptr, head, foot);
    }

    string translate_cnt(string src, int count, int mult, int start=0, bool dir_forward=true) {
        string t;
        
        if (dir_forward) {
            foreach(i; start .. count)
                t ~= src.replace("#", to!string(i * mult));
        } else {
            foreach_reverse(i; start .. count)
                t ~= src.replace("#", to!string(i * mult));
        }
        return t;
    }

    unittest {
        assert(translate("% @SI, ^ [@SI+&];", "skipjump", 17, "F", "tweak ptr", "", "") ==
                         "skipjump FSI, tweak ptr [FSI+17];");
        assert(translate("% @SI, ^ [@SI+&]; %%; %%%;", ["skipjump", "hoptrip", "hotsauce"], 17, "F", "tweak ptr", "", "") ==
                         "skipjump FSI, tweak ptr [FSI+17]; hoptrip; hotsauce;");
        assert(translate_cnt("mov AX, [SI+#];", 4, 5, 1, true) ==
                             "mov AX, [SI+5];mov AX, [SI+10];mov AX, [SI+15];");
        assert(translate_cnt("mov AX, [SI+#];", 4, 5, 1, false) ==
                             "mov AX, [SI+15];mov AX, [SI+10];mov AX, [SI+5];");
                             
        //header/footer
        assert(translate("nop;") ==
                        "asm pure nothrow @nogc {nop;}");
    }

    //multiplication magic. res is the result output. fast means it will drop multiplication to work less
    //since you're more interested in what will fit within UScaledInt, at which point it will shrink
    //what needs to multiply to get the full result.
    void mul_asm(Int[] res, const(Int)[] lhs, const(Int)[] rhs, bool faster=true) pure @nogc nothrow {
        assert(res.length >= (rhs.length+lhs.length), "Needs an output/buffer able to hold the largest possible multiplied value from input");
        res[] = 0;
        
        //0 in either by no length means 0 for a result.
        if (!lhs.length || !rhs.length)
            return;
        
        foreach(i, rhs_v; cast(Int[])rhs) {
            if (!rhs_v)
                continue;
            //s & p pointers to get proper location
            const(Int) *s = cast(const Int*) lhs.ptr;
            Int* p = &res[i];
            Int cnt = faster ? lhs.length - i : lhs.length;

            enum x = translate("
                    mov @SI, s;
                    mov @CX, cnt;
                    mov @DI, p;
            start:  mov @AX, rhs_v;
                    mul ^ [@SI];
            //?AX:?DX has the result
                    add [@DI], @AX;
                    adc [@DI+&], @DX;
            //manage carry, if applicable
                    jnc nocarry;
                    push @DI;
            carry:  add @DI,&;
                    add ^ [@DI+&], 1;
                    jc carry;
                    pop @DI;
            //advance
            nocarry:add @DI,&;
                    add @SI,&;
                    loop start;");
            
            mixin(x);
        }
    }

    //like div above, but we know we can do it with simple division, along with simple returns
    void div_asm_small(const Int[] n, Int d, Int[] q, out Int r) pure @nogc {
        assert(n.length <= q.length);
        //simple division, no guesswork
        //make pointers here due to ASM issues trying to get it to work
        Int *dividend = cast(Int *) n.ptr;
        Int *quotent = q.ptr;
        Int len = n.length;
        
        enum x = translate("
                mov @SI, dividend;
                mov @DI, quotent;
                mov @AX, &;
                mul ^ len;      //DX cleared? if it's a really really big num it isn't
                sub @AX, &;
                mov @BX, d;     //get divisor
                add @SI, @AX;   //get to the last element
                add @DI, @AX;
                mov @CX, len;   //counter for loop
                xor @DX, @DX;
        start:  mov @AX, [@SI];
                div @BX;
                mov [@DI], @AX;
                sub @SI, &;
                sub @DI, &;
                loop start;
                mov @BX, r;
                mov [@BX], @DX;");
        
        mixin(x);
    }

    //determines how many you can shift/lower the data in order to fit the max number of bits in a single block
    //intended for division to lower how many times it needs to be calculated and be faster overall
    int bitsUsed(T)(T val) pure @safe
    if (isUnsigned!T) {
        T mask = -1;
        int total=T.sizeof*8, bits=T.sizeof*8;
        
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
        assert(bitsUsed!uint(0) == 0);
        assert(bitsUsed!uint(1) == 1);
        assert(bitsUsed!uint(5) == 3);
        assert(bitsUsed!uint(100) == 7);
        assert(bitsUsed!uint(0xffffff) == 24);
        assert(bitsUsed!uint(0x800000) == 24);
        assert(bitsUsed!uint(0x700000) == 23);
        assert(bitsUsed!uint(-1) == 32);

        assert(bitsUsed!ulong(0) == 0);
        assert(bitsUsed!ulong(1) == 1);
        assert(bitsUsed!ulong(5) == 3);
        assert(bitsUsed!ulong(100) == 7);
        assert(bitsUsed!ulong(0xffffff) == 24);
        assert(bitsUsed!ulong(0x800000) == 24);
        assert(bitsUsed!ulong(0x700000) == 23);
        assert(bitsUsed!ulong(-1) == 64);
    }
}

struct UScaledInt(int Bits)
if (Bits > 1) {    //the 128 limit due to loop opcode
    enum Size = calcSizeFromBits(Bits);
    Int[Size] val;
    static immutable min = UScaledInt();
    static immutable max = UScaledInt(-1);
    
    //force a larger type if needed
    this(T)(T i) pure @safe @nogc nothrow
    if (isUnsigned!T) {
        (cast(T[])val)[0] = i;
    }
    
    this(T)(T i) pure @safe @nogc nothrow
    if (isSigned!T) {
        (cast(T[])val)[0] = i;
        if (i < 0) {
            (cast(T[])val)[1 .. $] = -1;
        }
    }

    this(string i) pure @nogc nothrow {
        UScaledInt t;

        foreach(ch; i) {
            if (ch == '-' || ch == '_')
                continue;
        
            t *= 10;
            t += ch - '0';
        }
        
        if (i.length && i[0] == '-')
            t.applyNeg();
        
        val[] = t.val[];
    }
    
    this(const void[] i) pure @nogc nothrow{
        assert(i.length*8 <= Bits);
        void[] t = this.val;
        t[0 .. i.length] = i[];
    }

    bool opEquals(size_t rhs) const @safe pure @nogc nothrow {
        return opEquals(UScaledInt(rhs));
    }

    bool opEquals(T)(T rhs) const @safe pure @nogc nothrow
    if (isIntegral!T) {
        return opEquals(UScaledInt(rhs));
    }
    
    bool opEquals(const UScaledInt rhs) const @safe pure @nogc nothrow {
        return val[] == rhs.val[];
    }

    int opCmp(size_t rhs) const @safe pure @nogc nothrow {
        return opCmp(UScaledInt(rhs));
    }

    int opCmp(const UScaledInt rhs) const @safe pure @nogc nothrow {
        foreach_reverse(i, v; rhs.val) {
            if (val[i] != v) {
                return val[i] > v ? 1 : -1;
            }
        }
        return 0;
    }

    //inc/dec
    version(Intel) {
        ref UScaledInt opUnary(string op)() pure @nogc nothrow
        if (op == "++" || op == "--") {
            static if (op == "++") {
                enum Inst = ["add", "adc"];
            } else {
                enum Inst = ["sub", "sbb"];
            }
            
            enum x = translate("
                    mov @BX, this;
                    % ^ [@BX], 1;" ~ translate_cnt("
                //the jnc is more useful when it gets really large...
                    jnc fin;
                    %% ^ [@BX+#], 0;", Size, Int.sizeof, 1) ~ "
                fin:;", Inst);
            
            mixin(x);

            return this;
        }
    } else {
        ref UScaledInt opUnary(string op)() pure @nogc nothrow
        if (op == "++" || op == "--") {
            foreach(ref v; val) {
                static if (op == "++") {
                    ++v;
                    if (v)
                        break;
                } else {
                    --v;
                    if (!v)
                        break;
                }
            }
            return this;
        }
    }
    
    UScaledInt opBinary(string op)(const UScaledInt rhs) const pure @nogc nothrow
    if (op == "+" || op == "-") {
        UScaledInt t = this;
        mixin("t"~op~"=rhs;");
        return t;
    }

    version(Intel) {
        ref UScaledInt opOpAssign(string op)(const UScaledInt rhs) pure @nogc nothrow
        if (op == "+" || op == "-") {
            static if (op == "+") {
                enum Inst = ["add", "adc"];
            } else {
                enum Inst = ["sub", "sbb"];
            }
        
            enum x = translate("
                    lea @SI, rhs;
                    mov @DI, this;
                    mov @AX, [@SI];
                    % [@DI], @AX;" ~ translate_cnt("
                    mov @AX, [@SI+#];
                    %% [@DI+#], @AX;", Size, Int.sizeof, 1), Inst);
            
            mixin(x);
            return this;
        }
    } else {
        ref UScaledInt opOpAssign(string op)(const UScaledInt rhs) pure @nogc nothrow
        if (op == "+" || op == "-") {
            long t;    //temporary, doubles as carry
            foreach(i, ref v; val) {
                t += v;
                static if (op == "+") {
                    t += rhs.val[i];
                } else {
                    t -= rhs.val[i];
                }
                v = cast(Int) t;
                //reset carry, will be 33rd bit
                t >>= Int.sizeof*8;
            }
            
            return this;
        }
    }
    
    UScaledInt opBinary(string op)(const UScaledInt rhs) const pure @nogc nothrow
    if (op == "*") {
        auto t = mul(rhs);
        return UScaledInt(t[0 .. Size]);
    }

    ref UScaledInt opOpAssign(string op)(UScaledInt rhs) pure @nogc nothrow
    if (op == "*") {
        auto t = mul(rhs);
        val[] = t[0 .. $/2];
        return this;
    }
    
    ref UScaledInt opOpAssign(string op, T)(T rhs) pure @nogc nothrow
    if (isIntegral!T && op == "*") {
        auto t = mul(UScaledInt(rhs));
        val[] = t[0 .. $/2];
        return this;
    }
    
    //two's compliment
    UScaledInt opNeg() const pure @nogc nothrow {
        UScaledInt t = this;
        t.val[] ^= Int.max;
        return ++t;
    }

    //same only locally...
    ref UScaledInt applyNeg() pure @nogc nothrow {
        val[] ^= Int.max;
        return ++this;
    }
    
    //merely returns what would be the sign bit
    bool getSign() const pure @safe @nogc nothrow {
        return (val[$-1] & 1 << (Int.sizeof*8-1)) != 0;
    }
    
    version(Intel) {
        auto mul(UScaledInt rhs) const pure @nogc nothrow {
            //UScaledInt!(Bits*2) n;
            Int[Size*2] n;    //raw type, otherwise this breaks (infinite template instantiation).
            mul_asm(n, val[], rhs.val[]);
            return n;
        }
    } else {
        //these 
        auto mul(UScaledInt rhs) const pure @safe @nogc nothrow {
            uint[Size*2] n;
            //need some shifts
            ulong t;
            uint c;
            
            foreach(i, l; rhs.val) {
                if (!l)
                    continue;
                t=c=0;
                foreach(i2, r; val) {
                    t = (cast(ulong)l * r) + c + n[i+i2];
                    n[i+i2] = cast(uint) t;
                    c = t >> 32;
                }
            }
            
            return n;
        }
    }
    
    version(Intel) {
/*  Perhaps the hardest part of this whole thing is the following function. Watched a video on a simple
    arbitrary division using only the most significant digit, which then collapses easily enough with
    div_asm_small where a number of passes are done each getting closer to the goal, taking the difference
    and adjusting the next pass.
    
    One key diference is i shift the divisor and dividend to fill a full 32/64 bit block first, my tests
    showed it went from 8 passes to 2. */
        static void div()(const UScaledInt n, UScaledInt d, ref UScaledInt q, ref UScaledInt r) pure @nogc {
            assert(d, "Divide by zero");
            foreach_reverse(i, divisor; d.val) {
                if (!divisor)
                    continue;
            //find most significant to divide by.
                if (i == 0) {
                    //simple division, no guesswork
                    //call the simpler separated one once
                    r.val[1 .. $] = 0;
                    div_asm_small(n.val[], divisor, q.val[], r.val[0]);
                } else {
                    UScaledInt quotent_t;
                    //recalculating to ensure we actually get double or this might fail. 200+200=400 bits,
                    //on 32bit you'd get 416 while 64 you'd get 448. Might just force everything be multiple of 64.
                    UScaledInt!(Size*2*Int.sizeof*8) mult_temp;      //x2 type because it can and will overflow
                    UScaledInt!(Size*2*Int.sizeof*8) n2 = n.val[];   //upscale because it's easier that way
                    bool dividend_sign = true;
                    int reduceby = bitsUsed(divisor);
                    alias dividend = r;
                    q = UScaledInt();
                    dividend = n;
                    
                    if (reduceby < (Int.sizeof*8)) {
                        --i;
                        divisor = (d>>reduceby).val[i];   //new divisor, should be fully filled
                    }
                    
                    do {
                        if (reduceby < (Int.sizeof*8))
                            dividend >>= reduceby;
                    
                        //divide
                        div_asm_small(dividend.val[i .. $], divisor, quotent_t.val[], mult_temp.val[0]); //remainder is junk

                        //add/sub to our current total
                        if (dividend_sign)
                            q += quotent_t;
                        else
                            q -= quotent_t;

                        //multiply
                        mul_asm(mult_temp.val, q.val[], d.val[], false);
                        
                        //subtract the difference
                        mult_temp -= n2;

                        dividend_sign = mult_temp.getSign();   //quick dirty check
                        
                        if (dividend_sign) {
                            dividend = -cast(UScaledInt) mult_temp.val[0 .. Size];
                        } else
                            dividend = cast(UScaledInt) mult_temp.val[0 .. Size];
                            
                    } while(dividend > d);
                    
                    //handle remainder
                    //handle off by one?
                    if (!dividend_sign) {
                        q -= 1;
                        r = d - dividend;
                    }
                    
                    break;
                }
            }
        }
    } else {
        //backup divide method using shift and subtract
        static void div(UScaledInt lhs, UScaledInt rhs, ref UScaledInt result, out UScaledInt remainder) pure @safe {
            UScaledInt mask = 1;
            result.val[] = 0;
            
            assert(rhs, "Divide by Zero");    //not zero
            
            //grow/shift
            while (!rhs.val[$-1] && lhs > rhs) {
                rhs <<= Int.sizeof*8;
                mask <<= Int.sizeof*8;
            }
            
            while (!rhs.getSign && lhs > rhs) {
                rhs <<= 1;
                mask <<= 1;
            }
            
            while(mask) {
                if (lhs >= rhs) {
                    lhs -= rhs;
                    result |= mask;
                }
                mask >>= 1;
                rhs >>= 1;
            }
            
            remainder = lhs;
        }
    }
    
    UScaledInt opBinary(string op)(const UScaledInt rhs) const pure @nogc
    if (op == "/" || op == "%") {
        UScaledInt q = void, r = void;
        div(this, rhs, q, r);
        static if (op == "/")
            return q;
        else
            return r;
    }

    ref UScaledInt opOpAssign(string op)(const UScaledInt rhs) pure @nogc
    if (op == "/" || op == "%") {
        UScaledInt n = this, r = void;
       
        static if (op == "/")
            div(n, rhs, this, r);
        else
            div(n, rhs, r, this);
        
        return this;
    }

    auto opBinary(string op)(size_t rhs) const pure @nogc
    if (op == "/" || op == "%") {
        UScaledInt q = void;
        size_t r = void;
        div(this.val, rhs, q.val, r);
        static if (op == "/")
            return q;
        else
            return r;
    }
    
    ref UScaledInt opOpAssign(string op)(size_t rhs) pure @nogc
    if (op == "/" || op == "%") {
        UScaledInt n = this;
        size_t r = void;
        
        div(n.val, rhs, this.val, r);
        static if (op == "%") {
            this.val[0] = r;
            this.val[1 .. $] = 0;
        }
        
        return this;
    }

    
    ref UScaledInt opOpAssign(string op)(const UScaledInt rhs) pure @safe @nogc nothrow
    if (op == "&" || op == "|" || op == "^") {
        mixin("val[]"~op~"=rhs.val[];");
        return this;
    }
    
    UScaledInt opBinary(string op)(const UScaledInt rhs) const pure @safe @nogc nothrow
    if (op == "&" || op == "|" || op == "^") {
        UScaledInt t=this;
        
        mixin("t.val[]"~op~"=rhs.val[];");
        return t;
    }
    
    ref UScaledInt opOpAssign(string op)(const int shiftby) pure @nogc nothrow
    if (op == "<<" || op == ">>" || op == ">>>") {
        mixin("this = this"~op~"shiftby;");
        return this;
    }
    
    version(Intel) {
        UScaledInt opBinary(string op)(int shiftby) const pure @nogc nothrow
        if (op == "<<") {
            assert(shiftby >= 0, "negative shift-by");
            assert(shiftby < Bits, "larger number of bits than the number holds, result would be 0");
            
            UScaledInt t;
            int skip = shiftby / (Int.sizeof*8);    //how many whole blocks to move by
            int offs = skip * Int.sizeof;
            shiftby -= skip * Int.sizeof*8;
            
            if(!shiftby) {
                //no complicated shifting or masking needed
                t.val[skip .. $] = val[0 .. $-skip];
            } else {
                Int* p = t.val.ptr;
                enum x = translate("
                        mov @SI, this;
                        mov @DI, p;
                        add @DI, offs;
                        mov @CX, shiftby;
                        xor @DX, @DX;       //prepare mask
                        inc @DX;
                        shl @DX, CL;
                        dec @DX;
                        xor @BX, @BX;       //the flags carry forward
                        mov @CX, Size;
                        sub @CX, skip;      //how many elements we won't have to worry about
                
                start:  push @CX;
                        mov @CX, shiftby;   //only need CL
                        mov @AX, [@SI];
                        rol @AX, CL;        //keep left of shift, right side junk now
                        not @DX;            //what to keep
                        and @AX, @DX;
                        or @AX, @BX;        //carryover bits from previous number
                        mov [@DI], @AX;
                        not @DX;
                        mov @AX, [@SI];
                        rol @AX, CL;
                        mov @BX, @AX;       //get bits for carryover and keep only them
                        and @BX, @DX;
                        add @SI, &;
                        add @DI, &;
                        pop @CX;
                        loop start;");
                
                mixin(x);
            }
            
            return t;
        }
        
        //note, setcarry is intended for being the -1 during shifting
        //the signed component should set that when calling, nothing else.
        UScaledInt opBinary(string op)(int shiftby, Int setcarry=0) const pure @nogc nothrow
        if (op == ">>" || op == ">>>") {
            assert(shiftby >= 0, "negative shift-by");
            assert(shiftby < Bits, "larger number of bits than the number holds, result would be 0, or -1");
            
            if (setcarry) {
                assert(op != ">>>");
                setcarry = Int.max;
            }
            
            UScaledInt t = void;
            int skip = shiftby / (Int.sizeof*8);    //how many whole blocks to move by
            shiftby -= skip * Int.sizeof*8;
            
            //set zero or negative flag status as appropriate, no need to be precise.
            t.val[] = setcarry;
            
            if(!shiftby) {
                //no complicated shifting or masking needed
                t.val[0 .. $-skip] = val[skip .. $];
            } else {
                Int* p = t.val.ptr;
                enum x = translate("
                        mov @SI, this;
                        add @SI, Size*& - &;//last element
                        mov @DI, p;         //p
                        add @DI, Size*& - &;
                        mov @AX, skip;
                        mov @DX, &;
                        mul @DX;            //clears EDX/mask while we're at it
                        sub @DI, @AX;
                        mov @CX, shiftby;
    //                    xor @DX, @DX;       //prepare mask, mul already cleared this
                        inc @DX;
                        shl @DX, CL;
                        dec @DX;
                        ror @DX, CL;        //swap right/left side
                        mov @BX, setcarry;  //either 0 or max
                        and @BX, @DX;       //keep, rather than set all of them accidently.
                        mov @CX, Size;
                        sub @CX, skip;      //how many elements we won't have to worry about
                
                start:  push @CX;
                        mov @CX, shiftby;   //only need CL
                        mov @AX, [@SI];
                        ror @AX, CL;        //keep right of shift, left side junk now
                        not @DX;            //what to keep
                        and @AX, @DX;
                        or @AX, @BX;        //carryover bits from previous number
                        mov [@DI], @AX;
                        not @DX;            //what carries over
                        mov @AX, [@SI];
                        ror @AX, CL;
                        mov @BX, @AX;       //get bits for carryover and keep only them
                        and @BX, @DX;
                        sub @SI, &;
                        sub @DI, &;
                        pop @CX;
                        loop start;");
                
                mixin(x);
            }
            
            return t;
        }
    } else {
        enum uint32 = uint.sizeof * 8;
        enum uint64 = ulong.sizeof * 8;
        //the shifts
        static void lshift(Int[] result, const(Int)[] value, int shiftby) pure @safe @nogc nothrow {
            assert(result.length == value.length);
            int skip = shiftby / uint32;    //how many whole blocks to move by
            shiftby -= skip * uint32;
            ulong t;

            result[0 .. skip] = 0;
            
            if (!shiftby)
                result[skip .. $] = value[0 .. $-skip];
            else
                foreach(i, ref v; result[skip .. $]) {
                    t |= (cast(ulong)value[i]) << shiftby;
                    v = cast(uint) t;
                    t >>= uint32;
                }
        }

        static void rshift(Int[] result, const(Int)[] value, int shiftby, Int setcarry=0) pure @safe @nogc nothrow {
            assert(value.length == result.length);
            int skip = shiftby / uint32;    //how many whole blocks to move by
            shiftby -= skip * uint32;
            int left = uint32 - shiftby;
            ulong t = setcarry ? -1L << (left+uint32) : 0;

            result[$-skip .. $] = setcarry ? -1 : 0;

            if (!shiftby)
                result[0 .. $-skip] = value[skip .. $];
            else
                foreach_reverse(i, ref v; result[0 .. $-skip]) {
                    t |= (cast(ulong)value[i+skip]) << left;
                    v = t >> uint32;
                    t <<= uint32;
                }
        }

        UScaledInt opBinary(string op)(int shiftby) const pure @safe @nogc nothrow
        if (op == "<<") {
            UScaledInt t = void;
            lshift(t.val[], this.val[], shiftby);
            return t;
        }
        
        UScaledInt opBinary(string op)(int shiftby, Int setcarry=0) const pure @safe @nogc nothrow
        if (op == ">>" || op == ">>>") {
            static if (op == ">>>") {
                assert(!setcarry);
            }
            
            UScaledInt t = void;
            rshift(t.val[], this.val[], shiftby, setcarry);
            return t;
        }
    }
    
    UScaledInt opBinary(string op, T)(T rhs) const pure @nogc nothrow
    if (isIntegral!(T) && (op == "+" || op == "-" || op == "*")) {
        mixin("return this"~op~"UScaledInt(rhs);");
    }

    ref UScaledInt opOpAssign(string op, T)(T rhs) pure @nogc nothrow
    if (isIntegral!(T) && (op == "+" || op == "-" )) {
        mixin("this"~op~"=UScaledInt(rhs);");
        return this;
    }
    
    UScaledInt opUnary(string op)() const pure @safe @nogc nothrow
    if (op == "~" || op == "-" ) {
        static if (op == "~") {
            UScaledInt t = void;
            t.val[] = val[] ^ -1;
        }
        static if (op == "-") {
            UScaledInt t = val.opNeg;
        }
        
        return t;
    }
    
    T opCast(T)()
    if (isIntegral!T || isSomeChar!T) {
        return (cast(T[])(cast(void[])val)[0 .. T.sizeof])[0];
    }
    
    bool opCast(T)()
    if (is(T == bool)) {
        return this != UScaledInt.init;
    }
    
    T opCast(T)()
    if (is(T == ScaledInt!Bits)) {
        ScaledInt!Bits t;
        t.val = this.val;
        return t;
    }
    
    /*
    //forcibly up/downcast
    T opCast(T)()
    if (is(TemplateOf!T == UScaledInt)) {
        static if (T.Size > Size) {
            //upcast, need more memory for it
            return T(val);
        } else {
            //downcast, just use what's there...
            return cast(UScaledInt!(T.Size)) this.val[0 .. T.Size];
        }
    }
    */
    
    /*creates a string of base10 to represent the number
      seeing as division is expensive, it's far faster to grab a bunch of digits and use a cheaper divides
      every 32bit block can hold 9 digits and 64bit can hold 18 digits, makes a mighty simple algorighm then*/
    string toString(bool honorSign=false) const pure {
        version(Intel) {
            //optimize for div_asm_small
            version(X86) {
                enum Digits = 9;
            }
            version(X86_64) {
                enum Digits = 18;
            }
            enum DigitsMod = 10L^^Digits;
            Int tmod;
        } else {
            //can't optimize, but since we have longs, may as well use them
            enum Digits = 18;
            enum DigitsMod = UScaledInt([cast(uint) (10L^^Digits), (10L^^Digits)>>32]);
            ulong tmod;
        }
        
        UScaledInt tmp = this;
        if (honorSign && getSign)
            tmp.applyNeg;
        
        char[Digits * (val.sizeof+1)] str;
        foreach_reverse(i, ref ch; str) {
            if ((i+1) % Digits == 0) {
                //need both division and mod at the same time, so we'll use one function call
                UScaledInt t = void;
                version(Intel) {
                    div_asm_small(tmp.val, DigitsMod, t.val, tmod);
                } else {
                    UScaledInt rem = void;
                    
                    div(tmp, DigitsMod, t, rem);
                    tmod = (cast(ulong) rem.val[1] << 32) | rem.val[0];
                }
                tmp = t;
            }
            ch = cast(char)('0' + (tmod%10));
            tmod /= 10;
            if (!tmod && !tmp) {
                //if signed, prepend that
                if (honorSign && getSign) {
                    --i;
                    str[i] = '-';
                }
                
            
                return str[i .. $].dup;
            }
        }

        assert(false);  //shouldn't get here.
    }
}

struct ScaledInt(int Bits)
if (Bits > 1) {    //the 128 limit due to loop opcode
//getting a failure due to asm precense in code...
//    enum min = UScaledInt!Bits(1)<<(Bits-1);
//    enum max = min + 1;
    enum Size = calcSizeFromBits(Bits);
    private UScaledInt!Bits _val;
    alias _val this;

   //force a larger type if needed
    this(T)(T i) pure @nogc nothrow 
    if (isIntegral!T) {
        _val = UScaledInt!Bits(i);
    }

    this(string i) @nogc nothrow {
        _val = UScaledInt!Bits(i);
    }
    
    /*
    this(const void[] i) pure @nogc nothrow {
        _val = UScaledInt!Bits(i);
    }
    */

    bool opEquals(T)(T rhs) const @safe pure
    if (isIntegral!T) {
        return _val.opEquals(ScaledInt!Bits(rhs));
    }
    
    bool opEquals(UScaledInt!Bits rhs) const @safe pure {
        //either highest bit is on, they can't be equal
        if (_val.getSign() || rhs.getSign())
            return false;
        
        return _val.val[] == rhs.val[];
    }
    
    bool opEquals(ScaledInt rhs) const @safe pure {
        return _val.val[] == rhs._val.val[];
    }
    
    int opCmp(T)(T rhs) const @safe pure
    if (isIntegral!T) {
        return opCmp(ScaledInt(rhs));
    }
    
    int opCmp(const UScaledInt!Bits rhs) const @safe pure {
        if (_val.getSign())
            return -1;
        
        return _val.opCmp(rhs);
    }
    
    int opCmp(const ScaledInt rhs) const @safe pure {
        int signs = (_val.getSign ? 2 : 0) | (rhs.getSign ? 1 : 0);
        
        switch(signs) {
            case 3:// return rhs._val.opCmp(_val);
            case 0: return _val.opCmp(rhs._val);
            case 1: return 1;
            case 2: return -1;
            
            default:
                assert(false);
        }
    }
    
    // >>
    ScaledInt opBinary(string op)(int shiftby) const pure
    if (op == ">>") {
        return cast(ScaledInt!Bits) _val.opBinary!">>"(shiftby, -1);
    }
    
    ref ScaledInt opOpAssign(string op)(int shiftby) const pure
    if (op == ">>") {
        _val.opOpAssign!">>"(shiftby, -1);
        return this;
    }
    
    // *, %, /
    ScaledInt opBinary(string op, T)(T rhs) const pure
    if ((op == "*" || op == "/" || op == "%") &&
            (is(T == ScaledInt!Bits) || is(T == UScaledInt!Bits))) {
        ScaledInt t = this;
        mixin("t"~op~"=rhs;");
        return t;
    }
    
    ref ScaledInt opOpAssign(string op, T)(T rhs) pure
    if ((op == "*" || op == "/" || op == "%") &&
            (is(T == ScaledInt) || is(T == UScaledInt!Bits))) {
        static if (is(T == ScaledInt)) {
            int signs = (getSign ? 2 : 0) | (rhs.getSign ? 1 : 0);
        } else {
            int signs = (getSign ? 2 : 0);
        }
        
        if (signs & 2)
            applyNeg();

        static if (is(T == ScaledInt!Bits)) {
            if (signs & 1)
                rhs.applyNeg();
            mixin("_val "~op~"= rhs._val;");
        } else {
            mixin("_val "~op~"= rhs;");
        }
        
        switch(signs) {
            //if mixed result is negative
            case 1:
            case 2: applyNeg();
                    goto case 3;    //yes fallthrough is desired
            //if both are signed/unsigned then it's positive
            case 3:
            case 0: break;
            
            default:
                assert(false);
        }
        
        return this;
    }
    
    //forward functions while keeping type for return
    ref ScaledInt opOpAssign(string op, T)(T rhs) pure
    if ((op == "&" || op == "|" || op == "^" || op == "<<") && 
        (is(T == ScaledInt) || is(T == UScaledInt!Bits) || is(T==int))) {
        mixin("_val"~op~"=rhs;");
        return this;
    }
    
    ScaledInt opBinary(string op, T)(T rhs) const pure
    if ((op == "&" || op == "|" || op == "^" || op == "<<") && 
        (is(T == ScaledInt) || is(T == UScaledInt!Bits) || is(T==int))) {
        ScaledInt tmp;
        mixin("tmp._val = this._val"~op~"rhs;");
        return tmp;
    }
    
    string toString() const pure {
        return _val.toString(true);
    }
}

unittest {
    assert(cast(ulong[])UCent([0xFEDCBA9876543210L, 0x89ABCDEF01234567L]).val == [0xFEDCBA9876543210L, 0x89ABCDEF01234567L]);
    assert(cast(ulong[])UCent([0x76543210, 0xFEDCBA98, 0x01234567, 0x89ABCDEF]).val == [0xFEDCBA9876543210L, 0x89ABCDEF01234567L]);
    assert(UCent(cast(int)-1) == UCent([-1L, -1L]));
    
    uint[4] x = [-1, 0, 0, 0];
    assert(UCent(cast(uint)-1) == UCent(x));
    
    x[1] = -1;
    assert(UCent(cast(ulong)-1) == UCent(x));
}

//opcmp / opequal
unittest {
    UCent l=1, l2=2, g = UCent([0,1]);
    
    //l & l2 at the same level, g on a different 'greater' level.
    //only test first two levels, on a 64bit machine there might only be 2 levels.
    
    //opEquals
    assert(l == l);
    assert(l2 == l2);
    assert(g == g);
    assert(g != l);
    assert(l != g);
    assert(l != l2);
    assert(l2 != g);
    
    assert(l < g);
    assert(g > l);
    assert(l < l2);
    assert(l2 > l);
    assert(l2 < g);
    assert(g > l2);
    
    assert(l >= l);
    assert(g <= g);
    
    //size_t
    assert(l == 1);
    assert(l2 == 2);
    assert(l2 != 1);
    assert(l != 2);

    assert(g != 1);
    assert(g != 2);
    
    assert(l2 > 1);
    assert(l < 2);
    assert(g > uint.max);
}

//need unittests for specific sizes/types otherwise testing this will likely fail for the wrong reasons later
//  ++/--
unittest {
    UCent zero, x, neg1, one, t;
    
    neg1.val[] = Int.max;
    one.val[0] = 1;
    
    --x;
    assert(x == neg1);
    ++x;
    assert(x == zero);
    ++x;
    assert(x == one);
    --x;
    assert(x == zero);
    
    x.val[0] = Int.max;
    t.val[1] = 1;
    x = UCent(Int.max);
    ++x;
    assert(x == t);
    t.val[0] = Int.max;
    t.val[1] = 0;
    --x;
    assert(x.val[0] == Int.max);
    assert(x.val[1] == 0);
}

//  +/-
unittest {
    alias Int = uint;
    UCent x256 = 256, x300=300, x556=556, zero, neg1, maxfirst=Int.max, t;
    --neg1;
    maxfirst.val[0] = Int.max;
    
    //+, no carry
    assert(x556 == x256+x300);
    assert(x556 == x256+300);
    assert(UCent(256+300) == x256+x300);
    
    t = x256;
    t += x300;
    assert(t == x556);
    
    //carry
    assert(UCent([299,1]) == maxfirst + x300);
    assert(UCent([299,1]) == maxfirst + 300);
    
    //carry from neg (to positive)
    assert(UCent(255) == neg1 + x256);
    assert(UCent(255) == neg1 + 256);
    
    //negative tests
    //no carry
    
    assert(UCent(44) == x300-x256);
    assert(UCent(44) == x300-256);
    t = x300;
    t -= x256;
    assert(t == UCent(44));
    
    t = x300;
    t -= 256;
    assert(t == UCent(44));
    
    //carry
    t = UCent([256, 1]);
    t -= 300;
    
    assert(t == UCent(Int.max - 43));

    //carry from pos (to neg)
    t = neg1 - 43;
    assert(t == UCent(256)-300);
}

//  <<, >>, >>>
unittest {
    UCent   orig =  UCent([0xFEDCBA9876543210L, 0x89ABCDEF01234567L]),
            sh4l =  UCent([0xEDCBA98765432100L, 0x9ABCDEF01234567FL]),
            sh64l = UCent([0L, 0xFEDCBA9876543210L]),
            sh68l = UCent([0L, 0xEDCBA98765432100L]),
            sh4r =  UCent([0x7FEDCBA987654321L, 0x089ABCDEF0123456L]),
            sh64r = UCent([0x89ABCDEF01234567L, 0L]),
            sh68r = UCent([0x089ABCDEF0123456L, 0L]),
            sh4r_b = UCent([0x7FEDCBA987654321L, 0xF89ABCDEF0123456L]),
            sh64r_b = UCent([0x89ABCDEF01234567L, 0xFFFFFFFFFFFFFFFFL]),
            sh68r_b = UCent([0xF89ABCDEF0123456L, 0xFFFFFFFFFFFFFFFFL]);
    
    assert(orig << 4 == sh4l);  //simple shift
    assert(orig << 64 == sh64l);//fixed no asm
    assert(orig << 68 == sh68l);//skip & shift
    
    assert(orig >> 4 == sh4r);
    assert(orig >> 64 == sh64r);
    assert(orig >> 68 == sh68r);

    assert(orig >>> 4 == sh4r); //>> and >>> the same
    assert(orig >>> 64 == sh64r);
    assert(orig >>> 68 == sh68r);

    //carry flag added intentionally (for signed shift)
    assert(orig.opBinary!">>"(4, 1) == sh4r_b);
    assert(orig.opBinary!">>"(64, 100) == sh64r_b);
    assert(orig.opBinary!">>"(68, 0xffffffff) == sh68r_b);
}

// & | ^
unittest {
    UCent orig = UCent([0xFEDCBA987654321UL, 0xFEDCBA987654321UL]),
           rhs = UCent([0x123456789ABCDEFL, 0x55555555_55555555L]),
           expected;
    Cent corig = cast(Cent) orig,
         crhs = cast(Cent) rhs;
//           cexpected;
    
    expected.val[] = orig.val[] & rhs.val[];
//    cexpected._val.val[] = expected.val[];
    assert((orig & rhs) == expected);
    assert((corig & rhs) == expected);
    assert((orig & crhs) == expected);
    assert((corig & crhs) == expected);

    expected.val[] = orig.val[] | rhs.val[];
    assert((orig | rhs) == expected);
    assert((corig | rhs) == expected);
    assert((orig | crhs) == expected);
    assert((corig | crhs) == expected);

    expected.val[] = orig.val[] ^ rhs.val[];
    assert((orig ^ rhs) == expected);
    assert((corig ^ rhs) == expected);
    assert((orig ^ crhs) == expected);
    assert((corig ^ crhs) == expected);
    
}

//  *
unittest {
    UCent orig = UCent(0xFEDCBA987654321UL);
    UCent[16] vals;
    ulong x2;
    
    foreach(i; 0 .. 16) {
        ulong x = i;
        x <<= 4*i;
        x2 |= x;
        
        vals[i] = orig * x;
        UCent t, t2;
        t = orig;
        t <<= 4*i;
        foreach(i2; 0 .. i) {
            t2 += t;
        }
        
        assert(t2 == vals[i]);  //check multiplication worked, remember 4bit x 128bit
    }

    UCent t;
    foreach(i; vals)
        t += i;
    
    assert(t == orig*x2);
    
    version(Intel) {
        UScaledInt!256 x256, x256_b, x256_c;
        
        //do a 128*128. Low will always be 1, high will always be F's, except the lowest of the high bits.
        //alternatively will be off from a full max  by a small margin.
        t.val[] = -1;
            
        mul_asm(x256.val, t.val, t.val, false);
        
        x256_b.val[]=-1;
        x256_c.val[0 .. $/2] = -1;
        
        x256_b -= x256_c;
        x256_b -= x256_c;
        
        /*alright, x256 & x256_b should be idential now.
          Here's the logic behind it:
          ff*ff = fe01
            +ff = ff00
            +ff = ffff
        */
        assert(x256 == x256_b);
        
        //check fast, only lower half matters.
        mul_asm(x256.val, t.val, t.val, true);

        assert(x256.val[0 .. $/2] == x256_b.val[0 .. $/2]);
        assert(x256 != x256_b); //upper half not calculated so it obviously doesn't match
        
        //quick exit with no length
        x256_b.val[]=0;

        x256.val[]=-1;
        mul_asm(x256.val, t.val, null);
        assert(x256 == x256_b);
        x256.val[]=-1;
        mul_asm(x256.val, null, t.val);
        assert(x256 == x256_b);
    }
    
    //check opassign
    t = UCent(255);
    t *= t;
    assert(t == UCent(255*255));
    t *= 100;
    assert(t == UCent(255*255*100));
}

// this(string)
unittest {
    //with multiply & add working, string assignment will work.
    
    //can't seem to get opEqual to work with these...
    assert(UCent("81985529216486895") == 81985529216486895L);
    assert(UCent("81985529216486895") == UCent([0x123456789ABCDEFL]));
    assert(UCent("-81985529216486895") == (UCent() - 81985529216486895));
    assert(UCent("-81985529216486895") == -81985529216486895L);
}

// neg, applyNeg
unittest {
    //should be 2's compliment, so...
    UCent x;
    
    x -= 100;
    x.applyNeg();
    assert(x == UCent(100));
    x = UCent(0);
    x.applyNeg();
    assert(x == UCent());

    assert(-UCent( 100) == UCent(-100));
    assert(-UCent(-100) == UCent( 100));
}

// getSign
unittest {
    UCent x = -1;
    
    assert(x.getSign);
    x.val[$-1] ^= 1<<(Int.sizeof*8-1);
    assert(!x.getSign);
    assert(x == UCent([0xffffffff_ffffffffL, 0x7fffffff_ffffffffL]));
}

// tostring
unittest {
    UCent t = -1;
    t >>= 1;
    
    assert(t.toString() == "170141183460469231731687303715884105727");
    assert((-t).toString(true) == "-170141183460469231731687303715884105727");
    assert(UCent().toString() == "0");
}

//  /
unittest {
    //division, the one part that sucks more than the rest. Oh well.
    //the simple division comes first. Simplest would have a test of a string to see what we expect.
    static T factorial(T = UCent)(int n) {
        T value = 1;
        foreach(i; 2 .. n+1)
            value *= i;
        
        return value;
    }
    
    //calculate factorial 34
    //https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic
    UCent fact34 = factorial(34);
    UCent fact21 = factorial(21);
    
    //values confirmed using bc
    assert(fact21.toString == "51090942171709440000");
    assert(fact34.toString == "295232799039604140847618609643520000000", fact34.toString);
    
    //for one level/size_t type that's fine. But now we need to go a bit bigger. At lest a 65bit number to ensure we're working
    //good even on 64bit machines.
    
    ++fact21;   //so we force a remainder.
    assert((fact34 / fact21).toString == "5778574175582207999");
    assert((fact34 % fact21).toString == "45312367996127232001");
    
    //unique case for div_asm_small never gets called, need to ensure it does when a single small value is present.
    
    UCent sm = 31415;
    assert((fact21 / sm).toString == "1626323163193042");
    assert((fact21 % sm).toString == "25571");
}

//signed specific tests
//assignments
unittest {
    assert(Cent(100).toString == "100");
    assert(Cent(-100).toString == "-100");
    assert(Cent("-100").toString == "-100");
//    assert(Cent([100,0]).toString == "100");  //not sure
}

//opcmp & opEquals
unittest {
    UCent upos = 10, upos2 = 200, uneg = -100;
    Cent  spos = 100, spos2 = 1000,
          sneg = -100, sneg2 = -1000;
    
    //check immediates
    assert(spos < 1000);
    assert(sneg2 > -2000);
    assert(spos == 100);
    assert(sneg == -100);
    //non-mixed Cents
    assert(spos < spos2);
    assert(sneg > sneg2);
    assert(sneg < spos);
    assert(spos == spos);
    assert(spos != spos2);
    
    //mixed same signs
    assert(spos > upos);
    assert(spos < upos2);
    assert(spos != upos);
    assert(spos == UCent(100));
    
    //mixed with neg/high
    assert(sneg < upos);
    assert(sneg < uneg);
    assert(spos < uneg);
    assert(sneg != uneg);
    assert(sneg != upos);
    
    //rhs neg left positive hasn't been tested yet
    assert(spos > sneg);
}

//ensure >> works right
unittest {
    Cent v = -100;
    
    assert(v == -100);
    assert(v>>3 == -13);
    //all other tests already done earlier
}


// * & /
unittest {
    ScaledInt!160 fact21 = "51090942171709440000",
         fact34 = "295232799039604140847618609643520000000";
    UScaledInt!160 ufact21 = "51090942171709440000";
    
    ++fact21;   //so we force a remainder.
    ++ufact21;
    
    assert((fact34 / fact21).toString == "5778574175582207999");
    assert((fact34 % fact21).toString == "45312367996127232001");
    
    fact21.applyNeg;
    
    assert((fact34 / fact21).toString == "-5778574175582207999");
    assert((fact34 % fact21).toString == "-45312367996127232001");
    
    fact34.applyNeg;
    
    assert((fact34 / fact21).toString == "5778574175582207999");
    assert((fact34 % fact21).toString == "45312367996127232001");

    fact21.applyNeg;

    assert((fact34 / fact21).toString == "-5778574175582207999");
    assert((fact34 % fact21).toString == "-45312367996127232001");
    

    //make sure mixed signs work.
    assert((fact34 / ufact21).toString == "-5778574175582207999");
    assert((fact34 % ufact21).toString == "-45312367996127232001");
    
    fact34.applyNeg;
    
    assert((fact34 / ufact21).toString == "5778574175582207999");
    assert((fact34 % ufact21).toString == "45312367996127232001");
    
    ufact21.applyNeg;   //unsigned still positive, so it will be way too big.

    assert(ufact21 > fact34);
    assert((fact34 / ufact21).toString == "0");
    assert((fact34 % ufact21) == fact34);
}

debug {
    unittest {
        int bitcount(Int[] v) {
            int total = v.length * Int.sizeof * 8;
            
            while (!v[$-1]) {
                v = v[0 .. $-1];
                total -= Int.sizeof * 8;
            }
            
            total -= Int.sizeof * 8;
            Int tmp = v[$-1];
            while (tmp) {
                ++total;
                tmp >>= 1;
            }
            
            return total;
        }
    
        writeln("Time to show off! Factorial 1-98!\tFact!N\tbits\tResults");
        UScaledInt!512 f = 1;
        foreach(i; 1 .. 99) {
            f *= i;
            writefln("%s\t%s\t%s", i, bitcount(f.val), f);
        }
    }
}