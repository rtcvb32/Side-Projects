/**
 * x86_64 specific extentions for ArbitaryInt. Use gen32 for generic all purpose code
 *
 * Authors:
 *    Era Scarecrow <rtcvb32@yahoo.com>
 */
module std.experimental.arbitraryint.amd64;

import std.traits : isIntegral, isSigned, isUnsigned;
import std.format : FormatSpec;
import std.experimental.arbitraryint.gen32;

alias mul32 = std.experimental.arbitraryint.gen32.mul;
alias div32 = std.experimental.arbitraryint.gen32.div;
alias div32_small = std.experimental.arbitraryint.gen32.div_small;

/*  Internal functions, unlimited add/sub/inc/dec/rshift/lshift/cmp/icmp/div_small_div/mul on one or two arrays

    For simplicity, it's little endian in structure. So [0] is the lowest value and [$-1] has the highest value.
    All functions use array slices.
*/
//pragma causing LDC to barf, "can't be always inline & not inline at the same time".
version(LDC) {} else {
    pragma(inline, true):
}

version(D_InlineAsm_X86_64) {
    //internal type, should be half the size of the largest type we can work with
    //so uint if we can work with longs, and longs if we can work with cent
    alias Int = ulong;

    enum IntBits = Int.sizeof*8;

    //forcibly turns off ASM coding.
    debug(NoAsm) {
        enum UseAsm = false;
    } else {
        enum UseAsm = true;
    }

    //like cmp, only reduces both signed flags before considering comparing.
    ptrdiff_t icmp(const(Int)[] lhs, const(Int)[] rhs) pure @safe nothrow @nogc {
        size_t signFlags = (getSign(lhs) ? 2 : 0) | (getSign(rhs) ? 1 : 0);

        if (signFlags == 2) return -1;
        if (signFlags == 1) return 1;
        
        signFlags = signFlags ? -1 : 0;
        //reduce excessive sign flag (0 / -1)
        lhs = reduceArray(lhs, signFlags);
        rhs = reduceArray(rhs, signFlags);
        
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
        lhs = reduceArray(lhs);
        rhs = reduceArray(rhs);
        
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
        assert(icmp([0, 0, Int.max], [-1])  < 0);
        assert(icmp([-1], [0, 0, Int.max])  > 0);
    }

    //multiply. length of result has to be as big as the lhs+rhs lengths.
    //faster means be less precise, and only return the length that the lhs gives, at which point
    //the lhs has to be as big if not bigger than the rhs.
    Int[] mul(Int[] res, const(Int)[] lhs, const(Int)[] rhs, bool faster = true) pure @nogc nothrow {
        assert(isUnsigned!Int);
        assert(res.length >= lhs.length + rhs.length);

        res[0 .. (faster ? lhs.length : $)] = 0;
        
        if (!__ctfe && UseAsm) {
            version(D_InlineAsm_X86_64) {
                ulong[1] extended = rhs[0];
                if (rhs.length == 1)
                    rhs = cast(Int[]) extended;

                foreach(i, rhs_v; cast(ulong[]) rhs) { //cast only, otherwise: integer constant expression expected instead
                    if (!rhs_v)
                        continue;
                    //s & p pointers to get proper location
                    const(Int)* s = &lhs[0];
                    auto p = &res[i+i];
                    long cnt = faster ? (lhs.length + 1 - i ) / 2 : lhs.length / 2;
                    if (cnt <= 0)
                        break;

                    asm pure nothrow @nogc {
                            mov RSI, s;
                            mov RCX, cnt;
                            mov RDI, p;
                            
                            mov R9, qword ptr rhs_v;       //load multiplier
                    start:  mov RAX, qword ptr [RSI];
                            mul R9;
                    //RAX:RDX has the result
                            add qword ptr [RDI], RAX;
                            adc qword ptr [RDI + 8], RDX;
                    //manage carry, if applicable
                            jnc nocarry;
                            mov RAX, RDI;
                    carry:  add RDI, 8;
                            add qword ptr [RDI + 16], 1;
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
        IntUL val;
        Int c;

        foreach(i, r; rhs)
        if (r) {
            val = c = 0;
            foreach(i2, IntUL l; faster ? lhs[0 .. $-i] : lhs)
            if (l || c) {
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
        bool longOnly = false;
        
        version(D_InlineAsm_X86_64) {
            longOnly = true;
        }
        
        if (!longOnly) {
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
        } else {
            //64-only tests
            Int[8] m;

            assert(mul(m[],[0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678], [0x11223344, 0x55667788], false) ==
                                    [0x467507C0, 0x45041640, 0xC71B4D5D, 0x4B16C050, 0x80A6459D, 0x612AA10, 0, 0]);
            assert(mul(m[],[0x11223344, 0x55667788], [0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678], false) ==
                                    [0x467507C0, 0x45041640, 0xC71B4D5D, 0x4B16C050, 0x80A6459D, 0x612AA10, 0, 0]);
            assert(mul(m[],[0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678],[0x11223344, 0x55667788, 0x99aabbcc, 0xddeeff00], false) ==
                                    [0x467507C0, 0x45041640, 0x87D6449D, 0x1DB8741D, 0xF025B2D6, 0xE87C8B4D, 0xAEC475F8, 0xFC82D70]);
            assert(mul(m[],[0x9abcdef0, 0x12345678, 0x9abcdef0, 0x12345678],[0x11223344, 0x55667788, 0x99aabbcc, 0xddeeff00]) ==
                                    [0x467507C0, 0x45041640, 0x87D6449D, 0x1DB8741D]);
        }
    }

    //modular division using native types, part of the larger division code needed.
    //stores the quotient in the result, and returns the remainder
    ulong div_small(ulong[] buff, const(ulong)[] n, ulong d, ulong[] result) pure nothrow @nogc {
        assert(n.length <= result.length);
        
        n = reduceArray(n);
        
        result[n.length .. $] = 0; //zeroize, can't possibly be anything there.
        result = result[0 .. n.length];
        
        auto dividend = &n[$-1];
        auto quotent = &result[$-1];
        ulong len = n.length;
        ulong remainder;
        
        //due to byte order and compatible using uint, gotta load/save 32bit at a time.
        asm pure nothrow @nogc {
                mov RSI, dividend;
                mov RDI, quotent;
                
                xor RDX, RDX;
                mov RBX, d;     //get divisor
                
                xor RDX, RDX;   //remainder of last divide
                mov RCX, len;   //counter for loop
        start:  mov RAX, qword ptr [RSI];
                div RBX;
                mov qword ptr [RDI], RAX;  //save low
                
                sub RSI, 8;
                sub RDI, 8;
                loop start;
                mov remainder, RDX;     //save remainder.
        }

        return remainder;
    }

    unittest {
        //fact34: 295 232799039 604140847 618609643 520000000

        n = [0, 0x445DA75B, 0x9EFCAC82, 0xDE1BC4D1];
        ulong[] reml = [295, 232_79903_96041_40847L, 618_60964_35200_00000L];
        q_res = [
            cast(Int[])[0],
            cast(Int[])[0x127, 0, 0, 0],
            cast(Int[])[0x1FE42B2F, 0x12D9A84, 0x10, 0],
        ];
        
        //18 digits version, more or less the same
        foreach_reverse(i, r; reml) {
            assert(r == div_small(cast(ulong[]) n, 1000_00000_00000_00000L, cast(ulong[]) q));
            assert(cmp(q[], q_res[i]) == 0);
            n[] = q[];
        }
    }

    /*  Perhaps the hardest part of this whole thing is the following function. Watched a video on a simple
    arbitrary division using only the most significant digit, which then collapses easily enough with
    div_asm_small where a number of passes are done each getting closer to the goal, taking the difference
    and adjusting the next pass.

    One key diference is i shift the divisor and dividend to fill a full 32/64 bit block first, my tests
    showed it went from 8 passes to 2.

    the buff must be at least 3 times larger than the n value, for temporaries. This avoids the gc
    */
    void div(Int[] buff, const(Int)[] n, const(Int)[] d, Int[] q, Int[] r) pure @nogc nothrow {
        assert(d, "Divide by zero");

        //reduction for n, q & r
        n = reduceArray(n);

        //zeroize never reached area and shorten
        q[n.length .. $] = 0;
        r[n.length .. $] = 0;
        q = q[0 .. n.length];
        r = r[0 .. n.length];
        
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
                Int[] mult_temp = buff[n.length .. (n.length + q.length + d.length)];
                bool dividend_sign = true;
                size_t reduceby = bitsUsed(divisor), Size = n.length;
                alias dividend = r;
                dividend[] = n[];
                q[] = 0;
                
                //removes custom reduceby, for speed testing
                debug(NoDivReduce) { reduceby = 0; }
                
                //new divisor, should be fully filled
                if (reduceby)
                    divisor = rshift(mult_temp[0 .. d.length], d, reduceby)[--i];
                    
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

    /+
    //
    void div64(ulong[] buff, const(ulong)[] n, const(ulong)[] d, ulong[] q, ulong[] r) pure @nogc nothrow {
        assert(d, "Divide by zero");

        //reduction for n, q & r
        n = reduceArray(n);

        //zeroize never reached area and shorten
        q[n.length .. $] = 0;
        r[n.length .. $] = 0;
        q = q[0 .. n.length];
        r = r[0 .. n.length];
        
        foreach_reverse(i, ulong divisor; d) {
            if (!divisor)
                continue;
        //find most significant to divide by.
            if (i == 0) {
                //simple division, no guesswork
                //call the simpler separated one once
                r[1 .. $] = 0;
                r[0] = div_small(null, n, divisor, q);
            } else {
                ulong[] quotent_t = buff[0 .. n.length];
                ulong[] mult_temp = buff[n.length .. (n.length + q.length + d.length)];
                bool dividend_sign = true;
                size_t reduceby = bitsUsed(divisor), Size = n.length;
                alias dividend = r;
                dividend[] = n[];
                q[] = 0;
                
                //removes custom reduceby, for speed testing
                debug(NoDivReduce) { reduceby = 0; }
                
                //new divisor, should be fully filled
                if (reduceby)
                    divisor = rshift(mult_temp[0 .. d.length], d, reduceby)[--i];
                    
                do {
                    if (reduceby)
                        rshift(dividend, dividend, reduceby);
                
                    //divide
                    div_small(null, dividend[i .. $], divisor, quotent_t); //remainder is junk

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
    +/

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
} else {
    static assert(false, "This shouldn't be included, you aren't using x86_64 code");
}