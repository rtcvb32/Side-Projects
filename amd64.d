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

    //multiply. length of result has to be as big as the lhs+rhs lengths.
    //faster means be less precise, and only return the length that the lhs gives, at which point
    //the lhs has to be as big if not bigger than the rhs.
    ulong[] mul(ulong[] res, const(ulong)[] lhs, const(ulong)[] rhs, bool faster = true) pure @nogc nothrow {
        assert(isUnsigned!ulong);
        assert(res.length >= lhs.length + rhs.length);

        res[0 .. (faster ? lhs.length : $)] = 0;
        
        if (!__ctfe && UseAsm) {
            version(D_InlineAsm_X86_64) {
                foreach(i, rhs_v; cast(ulong[]) rhs) { //cast only, otherwise: integer constant expression expected instead
                    if (!rhs_v)
                        continue;
                    //s & p pointers to get proper location
                    const(ulong)* s = &lhs[0];
                    auto p = &res[i];
                    long cnt = faster ? (lhs.length - i) : lhs.length;
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
        
        return cast(ulong[]) mul32(cast(uint[]) res, cast(const(uint)[]) lhs, cast(const(uint)[]) rhs, faster);
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
            ulong[4] m;
            
            assert(mul(m[],[0x123456789abcdef0L, 0x123456789abcdef0L], [0x1122334455667788L], false)
                        == [0xC5E365068397FF80L, 0xC71B4D5D4B16C050L, 0x137E856C77EC0D0L, 0]);
            assert(mul(m[],[0x1122334455667788L], [0x123456789abcdef0L, 0x123456789abcdef0L], false)
                        == [0xC5E365068397FF80L, 0xC71B4D5D4B16C050L, 0x137E856C77EC0D0L, 0]);
            assert(mul(m[],[0x123456789abcdef0L, 0x123456789abcdef0L], [0x123456789abcdef0L, 0x123456789abcdef0L], false)
                        == [0xA5E20890F2A52100L, 0x4D0F77FE1940EEDCL, 0xA878D6495A927AB9L, 0x14B66DC33F6ACDCL]);
            assert(mul(m[],[0x123456789abcdef0L, 0x123456789abcdef0L], [0x123456789abcdef0L, 0x123456789abcdef0L])
                        == [0xA5E20890F2A52100L, 0x4D0F77FE1940EEDCL]);
        }
    }

    //modular division using native types, part of the larger division code needed.
    //stores the quotient in the result, and returns the remainder
    ulong div_small(ulong[] buff, const(ulong)[] n, ulong d, ulong[] result) pure nothrow @nogc {
        assert(n.length <= result.length);
        
        if (__ctfe || !UseAsm) {
            if (d <= uint.max)
                return div32_small(null, cast(const(uint)[]) n, cast(uint) d, cast(uint[]) result);
            else {
                assert(buff.length >= n.length*4);
                assert(result.length == n.length);
                ulong[1] _d = d;
                div32(cast(uint[]) buff[n.length .. $], cast(const(uint)[]) n, cast(const(uint)[]) _d, cast(uint[]) result, cast(uint[]) buff[0 .. n.length]);
                return buff[0];
            }
        }
        
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

        uint[4] n = [0, 0x445DA75B, 0x9EFCAC82, 0xDE1BC4D1], q;
        ulong[] reml = [295, 232_79903_96041_40847L, 618_60964_35200_00000L];
        ulong[][] q_res = [
            [0L, 0],
            [0x127L, 0],
            [0x12D9A841FE42B2FL, 0x10],
        ];
        
        //18 digits version, more or less the same
        uint[n.length * 4] buff;
        foreach_reverse(i, r; reml) {
            assert(r == div_small(cast(ulong[]) buff, cast(ulong[]) n, 1000_00000_00000_00000L, cast(ulong[]) q));
            assert(cmp(cast(ulong[]) q, q_res[i]) == 0);
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
    void div(ulong[] buff, const(ulong)[] n, const(ulong)[] d, ulong[] q, ulong[] r) pure @nogc nothrow {
        assert(d, "Divide by zero");
        
        if(__ctfe || !UseAsm) {
            //generic version will deal with ctfe
            div32(cast(uint[]) buff, cast(const(uint)[]) n, cast(uint[]) d, cast(uint[]) q, cast(uint[]) r);
            return;
        }
        
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

    unittest {
        ulong[6] buff;
        ulong[2]  fact13p1=[0x17328CC01L, 0],
                fact21 = [0xC5077D36B8C40000L, 2],
                fact28 = [0xAD2CD59DAE000000L, 0x3D925BA47L],
                fact34 = [0x445DA75B00000000L, 0xDE1BC4D19EFCAC82L],
                q, r;
        
    //        5778574175582208000 & 0
        //even result
        div(buff, fact34, fact21, q, r);
        assert(q == [0x50319E98B3D2C000L, 0]);
        assert(r == [0, 0]);
        
        //force remainder, fact21+1
        inc(fact21);
    //        5778574175582207999 & 45312367996127232001
        div(buff, fact34, fact21, q, r);
        assert(q == [0x50319E98B3D2BFFFL, 0]);
        assert(r == [0x74D5DE9E04F14001L, 2]);

        //test with at least one negative interation calculation
        div(buff, fact28, fact13p1, q, r);
        assert(q == [0xA77C82A7F1BCE021L, 2]);
        assert(r == [0x6180D3DFL, 0]);

        //test forward to div_small, small enough divisor
        ulong[2] sm = [123456789, 0];
        div(buff, fact21, sm, q, r);
        assert(q == [0x605A95ECDCL, 0]);
        assert(r == [0x59765F5, 0]);
    }
} else {
    static assert(false, "This shouldn't be included, you aren't using x86_64 code");
}
