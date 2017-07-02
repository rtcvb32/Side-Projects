/**
 * generic functions and X86 32bit specific asm data for an ArbitraryInt.
 *
 * Authors:
 *    Era Scarecrow <rtcvb32@yahoo.com>
 */
module std.experimental.arbitraryint.gen32;

import std.traits : isIntegral, isSigned, isUnsigned;

/*  Internal functions, unlimited add/sub/inc/dec/rshift/lshift/cmp/icmp/div_small_div/mul on one or two arrays

    For simplicity, it's little endian in structure. So [0] is the lowest value and [$-1] has the highest value.
    All functions use array slices.
*/

//pragma causing LDC to barf, "can't be always inline & not inline at the same time".
version(LDC) {} else {
    pragma(inline, true):
}

//internal type, should be half the size of the largest type we can work with
//so uint if we can work with longs, and longs if we can work with cent
alias Int = uint;
enum IntBits = Int.sizeof*8;

//forcibly turns off ASM coding.
debug(NoAsm) {
    enum UseAsm = false;
} else {
    enum UseAsm = true;
}

//how many bits used from lower to higher. So 0x5 would return 3, while 0xff would be 8, and 0x100 is 9. etc.
size_t bitsUsed(T)(T val) pure @safe @nogc nothrow
if (isIntegral!T) {
    T mask = -1;
    size_t total = T.sizeof*8, bits = total;
    
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

    assert(bitsUsed(0x8000_00000000L) == 48);
}

//goes over an array and reduces it before being handled internally, either by compare, multiply or divide.
inout(T)[] reduceArray(T)(inout(T)[] arr, ptrdiff_t sign=0) pure @safe nothrow @nogc
if (isIntegral!T) {
    assert(sign == 0 || sign == -1);
    
    while(arr.length && arr[$-1] == cast(T)sign)
        arr = arr[0 .. $-1];
    
    return arr;
}

unittest {
    assert(reduceArray([100, 0]) == [100]);
    assert(reduceArray([100, -1]) == [100, -1]);
    assert(reduceArray([100, -1], -1) == [100]);
    assert(reduceArray([0, 0]) == []);

    assert(reduceArray([100L, 0L]) == [100L]);
    assert(reduceArray([100L, -1L]) == [100L, -1L]);
    assert(reduceArray([100L, -1L], -1) == [100L]);
    assert(reduceArray([0L, 0L]) == []);
}

//like cmp, only reduces both signed flags before considering comparing.
ptrdiff_t icmp(const(uint)[] lhs, const(uint)[] rhs) pure @safe nothrow @nogc {
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
ptrdiff_t cmp(const(uint)[] lhs, const(uint)[] rhs) pure @safe nothrow @nogc {
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
    uint[4] buff;
    uint[] small = buff[0 .. 2], large=buff[2 .. $];
    
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
T[] add(T)(T[] lhs, const(T)[] rhs) pure @safe @nogc nothrow
if(isIntegral!T && isUnsigned!T) {
    T t, carry;
    
    if (rhs.length > lhs.length)
        rhs = rhs[0 .. lhs.length];
    
    foreach(i, ref v; lhs[0 .. rhs.length]) {
        t = v + rhs[i] + carry;
        carry = (t < v) ? 1 : 0;
        v = t;
    }
    
    //carry leftover? Adjust accordingly
    if (carry)
        inc(lhs[rhs.length .. $]);

    return lhs;
}

//subtract any int array to another int array. rhs is truncated to the result/left side.
T[] sub(T)(T[] lhs, const(T)[] rhs) pure @safe @nogc nothrow
if(isIntegral!T && isUnsigned!T) {
    T t, carry;
    
    if (rhs.length > lhs.length)
        rhs = rhs[0 .. lhs.length];
    
    foreach(i, ref v; lhs[0 .. rhs.length]) {
        t = v + -rhs[i] - carry;
        carry = (t > v) ? 1 : 0;
        v = t;
    }
    
    //carry leftover? Adjust accordingly
    if (carry)
        dec(lhs[rhs.length .. $]);

    return lhs;
}

unittest {
    uint[3] buff;
    uint[] lhs = buff[], rhs;

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

T[] inc(T)(T[] val) pure @safe @nogc nothrow
if(isIntegral!T && isUnsigned!T) {
    foreach(ref v; val) {
        ++v;
        if (v)
            break;
    }
    return val;
}

T[] dec(T)(T[] val) pure @safe @nogc nothrow
if(isIntegral!T && isUnsigned!T) {
    foreach(ref v; val) {
        --v;
        if (v != -1)
            break;
    }
    return val;
}

unittest {
    uint[2] buff;
    assert(inc(buff[]) == [1, 0]);
    assert(dec(buff[]) == [0, 0]);
    assert(dec(buff[]) == [-1, -1]);
    assert(inc(buff[]) == [0, 0]);
}

//multiply. length of result has to be as big as the lhs+rhs lengths.
//faster means be less precise, and only return the length that the lhs gives, at which point
//the lhs has to be as big if not bigger than the rhs.
uint[] mul(uint[] res, const(uint)[] lhs, const(uint)[] rhs, bool faster = true) pure @nogc nothrow {
    assert(res.length >= lhs.length + rhs.length);

    res[0 .. (faster ? lhs.length : $)] = 0;
    
    if (!__ctfe && UseAsm) {
        version(D_InlineAsm_X86) {
            foreach(i, rhs_v; cast(uint[]) rhs) { //cast only, otherwise: integer constant expression expected instead
                if (!rhs_v)
                    continue;
                //s & p pointers to get proper location
                const(uint)* s = &lhs[0];
                uint* p = &res[i];
                size_t cnt = faster ? lhs.length - i : lhs.length;
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
    }
    
    //need some shifts
    ulong val;
    uint c;

    foreach(i, r; rhs)
    if (r) {
        val = c = 0;
        foreach(i2, ulong l; faster ? lhs[0 .. $-i] : lhs)
        if (l || c) {
            val = l * r + c + res[i + i2];
            res[i + i2] = cast(uint) val; //lower half saved
            c = val >> (uint.sizeof * 8);  //upper half becomes carry
        }
        
        //if there's a carry, we add it
        if (!faster && c)
            foreach(i2, ref l; res[i+lhs.length .. $]) {
                val = c + l;
                l = cast(uint) val;
                c = val >> (uint.sizeof * 8);
                if (!c)
                    break;
            }
    }
    
    return faster ? res[0 .. lhs.length]: res;
}

unittest {
    uint[4] m;
    
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
uint div_small(uint[] buff, const(uint)[] n, uint d, uint[] result) pure nothrow @nogc {
    enum IntBits = uint.sizeof * 8;
    assert(n.length <= result.length);
//    assert(buff.length >= n.length*4, "buffer for temporaries, compatibility for 64bit and other versions");
    
    //shrink empty untouched of n that only slows it down.
    n = reduceArray(n);
    result[n.length .. $] = 0; //zeroize, can't possibly be anything there.
    result = result[0 .. n.length];
    
    if (!__ctfe && UseAsm) {
        version(D_InlineAsm_X86){
            const(uint) *dividend = &n[$-1];
            uint *quotent = &result[$-1];
            uint len = n.length;
            uint r;
            
            asm pure nothrow @nogc {
                    mov ESI, dividend;
                    mov EDI, quotent;
                    xor EDX, EDX;   //clear remainder
                    mov EBX, d;     //get divisor
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
    }

    ulong val;
    foreach_reverse(i, v; n) {
        val |= v;
        uint t = cast(uint) (val / d);
        result[i] = t;
        val -= t * d;
        val <<= IntBits;
    }
    
    return val >> IntBits;
}

unittest {
    //fact34: 295 232799039 604140847 618609643 520000000
    uint[4] n = [0, 0x445DA75B, 0x9EFCAC82, 0xDE1BC4D1],
           q;

    uint[] rem = [295, 232799039, 604140847, 618609643, 520000000];
    uint[][] q_res = [
        cast(uint[])[0],
        cast(uint[])[0x127],
        cast(uint[])[0xBD3F013F, 0x44],
        cast(uint[])[0x1FE42B2F, 0x12D9A84, 0x10],
        cast(uint[])[0xFE3851EB, 0x62C1065F, 0xB9F2D8F9, 0x3],
    ];
    
    //basically do 9 digits at a time as though we were printing, easiest verification with known factorial!34
    foreach_reverse(i, r; rem) {
        assert(r == div_small(null, n[], 1_0000_00000, q[]));
        assert(cmp(q[], q_res[i]) == 0);
        n[] = q[];
    }
}

//left shift the value into the result. Value & result can be the same array.
T[] lshift(T)(T[] result, const T[] value, size_t shiftby)
if(isIntegral!T) {
    enum TBits = T.sizeof * 8;
    assert(result.length == value.length);
    assert(shiftby >= 0 && shiftby < (result.length * TBits));
    if(!shiftby && result is value)
        return result;

    size_t skip = shiftby / TBits;    //how many whole blocks to move by
    shiftby -= skip * TBits;

    if (!shiftby)
        result[skip .. $] = value[0 .. $-skip];
    else {
        T t, carry;
        foreach(i, ref v; result[skip .. $]) {
            t = (value[i] << shiftby) | carry;
            carry = value[i] >>> (TBits - shiftby);
            v = t;
        }
    }

    result[0 .. skip] = 0;
    
    return result;
}

//right shift the value into the result.  Value & result can be the same array.
T[] rshift(T)(T[] result, const T[] value, size_t shiftby, bool setcarry = false)
if(isIntegral!T) {
    enum TBits = T.sizeof * 8;
    assert(value.length == result.length);
    assert(shiftby >= 0 && shiftby < (result.length*TBits));
    if(!shiftby && result is value)
        return result;

    size_t skip = shiftby / TBits;    //how many whole blocks to move by
    shiftby -= skip * TBits;

    if (!shiftby)
        result[0 .. $-skip] = value[skip .. $];
    else {
        T t, carry = setcarry ? T(-1) << (TBits - shiftby) : 0;
        foreach_reverse(i, ref v; result[0 .. $-skip]) {
            t = (value[i+skip] >>> shiftby) | carry;
            carry = value[i+skip] << (TBits - shiftby);
            v = t;
        }
    }

    result[$-skip .. $] = setcarry ? T(-1) : 0;

    return result;
}

unittest {
    enum orig = [0x76543210, 0xfedcba98];
    uint[2] lhs = orig;
    
    lhs = 0; assert(lshift(lhs, orig, 0) == [0x76543210, 0xfedcba98]);
    lhs = 0; assert(lshift(lhs, orig, 4) == [0x65432100, 0xedcba987]);
    lhs = 0; assert(lshift(lhs, orig, 32) == [0, 0x76543210]);
    lhs = 0; assert(lshift(lhs, orig, 36) == [0, 0x65432100]);

    lhs = 0; assert(rshift(lhs, orig, 0) == [0x76543210, 0xfedcba98]);
    lhs = 0; assert(rshift(lhs, orig, 4) == [0x87654321, 0x0fedcba9]);
    lhs = 0; assert(rshift(lhs, orig, 32) == [0xfedcba98, 0]);
    lhs = 0; assert(rshift(lhs, orig, 36) == [0x0fedcba9, 0]);

    //third argument, anything positive becomes the carry
    lhs = 0; assert(rshift(lhs, orig, 0, true) == [0x76543210, 0xfedcba98]);
    lhs = 0; assert(rshift(lhs, orig, 4, true) == [0x87654321, 0xffedcba9]);
    lhs = 0; assert(rshift(lhs, orig, 32, true) == [0xfedcba98, 0xffffffff]);
    lhs = 0; assert(rshift(lhs, orig, 36, true) == [0xffedcba9, 0xffffffff]);
    
    //test against same buffer, make sure it doesn't cause issues
    lhs = orig; assert(lshift(lhs, lhs, 0) == [0x76543210, 0xfedcba98]);
    lhs = orig; assert(lshift(lhs, lhs, 4) == [0x65432100, 0xedcba987]);
    lhs = orig; assert(lshift(lhs, lhs, 32) == [0, 0x76543210]);
    lhs = orig; assert(lshift(lhs, lhs, 36) == [0, 0x65432100]);

    lhs = orig; assert(rshift(lhs, lhs, 0) == [0x76543210, 0xfedcba98]);
    lhs = orig; assert(rshift(lhs, lhs, 4) == [0x87654321, 0x0fedcba9]);
    lhs = orig; assert(rshift(lhs, lhs, 32) == [0xfedcba98, 0]);
    lhs = orig; assert(rshift(lhs, lhs, 36) == [0x0fedcba9, 0]);

    lhs = orig; assert(rshift(lhs, lhs, 0, true) == [0x76543210, 0xfedcba98]);
    lhs = orig; assert(rshift(lhs, lhs, 4, true) == [0x87654321, 0xffedcba9]);
    lhs = orig; assert(rshift(lhs, lhs, 32, true) == [0xfedcba98, 0xffffffff]);
    lhs = orig; assert(rshift(lhs, lhs, 36, true) == [0xffedcba9, 0xffffffff]);
}

/*  Perhaps the hardest part of this whole thing is the following function. Watched a video on a simple
arbitrary division using only the most significant digit, which then collapses easily enough with
div_asm_small where a number of passes are done each getting closer to the goal, taking the difference
and adjusting the next pass.

One key diference is i shift the divisor and dividend to fill a full 32/64 bit block first, my tests
showed it went from 8 passes to 2.

the buff must be at least 3 times larger than the n value, for temporaries. This avoids the gc
*/
void div(uint[] buff, const(uint)[] n, const(uint)[] d, uint[] q, uint[] r) pure @nogc nothrow {
    assert(d, "Divide by zero");

    //reduction for n, q & r
    n = reduceArray(n);

    //zeroize never reached area and shorten
    q[n.length .. $] = 0;
    r[n.length .. $] = 0;
    q = q[0 .. n.length];
    r = r[0 .. n.length];
    
    foreach_reverse(i, uint divisor; d) {
        if (!divisor)
            continue;
    //find most significant to divide by.
        if (i == 0) {
            //simple division, no guesswork
            //call the simpler separated one once
            r[1 .. $] = 0;
            r[0] = div_small(null, n, divisor, q);
        } else {
            uint[] quotent_t = buff[0 .. n.length];
            uint[] mult_temp = buff[n.length .. (n.length + q.length + d.length)];
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
    uint[12] buff;
    uint[4]  fact13p1=[0x7328CC01, 1, 0, 0],
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
    uint[4] sm = [123456789, 0, 0, 0];
    div(buff, fact21, sm, q, r);
    assert(q == [0x5A95ECDC, 0x60, 0, 0]);
    assert(r == [0x59765F5, 0, 0, 0]);
}

//assumed signed, returns the highest bit of the last element
bool getSign(T)(const T[] n) @safe pure nothrow @nogc
if(isIntegral!T) {
    return cast(bool) (n[$-1] & (T(1) << (T.sizeof*8 - 1)));
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
T[] neg(T)(T[] n) @safe pure nothrow @nogc
if(isIntegral!T && isUnsigned!T) {
    n[] = ~n[];
    return inc(n);
}

unittest {
    uint[2] val = [100, 0];
    
    assert(neg(val) == [-100, -1]);
    assert(neg(val) == [100, 0]);
    val[] = 0x55555555;
    assert(neg(val) == [0xAAAAAAAB, 0xAAAAAAAA]);
    assert(neg(val) == [0x55555555, 0x55555555]);
}
