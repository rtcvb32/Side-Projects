/*multi-level huffman & lzw
    Author: Ryan Cecil
    Date: Jan 06 2013
    Last Updated: Jan 08 2013
    Description: Incorporates useage of BitArray with LZ77 and huffman compression.
                Currently in development and API will likely change and be refactored.
                Highly Verbose for debugging, lacks full documentation and unittests.
                MLH (Multi-level-Huffman) lacks tree save/load functions.
                
                Be warned, it may look a little ugly...

    Compiler: Win32 dmd 2.061
    
                
    LZ Usage:
---
    //rather than hello world, forces compression. Borrowed from zlib
    string helloString = "Hello Hello";

    void[] compressed = lzCompress(helloString);
    writeln(compressed);   //raw compressed state

    string uncompressed = cast(string) lzDeCompress(compressed);
    writeln(uncompressed);

    assert(helloString == uncompressed);
---

    MLH Usage:
---
    string fox = "The quick red fox jumps over the lazy dog";

    //allows multiple scans
    //order 0/no levels. regular huffman.
    //higher orders greatly increases compression,
    //but tree overhead greatly increases too
    BitArray compressed;
    auto huffCode = huffmanScan(fox, 0);
    
    makeBitCodes(huffCode);                     //make scan usable for compress/decompress
    mlhWrite(compressed, huffCode);             //save tree
    mlhCompress(compressed, huffCode, fox, 0);  //compress data
    
    writeln(compressed);   //encoded is BitArray. Raw saved data.
    
    //reverse, decode & decompress
    int offset;
    huffCode = mlhRead(compressed, offset);
    auto compressedText = compressed[offset .. $];
    auto decoded = cast(string) mlhDecompress(huffCode, compressedText);

    assert(fox == decoded);
    writeln(decoded);
---

Updates:
    2/10/2013 - Removed most of uneeded outputs, removed 'w' from lz tag since it's
        misleading.
    2/26/2013 - Removed templates required using LZ, instead uses BitArray's intWrite.
        Added a partial LMZA implimentation, called LZE

  TODO: Add more documentation, Remove explicit level requirement for mlhCompress.
       Change names to better reflect actual purpose and intent.
       Update raw characters (multiple level) to use intWrite instead.
       Add more unittests.
       Add LZW alternative that allows any size for LZW struct (BitArray)
       Reorganize most mlh functions into struct
       Add constructors for mlh.
       Re-work functions to use ranges instead of const array & offset
       Remove debugging writeln's scattered throughout the code.
*/

module mlh;

import std.algorithm;
import std.stdio;
import bitmanip; //std. missing for local test copy.
import std.conv;

//lz77 implimentation. Not fully tested.
//for BitArray constants mostly
struct LZ {
    enum windowMin = 1;
    enum rawMin = 1;

    //settings
    int windowBits = 11;
    int lengthBits = 4;
    int rawBits = 7;

    //modifyable
    int windowPos; //going back
    int length;

    ///determine max window size (behind) that it can search
    int windowMax() const pure @property @safe {
        return (1 << windowBits) - windowMin;
    }

    ///minimum length for any compression possible.
    int lengthMin() const pure @property @safe {
        //if it's equal by bytes, 1 byte more
        //otherwise extra bits to the rounding of the byte is enough.
        return ((windowBits + lengthBits + 7) / 8) + 1;
    }
    
    ///maximum length this can support based on settings
    int lengthMax() const pure @property @safe {
        return (1 << lengthBits) + lengthMin - 1;
    }

    int rawMax() const pure @property @safe {
        return 1 << rawBits;
    }
    
    //any illegal data is 'raw'.
    bool isRaw() const pure @property @safe {
        return (windowPos < windowMin) || (windowPos > windowMax) ||
                (length < lengthMin) || (length > lengthMax);
    }

    void clear() pure @safe{ windowPos = length = 0; }

    void print() {
        writeln("windowMax: ", windowMax);
        writeln("lengthMin: ", lengthMin);
        writeln("lengthMax: ", lengthMax);
        writeln("windowPos: ", windowPos);
        writeln("rawMax: ", this.rawMax);
        writeln("length: ", length);
        writeln("isRaw: ", this.isRaw);
    }
}

unittest {
    LZ lz;
    assert(lz.isRaw);
    lz.windowPos = 1;
    assert(lz.isRaw);
    lz.length = 1;
    assert(lz.isRaw); //should be wrong until length is 3
    lz.length = 2;
    assert(lz.isRaw);
    lz.length = 3;
    assert(!lz.isRaw); //should be wrong until length is 3

    assert(lz.rawMax == 128);
    lz.rawBits = 3;
    assert(lz.rawMax == 8);
}

void[] lzCompress(const void[] rawInput,
                int windowBits = 11, int lengthBits = 4, int rawBits = 7,
                int maxBits = 16) {
    return lzCompress(rawInput, null, windowBits, lengthBits, rawBits, maxBits);
}

///
void[] lzCompress(const void[] rawInput, SwiftSearch ss,
                int windowBits = 11, int lengthBits = 4, int rawBits = 7,
                int maxBits = 16)
in {
    assert(windowBits <= maxBits, "window/distance has too many bits for signature");
    assert(lengthBits <= maxBits, "length has too many bits for signature");
    assert(rawBits < maxBits, "rawBits has too many bits for signature");
}
body {
    BitArray ba_data;
    LZ lz_data = LZ(windowBits, lengthBits, rawBits);
    ubyte[] input = cast(ubyte[]) rawInput;
    ubyte[] window;
    int rawStart = 0;
    
    ba_data.reserve(input.length * 10);
    //1 byte information specifying bit lengths for window/length
    //
    ba_data.intWrite(windowBits, 1, maxBits);
    ba_data.intWrite(lengthBits, 1, maxBits);
    ba_data.intWrite(rawBits,    0, maxBits-1);

    if (!ss)
        ss = swiftSearchScan(input);
    else
        ss = ss.dup;    //shallow copy
    
    foreach(int position; rawStart .. input.length) {
        if (position < rawStart)
            continue;

        lz_data = windowSearch(lz_data, ss, input, position);
        if (lz_data.isRaw)
            continue;
        
        //uncompressed data
        bulkRawWrite(lz_data, ba_data, input[rawStart .. position]);

        debug { writeln("LZ: dist: ", lz_data.windowPos, ", len: ", lz_data.length); }
        //window/compressed data
        ba_data ~= false;   //prefix
        ba_data.intWrite(lz_data.windowPos, lz_data.windowMin, lz_data.windowMax);
        ba_data.intWrite(lz_data.length, lz_data.lengthMin, lz_data.lengthMax);
        rawStart = position + lz_data.length;
        lz_data.clear();
    }
    
    //append last unused raw
    bulkRawWrite(lz_data, ba_data, input[rawStart .. $]);
    
    void[] buff;
    ba_data.getBuffer(buff);
    return buff[0 .. ((cast(int) ba_data.length + 7) / 8)];
}

//rough implimentations, scans the window area for the longest possible match
//returns the position & length, or isRaw
//minGain is how many bytes of compression so beyond the lengthMin
T windowSearch(T)(T lz, const ubyte[] input, int position, int minGain = int.max) {
    lz.clear();

    foreach(i; 0 .. position) {
        //if within window length of the search, and meets the minimum length
        if ((position-i) <= lz.windowMax && (position+lz.lengthMin) < input.length &&
                input[i .. i+lz.lengthMin] == input[position .. position+lz.lengthMin]) {
            int length = lz.lengthMin;

            //continue further scan to see how long it is
            //so long as the length doesn't exceed our max
            while((length < lz.lengthMax) &&
                    ((position+length) < input.length) &&
                    (input[i+length] == input[position+length])) {
                length++;
            }

            if (length > lz.length) {
                lz.length = length;
                lz.windowPos = position - i;
            }
        }

        //early quit when successfully finds 'long enough' match
        if ((lz.length - lz.lengthMin) >= minGain || lz.length == lz.lengthMax)
            break;
    }

    return lz;
}

alias uint[][] SwiftSearch;

//creates an array lookup of where 1-3 bytes matches are speeding up
//basic searches for window Search if included.
SwiftSearch swiftSearchScan(const ubyte[] input, int levels = 1) {
    SwiftSearch ss;

    assert(levels >= 1 && levels <= 3);
    //32bit can only really handle 3 bytes, good for most matches of 3 bytes or larger needed.

    ss.length = 1<<(levels * 8);

    foreach(i, ub; input[0 .. $-(levels - 1)]) {
        int val = 0;
        auto t = input[i .. i+levels];
        while (t.length) {
            val <<= 8;
            val |= t[0];
            t = t[1 .. $];
        }
        ss[val] ~= i;
    }

    return ss;
}

//a bit of duplication from here with windowSearch, likely to be more.
//Will be refactored later once I'm satisfied with a good working model

//rough implimentations, scans the window area for the longest possible match
//returns the position & length, or isRaw. dontChange is for slicing the
//now unreachable sections, so they aren't constantly rescanned as we progress
T windowSearch(T)(T lz, ref SwiftSearch ss, const ubyte[] input, int position, bool dontChange = false, int minGain = int.max) {
    lz.clear();

    if (!ss.length)
        return windowSearch(lz, input, position);

    uint[] scanArray;
    int offset;

    switch(ss.length) {
        case 1<<8: offset = input[position]; break;
        case 1<<16:
            if ((input.length - position) < 2)
                return lz;

            offset = (input[position] << 8) |
                    input[position + 1];
            break;
        case 1<<24:
            if ((input.length - position) < 3)
                return lz;
            offset = (input[position] << 16) |
                    (input[position + 1] << 8) |
                    input[position  + 2];
            break;
        default:
    }

    //get the whole section matching 1-3 characters
    scanArray = ss[offset];

    debug { writeln(scanArray); }

    while (scanArray.length && ((position - scanArray[0]) > lz.windowMax)) {
        scanArray = scanArray[1 .. $];    //shorten off ones that can't qualify
        continue;
    }

    //update to shorter array unless told otherwise
    if (!dontChange)
        ss[offset] = scanArray;

    foreach(i; scanArray) {
        if (i >= position)
            break;

        int minLength = lz.lengthMin;

        if ((position+minLength) < input.length &&
                input[i .. i + minLength] == input[position .. position + minLength]) {
            int length = minLength;

            //continue further scan to see how long it is
            //so long as the length doesn't exceed our max
            while(length < lz.lengthMax &&
                    (position+length) < input.length &&
                    input[i+length] == input[position+length]) {
                length++;
            }

            if (length > lz.length) {
                lz.length = length;
                lz.windowPos = position - i;
            }
        }

        //early quit when successfully finds 'long enough' match
        if ((lz.length - minLength) >= minGain || lz.length == lz.lengthMax)
            break;
    }

    debug { writeln("\nWindow Search: "); /*lz.print();*/ }
    return lz;
}

//takes whole length of input and prepends a 'raw' LZ block.
//repeats until full raw data is written
void bulkRawWrite(LZ lz, ref BitArray ba, const(ubyte)[] input) {
    //append last unused raw
    while(input.length) {
        lz.clear();
        lz.length = min(input.length, lz.rawMax);
        ba ~= true; //prepend 'raw' bit

        ba.intWrite(lz.length, lz.rawMin, lz.rawMax);
        debug { writeln("BRW - Raw: ", lz.length); }

        foreach(i, ub; input) {
            if (i >= lz.length)
                break;
            ba.rawWrite(ub);
            debug { writeln("    ", ub); }
        }
        input = input[lz.length .. $];
    }
}

void bulkRawWrite(LZE lz, ref BitArray ba, const(ubyte)[] input) {
    //append last unused raw
    while(input.length) {
        lz.raw.value = min(lz.rawMax, input.length);
        ba ~= false; //prepend 'raw' bit

        lz.raw.encode(ba);

        debug { writeln("BRW (lze) - Raw: ", lz.raw.value); writeln(ba);}

        foreach(i, ub; input) {
            if (i >= lz.raw.value)
                break;
            ba.rawWrite(ub);
            debug { writeln("    ", ub); }
            debug { writeln(ba); }
        }
        input = input[lz.raw.value .. $];
    }
}

///
void[] lzDeCompress(const void[] input, int maxBits = 16) {
    ubyte[] buffer;
    const BitArray ba = BitArray(cast(void[]) input);
    int window, length;
    int offset;
    bool isRaw;
    LZ lz;
    
    //16/16 limitation keeps down to 1 byte for data
    offset += ba.intRead(lz.windowBits, offset, 1, maxBits);
    offset += ba.intRead(lz.lengthBits, offset, 1, maxBits);
    offset += ba.intRead(lz.rawBits, offset, 0, maxBits - 1);
    int minSize = min(1 + lz.rawBits + 8, lz.windowBits + lz.lengthBits+1);
    
    while((ba.length - offset) >= minSize) {
        offset += ba.rawRead(isRaw, offset);
        if (isRaw) {
            offset += ba.intRead(lz.length, offset, lz.rawMin, lz.rawMax);
            for(int i = lz.length; i; i--) {
                ubyte ub;
                offset += ba.rawRead(ub, offset);
                buffer ~= ub;
            }
        } else {
            offset += ba.intRead(lz.windowPos, offset, lz.windowMin, lz.windowMax);
            offset += ba.intRead(lz.length, offset, lz.lengthMin, lz.lengthMax);
            
            //overlapping copy, likely long string like "*****"
            //can't have optimizations as they might not copy it right.screw it up.
            if (lz.windowPos < lz.length) {
                buffer.length = buffer.length + lz.length;
                auto lhs = buffer[($-lz.windowPos-lz.length) .. $];
                auto rhs = buffer[$-lz.length .. $];
                
/*              memmove/memcpy failed with sections so close to eachother. likely 32bit copy used
                import std.c.string;
                memcpy(rhs.ptr, lhs.ptr, lz..length);
*/
                foreach(i, ref b; rhs)
                    b = lhs[i];
            } else
                buffer ~= buffer[($-lz.windowPos) .. ($-lz.windowPos+lz.length)];
        }
    }

    return cast(void[]) buffer;
}

unittest {
    string s = "Hello Hello";
    //2 byte struct sequence
    void[] x = lzCompress(s);
    writeln(x);
//    writeln(cast(string) x);
//    writeln(x.length);
    string y = cast(string) lzDeCompress(x);
    assert(y == s);
    
    //one byte mini-compress
    void[] x2 = lzCompress(s,4,3);
    writeln(x2);
    
    string y2 = cast(string) lzDeCompress(x2);
//    writeln(cast(string)y);
//    assert(x2.length < s.length);
    assert(y2 == s);
    
    string dups = "--------";
    auto x3 = lzCompress(dups,4,3);
    writeln(x3);
    string y3 = cast(string) lzDeCompress(x3);
    writeln(y3);
    writeln(dups);
    assert(y3 == dups);

    string dups2 = "abcabcabc";
    auto x4 = lzCompress(dups2,4,3);
    writeln(x4);
    string y4 = cast(string) lzDeCompress(x4);
    assert(y4 == dups2);

    string longBuffer = dups ~ dups ~ dups ~ dups;
    x3 = lzCompress(longBuffer,4,3);
    writeln(x3);
    y3 = cast(string) lzDeCompress(x3);
    assert(y3 == longBuffer);

    //currently breaks
    x3 = lzCompress(longBuffer);
    writeln(x3);
    y3 = cast(string) lzDeCompress(x3);
    assert(y3 == longBuffer);
}

/*LZMA according to wikipedia using prefixes to optimize for different lengths/types
  of data. Unfortunately the details on distance is lacking and this isn't dictionary 
  based so the length and a few details are borrowed and improvised. 
  
  This is LZE:
    packed code (bit sequence), packet name, packet description
    0 + byteCode                LIT          A single byte encoded 
    1 + len + dist              MATCH        A typical LZ77 sequence 

    The length is encoded as follows:
    Length code (bit sequence) Description
    0+ 3 bits       The length encoded using 3 bits, gives the lengths range from 2 to 9.
    1+0+ 3 bits     The length encoded using 3 bits, gives the lengths range from 10 to 17.
    1+1+ 8 bits     The length encoded using 8 bits, gives the lengths range from 18 to 273.
    
    Distance is lacking, so we'll do our own.
    0   +4 bits     Distance from 1-16
    1+0 +6 bits     Distance from 17-81
    1+1 +11 bits    Distance from 82-2130
    
    Minimum code is 9 bits (raw) and 10 bits (short distance/length). No special EOF's
    to worry about.
*/

/*handles encoding/decoding of prefixed values in a range.
  This lets you put various combinations prefixes and the accumulated values
  in order to save data better. Also will encode the data to a BitArray for you.
*/
struct PrefixCodes {
    const(bool[])[] prefix;
    const(int)[] bitSizes;
    int rawMin; //adjusts as the min value before min/max figure it out
    //accumulates from prevoius values. So...
    int value;

    int maxLevels() const @property pure @safe { return prefix.length; }

    int minBits() const @property pure @safe {
        int bits = int.max;
        foreach(i, bs; bitSizes) {
            bits = min(bits, prefix[i].length + bs);
        }
        return bits;
    }

    int maxBits() const @property pure @safe {
        int bits;
        foreach(i, bs; bitSizes) {
            bits = max(bits, prefix[i].length + bs);
        }
        return bits;
    }

    static struct Calc {
        int level, min, max;
    }

    //get encoding details based on value
    Calc results() const pure @safe {
        Calc val;

        val.level = 0;
        val.min = rawMin;
        val.max = val.min + (1 << bitSizes[val.level]) - 1;

        while (((val.level+1) < bitSizes.length) && ((value < val.min) || (value > val.max))) {
            val.level++;
            val.min = val.max + 1;
            val.max = val.min + (1 << bitSizes[val.level]) - 1;
        }

        if (val.min > value || value > val.max)
            val.level = -1;

        return val;
    }

    //with the level known, gets details (for decoding)
    Calc results(int levels) const pure @safe {
        Calc val;

        if (levels >= bitSizes.length)
            return Calc(-1);

        val.level = 0;
        val.min = rawMin;
        val.max = val.min + (1 << bitSizes[val.level]) - 1;

        while (val.level < levels) {
            val.level++;
            val.min = val.max + 1;
            val.max = val.min + (1 << bitSizes[val.level]) - 1;
        }

        return val;
    }

    //returns the min/max/level the level covers.
    Calc resultsFrom(int checkValue) const pure @safe {
        Calc val;

        //check for out of range (Before the ranges)
        if (checkValue < rawMin)
            return Calc(-1);

        val.level = 0;
        val.min = rawMin;
        val.max = val.min + (1 << bitSizes[val.level]) - 1;

        while ((val.min > checkValue) && ((val.level + 1) < maxLevels)) {
            val.level++;
            val.min = val.max + 1;
            val.max = val.min + (1 << bitSizes[val.level]) - 1;
        }

        if ((val.min > checkValue) || (val.max < checkValue))
            return Calc(-1);

        return val;
    }

    //encodes sequence to bitarray
    int encode(ref BitArray array) const {
        auto prevLength = array.length;
        Calc val = results();
        if ((val.level < 0) || (val.level >= prefix.length))
            return 0;

        array ~= prefix[val.level];
        array.intWrite(value, val.min, val.max);
        return cast(int) (array.length - prevLength);
    }

    //decodes sequence to value, returns bits read.
    int decode(const BitArray array) {
        int length;
        foreach(i, pre; prefix) {
            if (array[0 .. pre.length] == pre) {
                length = pre.length;
                auto val = results(i);
                length += array.intRead(value, length, val.min, val.max);
                break;
            }
        }
        return length;
    }

    unittest {
        PrefixCodes pre = PrefixCodes( [[false],[true, false],[true, true]], [2,3,4], 1);
        //0 1-4, - 3 bits
        //1 5-12,- 5 bits
        //2 13-28- 6 bits

        //results
        auto c = pre.results();
        assert(c.level == -1); //0 not valid

        int i=1;
        while(i <= 4) {
            pre.value = i;
            c = pre.results();
            assert(c.level == 0);
            assert(c.min == 1);
            assert(c.max == 4);
            i++;
        }
        while(i <= 12) {
            pre.value = i;
            c = pre.results();
            assert(c.level == 1);
            assert(c.min == 5);
            assert(c.max == 12);
            i++;
        }
        while(i <= 28) {
            pre.value = i;
            c = pre.results();
            assert(c.level == 2);
            assert(c.min == 13);
            assert(c.max == 28);
            i++;
        }

        pre.value = i; //32
        c = pre.results();
        assert(c.level == -1); //0 not valid

        //results(levels)
        c = pre.results(0);
        assert(c.level == 0);
        assert(c.min == 1);
        assert(c.max == 4);

        c = pre.results(1);
        assert(c.level == 1);
        assert(c.min == 5);
        assert(c.max == 12);

        c = pre.results(2);
        assert(c.level == 2);
        assert(c.min == 13);
        assert(c.max == 28);

        c = pre.results(3);
        assert(c.level == -1); //0 not valid

        //encode
        BitArray ba;

        //level 0
        pre.value = 3;
        bool[] answer = [0,1,0];
        int len = pre.encode(ba);

        assert(len == answer.length);
        assert(ba == answer);

        //level 1
        ba.length = 0;
        pre.value = 10;
        answer = [1,0,1,0,1];
        len = pre.encode(ba);

        assert(len == answer.length);
        assert(ba == answer);

        //level 2
        ba.length = 0;
        pre.value = 28;
        answer = [1,1,1,1,1,1];
        len = pre.encode(ba);

        assert(len == answer.length);
        assert(ba == answer);

        //decode
        //level 0
        answer = [0,1,0,  1,1,1]; //3 + added junk
        ba = BitArray(answer);
        len = pre.decode(ba);
        assert(len == 3);
        assert(pre.value == 3);

        //level 1
        answer = [1,0,1,0,1, 1,1,1]; //10 + junk
        ba = BitArray(answer);
        len = pre.decode(ba);
        assert(len == 5);
        assert(pre.value == 10);

        //level 2
        answer = [1,1,1,1,1,1, 1,1,1];//28 + junk
        ba = BitArray(answer);
        len = pre.decode(ba);

        assert(len == 6);
        assert(pre.value == 28);

        //max bits
        //max levels
        assert(pre.maxBits == 6);
        assert(pre.maxLevels == 3);
    }
}

//lz77 implimentation. Not fully tested.
//for BitArray constants mostly
struct LZE {
/*
    static immutable bool[][] def_levelCodes = [[0,0],[0,1],[1,1],[1,0,0],[1,0,1]];
    static immutable int[] def_lenBits = [3,3,7,11,13];
    static immutable int[] def_distBits = [5,8,13,17,20];
    static immutable int[] def_rawBits = [2,4,6,8,10];

    PrefixCodes dist = PrefixCodes(def_levelCodes, def_distBits, 1);
    PrefixCodes len  = PrefixCodes(def_levelCodes, def_lenBits, 4);
    PrefixCodes raw  = PrefixCodes(def_levelCodes, def_rawBits, 1);
*/
    static immutable bool[][] def_levelCodes = [[0],[1,0],[1,1]];
    static immutable int[] def_lenBits = [3,3,8];
    static immutable int[] def_distBits = [5,8,8];

    static immutable bool[][] def_rawLevelCodes = [[false],[true]];
    static immutable int[] def_rawBits = [2,4];

    PrefixCodes dist = PrefixCodes(def_levelCodes, def_distBits, 1);
    PrefixCodes len  = PrefixCodes(def_levelCodes, def_lenBits, 4);
    PrefixCodes raw  = PrefixCodes(def_rawLevelCodes, def_rawBits, 1);

    enum windowMin = 1;
    enum rawMin = 1;

    //calculated via 'update' need to get real values for defaults.
    int windowMax = 544;
    int lengthMin = 4;
    int lengthMax = 275;
    int rawMax = 20;

    int windowPos;
    int length;

    //a more accurate one since a fixed one will require 6 bytes when
    //we know it can fit in as little as 2.
    int getLengthMin(int dist_, int len_) const @safe pure {
        auto distData = dist.resultsFrom(dist_);
        auto lenData = len.resultsFrom(len_);

        if ((distData.level == -1) || (lenData.level == -1))
            return int.max; //can't satisfy...

        //staircase... that was unintentional...
        int bits = 1;
        bits += len.prefix[distData.level].length;
        bits += dist.prefix[distData.level].length;
        bits += len.bitSizes[distData.level];
        bits += dist.bitSizes[distData.level];

        return ((bits + 7) / 8) + 1;
    }

    //update and adjust ints to more realistic values.
    void update() pure @safe { //@safe pure {
        int bits = 1;   //1 for raw/LZ
        PrefixCodes.Calc calc;

        //distance/window
        bits += dist.maxBits;

        calc = dist.results(dist.maxLevels-1);
        windowMax = calc.max;

        //length, get minimum size needed.
        bits += len.maxBits;
        int l = ((bits + 7) / 8) + 1;

        //update length min/max
        len.rawMin = l;     //calc.min is for that level, not lowest at level 0
        calc = len.results(len.maxLevels-1);

        lengthMin = l;
        lengthMax = calc.max;

        calc = raw.results(raw.maxLevels-1);
        rawMax = calc.max;
    }

    //any illegal data is 'raw'.
    bool isRaw() const @property {
        return (windowPos < windowMin) || (windowPos > windowMax) ||
                (length < lengthMin) || (length > lengthMax);
    }

    void clear() pure @safe { update(); windowPos = length = 0; }
}

unittest {
    LZE lze;
    lze.update();
    writeln(lze);
}

/**Partial LZMA implimentation. See notes
   full bit uses of length/window not possible currently
*/
void[] lzeCompress(const void[] rawInput, int rawStart = 0, SwiftSearch ss = null, LZE lz_data = LZE.init) {
    BitArray ba_data;
    debug {writeln("lzeCompress");}

    ubyte[] input = cast(ubyte[]) rawInput;
    ubyte[] window;

    ba_data.reserve(input.length * 16);
    
    if (!ss.length)
        ss = swiftSearchScan(input, 2);
    else
        ss = ss.dup;

    foreach(int position; rawStart .. input.length) {
        if (position < rawStart)
            continue;

        lz_data = windowSearch(lz_data, ss, input, position, ss.length > 65536);
        if (lz_data.isRaw)
            continue;

        bulkRawWrite(lz_data, ba_data, input[rawStart .. position]);

        ba_data ~= true;
        //save length
        lz_data.len.value = lz_data.length;
        lz_data.dist.value = lz_data.windowPos;

        lz_data.len.encode(ba_data);
        lz_data.dist.encode(ba_data);

        debug {
            writeln("LZE - len: ", lz_data.length, ", dist: ", lz_data.windowPos);
            writeln(ba_data);
        }
        
        //save distance/window offset
        rawStart = position + lz_data.length;
        lz_data.clear();
    }
    
    //raw at the end to handle?
    bulkRawWrite(lz_data, ba_data, input[rawStart .. $]);
    void[] buff;
    ba_data.getBuffer(buff);
    return buff[0 .. ((cast(int) ba_data.length + 7) / 8)];
}

///Partial LZMA implimentation. See notes
void[] lzeDeCompress(const void[] input, LZE lz_data = LZE.init) {
    ubyte[] buffer;
    const BitArray ba = BitArray(cast(void[]) input);
    int offset;
    debug {writeln("lzeDeCompress");}
    
    while((ba.length - offset) >= (lz_data.raw.minBits + 8)) {
        if (!ba[offset]) {
            offset++; //skip isRaw bit

            offset += lz_data.raw.decode(ba[offset .. $]);

            debug { writeln("LZE-Raw: ", lz_data.raw.value);
                    writeln(ba[offset .. $]);}

            ubyte ub;
            for(; lz_data.raw.value; lz_data.raw.value--) {
                offset += ba.rawRead(ub, offset);
                buffer ~= ub;
                debug { writeln("Raw: ", ub);
                        writeln(ba[offset .. $]);}
            }
        } else {
            offset++; //skip isRaw bit

            offset += lz_data.len.decode(ba[offset .. $]);
            offset += lz_data.dist.decode(ba[offset .. $]);
            int length = lz_data.len.value;
            int window = lz_data.dist.value;
            
            debug {
                writeln("LZE - len: ", length, ", dist: ", window);
                writeln(ba[offset .. $]);
            }
            
            //overlapping copy, likely long string like "*****"
            //can't have optimizations as they might not copy it right.screw it up.
            if (window < length) {
                buffer.length = buffer.length + length;
                auto lhs = buffer[($-window-length) .. $];
                auto rhs = buffer[$-length .. $];
                
                foreach(i, ref b; rhs)
                    b = lhs[i];
            } else
                buffer ~= buffer[($-window) .. ($-window+length)];
        }
    }

    return cast(void[]) buffer;
}


unittest {
    writeln("LZe half");

    string s = "Hello Hello";
    //2 byte struct sequence
    void[] x = lzeCompress(s);
    writeln(x);
    string y = cast(string) lzeDeCompress(x);
    assert(y == s);
    
    string dups = "--------";
    auto x3 = lzeCompress(dups);
    writeln(x3);
    string y3 = cast(string) lzeDeCompress(x3);
    writeln(y3);
    writeln(dups);
    assert(y3 == dups);

    string dups2 = "abcabcabc";
    auto x4 = lzeCompress(dups2);
    writeln(x4);
    string y4 = cast(string) lzeDeCompress(x4);
    assert(y4 == dups2);

    string longBuffer = dups ~ dups ~ dups ~ dups;
    x3 = lzeCompress(longBuffer);
    writeln(x3);
    //broken only due to minimum size of 6 which is wrong,
    //incomplete re-working in progress
    y3 = cast(string) lzeDeCompress(x3);
    assert(y3 == longBuffer);
}


//huffman starts here. unions/repacking may be done later.
struct Node {
    Node[] nextLevel;
    Tree decodeTree;        //just the start of the tree.
    BitArray[] bitCodes;    //0 level only
    bool singleElement;
    uint singleElementValue;
    
    uint convertedTo;        //simple conversion for type2 of mlh
    
    uint value;
    uint weight;
    
    uint _maxValues = ubyte.max + 1;
    
    void maxValues(uint max) @property pure nothrow { _maxValues = max; }
    uint maxValues()   const @property pure nothrow { return _maxValues; }
    uint maxNodesAllocate()  @property pure const   { return (_maxValues * 2) - 1; }
    
    //may impliment later; returned in the pair?
    enum State {
        OK,
        CodeMissing,    //code for encoding is missing
        BrokenInput,    //missing bits to finish a symbol/code
        OutputFull,     //output buffer is full
    }
    
    //for debugging mostly
    string toString(int level = 0) const {
        string tabs = "\t\t\t\t\t\t\t\t"[0 .. level];
        string x = tabs ~ "{ value: " ~ to!string(value) ~ "\t '" ~ (value > char.max ? '?' : cast(char) value) ~ "'  weight: " ~ to!string(weight) ~ " }\n";
        if (weight == 0)// || left || right)
            x = null;

        if (nextLevel.length) {
            if (weight)
                x ~= tabs ~ "NextLevel {\n";

            foreach(i, n; nextLevel) {
                string s = n.toString(level+1);

                x ~= s;
            }
            
            if (weight)
                x ~= tabs ~ "} // value: " ~ to!string(value) ~ "  '"~(value > char.max ? ' ' : cast(char) value)~"'\n";
        }
        
        if (bitCodes) {
            foreach(i, ref bc; bitCodes) {
                if (bc.length) {
                    x ~= bc.toString();
                    x ~= "\t - '" ~ (cast(char) i) ~ "'\n";
                }
            }
        }
        
        return x;
    }
    
    const(BitArray) encode(const ubyte[] path) const
    in {
        assert(weight); //no weight means no contents
        assert(path.length > 0);
        assert(nextLevel || bitCodes || (singleElement && singleElementValue == path[0]), 
                text(nextLevel, bitCodes, singleElement, singleElementValue, " ", this.value, to!string(this)));
    }
    body {
        if (path.length > 1) {
            //no weight/nextLevel means no contents
            assert(nextLevel, "No extention for path, currupted huffman struct!");
            assert(nextLevel[path[0]].weight,
                    ("Path/code has no weight, currupted huffman struct!", "\n", to!string(path[0])));
            return nextLevel[path[0]].encode(path[1 .. $]);
        } else {
            if (singleElement) {
                if (singleElementValue == path[0])
                    return BitArray();  //empty since there's nothing to encode.
                assert(0, "Should never get here! Single value doesn't match input!");
            } else {
                assert(bitCodes.length, "Missing encoding bits data!" ~ this.toString());
                return bitCodes[path[0]];
            }
        }
    }
    
    MLH_TreePair decode(const BitArray input, ubyte[] previous) const {
        if (previous.length) {
            return nextLevel[previous[0]].decode(input, previous[1 .. $]);
        }
    
        if (singleElement)
            return MLH_TreePair(0, singleElementValue);

        //else
        return decodeTree.decode(input);
    }
}

struct MLH_TreePair {
    ulong offset;
    int value;
    bool raw;
}

struct Tree {
    Tree* left, right;      //for building bitCodes
    int weight;
    Node* node;
    
    //for sorting, specifically for reordering weights
    int opCmp(const ref Tree rhs) const {
        //put empty weights at the end, the beginning it's sorted from lowest to highest.
        //So 123000 rather than 000123.
        if (weight == 0)
            return 1;
        if (rhs.weight == 0)
            return -1;
        return (weight > rhs.weight) - (weight < rhs.weight);
    }
    
    MLH_TreePair decode(const BitArray input) const {
        //assuming the bitarray is const to begin with. only handling slices now.
        if (left is null && right is null) {
            //if there's no decode tree at this point, it's mlh and we need another layer to decode.
            if (node is null) {
                //likely needs to be modified later if it takes other than ubyte.
                ubyte ub;
                input.rawRead(ub, 0);
                return MLH_TreePair(8, ub, true);
            } else
                return MLH_TreePair(0, node.value);
        }
        
        assert(input.length);
        MLH_TreePair tmp = input[0] ? right.decode(input[1 .. $]) : left.decode(input[1 .. $]);
        tmp.offset++;
        return tmp;
    }
}

Node huffmanScan(const void[] input, int levels = 0) {
    Node nodes;
    return huffmanScan(nodes, input, levels);
}

void defaultAllocate(ref Node nodes) {
    nodes.nextLevel.length = nodes.maxValues;

    foreach(i, ref n; nodes.nextLevel)
        n.value = i;
}

Node huffmanScan(Node nodes, const void[] input, int levels = 0, bool topLevel = true) {
    ubyte[] inp = cast(ubyte[]) input;

    if (topLevel && !nodes.nextLevel.length)
        nodes.defaultAllocate;
    
    for(int i; i<(inp.length - levels); i++) {
        Node* currentNode = &nodes.nextLevel[inp[i]];
        currentNode.weight++;
        nodes.weight++;

        if (levels) {
            if (currentNode.nextLevel.length == 0)
                defaultAllocate(*currentNode);
            huffmanScan(*currentNode, input[i+1 .. $], levels-1, false);
        }
        
        if (!topLevel)
            break;
    }
    
    return nodes;
}

void setFrom(ref Tree[] tree, ref Node head) @property {
    assert(tree.length >= head.nextLevel.length);
    
    foreach(ref n; head.nextLevel) {
        with(tree[n.value]) {
            node = &n;
            weight = n.weight;
        }
    }
    
    sort!()(tree);
}

//does arithmetic-like coding, but needs to be level0/order0 to work. 1 level is the same as order0.
Node makeBitCodes(ref Node codes, int levels) {
    int uniqueCodes;
    ubyte[256] convertTo;
    Node arithmetic;
    
    assert(levels, "need at least 1 level");
    
    foreach(i, ref node; codes.nextLevel) {
        if (node.weight) {
            convertTo[uniqueCodes] = cast(ubyte) i;
            uniqueCodes++;
        }
    }
    
    arithmetic.nextLevel.length = uniqueCodes ^^ levels;
    
    foreach(i, ref node; arithmetic.nextLevel) {
        int l = levels;
        int id = i;
        node.value = i;
        while(l) {
            node.weight += codes.nextLevel[convertTo[id % uniqueCodes]].weight;
            id /= uniqueCodes;
            l--;
        }
        arithmetic.weight += node.weight;
    }
    
//    writeln(arithmetic.nextLevel);
    makeBitCodes2(arithmetic);
    return arithmetic;
}

//cycles though and makes the huffman tree, as well as encodes them into bits.
void makeBitCodes2(ref Node codes) {
    //stack storage until we know if we even need it, and only allocate what we
    //need to decode the tree.
    Tree[] wholeTree;
    Tree[] range;
    int emptyNode;  //according to 'range'
    bool level0 = true;

    wholeTree.length = codes.nextLevel.length*2;
    wholeTree[0 .. $].setFrom(codes);
    writeln(wholeTree);
    
    foreach(i, ref n; wholeTree) {
        //no weights (left?) should be at end after being sorted
        if (!n.weight) {
            emptyNode = i;
            break;
        }

        if (n.node.nextLevel) {
            level0 = false;
            makeBitCodes(codes.nextLevel[n.node.value]); //n is a temp, can't use it.
        }
    }
    
/*    writeln(codes);
    writeln(wholeTree);
    writeln(emptyNode);
  */  
    assert(emptyNode);
    
    if (level0) {
        //if level 0 and has only one entry, no need to do any work.
        if (emptyNode == 1) {
            codes.singleElement = true;
            codes.singleElementValue = wholeTree[0].node.value;
        } else {
            assert((emptyNode*2)-1 <= wholeTree.length);
            //more than one entry.
            range = wholeTree[0 .. (emptyNode*2)-1].dup;
            
            while(range.length > 2 && range[0].weight && range[1].weight) {
                range[emptyNode].weight = range[0].weight + range[1].weight;
                
                //address stable since 0 & 1 won't be moving.
                range[emptyNode].left = &range[0];
                range[emptyNode].right = &range[1];
                
                emptyNode -= 1;
                range = range[2 .. $];
                
                sort!()(range[0 .. emptyNode]);
            }
            
            //check if there's only one value. (if so no need for bitcode allocation
            BitArray ba;    //for bitcode as it walks the tree.
            codes.bitCodes.length = codes.maxValues;
            
            nodeWalk(codes, &range[0], ba);
            codes.decodeTree = range[0];
//            nodeWalk(codes, &range[0], BitArray());
        }
    }
}

//cycles though and makes the huffman tree, as well as encodes them into bits.
void makeBitCodes(ref Node codes) {
    //stack storage until we know if we even need it, and only allocate what we
    //need to decode the tree.
    Tree[] wholeTree; wholeTree.length = codes.maxNodesAllocate;
    Tree[] range;
    int emptyNode;  //according to 'range'
    bool level0 = true;
    
    wholeTree.setFrom(codes);
    
    foreach(i, ref n; wholeTree) {
        //no weights (left?) should be at end after being sorted
        if (!n.weight) {
            emptyNode = i;
            break;
        }

        if (n.node.nextLevel) {
            level0 = false;
            makeBitCodes(codes.nextLevel[n.node.value]); //n is a temp, can't use it.
        }
    }
    
    assert(emptyNode);
    
    if (level0) {
        //if level 0 and has only one entry, no need to do any work.
        if (emptyNode == 1) {
            codes.singleElement = true;
            codes.singleElementValue = wholeTree[0].node.value;
        } else {
            assert((emptyNode*2)-1 <= wholeTree.length);
            //more than one entry.
            range = wholeTree[0 .. (emptyNode*2)-1].dup;
            
            while(range.length > 2 && range[0].weight && range[1].weight) {
                range[emptyNode].weight = range[0].weight + range[1].weight;
                
                //address stable since 0 & 1 won't be moving.
                range[emptyNode].left = &range[0];
                range[emptyNode].right = &range[1];
                
                emptyNode -= 1;
                range = range[2 .. $];
                
                sort!()(range[0 .. emptyNode]);
            }
            
            //check if there's only one value. (if so no need for bitcode allocation
            BitArray ba;    //for bitcode as it walks the tree.
            codes.bitCodes.length = codes.maxValues;
            
            nodeWalk(codes, &range[0], ba);
            codes.decodeTree = range[0];
//            nodeWalk(codes, &range[0], BitArray());
        }
    }
}

void nodeWalk(ref Node head, Tree* walk, ref BitArray code) {
    if (walk.left) {
        code ~= false;
        nodeWalk(head, walk.left, code);
        code = code[0 .. $-1];
    }

    if (walk.right) {
        code ~= true;
        nodeWalk(head, walk.right, code);
        code = code[0 .. $-1];
    }

    if (walk.left is null && walk.right is null) {
        head.bitCodes[walk.node.value] = code.dup;
    }
}

BitArray mlhCompress(const ref Node huffman, const void[] input, int levels) {
    BitArray output;
    mlhCompress(output, huffman, input, levels);
    return output;
}

ref BitArray mlhCompress(ref BitArray output, const ref Node huffman, const void[] input, int levels) {
    const ubyte[] inp = cast(const(ubyte[])) input;
    
    //if multiple levels, the first few aren't considered and aren't compressed.
    //if this is worked on and improved, this will be fixed. (it is in my C version)
    foreach(i, ub; inp[0 .. levels]) {
        output.rawWrite(ub);
    }
    
    //k, now we encode the remainder.
    foreach(i; 0 .. inp.length-levels)
        output ~= huffman.encode(inp[i .. i+levels+1]);
    
    return output;
}

void[] mlhDecompress(const ref Node huffman, const ref BitArray input, int forceLength = 0) {
    ubyte[] output;
    ulong offset;
    int levels;
    
    if (forceLength > 0)
        output.reserve(forceLength);
    
    while(offset < input.length || (forceLength && output.length < forceLength)) {
        auto decoded = huffman.decode(input[offset .. $], output[$-levels .. $]);
        offset += decoded.offset;
        output ~= cast(ubyte) decoded.value;
        if (decoded.raw)
            levels++;
    }
    
    return cast(void[]) output;
}

/*   Saving format. The first bit determines if this has another level/layer, recursive.
    The second bit(if another layer) determines if all values are raw or if they are all cycled via
    a 256bits (>32 characters), these are only for the path and not actual huffman trees. Should it not use
    the 256bit entire spread, length of how many codes there are are used.
    
     The codes are done going up, the higher up the smaller encoding can work allowing some minor
    extra compression.
    
     If it isn't another layer, then the following bits determine the tree structure, 0 for value,
    and 1 for split. So 1100100 should be
    an equal tree of 4 entries.
       bit number     bits equal
         1             1     
       /   \         /   \   
      2     5       1     1  
     / \   / \     / \   / \ 
    3   4 6   7   0   0 0   0
    
    Splits always go left before they go right (although as long as it's consistant
    it may not matter).
*/

//save mlh-tree into bitarray
void mlhWrite(ref BitArray output, const ref Node huffmanTree) {
    if (huffmanTree.singleElement) {
        output ~= false;    //no new levels
        output ~= false;    //no splits (tree encoding)
        
        output.rawWrite(cast(ubyte) huffmanTree.singleElementValue);
    } else if (huffmanTree.decodeTree.weight) {
        output ~= false;
        huffmanWriteTree(output, &huffmanTree.decodeTree);
    } else {
        output ~= true;
        //determine how many numbers are needed
        //if it's above 32 then we use 256 individual bits
        int codesCount;

        foreach(i, ref nl; huffmanTree.nextLevel) {
            if (nl.weight || nl.singleElement)
                codesCount++;
        }
        
        assert(codesCount, "Empty new level!");
        
        //use 256bit? If not, append the count.
        output ~= codesCount >= 32;
        if (codesCount < 32) {
            output.intWrite(codesCount, 1, 31);
        }

        int previousCode = 0;
        foreach(i, ref nl; huffmanTree.nextLevel) {
            //if 256bit note all as used/not used
            if (codesCount >= 32)
                output ~= nl.weight > 0;
            
            if (nl.weight) {
                //if not 256bit, output the whole code
                if (codesCount < 32) {
                    output.intWrite(i, previousCode, ubyte.max);
                    previousCode = i;
                }
                mlhWrite(output, huffmanTree.nextLevel[i]);
            }
        }
    }
}

//single (bottom) treewalk
void huffmanWriteTree(ref BitArray output, const Tree* tree) {
    if (tree.left is null && tree.right is null) {
        output ~= false;
        output.rawWrite(cast(ubyte) tree.node.value);
    } else {
        output ~= true;
        huffmanWriteTree(output, tree.left);
        huffmanWriteTree(output, tree.right);
    }
}

Node mlhRead(const BitArray input) {
    int offset;
    return mlhRead(input, offset);
}

//load tree
Node mlhRead(const BitArray input, ref int offset) {
    Node head;
    bool bit;

    head.defaultAllocate;
    head.weight = 1;
    
    offset += input.rawRead(bit, offset);
    if (bit) {  //another level
    
        //do we use 256 or individual bytes?
        offset += input.rawRead(bit, offset);
        if (bit) {
            //256 bits
            foreach(code, ref node; head.nextLevel) {
                offset += input.rawRead(bit, offset);
                if (bit)
                    node = mlhRead(input, offset);
            }
        } else {
            //individual bytes
            int codesCount;
            int previousCode = 0;
            int code;
            offset += input.intRead(codesCount, offset, 1, 31);
            for (;codesCount; codesCount--) {
                offset += input.intRead(code, offset, previousCode, ubyte.max);
                head.nextLevel[code] = mlhRead(input, offset);
                previousCode = code;
            }
        }
    } else {
        offset += huffmanReadTree(input[offset .. $], head);
    }

    return head;
}

Node huffmanReadTree(const BitArray input) {
    Node head;
    head.weight = 1;
    huffmanReadTree(input, head);
    return head;
}

int huffmanReadTree(const BitArray input, ref Node node) {
    node.bitCodes.length = node.maxValues;
    Tree[] tree;
    tree.length = node.maxNodesAllocate;
    node.defaultAllocate;
    int offset;
    int freeLeaf;
    BitArray code;
    
    huffmanReadTreeWalk(node, input, offset,
                tree, freeLeaf, code);

    if (freeLeaf > 1)
        node.decodeTree = tree[0];

    return offset;
}

//needs major reworking, but works for now.
void huffmanReadTreeWalk(ref Node node, 
            const BitArray input, ref int offset,
            Tree[] tree, ref int freeLeaf,
            ref BitArray code) {
    bool bit;
    int thisLeaf = freeLeaf;
    offset += input.rawRead(bit, offset);
    tree[freeLeaf].weight = 1;
    
    if (bit) {  //split
        freeLeaf++;
        tree[thisLeaf].left = &tree[freeLeaf];
        code ~= false;
        huffmanReadTreeWalk(node, input, offset,
                    tree, freeLeaf, code);
        code[$-1] = true;
        freeLeaf++;
        tree[thisLeaf].right = &tree[freeLeaf];
        huffmanReadTreeWalk(node, input, offset,
                    tree, freeLeaf, code);
        code = code[0 .. $-1];
    } else {    //value
        ubyte val;
        offset += input.rawRead(val, offset);
        
        tree[freeLeaf].node = &node.nextLevel[val];

        if (!freeLeaf) {
            node.singleElement = true;
            node.singleElementValue = val;
        } else {
            if (!node.bitCodes.length)
                node.bitCodes.length = node.maxValues;
            node.nextLevel[val].weight = 1;
            node.bitCodes[val] = code.dup;
        }

        freeLeaf++;
    }
}
/+
BitArray mlhCompress2(const ref Node huffman, const ref Node original, const void[] input, int levels) {
    BitArray output;
    mlhCompress2(output, huffman, original, input, levels);
    return output;
}

ref BitArray mlhCompress2(ref BitArray output, const ref Node huffman, const ref Node original, const void[] input, int levels) {
    const ubyte[] inp = cast(const(ubyte[])) input;
    int code;
    int len = huffman.nextLevel.length;

    //k, now we encode the remainder.
    foreach(i, ub; inp) {
        if (i && (i % levels) == 0) {
            output ~= huffman.bitCodes[code];
            code = 0;
        } else {
            code *= len;
            code += original.nextLevel[ub].convertedTo;
        }
    }
    
    return output;
}
+/

unittest {
    string jat = "just a test.";
    auto x = huffmanScan(jat, 0);
    auto y = huffmanScan(jat, 1);
    writeln(y);
    
    makeBitCodes(x);
    writeln(x);
    makeBitCodes(y);
    writeln(y);
    
    auto encoded = mlhCompress(x, jat, 0);
    writeln(encoded);
    auto encoded2 = mlhCompress(y, jat, 1);
    writeln(encoded2);
    
    string decoded = cast(string) mlhDecompress(x, encoded);
    writeln(decoded);
    string decoded2 = cast(string) mlhDecompress(y, encoded2);
    writeln(decoded2);
    
    string fox = "the quick red fox jumps over the lazy dog";
    x = huffmanScan(fox, 0);
    y = huffmanScan(fox, 1);
    auto z = huffmanScan(fox, 2);
    makeBitCodes(x);
    makeBitCodes(y);
    makeBitCodes(z);
    encoded = mlhCompress(x, fox, 0);
    encoded2 = mlhCompress(y, fox, 1);
    auto encoded3 = mlhCompress(z, fox, 2);

    writeln(encoded);
    writeln(encoded2);
    writeln(encoded3);

    decoded = cast(string) mlhDecompress(x, encoded, fox.length);
    decoded2 = cast(string) mlhDecompress(y, encoded2, fox.length);
    auto decoded3 = cast(string) mlhDecompress(z, encoded3, fox.length);
    writeln(decoded);
    writeln(decoded2);
    writeln(decoded3);
    
    BitArray tree;
    huffmanWriteTree(tree, &x.decodeTree);
    writeln(tree);
    
    writeln(x.bitCodes);
    
    writeln(tree);
    auto load_1 = huffmanReadTree(tree);
    writeln(load_1);
    
    auto encoded1_x = mlhCompress(load_1, fox, 0);
    assert(encoded1_x == encoded);
    auto decoded1_x = cast(string) mlhDecompress(load_1, encoded1_x, fox.length);
    assert(decoded1_x == decoded);
    
    BitArray tree2;
    tree2.print();
    
    
    writeln("\n----\n");
    
    mlhWrite(tree2, y);
    
    writeln(tree2);
    
    auto load_2 = mlhRead(tree2);
    auto encoded2_x = mlhCompress(load_2, fox, 1);
    assert(encoded2_x == encoded2);
    auto decoded2_x = cast(string) mlhDecompress(load_2, encoded2_x, fox.length);
    assert(decoded2_x == decoded2);
//    writeln("done!");


    string mlh2 = "000011122233344556667788";
    auto mlh2_s = huffmanScan(mlh2, 0);
    makeBitCodes(mlh2_s);
    writeln(mlh2_s);
    
    //vague and incomplete.
    /*
    auto type2 = makeBitCodes(mlh2_s, 2);
    writeln(type2);
    */
}

unittest {
//doc example.
    string fox = "The quick red fox jumps over the lazy dog";

    //allows multiple scans
    //order 0/no levels. regular huffman.
    //higher orders greatly increases compression,
    //but tree overhead greatly increases too
    BitArray compressed;
    auto huffCode = huffmanScan(fox, 0);
    
    makeBitCodes(huffCode);                     //make scan usable for compress/decompress
    mlhWrite(compressed, huffCode);             //save tree
    mlhCompress(compressed, huffCode, fox, 0);  //compress data
    
    writeln(compressed);   //encoded is BitArray. Raw saved data.
    
    //reverse, decode & decompress
    int offset;
    huffCode = mlhRead(compressed, offset);
    auto compressedText = compressed[offset .. $];
    auto decoded = cast(string) mlhDecompress(huffCode, compressedText);

    assert(fox == decoded);
    writeln(decoded);
}

int main(string[] args) {
//partial implimentation to allow you to use the algorithmns and test them with io
//disabled for statistical half
/+
    if (args.length == 1) {
        writeln("Usage: Algo [Decode]\nanything in a second argument is 'decode'");
        return 0;
    }

    ubyte[] raw;
    foreach (ubyte[] buffer; stdin.byChunk(2<<20)) { //1 meg should be plenty for size
        if (args[1] == "LZB") {
            if (args.length == 2) { //compress
                raw = buffer;
                auto ss = swiftSearchScan(buffer);
                for(int d = 1; d<=16; d++) {
                    for(int l = 1; l<=16; l++) {
                        for(int r=0; r <=8; r++) {
                            auto tmp = cast(ubyte[]) lzCompress(buffer, ss, d, l, r);
                            if (tmp.length < raw.length)
                                raw = tmp;
                            //length first so we can sort it usefully
                            stderr.writeln(tmp.length, " - LZB (",d+l+1," bits) - Distance: ", d, ", Length: ", l, ", RawBits: ", r, (raw.length == tmp.length) ? "  *" : "");
                        }
                    }
                }
            } else
                raw = cast(ubyte[]) lzDeCompress(buffer);
        }
        if (args[1] == "LZ") {  //default 11/4
            if (args.length == 2) //compress
                raw = cast(ubyte[]) lzCompress(buffer);
            else
                raw = cast(ubyte[]) lzDeCompress(buffer);
        } else if (args[1] == "LZE") {
            //Partial implimentation when enabled. Brings own source to about 16k.
            if (args.length == 2) //compress
                raw = cast(ubyte[]) lzeCompress(buffer);
            else
                raw = cast(ubyte[]) lzeDeCompress(buffer);
        } else if (args[1] == "LZEB") {
            //Brute force, output is useless and can'te be decompressed.
            //however it can be studied for results.
                auto ss1 = swiftSearchScan(buffer, 2);
//                auto ss2 = swiftSearchScan(buffer, 3);
                raw = buffer;
                bool firstUse = true;

                for(int len1  = 8;  len1 >= 1;      len1--)
                for(int len2  = 8;  len2 >= len1;   len2--)
                for(int len3  = 8;  len3 >= len2;   len3--)
                for(int dist1 = 8; dist1 >= 1;     dist1--)
                for(int dist2 = 10; dist2 >= dist1; dist2--)
                for(int dist3 = 20; dist3 >= dist2; dist3--)
                for(int raw1  = 4;  raw1 >= 1;      raw1--)
                for(int raw2  = 4;  raw2 >= raw1;   raw2--) {
                //to set in case got inturrupted.
                    if (firstUse && args.length == 10) {
                        firstUse = false;
                        len1 = to!int(args[2]);
                        len2 = to!int(args[3]);
                        len3 = to!int(args[4]);
                        dist1 = to!int(args[5]);
                        dist2 = to!int(args[6]);
                        dist3 = to!int(args[7]);
                        raw1 = to!int(args[8]);
                        raw2 = to!int(args[9]);
/*                        writeln(len1);
                        writeln(raw2);
                        LZE lze2;
                        lze2.len.bitSizes = [len1, len2, len3];
                        lze2.dist.bitSizes = [dist1, dist2, dist3];
                        lze2.raw.bitSizes = [raw1,raw2];
                        lze2.update();

                        writeln(lze2.len.rawMin);
                        */
                    }

                    LZE lze;
                    lze.len.bitSizes = [len1, len2, len3];
                    lze.dist.bitSizes = [dist1, dist2, dist3];
                    lze.raw.bitSizes = [raw1,raw2];

                    lze.update();
                    //two byte minimum required
                    if (lze.len.rawMin < 2)
                        continue;

                    ubyte[] tmp;

  /*                  if (lze.len.rawMin > 2)
                        tmp = cast(ubyte[]) lzeCompress(buffer, 0, ss2, lze);
                    else*/
                        tmp = cast(ubyte[]) lzeCompress(buffer, 0, ss1, lze);

                    if (tmp.length < raw.length)
                        raw = tmp;
                    //length first so we can sort it usefully
                    stderr.writefln("%d (%.2f:1) LZEB - Lengths: %d, %d, %d - Distances: %d, %d, %d - RawBits: %d, %d", tmp.length,
                            (cast(double) buffer.length) / tmp.length,
                            len1, len2, len3,
                            dist1, dist2, dist3,
                            raw1, raw2);

//                    auto t2 = cast(ubyte[]) lzeDeCompress(tmp, lze);

//                    writeln(buffer);
//                    writeln(t2);
//                    assert(t2 == buffer);
                }
/*
            } else
                assert(0, "Cannot decode, brute force left no decoding data behind");
                */
                raw = null;
        } else if (args[1] == "HUFF") {
            BitArray compressed;
            if (args.length == 2) { //compress
                auto huffCode = huffmanScan(buffer, 0);
                makeBitCodes(huffCode);
                mlhWrite(compressed, huffCode);
                mlhCompress(compressed, huffCode, buffer, 0);  //compress data
                compressed.getBuffer(raw);
            } else {
                compressed = BitArray(cast(void[]) buffer);
                int offset;
                auto huffCode = mlhRead(compressed, offset);
                auto compressedData = compressed[offset .. $];
                raw = cast(ubyte[]) mlhDecompress(huffCode, compressedData);
            }
        } else if (args[1] == "MLH") {
            assert(0, "Not implimented");
        } else if (args[1] == "MLH2") {
            assert(0, "Not implimented");
        }
        stdout.rawWrite(raw);
    }
    +/

    //for LZE brute forcing smaller portions for notes.
    ubyte[] buffer = cast(ubyte[]) stdin.byChunk(2<<20).front;

    auto ss1 = swiftSearchScan(buffer, 2);
//                auto ss2 = swiftSearchScan(buffer, 3);
    bool firstUse = true;

    int len1, len2, len3;
    int dist1, dist2, dist3;
    int raw1, raw2;
    LZE lze;
    ubyte[] tmp;
    bool[][] prefixes = [[0],[1,0],[1,1]];

    lze.len.prefix = prefixes;
    lze.dist.prefix = prefixes;
    lze.raw.prefix = prefixes;

    dist3 = dist2 = dist1 = 20;
    raw1 = raw2 = 1;
    for(len1  = 20;  len1 >= 1;      len1--)
    for(len2  = 20;  len2 >= len1;   len2--)
    for(len3  = 20;  len3 >= len2;   len3--) {
        lze.len.bitSizes = [len1, len2, len3];
        lze.dist.bitSizes = [dist1, dist2, dist3];
        lze.raw.bitSizes = [raw1,raw2];
        lze.update();

        tmp = cast(ubyte[]) lzeCompress(buffer, 0, ss1, lze);

        //length first so we can sort it usefully
        writefln("%d (%.2f:1) LZEB - Lengths: %d, %d, %d", tmp.length,
                (cast(double) buffer.length) / tmp.length,
                len1, len2, len3);
    }

    len3 = len2 = len1 = 20;
    raw1 = raw2 = 1;
    for(dist1  = 20;  dist1 >= 1;      dist1--)
    for(dist2  = 20;  dist2 >= dist1;   dist2--)
    for(dist3  = 20;  dist3 >= dist2;   dist3--) {
        lze.len.bitSizes = [len1, len2, len3];
        lze.dist.bitSizes = [dist1, dist2, dist3];
        lze.raw.bitSizes = [raw1,raw2];
        lze.update();

        tmp = cast(ubyte[]) lzeCompress(buffer, 0, ss1, lze);

        //length first so we can sort it usefully
        writefln("%d (%.2f:1) LZEB - Dist: %d, %d, %d", tmp.length,
                (cast(double) buffer.length) / tmp.length,
                dist1, dist2, dist3);
    }

    dist3 = dist2 = dist1 = 1;
    len3 = len2 = len1 = 1;
    for(raw1  = 20;  raw1 >= 1;      raw1--)
    for(raw2  = 20;  raw2 >= len1;   raw2--) {
        lze.len.bitSizes = [len1, len2, len3];
        lze.dist.bitSizes = [dist1, dist2, dist3];
        lze.raw.bitSizes = [raw1,raw2];
        lze.update();

        tmp = cast(ubyte[]) lzeCompress(buffer, 0, ss1, lze);

        //length first so we can sort it usefully
        writefln("%d (%.2f:1) LZEB - raw: %d, %d", tmp.length,
                (cast(double) buffer.length) / tmp.length,
                raw1, raw2);
    }

    return 0;
}
