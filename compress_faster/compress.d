module compress;

import std.stdio;
import std.conv;
import std.range;


enum AsciiChars = 128, Window = 1024*16, minCompress = 4;
alias RawType = int;
alias RawBuffer = RawType[Window];
alias ScanBuffer= RawType[][AsciiChars];

struct Buffer {
    ScanBuffer scan;
    RawBuffer raw;
}

//scans and creats offsets for each character, up to window length.
//all buffer offsets are +1 from the match, since you already know what the first character is
//the +1 is removed in the return for findLongest
void scanData(ref Buffer buffer, string haystack) {
    RawType[AsciiChars] lengths;
    if (haystack.length > Window)
        haystack = haystack[0 .. Window];
//    assert(Window >= haystack.length);
    
    foreach(v; haystack)
        ++lengths[v];
    
    int offset;
    foreach(i, ref arr; buffer.scan) {
        if (lengths[i]) {
            arr = buffer.raw[offset .. (offset+lengths[i])];
            offset += lengths[i];
        }
    }
    
    lengths[] = 0; //reset
    foreach(i, v; haystack) {
        buffer.scan[v][lengths[v]++] = cast(RawType) (i + 1);   //offset+1
    }
}

unittest {
    string toScan = "The quick brown fox jumped over the lazy dog!";
    Buffer buffer = void;
    scanData(buffer, toScan);// == toScan.length);
    
    //easiest test is probably the spaces.
    assert(buffer.scan[' '] == [4, 10, 16, 20, 27, 32, 36, 41]);
}

struct Window_LZ {
    uint matchLength;
    uint matchOffset;

    enum offbits3 = 12, lenbits3 = 8;
    enum offbits2 = 7, lenbits2 = 6;
//    enum offbits1 = 4, lenbits1 = 2;
    enum maxOffset3 = (1<<offbits3)-1, maxLength3 = ((1<<lenbits3)-1)+minCompress;
    enum controlChars = 33, maxOffset2 = 255-controlChars, maxLength2 = ((1<<lenbits2)-1)+minCompress;
//    enum maxOffset1 = (1<<offbits1)-1, maxLength1 = ((1<<lenbits1)-1)+minCompress;
    enum maxOffset = maxOffset3, maxLength = maxLength3;
    
    static assert(offbits3+lenbits3 <= 20); //4 control bits, 20 storage bits
    static assert(offbits2+lenbits2 <= 13); //3 control bits
//    static assert(offbits1+lenbits1 <= 6); //2 control bits
    
    //encodes to block of data. Returns the number of bytes it uses/exhausts
    int encode(ubyte[] outp) const {
        assert(outp.length >= 2);
        assert(matchOffset < maxOffset);
        assert(matchLength < maxLength);
        assert(matchLength >= minCompress);
//        assert(matchOffset >= minCompress);
        
        //reduce known minimum
//        if (matchOffset < maxOffset1 && matchLength < maxLength1) {
//            outp[0] = (0xc0 | ((matchLength - minCompress) << offbits1) | matchOffset) & 0xff;
//            return 1;
        if (matchOffset < maxOffset2 && matchLength < maxLength2) {
            static assert(offbits2 == 7);    //just to ensure this implimentation is taken care of.
            outp[0] = 0xc0 | (matchLength - minCompress) & 0xff;
            outp[1] = cast(ubyte) (matchOffset + controlChars);
            return 2;
        } else {
            assert(outp.length >= 3);
            //ripped from compress.c, and modified to take better advantage of distance?
            uint val = ((matchLength - minCompress) << offbits3) | matchOffset;
            outp[0] = 0x80 | ((val >> 14) & 0x3f);
            outp[1] = 0x80 |  (val >> 7)  & 0xff;
            outp[2] = 0x80 |   val        & 0xff;
            return 3;
        }
    }
    
    //decodes from block. Returns number of bytes it needed
    int decode(const ubyte[] inp) {
        assert(inp.length);
        assert((inp[0] & 0xc0) == 0xc0 ||
                    (   (inp[0] & 0x80) &&
                        (inp[1] & 0x80) &&
                        (inp[2] & 0x80) ));

        switch(inp[0] & 0xc0) {
            case 0x80:
                assert(inp.length >= 3);
                int val =   ((inp[0] & 0x7f) << 14) |
                            ((inp[1] & 0x7f) << 7) |
                            ((inp[2] & 0x7f));

                matchLength = minCompress + (val >> offbits3);
                matchOffset = val & ((1 << offbits3)-1);
                return 3;
            case 0xc0:
                matchLength = minCompress + (inp[0] & 0x3f);
                matchOffset = inp[1] - controlChars;
                return 2;
//                matchLength = minCompress + ((inp[0] >> offbits1) & ((1 << lenbits1)-1));
//                matchOffset = inp[0] & ((1 << offbits1)-1);
//                return 1;
            default:
                return 0;   //shouldn't have gotten here...
        }
    }
    
    void invertOffset() {
        matchOffset = maxOffset - matchOffset;
    }
}

unittest {
    Window_LZ t, t2;
    ubyte[3] buff;
    
    t = Window_LZ(31, 415);

    //return values and encoding the work
    assert(t.encode(buff) == 3);
    assert(t2.decode(buff) == 3);
    
    assert(t == t2);
    
    //lastly check the 1byte encoding
    t = Window_LZ(minCompress+1, minCompress+2);
    assert(t.encode(buff) == 2);
    assert(t2.decode(buff) == 2);
    
    assert(t == t2);
    
    //k, lets brute force the combinations this can take and make sure they encode/decode correctly.
    foreach(o; iota(minCompress, Window_LZ.maxOffset))
        foreach(l; iota(minCompress, Window_LZ.maxLength)) {
            t = Window_LZ(l, o);
            int l1 = t.encode(buff);
            int l2 = t2.decode(buff);
//            writeln(buff[0 .. l1], t, t2);
            assert(l1 == l2);
            assert(t == t2);
            
            auto t1 = t;
            t1.invertOffset();
            l1 = t1.encode(buff);
            l2 = t2.decode(buff);
            t2.invertOffset();
            
//            writeln(buff[0 .. l1], t, t1, t2);
            assert(l1 == l2);
            assert(t == t2);
        }
}

//find longest of needle in the haystack, but skips characters before skipBefore
//If the haystack is shorter than the scan's offsets it will short out.
Window_LZ findLongest(ref Buffer buffer, string haystack, string needle, int skipBefore = 0) {
    Window_LZ best;
    char n = void;
    
    if (needle.length < minCompress)
        return best;
    
    n = needle[0];
    needle = needle[1 .. $];
    
    foreach(offset; buffer.scan[n]) {
        if (offset >= haystack.length)
            break;
        if (offset <= skipBefore) {
            buffer.scan[n] = buffer.scan[n][1 .. $];
            continue;
        }
        
        Window_LZ current = Window_LZ(1, (offset-1) - skipBefore);
        string hs = haystack[offset .. $];
        foreach(i, c; needle) {
            if (hs.length > i && hs[i] == c)
                current.matchLength++;
            else
                break;
        }
        
        if (current.matchLength > Window_LZ.maxLength) {
            best = Window_LZ(Window_LZ.maxLength-1, current.matchOffset);
            break;  //can't get better, break out.
        }
        
        //if larger AND the minimum compress size
        if (current.matchLength >= minCompress &&
                current.matchLength >= best.matchLength)
            best = current;
    }
    
    return best;
}

unittest {
    string haystack = "The quick brown fox jumped over the lazy dog!";
    Buffer buffer = void;
    scanData(buffer, haystack);
    
    auto result = findLongest(buffer, haystack, "fox jumped, yeah!");
    assert(result.matchLength == "fox jumped".length);
    assert(result.matchOffset == 16);
    
    //short match, minimum not met.
    result = findLongest(buffer, haystack, "lazY");
    assert(!result.matchLength);
    assert(result.matchOffset == 0);
    
    //make sure it is going by largest rather than first
    haystack = "baaaabaaaaaaabaaa";
    scanData(buffer, haystack);

    result = findLongest(buffer, haystack, "baaaaaaaaaaaa");
    assert(result.matchLength == 8);
    assert(result.matchOffset == 5);
    
    //do a shorter haystack, ensure it doesn't try to scan from the haystack where it shouldn't.
    result = findLongest(buffer, haystack[0 .. 7], "baaaaaaaaaaaa");
    assert(result.matchLength == 5);
    assert(result.matchOffset == 0);
    
    //skip, for the sliding window.
    //check for longest match, if one is later take that one as it's closer to our source probably
    result = findLongest(buffer, haystack, "baaa");
    assert(result.matchLength == 4);
    assert(result.matchOffset == 13);

    //first offset can't work anymore since we're skipping it, has to be the 5-1
    result = findLongest(buffer, haystack, "baaaa", 1);
    assert(result.matchLength == 5);
    assert(result.matchOffset == 5-1);

/*    //first & second won't work, third has to match
    result = findLongest(buffer, haystack, "baaa", 5);
    assert(result.matchLength == 4);
    assert(result.matchOffset == 12-5);
    */
}

//actual compress function. no need for terminating 0
//buffer/RawBuffer able to be reused.
string compress(string haystack) {
    //minimum compress size needs to be met
    if (haystack.length < minCompress)
        return haystack;
    
    Buffer buffer = void;
    
    char[] compressed = new char[haystack.length];
    int used=minCompress, offset = minCompress;
    
    compressed[0 .. minCompress] = haystack[0 .. minCompress];  //can't compress the first min bytes
    scanData(buffer, haystack);
    
    for (;;) {
        if (offset >= Window) {
            //small buffers, has to be at the end
//            if (haystack.length - offset)
//                break;
            //keep window we know about.
            haystack = haystack[offset-Window_LZ.maxOffset .. $];
            if (haystack.length == Window_LZ.maxOffset)
                break;
            scanData(buffer, haystack);
            offset = Window_LZ.maxOffset;
            debug {
                writeln(haystack[offset .. $]);
                writeln(compressed[0 .. used]);
                writeln("rescanned!");
            }
        }
        
        auto found = findLongest(buffer, haystack[0 .. offset], haystack[offset .. $], offset - Window_LZ.maxOffset);
        
        if (found.matchLength) {
            found.invertOffset();
            used += found.encode(cast(ubyte[]) compressed[used .. $]);
            offset += found.matchLength;
        } else {
            compressed[used++] = haystack[offset++];
        }
        
        debug {
            writeln(haystack[offset .. $]);
            writeln(compressed[0 .. used]);
        }
        
        if (offset >= haystack.length)
            break;
    }
    
    compressed.length = used; //shrink to actal compressed data.
    return cast(string) compressed;
}

string decompress(string compressed) {
    //cycle 2 times, first to get the size, second to decompress.
    const (ubyte)[] t = cast(const(ubyte)[]) compressed;
    int size, offset;
    Window_LZ lz = void;
    
    while(t.length) {
        if (t[0] & 0x80) {
            t = t[lz.decode(t) .. $];
            size += lz.matchLength;
        } else {
            size++;
            t = t[1 .. $];
        }
    }
    
    char[] output = new char[size];
    auto outp = output;
    
    t = cast(const(ubyte)[]) compressed;
    while(t.length) {
        if (t[0] & 0x80) {
            t = t[lz.decode(t) .. $];
            debug {writeln(lz);}
            outp[0 .. lz.matchLength] =
                output[offset-lz.matchOffset .. (offset-lz.matchOffset+lz.matchLength)];
            offset += lz.matchLength;
            outp = outp[lz.matchLength .. $];
        } else {
            outp[0] = t[0];
            outp = outp[1 .. $];
            t = t[1 .. $];
            offset++;
        }
        debug {
            writeln(cast(char[]) t);
            writeln(output[0 .. offset]);
        }
    }
    
    return cast(string) output;
}

unittest {
    //source of text: https://issues.dlang.org/show_bug.cgi?id=15831#c4
    string haystack = "E.s!(E.s!(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).s(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).Result).s(E.s!(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).s(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).Result).Result.foo()";

//    for(int i=0; i<11; i++)
//        haystack ~= haystack; //to make really big strings for testing. About a meg
    
    auto comp = compress(haystack);
    auto orig = decompress(comp);
    
    assert(comp.length <= haystack.length);
    assert(orig == haystack);
}

/*
void main(string[] args) {
    import std.file;
    
    if (args.length < 3) {
        writeln("Usage: compress [cd] inputfile [outputfile]\n");
        return;
    }

    if (args[1] == "c") {
        auto inf = read(args[2]);
        File outf;
        if (args.length == 3)
            outf = stdout;
        else
            outf = File(args[3], "wb");
        
        auto compressed = compress(cast(string) inf);
        outf.write(compressed);
        outf.close();
    } else if (args[1] == "d") {
        auto inf = read(args[2]);
        File outf;
        
        if (args.length == 3)
            outf = stdout;
        else
            outf = File(args[3], "wb");
        
        auto decompressed = decompress(cast(string) inf);
        outf.write(decompressed);
        outf.close();
    } else {
        main(args[0 .. 0]); //print out usage again...
        return;
    }
}
*/