import std.stdio;

enum AsciiChars = 128, Window = 4096, minCompress = 4;
alias RawType = short;
alias RawBuffer = RawType[];
alias ScanBuffer= RawType[][];

//by having a static size (buffer) we can avoid lot sof individual allocations, as well
//as reuse the buffer.
void resetBuffer(ref RawBuffer buffer, ref ScanBuffer output, string haystack) {
    if (buffer is null)
        buffer = new RawType[Window];
    
    if (output is null)
        output.length = AsciiChars;
        
    assert(haystack.length);
    
    uint[AsciiChars] ascii;
    
    foreach(c; haystack)
        ascii[c]++;

    RawBuffer t = buffer;   //backup
    foreach(i, v; ascii) {
        if (v) {
            output[i] = t[0 .. v];
            t = t[v .. $];
        }
    }
}

//scans and creats offsets for each character, up to window length.
//returns how many characters we've scanned.
int scanData(ref ScanBuffer saved, string scan) {
    uint[AsciiChars] lengths;
    int scanned = scan.length;
    
    foreach(i, v; scan) {
        //make sure we don't exceed our length
        if (lengths[v] == Window) {
            scanned = i;
            break;
        }
        saved[v][lengths[v]++] = cast(RawType) i;
    }
    
    //shorten lengths
    foreach(i, v; lengths) {
        if (v)
            saved[i] = saved[i][0 .. v];
        debug {
            //mostly so we don't have huge arrays we don't need, if we need to output to view it.
            //otherwise clearing it loses a few cycles (probably?)
            if (!v)
                saved[i] = null;
        }
    }
    
    return scanned;
}

unittest {
    string toScan = "The quick brown fox jumped over the lazy dog!";
    ScanBuffer buffer;
    RawBuffer rawBuffer;
    resetBuffer(rawBuffer, buffer, toScan);
    assert(scanData(buffer, toScan) == toScan.length);
    
    //easiest test is probably the spaces.
    assert(buffer[' '] == [3, 9, 15, 19, 26, 31, 35, 40]);
    
    //test length return
    ubyte[] empty = new ubyte[Window];
    resetBuffer(rawBuffer, buffer, cast(string) empty);
    assert(scanData(buffer, cast(string) empty) == Window);
    assert(buffer[0].length == Window);
}

struct Window_LZ {
    uint matchLength;
    uint matchOffset;

    enum maxOffset = 2047, maxLength = 1023, blockSize = 3;
//    enum maxOffset = 15, maxLength = 15, blockSize = 3;   //for really small buffer checking
    
    //encodes to block of data. Returns the number of bytes it uses/exhausts
    int encode(ubyte[] outp) const {
        assert(outp.length >= 3);
        assert(matchOffset < maxOffset);
        assert(matchLength < maxLength);
//        assert(matchLength <= matchOffset);
        
        //ripped from compress.c, and modified to take better advantage of distance?
        outp[0] = 0x80 | ((matchLength >> 3) & 0x70) | ((matchOffset >> 7) & 0xf);
        outp[1] = 0x80 | matchLength & 0xff;
        outp[2] = 0x80 | matchOffset & 0xff;
        
        return blockSize;
    }
    
    //deodes from block. Returns number of bytes it needed
    int decode(const ubyte[] inp) {
        assert(inp.length >= 3);
        assert(inp[0] & 0x80); //ensure the 3 bits are on to show it's encoded. Not much else to say.
        assert(inp[1] & 0x80);
        assert(inp[2] & 0x80);
        
        matchLength = ((inp[0] & 0x70) << 3) | (inp[1] & 0x7f);
        matchOffset = ((inp[0] & 0xf) << 7) | (inp[2] & 0x7f);
        
//        assert(matchLength <= matchOffset);
        return blockSize;
    }
    
    void invertOffset() {
        matchOffset = maxOffset-matchOffset;
    }
}

unittest {
    Window_LZ t, t2;
    ubyte[3] buff;
    
    t = Window_LZ(314, 1597);
//    t = Window_LZ(3, 14);     //small buffer checking.

    t.encode(buff);
    t2.decode(buff);
    
    assert(t == t2);
    assert(buff == [0xAC, 0xBA, 0xBD]); //raw expected result.
    
    //inverts for decoding
    t.invertOffset();
    assert(t.matchOffset == t2.maxOffset-t2.matchOffset);
    
    //converts back to normal
    t.invertOffset();
    assert (t == t2);
}

//find longest of needle in the haystack, but skips characters before skipBefore
//If the haystack is shorter than the scan's offsets it will short out.
Window_LZ findLongest(const ScanBuffer scan, string haystack, string needle, int skipBefore = 0) {
    Window_LZ best;
    
    foreach(n; scan[needle[0]]) {
        if (n >= haystack.length)
            break;
        if (n < skipBefore)
            continue;
        
        Window_LZ current = Window_LZ(0, n - skipBefore);
        string hs = haystack[n .. $];
        foreach(i, c; needle) {
            if (hs.length > i && hs[i] == c)
                current.matchLength++;
            else
                break;
        }
        
        if (current.matchLength > Window_LZ.maxLength) {
            best = Window_LZ(Window_LZ.maxLength, current.matchOffset);
            break;  //can't get better, break out.
        }
        
        //if larger AND the minimum compress size
        if (current.matchLength >= minCompress &&
                current.matchLength > best.matchLength)
            best = current;
    }
    
    return best;
}

unittest {
    string haystack = "The quick brown fox jumped over the lazy dog!";
    ScanBuffer buffer;
    RawBuffer rawBuffer;
    resetBuffer(rawBuffer, buffer, haystack);
    scanData(buffer, haystack);
    
    auto result = findLongest(buffer, haystack, "fox jumped, yeah!");
    assert(result.matchLength == "fox jumped".length);
    assert(result.matchOffset == 16);
    
    //short match, minimum not met.
    result = findLongest(buffer, haystack, "lazY");
    assert(!result.matchLength);
    assert(result.matchOffset == 0);
    
    //make sure it is going by largest rather than first
    haystack = "baaabaaaaaaabaaaa";
    resetBuffer(rawBuffer, buffer, haystack);
    scanData(buffer, haystack);

    result = findLongest(buffer, haystack, "baaaaaaaaaaaa");
    assert(result.matchLength == 8);
    assert(result.matchOffset == 4);
    
    //do a shorter haystack, ensure it doesn't try to scan from the haystack where it shouldn't.
    result = findLongest(buffer, haystack[0 .. 7], "baaaaaaaaaaaa");
    assert(result.matchLength == 4);
    assert(result.matchOffset == 0);
    
    //skip, for the sliding window.
    //check for first match (0)
    result = findLongest(buffer, haystack, "baaa");
    assert(result.matchLength == 4);
    assert(result.matchOffset == 0);

    //first offset can't work anymore since we're skipping it, has to be the 4-1
    result = findLongest(buffer, haystack, "baaa", 1);
    assert(result.matchLength == 4);
    assert(result.matchOffset == 4-1);

    //first & second won't work, third has to match
    result = findLongest(buffer, haystack, "baaa", 5);
    assert(result.matchLength == 4);
    assert(result.matchOffset == 12-5);
}

//actual compress function. no need for terminating 0
//buffer/RawBuffer able to be reused.
string compress(ref RawBuffer buffer, string haystack) {
    //minimum compress size needs to be met
    if (haystack.length < minCompress)
        return haystack;
    
    char[] compressed = new char[haystack.length];
    RawType[][AsciiChars] scanBuffer;
    ScanBuffer scanBuff = scanBuffer[];
    int used=minCompress, scanned, offset = minCompress;
    
    compressed[0 .. minCompress] = haystack[0 .. minCompress];  //can't compress the first min bytes
    resetBuffer(buffer, scanBuff, haystack);
    scanned = scanData(scanBuff, haystack);
    
    for (;;) {
        if (offset >= scanned) {
            //small buffers, has to be at the end
            if (offset < Window_LZ.maxOffset)
                break;
            //keep window we know about.
            haystack = haystack[offset-Window_LZ.maxOffset .. $];
            if (haystack.length == Window_LZ.maxOffset)
                break;
            resetBuffer(buffer, scanBuff, haystack);
            scanned = scanData(scanBuff, haystack);
            offset = Window_LZ.maxOffset;
            debug {
                writeln(haystack[offset .. $]);
                writeln(compressed[0 .. used]);
                writeln("rescanned!");
            }
        }
        
        auto found = findLongest(scanBuff, haystack[0 .. offset], haystack[offset .. $], offset - Window_LZ.maxOffset);
        
        if (found.matchLength > 0) {
            used += found.encode(cast(ubyte[]) compressed[used .. $]);
            offset += found.matchLength;
        } else {
            compressed[used++] = haystack[offset++];
        }
        
        debug{
            writeln(haystack[offset .. $]);
            writeln(compressed[0 .. used]);
        }
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
            lz.invertOffset();
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
    
    RawBuffer rb;
    
    auto comp = compress(rb, haystack);
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
        
        RawBuffer rb;
        auto compressed = compress(rb, cast(string) inf);
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

//Benchmarking!
        extern (C) char *id_compress(char *id, int idlen, size_t *plen);
unittest {
    //source of text: https://issues.dlang.org/show_bug.cgi?id=15831#c4
    string haystack = "E.s!(E.s!(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).s(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).Result).s(E.s!(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).s(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).Result).Result.foo()";
    
    import std.datetime;
    import std.conv;
    
//    RawBuffer rb = new RawType[Window];
    RawType[Window] rb1;
    RawBuffer rb = rb1[];
    
    auto f1 = (){
        compress(rb, haystack);
    };
    
    auto f2 = (){
        size_t plen;
        id_compress(cast(char*) haystack.ptr, haystack.length, &plen);
    };
    
    auto memoryhungry_compress = benchmark!(f1)(100_000);
    auto original_compress = benchmark!(f2)(100_000);

    writeln(memoryhungry_compress);
    writeln(original_compress);
}