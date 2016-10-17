module compress_min;
import std.stdio;

enum AsciiChars = 128;
enum Window = 512, minCompress = 3;
alias RawType = short;
alias RawBuffer = RawType[Window * AsciiChars];
alias ScanBuffer= RawType[][AsciiChars];

struct Buffer {
    ScanBuffer scan;
    RawBuffer raw;

    //by having a static size (buffer) we can avoid lot sof individual allocations, as well
    //as reuse the buffer.
    void reset() {
        RawType[] t = raw[]; //backup so we don't exhaust the real one by accident
        foreach(ref o; scan) {
            o = t[0 .. Window];
            t = t[Window .. $];
        }
    }
}

//scans and creats offsets for each character, up to window length.
//returns how many characters we've scanned.
int scanData(ref ScanBuffer saved, string haystack) {
    int[AsciiChars] lengths;
    int scanned = haystack.length;
    
    foreach(i, v; haystack) {
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
    Buffer buffer;
    buffer.reset();
    assert(scanData(buffer.scan, toScan) == toScan.length);
    
    //easiest test is probably the spaces.
    assert(buffer.scan[' '] == [3, 9, 15, 19, 26, 31, 35, 40]);
    
    //test length return
    ubyte[] empty = new ubyte[Window * 2];
    buffer.reset();
    assert(scanData(buffer.scan, cast(string) empty) == Window);
    assert(buffer.scan[0].length == Window);
}

struct Window_LZ {
    uint matchLength;
    uint matchOffset;

    enum ctrlChars = 31;
    enum maxOffset = 255-ctrlChars, maxLength = 127, blockSize = 2;
    
    //encodes to block of data. Returns the number of bytes it uses/exhausts
    int encode(ubyte[] outp) const {
        assert(outp.length >= blockSize);
        assert(matchOffset < maxOffset);
        
        //ripped from compress.c, and modified to take better advantage of distance?
        outp[0] = 0x80 | (matchLength-minCompress) & 0x7f;
        outp[1] = (matchOffset+ctrlChars) & 0xff;
        
        return blockSize;
    }
    
    //deodes from block. Returns number of bytes it needed
    int decode(const ubyte[] inp) {
        assert(inp.length >= 3);
        assert(inp[0] & 0x80); //ensure bit's on to show it's encoded. Not much else to say.
        
        matchLength = (inp[0] & 0x7f) + minCompress;
        matchOffset = inp[1] - ctrlChars;
        
        return blockSize;
    }
    
    void invertOffset() {
        matchOffset = maxOffset - matchOffset;
    }
}

unittest {
    Window_LZ t, t2;
    ubyte[3] buff;
    
    t = Window_LZ(31, 41);

    t.encode(buff);
    t2.decode(buff);
    
    assert(t == t2);
    
    //inverts for decoding
    t.invertOffset();
    assert(t.matchOffset == t2.maxOffset-t2.matchOffset);
    
    //converts back to normal
    t.invertOffset();
    assert(t == t2);
}

//find longest of needle in the haystack, but skips characters before skipBefore
//If the haystack is shorter than the scan's offsets it will short out.
Window_LZ findLongest(ref ScanBuffer scan, string haystack, string needle, int skipBefore = 0) {
    Window_LZ best;
    char need = needle[0];

    //canabalize our scanner...
    while(scan[need].length && scan[need][0] < skipBefore)
        scan[need] = scan[need][1 .. $];

    foreach(n; scan[need]) {
        if (n >= haystack.length)
            break;
        
        //1 & +1 since we know the first character matches for sure.
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
    Buffer buffer;
    buffer.reset();
    scanData(buffer.scan, haystack);
    
    auto result = findLongest(buffer.scan, haystack, "fox jumped, yeah!");
    assert(result.matchLength == "fox jumped".length);
    assert(result.matchOffset == 16);
    
    //short match, minimum not met.
    result = findLongest(buffer.scan, haystack, "laZY");
    
    //make sure it is going by largest rather than first
    haystack = "baaabaaaaaaabaaaa";
    buffer.reset();
    scanData(buffer.scan, haystack);

    result = findLongest(buffer.scan, haystack, "baaaaaaaaaaaa");
    assert(result.matchLength == 8);
    assert(result.matchOffset == 4);
    
    //do a shorter haystack, ensure it doesn't try to scan from the haystack where it shouldn't.
    result = findLongest(buffer.scan, haystack[0 .. 7], "baaaaaaaaaaaa");
    assert(result.matchLength == 4);
    assert(result.matchOffset == 0);
    
    //skip, for the sliding window.
    //check for first match (0)
    result = findLongest(buffer.scan, haystack, "baaa");
    assert(result.matchLength == 4);
    assert(result.matchOffset == 0);

    //first offset can't work anymore since we're skipping it, has to be the 4-1
    result = findLongest(buffer.scan, haystack, "baaa", 1);
    assert(result.matchLength == 4);
    assert(result.matchOffset == 4-1);

    //first & second won't work, third has to match
    result = findLongest(buffer.scan, haystack, "baaa", 5);
    assert(result.matchLength == 4);
    assert(result.matchOffset == 12-5);
}

//actual compress function. no need for terminating 0
//skipbytes is where the actual needle starts.
//Before that point it's effectively the dictionary.
string compress(string haystack, char[] compressed = null) { //, int skipbytes = 0
    //minimum compress size needs to be met
    if (haystack.length < minCompress)
        return haystack;
    
    Buffer buffer = void;
    if (!compressed.length)
        compressed = new char[haystack.length];
    int used = minCompress, scanned, offset = minCompress;
    
    compressed[0 .. minCompress] = haystack[0 .. minCompress];  //can't compress the first min bytes
    buffer.reset();
    scanned = scanData(buffer.scan, haystack);
    
    for (;;) {
        if (offset >= scanned) {
            //small buffers, has to be at the end
            if (offset < Window_LZ.maxOffset)
                break;
            //keep window we know about.
            haystack = haystack[offset-Window_LZ.maxOffset .. $];
            if (haystack.length == Window_LZ.maxOffset)
                break;
            buffer.reset();
            scanned = scanData(buffer.scan, haystack);
            offset = Window_LZ.maxOffset;
            debug {
                writeln(haystack[offset .. $]);
                writeln(compressed[0 .. used]);
                writeln("rescanned!");
            }
        }
        
        auto found = findLongest(buffer.scan, haystack[0 .. offset], haystack[offset .. $], offset - Window_LZ.maxOffset);
        
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

/*
//parallel compress, same as compress except breaks it into blocks.
//slightly worse compression but makes use of more cores, if it exceeds a certain length.
string parallelCompress(string haystack, int BlockSize = 4096) {
    import std.algorithm;
    import std.concurrency;
    import std.parallelism : totalCPUs;
    
    //too small, don't bother with parallel
    if (haystack.length <= BlockSize)
        return compress(haystack);

    string result;
    shared char[] shared_buffer;
    enum TIDMAX = 16;
    int maxThreads = min((haystack.length+(BlockSize-1)) / BlockSize, TIDMAX, totalCPUs);
    Tid[TIDMAX] tids;
    shared_buffer = new shared char[BlockSize * maxThreads];
    
    //spawn each of them and save them in order
    for(int i = 0; i < maxThreads; i++) {
        tids[i] = spawn(&spawnedCompress, thisTid, shared_buffer[i*BlockSize .. (i+1)*BlockSize]);
    }
    
    //work
//    while(hay
    
    //close them down
    for(int i = 0; i < maxThreads; i++) {
        send(tids[i], false);
    }

    return result;
}
*/

unittest {
    //source of text: https://issues.dlang.org/show_bug.cgi?id=15831#c4
    string haystack = "E.s!(E.s!(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).s(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).Result).s(E.s!(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).s(E.s!(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).s(E.s!(E.s!(int).s(int).Result).s(E.s!(int).s(int).Result).Result).Result).Result).Result.foo()";
    
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
