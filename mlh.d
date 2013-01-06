/*multi-level huffman & lzw
    Author: Ryan Cecil
    Date: Jan 06 2013
    Description: Incorporates useage of BitArray with LZW and huffman compression.
                Currently in development and API will likely change and be refactored.
                Highly Verbose for debugging, lacks full documentation and unittests.
                MLH (Multi-level-Huffman) lacks tree save/load functions.
                
                LZW may be wrong name; However it's a window sliding compression using
                a fixed size for window offset, size, and raw/unencoded characters.
                
                Be warned, it may look a little ugly...

    Compiler: Win32 dmd 2.061 beta
                
    LZW Usage:
---
    //rather than hello world, forces compression. Borrowed from zlib
    string helloString = "Hello Hello";

    //2 byte struct sequence by default
    void[] compressed = lzwCompress!()(helloString);
    writeln(compressed);   //raw compressed state

    string uncompressed = cast(string) lzwDeCompress!()(compressed);
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
    auto huffCode = huffmanScan(fox, 0);
    
    //update to use stats for compress/decompress 
    makeBitCodes(huffCode);
    auto compressed = mlhCompress(huffCode, fox, 0);
    writeln(encoded);   //encoded is BitArray. Raw saved data.
    
    auto decoded = cast(string) mlhDecompress(huffCode, encoded); //optional third argument, forced output length
    writeln(decoded);
---
*/

import std.algorithm;
import std.stdio;
import bitmanip; //std. missing for local test copy.
import std.conv;

//lzw implimentation. Not fully tested.
align(1) struct LZW(int window, int length) {
    static assert((length + window + 1) % 8 == 0, "must be in byte incriments");
    align(1) static struct Raw {
        mixin(bitfields!(
                bool, "isRaw", 1,
                uint, "length", window + length));
    }
    align(1) static struct Compacted {
        mixin(bitfields!(
                bool, "isRaw", 1,   //identical field required to work.
                uint, "window", window,
                uint, "length", length));
    }
    
    //union broke right now, so we use two structs instead. 2.061 beta
    union {
        Raw raw;
        Compacted compact;
    }
    bool isRaw() @property const @safe nothrow {
        return raw.isRaw;
    }
    string toString() const {
        if (isRaw)
            return "LZW-Raw{" ~ to!string(raw.length) ~ "}";
        //else
        return "LZW    {window: " ~ to!string(compact.window) ~ ", length:" ~ to!string(compact.length) ~ "}";
    }
    enum windowMin = 1;
    enum windowMax = (1 << window) - 1;
    enum lengthMin = LZW.sizeof + 1; //to actually get compression
    enum lengthMax = (1 << length) - 1;
    enum rawMax = 1 << (window + length);
}

static assert(LZW!(11,4).sizeof == 2, "not compacted as small struct");

void[] lzwCompress(int window = 11, int length = 4)(const void[] rawInput) {
    alias LZW!(window, length) Lzw;
    BitArray ba_data;
    Lzw lzw_data;
    ubyte[] input = cast(ubyte[]) rawInput;
    ubyte[] window;
    int rawStart = 0;
    
    ba_data.reserve(input.length * 10);

    foreach(int position; rawStart .. input.length) {
        if (position < rawStart)
            continue;

        lzw_data = windowSearch!Lzw(input, position);
        if (lzw_data.isRaw)
            continue;
        
        //uncompressed data
        bulkRawWrite!Lzw(ba_data, input[rawStart .. position]);
        debug {writeln(ba_data);}
        rawStart = position + lzw_data.compact.length;
        
        //compressed data
        ba_data.rawWrite(lzw_data);
        debug {writeln(lzw_data);writeln(ba_data);}
    }
    
    //append last unused raw
    bulkRawWrite!Lzw(ba_data, input[rawStart .. $]);
    
    void[] buff;
    ba_data.getBuffer(buff);
    return buff[0 .. (cast(int) ba_data.length / 8)];
}

T windowSearch(T)(const ubyte[] input, int position) {
    T lzw;
    lzw.raw.isRaw = true;

    foreach(i; 0 .. position) {
        if ((position-i) <= T.windowMax && //(position-i) >= T.lengthMin &&
                input[i .. i+T.lengthMin] == input[position .. position+T.lengthMin]) {
            T lzwCurrent;
            int length = T.lengthMin;
            lzwCurrent.compact.isRaw = false;
            lzwCurrent.compact.window = position - i;

            while(length < T.lengthMax &&
                    (position+length) < input.length &&
                    input[i+length] == input[position+length]) {
                length++;
            }
            lzwCurrent.compact.length = length;

            if (lzw.compact.length < lzwCurrent.compact.length)
                lzw = lzwCurrent;
        }
    }

    return lzw;
}

void bulkRawWrite(T)(ref BitArray ba, const(ubyte)[] input) {
    T lzw;

    //append last unused raw
    while(input.length) {
        lzw.raw.isRaw = true;
        lzw.raw.length = min(input.length, T.rawMax);
        ba.rawWrite(lzw);
        debug { writeln(lzw); }
        foreach(i, ub; input) {
            if (i >= lzw.raw.length)
                break;
            ba.rawWrite(ub);
            debug { writeln(ba[$-8 .. $].toString ~ " - " ~ to!string(cast(char) ub)); }
        }
        input = input[lzw.raw.length .. $];
    }
}

void[] lzwDeCompress(int window = 11, int length = 4)(const void[] input) {
    alias LZW!(window,length) Lzw;
    Lzw lzw;
    ubyte[] buffer;
    const BitArray ba = BitArray(cast(void[]) input);
    int offset;
    
    while(offset < ba.length) {
        offset += ba.rawRead(lzw, offset);
        writeln(lzw.toString());
        if (lzw.isRaw) {
            for(int i = lzw.raw.length; i; i--) {
                ubyte ub;
                offset += ba.rawRead(ub, offset);
                buffer ~= ub;
            }
        } else {
            //overlapping copy, likely long string like "*****"
            //can't have optimizations as they might not copy it right.screw it up.
            if (lzw.compact.window < lzw.compact.length) {
                buffer.length = buffer.length + lzw.compact.length;
                auto lhs = buffer[($-lzw.compact.window-lzw.compact.length) .. $];
                auto rhs = buffer[$-lzw.compact.length .. $];
                //memmove(rhs.ptr, lhs.ptr, lzw.compact.length);
                foreach(i, ref b; rhs)
                    b = lhs[i];
            } else
                buffer ~= buffer[($-lzw.compact.window) .. ($-lzw.compact.window+lzw.compact.length)];
        }
    }

    return cast(void[]) buffer;
}

unittest {
    string s = "Hello Hello";
    //2 byte struct sequence
    void[] x = lzwCompress!()(s);
    writeln(x);
    string y = cast(string) lzwDeCompress!()(x);
//    writeln(cast(string)y);
//    assert(x.length < s.length);
    assert(y == s);
    
    //one byte mini-compress
    void[] x2 = lzwCompress!(4,3)(s);
    writeln(x2);
    
    string y2 = cast(string) lzwDeCompress!(4,3)(x2);
//    writeln(cast(string)y);
    assert(x2.length < s.length);
    assert(y2 == s);
    
    string dups = "--------";
    auto x3 = lzwCompress!(4,3)(dups);
    writeln(x3);
    string y3 = cast(string) lzwDeCompress!(4,3)(x3);
    assert(y3 == dups);

    string dups2 = "abcabcabc";
    auto x4 = lzwCompress!(4,3)(dups2);
    writeln(x4);
    string y4 = cast(string) lzwDeCompress!(4,3)(x4);
    assert(y4 == dups2);

    string longBuffer = dups ~ dups ~ dups ~ dups;
    x3 = lzwCompress!(4,3)(longBuffer);
    writeln(x3);
    y3 = cast(string) lzwDeCompress!(4,3)(x3);
    assert(y3 == longBuffer);

    //currently breaks
    /*
    x3 = lzwCompress!()(longBuffer);
    writeln(x3);
    y3 = cast(string) lzwDeCompress!()(x3);
    assert(y3 == longBuffer);
    */
    
    }

//huffman starts here. unions/repacking may be done later.
struct Node {
    Node[] nextLevel;
    Tree decodeTree;        //just the start of the tree.
    BitArray[] bitCodes;    //0 level only
    bool singleElement;
    int singleElementValue;
    int value;
    int weight;
    
    enum maxValues = ubyte.max + 1;
    enum maxNodesAllocate = (maxValues * 2) - 1;
    
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
//                if (!s)
//                    s = tabs ~ "{ " ~ to!string(i) ~ " }\n";

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
                assert(bitCodes.length, this.toString());
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

void defaultAllocate(ref Node nodes) @property {
    nodes.nextLevel.length = Node.maxValues;

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

//cycles though and makes the huffman tree, as well as encodes them into bits.
void makeBitCodes(ref Node codes) {
    //stack storage until we know if we even need it, and only allocate what we
    //need to decode the tree.
    Tree[Node.maxNodesAllocate] wholeTree;
    Tree[] range;
    int emptyNode;  //according to 'range'
    bool level0 = true;
    
    wholeTree[0 .. $].setFrom(codes);
    
    foreach(i, ref n; wholeTree) {
        //no weights (left?) should be at end after being sorted
        if (!n.weight) {
            emptyNode = i;
            break;
        }

        if (n.node.nextLevel) {
            level0 = false;
            makeBitCodes(codes.nextLevel[n.node.value]); //n is a temp, can't use it.
            continue;
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
            codes.bitCodes.length = Node.maxValues;
            
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
    const ubyte[] inp = cast(const(ubyte[])) input;
    
    //if multiple levels, the first few aren't considered and aren't compressed.
    //if this is worked on and improved, this will be fixed. (it is in my C version)
    foreach(i, ub; inp[0 .. levels]) {
        output.rawWrite(ub);
        writeln(output);
    }
    
    //k, now we encode the remainder.
    foreach(i; 0 .. inp.length-levels) {
        writeln(cast(char[]) inp[i .. i+levels+1], " - ", huffman.encode(inp[i .. i+levels+1]));
        output ~= huffman.encode(inp[i .. i+levels+1]);
        writeln(output);
    }
    
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

//save tree

//load tree

unittest {
    auto x = huffmanScan("just a test.", 0);
    auto y = huffmanScan("just a test.", 1);
    writeln(y);
    
    makeBitCodes(x);
    writeln(x);
    makeBitCodes(y);
    writeln(y);
    
    auto encoded = mlhCompress(x, "just a test.", 0);
    writeln(encoded);
    auto encoded2 = mlhCompress(y, "just a test.", 1);
    writeln(encoded2);
    
    string decoded = cast(string) mlhDecompress(x, encoded);
    writeln(decoded);
    string decoded2 = cast(string) mlhDecompress(y, encoded2);
    writeln(decoded2);
    
    string fox = "The quick red fox jumps over the lazy dog";
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
}

void main(){}