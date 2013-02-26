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

import std.algorithm;
import std.stdio;
import bitmanip; //std. missing for local test copy.
import std.conv;

//lz77 implimentation. Not fully tested.
//merely a struct format
align(1) struct LZ(int windowBits, int lengthBits) {
    static assert((lengthBits + windowBits + 1) % 8 == 0, "must be in byte incriments");
    align(1) static struct Raw {
        mixin(bitfields!(
                bool, "isRaw", 1,
                uint, "length", windowBits + lengthBits));
    }
    align(1) static struct Compacted {
        mixin(bitfields!(
                void, "", 1,
                uint, "window", windowBits,
                uint, "length", lengthBits));
    }
    
    //Forward reference issues if we try to convert to all the data within
    //the union. v2.061
    union {
        private Raw raw;
        private Compacted compact;
    }
    
    bool isRaw() @property const @safe pure nothrow {
        return raw.isRaw;
    }
    
    void isRaw(bool r) @property @safe pure nothrow {
        raw.isRaw = r;
        raw.length = 0;
    }
    
    uint length() @property const @safe pure nothrow {
        return raw.isRaw ? raw.length : compact.length;
    }
    
    void length(uint len) @property @safe pure nothrow {
        if (raw.isRaw)
            raw.length = len;
        else
            compact.length = len;
    }

    uint window() @property const @safe pure nothrow {assert(!isRaw); return compact.window;}
    void window(uint pos) @property @safe pure nothrow {assert(!isRaw); compact.window = pos;}

    string toString() const {
        if (isRaw)
            return "LZ-Raw {" ~ to!string(raw.length) ~ "}";
        //else
        return "LZ    {window: " ~ to!string(compact.window) ~ ", length:" ~ to!string(compact.length) ~ "}";
    }

    enum windowMin = 1;
    enum windowMax = (1 << windowBits) - 1;
    enum lengthMin = LZ.sizeof + 1; //to actually get compression
    enum lengthMax = (1 << lengthBits) - 1;
    enum rawMax = (1 << (windowBits + lengthBits)) - 1;
}

static assert(LZ!(11,4).sizeof == 2, "not compacted as small struct");

///
void[] lzCompress(int window = 11, int length = 4)(const void[] rawInput) {
    alias LZ!(window, length) Lz;
    BitArray ba_data;
    Lz lz_data;
    ubyte[] input = cast(ubyte[]) rawInput;
    ubyte[] window;
    int rawStart = 0;
    
    ba_data.reserve(input.length * 10);

    foreach(int position; rawStart .. input.length) {
        if (position < rawStart)
            continue;

        lz_data = windowSearch!Lz(input, position);
        if (lz_data.isRaw)
            continue;
        
        //uncompressed data
        bulkRawWrite!Lz(ba_data, input[rawStart .. position]);
        rawStart = position + lz_data.length;
        
        //window/compressed data
        ba_data.rawWrite(lz_data);
    }
    
    //append last unused raw
    bulkRawWrite!Lz(ba_data, input[rawStart .. $]);
    
    void[] buff;
    ba_data.getBuffer(buff);
    return buff[0 .. (cast(int) ba_data.length / 8)];
}

//rough implimentations, scans the window area for the longest possible match
//returns the position & length, or isRaw
T windowSearch(T)(const ubyte[] input, int position) {
    T lz;
    lz.isRaw = true;

    foreach(i; 0 .. position) {
        if ((position-i) <= T.windowMax && (position+T.lengthMin) < input.length &&
                input[i .. i+T.lengthMin] == input[position .. position+T.lengthMin]) {
            T lzCurrent;
            int length = T.lengthMin;
            lzCurrent.isRaw = false;
            lzCurrent.window = position - i;

            while(length < T.lengthMax &&
                    (position+length) < input.length &&
                    input[i+length] == input[position+length]) {
                length++;
            }
            lzCurrent.length = length;

            if (lz.length < lzCurrent.length)
                lz = lzCurrent;
        }
    }

    return lz;
}

//takes whole length of input and prepends a 'raw' LZ block.
//repeats until full raw data is written
void bulkRawWrite(T)(ref BitArray ba, const(ubyte)[] input) {
    T lz;

    //append last unused raw
    while(input.length) {
        lz.isRaw = true;
        lz.length = min(input.length, T.rawMax);
        ba.rawWrite(lz);

        foreach(i, ub; input) {
            if (i >= lz.length)
                break;
            ba.rawWrite(ub);
        }
        input = input[lz.length .. $];
    }
}

void[] lzDeCompress(int window = 11, int length = 4)(const void[] input) {
    alias LZ!(window,length) Lz;
    Lz lz;
    ubyte[] buffer;
    const BitArray ba = BitArray(cast(void[]) input);
    int offset;
    
    while(offset < ba.length) {
        offset += ba.rawRead(lz, offset);
        if (lz.isRaw) {
            for(int i = lz.raw.length; i; i--) {
                ubyte ub;
                offset += ba.rawRead(ub, offset);
                buffer ~= ub;
            }
        } else {
            //overlapping copy, likely long string like "*****"
            //can't have optimizations as they might not copy it right.screw it up.
            if (lz.compact.window < lz.compact.length) {
                buffer.length = buffer.length + lz.compact.length;
                auto lhs = buffer[($-lz.compact.window-lz.compact.length) .. $];
                auto rhs = buffer[$-lz.compact.length .. $];
                
/*              memmove/memcpy failed with sections so close to eachother.
                import std.c.string;
                memcpy(rhs.ptr, lhs.ptr, lz.compact.length);
*/
                foreach(i, ref b; rhs)
                    b = lhs[i];
            } else
                buffer ~= buffer[($-lz.compact.window) .. ($-lz.compact.window+lz.compact.length)];
        }
    }

    return cast(void[]) buffer;
}

unittest {
    string s = "Hello Hello";
    //2 byte struct sequence
    void[] x = lzCompress!()(s);
    writeln(x);
    string y = cast(string) lzDeCompress!()(x);
//    writeln(cast(string)y);
//    assert(x.length < s.length);
    assert(y == s);
    
    //one byte mini-compress
    void[] x2 = lzCompress!(4,3)(s);
    writeln(x2);
    
    string y2 = cast(string) lzDeCompress!(4,3)(x2);
//    writeln(cast(string)y);
    assert(x2.length < s.length);
    assert(y2 == s);
    
    string dups = "--------";
    auto x3 = lzCompress!(4,3)(dups);
    writeln(x3);
    string y3 = cast(string) lzDeCompress!(4,3)(x3);
    writeln(y3);
    writeln(dups);
    assert(y3 == dups);

    string dups2 = "abcabcabc";
    auto x4 = lzCompress!(4,3)(dups2);
    writeln(x4);
    string y4 = cast(string) lzDeCompress!(4,3)(x4);
    assert(y4 == dups2);

    string longBuffer = dups ~ dups ~ dups ~ dups;
    x3 = lzCompress!(4,3)(longBuffer);
    writeln(x3);
    y3 = cast(string) lzDeCompress!(4,3)(x3);
    assert(y3 == longBuffer);

    //currently breaks
    x3 = lzCompress!()(longBuffer);
    writeln(x3);
    y3 = cast(string) lzDeCompress!()(x3);
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

void main(){}