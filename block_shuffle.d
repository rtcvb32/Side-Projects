import std.random;
import std.stdio;

enum Phases = 3;

//2 byte only, otherwise too large
struct Block16 {
    ushort[] trans, rev_trans;
    
    void init(int seed) {
        init(Random(seed));
    }
    
    void init(Random gen) {
        trans.length = 2^^16;
        rev_trans.length = 2^^16;
        foreach(i, ref v; trans)
            v=i & 0xffff;
        
//        auto gen = Random(seed);
        randomShuffle(trans, gen);
        //check for dud encoding? the odds are incredibly low...

        foreach(i, v; trans)
            rev_trans[v]= i & 0xffff;
    }
    
    void encrypt(void[] data) {
        assert(data != null);
        assert(trans != null);
        ushort[] d = cast(ushort[]) data;
        foreach(ref v; d)
            v = trans[v];
    }
    
    void decrypt(void[] data) {
        assert(data != null);
        assert(rev_trans != null);
        ushort[] d = cast(ushort[]) data;
        foreach(ref v; d)
            v = rev_trans[v];
    }
    
}

//increases the 16 to a 64 block by rearranging and xor steps?
struct Block64 {
    Block16 b16;
    ubyte[8] reorder;
    ubyte[8] rev_reorder;
    long[Phases] xor;
    
    void init(int seed) {
        foreach(i, ref re; reorder)
            re = i & 0xff;

        auto gen = Random(seed);
        repeat:
            randomShuffle(reorder[], gen);
            foreach(i, v; reorder)
                if (i == v)
                    goto repeat;

        //for reverse shuffling
        foreach(i, re; reorder)
            rev_reorder[re] = i & 0xff;
        
        foreach(ref x; xor) {
            x = cast(long)gen.front << 33; //33 since unsigned ints will have 31 bits not 32 for rng
            gen.popFront;
            x ^= cast(long)gen.front << 20; //overlap make sure every bit is covered
            gen.popFront;
            x ^= gen.front;
            gen.popFront;
        }
        
        b16.init(gen);
    }
    
    long shuffle(long val, ubyte[] reord) {
        l4u l, r;
        l.Long = val;
        
        foreach(i, re; reord)
            r.Byte[re] = l.Byte[i];
        
        return r.Long;
    }
    
    //encrypt a single block
    long encrypt(long val) {
        l4u v;
        v.Long = val;
        
        foreach(x; xor) {
            b16.encrypt(v.UShort[]);
            debug { writefln("%16x %s", v.Long, "-enc"); }
            v.Long = shuffle(v.Long, reorder);
            debug { writefln("%16x %s", v.Long, "-reorder"); }
            v.Long ^= x;
            debug { writefln("%16x %s", v.Long, "-xor"); }
        }
        return v.Long;
    }
    
    //encrypt array of blocks
    void encrypt(long[] val) {
        foreach(ref v; val) {
            debug { writefln("%16x %s", v, "-orig"); }
            v = encrypt(v);
        }
    }
    
    //encrypt a single block
    long decrypt(long val) {
        l4u v;
        v.Long = val;
        
        foreach_reverse(x; xor) {
            v.Long ^= x;
            v.Long = shuffle(v.Long, rev_reorder);
            b16.decrypt(v.UShort[]);
        }
        return v.Long;
    }
    
    //encrypt array of blocks
    void decrypt(long[] val) {
        foreach(ref v; val) {
            v = decrypt(v);
        }
    }
    
    //low level union stuff
    union l4u {
        long Long;
        ushort[4] UShort;
        ubyte[8] Byte;
    }
}

//take 9 of the 64bit blocks and encrypt it as a single block, 1 block is salt
struct Block576 {
    Block64[Phases] b64;
    ubyte[72] reorder, rev_reorder;
    
    void init(uint[] blockSeeds) {
        uint reorderSeed;
        assert (blockSeeds.length >= Phases);
        
        //generate block for each stage.
        foreach(i, ref b; b64) {
            reorderSeed ^= blockSeeds[i];
            b.init(blockSeeds[i]);
        }
        
        
        //generate reshuffle grid
        //each block distributes it's 8 to the other 8 leaving the current block
        //without any of it's original data.
        ubyte[9] pseudoblock;
        foreach(i, ref b; pseudoblock)
            b = (i*8-1) & 0xff;

//        reorder[0 .. 8] = pseudoblock[1 .. $];
        
        for(int i=0; i < 8; i++) {
            foreach(ref b; pseudoblock)
                b++;
            
            ubyte[8] tmpblock;
            tmpblock[0 .. i] = pseudoblock[0 .. i];
            tmpblock[i .. $] = pseudoblock[i + 1 .. $];
            
            reorder[i*8 .. (i+1)*8] = tmpblock[];
        }

        //covers x9 not covered before for final block.
        foreach(i, ref b; pseudoblock)
            b = (i*9) & 0xff;
        
        reorder[8*8 .. $] = pseudoblock[0 .. 8];
        
        //check that everything actually gets covered
        debug { //writeln(reorder);
            ubyte[72] x;
            foreach(i, b; reorder) {
                writefln("%d, %d -> %d, %d", i/8, i%8, b/8, b%8);
                x[b]=1;
            }
            
            //if all 1's, we're good.
            writeln(x);
        }
        
        debug { writeln(reorder); }
        auto gen = Random(reorderSeed);
        
        for(int i = 0; i < 9; i++)
            randomShuffle(reorder[i*8 .. (i+1)*8], gen);
            
        //generate unshuffle
        foreach(ubyte i, v; reorder)
            rev_reorder[v] = i;
            
        debug {
            writeln(reorder);
            writeln(rev_reorder);
        }
    }

    void shuffle(ubyte[] val, ubyte[] reord) {
        ubyte[8*9] tmp = val[];
        
        foreach(i, re; reord)
            val[re] = tmp[i];
    }
    
    void encrypt(long[] data) {
        while(data.length >= 9) {
            auto block = data[0 .. 9];
            foreach(ref b; b64) {
//                writeln(cast(void[])block);
                b.encrypt(block);
//                writeln(cast(void[])block);
                shuffle(cast(ubyte[]) block, reorder);
            }
            
            data = data[9 .. $];
        }
    }
    
    void decrypt(long[] data) {
        while(data.length >= 9) {
            auto block = data[0 .. 9];
            foreach_reverse(ref b; b64) {
//                writeln(cast(void[])block);
                shuffle(cast(ubyte[]) block, rev_reorder);
//                writeln(cast(void[])block);
                b.decrypt(block);
            }
            
            data = data[9 .. $];
        }
    }
}

// 2^576 random numbers, or 
struct Random576 {
    Block576 block;
    long[9] iter;
    long[9] values;
    int offset;
    enum empty = false; //i doubt we could exhaust this in our lifetimes...
                        //2.2259736132579408065445226891768e+174 values, double if you want ints instead of longs
    
    void seed(uint[] seeds) {
        block.init(seeds);
        
        /*
        if (iter.length <= seeds.length)
            iter[0 .. seeds.length] = seeds[];
        else
            iter[] = seeds[0 .. iter.length];
        */
        
        block.encrypt(iter);    //basically random starting point(encrypted).
        values[] = iter[];
        block.encrypt(values);  //random data
        offset = 0;
    }
    
    auto front() {
        return values[offset];
    }
    
    void popFront() {
        offset++;
        if (offset >= iter.length) {
            int i;
            do {
                iter[i]++;
            } while(i < iter.length && iter[i++] == 0);
            values[] = iter[];
            block.encrypt(values);  //randomize data
            offset = 0;
        }
    }
    

}

unittest {
    Block16 x;
    
    x.init(100);
    
    ushort[] t = [3,1,4,1,5,9,31415,9999,1,0,0,0,2,0,0,0];
    auto t2 = t.dup;
    
/*    writeln(t);
    x.encrypt(t);
    writeln(t);
    x.encrypt(t);
    writeln(t);
    x.decrypt(t);
    writeln(t);
    x.decrypt(t);
    writeln(t);
*/
    assert(t == t2); //should be the same
    x.encrypt(t);
    assert(t != t2); //encrypted garbage
    x.decrypt(t);
    assert(t == t2); //returned to normal
    
    Block64 b64;
    b64.init(100);
    
//    writeln(t);
    b64.encrypt(cast(long[]) t);
    assert(t != t2); //encrypted garbage
//    writeln(t);
    b64.decrypt(cast(long[]) t);
//    writeln(t);
    assert(t == t2); //returned to normal
    
    uint[] seed = [1,2,3]; //,4];//,5,6,7,8];
    
    Block576 b576;
    b576.init(seed);
    
    //writeln(b576);

    //sample text to encrypt/decrypt.
    char[] yogi = "Hey Booboo! Do you want to go steal a picnic basket? - Yogi bear__SALT__".dup;
    
    writeln(yogi);
//    writeln(cast(void[]) yogi);
    b576.encrypt(cast(long[]) yogi);
//    writeln();
    writeln(cast(void[]) yogi);
//    writeln();
    b576.decrypt(cast(long[]) yogi);
/*    writeln();
    writeln(cast(void[]) yogi);*/
    writeln(yogi);
    
    long[9] bignums;
    
    for(int i=0; i<10; i++) {
        bignums[0]=i;
        writeln(cast(void[]) bignums);
        b576.encrypt(bignums);
        writeln(cast(void[]) bignums);
        b576.decrypt(bignums);
        writeln(cast(void[]) bignums, "\n");
    }
    
    Random576 r576;
    
    r576.seed(seed);
    writeln(r576.iter);
    writeln(r576.values);
    for(int i=0; i<100; i++) {
        writefln("%x", r576.front);
        r576.popFront;
    }
}