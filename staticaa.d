import std.stdio;
import std.traits;

int getAALen(T)(T aa)
if (isAssociativeArray!T) {
    uint len = aa.keys.length;
    uint[] hashes, checks;
    
    hashes.length = len;
    
    foreach(i, k; aa.keys)
        hashes[i] = k.hashOf;
    
    uint repeat = 1;
    while(repeat--) {
        checks.length = len;
        checks[] = 0;
        
        foreach(h; hashes) {
            if (checks[h % len]) {
                repeat = 1;
                len += 1;
                break;
            } else
                checks[h % len] = h;
        }
    }
    
    return len;
}

struct StaticAA(K, V, int mapLen, int Len) {
    int[mapLen] off_map;
    uint[Len] keyHashes;
    K[Len] keys;
    V[Len] values;

    alias length = Len;
    enum INVALID = -1;
    
    this(V[K] start) {
        int i;
        off_map[] = INVALID;
        
        assert(Len == start.length);
        
        foreach(k, v; start) {
            uint hash = k.hashOf;
            int off = hash % mapLen;
            assert(off_map[off] == INVALID); //this should ensure there's no overlaps
            keyHashes[i] = hash;
            keys[i] = k;
            values[i] = v;
            off_map[off] = i;
            i++;
        }
    }
    
    ref V opIndex(const K ind) {
        uint hash = ind.hashOf;
        uint off = hash % mapLen;
        assert(off_map[off] != INVALID);
        assert(keyHashes[off_map[off]] == hash);
        
        return values[off_map[off]];
    }
    
    void opIndex(const K ind, V val) {
        uint hash = ind.hashOf;
        uint off = hash % mapLen;
        assert(off_map[off] != INVALID);
        assert(keyHashes[off_map[off]] == hash);
        
        values[off_map[off]] = val;
    }
    
    int opApply(int delegate(ref V) dg) {
        int ret;
        foreach(i, ref kh; keyHashes) {
            if (kh) {
                ret = dg(values[i]);
                if (ret)
                    break;
            }
        }
        return ret;
    }

    int opApply(int delegate(ref const K, ref V) dg) {
        int ret;
        foreach(i, ref kh; keyHashes) {
            if (kh) {
                ret = dg(keys[i], values[i]);
                if (ret)
                    break;
            }
        }
        return ret;
    }
}


unittest {
    import std.stdio;

    enum AA = ["one":1, "two":2, "three":3, "four":4, "five":5, "six":6, "seven":7, "eight":8, "nine":9, "zero":0];
    auto SAA = StaticAA!(string, int, getAALen(AA), AA.length)(AA);
//    writeln(getAALen(AA));

    //verify static and AA are identical with keys and values.
    foreach(k, v; AA) {
        assert(SAA[k] == v);
    }
    /*
    foreach(k; SAA.keys)
        if (k)
            writeln(k);

    foreach(v; SAA.values)
        if (v)
            writeln(v);

    foreach(k, v; AA)
        writeln(k, "\t", v);

    foreach(v; AA)
        writeln(v);
    */
    /*
    foreach(k, v; SAA)
        writeln(k, "\t", v);

    foreach(v; SAA)
        writeln(v);
    */
    /*
    writeln(AA);
    writeln(SAA);
    
    writeln(SAA.keys);
    writeln(SAA.values);
    */
    
    //make sure these compile
    foreach(k, v; SAA){}
    foreach(ref k, v; SAA){}
    foreach(k, ref v; SAA){}
    foreach(ref k, ref v; SAA){}
    foreach(v; SAA){}
    foreach(ref v; SAA){}
    
    assert(SAA["one"] == 1);
    SAA["one"] = 100;
    assert(SAA["one"] == 100);
    
}
