import std.stdio;
import compress : compress_big = compress;
import compress_min : compress_min = compress;
import core.memory;
import core.thread;
import std.range;
import std.file;
import std.datetime;
import std.conv;

//Benchmarking!
extern (C) void c_free(void *ptr);
extern (C) char *id_compress(char *id, int idlen, size_t *plen);
extern (C) char *id_compress_reduced(char *id, int idlen, size_t *plen);
alias RawType = int;
alias RawBuffer = RawType[];
alias ScanBuffer= RawType[][];

void main(string[] args) {
    //source of text: https://issues.dlang.org/show_bug.cgi?id=15831#c4
    static string haystack = "testexpansion.s!(testexpansion.s!(testexpansion.s!(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).s(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).Result).s(testexpansion.s!(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).s(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).Result).Result).s(testexpansion.s!(testexpansion.s!(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).s(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).Result).s(testexpansion.s!(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).s(testexpansion.s!(testexpansion.s!(int).s(int).Result).s(testexpansion.s!(int).s(int).Result).Result).Result).Result).Result.foo()";
    static RawBuffer rb;

    auto f1 = () {
        compress_min(haystack);
    };
    
    auto f2 = () {
        compress_big(haystack);
    };
    
    auto f3 = () {
        size_t plen;
        id_compress(cast(char*) haystack.ptr, haystack.length, &plen);
    };
    
    auto f4 = () {
        size_t plen;
        id_compress_reduced(cast(char*) haystack.ptr, haystack.length, &plen);
    };
    
    
    
//    TickDuration min_compress, memoryhungry_compress, original_compress, original_reduced_compress;
    struct D {
        void function() del;
        string name;
        int order;
        TickDuration tick; 
        void call() {del();}
    }
    auto data = [
        D(f3, "id_compress", 3),
        D(f4, "id_compress_reduced", 4),
        D(f1, "min_compress", 1),
        D(f2, "big_compress", 2),
    ];
    
    if (args.length > 1) {
        int i = to!int(args[1]);
        data = [data[i]];
    }
    if (args.length > 2) {
        //load x file for an input for testing.
        haystack = cast(string) read(args[2]);
    }
    
    int samples = 100;
    int rounds = 100000 / samples;
    int baseline = 0;
    
    if (args.length > 3) {
        rounds = to!int(args[3]);
    }
    
    GC.reserve(1024^^3);    //a gig reserve?
    GC.disable();
    foreach(ref job; data) {
//        writeln(job);
//        GC.collect();
        thread_joinAll();
        auto indirectCall() { job.del(); } //because telling it a function location requires this?
        job.tick = benchmark!(indirectCall)(rounds)[0];
        foreach(i; iota(0, samples)) {
            auto current = benchmark!(indirectCall)(rounds)[0];
            if (current < job.tick)
                job.tick = current;
        }
    }

    if (args.length > 1) {
        writef("%20s: %4dms %8dμs, ", data[0].name, data[0].tick.msecs, data[0].tick.usecs);
    } else {
        foreach(job; data) {
            writefln("%20s: %dms %dμs, %g", job.name, job.tick.msecs, job.tick.usecs, cast(double) job.tick.usecs / cast(double) data[baseline].tick.usecs);
        }
    }
    string mh;
    char *id;
    
    write(haystack.length, "\t(source size) -> ");
    
    foreach(job; data) {
        size_t plen;
        switch(job.order) {
            case 3:
                id=id_compress(cast(char*) haystack.ptr, haystack.length, &plen);
                mh = cast(string) id[0 .. plen];
//                writeln(mh.length, "\t", mh);
                break;
            case 4:
                id=id_compress_reduced(cast(char*) haystack.ptr, haystack.length, &plen);
                mh = cast(string) id[0 .. plen];
//                writeln(mh.length, "\t", mh);
                break;
            case 2:
                mh=compress_big(haystack);
//                writeln(mh.length, "\t", mh);
                break;
            case 1:
                mh=compress_min(haystack);
//                writeln(mh.length, "\t", mh);
                break;
            default:
        }
        writeln(mh.length);
    }
}