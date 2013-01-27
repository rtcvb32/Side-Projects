/*    Title: Minimum Compare Sort / MCS
     Author: Ryan Cecil
       Date: 1/26/2013
Description: While watching a lecture with Richard Buckland regarding sorting,
            it was brought up that no sorting algorithmn to date could sort data
            using the minimum number of compares. Due to increased complexity this
            makes sense.
             
             One of the examples of sorting was three cards, to ensure proper
            sorting it could be done between two and three compares/swaps. If you
            expanded that to a whole tree of groups of three, the effect should be
            similar if not identical to getting the minimum compare.
             
             If we were to change our concentration from sorting using ^2 and go
            to ^3, then certain assumptions can be made that you couldn't do before.
            These assumptions although don't make it a stable sort, it can decrease
            the required compares to do a proper sorting.
            
             In C++ this is not possible as overloads for each compare means all
            sorting is done via less-than compares. With D on the other hand, a
            result of it's relationship is preserved and can be used.
    
    Version notes:
    1/27/13 V0.2 - In transition for making notes of equality in compares.
*/
import std.stdio;
import std.algorithm;
import std.range;
import std.conv;
import std.array;    //maybe?
import std.exception;
import std.traits;

enum Reorder {
    empty=-1, l,
    lm, ml,
    lmr, lrm,
    mlr, mrl,
    rlm, rml,
    
/*  For those others reading, this is in preparation for a far more complex overhead to remove
    additional compares by use of assumption. Example: Normal compares for sorting goes
    lm, mr, (maybe lr), being left & middle, and middle and right, and maybe left and right.
    If left is less than middle, and middle is less than right, then left SHOULD be less than right.
    
    Now without equals being part of the equation, let's say we have 3 numbers 110 to do.
    left is not greater than middle, and middle is greater than right, but doesn't know if it's
    less than left. If we keep track of if the first compare was equal, then with only the
    two compares we know the answer is rlm, rather than needing the third compare.
    
    Now if the overhead slows it down or helps will only be told during profiling. 
    
    Combinations with equals noted. 1 & 2 refer to left/middle/right
    left & right checks unneeded after it's been re-ordered. This is for masking*/
    mask = 0x0f,        //for normal compares during reordering
    lm_1 = lm | 0x10, 
    lmr_1 = lmr | 0x10, lmr_2 = lmr | 0x20, lmr_3 = lmr | 0x30,

    lrm_1 = lrm | 0x10, lrm_2 = lrm | 0x20,
    mlr_1 = mlr | 0x10, mlr_2 = mlr | 0x20,
    mrl_1 = mrl | 0x10, mrl_2 = mrl | 0x20,
    rlm_1 = rlm | 0x10, rlm_2 = rlm | 0x20,
    rml_1 = rml | 0x10, rml_2 = rml | 0x20,
}

enum RangeState {
    hasValues = 0,  //Range's data is for values
    hasSubRanges,   //Range's data points to other ranges.
    thirdUnordered, //no need for second compare to know we use the middle value
    allUnordered,
    lmEqual = 0x10,
    mrEqual = 0x20,
    allEqual = 0x30,
}

///
struct MCS(T) {
    alias T Type;
    T[] area;           //slice of sortable data. May need different name.
    Range[] ranges;     //inner ranges data handling.
    bool recentSave;    //if save has been used, needs update during popFront
    int current = Reorder.empty; //current refers to the next element in the sorted list.

    //makes the range
    this(T[] input) {
        area = input;
        int length = input.length;
        int needed;
        
        while (length) {
            needed += (length / 3) + ((length % 3) ? 1 : 0);
            length /= 3;
        }
        assert(needed);
        ranges.reserve(needed);
        ranges ~= Range();
        ranges[0] = Range(this, 0, input);
        current = ranges[0].front;
    }
    
    ///
    bool empty() {
        return current == Reorder.empty;
    }
    
    ///
    ref T front()
    in {
        assert(!empty);
        assert(!area.empty);
    }
    body {
        return area[current];
    }
    
    ///
    void popFront()
    in {
        assert(!area.empty);
        assert(!empty);
    }
    body {
        //if recently saved, our ranges data is going to be wrong and point to the older data.
        //need to update those, otherwise as a forward range this fails.
        if (recentSave) {
            foreach(ref r; ranges)
                r.master = &this;
            recentSave = false;
        }
    
        ranges[0].popFront();
        current = ranges[0].front;
    }

    //postblit & opAssign may need additions

    
    MCS save() {
        MCS tmp = this;
        tmp.ranges = ranges.dup;
        tmp.recentSave = true;
        return tmp;
    }
    
    //for debugging
    void print() {
        writeln("MCS isEmpty?:\t   ", empty);
        writeln("MCS Current value (offset): ", current);
        if (current != Reorder.empty)
            writeln("MCS Current value (value):  ", area[current]);
        writeln("MCS Area (Values): ", area);
        writeln("MCS Ranges (structs):");
        foreach(i, r; ranges) {
            if (!r.empty) {
                writeln(i, ": {");
                r.print();
                writeln("}");
            }
        }
        

    }
    
    static struct Range {
        MCS* master;            //points to our master range (MCS)
        RangeState rangeState;  //determines how to use triRange

        //triRange, holds pointers to either the data or to the structs.
        int[3] triRange = [Reorder.empty, Reorder.empty, Reorder.empty];
        static Range emptyRange;

        //recursively create range splitting it up.
        //try and choose the most useful/diverse setup which
        //makes use of ^3
        this(ref MCS mcs, int offset, Type[] elems) {
            master = &mcs;
            
            if (elems.length <= 3) {
                setValues3(mcs, offset, elems);

                Reorder order = determineReorder(mcs.area, triRange);
                orderSort(order, triRange);
            } else {
                // 2/2 is better than 3/1 in this case.
                if (elems.length == 4)
                    setValues4(mcs, offset, elems);
                else
                    setValuesMore(mcs, offset, elems);

                Reorder order = determineReorderSubRange(mcs, triRange);
                orderSort(order, triRange); //sort real order
            }
        }


        //3 values/items or less
        void setValues3(ref MCS mcs, int offset, Type[] elems) {
            int len = elems.length, i = 0;
            for(;i < len; i++) {
                triRange[i] = offset + i;
            }
        }

        //simplifying handling of ranged ojects and referencing it.
        ref Range rangeRef(int spot)
        in {
            assert(rangeState);
        }
        body {
            if (triRange[spot] == Reorder.empty)
                return emptyRange;
            return (*master).ranges[triRange[spot]];
        }

        ref Range leftRange()   { return rangeRef(0); }
        ref Range middleRange() { return rangeRef(1); }
        ref Range rightRange()  { return rangeRef(2); }
        
        void shiftLeft() {
            triRange[0] = triRange[1];
            triRange[1] = triRange[2];
            triRange[2] = Reorder.empty;
        }
        
        //split it into 2/2 rather than 3/1, Should give slightly better performance.
        void setValues4(ref MCS mcs, int offset, Type[] elems) {
            rangeState = RangeState.hasSubRanges;
            triRange[0] = mcs.ranges.length;
            triRange[1] = triRange[0] + 1;
            
            mcs.ranges.length = mcs.ranges.length + 2;
            mcs.ranges[$-2] = Range(mcs, offset, elems[0 .. 2]);
            mcs.ranges[$-1] = Range(mcs, offset + 2, elems[2 .. $]);
        }
        
        void setValuesMore(ref MCS mcs, int offset, Type[] elems) {
                rangeState = RangeState.hasSubRanges;
                int third = elems.length / 3;
                int first = third, second = third;
                int power = 3;

                while(power < third)
                    power *= 3;
                
                //uneven remainder is thrown on third
                third += elems.length - (third * 3);

                //grab from third one,
                while (first < power && (second || third)) {
                    first++;
                    if (third)
                        third--;
                    else
                        second--;
                }
                
                //grab from second
                while (second < power && third) {
                    second++;
                    third--;
                }
                
                //should now be ready.
                
                //first
                triRange[0] = mcs.ranges.length;
                mcs.ranges ~= Range();
                mcs.ranges[$-1] = Range(mcs, offset, elems[0 .. first]);
                
                //second
                triRange[1] = mcs.ranges.length;
                mcs.ranges ~= Range();
                mcs.ranges[$-1] = Range(mcs, offset+first, elems[first .. (first+second)]);
                
                //third
                if (third) {
                    triRange[2] = mcs.ranges.length;
                    mcs.ranges ~= Range();
                    mcs.ranges[$-1] = Range(mcs, offset+first+second, elems[(first+second) .. $]);
                }
            }
      
        bool empty() {
            return triRange[0] == Reorder.empty;
        }
            
        //returns 'empty' when empty, for outer struct.
        //otherwise gives offset of the next sorted item
        int front() {
            return rangeState ? leftRange.front : triRange[0];
        }
        
        //gets value for compare purposes.
        ref T getValue() {
            return (*master).area[front];
        }
            
        void popFront()
        in {
            assert(!empty);
        }
        body {
            //if there's only one element or isn't a subrange copy/shift works.
            if ((rangeState == RangeState.hasValues) || middleRange.empty) {
                //left is gone, shift and no need to compare.
                shiftLeft();
            // haveSubRange && [1] not empty.
            } else {
                assert(!leftRange.empty);
                assert(!middleRange.empty);
                leftRange.popFront;
                
                //if after popping it's empty, shift or move up one level.
                if (leftRange.empty) {
                    //check for only one (to be) remaining
                    if (rightRange.empty) {
                        this = middleRange;
                        return;
                    }

                    shiftLeft();
                    //check if unordered, if it's ordered further checks aren't
                    //required for sorted order.
                    if (rangeState == RangeState.hasSubRanges)
                        return;
                }

                //previous assumption based on location, needs full resort
                if (rangeState == RangeState.thirdUnordered) {
                    auto order = determineReorderSubRange!(MCS, false)(*master, triRange);
                    orderSort(order, triRange);
                    rangeState = RangeState.hasSubRanges;
                } else if (leftRange.getValue > middleRange.getValue) {
                //determine if reorder is needed
//                    auto order = determineReorderSubRange!(MCS, true)(*master, triRange);
//                    orderSort(order, triRange);
                    
                    //drops from 31 max to 25
                    if (triRange[2] != Reorder.empty) {
                        orderSort(Reorder.rlm, triRange);
                        debug {assert(leftRange.getValue <= rightRange.getValue);}
                        rangeState = RangeState.thirdUnordered;
                    } else
                        orderSort(Reorder.ml, triRange);
                }
            }
        }
        
        void print() {
            writeln("\trangeState:", rangeState);
            writeln("\t", triRange);
            foreach(i; triRange) {
                if (i != Reorder.empty) {
                    if (!rangeState) {
                        writeln("\t", (*master).area[i]);
                    } else {
                        writeln("\t", 
                        (*master).area[
                            (*master).ranges[i].front]);
                    }
                }
            }
        }
    }
}

//so sub-levels
unittest {
    int[] list = [1,2,3,4,5,6,7,8,9];
    alias MCS!(int) MCSi;
    
    //one, empty and useless...
    auto mcs = MCSi(list[0 .. 1]);
    
    //get item, confirm, then destroy it.
    assert(!mcs.empty);
    assert(mcs.ranges[0].rangeState == RangeState.hasValues);
    assert(mcs.front == 1);
    mcs.popFront;
    assert(mcs.empty);
    
    //two
    mcs = MCSi(list[0 .. 2]);
    assert(!mcs.empty);
    assert(mcs.ranges[0].rangeState == RangeState.hasValues);
    assert(mcs.front == 1);
    mcs.popFront;
    assert(!mcs.empty);
    assert(mcs.front == 2);
    mcs.popFront;
    assert(mcs.empty);
    
    //three
    mcs = MCSi(list[0 .. 3]);
    assert(!mcs.empty);
    assert(mcs.ranges[0].rangeState == RangeState.hasValues);
    assert(mcs.front == 1);
    mcs.popFront;
    assert(!mcs.empty);
    assert(mcs.front == 2);
    mcs.popFront;
    assert(!mcs.empty);
    assert(mcs.front == 3);
    mcs.popFront;
    assert(mcs.empty);
    
    //four, two sets of two
    mcs = MCSi(list[0 .. 4]);
    int empty = cast(int) Reorder.empty;
    
    assert(mcs.ranges[0].rangeState);
    assert(mcs.ranges[0].triRange == [1,2,empty]);

    assert(!mcs.ranges[1].rangeState);
    assert(mcs.ranges[1].triRange == [0,1,empty]);

    assert(!mcs.ranges[2].rangeState);
    assert(mcs.ranges[2].triRange == [2,3,empty]);
    
    //five, should be three and two
    mcs = MCSi(list[0 .. 5]);
    assert(mcs.ranges[0].rangeState);
    assert(mcs.ranges[0].triRange == [1,2,empty]);

    assert(!mcs.ranges[1].rangeState);
    assert(mcs.ranges[1].triRange == [0,1,2]);

    assert(!mcs.ranges[2].rangeState);
    assert(mcs.ranges[2].triRange == [3,4,empty]);

    //test range
    foreach(i, value; list[0 .. 5]) {
        assert(!mcs.empty);
        assert(value == mcs.front);
        mcs.popFront();
    }

    assert(mcs.empty);  //should be empty now.
    
    //nine, equal across the board
    mcs = MCSi(list[0 .. 9]);
    assert(mcs.ranges[0].rangeState);
    assert(mcs.ranges[0].triRange == [1,2,3]);

    assert(!mcs.ranges[1].rangeState);
    assert(mcs.ranges[1].triRange == [0,1,2]);
    assert(!mcs.ranges[2].rangeState);
    assert(mcs.ranges[2].triRange == [3,4,5]);
    assert(!mcs.ranges[3].rangeState);
    assert(mcs.ranges[3].triRange == [6,7,8]);

    //check it's doing the initial sort.
    list = [0,1,2,
            3,5,4,
            8,6,7,
            11,10,9];   //also second layer
    mcs = MCSi(list);

    foreach(i, l; list) {
        assert(mcs.front == i);
        mcs.popFront();
    }
    assert(mcs.empty);
    
    //now for them to be randomly placed.
    list = [5,4,0,
            8,1,3,
            7,6,2];

    mcs = MCSi(list);
    foreach(i, l; list) {
//        mcs.print();
//        writeln(i, "): ", mcs.front);
        assert(mcs.front == i);
        mcs.popFront();
    }
    assert(mcs.empty);
}

//takes calls determineReorder but handles the indirection issue.
Reorder determineReorderSubRange(T, bool skipFirst = false)(T mcs, ref const int[3] subRangeOffsets) {
    int[3] valueOffsets = [Reorder.empty, Reorder.empty, Reorder.empty];
    foreach(index, offset; subRangeOffsets) {
        if (offset != Reorder.empty)
            valueOffsets[index] = mcs.ranges[offset].front;
    }

    return determineReorder!(mcs.Type, skipFirst)(mcs.area, valueOffsets);
}

//hopefully will specify all the reordering information with minimal queries.
//you only need 1 compare to sort 2 items, and 2-3 for 3 items. no more than 5 for 4.
//added complexity to reduce to minimum sorting comparisons and decisions.
Reorder determineReorder(T, bool skipFirst = false)(T[] elems, ref const int[3] offsets)
if (!skipFirst)
in {
    assert(!elems.empty, "determineReorder: empty block");
    assert(offsets[0] != Reorder.empty, "determineReorder: cannot determine order of empty block");
}
body { with(Reorder) {
    if (offsets[1] == Reorder.empty)
        return Reorder.l;
    
    int[3] cmpResults;

    T* left = &elems[offsets[0]];
    T* middle = &elems[offsets[1]];
    cmpResults[0] = (*left).opCmp(*middle);
    
    Reorder state = (cmpResults[0] > 0) ? ml: lm;

    if (offsets[2] != Reorder.empty) {
        //right only applicable on 3's
        T* right = &elems[offsets[2]];

        if (state == lm) {
            //lmr, //lrm, //mrl,
            cmpResults[1] = (*middle).opCmp(*right);
            if (cmpResults[1] > 0) {
                //lrm, //mrl,
                cmpResults[2] = (*left).opCmp(*right);
                if (cmpResults[2] > 0)
                    state = mrl;
                else
                    state = lrm;
            } else {
                debug {
                    assert((*right) >= (*left));    //assure it's correct.
                }
                state = lmr;
            }
        } else    //since it's separated out..
            state = determineReorder!(T, true)(elems, offsets);
    }

    return state; //already compared so it's 2 elements
}}

//same as above, except it assumes 0 and 1 already are backwards. It's when
//you compare to see if left > right, if true you don't need to compare again.
Reorder determineReorder(T, bool skipFirst)(T[] elems, ref const int[3] offsets)
if (skipFirst)
in {
    assert(!elems.empty);
}
body { with(Reorder) {
    Reorder state = ml;
    int[3] cmpResults = [1, 0, 0]; //0 (1+2) already false

    if (offsets[2] == Reorder.empty)
        return Reorder.ml;
    
    T* left = &elems[offsets[0]];
    T* middle = &elems[offsets[1]];
    T* right = &elems[offsets[2]];
    
    cmpResults[2] = (*left).opCmp(*right);

    //mlr, //rml //rlm,
    if (cmpResults[2] > 0) {
        //rml //rlm,
        cmpResults[1] = (*middle).opCmp(*right);
        if (cmpResults[1] > 0)
            state = rml;
        else
            state = rlm;
    } else {
        //mlr,
        debug {
            assert((*right) >= (*middle));    //assure it's correct.
        }
        state = mlr;
    }
    return state;
}}

//for compares with numerics to help with assumptions of order.
int opCmp(T)(T l, T r) if (isNumeric!(T)) { return l - r; }
unittest {
    int a, b;
    assert(a.opCmp(b) == 0);
    a = 1;
    assert(a.opCmp(b) > 0);
    assert(a.opCmp(b) >= 0);
    b = 2;
    assert(a.opCmp(b) < 0);
    assert(a.opCmp(b) <= 0);
    assert(a.opCmp(b) != 0);
}

void orderSort(Reorder order, int[] offsets)
in {
    assert(offsets.length >= 2);
    assert(offsets.length <= 3);
}
body { with(Reorder) {
    //check for no changes needed
    enum { left = 0, middle = 1, right = 2 }
    switch (order & Reorder.mask) {
        case rlm:   //becomes mlr, then lmr
            swap(offsets[left], offsets[right]);

        case ml, mlr:
            swap(offsets[left], offsets[middle]);
            break;

        case rml:
            swap(offsets[left], offsets[right]);
            return;

        case mrl: //becomes lrm, then lmr
            swap(offsets[left], offsets[right]);
        
        case lrm:
            swap(offsets[middle], offsets[right]);
            break;
        
        case empty, l, lm, lmr:
            break;
            
        default:
            assert(0, text("Unhandled case:", order, " (", (order&Reorder.mask), ")"));
    }
}}


unittest {
    Int[3][] permutation;
    
    //all permutations, allows to check for stability.
    foreach(a; 0 .. 3)
        foreach(b; 0 .. 3)
            foreach(c; 0 .. 3) {
                permutation ~= Int.makeInt(a,b,c);
    }
    
    foreach(ref pm; permutation) {
        Int.count = 0;
        int[3] ordering = [0,1,2];
        Reorder order = determineReorder(pm[], ordering);
        debug { write(pm, "\t", order, "\t - "); }
        Int[] tmp = pm[0 .. $];
        orderSort(order, ordering[0 .. $]);
        
        //forcibly reorder for test.
        Int[3] pm2 = [
            pm[ordering[0]],
            pm[ordering[1]],
            pm[ordering[2]],
        ];
        
        Reorder order2 = determineReorder(pm[], ordering);
        
        debug { writeln(ordering, " - ", order2 ," compares = ", Int.count); }

        Int l = pm2[0];
        Int m = pm2[1];
        Int r = pm2[2];
        
        assert(order2 == Reorder.lmr);                          //what our reorder says
        assert(l <= m, "Threeway sort/swap failed! l > m");     //ensure integrity of compares vs reorder output
        assert(m <= r, "Threeway sort/swap failed! m > r");
        assert(l <= r, "Threeway sort/swap failed! l > r");

        /*
        //check stability of sort when identical values are present.
        if (l == m)
            assert(l.stable < m.stable, "Not stable! l == m, but not l.stable < m.stable");
        if (m == r)
            assert(m.stable < r.stable, "Not stable! m == r, but not m.stable < r.stable");
        if (l == r)
            assert(l.stable < r.stable, "Not stable! l == r, but not l.stable < r.stable");
        */
    }
    
    //test vs 2 values now.
    Int[2] perm = [Int(0,0), Int(1,1)];
    enum empty = cast(int) Reorder.empty;
    int[3] ordering = [0, 1, empty];
    
    Reorder order = determineReorder(perm[], ordering);
    assert(order == Reorder.lm, "2 part ordering (in order) failed");
    
    //reorder it test (do nothing)
    orderSort(order, ordering[0 .. $]);
    assert(ordering == [0, 1, empty]);
    
    //backwards
    perm = [Int(1,0), Int(0,1)];
    ordering = [0, 1, empty];

    order = determineReorder(perm[], ordering);
    assert(order == Reorder.ml, "2 part ordering (backwards) failed");
    orderSort(order, ordering[0 .. $]);
    assert(ordering == [1, 0, empty]);
    
    //equal values
    perm = [Int(10,0), Int(10,1)];
    ordering = [0, 1, empty];

    order = determineReorder(perm[], ordering);
    assert(order == Reorder.lm, "2 part ordering (same) failed answer");
    orderSort(order, ordering[0 .. $]);
    assert(ordering == [0, 1, empty], "identical values ordering failed stability test");
}

unittest {
    //brute force x elements.
    enum elements = 9;
    ulong combinations;
    Int[elements] toSort;
    Int[elements] sorted, sorted2;
    ulong lazyCount;
    ulong runCount;
    ulong maxCount; //noted for any individual compare
    
    void genElements(Int[] toFill) {
        for (int i = 0; i<elements; i++) {
            toFill[0] = Int(i, elements-toFill.length);

            if (toFill.length > 1) {
                genElements(toFill[1 .. $]);
            } else {
                //help identify if failed to sort/reorder correctly
                sorted[] = Int(-1);
                Int.count = 0;
                auto mcs = MCS!Int(toSort);
                int lazyCnt = Int.count;    //minimum setting up the structures
                int i2;
                
                foreach(x; mcs.save) {
                    sorted[i2] = x;
                //if identical, check stability order.
//                    if (i2 && (x.i == sorted[i2 - 1].i))
//                        assert(x.stable > sorted[i2-1].stable);
                    i2++;
                }
                

                lazyCount += lazyCnt;
                runCount += Int.count;
                combinations++;

                //only print larger compares, to find edge cases to improve on.
                if (maxCount < Int.count)
                    writeln(toSort, "-", sorted ," compares: ", lazyCnt, "/", Int.count,
                            //max used since the if check can be removed.
                            " : ", max(maxCount, Int.count));

                maxCount = max(maxCount, Int.count);
                sorted2[] = toSort[];
                sorted2.sort;
                assert(sorted2 == sorted);  //ensures data integrity & order
            }
        }
    }

    genElements(toSort);

    writeln("Lazy Average: ", (cast(double) lazyCount) / combinations);
    writeln("     Average: ", (cast(double) runCount)  / combinations);
}
/* - currently
$ ./mcs.exe
[0, 0, 0, 0, 0, 0, 0, 0, 0]-[0, 0, 0, 0, 0, 0, 0, 0, 0] compares:  8/12 : 12
[0, 0, 0, 0, 0, 0, 0, 1, 0]-[0, 0, 0, 0, 0, 0, 0, 0, 1] compares:  9/13 : 13
[0, 0, 0, 0, 0, 1, 0, 0, 0]-[0, 0, 0, 0, 0, 0, 0, 0, 1] compares:  8/14 : 14
[0, 0, 0, 0, 0, 1, 0, 1, 0]-[0, 0, 0, 0, 0, 0, 0, 1, 1] compares:  9/15 : 15
[0, 0, 0, 0, 1, 0, 0, 1, 0]-[0, 0, 0, 0, 0, 0, 0, 1, 1] compares: 10/16 : 16
[0, 0, 0, 1, 2, 1, 0, 2, 0]-[0, 0, 0, 0, 0, 1, 1, 2, 2] compares: 11/17 : 17
[0, 0, 1, 0, 0, 1, 0, 2, 0]-[0, 0, 0, 0, 0, 0, 1, 1, 2] compares:  9/18 : 18
[0, 0, 1, 0, 1, 0, 0, 2, 0]-[0, 0, 0, 0, 0, 0, 1, 1, 2] compares: 10/19 : 19
[0, 0, 1, 1, 2, 1, 0, 2, 0]-[0, 0, 0, 0, 1, 1, 1, 2, 2] compares: 11/20 : 20
[0, 0, 3, 1, 3, 1, 0, 2, 0]-[0, 0, 0, 0, 1, 1, 2, 3, 3] compares: 11/21 : 21
[0, 1, 3, 0, 3, 2, 0, 4, 2]-[0, 0, 0, 1, 2, 2, 3, 3, 4] compares: 10/22 : 22
[0, 1, 3, 1, 4, 2, 0, 3, 2]-[0, 0, 1, 1, 2, 2, 3, 3, 4] compares: 11/23 : 23
[0, 3, 1, 1, 4, 2, 0, 3, 2]-[0, 0, 1, 1, 2, 2, 3, 3, 4] compares: 12/24 : 24
Lazy Average: 10.4107
     Average: 18.7811
*/

//an int, but retains counts for sorting comparison.
struct Int {
    static int count;
    static int saved;
    
    int i;
    int stable;    //hidden index referring to which order these were assigned.
    
    int opEquals(const Int rhs) const {
        return (i == rhs.i);
    }
    
    int opCmp(const Int rhs) const {
        count++;
        return i - rhs.i;
    }
    
    static Int[3] makeInt(int a, int b, int c) {
        Int[3] t = [Int(a, 0), Int(b, 1), Int(c, 2)];
        return t;
    }
    
    //mostly for comparing between portions
    static void save() {
        saved = count;
    }
    
    static void clear() {
        count = saved = 0;
    }
    
    //uncluttered debug output.
    string toString() {
        return to!string(i);
    }
}

int main() {
    return 0;
}