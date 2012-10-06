/**
	Author: Era Scarecrow <rtcvb32@yahoo.com>
	
	Basic handling of flags and their types. Since you can't go int to ENUM, this
	structure will set, and test flags appropriately. As there's only one state (integral)
	you can (hopefully) forcibly cast an int into HandleFlags so long as the size is the same.

---
	enum ETEST {none = 0, one = 1, two = 2, three = 3, four = 4}
	HandleFlags!(ETEST, int) ftest;

	ftest.setFlag(ETEST.two);	//check flag setting
	assert(ftest.state == 2);	//int and enum compares.
	assert(ftest.state == ftest.two);  //ftest.one and ftest.Enum.one accessible and shortcuts

	//shortcuts avaliable.
	assert(ftest.one == ETEST.one && ftest.Enum.one == ETEST.one);

	ftest.setFlag(ftest.four);	//set a second flag
	assert(ftest.state == 6);	//2+4, no 6 enum.

	//Flags can also encompass multiple bits. in this case all bits returned with an AND must match the flag.
	//using checkAll, all of the flags MUST be true in order to be true.
	with(ftest.Enum) {
		ftest.state = 0;
		ftest.setFlag(one);
		assert(!ftest.checkAll(one, two, three));  //can't be true, only 1 is there

		ftest.setFlag(two);
		assert(ftest.checkAll(one, two, three));   //must be true, since 1+2 includes 3.
	}
---

*/
module smartmerged.flags;

import std.traits;
import std.exception;
import std.traits;

import std.stdio : writeln;
import std.conv;

@trusted:

unittest{
	enum ETEST {none = 0, one = 1, two = 2, three = 3, four = 4}
	HandleFlags!(ETEST, int) ftest;

	ftest.setFlag(ETEST.two);	//check flag setting
	assert(ftest.state == 2);	//int and enum compares.
	assert(ftest.state == ftest.two);  //ftest.one and ftest.Enum.one accessible and shortcuts

	//shortcuts avaliable.
	assert(ftest.one == ETEST.one && ftest.Enum.one == ETEST.one);

	ftest.setFlag(ftest.four);	//set a second flag
	assert(ftest.state == 6);	//2+4, no 6 enum.

	//Flags can also encompass multiple bits. in this case all bits returned with an AND must match the flag.
	//using checkAll, all of the flags MUST be true in order to be true.
	with(ftest.Enum) {
		ftest.state = 0;
		ftest.setFlag(one);
		assert(!ftest.checkAll(one, two, three));  //can't be true, only 1 is there

		ftest.setFlag(two);
		assert(ftest.checkAll(one, two, three));   //must be true, since 1+2 includes 3.
	}
}

class OverlapError : Error {
	this(string msg = null) {
		if (msg !is null)
			super("Warning! Overlap possible, This could be order specific for flags behavior.\n\n" ~ msg);
		else
			super("Warning! Overlap possible, This could be order specific for flags behavior.");
	}
}

/**
 * Returns any potential pairs of overlapping enum flags.
 */
auto getOverlaps(E...)(E flags)
if (is(typeof(flags[0]) == enum)) {
	alias typeof(flags[0]) F;
	F[2][] pairs;
	static if (flags.length > 1) {
		foreach(i, l; flags) {
			foreach(r; flags[i+1 .. $]) {
				if (l & r) {
					pairs ~= [l,r];
				}
			}
		}
	}
	
	return pairs;
}

/**
 * Checks enum contents for any of them that overlap. So an enum containing 1,2 & 3 would overlap, while 1, 2 & 4 wouldn't
 * This can help detect potential flag issues later.
 */
template enumBitsOverlap(alias E)
if (is(E == enum)) {
	enum enumBitsOverlap = getOverlaps(EnumMembers!(E));
}

unittest {
	enum Overlap {none, one, two, three }
	enum NoOverlap {none, one, two, four = 4 }
	
	assert(enumBitsOverlap!(Overlap) !is null);
	assert(enumBitsOverlap!(NoOverlap) is null);
}


///
struct HandleFlags(E, I)
if (is(E == enum) && isIntegral!(I) && isFloatingPoint!(I) == false) {
	I state;	///Holds state.
	alias E Enum;
	alias Enum this;
	
	enum containsOverlaps = enumBitsOverlap!(E);
	enum overlapsString = to!string(containsOverlaps);
	
	this(E[] flag...) {
		setFlag(flag);
	}
	 
	/**
	 * checks to see if two flags may interfere with eachother, specifically clearing or xoring.
	 * This can cause problems where you may get a completely unexpected or invalid flag combination,
	 * Then it's order specific or shouldn't encompass all the flags supplied. By only using non-overlapping
	 * flags do you ensure behavior doesn't change.
	 *
	 * In the following example, we have C set, if we flip A and C we think A is on and C is off, while
	 * in reality B is set.
	 ---
	enum FLAGS { A = 1, B, C }
	auto t = HandleFlags!(FLAGS, int)(C);

	with (FLAGS) {
		t.flipFlag(A, C);
	}
	
	//logically not having specific knowledge of how flags interact (as most of the time they shouldn't),
	//then this normally should be true
	assert(t.check(A));		//fails
	assert(!t.check(C));	//fails
	
	//when in reality
	assert(!t.check(A));
	assert(t.check(B));
	assert(!t.check(C));
	
	//has our enum been this, it would have worked.
	enum FLAGS { A = 1, B = 2, C = 4 }
	with (FLAGS) {
		t.flipFlag(A, C);
	}
	assert(t.check(A));		//passes as expected
	assert(!t.check(C));	//passes
	---
	
	* The reason multiple bits will be allowed for flags is mostly for checks, consider
	---
	enum FLAGS { A = 1, B = 2, Both = A|B }
	---
	* In this case Both is only true if A & B are on. Forcing a checkAll for multiple flags or specific states 
	*/
	static bool canOverlap(E[] flag...) @safe pure nothrow {
		for(int i; i < (flag.length-1); i++)
			foreach(fl; flag[i+1 .. $]) {
				if (flag[i] & fl)
					return true;
			}

		return false;
	}

	///checks if all bits in 'flag' are present.
	bool isFlagSet(E flag) const @safe pure nothrow {
		return (state & flag) == flag;
	}

	///Returns true/false if any specified flags has been set.
	bool check(E[] flag ...) const @safe pure nothrow
	in {
		assert(flag.length, "Empty flags list");	//logically not an error, but we aren't checking anything otherwise.
		//overlap not an issue
	}
	body {
		foreach(fl; flag) {
			if (isFlagSet(fl))
				return true;
		}
		return false;
	}

	///Returns true if (and only if) all supplied flags are true/on.
	bool checkAll(E[] flag ...) const @safe pure nothrow
	in {
		assert(flag.length, "Empty flags list");
		//overlap not an issue
	}
	body {
		foreach(fl; flag) {
			if (!isFlagSet(fl))
				return false;
		}
		return true;
	}

	/** Checks if a flag has been set, returning the first matching flag,
		otherwise returning the Else flag.*/
	E checkElse(E Else, E[] flag...) const @safe pure nothrow
	in {
		assert(flag.length, "Empty flags list");
		//overlap not an issue
	}
	body {
		foreach(fl; flag) {
			if (isFlagSet(fl))
				return fl;
		}

		return Else;
	}

	///Sets specific flag(s) on
	void setFlag(E[] flag...) @safe pure nothrow
	in {
		assert(flag.length, "Empty flags list");
		//overlap not an issue
	}
	body {
		foreach(fl; flag) {
			state |= fl;
		}
	}

	///turns listed flags off.
	void clearFlag(E[] flag...) @safe pure nothrow
	in {
		assert(flag.length, "Empty flags list");
		static if (containsOverlaps) {
			if (canOverlap(flag)) {
				throw new OverlapError(overlapsString);
			}
		}
	}
	body {
		foreach(fl; flag) {
			state &= (~fl);
		}
	}

	///reverses the state of a specific flag.
	void flipFlag(E[] flag...) @safe pure nothrow
	in {
		assert(flag.length, "Empty flags list");
		static if (containsOverlaps) {
			if (canOverlap(flag))
				throw new OverlapError(overlapsString);
		}

	}
	body {
		foreach(fl; flag) {
			state ^= fl;
		}
	}

	///keeps (ands) only specified flags
	void keepOnly(E[] flag...) @safe pure nothrow
	in {
		assert(flag.length, "Empty flags list");
		static if (containsOverlaps) {
			if (canOverlap(flag))
				throw new OverlapError(overlapsString);
		}

	}
	body {
		I mask;
		foreach(fl; flag){
			mask |= fl;
		}
		state &= mask;
	}
}

unittest {
	enum ETEST {
		none = 0,
		one = 1,
		two = 2,
		three = 3,
		four = 4
	}
	with(ETEST) {
		HandleFlags!(ETEST, int) ftest;

		//test all flags off. Uses int checks.
		assert(ftest.state == 0);	//start empty.

		//set 2 flag
		ftest.setFlag(two);
		assert(ftest.state == 2);

		//set 4, should now be 6
		ftest.setFlag(four);
		assert(ftest.state == 6);

		//turn off 2 bit
		ftest.clearFlag(two);
		assert(ftest.state == 4);

		//flip 1
		ftest.flipFlag(one, two);		//4+1+2 = 7
		assert(ftest.state == 7);
		ftest.flipFlag(two);			//4+1
		assert(ftest.state == 5);
	
		//check both flags are present
		assert(ftest.checkAll(one, four));
		assert(ftest.check(one, four));
		///check failure of flags
		assert(!ftest.checkAll(two, four));

		//flip 1 again.
		ftest.flipFlag(one);			//4+0
		assert(ftest.state == 4);

		//check truth/else
		ETEST x = ftest.checkElse(none, four);
		assert(x == four);

		x = ftest.checkElse(none, one);
		assert(x == none);

		assert(ftest.check(four) == true);
		assert(ftest.check(two) == false);

		ftest = HandleFlags!(ETEST, int)(one, two);
		assert(ftest.checkAll(one, two));
		assert(ftest.state == 3);

		assertThrown!Error(ftest.check(), "Not throwing an error/warning");
		assertThrown!Error(ftest.checkAll(), "Not throwing an error/warning");
		assertThrown!Error(ftest.checkElse(ETEST.none), "Not throwing an error/warning");
		assertThrown!Error(ftest.setFlag(), "Not throwing an error/warning");
		assertThrown!Error(ftest.clearFlag(), "Not throwing an error/warning");
		assertThrown!Error(ftest.flipFlag(), "Not throwing an error/warning");
		assertThrown!Error(ftest.keepOnly(), "Not throwing an error/warning");

		assertThrown!Error(ftest.check([]), "Not throwing an error/warning");
		assertThrown!Error(ftest.checkAll([]), "Not throwing an error/warning");
		assertThrown!Error(ftest.checkElse(ETEST.none,[]), "Not throwing an error/warning");
		assertThrown!Error(ftest.setFlag([]), "Not throwing an error/warning");
		assertThrown!Error(ftest.clearFlag([]), "Not throwing an error/warning");
		assertThrown!Error(ftest.flipFlag([]), "Not throwing an error/warning");
		assertThrown!Error(ftest.keepOnly([]), "Not throwing an error/warning");

		ftest.keepOnly(one);
		assert(ftest.state == 1);

		//Flags can also encompass multiple bits. in this case all bits returned with an AND must match the flag.
		assert(!ftest.checkAll(one, two, three));  //can't be true, only 1 is.
		ftest.state = 3;
		assert(ftest.checkAll(one, two, three));   //must be true, since 1+2 includes 3.

		
		//canOverlap tests. Important for multiple bitset check modificaions regarding removing flags
		//setting/checking on the other hand is safe.
		assert(ftest.canOverlap(one,two,four) == false);
		assert(ftest.canOverlap(three,four) == false);
		
		assert(ftest.canOverlap(one,three) == true);
		assert(ftest.canOverlap(two,three) == true);
		assert(ftest.canOverlap(one,two,three) == true);

		//ensure such an overlap throws
		assertThrown!OverlapError(ftest.flipFlag(one, three), "Not throwing Overlapping error");
		assertThrown!OverlapError(ftest.clearFlag(one, three), "Not throwing Overlapping error");
		assertThrown!OverlapError(ftest.keepOnly(one, three), "Not throwing Overlapping error");
		assertThrown!OverlapError(ftest.flipFlag(one, three), "Not throwing Overlapping error");
		assertThrown!OverlapError(ftest.clearFlag(one, three), "Not throwing Overlapping error");
		assertThrown!OverlapError(ftest.keepOnly(one, three), "Not throwing Overlapping error");
		
		assertNotThrown!()(ftest.flipFlag(one, two), "Should not throw");
		assertNotThrown!()(ftest.clearFlag(one, two), "Should not throw");
		assertNotThrown!()(ftest.keepOnly(one, two), "Should not throw");
		assertNotThrown!()(ftest.flipFlag(one, two), "Should not throw");
		assertNotThrown!()(ftest.clearFlag(one, two), "Should not throw");
		assertNotThrown!()(ftest.keepOnly(one, two), "Should not throw");
	
		//test alias this for enums
		assert(ftest.none == ETEST.none);
		assert(ftest.one == ETEST.one);
		assert(ftest.two == ETEST.two);
		assert(ftest.three == ETEST.three);
		assert(ftest.four == ETEST.four);
		
		assert(HandleFlags!(ETEST, int).four == ETEST.four);
		
		alias HandleFlags!(ETEST, int) Etest;	//as a new aliased type
		
		assert(Etest.four == ETEST.four);
		assert(is(Etest.Enum == ETEST));
		
//		ftest.flipFlag(one, three);	//intentionally to see the output message
	}
}
