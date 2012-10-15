/**
	Author: Era Scarecrow <rtcvb32@yahoo.com>
	Date: 14 April 2012
	License: GPLv3
	
	Description: Basic handling of flags and their types. Since you can't go int to ENUM, this
	structure will set, and test flags appropriately. As there's only one state (integral)
	you can (hopefully) forcibly cast an int into HandleFlags so long as the size is the same.

---
	enum ETEST {none = 0, one = 1, two = 2, three = 3, four = 4}
	HandleFlags!(ETEST, int) ftest;

	ftest.setFlag(ETEST.two);	//check flag setting
	assert(ftest.state == 2);	//int and enum compares.
	assert(ftest.state == ftest.Enum.two);  //ftest.Enum.one accessible

	//shortcuts avaliable.
	assert(ftest.Enum.one == ETEST.one);

	ftest.setFlag(ETEST.four);	//set a second flag
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

	//binary operations possible and even tests.
	with(ftest.Enum) {
		ftest.state = 0;
		assert(!ftest);
		//opOpBinary not workable, so no += or similar
		ftest = ftest | two; 
		assert(ftest && ftest.state == 2);

		ftest = ftest ^ [one, two];
		assert(ftest && ftest.state == 1);

		assert(ftest & one);
		assert(!(ftest & two));

		assert(!(ftest - one));

		// | and + are the same, so 1 + 1 = 1. (Setting the flag with + doesn't equal arithmetic)
		ftest = ftest + one;
		assert(ftest && ftest.state == 1);
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

//duplicate of above documentation, for correctness.
unittest {
	enum ETEST {none = 0, one = 1, two = 2, three = 3, four = 4}
	HandleFlags!(ETEST, int) ftest;

	ftest.setFlag(ETEST.two);	//check flag setting
	assert(ftest.state == 2);	//int and enum compares.
	assert(ftest.state == ftest.Enum.two);  //ftest.Enum.one accessible

	//shortcuts avaliable.
	assert(ftest.Enum.one == ETEST.one);

	ftest.setFlag(ETEST.four);	//set a second flag
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

	//binary operations possible and even tests.
	with(ftest.Enum) {
		ftest.state = 0;
		assert(!ftest);
		//opOpBinary not workable, so no += or similar
		ftest = ftest | two; 
		assert(ftest && ftest.state == 2);

		ftest = ftest ^ [one, two];
		assert(ftest && ftest.state == 1);

		assert(ftest & one);
		assert(!(ftest & two));

		assert(!(ftest - one));

		// | and + are the same, so 1 + 1 = 1. (Setting the flag with + doesn't equal arithmetic)
		ftest = ftest + one;
		assert(ftest && ftest.state == 1);
	}
}

///
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
auto getOverlaps(E)(const E[] flags ...)
if (is(E == enum)) {
	E[2][] pairs;
	if (flags.length > 1) {
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
	I state;		///Holds state.
	alias E Enum;	///
	alias convToBool this;

	/**simple bool conversion, prevents treating the whole thing as an numeric type
	   and opOpBinary operations won't have unwanted effects.*/
	bool convToBool() @safe pure nothrow const {
		return state != 0;
	}
	
	enum containsOverlaps = enumBitsOverlap!(E);		///
	enum overlapsString = to!string(containsOverlaps);	///
	
	///Start with flags set
	this(E[] flags ...) {
		setFlag(flags);
	}
	
	///Convert numeric's value into a flag
	this(I state) {
		this.state = state;
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
	static bool canOverlap(const E[] flag...) @safe pure nothrow {
		for(int i; i < (flag.length-1); i++)
			foreach(fl; flag[i+1 .. $]) {
				if (flag[i] & fl)
					return true;
			}

		return false;
	}

	///checks if all bits in one enum flag are present.
	bool isFlagSet(E flag) const @safe pure nothrow {
		return (state & flag) == flag;
	}

	///Returns true/false if any specified flags has been set.
	bool check(const E[] flags ...) const @safe pure nothrow
	in {
		assert(flags.length, "Empty flags list");	//logically not an error, but we aren't checking anything otherwise.
		//overlap not an issue
	}
	body {
		foreach(fl; flags) {
			if (isFlagSet(fl))
				return true;
		}
		return false;
	}

	///Returns true if (and only if) all supplied flags are true/on.
	bool checkAll(const E[] flags ...) const @safe pure nothrow
	in {
		assert(flags.length, "Empty flags list");
		//overlap not an issue
	}
	body {
		foreach(fl; flags) {
			if (!isFlagSet(fl))
				return false;
		}
		return true;
	}

	/** Checks if a flag has been set, returning the first matching flag,
		otherwise returning the Else flag.*/
	E checkElse(E Else, const E[] flags ...) const @safe pure nothrow
	in {
		assert(flags.length, "Empty flags list");
		//overlap not an issue
	}
	body {
		foreach(fl; flags) {
			if (isFlagSet(fl))
				return fl;
		}

		return Else;
	}

	///Sets specific flag(s) on
	void setFlag(const E[] flags ...) @safe pure nothrow
	in {
		assert(flags.length, "Empty flags list");
		//overlap not an issue
	}
	body {
		foreach(fl; flags) {
			state |= fl;
		}
	}

	///turns listed flags off.
	void clearFlag(const E[] flags ...) @safe pure nothrow
	in {
		assert(flags.length, "Empty flags list");
		static if (containsOverlaps) {
			if (canOverlap(flags)) {
				throw new OverlapError(overlapsString);
			}
		}
	}
	body {
		foreach(fl; flags) {
			state &= (~fl);
		}
	}

	///reverses the state of a specific flag.
	void flipFlag(const E[] flags ...) @safe pure nothrow
	in {
		assert(flags.length, "Empty flags list");
		static if (containsOverlaps) {
			if (canOverlap(flags))
				throw new OverlapError(overlapsString);
		}

	}
	body {
		foreach(fl; flags) {
			state ^= fl;
		}
	}

	///keeps (ands) only specified flags
	void keepOnly(const E[] flags ...) @safe pure nothrow
	in {
		assert(flags.length, "Empty flags list");
		static if (containsOverlaps) {
			if (canOverlap(flags))
				throw new OverlapError(overlapsString);
		}
	}
	body {
		I mask;
		foreach(fl; flags){
			mask |= fl;
		}
		state &= mask;
	}

	//NOTE on binary flag operations. Checks regarding overlapping types not tested (as of yet)

	///Binary operators for those that want to treat it like that. 
	HandleFlags opBinary(string op)(HandleFlags rhs) @trusted pure nothrow const {
		HandleFlags tmp = this;
		switch(op) {
			case "+", "|": tmp.state |= rhs.state; break;
			case "-": tmp.state &= (~rhs.state); break;
			case "^": tmp.state ^= rhs.state; break;
			case "&": tmp.state &= rhs.state; break;
			default:
		}

		return tmp;
	}

/+
	// not currently working without explicit calling.
	ref HandleFlags opOpBinary(string op)(const E[] rhs ...) @trusted pure nothrow
	if (op == "+" || op == "-" || op == "^" || op == "&" || op == "|") {
		switch(op) {
			case "+", "|": setFlag(rhs); break;
			case "-": clearFlag(rhs); break;
			case "^": flipFlag(rhs); break;
			case "&": keepOnly(rhs); break;
			default:
		}
		return this;
	}
+/

	///Binary operators for those that want to treat it like that. Single flags and whole Flag groups
	HandleFlags opBinary(string op)(const E[] rhs ...) @trusted pure nothrow const
	if (op == "+" || op == "-" || op == "^" || op == "&" || op == "|") {
		HandleFlags tmp = this;
		switch(op) {
			case "+", "|": tmp.setFlag(rhs); break;
			case "-": tmp.clearFlag(rhs); break;
			case "^": tmp.flipFlag(rhs); break;
			case "&": tmp.keepOnly(rhs); break;
			default:
		}
		return tmp;
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
	
		//removed: alias Enum this. Was replaced by alias convToBool this.

		assert(HandleFlags!(ETEST, int).Enum.four == ETEST.four);
		
		alias HandleFlags!(ETEST, int) Etest;	//as a new aliased type
		
		assert(Etest.Enum.four == ETEST.four);
		assert(is(Etest.Enum == ETEST));
		
//		ftest.flipFlag(one, three);	//intentionally to see the output message

		//now for added binary operators. Tests try to incorporate both set and unset
		//changes present
		ftest = Etest(one, two);
		Etest ftest2 = Etest(two, four);
		Etest noChanges, noChangesHF, changes, changesHF;
		Etest resultsNoChanges, resultsChanges;

			////+, |
		noChanges = ftest + two;	//no change
		noChangesHF = ftest + ftest; //no change, all same flags
		changes = ftest + four;		//add flag
		changesHF = ftest + ftest2; //or/add all flags missing.
		resultsNoChanges = Etest(one, two);
		resultsChanges = Etest(one, two, four);

		assert(noChanges == resultsNoChanges);
		assert(noChangesHF == resultsNoChanges);
		assert(changes == resultsChanges);
		assert(changesHF == resultsChanges);

		////array version, to be finished later.
		noChanges = ftest + [two];	//no change
		changes = ftest + [two, four];		//add flag
		assert(noChanges == resultsNoChanges);
		assert(changes == resultsChanges);

		noChanges = ftest | two;	//no change
		noChangesHF = ftest | ftest; //no change, all same flags
		changes = ftest | four;		//add flag
		changesHF = ftest | ftest2; //or/add all flags missing.
//		resultsNoChanges = Etest(one, two);
//		resultsChanges = Etest(one, two, four);

		assert(noChanges == resultsNoChanges);
		assert(noChangesHF == resultsNoChanges);
		assert(changes == resultsChanges);
		assert(changesHF == resultsChanges);

		//array
		noChanges = ftest; changes = ftest;
		noChanges = ftest | [two];			//no change
		changes = ftest | [two, four];		//add flag
		assert(noChanges == resultsNoChanges);
		assert(changes == resultsChanges);

			////-
		ftest2 = Etest(four);
		noChanges = ftest - four;
		noChangesHF = ftest - ftest2;
		ftest2 = Etest(two, four);
		changes = ftest - two;
		changesHF = ftest - ftest2;

		resultsNoChanges = Etest(one, two);
		resultsChanges = Etest(one);

		assert(noChanges == resultsNoChanges);
		assert(noChangesHF == resultsNoChanges);
		assert(changes == resultsChanges);
		assert(changesHF == resultsChanges);

		//array
		noChanges = ftest; changes = ftest;
		noChanges = ftest - [four];			//no change
		changes = ftest - [two, four];		//add flag
		assert(noChanges == resultsNoChanges);
		assert(changes == resultsChanges);

			////&
		ftest2 = Etest(one, two, four);
//		noChanges = ftest & four;	//only one flag, so we can't keep both flags.
		noChangesHF = ftest & ftest2;
		ftest2 = Etest(two, four);	//overlapping 2

		changes = ftest & two;		//keep only 2
		changesHF = ftest & ftest2;

		resultsNoChanges = Etest(one, two);
		resultsChanges = Etest(two);

//		assert(noChanges == resultsNoChanges);
		assert(noChangesHF == resultsNoChanges);
		assert(changes == resultsChanges);
		assert(changesHF == resultsChanges);
		
		//array
		noChanges = ftest; changes = ftest;
		noChanges = ftest & [one, two];			//no change
		changes = ftest & [two];		//add flag
		assert(noChanges == resultsNoChanges);
		assert(changes == resultsChanges);

			////^
		ftest = Etest(one, two);
		ftest2 = Etest(two, four); //1 overlapping
		changes = ftest ^ two;		//remove flag
		changesHF = ftest ^ ftest2; //swap 2

		resultsChanges = Etest(one);
		assert(changes == resultsChanges);
		resultsChanges = Etest(one, four);
		assert(changesHF == resultsChanges);

		//second xor check for changes
		changes = ftest ^ four;		//remove flag
		resultsChanges = Etest(one, two, four);
		assert(changes == resultsChanges);

		//array
		changes = ftest ^ [two, four];		//add flag
		resultsChanges = Etest(one, four);
		assert(changes == resultsChanges);

		//convert to bool tests
		assert(ftest);
		assert(ftest & two);
		assert(!(ftest & four));

		ftest2 = Etest();
		assert(!ftest2);
		assert(!(ftest2 & ftest));
		assert(ftest2 + ftest);

		/* opOpBinary - not scalar problems. No slices probably. 
		changes = ftest; noChanges = ftest;
		noChanges += two;	//no change
		changes += four;		//add flag
		assert(noChanges == resultsNoChanges);
		assert(changes == resultsChanges);
		changes = ftest; noChanges = ftest;
		noChanges += [two];	//no change
		changes += [two, four];		//add flag
		assert(noChanges == resultsNoChanges);
		assert(changes == resultsChanges);
		*/

	}
}
