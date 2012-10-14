/++
 Author:  Era Scarecrow <rtcvb32@yahoo.com>
 Date:    11 Oct 2012
 Project: Polymorphic Structs
 Licence: GPLv3
 
 Description: With the idea of polymorphism to add behavior to classes, classes sometimes are too bulky, too slow, too encumbersome for some tasks. Plus they live in the heap so allocating thousands of them can be slower. However if you don't need to extend the data with further mutable data and increaes the data signature, then there's no reason you can't make a struct polymorphic by nature.
 
<pre>
  Limitations to be expected with using a polymorphic struct is that
   1) Data size cannot change
   2) The function names either can be normal or appended like with 'Poly_' to make the function polymorphic.
   3) Polymorphic functions (in the same struct) need 'this.' prepended to it.
   4) All functions need to be known ahead of time.
   5) Template functions (outside of auto detection) cannot be used polymorphically.
   6) Referring to previous/super functions unavaliable (for now).
   7) No automatic type setup. Means you have to create the object and set it to it's inhereted type afterwards
</pre>
   This is the initial experimental library. So there are likely bugs, not all the unittests are written, and the implimentation can change at any time. Example crypto library present as proof of concept with unittests. It relies on heavy use of mixins and can slow down compiling of your code.
---
  auto crypto = PolyCryptoBase.factory("CryptoROT13");
  crypto.setString("(Whfg n yvggyr pelcgb grfg)");
  crypto.encipher;
  assert(crypto.data.crypted == "(Just a little crypto test)");
---

  Setting up your structures to use requires all the structs to have certain additions before hand. Use the below as a skeleton to add to your code.
  
---
  struct Example {
    //sets up Enum, _prefix, _types and aliases for polymorphing to the various structs.
    mixin(expandTypes("Inner_Data_Struct_Name", "Prefix_", "Several,Struct_Names"));
    //Enum { Several = 1, Struct_Names = 3} // is silently added by expandTypes

    //inner data struct, must contain the polymorphic type identifyer.
    struct Inner_Data_Struct_Name {
      Enum polyMorphType;      //defaults to base/first entry above, which is 'Several' in this case.
      string somethingMutable; //any other variables here, just as an example.
    }

    //any and all data/accessible members here, but mostly your mutable data as above.
    Inner_Data_Struct_Name data;

    //This struct name, inner data's struct name, allocated/named variable containing the polymorphic state.
    mixin PolyMorphic!("Example", Inner_Data_Struct_Name, "data");
  }
  

  //first named of structs is the base/root.
  struct Several {
    //required mixin, will fill in details for attaching to polymorph struct 'Example'
    mixin PolyMorphicInclude!(Example);

    /*DO NOT ADD MORE DATA/VARIABLES HERE!. Doing so will cause a compile-time error, the size
      must be statically known ahead of time (for safety) and only in the base (Example) can
      there the data be visible/accessible. static variables and enums should be okay to declare.

      anything containing the 'prefix' noted before hand
      are automatically polymorphable, so long as you don't call them with the prefix.*/
    void Prefix_anotherFunc();

    //no prefix, so likely not intended to be polymorphed, or only existing function
    void someFunc() {
      /*to call anotherFunc (even from this struct) you don't ever
        include the prefix. opDispatch of the base struct (Example)
        will add the prefix during checks to determine which struct
        can be called.
      */
      this.anotherFunc();

      //accessing data that all structs have access to.
      data.somethingMutable = "Several.anotherFunc called!"; 
    }
  }
  
  struct Struct_Names {
    //required mixin, minimal set up.
    mixin PolyMorphicInclude!(Example);

    /*this will only ever be called if the type is for 'Struct_Names', ie 'Enum.Struct_Names'. But if there
      is another funciton inside this structure that calls 'anotherFunc', add the prefix to force polymorph
      behavior.*/
    void anotherFunc() {
      //non existing function call handled by Example.opDispatch
      this.someFunc();

      //accessing data that all structs have access to.
      data.somethingMutable = "Struct_Names.anotherFunc called!"; 
    }
  }
---

  When only one possible matches are present then checks are never needed and jumps to the appropriate function, which Self optimizes. (If the function is in an inhereted but not the base class, it is possible to have an asserted runtime error) Const/non-const must match all other shared name declarations, if only using in a mutable state it won't complain, but if you try to call a mutable version of a function vs a const/immutable version that can be called/overloaded with a particular set of arguments, then the compiler will assert during compile-time and let you know. Tracing a polymorphed function that isn't calling is quite an annoying bug. 'static' is considered const as it never touches struct data.
 
  Write the structures and debug them normally without adding polymorph abilities, then when you update rename/add the prefix
 and the mixin, remove non-changing functions. Any functions calling the ones present in that struct need to have them prefixed.
 
 Here's a more realistic example from this file, although heavily stripped. See the full source for more details.

---
struct PolyCryptoBase {
  ///Enum, _prefix and _types don't exist until this calls.
  mixin(expandTypes("Data", "Poly_", "CryptoBase,CryptoROT13"));

  struct Data {
    Enum polyMorphType;
    string original;
    char[] crypted;  ///mutable copy
  }
  Data data;

  mixin PolyMorphic!(PolyCryptoBase, Data, "data");
}

struct CryptoBase {
  mixin PolyMorphicInclude!(PolyCryptoBase);
  
  ///Does basic enciphering, individual letters only
  static void Poly_cipherChar(ref char c) {c ^= 0x7f;}
  
  ///set the plaintext (resetting crypted text as well)
  void setString(string input) { crypted = input.dup; }
  
  ///encrypt cipher string (which done twice will decrypt it)
  void encipher() @property {
    foreach(ref ch; data.crypted) {
        //calls polymorphed version from opDispatch
      this.cipherChar(ch);
    }
  }
}

struct CryptoROT13 {
  mixin PolyMorphicInclude!(PolyCryptoBase);
  
  void cipherChar(ref char c);
}
---
+/

module smartmerged.polymorphicstruct;

import std.stdio;
import std.traits;
import std.conv;
import std.algorithm;
import std.range;
import std.exception;

enum MutableState { isMutable, isConst, isImmutable}

/*Checks if a particular call is legal, be it calling a function or accessing a variable.
  depending on the results enumCheck and calling are prepared ahead of time for opDispatch
  returns an array of the results. accessing/aliasing them individually slows compiling to a crawl.*/
template isLegalCall(string VariableAccess, string TypeCheck, string prefix, string funCall, string structName, MutableState state, Vars ...) {
	enum objWithState = (state == MutableState.isConst ? "const ": "")
		~ (state == MutableState.isImmutable ? "immutable " : "")
		~ structName;
	//if it's two prefixes, say Poly_Poly_, then it's recursively checking and instead should fail.
	//prevents the checks below from getting stuck
	static if (funCall.length >= prefix.length && prefix == funCall[0 .. prefix.length]) {
		enum polyFunctionLike = false;
		enum polyDataLike = false;
		enum functionLike = false;
		enum dataLike = false;
	} else {
		mixin("enum polyFunctionLike = hasMember!("~structName~",\""~prefix~funCall~"\") && is(typeof({"~objWithState~" x="~structName~"(); x."~prefix~funCall~"(Vars);}));"
			~ "enum polyDataLike = Vars.length == 0 && hasMember!("~structName~",\""~prefix~funCall~"\") && is(typeof({"~objWithState~" x="~structName~"(); x."~prefix~funCall~";}));"
			~ "enum functionLike = hasMember!("~structName~",\""~funCall~"\") && is(typeof({"~objWithState~" x="~structName~"(); x."~funCall~"(Vars);}));"
			~ "enum dataLike = Vars.length == 0 && hasMember!("~structName~",\""~funCall~"\") && is(typeof({"~objWithState~" x="~structName~"(); x."~funCall~";}));"
			);
	}
	//the following to detect when const/non-const is potentially possible for a call and errs
	static if (state != MutableState.isMutable) {
		mixin("enum polyFunctionLikeStateless = hasMember!("~structName~",\""~prefix~funCall~"\") && is(typeof({"~structName~" x="~structName~"(); x."~prefix~funCall~"(Vars);}));"
		~ "enum polyDataLikeStateless = Vars.length == 0 && hasMember!("~structName~",\""~prefix~funCall~"\") && is(typeof({"~structName~" x="~structName~"(); x."~prefix~funCall~";}));"
		~ "enum functionLikeStateless = hasMember!("~structName~",\""~funCall~"\") && is(typeof({"~structName~" x="~structName~"(); x."~funCall~"(Vars);}));"
		~ "enum dataLikeStateless = Vars.length == 0 && hasMember!("~structName~",\""~funCall~"\") && is(typeof({"~structName~" x="~structName~"(); x."~funCall~";}));"
		);
		static assert(polyFunctionLike == polyFunctionLike, "Polymorph function "~funCall~" contains mutable and const mismatches");
		static assert(polyDataLike == polyDataLikeStateless, "Polymorph function "~funCall~" (property?) contains mutable and const mismatches");
		static assert(functionLike == functionLikeStateless, "Function "~funCall~" contains mutable and const mismatches");
		static assert(dataLike == dataLikeStateless, "Function "~funCall~" (property?) contains mutable and const mismatches");
	
	}

	enum enumCheck = "if (("~VariableAccess~"."~TypeCheck~" & Enum."~structName~") == Enum."~structName~")";
	enum calling = "return (cast("~objWithState~") "~VariableAccess~")."~ending(prefix, funCall, functionLike, dataLike, polyFunctionLike, polyDataLike);
	enum isAnyLegal = functionLike || dataLike || polyFunctionLike || polyDataLike;

	enum isLegalCall = [ "objWithState" : objWithState,
		"enumCheck" : enumCheck,
		"calling": calling,
		 //can't be binary, seems dumb but will work fine.
		"isAnyLegal" : isAnyLegal ? "true" : "false",
		"polyFunctionLike": polyFunctionLike ? "true" : "false",
		"polyDataLike" : polyDataLike ? "true" : "false",
		"functionLike" : functionLike ? "true" : "false",
		"dataLike" : dataLike ? "true" : "false"];
}

//funtion like, data like, polymorphed?
//args is used silently forwarded, which is a problem to be solved another time.
string ending(E)(string prefix, string funcCall, E fl, E dl, E pfl, E pdl) {
	if (pfl) return prefix ~ funcCall ~ "(args);";
	if (pdl) return prefix ~ funcCall ~ ";";
	if (fl)  return funcCall ~ "(args);";
	if (dl)  return funcCall ~ ";";
	return "";	//asserting seems pointless here with the calling above.
}

/**expands to Enum declaration and aliased versions of all struct types.
   must be inputted as a comma delimited string. no spaces or
   extras. So "Base,SubRecord" would be two structs.
   creates the background enum references as well as checks all the data types match
   as no resizes/changes are allowed.*/
string expandTypes(string dataStruct, string prefix, string types) {
	string structs;
	string enums = "enum _prefix=\""~prefix~"\";enum _types=\""~types~"\";enum Enum {";
	string sizeChecks;
	int i;	//cannot infer types?
	foreach(str; splitter(types, ",")) {
		//planned later, multiple inheretance like classes will work if related ones retain similar
		//binary patterns. So say a struct with 111b, will let other struct functions of 11b or 1b as
		//they retain the full patern minus the expanded parts. No way set that up at present.
		
		//Idea: Use a new array with a split that has super->struct, this would have it append that type's
		//  value making the inhereited types work.
		enums ~= str~"=(1<<"~(to!string(i))~")|1,";
		structs ~= "alias "~str~" "~prefix~str~";";
		sizeChecks ~= "static assert("~dataStruct~".sizeof=="~str~".sizeof, \""~str~": "
				~"Polymorphic struct size doesn't match fixed size\");";
		
		i++;
	}
		enums ~= "}";
	return sizeChecks ~ structs ~ enums;
}
unittest {
	string expected = `static assert(Data.sizeof==Base.sizeof, "Base: Polymorphic struct size doesn't match fixed size");static assert(Data.sizeof==SubRecord.sizeof, "SubRecord: Polymorphic struct size doesn't match fixed size");alias Base Poly_Base;alias SubRecord Poly_SubRecord;enum _prefix="Poly_";enum _types="Base,SubRecord";enum Enum {Base=(1<<0)|1,SubRecord=(1<<1)|1,}`;
	string returned = expandTypes("Data", "Poly_", "Base,SubRecord");
	assert(returned == expected, "\n"~expected~"\n"~returned);
}

///Basic template added to inhereted/behavior structs. When using this, make and use no other variables
mixin template PolyMorphicInclude(PT) {
	PT polyBase;
	alias polyBase this;
//	@disable this();
}

///To add opCmp that is polymorphic (and not all encompassing in the polymorph base) then include
///this after makePolyMorphicCall
mixin template PolyMorphicCompare(PT) {
	///if not, forward reference to Poly_opCmp
	int opCmp(ref const PT rhs) const {
		return this.opDispatch!("opCmp")(rhs);
	}
}

//checks for potential matches and selects the best one. Checks for const/mutable signature breaks,
//and builds compatct enum checks. If there's only one possibility, only that is returned instead.
string makePolyMorphicCall(string VariableAccess, string TypeCheck,
		string prefix, string funCall, Enum, MutableState state, Vars ...)() {
	string calls;
	string results;
	string firstSuccess;
	int found;

	//TODO: add some type of super/parent management for multiple levels.

	foreach(e; EnumMembers!(Enum)) {
		enum ilc = isLegalCall!(VariableAccess, TypeCheck, prefix, funCall, to!string(e), state, Vars);
		static if (ilc["isAnyLegal"] == "true") {
			debug {
				results ~= "\n// objWithState " ~ ilc["objWithState"];
				results ~= "\t pfl = " ~ ilc["polyFunctionLike"];
				results ~= "\t fl  = " ~ ilc["functionLike"];
				results ~= "\t pdl = " ~ ilc["polyDataLike"];
				results ~= "\t dl  = " ~ ilc["dataLike"];
				results ~= "\t enum= " ~ ilc["enumCheck"];
				results ~= "\t call= " ~ ilc["calling"];
				results ~= "\n";
			}
			found++;
			firstSuccess = ilc["calling"];
			calls = ilc["enumCheck"] ~ ilc["calling"] ~ calls;
		}
	}
	
	if (!calls.length) {
		assert(0, "//Cannot identify function "~funCall~" or "~prefix~funCall);
	} else {
		calls ~= "assert(0, \"Failed to find appropriate funciton call for "~funCall~" or "~prefix~funCall~" during runtime.\");";
	}
	
	if (found == 1)
		calls = firstSuccess;	//remove enum check.

	return results ~ calls;
}

/+
/*
*/
mixin template PolyMorphicIsLegalCast(PT)
if (hasMember!(PT, "Enum")) {
	/**since i am not sure of how to do all casting checks
	  nor without breaking code, legal checks is likely safer.
	  This is only for basic type, no const or anything (would that even transfer?)
	  This will likely be redone to remove excess checks/runtime later
	  */
	bool isLegalCast(T)() const @property
	if (is(T == PT)) {
		return true;
	}
	
	bool isLegalCast(T)() const @property
	if (!is(T == PT) && hasMember!(Enum, to!string(T))) {
		foreach(e; EnumMembers!(Enum)) {
			static if (to!string(e) == T) {
				//getPolyMorphState present in other template mixin.
				if ((getPolyMorphState & e) == e)
					return true;
			} else
				return false;	//exact check found, at this point break out.
		}
		
		//just in case.. i doubt it ever reaches here.
		return false;
	}
}
+/

/**The polymorphic header for the base.
   PT is the PolymorphicType,
   DT is your Data struct (including enum for type)
   Variable access is what you initialize it using: ie Data data;
   TypeCheck is where your enum check is located. Ie: struct Data { Enum polyMorphType}, being 'polyMorphType'
   
   Adds opDispatch calls, getPolyMorphState/setPolyMorphState, and factory (only for these known subrecords)
*/
mixin template PolyMorphic(PT, DT, string VariableAccess, string TypeCheck = "polyMorphType")
if (hasMember!(PT, "Enum") && hasMember!(PT, "_prefix") && hasMember!(PT, "_types") &&
		hasMember!(DT, TypeCheck) && hasMember!(PT, VariableAccess ~ "." ~ TypeCheck)) {
		
	//if there are two levels of your prefix (say Poly_Poly_) then it's recursively
	//trying and shouldn't be allowed any further. Not listed as part of the API as it's 'magic'
	auto ref opDispatch(string fun, Args ...)(auto ref Args args) @property
	if (fun.length <= _prefix.length*2 || fun[0 .. (_prefix.length*2)] != _prefix~_prefix) {
		enum x = makePolyMorphicCall!(VariableAccess, TypeCheck, _prefix, fun, Enum, MutableState.isMutable, args)();
		debug {	//lots of output with lots of calls. You have been warned!!
			writeln(to!string(data.polyMorphType));
			writeln(fun);
			writeln(x);
		}
		mixin(x);
	}

	auto ref opDispatch(string fun, Args ...)(auto ref Args args) @property const
	if (fun.length <= _prefix.length*2 || fun[0 .. (_prefix.length*2)] != _prefix~_prefix) {
		enum x = makePolyMorphicCall!(VariableAccess, TypeCheck, _prefix, fun, Enum, MutableState.isConst, args)();
		debug {
			writeln(to!string(data.polyMorphType));
			writeln(fun);
			writeln(x);
		}
		mixin(x);
	}
	
	auto ref opDispatch(string fun, Args ...)(auto ref Args args) @property immutable
	if (fun.length <= _prefix.length*2 || fun[0 .. (_prefix.length*2)] != _prefix~_prefix) {
		enum x = makePolyMorphicCall!(VariableAccess, TypeCheck, _prefix, fun, Enum, MutableState.isImmutable, args)();
		debug {
			writeln(to!string(data.polyMorphType));
			writeln(fun);
			writeln(x);
		}
		mixin(x);
	}

	///set the current polymorphic state.
	void setPolyMorphState(Enum type) @property {
		mixin(VariableAccess~"."~TypeCheck~" = type;");
	}
	
	///Get current state/behavior.
	Enum getPolyMorphState() const @property {
		mixin("return "~VariableAccess~"."~TypeCheck~";");
	}
	

/+  //can't compile right now.
	//isLegalCast moved to mixin template, but not working either.
	///currently cannot do without fully re-implimenting alternate opAssign, opCmp. How annoying.
	///returns if the type is equal (or accessed by inheretice)
	///or that's the idea.
	bool opEquals(Enum type) const {
		mixin("return ("~VariableAccess~"."~TypeCheck~" & type) == type;");
	}
	///set type via normal assignment
//	ref PT opAssign(E)(E type) 
//	if (is(E == Enum)) {
	ref PT opAssign(Enum type) {
		this.setPolyMorphState(type);
		return this;
	}
+/


	///factory-like function. Not sure about filename references so only struct name is used.
	static PT factory(string structName) {
		PT tmp;
		foreach(e; EnumMembers!(Enum)) {
			enum str = to!string(e);
			if (structName == str) {
				tmp.setPolyMorphState(e);
				break;
			}
		}
		return tmp;
	}
}

private:
/** This is not intended as a serious crypto library, just enough to test several factors to ensure the polymorphic
    nature is being done. As other items need to be tested this crypto will be improved to handle those tests.*/
struct PolyCryptoBase {
	///Enum, _prefix and _types don't exist until this calls.
	mixin(expandTypes("Data", "Poly_", "CryptoBase,CryptoXOR13,CryptoROT13"));

	///
	struct Data {
		Enum polyMorphType; ///
		string original; ///
		char[] crypted;	///mutable copy
	}
	Data data; ///

	///Seems opCmp opEquals and similar functions go in the poly base. That's livable.
	///if not, forward reference to Poly_opCmp
	
	mixin PolyMorphicCompare!(PolyCryptoBase);
	mixin PolyMorphic!(PolyCryptoBase, Data, "data"); ///
}

///
struct CryptoBase {
	mixin PolyMorphicInclude!(PolyCryptoBase); ///
	
	///Seems opCmp opEquals and similar functions go in the poly base. That's livable.
	///if not, forward reference to Poly_opCmp
	int Poly_opCmp(ref const PolyCryptoBase rhs) const {
		return data.original.length - rhs.data.original.length;
	}
	
	///if it matches the original string
	bool isCrypted() @property pure @safe nothrow{
		return data.original != data.crypted;
	}
	
	///Does basic enciphering, individual letters only
	static void Poly_cipherChar(ref char c) pure @safe nothrow {
		c ^= 127;
	}
	
	///set the plaintext (resetting crypted text as well)
	void setString(string input) @property {
		data.original = input;
		data.crypted = input.dup;
	}
	
	///encrypt cipher string (which done twice will decrypt it)
	void encipher() @property {
		foreach(ref ch; data.crypted) {
			this.cipherChar(ch);
		}
	}
}

///
struct CryptoXOR13 {
	mixin PolyMorphicInclude!(PolyCryptoBase);///
	
	/**non matching signatures, but still compatible
	   requires 'static' or const to work, otherwise
	   potential for only calling a different version
	   may apply making some bugs hard to find.*/
	static void cipherChar(ref char c) pure @safe {
		c ^= 13;
	}
}

///
struct CryptoROT13 {
	mixin PolyMorphicInclude!(PolyCryptoBase);///
	
	///ditto, our const-only version
	void cipherChar(ref char c) const {
		if (c >= 'a' && c <= 'z') {
			c += 13;
			if (c > 'z')
				c -= 26;
		}
		if (c >= 'A' && c <= 'Z') {
			c += 13;
			if (c > 'Z')
				c -= 26;
		}
	}
}

unittest {
	PolyCryptoBase crypto;
	crypto.setString = "Hello world!";
	char[] str = "Hello world!".dup;
	str[] ^= 127;
	
	assert(!crypto.isCrypted);
	crypto.encipher;

	assert(crypto.isCrypted);
	assert(crypto.data.crypted == str);

	crypto.encipher;
	assert(!crypto.isCrypted);

	str[] ^= 127;
	assert(crypto.data.crypted == str);
	
	//k XOR13
	crypto.setPolyMorphState(crypto.Enum.CryptoXOR13);
	str[] ^= 13;

	crypto.encipher;
	assert(crypto.data.crypted == str);
	crypto.encipher;
	assert(!crypto.isCrypted);
	str[] ^= 13;
	assert(crypto.data.crypted == str);
	
	//k ROT13.
	crypto.setPolyMorphState(crypto.Enum.CryptoROT13);
	crypto.setString("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
	crypto.encipher;
	assert(crypto.data.crypted == "nopqrstuvwxyzabcdefghijklmNOPQRSTUVWXYZABCDEFGHIJKLM");

	//const/immutable tests.
	const PolyCryptoBase c_crypto = crypto;
	auto i_crypto = cast(immutable PolyCryptoBase) crypto;
	
	//compile time checks for incompatibility
	assert(!is(typeof(c_crypto.setString("break!"))), "const/mutable compile-time checks failed!");
	assert(!is(typeof(c_crypto.encipher)), "const/mutable compile-time checks failed!");

	//check if ref is working, plus const version
	char c = 'A';
	c_crypto.cipherChar(c);
	assert(c == 'N');
	i_crypto.cipherChar(c);	//static so immutable is happy
	assert(c == 'A');
	
	crypto = PolyCryptoBase.factory("CryptoROT13");
	crypto.setString("(Whfg n yvggyr pelcgb grfg)");
	crypto.encipher;

	assert(crypto.data.crypted == "(Just a little crypto test)");
	
	//compare for equality
	assert(crypto !is c_crypto, "opCmp of previous version broken");
	assert(crypto != c_crypto, "opCmp of previous version broken");
	assert(crypto is crypto, "opCmp of same object broken");
	assert(crypto == crypto, "opCmp of same object broken");
	
	CryptoROT13 rot13 = cast(CryptoROT13) crypto;
	assert(crypto == rot13, "contents not equal and should be"); //alias this should make them work still.
	assert(rot13 == crypto, "contents not equal and should be");
	assert(rot13 == rot13, "contents not equal and should be");
	assert(rot13 != c_crypto); //alias this should make them work still. Crypto string is different.
	
	assert(c_crypto > crypto);	//test polymorphic opCmp 
}
