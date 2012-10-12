module smartmerged.polymorphicstruct;
/**
 Author:  Era Scarecrow <rtcvb32@yahoo.com>
 Date:    11 Oct 2012
 Project: Polymorphic Structs
 Licence: GPLv3
 
 Description: With the idea of polymorphism to add behavior to classes, classes sometimes are too bulky, too
  slow, too encumbersome for some tasks. Plus they live in the heap so allocating thousands of them can
  be slower. However if you don't need to extend the data with further mutable data and increaes the data
  signature, then there's no reason you can't make a struct polymorphic by nature.
 
  Limitations to be expected with using a polymorphic struct is that
   1) Data size cannot change
   2) The function names either can be normal or appended like with 'Poly_' to make the function polymorphic.
   3) Polymorphic functions (in the same struct) need 'this.' prepended to it.
   4) All functions need to be known ahead of time.
   5) Template functions (outside of auto detection) cannot be used polymorphically.

   This is the initial experimental library. So there are likely bugs, not all the unittests are written, and the
  implimentation can change at any time. Example crypto library present as proof of concept.
---
  auto crypto = PolyCryptoBase.factory("CryptoROT13");
  crypto.setString("(Whfg n yvggyr pelcgb grfg)");
  crypto.encipher;
  assert(crypto.data.crypted == "(Just a little crypto test)");
---
*/


import std.stdio;
import std.traits;
import std.conv;
import std.algorithm;
import std.range;

enum MutableState { isMutable, isConst, isImmutable}

//Checks if a particular call is legal, be it calling a function or accessing a variable.
//depending on the results enumCheck and calling are prepared ahead of time for opDispatch
//returns an array of the results. accessing/aliasing them individually slows compiling to a crawl.
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
		//they retain the full patern minus the expanded parts. No way to represent that at present.
		enums ~= str~"=(1<<"~(to!string(i))~")|1,";
		structs ~= "alias "~str~" "~prefix~str~";";
		sizeChecks ~= "static assert("~dataStruct~".sizeof == "~str~".sizeof, \""~str~": "
				~"Polymorphic struct size doesn't match fixed size\");";
		
		i++;
	}
		enums ~= "}";
	return sizeChecks ~ structs ~ enums;
}
unittest {
	string expected = `static assert(Data.sizeof == Base.sizeof, "Base: Polymorphic struct size doesn't match fixed size");static assert(Data.sizeof == SubRecord.sizeof, "SubRecord: Polymorphic struct size doesn't match fixed size");alias Base Poly_Base;alias SubRecord Poly_SubRecord;enum _prefix="Poly_";enum _types="Base,SubRecord";enum Enum {Base=(1<<0)|1,SubRecord=(1<<1)|1,}`;
	string returned = expandTypes("Data", "Poly_", "Base,SubRecord");
	assert(returned == expected, expected~returned);
}

mixin template PolyMorphicInclude(PT) {
	PT polyBase;
	alias polyBase this;
//	@disable this();
}

//checks for potential matches and selects the best one. Checks for const/mutable signature breaks,
//and builds compatct enum checks. If there's only one possibility, only that is returned instead.
string makePolyMorphicCall(string VariableAccess, string TypeCheck,
		string prefix, string funCall, Enum, MutableState state, Vars ...)() {
	string calls;
	string results;
	string firstSuccess;
	int found;
	
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
	//trying and shouldn't be allowed any further
	auto ref opDispatch(string fun, Args ...)(auto ref Args args) @property
	if (fun.length <= _prefix.length*2 || fun[0 .. (_prefix.length*2)] != _prefix~_prefix) {
		enum x = makePolyMorphicCall!(VariableAccess, TypeCheck, _prefix, fun, Enum, MutableState.isMutable, args)();
		debug{
			writeln(to!string(data.polyMorphType));
			writeln(fun);
			writeln(x);
		}
		mixin(x);
	}

	auto ref opDispatch(string fun, Args ...)(auto ref Args args) @property const
	if (fun.length <= _prefix.length*2 || fun[0 .. (_prefix.length*2)] != _prefix~_prefix) {
		enum x = makePolyMorphicCall!(VariableAccess, TypeCheck, _prefix, fun, Enum, MutableState.isConst, args)();
		debug{
			writeln(to!string(data.polyMorphType));
			writeln(fun);
			writeln(x);
		}
		mixin(x);
	}
	
	auto ref opDispatch(string fun, Args ...)(auto ref Args args) @property immutable
	if (fun.length <= _prefix.length*2 || fun[0 .. (_prefix.length*2)] != _prefix~_prefix) {
		enum x = makePolyMorphicCall!(VariableAccess, TypeCheck, _prefix, fun, Enum, MutableState.isImmutable, args)();
		debug{
			writeln(to!string(data.polyMorphType));
			writeln(fun);
			writeln(x);
		}
		mixin(x);
	}

	///
	void setPolyMorphState(Enum e) {
		data.polyMorphType = e;
	}
	
	Enum getPolyMorphState() const {
		return data.polyMorphType;
	}

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

//okay, above does basic testing. Now we need to check more completely, so how about a cute little crypto?
//This is not intended as a serious crypto library, just enough to test several factors to ensure the polymorphic
//nature is being done.
struct PolyCryptoBase {
	//Enum, _prefix and _types don't exist until this calls.
	mixin(expandTypes("Data", "Poly_", "CryptoBase,CryptoXOR13,CryptoROT13"));

	struct Data {
		Enum polyMorphType;
		string original;
		char[] crypted;	//mutable copy
	}
	Data data;

	mixin PolyMorphic!(PolyCryptoBase, Data, "data");
}

struct CryptoBase {
	mixin PolyMorphicInclude!(PolyCryptoBase);
	
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

struct CryptoXOR13 {
	mixin PolyMorphicInclude!(PolyCryptoBase);
	
	//non matching signatures, but still compatible
	//requires 'static' or const to work, otherwise
	//potential for only calling a different version
	//may apply making some bugs hard to find.
	static void cipherChar(ref char c) pure @safe {
		c ^= 13;
	}
}

struct CryptoROT13 {
	mixin PolyMorphicInclude!(PolyCryptoBase);
	//ditto, our const-only version
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
	
//	c_crypto.setString("break!");	//won't compile due to const/mutable incompatibility
//	c_crypto.encipher;
//	writeln(c_crypto.data);

	//check if ref is working, plus const
	char c = 'A';
	c_crypto.cipherChar(c);
	assert(c == 'N');
	i_crypto.cipherChar(c);	//static so immutable is happy
	assert(c == 'A');
	
	crypto = PolyCryptoBase.factory("CryptoROT13");
	crypto.setString("(Whfg n yvggyr pelcgb grfg)");
	crypto.encipher;

	assert(crypto.data.crypted == "(Just a little crypto test)");
}
