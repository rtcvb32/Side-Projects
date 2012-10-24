/++
 Author:  Era Scarecrow <rtcvb32@yahoo.com>
 Date:    23 Oct 2012
 Project: Polymorphic Structs mini-crypto
 Licence: GPLv3
 
 Description: Mini crypto with the sole purpose of testing the PolyMorphicStructs

 Here's a stripped version for basic layout of how the structs are constructed.

---
struct PolyCryptoBase {
  ///Enum, _prefix and _types don't exist until this calls.
  mixin(expandTypes("Data", "Poly_", "CryptoBase,CryptoROT13"));

  struct Data {
    Enum polyMorphType;
    char[] crypted;
  }
  Data data;

  mixin PolyMorphic!("smartmerged.polymorphicstruct_crypto", PolyCryptoBase, Data, "data");
}

struct CryptoBase {
  mixin PolyMorphicInclude!(PolyCryptoBase);
  
  ///Does basic xor enciphering, individual letters only
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

module smartmerged.polymorphicstruct_crypto;

import smartmerged.polymorphicstruct;

/** This is not intended as a serious crypto library, just enough to test several factors to ensure the polymorphic
    nature is being done. As other items need to be tested this crypto will be improved to handle those tests.*/
struct PolyCryptoBase {
    ///Enum, _prefix and _types don't exist until this calls.
    mixin(expandTypes("Data", "Poly_", "CryptoBase,CryptoXOR13,CryptoROT13"));

    ///
    struct Data {
        Enum polyMorphType; ///
        string original; ///
        char[] crypted;    ///mutable copy
    }
    Data data; ///

    ///Seems opCmp opEquals and similar functions go in the poly base. That's livable.
    ///if not, forward reference to Poly_opCmp
    
    mixin PolyMorphicCompare!(PolyCryptoBase);
    mixin PolyMorphic!("smartmerged.polymorphicstruct_crypto", PolyCryptoBase, Data, "data"); ///
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
    crypto.setPolyMorphState("CryptoXOR13");
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
    i_crypto.cipherChar(c);    //static so immutable is happy
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
    
    assert(c_crypto > crypto);    //test polymorphic opCmp 
}

void main(){}