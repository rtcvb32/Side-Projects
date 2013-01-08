/**
    Author: Ryan Cecil
    Date: Jan 08 2013
    Description: Sometimes the return value of something isn't on a particular variable,
                but from a particular state. Variants and semi-polymorphic types can make
                use of this.

Example:
---
    struct S {
        bool choice;
        int a = 100;
        int b = 200;

        mixin(multiAccess!(
                int,        //return type
                "test",     //function call/name
                "nothrow",  //attributes
                choice,     //variable/call that determines which to call
                true,       //make read function
                true,       //make write function
                a, false,   //choose a if 'choice' is false
                b, true));  //choose b if 'choice' is true
    }

    S s;
    assert(s.test == 100);
    s.test = 150;
    assert(s.test == 150);
    
    s.choice = true;
    assert(s.test == 200);
    s.test = 250;
    assert(s.test == 250);

    //manual confirmation of values in correct spots
    assert(s.a == 150 && s.b == 250);
---
*/

module multiaccess;

import std.exception;

/** redirect access for multiple(s) based on a single redirection value.
    Type   - return/set type.
    name   - function/calling name
    attributes - "nothrow @safe" etc
    choiceName - Variable in which determines which variable/call to make
    read   - Make read access function
    write  - make write access function
    T      - Pairs, Variable & matching value (that choiceName needs to match to call)
*/
template multiAccess(Type, string name, string attributes, alias choiceName, bool read, bool write, T ...) {
    static assert(T.length % 2 == 0, "MultiAccess: Must be in pairs, AccessName & exact value");
    static assert(read || write, "MultiAccess: '"~name~"' Needs to read and/or write");
    enum multiAccess = multiAccessFunctions!(Type, name, attributes, choiceName, read, write, T);
}

//separates to read/write versions
template multiAccessFunctions(Type, string name, string attributes, alias choiceName,  bool read, bool write, T ...) {
    enum multiAccessFunctions = (read ? multiAccessRead!(Type, name, attributes, choiceName, T) : "")
                ~ (write ? multiAccessWrite!(Type, name, attributes, choiceName, T) : "");
}

//read & write different enough to require separate functions
template multiAccessRead(Type, string name, string attributes, alias choiceName, T ...) {
    enum switchValue = "switch(" ~ choiceName.stringof ~ ") {" ~ multiSwitchCases!(true, T) ~ "}";
    enum multiAccessRead = Type.stringof ~ " " ~ name ~ "() " ~ attributes ~ " const { "
            ~ switchValue ~ "}";
}

template multiAccessWrite(Type, string name, string attributes, alias choiceName, T ...) {
    enum switchValue = "switch(" ~ choiceName.stringof ~ ") {" ~ multiSwitchCases!(false, T) ~ "}";
    enum multiAccessWrite = "void " ~ name ~ "(" ~ Type.stringof ~ " value) " ~ attributes ~ " { "
            ~ switchValue ~ "}";
}

//based on switch case, recursive call creates each 'case' and it's appropriate return/set
template multiSwitchCases(bool isRead, T ...) {
    static if (T.length > 2) {
        enum next = multiSwitchCases!(isRead, T[2 .. $]);
    } else {
        //default 'no match' case
        enum next = "default: assert(0, \"multiAccess Unknown Value!\");";
    }
    enum multiSwitchCases = "case " ~ T[1].stringof ~ ":"
        ~ (isRead ? "return " ~ T[0].stringof ~ ";"
                  : T[0].stringof ~ " = value;break; "
        ) ~ next;
}

unittest {
    struct S {
        int choice;
        int a = 100;
        int b = 200;
        int c = 300;

        mixin(multiAccess!(int, "test", "@safe pure nothrow", choice, true, true,
            a, 0,
            b, 1,
            c, 2));
    }

    S s;
    s.choice = 0;
    assert(s.test == 100);
    s.test = 150;
    assert(s.test == 150);
    
    assert(s.a == 150 && s.b == 200 && s.c == 300);

    s.choice = 1;
    assert(s.test == 200);
    s.test = 250;
    assert(s.test == 250);

    assert(s.a == 150 && s.b == 250 && s.c == 300);

    
    s.choice = 2;
    assert(s.test == 300);
    s.test = 350;
    assert(s.test == 350);

    assert(s.a == 150 && s.b == 250 && s.c == 350);
    
    s.choice = 100; //only 0-2 valid
    assertThrown!(Error)(s.test, "Impossible multiaccess value/read");
    assertThrown!(Error)(s.test = 1, "Impossible multiaccess value/write");
}
