# Design decisions and the reasoning behind them

## Syntax decisions:

### Records, choice types:

Here the most important thing is to make it painfully obvious at a glance, that a type is a choice type, or a record type.
To this end:
1. Record and choice types should be prefixed with `record` or `choice` respectively, to make it explicit even in simple cases (only 1 type field, single line types etc).
2. Record and choice types should use different tokens: `{`, `;`, `}` for record, and `=`, `|` for choice types for complex cases (type with many fields)
3. Only one of them can have an optional field delimiter (out of `;` and `|`), to make sure they are still visually well separable with only mandatory characters
4. The optional delimiter is the record's `;`, because it is expected to be the more often used type.

### Functions:

1. Just as record and choice types are explicitly marked as such by the `record` and `choice` keyword, functions should be explicitly marked with the `function` keyword.  
Note that in a language like php I myself dislike the need to use `function` keyword for function definitions, as it is evident from the definition itself that it is a function. However, in the context of an IDL such as this, there is no function body to speak of, and without it, a function declaration can easily look like a record declaration from afar. Furthermore this is also a question of consistency, in this case, the IDL is designed as such that every main item consistently has its own mandatory keyword clearly identifying it to the reader (eg.: `namespace`, `record`, `choice`, `function`).
2. Function return types should be delimited with `->` from the parameter list.  
While in programming languages I like both the C style return value comes first, and the UML style returnvalue is marked as the "type" of the function with the same `:` delimiter as a field type would be, in the context of this IDL I prefer `->` style mainly for the following reason: it is just much more easily parseable by a quick glance than the other alternatives. For the C style, if the return type is a complex type, there is an additional hoop to jump through (however minor) thinking "wait, is this the return type, or the function name?" and with the `:` delimiter, it is already used for the parameter type declarations, so I have to look for the parameter list enclosing brace to see that yes, this is the return type.
3. Functions with always the same return value are signified with `unit` return type (would be `void` in C style languages) This is to signify the concept, that every function should return "something" if there is no meaningful dependency between the function's input parameters and the return value, then it will always return the same thing, the `unit` type which always has the same value. The main difference here from the `void` concept, is that void conceptually allows for us to net even know if the function returned or not (of course due to implementation details we de facto always know it in a C like language). This point I feel is more important in the intended usecase of this project: RPC calls, where the distinction of "this function always returns the same thing, but I should be able to depend on it returning" and "this function will (propably) do something, but is completely fire and forget and I wont ever hear back anything in return" is more meaningful.  
Note: currently I do not intend to implement the `void` concept to the IDL, but based on usage experience / feedback, it is definitely a feature candidate.
4. As the intended problem domain is RPC calls, mutable `reference` and `out` parameters are not supported, nor are they planned to be at a later date.
5. Should functions be first class citizens, just like types? As a consequence, should functions be able to take function parameters, and return function delegates just like any other type? As a functional programmer, I see great potential in API expressiveness with first class funcitions, but on the other hand, especially in the RPC context, passing around function delegates are their references is a major foot-gun that can cause hard to log, and hard to debug scenarios if used without care. My current thinking is: Allow on the AST / Parsing level, but make it an unsupported by default opt-in feature on the generator level.

## Disambiguation of uncommon words used above:
1. RPC -> Remote Procedure Call
2. Choice type -> Discriminated union type
3. IDL -> Interface Definition Language (or Layer)