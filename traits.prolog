%%
%%
%%  [struct, Name]
%%  [class, Name]
%%  [array, Type]
%%  [pointer, Type]
%%
%%

% traits/3
%

% If the arguments are all either types that are arithmetic types.
traits(isArithmetic, Type1, Type2) :-
    traits(isArithmetic, Type1), traits(isArithmetic, Type2).

% If the string is valid property of the type.
traits(hasMember, Type, init) :- type(Type).
traits(hasMember, Type, sizeof) :- type(Type).
traits(hasMember, Type, alignof) :- type(Type).
traits(hasMember, Type, mangleof) :- type(Type).
traits(hasMember, Type, stringof) :- type(Type).
traits(hasMember, Type, max) :- integerType(Type).
traits(hasMember, Type, min) :- integerType(Type).
traits(hasMember, Type, infinity) :- floatingType(Type).
traits(hasMember, Type, nan) :- floatingType(Type).
traits(hasMember, Type, dig) :- floatingType(Type).
traits(hasMember, Type, epsilon) :- floatingType(Type).
traits(hasMember, Type, max) :- integerType(Type).
traits(hasMember, [struct, _, [Field, Type]], Property) :-
    type(Type), Field = Property.

% traits/2
%

% If the arguments are all either types that are arithmetic types.
traits(isArithmetic, Type) :- traits(isIntegral, Type).
traits(isArithmetic, Type) :- traits(isFloating, Type).

% Works like isArithmetic, except it's for integral types.
traits(isIntegral, [enum, Type]) :- signedIntegerType(Type).
traits(isIntegral, [enum, Type]) :- unsignedIntegerType(Type).
traits(isIntegral, Type) :- signedIntegerType(Type).
traits(isIntegral, Type) :- unsignedIntegerType(Type).

% Works like isArithmetic, except it's for floating types.
traits(isFloating, [enum, Type]) :- floatingType(Type).
traits(isFloating, Type) :- floatingType(Type).

% Works like isArithmetic, except it's for unsigned types.
traits(isUnsined, [enum, Type]) :- unsignedIntegerType(Type).
traits(isUnsined, Type) :- unsignedIntegerType(Type).

% Works like isArithmetic, except it's for scalar types.
traits(isScalar, [enum, [pointer, Type]]) :- traits(isScalar, [pointer, Type]).
traits(isScalar, [enum, Type]) :- traits(isArithmetic, Type).
traits(isScalar, [pointer, Type]) :- type(Type).
traits(isScalar, Type) :- traits(isArithmetic, Type).

% If the type's default initializer is all zero.
traits(isZeroInit, void).
traits(isZeroInit, byte).
traits(isZeroInit, short).
traits(isZeroInit, int).
traits(isZeroInit, long).
traits(isZeroInit, cent).
traits(isZeroInit, ptrdiff_t).
traits(isZeroInit, bool).
traits(isZeroInit, ubyte).
traits(isZeroInit, ushort).
traits(isZeroInit, uint).
traits(isZeroInit, ulong).
traits(isZeroInit, ucent).
traits(isZeroInit, size_t).
traits(isZeroInit, string).
traits(isZeroInit, [enum, byte]).
traits(isZeroInit, [enum, short]).
traits(isZeroInit, [enum, int]).
traits(isZeroInit, [enum, long]).
traits(isZeroInit, [enum, string]).
traits(isZeroInit, [enum, cent]).
traits(isZeroInit, [enum, ptrdiff_t]).
traits(isZeroInit, [enum, bool]).
traits(isZeroInit, [enum, ubyte]).
traits(isZeroInit, [enum, ushort]).
traits(isZeroInit, [enum, uint]).
traits(isZeroInit, [enum, ulong]).
traits(isZeroInit, [enum, ucent]).
traits(isZeroInit, [enum, size_t]).
traits(isZeroInit, [pointer, Type]) :- type(Type).
traits(isZeroInit, [array, Type]) :- type(Type).
traits(isZeroInit, [struct, Name]) :- ident(Name).
traits(isZeroInit, [struct, Name, [Field, Type]]) :-
    ident(Name), ident(Field), traits(isZeroInit, Type).
traits(isZeroInit, [class, Name]) :- ident(Name).
traits(isZeroInit, [union, Name]) :- ident(Name).


% utils

keyword(Id) :- basicDataType(Id).
keyword(Id) :- Id = enum.
keyword(Id) :- Id = struct.
keyword(Id) :- Id = class.
keyword(Id) :- Id = union.
keyword(Id) :- Id = size_t.
keyword(Id) :- Id = ptrdiff_t.
keyword(Id) :- Id = string.

type(Type) :- basicDataType(Type).
type(Type) :- derivedDataType(Type).
type(Type) :- userDefinedType(Type).
type(Type) :- Type = size_t.
type(Type) :- Type = ptrdiff_t.
type(Type) :- Type = string.

basicDataType(Type) :- Type = void.
basicDataType(Type) :- Type = bool.
basicDataType(Type) :- Type = byte.
basicDataType(Type) :- Type = ubyte.
basicDataType(Type) :- Type = short.
basicDataType(Type) :- Type = ushort.
basicDataType(Type) :- Type = int.
basicDataType(Type) :- Type = uint.
basicDataType(Type) :- Type = long.
basicDataType(Type) :- Type = ulong.
basicDataType(Type) :- Type = cent.
basicDataType(Type) :- Type = ucent.
basicDataType(Type) :- floatingType(Type).
basicDataType(Type) :- Type = char.
basicDataType(Type) :- Type = wchar.
basicDataType(Type) :- Type = dchar.

derivedDataType([array, Type]) :- type(Type).
derivedDataType([pointer, Type]) :- type(Type).

userDefinedType([enum, Type]) :- not(Type = void), type(Type).
userDefinedType([struct, Id]) :- ident(Id).
userDefinedType([class, Id]) :- ident(Id).
userDefinedType([union, Id]) :- ident(Id).

floatingType(Type) :- Type = float.
floatingType(Type) :- Type = double.
floatingType(Type) :- Type = real.
floatingType(Type) :- Type = ifloat.
floatingType(Type) :- Type = idouble.
floatingType(Type) :- Type = ireal.
floatingType(Type) :- Type = cfloat.
floatingType(Type) :- Type = cdouble.
floatingType(Type) :- Type = creal.

integerType(Type) :- unsignedIntegerType(Type).
integerType(Type) :- signedIntegerType(Type).

unsignedIntegerType(Type) :- Type = bool.
unsignedIntegerType(Type) :- Type = ubyte.
unsignedIntegerType(Type) :- Type = char.
unsignedIntegerType(Type) :- Type = wchar.
unsignedIntegerType(Type) :- Type = dchar.
unsignedIntegerType(Type) :- Type = ushort.
unsignedIntegerType(Type) :- Type = uint.
unsignedIntegerType(Type) :- Type = ulong.
unsignedIntegerType(Type) :- Type = ucent.
unsignedIntegerType(Type) :- Type = size_t.

signedIntegerType(Type) :- Type = byte.
signedIntegerType(Type) :- Type = short.
signedIntegerType(Type) :- Type = int.
signedIntegerType(Type) :- Type = long.
signedIntegerType(Type) :- Type = cent.
signedIntegerType(Type) :- Type = ptrdiff_t.


ident(Id) :- not(keyword(Id)).
