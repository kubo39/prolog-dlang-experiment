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

% traits/2
%

% If the arguments are all either types that are arithmetic types.
traits(isArithmetic, Type) :- traits(isIntegral, Type).
traits(isArithmetic, Type) :- traits(isFloating, Type).

% Works like isArithmetic, except it's for integral types.
traits(isIntegral, Type) :- signedIntegerType(Type).
traits(isIntegral, Type) :- unsignedIntegerType(Type).

% Works like isArithmetic, except it's for floating types.
traits(isFloating, Type) :- floatingType(Type).

% Works like isArithmetic, except it's for unsigned types.
traits(isUnsined, Type) :- unsignedIntegerType(Type).

% Works like isArithmetic, except it's for scalar types.
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
traits(isZeroInit, [struct, Id]) :- ident(Id).
traits(isZeroInit, [class, Id]) :- ident(Id).
traits(isZeroInit, [union, Id]) :- ident(Id).
traits(isZeroInit, [pointer, Type]) :- type(Type).
traits(isZeroInit, [array, Type]) :- type(Type).


% utils

type(Type) :- Type = void.
type(Type) :- Type = floatingType(Type).
type(Type) :- Type = signedIntegerType(Type).
type(Type) :- Type = unsignedIntegerType(Type).
type(Type) :- Type = string.

type([struct, Id]) :- ident(Id).
type([class, Id]) :- ident(Id).
type([union, Id]) :- ident(Id).

type([array, Type]) :- type(Type).
type([pointer, Type]) :- type(Type).

floatingType(Type) :- Type = float.
floatingType(Type) :-Type = double.
floatingType(Type) :-Type = real.
floatingType(Type) :-Type = cfloat.
floatingType(Type) :-Type = cdouble.
floatingType(Type) :-Type = creal.
floatingType(Type) :-Type = ifloat.
floatingType(Type) :-Type = idouble.
floatingType(Type) :-Type = ireal.

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


ident(Id) :- not(type(Id)).
