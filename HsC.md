
#The HsC language

HsC is a statically typed language with optional garbage collection.
It combines features from object-oriented languages such as C++ and C# 
with concepts from functional languages like Haskell.

#Tutorial

This is a simple introduction to using the language.

##Functions

A function can take zero or more parameters and can possibly return a result.
By default, functions can have side effects.
A function declaration looks like this:

    f x y = x * y
    g a cond = if cond then a > 0f 
               else a < 0f

The automatic type inference makes it so `f` will work on any types 
X, Y for which a function (*) : x -> y -> a is defined.
`g` explicitly uses a Float through the syntax `0f`, 
and so it has the type Float -> Bool -> Bool.
    
Functions are called like this:

    h x = g (f x 1) True
    
It is possible to explicitly constrain the type of a parameter:

    addfi x:Float y:Int = x + fromInt y
Or if there are multiple parameters of the same type:

    addf x, y:Float = x + y
By default, the return value of a function is the result of its last statement.
It is possible to explicitly return through `return expr`.
An explicit return value is declared like this:

    addf x, y:Float -> Float = x + y
However, this is rarely needed.

###Named arguments
A function can be called with explicitly named arguments, which can be provided in any order.

    stuff = addf 'y 5f 'x 7f
    stuff2 = addf 'y (something a b) 'x (somethingelse 'c 5 'd 6)

Functions can declare default parameters, which do not need to be provided.
Default parameters are only used if the function is called with named arguments.

    drawChar text:Char (color = Black) = ...
    drawBlackChar ch = drawChar 'text ch
    drawWhiteChar ch = drawChar ch White
    
##Operators
An operator is a function that takes one or two arguments.
Operators can be defined like normal functions:

    (*) = mul
    (<>) lhs rhs = if valid lhs then lhs else rhs
    (-) a = negate a
    
An operator can be binary or unary, where binary operators can be left- or right associative. This can be declared separately, together with precedence:

    prefix 10 (-)
    infix 6 (*)
    infix 5 (+)
    infixr (<>)
	
    
##Statements
Each block consists of one or more statements.
A block is opened after one of the following:
 * A function declaration `f x = `
 * A block declaration `block b = `
 * An if expression `if cond then stmt else stmt`
 * A for loop: `for x in sequence do`
 * A while loop: `while cond do`
 * A case expression: `case expr of`

A block starts with the first non-whitespace character after its opener. 
Each statement must have the same indentation, which must be higher than the previous block.

Examples:

    f x = x // Single line block
    g x =
      let x = x + 2 // Multi-line block
      let y = x * 2
      y
    
    h x = if x ≠ 5 then True else False ; Single-line block
    
    i x =                      ; Equivalent to h
      if x ≠ 5 then
        True
      else
        False
        
    j x = if x ≠ 5 then True     ; Equivalent to h
          else False
        
    k x:Float = 
      let x:Int = truncate x
      case x of
        0..3 -> True
        6..7 -> True
        80   -> True
        _    -> False
        
##Comments
Comments are the same as in C/C++.
A single-line comment is indicated by `//`. 
This means that the rest of the line it appears on is a comment.
A documentation comment starts with `///`:

    /// Draws a line to the current canvas.
    /// start: The first point of the line.
    /// end: The second point.
    /// color: The color to draw with
    drawLine start end color = ...
    
A multi-line comment starts with `/*` and ends with `*/`, 
while a multiline documentation comment starts with `/**` and ends with `**/`.
    
##Variables and constants
A global or function constant is declared with `let name =`:

    gBufferSize = 256
    gMaxThreads = 16
    
    something x =
      let maxCount = x*2
      let minCount = x/2
      (maxCount, minCount)
      
A constant cannot be changed after it has been assigned, but its value may change for each
execution of the program/function.

A variable is declared with `var name =`:

    var gCurrentThreads = 1
    
    countUp bound =
      var c = 0
      for _ in 0..bound do inc c
      c
A variable can be changed after it has been assigned. 
A variable declared outside of a function exists for the duration of the program.

##Types
HsC supports many different ways of dealing with types.

###Primitives
HsC contains the following builtin primitive types:

    U8  // Unsigned 8-bit integer
    I8  // Signed 8-bit integer
    U16 // Unsigned 16-bit integer
    I16 // Signed 16-bit integer
    U32 // Unsigned 32-bit integer
    I32 // Signed 32-bit integer
    U64 // Unsigned 64-bit integer
    I64 // Signed 64-bit integer 
    
    F32 // 32-bit floating point
    F64 // 64-bit floating point
    Ref // garbage-collected safe reference
    Ptr // unsafe pointer type
    Vec // SIMD vector type
The following additional primitive types are defined in the standard library:

    Int     // alias for I32
    Size    // integer, has the same size as a native pointer
    Float   // alias for F32
    Double  // alias for F64
    Char    // part of a UTF-8 character
    Unichar // a UTF-32 character
    String  // a string of characters, with special operations
    Array   // a fixed-size array of some type
    
### Aliases
An alias is a different name for the same type, 
and can be used interchangeably with the type it represents:

    type Index: Int
    f index:Index = ...
    
    g = f 5  // This is correct, since Index is an alias for Int.
    
It is also possible to declare an alias that is incompatible with the underlying type:

    newtype Index: Int
    f index:Index = ...
    
    g = f 5          // This is not correct, since Index is a different type.
    g = f (Index 5)  // This is correct - we construct an instance of Index with an Int.

### Arrays
An array is a fixed-size set of objects. It is created like this:

    ints = [0, 1, 2, 3, 4, 5]
    

### Tuples
A tuple is a combination of two or more types. 
It is mostly used to easily define anonymous aggregate types, 
and to return multiple values from a function.
A tuple can be used like this:

    dimension = (10, 5)
    area =
      (w, h) = dimension
      w * h

### Data types
A Data type is an aggregate of multiple other types:

    data Rectangle = var left, right, top, bottom: Int
    data Something =
      bool: Bool
      int:  Int
      x, y: Float
      
You can define functions that operate on data:

    width r:Rectangle = r.right - r.left

It is also possible to define a function inside the type.
In that case, the compiler will automatically add a parameter called `self` as
the first parameter, which can be accessed implicitly:

    data Dimension = 
      var width, height: Int
      area = width * height

Functions that operate on a type can be called like normal functions:

    somearea = area (Dimension 4 5)
They can also be called using a special scope syntax:

    somemorearea = (Dimension 6 5).area

Data types can contain special functions called `init`. 
These are called implicitly when a data type is created:

    data Shape =
      color:Color
      size:Dimension
      init   = init White
      init c = 
        color = c
        size = Dimension 0 0
    
    f = Shape       // Makes a white shape
    g = Shape Black // Makes a black shape
    
In most cases, the constructor will consist of simply setting a value.
Therefore, it is possible to define default constructors:

    data Shape = 
      let color:Color = White            // Use a white color by default
      let size:Dimension = Dimension 0 0

Because the field type can now be inferred, we can even omit the type:

    data Shape =
      let color = White
      let size = Dimension 0 0
      
The type can now be constructed in several ways:

    f = Shape              // Still makes a white shape
    g = Shape 'color Black // There are no explicit constructors, so we tell it 
                           // what fields we set.

### Variants
Sometimes we want to create a type that can have several states, each with its own data.
For this, we can use variants:

    variant Color = White | Black | Red | Green | Blue

    variant Expr = Num Float
                 | Var String
                 | Add &Expr &Expr
                 | Mul &Expr &Expr // We have to use references here, or the type would 
                                   // have an infinite size.

Variants can be used in the following way:

    calc e = case e of
                Num n       -> n
                Var s       -> lookupVar s
                Add lhs rhs -> calc lhs + calc rhs
                Mul lhs rhs -> calc lhs * calc rhs

This can be made to look better by using pattern matching directly:

    calc (Num n)       = n 
    calc (Var s)       = lookupVar s
    calc (Add lhs rhs) = calc lhs + calc rhs
    calc (Mul lhs rhs) = calc lhs * calc rhs

In order to support some common use cases of enumerations you can convert the type of a variant to a number:

    enum a = ... // Returns a zero-based index for the provided variant constructor.
                 // Red.enum == 2
                    
### Classes
In addition to variants, HsC also supports polymorphic classes.
Classes mostly work the same way as Data types, but additionally provide the following:
 * Inheritance - a class can inherit at most one other class and any number of traits.
 * Runtime polymorphism - a class can override functions from its base class.
 * Runtime types - a class contains type info that can be retrieved at runtime (as opposed to static info for other types).

A class is defined like this:

    class Shape =
      'area = 0             // Defines a polymorphic function.
      'draw: Context = ()   // Defines an abstract polymorhic function.
      
      color c = color_ = c  // Defines a normal function.
                updateColor
      init c = color c
                 
      var color_ = White
      
    class Circle: Shape =   // Defines a class that inherits from Shape.
      var radius = 0f
      'area = pi*pi*radius  // Overrides the polymorphic function.
      'draw = ...
      init = base.init Black  // Initializes the base class.
                              // If no initializer is provided, it is inherited from the base.

To dynamically cast a base class to a superclass there is the `as` operator.
`as` returns an object of the type `?a`, which is short for `Maybe &a`.
This means that it is necessary to check if the cast succeeded before using the result.

    f x:Shape = if r = (x as Circle)?.radius then r
                else 0f
It is also possible to check if a reference is of a certain type through the `is` operator.

    g x:Shape = x is Circle
	
### Monads
The type system in HsC supports implementing monad types without any magic needed.
However, we define a special syntax for using monads, since using operators would make it complicated and ugly.
Like in OCaml, the syntax is used through 'perform' blocks:

    // We assume that we have a Parsec-like parser library here.
    if_expr -> Parser Expr = perform
        reserved "if"
        cond <- expr
        reserved "then"
        true <- expr
        reserved "else"
        false <- expr
        return (IfExpr cond true false)

This can be implemented in a very simple way.
 * A normal function call statement is translated to "call >> next_stmt"
 * An "x <- call" statement is translated to "call >>= (\x -> next_stmt x)"
	
## Memory
HsC uses garbage collection by default. An object can be allocated with the `new` operator:
    dim -> new Dimension 0 0
This returns a reference of type `Dimension'`. References mostly work the same way as in 
languages like C#, but a large difference is that a reference always contains a valid value.
To represent optional values there is the `Maybe` type, with special syntax to remove boilerplate:

    data Node =
      var transform:Float4x4? = Nothing
      
      position = if Just transform then transform * zero
                 else zero::Float4
	
## Reflection
HsC supports static reflection for all types and dynamic reflection for classes.
Reflection data can be retrieved with the `typeinfo` function on a type or instance:

    F32.typeinfo.name == "F32"
    F32.typeinfo.size == 4
    0f.typeinfo.fields == []
    False.typeinfo.name == "Bool"
    
    Dimension.typeinfo.fields == [("width", 0, F32.typeinfo), ("height", 4, F32.typeinfo)]

When called on a class *instance*, typeinfo returns the dynamic type of the instance. Example:
    
    class A = var a : Int
    class B : A = var b : Int
    
    f x:#A = x.typeinfo.fields.size
    
    do f (#B 2 3) // Returns 2, not 1.
    
Reflection can be used for automatic serialization and deserialization, and a lot more.

##Blocks
Code blocks are defined by indentation. Each new expression in a block must have the same indentation as the first non-whitespace character after a block opener. A code block as a whole is also an expression which takes the value of the last expression in the block. A line with higher indentation than the first line is part of the last expression on the previous line, while a line with lower indentation closes the block.

Blocks are opened by the following:
 * The (=) operator
 * `var`
 * `let`
 * `do`
 * `then` after `if`
 * `else` after `if`
 * `of` after `case`

As a special case, the else-part of an if-statement can be on the same line as the `if` to improve readability.
It is also possible to separate function arguments by line; each line will act as surrounding parentheses around its contents.

Examples:

    let x = 5 `pow` 6
    let y = x * x      // Two separate expressions in the same block.
    
    let z = let a = 5
            a * 2      // x == 10, while a goes out of scope after this line.
            
    var
      a = 10
      b = 5
      c = 15           // Three variable bindings.
    
    number a = 1 * 2 * 3 * 4 * 5 *
                  6 * 7 * 8
                  * 9 * a             // This is ugly, but valid
                  
    if number 5 == 10 then
      something 10
      10
    else
      number 5
            
    fn_with_many_params
      something x y
      somethingElse z 6
      23 * number 8
      0
      Nothing
