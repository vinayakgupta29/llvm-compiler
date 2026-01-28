# Rules :

## Basic Syntax Rules 
- Scope is defined by `{}`
- Lines are ended by semi-colon or new line char
```
print("Hello")
print("World")

print("My Name is");print("hello")

```
- Single line comments are `//` and multi line comments are `/{ }/`
- Documentation comments are defined with `!/{...}/` and must be followed by a member (variable,function, procedure, type, module, operator, typeclass)
- Accessibility is defined by capitalization for all members including primitive types

## Naming Convention
- variables/constants and functions are all in snakeCase such as sin, toString, pi, e.
- Modules, Sub-modules and Types are all PascalCase like Int, Float, Bool, Math, Math.Trig. 
- when aliasing modules although `m = @using("Math")` is allowed but conventionally should be `M = @using(Math)` or `Tr=@using("Math.Trig")`

## Variables/Names
- By default names are immutable.
- Names are single bindings by default
- They can only be parsed by value no parsing by reference.
- lifetime is only as long as the scope. The scope goes out of execution the variable is out of accesibility and the memory is cleared.
- variable lifetime is Like Rust's ownership model. Just there is no borrowing here.
- when the user does the following:
```
a = 5
b = a
```
only a's value is copied to b and a is still alive cause it's in the same scope.
- And we also allow shadow Bindings like Rust and Pascal. so 
```
a = 5
print(a) // prints 5
{
    
    print(a) // still prints 5
    a = 6
    print(a) // prints 6
} // this is allowed cause internally a inside the scope is different and a outside the scope is different.
```
- white spaces are obsolete in variable declaration. In the example is just for syntax
- Names are accessible to the children scope with a `super` keyword for each level used only in modules. similar to rust.
- But in case of say Mod.Sub.Sub.Sub.somFunc() and then taking some constant from the root module say Mod in this function the call would create a hell of super like super.super.super.some_const which is not dev friendly.
- Hence only for Modules we allow using a `@root` directive refering to the module root. so instead of writing super.super.super.some_const we can write @root.some_const. and this is resolved at compile time.
- Any Name that is accessed outside the given scope must be explicitly declared with super if it's from a parent module else would be resolved from the modules imported in the project. 
```
d : Int;
x : Int = 5;
y : Int = 5;
a,b : Uint = 4,5;
```
- types are optional but once defined can't be re-assigned. so a variable defined as Int must be only assigned an int value.
```
hello := 5 // only readable inside the module. unless specified in the module export list
:= // this is a compile time type inference and 
= the type must be specified as in the previous example with types.
```

## Resuable Code Block 

### Functions 
Code blocks with a return value.

### Methods
Code blocks with no return value and just a procedure. For use cases Methods return the value Nil. Nil can't be assigned it's equivalent to haskell's Nothing.

- Functions and methods both can be curried by default like haskell
- When currying the args are handled positionally so any arg at any point is replaced by _ is then available for currying
- Functions support composition using `f . g` here the white space is significant
- Functions are first class members and can be parsed as arguments to higher order functions.
- Return keyword is replaced by `=>` 
- Procedures are defined with `proc` keyword and functions with `func`.
- a procedure must always return Nil.
- Nil behaves the same way as Rust's Unit hence it's a single valued type defined with no value
`data Nil`

```
// By defult arguments are parsed positional and in currying if any arg is replced by a `_` then is available for the curried function.
func f(x, y) => x * 2 + y

func f1(x, y: Int) -> Int {
    // other expressions
    => x ^ x * y;
}

f2 = \(x) => x * 2

f3 = \(x, y) {
    a = x ^ 2
    b = y ^ 2
    => a + b;
}

func f4 = f(2, _)  // partial application
f4(4)              // returns 8
func f6(a,b,c)
func f7 = f6 (_,b,c) // here if anyone calls f7 with an arg then that arg is placed at _
func f8 = f6 (a,_b,c)
// and you can also do it named like
func f9 (x) = f6(x,b,c)
f5(f = (a, b: Int) -> Int, c) -> Int {
    // function that takes a function as an argument
}

f5(f3, 0)          // call example

proc print(s:string){
os.stdin.write(s)
}

print("Hello");
a = print("World"); // this is illegal and throws a compiler error
```

- Functions and operators are handled by pattern matching like haskell the call site matches with the signature of definition if not matched then throw an error undefined function f(Vec3i, Vec3i)->Vec3i and similar for operators.
```
func f (x,y :A)->A // signature only
f(x,y:Int)->Int {
    => x+y;
}
f(a,b:Vec2i)->Vec2i{
    => Vec2i{

    }
}
```

## Types 
- Bool = true, false, t, f equivalent to i1.
- Int = Limit to Intmin and Intmax depending on the architecture is i32 by default.
- Uint = Limit to Uintmin and Uintmax depending on the architecture is u32 by default.
- Float = Limit to Floatmin and Floatmax depending on the architecture is f32 by default.
- Also contains (I1, I8, I16, I32, I64, I128, U1, U8, U16, U32, U64, U128, F32, F64).
- Char = in single quotes 'a', '\n' equivalent to i8.
- UniChar = stores unicode chars equivalent to i32.
- UniString = stores unicode strings equivalent to i32[]. have to constructed using `"0#hello"`
- String = in double quotes and can be treated as a vector of chars 
- Types have zero values if a name is declared but not defined a value then the zero value is assigned lazily and used.
- The types module contains the min and max values for each type accesible as that type only. as `i8_min`, `i8_max`, `u8_min`, `u8_max` etc. 

| Type  | Zero Value |
|-------|-----------|
| String | ""        |
| Char   | ''        |
| Int    | 0         |
| Float  | 0.0       |
| Bool   | false     |
```
str = "Hello"
str@1 // return e as char
```
- Uppercase single letters can be used as type generics but need to be consistent
```
func Eq(A,A)->Bool // This is equivalent to haskell
Eq :: a -> a -> Bool
```

## Collection Types
- `[1,2,3]` Vector
- `[[1,2,3],[2,3,4]]` N-D Tensor can't je jagged but have to be specific in their dimensions for example the given one is 2x3, 
- #{a, b, c} Set // Must be unique in value. Multidimensional sets are allowed, set must be of a single type.
```
#{[1,2,3],[2,3,4]} // this is a set of vetors of size 3 and type Int
```
- members of set and Vectors are accesible by @ operator but has an inbuilt length check.
```
var = [1,2,3]
var@0 // return 1
//declaration with static type and size 
var1 :Int[6]
// Here if the 
```
## Operators
- Infix and prefix both are allwed
- prefix must be defined inside a paranthesis like Clojure even more complex in the same way as S expressions.
```
(+ 3 4)
(* 5 2)
```
- on arithmetic operators we do same types on both sides so no co-ersion on int to float.
```
1+0.2 // not allowed
1.0+2.0 // allowed
```
- Bitwise operator are only to be performed on boolean or Special strings like binary string , oct Strings and hex string "0x"(hex string) "0b00100"(binary string) "0o750" (Oct string) and not on int and floats
- Operators must be defined with haskell like type generics and have gaurd clauses to handle them for each type.
Arithmatic operators : +,-,*,/,^(exp only defined on Int and Uint), _/_ (floor division)
Logical Operators : == (strict value based equality), >=, <=, /= (not equal to), &&, ||, ! (logical not)
Bitwise operators : .&, .|, .^ (XOR), .~ not.
- The minus would be both infix and prefix as in `4 - 5` and `(-6)` 
- Negative numbers should be in parentheses
- Floats can also be defined in the e notation like 1e10 or 1e(-10)

## Control Flow

### If-Else
```
if (cond) {
=> result;
}else {
=> result 
}
// in an if-else block if the `if` block returns then else block must also return the same type if not then just calculate the expressions
```
- if can't exist without else. 
- both branches must have the same return type.

```
if (x>0){
    print("Positive")
}else{
    print(Negative)
}// here print is a procdeure and returns a Nil type as discussed
```

### Gaurd Clauses
- Gaurd clauses are syntactic sugar for boolean conditions and just makes the if-elif-else ladder look better and easy to read.
- gaurd clauses must be enclosed within a {} block returns values in the following manners
- gaurd clauses are non-exhaustive and the default case with `_` or `otherwise` must be defined if there is a gaurd clause without a _ case then 
throw a compile time error and terminate the compilation. 
- gaurd clauses are matched in the sequence defined by the user. so it is advised to write the _ / otherwise caluse at the last.

> Note : Otherwise is an alias for True defined in the Prelude not a language keyword. and can be used anywhere but 
> `_` is reserved it served as a syntactic sugar for placeholders, in gaurd clauses it is used to be replaced by otherwise/default case
> In Tuples it is used to drop values like (_,b) = t1 
> In function currying it is used as a placeholder as defined above in section [Functions](#resuable-code-block)

```
abs(x:Int)->Bool{
    | x < 0 => x*(-1)
    | otherwise => x // otherwise here is not a keyword it is an alias for True defined in the Prelude
}
myfunc(x:Int)-> String {
    | x == 1 => "Is Unity"
    | x in [2,5,7,11] => {
        //other expression
        => "Is Prime"
    }
    | x in [13,...=19] => "Is Teen"
    | _ => "Too Big"
}
val = 5
result = {
    | val > 5 => "Too Small"
    | val == 5 => "Limited"
    | _ => "Too big"
} // here result will be assigned the string "Limited"
```
## Lists 
- can be defined in with fixed size or variable size but must be type homogenous
```
xs = [1,2,3]
ys :Int[];
zs :Int[6]
```
- Lists are linked-lists under the hood.
- Concatenation is handled the same way as haskell 
- `:` this is the cons operator in haskell
- `++` is a list concat operator

## Composite Types Structs
- Rust/Zig like structs with methods in them.
```
data Vec2i {
    x:Int = 0,
    y:Int = 0,
    }
val = Vec2i{
x= 5,
y=6,
}

```
- Allows you to define default values. 
- If no value is assigned then  defult is used. But can't be re-assigned later. By later i don't mean during run time i meant that if no value is given it is assigned the default/zero values at compile time as a default constructor. Still maintaining the single-binding rule. 
```
v:Vec2i 
/{ this is initialized as Vec2i {
    x=0,
    y=0,
    } and can't be re-assgned later in the code unless it's defined as a function arg.
    }/
    
```
- Structs/Records can either be initialized by named parameters like above or can be pattern matched to imply memmbers positionally
```
val1 = Vec2i(1,2) // this is same as val := Vec2i(x=1,y=2)
```
- Structs/data 

## Sum types or Enums

- they are similar to enums in Rust and Sum types in functional languages
```
data Maybe x = Nothing | Just x;

data Color = 
    | RED | BLUE 
    | GREEN;
```
- here the semicolon is necessary and white spaces and new lines in Sum type definition are not really significant.
- The type Bool is actually a Sum type defined as 
`data Bool = True | False`

## Custom operators
Similar to haskell but defined as 
```

(+)(infixr 5)(a,b:Vec2i) -> Vec2i{
    => Vec2i{
        x =  a.x+b.x
        y = a.y+b.y
    }
}
```
- Fixity (infixl, infixr, infix) is to be declared haskell like outside the definition body but near it for readability
- An operator can only take max of 2 args and min of 1 chars
- operators are functions but infix
- only special chars are allowed in an operator so ^[^a-zA-Z0-9]
- There is no operator overloading if an operator is defined on a class and that class is in scope so new operator can't be defined if found then throw error at compile tim.

## Custom Types/Type classes

- A type class shows capability constraints and tells you what can/must do
- Can be used in a function and is Resolved at compile time.
- Can be defined as 
- It also defines operators as well and operator overloading is performed with typeclass instaces
- Typeclasses can have default behavior if needed or else define just function signature.
```
class Numeric A {
(+)(infixr 5)(A, A) -> A

}

instance Numeric Int {
    (+)(x : Int,y: Int) -> Int {
        x+y
    }
}
```
## Modules
- Modules are hierarchical
- The modules name must match the file name case-insensitive.
- a Sub-module can be defined in the same file as a parent module but can also be defined in a different file at a dir level.
```
//math.vks
mod Math (PI, CONST_E, Trig) {
    PI : Float = 3.141592;
    CONST_E := 2.718281828459045
    exp(x,y:Float):Float{}
    +mod Trig
    +mod Hypr
}
// math/trig.vks
mod Trig (sin, cos, tan) {
    func sin(){}
    func cos(){}
}

// math/hypr.vks 
mod Hypr (sinh, cosh){
func sinh(x:float){
    (super.exp(super.CONST_E,x) - super.exp(super.CONST_E, (-x)))/2
    }
}
```
```
math.vks            // top-level Math module
math/
├── trig.vks        // Trig submodule
└── hypr.vks        // Hypr submodule

```
- a module root is either <mod_name.vks> or mod.vks
- conventionally it's the <mod_name.vks>
- calling a module in another program
```
// app.vks 
/{ the top level of any vks file would be either app (with no name specifications ) where it generates and executeable and looks 
for the start function with Von return type. if it begins with a mod block it's a module}/
app Main {
use Math;
sqrt := "SQRT"
@start(){
    sqrt() // here this would throw an error because sqrt in this scope is a variable and not a function. but
    Math.sqrt(5) // this will not throw and error and execute Math.sqrt 
    // hence unless the name is specified with the correct module the nearest resolution is performed. but the following example is also valid.
}
}
// ts.vks
app Main{
    @using("Math.Trig") // this expplicitly only exposes what the Math.Trig modules exposes and not what the whole math modules does
    Hy := @using("Math.Hypr") // Modules can be aliased by being assigned to a name.
    @start (){
        sin(5) // this resolves to the sin defined in Math.Trig since there is not conflict with any of the local definitions or definitions in ay other imports.
    }

}
```
- Super only resolves to the parent module like rust. we're just using a different syntax
- module exports are defined in a tuple at the start of the module definition like in the example only the 
names (constants, types, functions, sub-modules) defined there are exported else remain accessible only inside the module and it's submodule.
- and super is not restricted to just one parent if i do `super.super.foo` looks for `foo` in the parent of the parent.
- Modules are Tree like each module can have multiple sub-modules but a submodule can't have multiple parents. 
- Names are resolved under scope in a tree mechanism and internally can be read as `app_Main.start` and `mod_Math.Trig.sin` or `mo_Math.sqrt` 
- Module and app can't have duplication that is 

```
// foo.vks
mod Math {}

//baa.vks
mod Math {}
```
and then `vks build foo.vks baa.vks` would throw error of module name duplication.
## Von data type
- Following the same single-binding rule nested keys are allowed.
`data Key`  Grammar rule `Key := identifier ( "." identifier )*`

`data Von =
    | VNull
    | VBool   Bool
    | VInt    Int
    | VFloat  Float
    | VString String
    | VArray  [Von]
    | VAttr   [(Key, Von)]
`


## I/O 
- For now there is no stdin-stdout directive.
- The input is in the source code and the output will be given to a file.
- The predule define a writeOutput procedure that writes to it and it takes a data Types know as Von.
- Von is similar to json and is language premitive shipped with Prelude. Von is a Map type data constructor with name accessible and appendable. 
- Make a Typeclass FromVon and ToVon that can have methods to convert Language types like struct or tuples or anyother to Von string.
```
app Main {
    @start(){
        v1 : Von

        result = 2+1;
        @output("output.txt")
        => Von {

        }
    }
}

```

## Project Management

- This program can stand as a single file `hello.vks` and compiled through `vks build hello.vks` but for larger projects we're gonna need a config file so.
- The config file is defined in the following format.
- when running `vks init .` in a empty dir initialize this project with this config file with a simple main app. 

```
// config.von
// von = Vks Object Notation and supports comments the same way as vks does.

{
    name = "my_app";            // name of the binary compiled should be the same as app name defined with app keyword in the source code.
    type=app // can be app | module
    version = "0.1.0";          // version of the app
    root = "main.vks"
    dependencies = [
        {
            name = "Some_package";
            version = "0.2.0";
            path = "";           // GitHub or any release URL where version will be appended
        }
    ]; // this remains blank for now package management and external name resolution would be done in future

    compilerFlags = [];          // list of compiler flags for build

// Leave this as a blank strings or even a blank object these scripts will be handled in the future.
    scripts = {
        build = "";              // script called by `vks build .` 
        run = "";                // script called by `vks run .`
    };
}
```
