We here at Gimbling Science take cursedness very seriously. I’m Gim! I own the place. This is my first article on this (unchanged until now) website as part of The Writing Gaggle in the Rust Community Discord Server. I’m generally pretty excited.

> Cat: And so am I!

That eager voice you heard is the lovely Cat, my assistant. Rest assured, she won’t interrupt you too much. Isn’t that _right_, Cat?

> Cat: I’ll try (meow)

She’s the backbone of this corporation. Cute as a cat (duh), too. Sorry, fellas. She’s married. To cursed!

# Right, where were we?

Let’s hold our meows together, folks, and get our weapons together. My weapon of choice is [F#](https://fsharp.org/).

For the sake of simplicity, this article will only focus on basic _math_. Emphasis on _basic_.

> Cat: Did you just~

Shhh.

# # What’s the fuzz, Gim?

Now we all know the good ol’ Pratt Parsing. Some people even prefer recursive descent. But nothing is as cursed as _The FORTRAN way._

Let’s dive into [Wikipedia…](https://en.wikipedia.org/wiki/Operator-precedence_parser)

> Another approach is to first fully parenthesize the expression, inserting a number of parentheses around each operator, such that they lead to the correct precedence even when parsed with a linear, left-to-right parser. This algorithm was used in the early [FORTRAN I](https://web.archive.org/web/20230517120658/https://en.wikipedia.org/wiki/Fortran#FORTRAN) compiler: [[7]](https://en.wikipedia.org/wiki/Operator-precedence_parser#cite_note-Padua2000-7)

> The Fortran I compiler would expand each operator with a sequence of parentheses. In a simplified form of the algorithm, it would
> 
> - replace `+` and `–` with `))+((` and `))-((`, respectively;
> - replace `*` and `/` with `)*(` and `)/(`, respectively;
> - add `((` at the beginning of each expression and after each left parenthesis in the original expression; and
> - add `))` at the end of the expression and before each right parenthesis in the original expression.
> 
> Although not obvious, the algorithm was correct, and, in the words of [Knuth](https://web.archive.org/web/20230517120658/https://en.wikipedia.org/wiki/Donald_Knuth), “The resulting formula is properly parenthesized, believe it or not.” [[8]](https://web.archive.org/web/20230517120658/https://en.wikipedia.org/wiki/Operator-precedence_parser#cite_note-Knuth1962-8)

Look, I said cursed _for a reason._ And in the words of Knuth, it _works_. So don’t touch it.

> Cat: I don’t think Knuth said that, Gim…

# The Lexer

That means all we have is parenthesis for scoping, integers for integers, and operators for operating.

```fsharp
let rec private lex_ (source: char ReadOnlySpan) (index: int) (collector: Token array) =  
	if index = source.Length then
		collector
	else
		let character = source[index]
  
		match character with  
		| ' ' -> lex_ source (index + 1) collector  
		| '+' -> lex_ source (index + 1) (Array.append collector [| RParen; RParen; Add; LParen; LParen |])  
		| '-' -> lex_ source (index + 1) (Array.append collector [| RParen; RParen; Sub; LParen; LParen |])  
		| '/' -> lex_ source (index + 1) (Array.append collector [| RParen; Div; LParen |])  
		| '*' -> lex_ source (index + 1) (Array.append collector [| RParen; Mul; LParen |])  
		| '(' -> lex_ source (index + 1) (Array.append collector [| LParen; LParen; LParen |])  
		| ')' -> lex_ source (index + 1) (Array.append collector [| RParen; RParen; RParen |])  
		| char when Char.IsDigit char ->  
			let index_till_int = (consumeWhile source (Char.IsDigit >> not) index)  
			let int = double (source.Slice(index, index_till_int - index).ToString())  
			lex_ source index_till_int (Array.append collector [| Integer int |])  
		| char -> failwith $"Unknown token {char}"  
  
let lex (source: char ReadOnlySpan) = lex_ source 0 [||]
```

Let’s go step-by-step, understanding what’s going on here. Now I’m a _Span/ReadOnlySpan shill_. Functional programming doesn’t have to be slow! I blame the linked lists! The govermen~

> Cat: Sir, the lexer.

Right. A brief overview of what’s going on here is this. Our main lexer is in a private function `lex_`. I’ll explain later why this needs to be a thing.

We take in a parameter, containing the original source code. Strings in .NET land are arrays of _chars._ They even have an implicit conversion operator to `ReadOnlySpan<char>`. The second being the _index_ we’re currently at. And the third being a collector that we append our tokens into.

We get our current character by indexing the span with the current index, and we iterate by recursing with the index bumped up. This is _fast!_ As fast as traditional looping. Although, this is talks for later.

If the current index is same as the length of the original source, we can conclude that we have reached the end of the source and we can stop lexing further. Now it’s pretty simple, our grammar isn’t really that complex. In a real compiler, I would use a lexer generator like [Logos](https://web.archive.org/web/20230517120658/https://lib.rs/logos) instead however.

We match over the character, ignoring the whitespace entirely and just bumping the index. Then matching over the different operators and…

> Cat: WTF?!

Cat, did you not read the script? Just scroll up a bit, you lazy bastard.

At the lexer stage, we transform the operators into what the algorithm defines.

> - replace `+` and `–` with `))+((` and `))-((`, respectively;
> - replace `*` and `/` with `)*(` and `)/(`, respectively;

Same with the parenthesis.

> - add `((` at the beginning of each expression and after **each left parenthesis in the original expression**; and
> - add `))` at the end of the expression and before **each right parenthesis in the original expression**.

To lex integers, we increment the current index till `source[index]` is a _digit_. Then we fetch a sub-span from the original span, convert it into a string, and parse it as a **`double`**.

> Cat: Sir, you remember that time you threatened people to blow them up with combustible floats?

Cat, this is desparate times.

That’s our entire lexer! That’s all we have. Fear not, war hero, we will implement this same algorithm in the future in a _real_ language with complex expressions and not just integers. We don’t even have unops yet.

# The Parser

Our parser needs to parse into a some structure. Our grammar isn’t particularly complex enough to have a proper AST. However, we can represent an expression using a structure like this.

```fsharp
[<RequireQualifiedAccess>]  
type Expr =
    | Add of Expr * Expr  
    | Sub of Expr * Expr  
    | Mul of Expr * Expr  
    | Div of Expr * Expr  
    | Paren of Expr  
    | Integer of double  
  
    override self.ToString() =  
        match self with  
            | Add(expr, expr1) -> $"{expr}+{expr1}"  
            | Sub(expr, expr1) -> $"{expr}-{expr1}"  
            | Mul(expr, expr1) -> $"{expr}*{expr1}"  
            | Div(expr, expr1) -> $"{expr}/{expr1}"  
            | Paren expr -> $"({expr})"  
            | Integer i -> $"{i}"
```

We even have a cute little ToString method so we can sanely print our expressions for debugging (Who knows?)

```fsharp
let rec parse_ (tokens: Token list) =  
	let mutable lhs, lhs_xs =  
		match tokens with  
		| [] -> None, []  
		| Integer i :: xs -> Some(Expr.Integer i), xs  
		| LParen :: RParen :: xs -> None, xs  
		| LParen :: xs ->  
			let expr, xs = parse_ xs  
  
			let xs =  
				match xs with  
				| RParen :: xs -> xs  
				| x :: _ -> failwith $"Expected to consume ), found {x}"  
				| [] -> failwith "Expected to consume ), found EOF"  
  
			Option.map Expr.Paren expr, xs  
		| x :: _ -> failwith $"Wanted to start parsing an expression, found {x}"  
  
	let bind (fn: 'a -> 'a Option * 'c) (option: 'a option) (xs: 'c) =  
		match option with  
		| Some x -> fn x  
		| None -> None, xs  
  
	bind  
		(fun lhs ->  
			match lhs_xs with  
			| Add :: xs ->  
				let rhs, xs = parse_ xs  
				(Option.map (fun rhs -> Expr.Add(lhs, rhs)) rhs), xs  
			| Sub :: xs ->  
				let rhs, xs = parse_ xs  
				(Option.map (fun rhs -> Expr.Sub(lhs, rhs)) rhs), xs  
			| Mul :: xs ->  
				let rhs, xs = parse_ xs  
				(Option.map (fun rhs -> Expr.Mul(lhs, rhs)) rhs), xs  
			| Div :: xs ->  
				let rhs, xs = parse_ xs  
				(Option.map (fun rhs -> Expr.Div(lhs, rhs)) rhs), xs  
			| [] -> Some lhs, []  
			| RParen :: _ as xss -> Some lhs, xss  
			| otherwise -> failwith $"Expected to consume either expression end or an operator, found {otherwise}")  
	lhs  
	lhs_xs
```

If you can understand F#, or ML, or any ML-like language, try having a go for yourself. (Definitely not making my job here easier.)

> Cat: _whistles_ He always was a lazy bu~

Pick your next words very wisely, Cat.

Fear not, our parser is separate into _two_ parts.

# The LHS

First, we match over an integer token, if it’s matched then, then we form an integer expression. Then we match over paranthesis, no fancy business here really, we just iterate over the LParen `(` and then parse an expression, ending with _consuming_ an RParen `)`. Consuming being, that we _expect_ a specific token to be there, and bailing if the token isn’t there.

Note how we didn’t really care about the operators…till now.

# The RHS

Now we look if our LHS expression is followed with an operator, i.e Add/Mul/Sub/Div. Then, we parse _another_ expression and we form the appropriate expression. If the operator was Add, then an addition expression, and so on.

# What the F#$%?

If you’re coming from Pratt Parsing, this will be Really odd for you. This is very similar, yet so far. We don’t care about the precedence magic in our parser **At All.**

But we might be forgetting something here…

> - add `((` at the **beginning of each expression** and after each left parenthesis in the original expression; and
> - add `))` at the **end of the expression** and before each right parenthesis in the original expression.

Ahaha!~ Let’s get at it. Careful readers might’ve noticed that similar to our lexer pattern before, our parser function was named `parse_`.

So, let’s a make a wrapper that implements these additions.

```fsharp
let parse tokens =  
	fst (parse_ ([ LParen; LParen ] @ tokens @ [ RParen; RParen ]))
```

There we go.

# The Interpreter

> Cat: Sir, It’s 22:42

Sleep is for the weak, Cat.

```fsharp
let rec interpret (expr: Expr) =  
	match expr with  
		| Expr.Add(expr, expr1) -> (interpret expr) + (interpret expr1)  
		| Expr.Sub(expr, expr1) -> (interpret expr) - (interpret expr1)  
		| Expr.Mul(expr, expr1) -> (interpret expr) * (interpret expr1)  
		| Expr.Div(expr, expr1) -> (interpret expr) / (interpret expr1)  
		| Expr.Paren expr -> interpret expr  
		| Expr.Integer i -> i
```

Look, do I _need_ to explain this? _Something something, is left to the reader as an exercise._

# The Final Result

So, Olympian/War Hero/Homeless person, did you have fun on this wild journey from the past? (Say yes, you. I still have a dozen of those combustible floats with me.)

Old algorithms are a blast, and this is no exception. It _works_ and it works magically well. And, it’s pretty non-obvious on _why_. But it clicks after a hot second of “WTF?”, it simulates precedence using parenthesis depth.

Now, I’m going to hit the sack. _Zzz….._

> Cat: He’s gone, you should too! He sleep-walks with them combustible floats at hand. Be free! You don’t _want_ to be in his cursed cave again!

Cat, I’m still awake.

> Cat: Fuck.

---