---
layout: post
title:  "Pure AST based linting sucks"
date:   2020-09-18 11:00:00
categories: rust
---

First blog post! For this blog post i would like to talk about various issues around linters which exclusively use ASTs (Abstract Syntax Tree) for linting source code, and a tree representation you may not know about. Over many months of working on a JavaScript linter in rust called [rslint](https://github.com/RDambrosio016/RSLint/tree/dev) i have encountered numerous issues and have explored many concepts which i feel i should share to offer insights into why i believe purely AST based linting is unfit for complex linters.

This post mostly refers to the issues with pure typed AST based linting in statically typed languages such as rust, However, most of the issues occur in dynamically typed languages too.

I will be giving a small introduction to how linters work, feel free to skip to the [Problems with ASTs](#Problems-with-ASTs) if you already know how linters work.

# Table of contents

* TOC
{:toc}

# So how does a linter work?

Linters are programs which statically analyze code for semantic, stylistic, and potentially erroneous issues. Linters traditionally work by taking source code, then running a parser on the source code, which yields an abstract representation of the program called an abstract syntax tree. Popular examples of linters include ESLint for JavaScript, and Clippy for rust. Some linters further rely on typechecking information like clippy, however i will not be focusing on that aspect for this post.

Once an abstract syntax tree has been built, the linter crawls (visits) the tree's nodes (Items like an if statement, an if statement's condition, its body, etc) and runs specific "rules" on the node. An example of a rule may be ESLint's `no-empty`, which visits block statements and checks if they contain no nodes and no comments. 

Linters often offer autofix too, where the linter changes the source code to fix the issue for you. Usually this relies on text transformations, but many linters use AST transformations too.

# Problems with ASTs

Purely typed ASTs may appear to be the logical choice best choice when making a linter, however, ASTs have many inherent flaws which can make writing stylistic or more complex rules extremely inefficient or difficult. The main issues with purely typed ASTs include the following: 

- Often expensive to clone and pass around 
- Complex Node traversal is impossible without runtime typing, and even with runtime typing it is extremely constrained. 
- No straightforward way to obtain individual tokens or child nodes
- Lossy, no way to cleanly and efficiently represent whitespace inside of them, which is crucial for stylistic linting.
- Require explicit handling of their structures in the parser
- Mutable, which is fine for most cases but you really do not want mutable trees for static analysis in a language without a GC. 
- Makes error recovery for a parser very constrained 
- Incremental reparsing is impossible 

Let's go through each of the issues one by one:

## Expensive to clone 

AST nodes usually hold a decently large amount of information, this means if you want to pass around a node to an individual sub-analyzer (say, scope analysis) you must either pass references (would likely get you into lifetime hell very quickly), or clone the structure. Cloning the structure can often be relatively expensive due to the amount of information it holds, lets take a look at the [swc](https://github.com/swc-project/swc) type definition for a Js/Ts class:

```rust
/* Irrelevant serde attributes omitted */
pub struct Class {
    pub span: Span,
    pub decorators: Vec<Decorator>,
    pub body: Vec<ClassMember>,
    pub super_class: Option<Box<Expr>>,
    pub is_abstract: bool,
    pub type_params: Option<TsTypeParamDecl>,
    pub super_type_params: Option<TsTypeParamInstantiation>,
    pub implements: Vec<TsExprWithTypeArgs>,
}
```

Now let's say you made a scope analyzer, and you would like to be able to get the declared classes inside of a scope. You would have to expensively clone the class, or store the span, which leaves out info you may want such as the class' body. This often constrains the info a sub-analyzer can hold, and subsequently constrains complex error rendering. 

## Complex node traversal is impossible

It is very common for a rule to want to know about the node's parent/sibling/children/descendants. However, that is impossible without runtime typing in a statically typed language. Why exactly? well, AST nodes are standalone, they do not know about parents or siblings, they can only traverse top-down using explicit visitors. This brings about a couple of issues:

### Need for visitors

ASTs are typed, which means to visit each node you need to explicitly handle every single node and visit each of their children explicitly. Which means you must either use a complex proc macro to generate a visit trait for you, or you need to implement it yourself, which is a gigantic pain and is impractical. 

Moreover, Visit traits work by offering the visit functions as provided trait methods, but this means when you override them to check items you end up not visiting child nodes! This means you need must visit each child manually, which results in a ton of boilerplate code. A real world example of why this is a problem is [deno_lint](https://github.com/denoland/deno_lint), deno lint is incapable of catching `no-unsafe-finally` for this code: 

```js
try {
  /* */
} finally {
  try {
    /* */
  } finally {
    throw "oh no";
  }
}
```

This is because deno_lint's `no-unsafe-finally` rule overrides the try statement visit method:
```rust
fn visit_try_stmt(&mut self, try_stmt: &TryStmt, _parent: &dyn Node) { /* */ }
```
And does not recursively visit the body of the try statement, the catch body, or the finalizer statements.

To fix this major issue in many of deno_lint's rules it would have to change every single visit function on every rule to include recursively visiting the children manually.

And finally, requiring a visitor introduces a big amount of boilerplate into each rule definition, let's use deno_lint as an example again:

```rust
impl LintRule for NoUnsafeFinally {
  fn new() -> Box<Self> {
    Box::new(NoUnsafeFinally)
  }

  fn code(&self) -> &'static str {
    "no-unsafe-finally"
  }

  fn lint_module(&self, context: Arc<Context>, module: &Module) {
    let mut visitor = NoUnsafeFinallyVisitor::new(context);
    visitor.visit_module(module, module);
  }
}

struct NoUnsafeFinallyVisitor {
  context: Arc<Context>,
}

impl NoUnsafeFinallyVisitor {
  fn new(context: Arc<Context>) -> Self {
    Self { context }
  }
}

impl Visit for NoUnsafeFinallyVisitor {
  noop_visit_type!();

  fn visit_try_stmt(&mut self, try_stmt: &TryStmt, _parent: &dyn Node) {
    /* rule checks omitted*/
  }
}
```

Compared to rslint's implementation (which is able to catch nested errors) which uses bottom-up node checking:

```rust
declare_lint! {
    /* documentation omitted */
    #[derive(Default)]
    NoUnsafeFinally,
    errors,
    "no-unsafe-finally"
}

pub const CONTROL_FLOW_STMT: [SyntaxKind; 4] = [BREAK_STMT, CONTINUE_STMT, THROW_STMT, RETURN_STMT];

#[typetag::serde]
impl CstRule for NoUnsafeFinally {
    fn check_node(&self, node: &SyntaxNode, ctx: &mut RuleCtx) -> Option<()> {
        if CONTROL_FLOW_STMT.contains(&node.kind()) && node.parent()?.parent()?.is::<ast::Finalizer>() {
            self.output(node, ctx);
        }
        None
    }
}

/* self.output error rendering */
```

This way of using a visitor means each rule is actually made up of two structs, and the second struct needs to store an `Arc<Context>` to be able
to add diagnostics. This further introduces more issues i won't get deep into but they include requiring the diagnostics vector in the context to be locked (through a mutex) to add a diagnostic, and not being able to cleanly refer to each rule's diagnostics/outcome.

### Bottom-up rule checking

Bottom-up rule checking is impossible, which many rules benefit greatly from, such as [`no-await-in-loop`](https://eslint.org/docs/rules/no-await-in-loop).
This is because a node cannot know about its parent, swc's visitor attempts to slightly get around this with a `_parent: &dyn Node` on ever visitor function which is a simple wrapper on top of [`Any`](https://doc.rust-lang.org/std/any/trait.Any.html). This still does not allow for recursive bottom-up checking, and it introduces the mess of needing to downcast the parent. 

The fact that bottom-up rule checking is impossible means every rule must be a bottom-down check, which is usually ugly for the following reasons:

- Oftentimes a rule wants to check a node, but only if its the child of a specific node, which requires explicit handling of each possible parent (see: `no-extra-boolean-cast`)
- It results in duplicate code for each parent 

Proof of this can be found in deno_lint's [implementation](https://github.com/denoland/deno_lint/blob/master/src/rules/no_await_in_loop.rs) of `no-await-in-loop`, which is 182 lines, versus rslint's [implementation](https://github.com/RDambrosio016/RSLint/blob/dev/rslint_core/src/groups/errors/no_await_in_loop.rs) which is 35 lines.

## No easy access to tokens or child nodes 

ASTs are lossy, like the name implies, this means that it is impossible to straight from a node to its tokens. It may seem like a small issue but in reality it poses a ton of issues, these include, but are not limited to:

- No lexical equality (counting `foo. bar` and `foo.bar` as the same expression) 
- No simple implementation of individual token based rules
- No easy syntax highlighting without reinvoking the lexer 
- No easy way to refer to an individual token for pretty errors 

To get around this, tools like ESLint store the tokens in a store then have methods on a context object to get the tokens. As you can probably guess, this brings about more issues: 

- No interning, which can make storing tokens expensive for things like `let`, `var`, etc. 
- Another thing to implement and maintain.
- Tokens don't know about their parent AST nodes .
- cont: Tokens cannot traverse their siblings, next tokens, prev tokens, etc.
- No whitespace and comment tokens, which results in ugly handling of comments and comment directives.

## Lossy

ASTs are lossy, this makes stylistic linting pretty challenging, and it often involves digging back into the source code to get whitespace. Not only is this ugly, it is inefficient and impractical. With a lossless syntax tree we can simply crawl the tree and search for whitespace tokens (which include all whitespace). As mentioned above, tokens also can crawl tokens before them, and even their parent. We can then refer back to these tokens in pretty errors to make fixing errors even easier for the user.

## Require explicit handling in the parser

This is a mostly minor thing, but needing to worry about the constructed nodes in the parser adds more boilerplate to the parser.
As a practical example, lets take swc's [implementation](https://github.com/swc-project/swc/blob/master/ecmascript/parser/src/parser/expr.rs) of array literal parsing:

```rust
fn parse_array_lit(&mut self) -> PResult<Box<Expr>> {
    trace_cur!(parse_array_lit);

    let start = cur_pos!();

    assert_and_bump!('[');
    let mut elems = vec![];

    while !eof!() && !is!(']') {
        if is!(',') {
            expect!(',');
            elems.push(None);
            continue;
        }
        elems.push(
            self.include_in_expr(true)
                .parse_expr_or_spread()
                .map(Some)?,
        );
        if is!(',') {
            expect!(',');
        }
    }

    expect!(']');

    let span = span!(start);
    Ok(Box::new(Expr::Array(ArrayLit { span, elems })))
}
```

And compare it with the rslint parser's [implementation](https://github.com/RDambrosio016/RSLint/blob/dev/rslint_parser/src/syntax/expr.rs):

```rust
pub fn array_expr(p: &mut Parser) -> CompletedMarker {
    let m = p.start();
    p.expect(T!['[']);

    while !p.at(EOF) && !p.at(T![']']) {
        if p.eat(T![,]) {
            continue;
        }
        if p.at(T![...]) {
            spread_element(p);
        } else {
            assign_expr(p);
        }
        if !p.at(T![']']) {
            p.expect(T![,]);
        }
    }

    p.expect(T![']']);
    m.complete(p, ARRAY_EXPR)
}
```

The parser does not have to worry about the final AST node, it simply calls functions which produce events in the parser, these events are then processed into a syntax tree.

## Mutable 

ASTs are usually mutable, this is great for tools like transpilers, but it is not fit for linters, which are for the most part static. This ties back to [Expensive to clone](##Expensive-to-clone), you can't just give out nodes without either cloning or taking a reference to them. 

However, i should add, mutable ASTs are great for autofix, however, autofix is a relatively small part of linting, and it is not worth it to make rule checking a mess so autofix can work great. Additionally, autofix with immutable trees is not impossible, it is just slightly harder than using mutable trees. 

## Next to no error recovery

Error recovery refers to a parser being able to take in incorrect source code, and still parse a mostly correct AST out of it. Most linters do not attempt this at all, for example, espree, and swc_ecmascript (ESLint and deno_lint's parsers respectively) make no attempt at recovery. When the parsers encounter an error they return an Err result and quit parsing, producing no AST. 

Why is that? well, you can't just make an AST node field if you have no data (duh). If the parser expected an expression which is a field in the AST node, then what are we going to put for the struct?, simple, we dont make the struct and stop parsing :)

This means it is impossible for the linters to lint wrong code, which is an amazing feature for on-the-fly linting in things such as IDEs. Not having AST fields to worry about allows rslint to parse this invalid code: 

```js
if true {
  /* */
} else {
  /* */
}
```

And lint it successfully:

```
error[SyntaxError]: Expected token `L_PAREN` but instead found `TRUE_KW`
  ┌─ tests\main.js:1:4
  │
1 │ if true {
  │    ^^^^ Unexpected

error[SyntaxError]: Expected token `R_PAREN` but instead found `L_CURLY`
  ┌─ tests\main.js:1:9
  │
1 │ if true {
  │         ^ Unexpected

error[no-constant-condition]: Unexpected constant condition
  ┌─ tests\main.js:1:4
  │  
1 │   if true {
  │      ^^^^ this condition is always truthy...
2 │     /* */
3 │   } else {
  │ ┌────────'
4 │ │   /* */
5 │ │ }
  │ └─' ...which makes this unreachable
```

RSLint's parser can produce a syntax tree from any source code, because the final output's nodes are not required to have any data.

Note however that rslint_parser is still in early development, and error recovery can be buggy and explosive at times, this will get better in the future however. 

# Alternatives to the standard AST

RSLint uses a library called [rowan](https://docs.rs/rowan/0.10.0/rowan/index.html), which was developed for the [rust analyzer](https://github.com/rust-analyzer/rust-analyzer) project. Rowan models trees as immutable syntax trees called Green trees, which are composed of GreenNodes. 

Green nodes are dynamically sized types which use a single allocation for the header and children as follows:

```
*-----------+------+----------+------------+--------+--------+-----+--------*
| ref_count | kind | text_len | n_children | child1 | child2 | ... | childn |
*-----------+------+----------+------------+--------+--------+-----+--------*
```

The trees are fully immutable, which allows the tree to use interning, therefore the tree is allowed to structurally share subtrees. This interning helps with memory usage a fair amount. Moreover, rowan uses `TextSize` and `TextRange` for storing text length, as opposed to usizes, this is fine because linting/parsing 4gb+ JavaScript files is not something rslint will ever support. Furthermore, tokens use `SmolStr`, which is a structure optimized for small strings.

GreenNodes are not used manually, rowan provides structures called `SyntaxNode` and `SyntaxToken`. These structures are simply an Rc around some node data which refers back to a non null pointer to the tree. Then AST nodes are modeled as structs which implement `AstNode`, each struct has functions which try to find some data inside of the children of the ast node's untyped `SyntaxNode`. Every single item on every ast node is optional, this is to allow for 100% error recovery. The conversion between AstNode and untyped SyntaxNode is free.

You might be asking why this is a game changer, well, we now have 4 layers to each node ranging from high level to low level, and we can access each one of them freely with little cost!

Let's take a look at what this layout looks like for If statements:

AstNode:

```rust
pub struct IfStmt {
    pub(crate) syntax: SyntaxNode,
}
impl IfStmt {
    pub fn if_token(&self) -> Option<SyntaxToken> { /* */ }
    pub fn condition(&self) -> Option<Condition> { /* */ }
    pub fn cons(&self) -> Option<Stmt> { /* */ }
    pub fn else_token(&self) -> Option<SyntaxToken> { /* */ }
    pub fn alt(&self) -> Option<Stmt> { /* */ }
}
```

SyntaxNode:

```js
IF_STMT@0..38
  IF_KW@0..2 "if"
  WHITESPACE@2..3 " "
  CONDITION@3..9
    L_PAREN@3..4 "("
    LITERAL@4..8
      TRUE_KW@4..8 "true"
    R_PAREN@8..9 ")"
  WHITESPACE@9..10 " "
  BLOCK_STMT@10..21
    L_CURLY@10..11 "{"
    WHITESPACE@11..14 "\n  "
    COMMENT@14..19 "/* */"
    WHITESPACE@19..20 "\n"
    R_CURLY@20..21 "}"
  WHITESPACE@21..22 " "
  ELSE_KW@22..26 "else"
  WHITESPACE@26..27 " "
  BLOCK_STMT@27..38
    L_CURLY@27..28 "{"
    WHITESPACE@28..31 "\n  "
    COMMENT@31..36 "/* */"
    WHITESPACE@36..37 "\n"
    R_CURLY@37..38 "}"
```

Tokens:

```js
[
    IF_KW@0..2 "if",
    WHITESPACE@2..3 " ",
    L_PAREN@3..4 "(",
    TRUE_KW@4..8 "true",
    R_PAREN@8..9 ")",
    WHITESPACE@9..10 " ",
    L_CURLY@10..11 "{",
    WHITESPACE@11..14 "\n  ",
    COMMENT@14..19 "/* */",
    WHITESPACE@19..20 "\n",
    R_CURLY@20..21 "}",
    WHITESPACE@21..22 " ",
    ELSE_KW@22..26 "else",
    WHITESPACE@26..27 " ",
    L_CURLY@27..28 "{",
    WHITESPACE@28..31 "\n  ",
    COMMENT@31..36 "/* */",
    WHITESPACE@36..37 "\n",
    R_CURLY@37..38 "}",
]
```

Text:

```js
if (true) {
  /* */
} else {
  /* */
}
```

What does this `Typed <-> Untyped <-> Tokens <-> Source code` layout allow us to do? a lot. This is an incomplete list of the things this layout allows:

- Parent/Ancestors/Descendants/Children/Siblings traversing 
- Bottom-up rule checking
- Extremely cheap node/token cloning
- Complete error recovery
- Totally lossless syntax tree 
- Free typed to untyped conversion with no runtime typing 
- Lexical equality 
- Cheap syntax highlighting 
- No need for messy visitors 
- Incrementally reparsing source code 
- Sharing nodes across sub-analyzers (such as scope analysis) and sub-parsers (directive parsing)
- Ability to catch nested errors without having to manually visit nodes 
- Ability to check individual tokens in the tree easily and cheaply 
- Parallelized node checking 
- Linting of syntactically incorrect source code 
- Syntactically complex error rendering (e.g. labelling individual tokens) 
- Ability to leverage untyped nodes for rule checking, which makes writing rules more concise 
- Ability to write very complex rules relying on multiple distinct nodes 
- Requires no handling of whitespace in the parser itself 
- Allows for lossy and lossless parsing 

## Downsides 

This layout however has a few, mostly minor downsides: 

- Ast node fields are functions, not fields. 
- (Particular to rslint) all ast node fields are optional
- Requires a lot of setup compared to struct ast nodes (see xtask/codegen)
- Autofix and ast mutation in general is harder 

# Acknowledgements 

Huge thanks to [matklad](https://github.com/matklad), he is the original author of rowan and rust analyzer. Much of the code rslint's parser uses is taken from rust analyzer's parser and codegen, he has also answered way too many of my dumb questions on zulip :)

# Further reading 

- [A more detailed explanation of rowan's syntax implementation](https://github.com/rust-analyzer/rust-analyzer/blob/master/docs/dev/syntax.md)

- [RSLint's dev docs on the syntax topic](https://github.com/RDambrosio016/RSLint/blob/dev/docs/dev/syntax.md)

- [rslint_parser](https://github.com/RDambrosio016/RSLint/tree/dev/rslint_parser/src)

If you found this blog post interesting, i suggest you check out [rslint](https://github.com/RDambrosio016/RSLint/tree/dev), and perhaps contribute? 👀
