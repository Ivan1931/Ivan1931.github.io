---
title: Functional cellular automatons
layout: default
---
# Intro
In this post I'm going to show you how to specify a monad for creating cellular automatons on 2D grid. 
Using this monad we will derive some methods for combining cellular automatons together to create
more complex cellular automaton from simple building blocks - in other words providing a means of
abstraction and combination.

This post is targeted towards people who are more familiar with Haskell. This is **not** a monad tutorial but rather a demonstration of a different
monad from ones you may have seen before ;)
If you have never seen Haskell before, that's okay. Check out [learnyouahaskell](http://learnyouahaskell.com/) for the canonical tutorial book.

# Problem
A cellular automaton consists of a set of cells where each cell carries some state. 
For example a number or an "on/off" state.
The cells are arranged in a grid of multiple dimensions. 


A group of rules are defined that map cell one state to another state. These mappings are affected by
the cells location in a grid and the state of other cells as well as it's current state. 
Commonly the neighbouring cells that we are interested in are all those with a distance of 1 from the cell we're currently looking at. 
This is known as a [Moore neighbourhood](https://en.wikipedia.org/wiki/Moore_neighborhood).
Successively applying rules to every cell on a grid produces a sequence of grids which usually have some interesting properties
or behaviors when considered as a system.

The most popular example of a cellular automaton is the Game of Life which is covered in pretty much every 
CS101 course. Other examples of cellular automatons include 
[Brians brain](https://en.wikipedia.org/wiki/Brian's_Brain), 
[Langton's](https://en.wikipedia.org/wiki/Langton's_ant), 
[Wire world](https://en.wikipedia.org/wiki/Wireworld) and
[Codds CA](https://en.wikipedia.org/wiki/Codd's_cellular_automaton).

In this post I'll be focussing on automatons with 2D grids and transition functions that operate on Moore neighbourhoods.

# Let's get too it
The first thing we should do is decide on how to represent our grid.

Let's define our types:

```haskell
type Coord  = (Int, Int)
type Grid c = Map Coord c
```

In the first line we define our coordinate system (`Coord`) - a 2D Euclidean Space.
In the second line we define the concept of a grid as a mapping between `Coord` and some type `c`.
The type `c` is an arbitrary type describes the possible states of cells that can exist in our automaton.

Now that we have represented the state of grids and cells, let's represent the transition rule. 

```haskell
newtype Rule c a = Rule { rule :: Grid c -> Coord -> a }
```

This definition is pretty much the crux of this post. We choose to represent a rule as a function that
maps a grid with a cell type `c` and a coordinate (`Coord`) to any other type `a`.

In other words, our function can make use of information from the entire grid and also a specific grid coordinate
to produce any value. We could consider the `Coord` argument in our `rule` function as a directive to view a specific cell on the grid.

Any function with the same signature as `rule` can converted to a  `Rule` using the `Rule` type constructor. 
We will see why this is useful later.

# Defining type classes
![Rules are monads]({{site.url}}/assets/images/wiitu.jpg)

`Rule` is a monad.

Why does that matter and what can we do with that information? 

Before we create instances of our `Rule` type for [Monad](https://wiki.haskell.org/Typeclassopedia#Monad) 
let's get our feet wet by defining [Applicative](https://wiki.haskell.org/Typeclassopedia#Applicative) 
and [Functor](https://wiki.haskell.org/Typeclassopedia#Functor) instances.

## Functor
I like to view functors as tuppaware boxes with labels on them. They provide a
container with some contextual information on the contained type. For example: "this tuppaware can contain
bacon" is conceptually similar to "this HTTP Request can contain unsanitized inputs". `fmap` is a function that
is aware of the functors context when it acts on the contained type. 

Here it is:

```haskell
instance Functor (Rule c) where
    fmap f (Rule r) = 
        Rule (\ grid xy -> f $ r grid xy)
```


Here we are wrapping the result of applying `r` (our transition function) to it's arguments in another function
called `f`. Our functor is pretty cool, since we can now transform the output of our rule into something else.
`c` is the type of cells in the automaton.


## Applicative
Here is applicative instantiation for our `Rule` structure.

```haskell
instance Applicative (Rule c) where
    pure a = Rule (\_ _ -> a)
    Rule f <*> Rule r =
        Rule (\ grid xy -> (f grid xy) $ r grid xy)
```

We use our `Applicative` instance to specify how to apply a function that is to be evaluated in the context of our `Rule` 
to some object that is also in that context. To do this we apply the `Rule` function `f` in it's "computational context".
In other words we apply it to the `Grid` and `Coord` arguments. This returns a function which we can then apply to the result of applying `r`
in context. This is essentially the generalised idea of function application in a "computational context", in other words "applicative".

## Monad
Here comes a mouth full of monad!

```haskell
instance Monad (Rule c) where
    return = pure
    Rule r >>= f = Rule $ \ grid xy ->
        let
            a = r grid xy
            Rule f' = f a
        in
            f' grid xy
```

A monad can be thought of as function composition in a computational context. In our implementation of bind, we evaluate the left argument in it's context -
the grid and the coord we are viewing to produce `a`. We then call `f` on `a` to create our new Monad. We are essentially performing function composition
in a monadic context :) Our monad essentially evaluates the rule (producing `a`), takes it's result and feeds it into a another function that creates
a new rule in the same monadic context (`Coord` and state of the `Grid c`).

Let's define a function called `runRule` which accepts a `Rule` and a `Grid`, applies the rule and creates a new grid based on the transition function
and the coordinates.

```haskell
import qualified Data.Map as Map

runRule :: Rule c a -> Grid c -> Grid a
runRule (Rule r) grid = Map.mapWithKey f grid
    where f xy _ = r grid xy
```

## Interacting with Collective State
Now that we have a definition for our monad, let's define some helpful methods to help us view information in other cells. In other words
we need a way of interfacing with the collective state of our the grid.

### Refering to other cells
Cellular automatons are not very interesting if they have no mechanism to view the state of their neighbours.
To accomplish this task, let's define a `Rule` that will refer to a different cell from the one we are currently focussing on.
If the Cell does not exist for some reason (cell not being known to the grid) we make use of the `Default` type class from the
[Default](https://hackage.haskell.org/package/data-default) library to provide a default value for our Cell

```haskell
import Data.Default

relativeFrom :: (Default c) => Coord -> Rule c c
relativeFrom (x, y) =
    Rule (\ grid (x', y') -> (x' + x, y' + y) `getOrZero` grid)
    where getOrZero = Map.findWithDefault def
```

`relativeFrom` also has a type of `Rule c c` because it is a rule that maps a cell on a grid to another cell on the same grid. Hence it's
type is the same as the grid type.

Now that we have the `relativeFrom` function it is straightforward to define rules that refer to the current function being viewed and the cells in its
Moore_neighborhood. This is accomplished using the `self` and `neighbours` rules.

```haskell
self :: Rule c c
self = relativeFrom (0, 0)

neighbours :: Rule c [c]
neighbours = mapM relativeFrom relativePositions
    where
    relativePositions = [(x, y) | x <- [-1,0,1], 
                                  y <- [-1,0,1], 
                                  (x, y) /= (0,0)]
```

We might also want to count how many cells in our neighbourhood exhibit a property that we can represent using a predicate. This function
can be defined as follows:

```haskell
countAround :: (c -> Bool) -> Rule c Int
countAround predicate = 
    let
        count = length . filter predicate
    in
        fmap count neighbours
```

We make use of the fact that our `Rule` - like all monads - is also a functor.
We define `count` which finds how many items satisfy the predicate in our list.
`fmap` then maps that function into the neighbours `Rule` producing a final `Rule` 
that does what it says on the (rule name | tin).

## Game of Life
At last we have enough basic primitives to define something useful. Bellow is how we define a `Rule` for Conways Game of Life!

### Rules
Here is a refresher of the rules of the game of life

* If a cell is `Alive`
    * If precisely two or three neighbours are alive then stay alive
    * Otherwise the cell dies from under or over population
* If a cell is `Dead`
    * If precisely three of it's neighbours are `Alive` then come alive
    * Otherwise stay `Dead`!

### Data
First we should define what a Cell it in the context of the G.O.L.

```haskell
data Cell = Alive | Empty
          deriving (Eq, Show, Read, Enum)

instance Default Cell where
    def = Empty
```

Notice that we also define the default value of our Cell.

### Rule
Finally we can define the Game Of Life cellular automaton using the `Rule` monad!

```haskell
gameOfLife :: Rule Cell Cell
gameOfLife = do
    s <- self
    liveCells <- countAround (==Alive)
    if s == Alive then
        case liveCells of
            2 -> return Alive
            3 -> return Alive
            _ -> return def
    else
        case liveCells of
            3 -> return Alive
            _ -> return def
```

Awesome! Haskell is really cool because it allows us to embed arbitrary state (or "computational context" if you like) in syntax using "do-notation".
But that's not all! As we can see above, there are ways and means of combining our automaton specifications.

Here is an example of a Game of Life automaton generated using the `gameOfLife` monad.

![Game of Life]({{site.url}}/assets/images/gol.gif)

# Composition of Automatons!
Now it's time for composition. For the rest of the post we will see how to compose a Game of Life with Wireworld and another Automaton.

## Grid Type
Intuitively, for two types of automatons to interact, their states have overlapping constraints. We could do this by:

* Altering our types so that the GoL and wireworld share the tame types
* Using the `Either` monad

In this post we choose the former approach.

## Definition of Wire World
A refresher on the Rules of WireWorld:

### Cell Types
There are four types of Wireworld Cells:

* Conductors
* Electron Head
* Electron Tail
* Empty

We represent these types by adding them to our original Game of Life type so that we can compose them later:
```haskell
data Cell = Alive | Empty | Head | Tail | Conductor
          deriving (Eq, Show, Read, Enum)
```

### Transition Rules
WireWorld has the following rules governing transition:

* `Empty` -> `Empty`
* `Head`  -> `Tail`
* `Tail`  -> `Conductor`
* `Conductor` -> `Head` if exactly one or two neighbouring cells are electron heads
* `Conductor` -> `Conductor` if the above condition is not true

Using the `Rule` monad we can represent wireworld like this:

```haskell
wireWorld :: Rule Cell Cell
wireWorld = do
    s <- self
    n <- countAround (==Head)
    return $ case s of
        Conductor ->
            if n == 1 || n == 2 then
                Head
            else
                Conductor
        Head     -> Tail
        Tail     -> Conductor
        a        -> a
```

Here is the simple wireworld diodes being simulated using the monad:

![Wire world diodes]({{site.url}}/assets/images/ww.gif)

## Combination
Now that we have representations for wireworld and GoL we should consider how to combine them:
Both of the monads, when evaluated, could produce different results for the same Cell. How do we deal
with that?

### liftM2 to the rescue!
![Do you even lift]({{site.url}}/assets/images/duel.jpg)

Do you even lift? I most certainly do not in real life. But that's okay because haskell provides us with liftM2:

```haskell
liftM2  :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM2 f m1 m2 = do { x1 <- m1; x2 <- m2; return (f x1 x2) }
```
`liftM2` accepts a pure function and two monads as arguments. It applies the pure function to the monads while respecting their computational context. 
So for example consider the maybe monad (`Maybe a`). If one of the argument monads where `Nothing` then result of using `liftM2` on any pure function
would also be `Nothing`.

This is very useful for combining our `Rules`.

## Combining Game of Life and Wire World

Awesome so now we have `liftM2`, a `Rule` for GoL and a `Rule` for wireworld. We have the building blocks for a more complex cellular automaton.
This is how we create a `Rule` that allows both the GoL and wireworld automatons to retain their behaviors and operate independently (sort of):

```haskell
gameOfWireWorld :: Rule Cell Cell
gameOfWireWorld = liftM2 pick gameOfLife wireWorld
    where pick Cell -> Cell -> Cell
          pick Alive Empty     = Alive
          pick Empty Alive     = Empty
          pick _     a         = a
```

The `pick` function dictates that GoL can only operate `Empty` and `Alive` cells. Wire World operates on all other Cells.
Because of the general nature of `liftM2` this function could be further generalised to other rule monads that produce other
types like `Bool`, `String` or even numbers.

`wireWorld` and `gameOfLife` are coexisting in `gameOfWireWorld` bellow:

![Game of WireWorld]({{site.url}}/assets/images/ww-gol.gif)

### Interaction between Game of Life and Wire World
Finally, let's think about a rule describing how the GoL and wireworld monads can interact. Life does after all require some form of energy, right? 
We will define the following rule:

* If Cell is Empty and precisely one neighbour is electron `Head` then become `Alive`
* Otherwise return Empty

Here it is:

```haskell
comeAlive :: Rule Cell Cell
comeAlive = do
    s <- self
    n <- countAround (==Head)
    return $
        if s == def && n == 1 then
            Alive
        else
            def -- Default value : The empty type
```

`comingAlive` is not very interesting and exhibits behavours we could consider regressive.
It will make `Conductor` and `Tail` cells empty if left alone.
But when combined with `gameOfWireWorld` using `liftM2` the `Rule` creates a simple mechanism of interaction between a wireworld
circuit and GoL cells. Perhaps this is just a metaphor for the interaction between nature and machinery? Without further ado!

```haskell
gameOfWireWorldComingAlive :: Rule Cell Cell
gameOfWireWorldComingAlive = liftM2 pick gameOfWireWorld comeAlive
    where pick Empty Alive = Alive
          pick a     _     = a
```

Since `comingAlive` awakens only empty Cells, we allow it to operate when it does not interfere with a non-empty `gameOfWireWorld` cell.
Otherwise we allow `gameOfWireWorld` to operate normally. The addition of `comingAlive` results in some completely different behavior in
the `gameOfWireWorld` `Rule`.

![A wireworld game of life coming alive]({{site.url}}/assets/images/ww-gol-ca.gif)

# Conclusion
So there you have it folks! An example of how Monads provide powerful mechanisms for means of abstraction and combination. With a few simple components we
could continue to build arbitrarily complex cellular automatons through composition. I created an example project which has examples, seed grid
configurations and a simpler viewer.
You can find it [over here](https://github.com/Ivan1931/gol-rules-haskell)

I hope you enjoyed reading this post as much as I did making it!
