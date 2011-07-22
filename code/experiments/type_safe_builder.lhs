We need the empty datatypes GHCI extension to implement phantom types. I'll explain what they are and why we need them later, but this declaration has to go at the top of the file.

> {-# LANGUAGE EmptyDataDecls #-}

We want to be able to describe orders with the following syntax:

bid 250 `for` 5 `units_of` security
ask 100  `for` 8 `units_of` security
buy 300 `units_of` security
sell 200 `units_of` security

(Sadly 'of' is a reserved word in Haskell).

First, let's define some synonyms for the types of which our order is composed.

> type Price = Int
> type Quantity = Int
> type Security = String -- For now.

We want an Order type which can represent bids and asks, as well as buys and sells. We also want to be able to incrementally build them up using the 'for' and 'units_of' combinators, so their member values must be optional. However, using the Maybe monad to represent optional values is going to introduce a lot of noise to our data constructors (eg "Bid (Just 250) (Just 5) (Just "GOOG")") and doesn't allow compile time checking that a function only takes complete orders. The solution is to use 'phantom types' that can encode the presence or absence of a certain value-level member:

> class OrderField a 

> data NoValue
> data HasPrice
> data HasQuantity
> data HasSecurity

> instance OrderField NoValue
> instance OrderField HasPrice
> instance OrderField HasQuantity
> instance OrderField HasSecurity

We can then parameterise our Order type constructor over these values:

> data Order a b c = Bid1 Price | Bid2 Price Quantity | Bid Price Quantity Security | Ask1 Price | Ask2 Price Quantity | Ask Price Quantity Security | Buy1 Quantity | Buy Quantity Security | Sell1 Quantity | Sell Quantity Security deriving (Eq, Show)

Now let's write our bid, ask, buy and sell functions. These are easy as they are just synonyms for the respective value constructors that only take a single value:

> bid :: Price -> Order HasPrice NoValue NoValue
> bid = Bid1

> ask :: Price -> Order HasPrice NoValue NoValue
> ask = Ask1

> buy :: Quantity -> Order NoValue HasQuantity NoValue
> buy = Buy1

> sell :: Quantity -> Order NoValue HasQuantity NoValue
> sell = Sell1

This just leaves us with our 'for' and 'units_of' combinators, which have the following type signatures:

> for :: Order HasPrice NoValue NoValue -> Quantity -> Order HasPrice HasQuantity NoValue

> units_of :: (OrderField a) => Order a HasQuantity NoValue -> Security -> Order a HasQuantity HasSecurity

(note we use a type variable here as 'units_of' can be applied to limit orders which will have a price, and market orders which will not)

Now we write the implementation of these. There's a little cruft involved as we have to match the value constructor of the argument in order to find the value constructor for the output:

> for (Bid1 price) quantity = Bid2 price quantity
> for (Ask1 price) quantity = Ask2 price quantity

We leave 'for' undefined in the case of market orders, as they do not admit a price.

> units_of (Bid2 price quantity) security = Bid price quantity security
> units_of (Ask2 price quantity) security = Ask price quantity security
> units_of (Buy1 quantity) security       = Buy quantity security
> units_of (Sell1 quantity) security       = Sell quantity security

Now  let's try it out! In GHCI:

Prelude> :l type_safe_builder.lhs
[1 of 1] Compiling Main             ( type_safe_builder.lhs, interpreted )
Ok, modules loaded: Main.
Prelude> bid 200 `for` 50 `units_of` "GOOG"
Bid 200 50 "GOOG"
Prelude> ask 100 `for` 20 `units_of` "VODA"
Ask 100 20 "Voda"
Prelude> buy 20 `units_of` "MSFT"
Buy 20 "MSFT"
Prelude> sell 10000 `units_of` "NWSA"
Sell 10000 "NWSA"
