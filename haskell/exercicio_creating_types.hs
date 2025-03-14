{-
 - We are going to create some types for a deck of cards
 - The cards need to have an ordering, based on the standard ranking http://en.wikipedia.org/wiki/Standard_52-card_deck#Rank_and_color
 - We are assuming Aces are high.
 - Therefore, the following statements should be true:
 -    (Card Ace Spades) > (Card King Spades)
 -    (Card Two Clubs) < (Card Three Clubs)
 -
 - We are going to provide our own implementation of the Show typeclass for the Card type.
 - When displaying the Card instance in GHCI, or calling show (Card digit suit), the String which should be displayed is "The <Digit> of <Suit>"
 -
 - Uncomment the following declarations to complete the implementation, and provide an implementation for instance Show Card
 -}

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Eq, Ord)
data Digit = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Eq, Ord)
data Card = Card Digit Suit deriving (Ord)

instance Show Suit where
    show Spades   = "Spades"
    show Hearts   = "Hearts"
    show Diamonds = "Diamonds"
    show Clubs    = "Clubs"

instance Show Digit where
    show Two   = "Two"
    show Three = "Three"
    show Four  = "Four"
    show Five  = "Five"
    show Six   = "Six"
    show Seven = "Seven"
    show Eight = "Eight"
    show Nine  = "Nine"
    show Ten   = "Ten"
    show Jack  = "Jack"
    show Queen = "Queen"
    show King  = "King"
    show Ace   = "Ace"

instance Show Card where
    show (Card digit suit) = "The " ++ show digit ++ " of " ++ show suit

instance Eq Card where
    (Card digit1 suit1) == (Card digit2 suit2) = (digit1 == digit2) && (suit1 == suit2)

-- We should be able to provide a function which returns the higher ranked card:
betterCard :: Card -> Card -> Card
betterCard x y
    | x > y     = x
    | otherwise = y

-- Here is a new Typeclass, which represents some kind of playing hand in a game.
-- It returns True for a "winning hand", depending on the rules for the type of class we are playing with
class Hand a where
    play :: [a] -> Bool

-- Implement Hand for Card, where play returns true if the list contains the Ace of Spades
instance Hand Card where
    play c = undefined

-- Create a new Coin type
--data Coin = 

-- Implement Hand for Coin, where play returns true if there are ten heads in a row in the list
-- instance Hand Coin where
--         play c =  undefined

-- Have a play with implementing Hand for some other types, for instance Int and Bool