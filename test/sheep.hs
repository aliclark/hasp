
{- Author:     Jeff Newbern
    Maintainer: Jeff Newbern <jnewbern@nomaware.com>
    Time-stamp: <Mon Nov 10 11:59:14 2003>
    License:    GPL
-}
{- DESCRIPTION

Example 1 - Our first monad

Usage: Compile the code and execute the resulting program.
       It will print Dolly's maternal grandfather.
-}
-- everything you need to know about sheep
data Sheep = Sheep String (Maybe Sheep) (Maybe Sheep);

name = (\(Sheep name _ _) -> name);
mother = (\(Sheep _ mother _) -> mother);
father = (\(Sheep _ _ father) -> father);

-- we show sheep by name
instance Show Sheep where {
  show = ((.) show name);
};

-- comb is a combinator for sequencing operations that return Maybe
comb :: ((->) (Maybe a) ((->) ((->) a (Maybe b)) (Maybe b)));
comb = (\s_gensym_v_0 -> (\s_gensym_v_1 -> (case (s_gensym_v_0, s_gensym_v_1) of {
  (Nothing, _) -> Nothing;
  ((Just x), f) -> (f x);})));

-- now we can use `comb` to build complicated sequences
maternalGrandfather :: ((->) Sheep (Maybe Sheep));
maternalGrandfather = (\s -> (comb (comb (Just s) mother) father));

fathersMaternalGrandmother :: ((->) Sheep (Maybe Sheep));
fathersMaternalGrandmother = (\s -> (comb (comb (comb (Just s) father) mother) mother));

mothersPaternalGrandfather :: ((->) Sheep (Maybe Sheep));
mothersPaternalGrandfather = (\s -> (comb (comb (comb (Just s) mother) father) father));

-- this builds our sheep family tree
breedSheep :: Sheep;
breedSheep = (let {
    adam = (Sheep "Adam" Nothing Nothing);
    eve = (Sheep "Eve" Nothing Nothing);
    uranus = (Sheep "Uranus" Nothing Nothing);
    gaea = (Sheep "Gaea" Nothing Nothing);
    kronos = (Sheep "Kronos" (Just gaea) (Just uranus));
    holly = (Sheep "Holly" (Just eve) (Just adam));
    roger = (Sheep "Roger" (Just eve) (Just kronos));
    molly = (Sheep "Molly" (Just holly) (Just roger));
  } in (Sheep "Dolly" (Just molly) Nothing));

-- print Dolly's maternal grandfather
main :: IO ();
main = (let {
    dolly = breedSheep;
  } in (print (maternalGrandfather dolly)));

-- END OF FILE

