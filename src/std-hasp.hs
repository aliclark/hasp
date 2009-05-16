
-- These help provide alternatives to some of Haskell's syntax.
module Hasp where {
  type List a = [a];

  -- These are too useful not to have.
  inc :: (Num a) => ((->) a a);
  inc = ((+) 1);

  dec :: (Num a) => ((->) a a);
  dec = (\x -> ((-) x 1));

  zero :: (Num a) => ((->) a Bool);
  zero = ((==) 0);

  cons :: ((->) a ((->) (List a) (List a)));
  cons = (:);

  -- Conditionals to replace if-then-else, guards, and case-of.
  if' :: ((->) Bool ((->) a ((->) a a)));
  if' = (\s_gensym_v_0 -> (\s_gensym_v_1 -> (\s_gensym_v_2 -> (case (s_gensym_v_0, s_gensym_v_1, s_gensym_v_2) of {
  (True, x, _) -> x;
  (False, _, y) -> y;}))));

  condList :: ((->) (List (Bool, a)) ((->) a a));
  condList = (\s_gensym_v_3 -> (\s_gensym_v_4 -> (case (s_gensym_v_3, s_gensym_v_4) of {
  ([], e) -> e;
  (((:) (test, v) xs), e) -> (if' test v (condList xs e));})));

  caseList :: (Eq a) => ((->) a ((->) (List (a, b)) ((->) b b)));
  caseList = (\s_gensym_v_5 -> (\s_gensym_v_6 -> (\s_gensym_v_7 -> (case (s_gensym_v_5, s_gensym_v_6, s_gensym_v_7) of {
  (x, [], y) -> y;
  (x, ((:) (z, v) ts), y) -> (if' ((==) x z) v (caseList x ts y));}))));

  -- Accessors, a replacement for records.
  type Getter whole part = ((->) whole part);

  type Setter whole part = ((->) whole ((->) part whole));

  data Accessor whole part = Accessor (Getter whole part) (Setter whole part);

  getter :: ((->) (Accessor n a) (Getter n a));
  getter = (\s_gensym_v_8 -> (case (s_gensym_v_8) of {
  ((Accessor getter setter)) -> getter;}));

  setter :: ((->) (Accessor n a) (Setter n a));
  setter = (\s_gensym_v_9 -> (case (s_gensym_v_9) of {
  ((Accessor getter setter)) -> setter;}));

  get :: ((->) n ((->) (Accessor n a) a));
  get = (\s_gensym_v_10 -> (\s_gensym_v_11 -> (case (s_gensym_v_10, s_gensym_v_11) of {
  (rec, (Accessor getter setter)) -> (getter rec);})));

  set :: ((->) n ((->) (Accessor n a) ((->) a n)));
  set = (\s_gensym_v_12 -> (\s_gensym_v_13 -> (\s_gensym_v_14 -> (case (s_gensym_v_12, s_gensym_v_13, s_gensym_v_14) of {
  (rec, (Accessor getter setter), v) -> (setter rec v);}))));

  update :: ((->) n ((->) (Accessor n a) ((->) ((->) a a) n)));
  update = (\s_gensym_v_15 -> (\s_gensym_v_16 -> (\s_gensym_v_17 -> (case (s_gensym_v_15, s_gensym_v_16, s_gensym_v_17) of {
  (rec, (Accessor getter setter), f) -> (setter rec (f (getter rec)));}))));
}

