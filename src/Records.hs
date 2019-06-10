{-# LANGUAGE TemplateHaskell #-} -- Enable the TemplateHaskell language extension
--There are many language extensions; most are beyond the scope of this talk

module Records where

import Control.Lens hiding (element)

-- We can name the entries in our data type, creating a "record".
data Point = Point { _x :: Double, _y :: Double } deriving (Show)

--We can create a record with or without use of the name:
-- Point 1.0 2.0 is the same as Point { _x = 1.0, _y = 2.0}
point1 = Point 1.0 2.0
point2 = Point { _x = 1.0, _y = 2.0} --beware: this style allows you to have unitialized fields!
--_x Point {_y = 2.0} *** Exception: Missing field in record construction _x
-- but you can compile with -Wmissing-fields to check
xposition = _x point1 -- Haskell creates functions with the same name to access the entry

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

-- Suppose we wanted to move an atom by 1 in the x position.
shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point xvar yvar)) = Atom e (Point (xvar + 1) yvar)
{- This is not very nice, and it gets less nice the more nested records you have. Note:
xvar and yvar are just variables here; there need not be any relation to the name of the entry -}

-- Note: Underscores are not needed for plain records, but are needed for later

-- Here we are using the Lens library which (using TemplateHaskell) creates the functions "point" and "x" below
makeLenses ''Atom
makeLenses ''Point

shiftAtomX' :: Atom -> Atom
shiftAtomX' = over (point . x) (+ 1) -- This is Very Nice!  For example, if we wanted to add a "mass" entry to Atom, we wouldn't have to change this function.

carbon1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0} }

shifted = shiftAtomX' $ shiftAtomX carbon1

-- Now let's get more complicated
data Molecule = Molecule { _atoms :: [Atom] } deriving (Show)

makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+ 1)
{- This almost looks like imperative / object-oriented code! :D -}

atom1 = Atom { _element = "C", _point = Point { _x = 1.0, _y = 2.0 } }
atom2 = Atom { _element = "O", _point = Point { _x = 3.0, _y = 4.0 } }
molecule = Molecule { _atoms = [atom1, atom2] }
-- shiftMoleculeX molecule
-- Molecule {_atoms = [Atom {_element = "C", _point = Point {_x = 2.0, _y = 2.0}},Atom {_element = "O", _point = Point {_x = 4.0, _y = 4.0}}]}

