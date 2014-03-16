{-# LANGUAGE PatternGuards #-}

module Text.Latex where

import Text.PrettyPrint
import Data.List

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

parensIf ::  Bool -> Doc -> Doc
parensIf True = parens
parensIf False = id

bracesIf ::  Bool -> Doc -> Doc
bracesIf True = braces
bracesIf False = id

class Pretty p where
  ppr :: Int -> p -> Doc

-------------------------------------------------------------------------------
-- LaTeX
-------------------------------------------------------------------------------

newline :: Doc
newline = text "\\\\"

cmd :: String -> Doc
cmd x = char '\\' <> text x

app :: String -> [Doc] -> Doc
app x xs = cmd x <> hcat (map braces xs)

-- Inline math
inline :: Doc -> Doc
inline body = text "$$" $$ body $$ text "$$"

-- Display math
display :: Doc -> Doc
display body = text "\\[" $$ body $$ text "\\]"


-- begin end block
be :: Doc -> Doc -> Doc
be x body = cmd "begin" <> braces x $$ body $$ cmd "end" <> braces x

-- begin end block with arguments
bea :: Doc -> Doc -> Doc -> Doc
bea x y body = cmd "begin" <> braces x <> braces y $$ body $$ cmd "end" <> braces x

-------------------------------------------------------------------------------
-- Delimiters
-------------------------------------------------------------------------------

left, right :: Doc -> Doc
left o = cmd "left" <+> o
right o = cmd "right" <+> o

wrap :: Doc -> Doc -> Doc -> Doc
wrap l r body = left l <+> body <+> right r

langle, rangle :: Doc
langle = cmd "langle"
rangle = cmd "rangle"

lfloor, rfloor :: Doc
lfloor = cmd "lfloor"
rfloor = cmd "rfloor"

bigParens :: Doc -> Doc
bigParens = wrap lparen rparen

bigBraces :: Doc -> Doc
bigBraces = wrap lbrace rbrace

bigBrackets :: Doc -> Doc
bigBrackets = wrap lbrack rbrack

bigAngles :: Doc -> Doc
bigAngles = wrap langle rangle

bigFloor :: Doc -> Doc
bigFloor = wrap lfloor rfloor

-------------------------------------------------------------------------------
-- Math
-------------------------------------------------------------------------------

-- →
to :: Doc
to = cmd "to"

-- ↦
mapsto :: Doc
mapsto = cmd "mapsto"

-- ∀
forall :: Doc
forall = cmd "forall"

-- ∃
exists :: Doc
exists = cmd "exists"

-- †
dagger :: Doc
dagger = cmd "dagger"

-- ‡
ddagger :: Doc
ddagger = cmd "ddagger"

--  ∞
infty :: Doc
infty = cmd "infty"

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

arccos = undefined
cos = undefined
csc = undefined
exp = undefined
ker = undefined
min = undefined
sinh = undefined
arcsin = undefined
cosh = undefined
deg = undefined
gcd = undefined
lg = undefined
ln = undefined
pr = undefined
arctan = undefined
cot = undefined
det = undefined
hom = undefined
log = undefined
sec = undefined
tan = undefined
arg = undefined
coth = undefined
dim = undefined
inf = undefined
max = undefined
sin = undefined
tanh = undefined

-------------------------------------------------------------------------------
-- Greek
-------------------------------------------------------------------------------

greek :: [String]
greek = [
    "alpha"   , "beta"      , "gamma"
  , "gammau"  , "delta"     , "deltau"
  , "epsilon" , "varepsilon", "zeta"
  , "eta"     , "theta"     , "thetau"
  , "iota"    , "kappa"     , "lambda"
  , "lambdau" , "mu"        , "nu"
  , "xi"      , "xiu"       , "pi"
  , "varpi"   , "piu"       , "rho"
  , "varrho"  , "sigma"     , "varsigma"
  , "sigmau"  , "tau"       , "upsilon"
  , "upsilonu", "phi"       , "varphi"
  , "phiu"    , "chi"       , "psi"
  , "psiu"    , "omega"     , "omegau"
  ]


-------------------------------------------------------------------------------
-- Text Formats
-------------------------------------------------------------------------------

mathbf, mathrm, mathcal, mathsf, mathtt, mathit :: Doc -> Doc
mathbf l = app "mathbf" [l]
mathrm l = app "mathrm" [l]
mathcal l = app "mathcal" [l]
mathsf l = app "mathsf" [l]
mathtt l = app "mathtt" [l]
mathit l = app "mathit" [l]

-------------------------------------------------------------------------------
-- Operator
-------------------------------------------------------------------------------

operator :: String -> Doc -> Doc -> Doc
operator o a b = a <+> cmd o <+> b

-- Dot
cdot :: Doc -> Doc -> Doc
cdot = operator "cdot"

-- Asterisk
ast :: Doc -> Doc -> Doc
ast = operator "ast"

-- Circ
circ :: Doc -> Doc -> Doc
circ = operator "circ"

-- Times
times :: Doc -> Doc -> Doc
times = operator "times"

-- Bullet
bullet :: Doc -> Doc -> Doc
bullet = operator "bullet"

-- Wedge
wedge :: Doc -> Doc -> Doc
wedge = operator "wedge"

-- Wedge
star :: Doc -> Doc -> Doc
star = operator "star"

-- Cup
cup :: Doc -> Doc -> Doc
cup = operator "cup"

-- Cap
cap :: Doc -> Doc -> Doc
cap = operator "cap"

oplus :: Doc -> Doc -> Doc
oplus = operator "coplus"

ominus :: Doc -> Doc -> Doc
ominus = operator "ominus"

otimes :: Doc -> Doc -> Doc
otimes = operator "otimes"

oslash :: Doc -> Doc -> Doc
oslash = operator "oslash"

odot :: Doc -> Doc -> Doc
odot = operator "odot"

-------------------------------------------------------------------------------
-- Subscript and Superscript
-------------------------------------------------------------------------------

sup :: Doc -> Doc -> Doc
sup x y = x <> char '^' <> braces y

sub :: Doc -> Doc -> Doc
sub x y = x <> char '_' <> braces y

subsup :: Doc -> Doc -> Doc -> Doc
subsup a b x = sup (sub a x) b

-------------------------------------------------------------------------------
-- Fractions
-------------------------------------------------------------------------------

frac, cfrac, dfrac :: Doc -> Doc -> Doc
frac a b = app "frac" [a,b]
cfrac a b = app "cfrac" [a,b]
dfrac a b = app "dfrac" [a,b]

cfraction :: Doc -> [Doc] -> Doc
cfraction = foldr frac

-------------------------------------------------------------------------------
-- Calculus
-------------------------------------------------------------------------------

-- Indefinite integral
int :: Doc -> Doc -> Doc
int x v = cmd "int" <+> x <+> v

iint :: Doc -> Doc -> Doc
iint x v = cmd "iint" <+> x <+> v

iiint :: Doc -> Doc -> Doc
iiint x v = cmd "iiint" <+> x <+> v

oint :: Doc -> Doc -> Doc
oint x v = cmd "oint" <+> x <+> v

oiint :: Doc -> Doc -> Doc
oiint x v = cmd "oiint" <+> x <+> v

oiiint :: Doc -> Doc -> Doc
oiiint x v = cmd "oiiint" <+> x <+> v

-- Indefinite integral
dint :: Maybe Doc -> Maybe Doc -> Doc -> Doc -> Doc
dint u l x v |
  Just u' <- u,
  Just l' <- l = subsup (cmd "int") u' l' <+> x <+> v

dint u l x v |
  Just u' <- u,
  Nothing <- l
  = sup (cmd "int") u' <+> x <+> v

dint u l x v |
  Nothing <- u,
  Just l' <- l
  = sub (cmd "int") l' <+> x <+> v

dint _ _ x v = Text.Latex.int x v

diint :: Doc -> Doc -> Doc -> Doc -> Doc
diint u l x v = subsup (cmd "int") u l <+> x <+> v

-- d/dx
diff :: Doc -> Doc
diff v = frac (char 'd') (char 'd' <> v)

diff' :: Doc -> Doc -> Doc
diff' a b = frac (char 'd' <> a ) (char 'd' <> b)

-- limits
lim :: Doc -> Doc -> Doc
lim a b = sub (cmd "lim") a <> b

-------------------------------------------------------------------------------
-- Combinatorics
-------------------------------------------------------------------------------

sum :: Maybe Doc -> Maybe Doc -> Doc -> Doc
sum u l x |
  Just u' <- u,
  Just l' <- l = subsup (cmd "sum") u' l' <+> x

sum u l x |
  Just u' <- u,
  Nothing <- l
  = sup (cmd "sum") u' <+> x

sum u l x |
  Nothing <- u,
  Just l' <- l
  = sub (cmd "sum") l' <+> x

sum _ _ x = cmd "sum" <+> x

prod :: Maybe Doc -> Maybe Doc -> Doc -> Doc
prod u l x |
  Just u' <- u,
  Just l' <- l = subsup (cmd "prod") u' l' <+> x

prod u l x |
  Just u' <- u,
  Nothing <- l
  = sup (cmd "prod") u' <+> x

prod u l x |
  Nothing <- u,
  Just l' <- l
  = sub (cmd "prod") l' <+> x

prod _ _ x = cmd "prod" <+> x

-------------------------------------------------------------------------------
-- Accents
-------------------------------------------------------------------------------

hat :: Doc -> Doc
hat l = app "hat" [l]

acute :: Doc -> Doc
acute l = app "acute" [l]

bar :: Doc -> Doc
bar l = app "bar" [l]

dot :: Doc -> Doc
dot l = app "dot" [l]

breve :: Doc -> Doc
breve l = app "breve" [l]

check :: Doc -> Doc
check l = app "check" [l]

grave :: Doc -> Doc
grave l = app "grave" [l]

vec :: Doc -> Doc
vec l = app "vec" [l]

ddot :: Doc -> Doc
ddot l = app "ddot" [l]

tilde :: Doc -> Doc
tilde l = app "tilde" [l]

-------------------------------------------------------------------------------
-- Arrays and Alignment
-------------------------------------------------------------------------------

rows :: [Doc] -> Doc
rows xs = hsep (intersperse (char '&') xs)

cols :: [Doc] -> Doc
cols xs = vcat (map (<+> newline) xs)

arrbody :: [[Doc]] -> Doc
arrbody = cols . map rows

array :: [[Doc]] -> Doc
array xs = be (text "array") (arrbody xs)

align :: String -> [Doc] -> Doc
align a xs  = bea (text "align*") (text a) (hsep $ punctuate (char '&') xs)
