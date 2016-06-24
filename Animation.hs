-----------------------------------------------------------------------------
-- |
-- Module      :  Animation
-- Copyright   :  (c) Ross Paterson 2012-2015
-- License     :  BSD
-- Maintainer  :  Ross Paterson
-- Stability   :  experimental
-- Portability :  portable
--
-- A library for describing simple animations as Scalable Vector Graphics
-- documents.
--
-----------------------------------------------------------------------------

module Animation (
    -- * Simple animations
    Animation,
    svg,
    -- ** Simple shapes
    rect, ellipse, circle, polygon,
    -- ** Modifying animations
    -- *** Interiors of shapes
    withoutPaint, withPaint, withGenPaint,
    -- *** Borders of shapes
    withoutBorder, withBorder, withGenBorder,
    -- *** Transparency
    translucent,
    -- *** Spatial transformations
    translate, scale, rotate,
    -- ** Combining animations
    plus, combine,
    -- * Time-varying values
    Varying,
    always, onceSteps, onceSmooth, repeatSteps, repeatSmooth,
    -- ** Special cases
    cycleSteps, cycleSmooth, spinner,
    -- * Colours
    Colour,
    rgb, hsl,
    -- ** HTML colour names
    white, silver, gray, black, red, maroon, yellow, olive,
    lime, green, aqua, teal, blue, navy, fuchsia, purple,
    -- ** Other colour names
    grey, cyan, magenta,
    -- * Basic types
    Length, Point, Angle, Time, Fraction,
    -- * Examples
    -- $examples
  ) where

import Numeric (showHex, showFFloat)

infixr 4 `plus`

-- | An animation is an SVG content string describing a number of dynamic
-- coloured shapes.
--
-- A number of functions for creating and manipulating such animations are
-- provided below.
newtype Animation = Anim String

-- | Picture formed by drawing the second picture over the first.
plus :: Animation -> Animation -> Animation
plus (Anim pic1) (Anim pic2) = Anim (pic1 ++ pic2)

-- | Combination of a list of pictures, with later ones drawn over
-- earlier ones.
combine :: [Animation] -> Animation
combine = foldr plus (Anim [])

-- | A value that may vary over time.
data Varying v
    = Constant v
        -- ^ A constant value
    | Varying v [(Time, v)] Change AtEnd
        -- ^ @'Varying' v0 [(t1, v1), ..., (tn, vn)] c e@ is a value
        -- that is initially @v0@, then changes to @v1@ at time @t1@
        -- and so on until time @tn@.

mapVarying :: (a -> b) -> Varying a -> Varying b
mapVarying f (Constant v) = Constant (f v)
mapVarying f (Varying v steps change atend) =
    Varying (f v) [(t, f x) | (t, x) <- steps] change atend

-- | How to change from one value to another
data Change
    = Discrete      -- ^ replace the current value with the new one
    | Linear        -- ^ smoothly change from the current value to the
                        -- new one (for value types that permit this)

-- | What to do at the end of the animation
data AtEnd
    = Remove        -- ^ return to the original value
    | Freeze        -- ^ stay at the last value
    | Repeat        -- ^ repeat the animation

-- | A constant value, the same at all times.
always :: v -> Varying v
always x = Constant x

-- | A value that switches between a series of values at given times.
--
-- @'onceSteps' v0 [(t1, v1), ..., (tn, vn)]@ is a value that is
-- initially @v0@, then switches to @v1@ at time @t1@ and so on until
-- time @tn@, after which it remains at value @vn@.
onceSteps :: v -> [(Time, v)] -> Varying v
onceSteps v tvs = Varying v tvs Discrete Freeze

-- | Like 'onceSteps', except that the value changes smoothly.
onceSmooth :: v -> [(Time, v)] -> Varying v
onceSmooth v tvs = Varying v tvs Linear Freeze

-- | Like 'onceSteps', but repeat the list of switches.
--
-- @'repeatSteps' v0 [(t1, v1), ..., (tn, vn)]@ is a value that is
-- initially @v0@, then switches to @v1@ at time @t1@ and so on until
-- time @tn@, after which it returns to @v1@ and the cycle repeats.
repeatSteps :: v -> [(Time, v)] -> Varying v
repeatSteps v tvs = Varying v tvs Discrete Repeat

-- | Like 'repeatSteps', except that the value changes smoothly.
repeatSmooth :: v -> [(Time, v)] -> Varying v
repeatSmooth v tvs = Varying v tvs Linear Repeat

-- | @'cycleSteps' t vs@ steps between the elements of @vs@ every @t@ seconds,
-- repeating at the end of the list.
cycleSteps :: Time -> [v] -> Varying v
cycleSteps t (v0:vs) =
    repeatSteps (head vs)
        [(fromIntegral n*t, v) | (n, v) <- zip [(0::Int)..] (v0:vs++[v0])]
cycleSteps _ [] = error "cycleSteps given empty list of steps"

-- | @'cycleSmooth' t vs@ shifts smoothly between the elements of @vs@
-- every @t@ seconds, repeating at the end of the list.
cycleSmooth :: Time -> [v] -> Varying v
cycleSmooth t (v0:vs) =
    repeatSmooth (head vs)
        [(fromIntegral n*t, v) | (n, v) <- zip [(0::Int)..] (v0:vs++[v0])]
cycleSmooth _ [] = error "cycleSmooth given empty list of steps"

-- | @'spinner' t@ is an angle that smoothly increases from @0@ to @360@
-- degrees every @t@ seconds (or the reverse of @t@ is negative).
-- An animation @'rotate' ('spinner' t) pic@ rotates @pic@ clockwise
-- every @t@ seconds (counterclockwise if @t@ is negative).
spinner :: Time -> Varying Angle
spinner t
  | t > 0 = repeatSmooth 0 [(0, 0), (t, 360)]
  | otherwise = repeatSmooth 0 [(0, 360), (-t, 0)]

animate :: String -> Varying String -> (Attribute, String)
animate attrName (Constant v) = ((attrName, v), "")
animate attrName (Varying v steps change atend) =
    ((attrName, v), emptyElement "animate" attrs)
  where
    attrs = ("attributeName", attrName) :
            atEndAttr atend :
            animateAttrs steps change

atEndAttr :: AtEnd -> Attribute
atEndAttr Remove = ("fill", "remove")
atEndAttr Freeze = ("fill", "freeze")
atEndAttr Repeat = ("repeatCount", "indefinite")

animateAttrs :: [(Time, String)] -> Change -> [Attribute]
animateAttrs steps change =
    ("begin", showNumber begin) :
    ("dur", showNumber duration) :
    ("keyTimes", mkList [showNumber ((t-begin)/duration) | (t, _) <- steps]) :
    ("values", mkList [v | (_, v) <- steps]) :
    changeAttrs change
  where
    begin = fst (head steps)
    finish = fst (last steps)
    duration = finish - begin
    mkList [] = error "varying value with an empty list of steps"
    mkList [x] = x
    mkList (x:xs) = x ++ ';' : mkList xs

changeAttrs :: Change -> [Attribute]
changeAttrs Discrete = [("calcMode", "discrete")]
changeAttrs Linear = []

-- | @'svg' w h pic@ produces a Scalable Vector Graphics document
-- rendering the picture in a rectangular area of width @w@ and height @h@,
-- both measured in picture units (initially pixels).
-- The origin is at the top left of the drawing area.
-- Positions within the drawing area can be specified using the 'Point' type.
--
-- Shapes within the picture are drawn with 'black' interiors and with
-- zero-width borders unless otherwise specified.
svg :: Length -> Length -> Animation -> String
svg w h (Anim pic) =
    xml "svg" "-//W3C//DTD SVG 1.1//EN"
        "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd"
        (element "svg"
            [("width", showNumber w),
             ("height", showNumber h),
             ("xmlns", "http://www.w3.org/2000/svg"),
             ("version", "1.1")]
            pic)

-- Shapes

-- | @'rect' w h@ describes a rectangle of width @w@ and height @h@,
-- with its top left corner at the origin.
rect :: Varying Length -> Varying Length -> Animation
rect w h = Anim (element "rect" [w_attr, h_attr] (w_anim ++ h_anim))
  where
    (w_attr, w_anim) = animate "width" (mapVarying showNumber w)
    (h_attr, h_anim) = animate "height" (mapVarying showNumber h)

-- | @'ellipse' rx ry@ describes an ellipse centred on the origin,
-- with horizontal and vertical radii @rx@ and @ry@ respectively.
-- See 'circle' for the special case where the radii are the same.
ellipse :: Varying Length -> Varying Length -> Animation
ellipse rx ry =
    Anim (element "ellipse" [rx_attr, ry_attr] (rx_anim ++ ry_anim))
  where
    (rx_attr, rx_anim) = animate "rx" (mapVarying showNumber rx)
    (ry_attr, ry_anim) = animate "ry" (mapVarying showNumber ry)

-- | @'circle' r@ is a circle with radius @r@, centred on the origin.
-- A circle is an 'ellipse' in which the two radii are the same.
circle :: Varying Length -> Animation
circle r = Anim (element "circle" [r_attr] r_anim)
  where (r_attr, r_anim) = animate "r" (mapVarying showNumber r)

-- | A polygon with vertices at the listed points.
polygon :: [Point] -> Animation
polygon points =
    Anim (emptyElement "polygon"
        [("points", unwords (map showPoint points))])

-- Modifying pictures

-- | A generalization of 'withPaint' permitting partially transparent paint.
--
-- @'withGenPaint' c v pic@ is a picture obtained from @pic@ by filling
-- shapes whose colour has not already been specified with colour @c@
-- and opacity @v@.
withGenPaint :: Varying Colour -> Varying Fraction -> Animation -> Animation
withGenPaint colour opacity (Anim pic) =
    graphic [f_attr, o_attr] (f_anim ++ o_anim ++ pic)
  where
    (f_attr, f_anim) = animate "fill" (mapVarying showColour colour)
    (o_attr, o_anim) = animate "fill-opacity" (mapVarying showNumber opacity)

-- | @'withPaint' c pic@ is a picture obtained from @pic@ by filling
-- shapes whose colour has not already been specified with colour @c@.
--
-- (equivalent to @'withGenPaint' c ('always' 1) pic@)
withPaint :: Varying Colour -> Animation -> Animation
withPaint colour = withGenPaint colour (Constant 1)

-- | @'withoutPaint' pic@ is a picture obtained from @pic@ by making
-- the interiors of shapes whose colour has not already been specified
-- fully transparent.
--
-- (equivalent to @'withGenPaint' 'black' ('always' 0) pic@)
withoutPaint :: Animation -> Animation
withoutPaint = withGenPaint (Constant black) (Constant 0)

-- | A generalization of 'withBorder' permitting partially transparent borders.
--
-- @'withGenBorder' c t v pic@ is a picture obtained from @pic@ by drawing
-- shapes whose border has not already been specified with borders of
-- colour @c@, thickness @t@ and opacity @v@.
withGenBorder :: Varying Colour -> Varying Fraction -> Varying Length -> Animation -> Animation
withGenBorder colour opacity thickness (Anim pic) =
    graphic [s_attr, o_attr, w_attr] (s_anim ++ o_anim ++ w_anim ++ pic)
  where
    (s_attr, s_anim) = animate "stroke" (mapVarying showColour colour)
    (o_attr, o_anim) = animate "stroke-opacity" (mapVarying showNumber opacity)
    (w_attr, w_anim) = animate "stroke-width" (mapVarying showNumber thickness)

-- | @'withBorder' c t pic@ is a picture obtained from @pic@ by drawing
-- shapes whose border has not already been specified with borders of
-- colour @c@ and thickness @t@.
--
-- (equivalent to @'withGenBorder' c t ('always' 1) pic@)
withBorder :: Varying Colour -> Varying Length -> Animation -> Animation
withBorder colour thickness = withGenBorder colour (Constant 1) thickness

-- | @'withBorder' c t pic@ is a picture obtained from @pic@ by drawing
-- the shapes whose border has not already been specified without borders.
--
-- (equivalent to @'withGenBorder' c ('always' 0) ('always' 1) pic@)
withoutBorder :: Animation -> Animation
withoutBorder = withGenBorder (Constant black) (Constant 1) (Constant 0)

-- | @'translucent' v pic@ is a picture identical to @pic@ but with
-- opacity @v@, ranging from 0 (fully transparent) to 1 (fully opaque).
translucent :: Varying Fraction -> Animation -> Animation
translucent opacity (Anim pic) = graphic [attr] (anim ++ pic)
  where (attr, anim) = animate "opacity" (mapVarying showNumber opacity)

-- | @'translate' (x,y) pic@ is a picture obtained by moving @pic@ right
-- by @x@ units and down by @y@ units.
translate :: Varying Point -> Animation -> Animation
translate p pic = transform "translate" (mapVarying showPoint p) pic

-- | @'scale' s pic@ is a picture obtained by scaling @pic@ by a factor @s@.
-- Thus @scale ('always' (2, 2)) pic@ is twice as big,
-- and @scale ('always' (0.5, 0.5)) pic@ is half the size.
scale :: Varying Point -> Animation -> Animation
scale s pic = transform "scale" (mapVarying showPoint s) pic

-- | @'rotate' angle pic@ is a picture obtained by rotating @pic@ by
-- @angle@ degrees clockwise around the origin.
rotate :: Varying Angle -> Animation -> Animation
rotate angle pic = transform "rotate" (mapVarying showNumber angle) pic

transform :: String -> Varying String -> Animation -> Animation
transform name (Constant v) (Anim pic) = graphic [attr] pic
  where attr = ("transform", name ++ "(" ++ v ++")")
transform name (Varying v steps change atend) (Anim pic) =
    graphic [attr] (anim ++ pic)
  where
    attr = ("transform", name ++ "(" ++ v ++")")
    anim = emptyElement "animateTransform" attrs
    attrs = ("attributeName", "transform") :
            ("type", name) :
            atEndAttr atend :
            animateAttrs steps change

graphic :: [Attribute] -> String -> Animation
graphic attrs pic = Anim (element "g" attrs pic)

-- Colours

-- | An abstract representation of a colour.
-- Colours can be constructed using the functions 'rgb' (from their red,
-- green and blue components) and 'hsl' (specifying hue, saturation
-- and lightness).
-- Several named colours are also provided for convenience.
data Colour = RGB Fraction Fraction Fraction

-- | A colour with the specified red, green and blue components.
-- For example, 'red' is equivalent to @'rgb' 1 0 0@.
rgb :: Fraction -- ^ red component
    -> Fraction -- ^ green component
    -> Fraction -- ^ blue component
    -> Colour
rgb r g b
  | r < 0 || r > 1 = error $ "red component out of range: " ++ show r
  | g < 0 || g > 1 = error $ "green component out of range: " ++ show g
  | b < 0 || b > 1 = error $ "blue component out of range: " ++ show b
  | otherwise = RGB r g b

-- | A colour specified by hue, saturation and lightness components,
-- which is often more convenient than 'rgb'.
--
-- Note that lightness constrains the effect of saturation: saturation
-- has the most effect for a lightness value of @0.5@, and little effect
-- for lightness values near @0@ or @1@.
-- For example,
--
--  * colours of the form @'hsl' 0 0 v@ are shades or grey, ranging from
--    'black' (@v@ = 0) to 'white' (@v@ = 1).
--
--  * colours of the form @'hsl' h 1 0.5@ are bright pure colours,
--    e.g. 'red' (@h = 0@), 'yellow' (@h = 60@) and 'blue' (@h = 240@).
--
hsl :: Angle    -- ^ hue component: a position on the colour wheel,
                -- e.g. @0@ = red, @120@ = green, @240@ = blue.
    -> Fraction -- ^ saturation component: the strength of the colour,
                -- ranging from @0@ (grey) to @1@ (maximum strength for
                -- the given lightness).
    -> Fraction -- ^ lightness component: the lightness of the colour,
                -- ranging from @0@ (black) to @1@ (white).
    -> Colour
hsl h s l
  | s < 0 || s > 1 = error $ "saturation component out of range: " ++ show s
  | l < 0 || l > 1 = error $ "lightness component out of range: " ++ show l
  | otherwise = RGB r g b
  where
    chroma = (1 - abs (2*l - 1)) * s
    component c = (min 1 (max 0 (2 - 6*abs (snd (properFraction (c/360 + 1/2) :: (Int, Double)) - 1/2)))) * chroma - chroma/2 + l
    r = component h
    g = component (h + 240)
    b = component (h + 120)

-- | <<http://www.w3.org/TR/html4/images/white.gif>>
-- The colour white.
white :: Colour
white = RGB 1 1 1

-- | <<http://www.w3.org/TR/html4/images/silver.gif>>
-- A light grey colour.
silver :: Colour
silver = RGB 0.75 0.75 0.75

-- | <<http://www.w3.org/TR/html4/images/gray.gif>>
-- A medium grey colour (half way between 'white' and 'black').
gray :: Colour
gray = RGB 0.5 0.5 0.5

-- | <<http://www.w3.org/TR/html4/images/black.gif>>
-- The colour black.
black :: Colour
black = RGB 0 0 0

-- | <<http://www.w3.org/TR/html4/images/red.gif>>
-- The primary colour red.
red :: Colour
red = RGB 1 0 0

-- | <<http://www.w3.org/TR/html4/images/maroon.gif>>
-- A darker version of 'red'.
maroon :: Colour
maroon = RGB 0.5 0 0

-- | <<http://www.w3.org/TR/html4/images/yellow.gif>>
-- The secondary colour yellow (opposite of 'blue').
yellow :: Colour
yellow = RGB 1 1 0

-- | <<http://www.w3.org/TR/html4/images/olive.gif>>
-- A darker version of 'yellow'.
olive :: Colour
olive = RGB 0.5 0.5 0

-- | <<http://www.w3.org/TR/html4/images/lime.gif>>
-- The primary colour bright green.
lime :: Colour
lime = RGB 0 1 0

-- | <<http://www.w3.org/TR/html4/images/green.gif>>
-- A darker version of 'lime'.
green :: Colour
green = RGB 0 0.5 0

-- | <<http://www.w3.org/TR/html4/images/aqua.gif>>
-- The secondary colour aqua or cyan (opposite of 'red').
aqua :: Colour
aqua = RGB 0 1 1

-- | <<http://www.w3.org/TR/html4/images/teal.gif>>
-- A darker version of 'aqua'.
teal :: Colour
teal = RGB 0 0.5 0.5

-- | <<http://www.w3.org/TR/html4/images/blue.gif>>
-- The primary colour blue.
blue :: Colour
blue = RGB 0 0 1

-- | <<http://www.w3.org/TR/html4/images/navy.gif>>
-- A darker version of 'blue'.
navy :: Colour
navy = RGB 0 0 0.5

-- | <<http://www.w3.org/TR/html4/images/fuchsia.gif>>
-- The secondary colour fuchsia or magenta (opposite of 'lime').
fuchsia :: Colour
fuchsia = RGB 1 0 1

-- | <<http://www.w3.org/TR/html4/images/purple.gif>>
-- A darker version of 'fuchsia'.
purple :: Colour
purple = RGB 0.5 0 0.5

-- | <<http://www.w3.org/TR/html4/images/gray.gif>>
-- An alias for 'gray'
grey :: Colour
grey = gray

-- | <<http://www.w3.org/TR/html4/images/aqua.gif>>
-- An alias for 'aqua' (opposite of 'red').
cyan :: Colour
cyan = aqua

-- | <<http://www.w3.org/TR/html4/images/fuchsia.gif>>
-- An alias for 'fuchsia' (opposite of 'lime').
magenta :: Colour
magenta = fuchsia

-- Basic types

-- | A value ranging between 0 and 1 (inclusive).
type Fraction = Double

-- | A distance, measured in picture units (initially pixels).
type Length = Double

-- | An angle, measured in degrees.
type Angle = Double

-- | A time, measured in seconds.
type Time = Double

-- | Coordinates of a position within a picture, measured in picture
-- units (initially pixels).  The first coordinate increases moving to
-- the right; the second increases moving down.
type Point = (Length, Length)

showPoint :: Point -> String
showPoint (x, y) = showNumber x ++ "," ++ showNumber y

-- simple XML output

xml :: String -> String -> String -> String -> String
xml doctype pub_id url body =
    unlines [
        "<?xml version=\"1.0\" encoding=\"US-ASCII\" standalone=\"no\"?>",
        "<!DOCTYPE " ++ doctype ++ " PUBLIC \"" ++ pub_id ++ "\"",
        "  \"" ++ url ++ "\">",
        body]

showColour :: Colour -> String
showColour (RGB r g b) = "#" ++ showComp r ++ showComp g ++ showComp b
  where
    showComp x
      | length hex == 1 = "0" ++ hex
      | otherwise = hex
      where hex = showHex (round (255*x) :: Int) ""

showNumber :: Double -> String
showNumber x = trim (showFFloat (Just 6) x "")
  where
    -- drop trailing zeroes (and decimal point, if no zeroes)
    trim = reverse . dropWhile (== '.') . dropWhile (== '0') . reverse

type Attribute = (String, String)

element :: String -> [Attribute] -> String -> String
element name attrs "" =
    "<" ++ name ++ attributes attrs ++ "/>"
element name attrs body =
    "<" ++ name ++ attributes attrs ++ ">" ++ body ++ "</" ++ name ++ ">"

emptyElement :: String -> [Attribute] -> String
emptyElement name attrs = "<" ++ name ++ attributes attrs ++ "/>"

attributes :: [Attribute] -> String
attributes attrs = concat [" " ++ n ++ "=\"" ++ v ++ "\"" | (n, v) <- attrs]

-- $examples
--
-- This example shows a simple 'polygon' as well as making colours with
-- 'hsl'.
--
-- > triangle :: Animation
-- > triangle = withPaint (always (hsl 0.4 0.5 0.25))
-- >         (polygon [(10,10), (10,100), (150, 10)])
--
-- <<triangle.svg>>
--
-- This example illustrates transparency, showing how 'plus' puts the
-- second picture on top of the first, as well as drawing the borders
-- of shapes.
--
-- > roundel :: Animation
-- > roundel =
-- >     translate (always (100, 75))
-- >         (withBorder (always red) (always 20)
-- >             (withoutPaint (circle (always 50))))
-- >     `plus`
-- >     translate (always (25, 62.5))
-- >         (withGenPaint (always blue) (always 0.75)
-- >             (rect (always 150) (always 25)))
--
-- <<roundel.svg>>
--
-- This example shows an animation, achieved by supplying a varying
-- position to 'translate':
--
-- > bouncing :: Animation
-- > bouncing =
-- >     withPaint (always red)
-- >         (translate (cycleSmooth 0.5
-- >                  [(50, 100), (150, 50), (250, 100),
-- >                   (350, 150), (250, 200), (150, 150)])
-- >             (circle (always 20)))
-- >    `plus`
-- >    withBorder (always navy) (always 10)
-- >        (withoutPaint
-- >            (translate (always (30, 30)) (rect (always 340) (always 190))))
--
-- <<bouncing.svg>>
