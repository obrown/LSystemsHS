-- Examples:
-- -------------
-- drawLSystem bush3 6 rainbowColour
-- drawLSystem hilbert 5 (repeat white)
-- drawLSystem plant 6 treeColour 

module LSystems where

import Graphics

type Rule
  = (Char, String)

type Rules
  = [Rule]

type System
  = (Double, String, Rules)

bush3, asymmetree, hilbert, treeD, plant, cross, triangle, arrowHead,
  peanoGosper, dragon, snowflake, tree, bush :: System

type Vertex
  = (Double, Double, Double)

type Vector = Vertex

type Matrix
  = (Vector, Vector, Vector)

type Quaternion
  = (Double, Double, Double, Double)

type TurtleState
  = (Vertex, Quaternion)

type Stack
  = [TurtleState]

type ColouredLine
  = (Vertex, Vertex, Colour)

--  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --  --

-- Returns the rotation angle for the given system.
angle :: System -> Double
angle (a, _, _) = a

-- Returns the base string for the given system.
base :: System -> String
base (_, b, _) = b

-- Returns the set of rules for the given system.
rules :: System -> Rules
rules (_, _, r) = r

-- Look up a character in the set of rules.
-- Pre: the character exists in the set of rules.
lookupChar :: Char -> Rules -> String
lookupChar char rules 
  = concat [ru | (c, ru) <- rules, c == char] 

-- Expand a command once
expandOne :: Rules -> String -> String
expandOne rules [] = []
expandOne rules (s:ss) 
  = (lookupChar s rules) ++ (expandOne rules ss) 

-- Expand a command `n' times 
expand :: Rules -> String -> Int -> String
expand _ str 0 = str 
expand rules s n
  = expand rules (expandOne rules s) (n - 1) 
  
-- Move a turtle.
--  * 'F' moves distance 1 in the current direction.
--  * 'L' rotates left according to the given angle.
--  * 'R' rotates right according to the given angle.
--  * '^' pitches up according to the given angle 
--  * '&' pitches down according to the given angle 
--  * '<' rolls left according to the given angle.
--  * '>' rolls right according to the given angle.
move :: Char -> TurtleState -> Double -> TurtleState
move command (position@(x, y, z), orientation) angle 
  | command == 'F' = ((x + mX, y + mY, z + mZ), orientation)
  | command == '^' = (position, pitch angle)
  | command == '&' = (position, pitch (-angle))
  | command == 'L' = (position, yaw angle)
  | command == 'R' = (position, yaw (-angle))
  | command == '>' = (position, roll angle)
  | command == '<' = (position, roll (-angle))
  | otherwise = error "Invalid movement"
  where (mX, mY, mZ) = postMultiply (getRotationMatrix orientation) (0, 1, 0)
        pitch        = rotate orientation (1, 0, 0)
        roll         = rotate orientation (0, 1, 0)
        yaw          = rotate orientation (0, 0, 1)

-- Calculates the new orientation of the turtle by rotating
-- the current orientation about the given axis
rotate :: Quaternion -> Vector -> Double -> Quaternion
rotate old axis angle
  = multiplyQuaternions old new
  where new = axisAngleToQuat axis angle

-- Multiplies two quaternions together, compounding their
-- effects
multiplyQuaternions :: Quaternion -> Quaternion -> Quaternion
multiplyQuaternions (aw, ax, ay, az) (bw, bx, by, bz)
  = (r1, r2, r3, r4)
  where r1 = aw*bw - ax*bx - ay*by - az*bz 
        r2 = aw*bx + ax*bw + ay*bz - az*by
        r3 = aw*by + ay*bw + az*bx - ax*bz
        r4 = aw*bz + az*bw + ax*by - ay*bx
        
-- Converts a rotation of an angle about an axis into a 
-- quaternion
axisAngleToQuat :: Vector -> Double -> Quaternion
axisAngleToQuat (x, y, z) angle
  = (c, x*s, y*s, z*s)
  where theta = angle / 2
        c     = cosD theta
        s     = sinD theta 

-- Converts a quaternion into a 3x3 rotation matrix
getRotationMatrix :: Quaternion -> Matrix
getRotationMatrix (w, x, y, z) =
  let y2 = y^2 ; x2 = x^2 ; z2 = z^2
      xy = x*y ; xz = x*z ; yz = y*z
      zw = z*w ; yw = y*w ; xw = x*w

      r1c1 = 1 - 2*(y2 + z2) 
      r2c1 = 2*(xy + zw) 
      r3c1 = 2*(xz - yw)
      r1c2 = 2*(xy - zw)
      r2c2 = 1 - 2*(x2 + z2)
      r3c2 = 2*(yz + xw)
      r1c3 = 2*(xz + yw)
      r2c3 = 2*(yz - xw)
      r3c3 = 1 - 2*(x2 + y2)
  in ((r1c1, r2c1, r3c1), (r1c2, r2c2, r3c2), (r1c3, r2c3, r3c3))

-- Post-multiplies a 3x3 matrix with a 3 value column vector
postMultiply :: Matrix -> Vector -> Vector
postMultiply ((a, b, c), (d, e, f), (g, h, i)) (v1, v2, v3)
  = (a*v1 + d*v2 + g*v3, b*v1 + e*v2 + h*v3, c*v1 + f*v2 + i*v3)

-- Converts from degrees to radians
radians :: Double -> Double
radians degrees
  = degrees*(pi/180)

cosD = cos . radians
sinD = sin . radians

-- Trace lines drawn by a turtle using the given colour, following the
-- commands in the string and assuming the given initial angle of rotation.
trace :: String -> Double -> [Colour] -> [ColouredLine]
trace [] _ _ = []
trace commands angle colours
  = trace' commands [((0, 0, 0), (1, 0, 0, 0))]
  where
    trace' :: String -> [TurtleState] -> [ColouredLine] 
    trace' [] _ = []
    trace' (c:cs) stack@(st:sts) 
      | c == '['  = trace' cs (st:stack) 
      | c == ']'  = trace' cs sts 
      | c == 'F'  = (sPos, ePos, col) : trace' cs (end:sts) 
      | otherwise = trace' cs (end:sts)
      where end@(ePos, _)   = move c st angle
            start@(sPos, _) = st
            col             = colours !! ((length stack) - 1)


--  --  --  --  --  --  --
-- Colouring Rules
--  --  --  --  --  --  --
treeColour    = (brown : brown : brown : repeat green)
rainbowColour = (red : orange : yellow : green 
                : blue : indigo : violet : repeat white)

--  --  --  --  --  --  --
-- 3 dimensional systems
--  --  --  --  --  --  --
bush3 =
  ( 28
  , "M"
  , [ ('M', "MN[-N&N<NMN][<N+N+N&NMN]MN[-N-N&N>NMN][+N&NMN]")
    , ('N', "N")
    , ('+', "+")
    , ('-', "-")
    , ('^', "^")
    , ('&', "&")
    , ('[', "[")
    , (']', "]")
    , ('<', "<")
    , ('>', ">")
    ]
  )

treeD
  = ( 30
    , "X"
    , [ ('X', "M[+X][-X][&X][^X]")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      , ('&', "&")
      , ('^', "^")
      ]
    )


hilbert
  = ( 90
    , "X"
    , [ ('X', "^<XM^<XMX-M^>>XMX&M+>>XMX-M>X->") 
      , ('M', "M")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      , ('^', "^")
      , ('&', "&")
      , ('<', "<")
      , ('>', ">")
      ]
    )

asymmetree
  = ( 22
    , "M"
    , [ ( 'M' , "M[+M][--M][<^M][>&>M]")
        , ('+' , "+")
        , ('-' , "-")
        , ('^' , "^")
        , ('&' , "&")
        , ('[' , "[")
        , (']' , "]")
        , ('<' , "<")
        , ('>' , ">") 
      ]
    )

--  --  --  --  --  --  --
-- 2 dimensional systems
--  --  --  --  --  --  --
plant
  = ( 25
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

cross
  = ( 90
    , "M-M-M-M"
    , [ ('M', "M-M+M+MM-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

triangle
  = ( 90
    , "-M"
    , [ ('M', "M+M-M-M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

arrowHead
  = ( 60
    , "N"
    , [ ('M', "N+M+N")
      , ('N', "M-N-M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

peanoGosper
  = ( 60
    , "M"
    , [ ('M', "M+N++N-M--MM-N+")
      , ('N', "-M+NN++N+M--M-N")
      , ('+', "+")
      , ('-', "-")
      ]
    )

dragon
  = ( 45
    , "MX"
    , [ ('M', "A")
      , ('X', "+MX--MY+")
      , ('Y', "-MX++MY-")
      , ('A', "A")
      , ('+', "+")
      , ('-', "-")
      ]
    )

snowflake
  = ( 60
    , "M--M--M"
    , [ ('M', "M+M--M+M")
      , ('+', "+")
      , ('-', "-")
      ]
    )

tree
  = ( 45
    , "M"
    , [ ('M', "N[-M][+M][NM]")
      , ('N', "NN")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

bush
  = ( 22.5
    , "X"
    , [ ('X', "M-[[X]+X]+M[+MX]-X")
      , ('M', "MM")
      , ('[', "[")
      , (']', "]")
      , ('+', "+")
      , ('-', "-")
      ]
    )

mapper :: Rules
mapper
  = [ ('M', "F")
    , ('N', "F")
    , ('X', "")
    , ('Y', "")
    , ('A', "")
    , ('[', "[")
    , (']', "]")
    , ('+', "L")
    , ('-', "R")
    , ('&', "&")
    , ('^', "^")
    , ('>', ">")
    , ('<', "<")
    ]

lSystem :: System -> Int -> String
lSystem (_, base, rs) n
  = expandOne mapper (expand rs base n)

drawLSystem :: System -> Int -> [Colour] -> IO ()
drawLSystem system n colour
  = drawLines (trace (lSystem system n) (angle system) colour)
