{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import qualified Control.Foldl as L
import Control.Monad.Primitive (unsafeInlineIO)
import qualified Data.GraphViz as GV
import qualified Data.Map as Map
import qualified Data.Text as Text
import Diagrams.Backend.SVG (SVG, renderSVG)
import Diagrams.Prelude hiding (Loop, (<>))
import Diagrams.TwoD ()
import qualified Diagrams.TwoD.GraphViz as DGV
import qualified Diagrams.TwoD.Text
import Protolude as P

data Class
  = Magma
  | Unital
  | Associative
  | Commutative
  | Invertible
  | Idempotent
  | Absorbing
  | Group
  | AbelianGroup
  | Additive
  | Subtractive
  | Multiplicative
  | Divisive
  | Distributive
  | Semiring
  | Ring
  | IntegralDomain
  | Field
  | ExpField
  | QuotientField
  | UpperBoundedField
  | LowerBoundedField
  | TrigField
  -- Higher-kinded numbers
  | AdditiveAction
  | SubtractiveAction
  | MultiplicativeAction
  | DivisiveAction
  | Module
  -- Lattice
  | JoinSemiLattice
  | MeetSemiLattice
  | Lattice
  | BoundedJoinSemiLattice
  | BoundedMeetSemiLattice
  | BoundedLattice
  -- Integrals
  | Integral
  -- Measure
  | Normed
  | Signed
  | Epsilon
  -- Instances
  | Double'
  | Int'
  deriving (Show, Eq, Ord)

data Family
  = Addition
  | Multiplication
  | Actor
  deriving (Show, Eq, Ord)

data Dependency = Dependency
  { _class :: Class
  , _dep :: Class
  , _op :: Maybe Family
  } deriving (Show, Eq, Ord)

dependencies :: [Dependency]
dependencies =
  [ Dependency Unital Magma Nothing
  , Dependency Associative Magma Nothing
  , Dependency Commutative Magma Nothing
  , Dependency Invertible Magma Nothing
  , Dependency Idempotent Magma Nothing
  , Dependency Absorbing Magma Nothing
  , Dependency Group Unital Nothing
  , Dependency Group Invertible Nothing
  , Dependency Group Associative Nothing
  , Dependency AbelianGroup Unital Nothing
  , Dependency AbelianGroup Invertible Nothing
  , Dependency AbelianGroup Associative Nothing
  , Dependency AbelianGroup Commutative Nothing
  , Dependency Additive Commutative (Just Addition)
  , Dependency Additive Unital (Just Addition)
  , Dependency Additive Associative (Just Addition)
  , Dependency Subtractive Invertible (Just Addition)
  , Dependency Subtractive Additive (Just Addition)
  , Dependency Multiplicative Unital (Just Multiplication)
  , Dependency Multiplicative Associative (Just Multiplication)
  , Dependency Multiplicative Commutative (Just Multiplication)
  , Dependency Divisive Invertible (Just Multiplication)
  , Dependency Divisive Multiplicative (Just Multiplication)
  , Dependency Distributive Additive (Just Addition)
  , Dependency Distributive Multiplicative (Just Multiplication)
  , Dependency Distributive Absorbing Nothing
  , Dependency Semiring Additive (Just Addition)
  , Dependency Semiring Distributive Nothing
  , Dependency Semiring Associative (Just Multiplication)
  , Dependency Semiring Unital (Just Multiplication)
  , Dependency Ring Distributive Nothing
  , Dependency Ring Subtractive (Just Addition)
  , Dependency IntegralDomain Ring Nothing
  , Dependency Field Ring Nothing
  , Dependency Field Divisive (Just Multiplication)
  , Dependency ExpField Field Nothing
  , Dependency QuotientField Field Nothing
  , Dependency QuotientField Ring Nothing
  , Dependency TrigField Field Nothing
  , Dependency UpperBoundedField Field Nothing
  , Dependency LowerBoundedField Field Nothing
  -- higher-kinded relationships
  , Dependency AdditiveAction Additive (Just Actor)
  , Dependency SubtractiveAction Subtractive (Just Actor)
  , Dependency MultiplicativeAction Multiplicative (Just Actor)
  , Dependency DivisiveAction Divisive (Just Actor)
  , Dependency Module Distributive (Just Actor)
  , Dependency Module MultiplicativeAction Nothing
  -- Lattice
  , Dependency JoinSemiLattice Associative Nothing
  , Dependency JoinSemiLattice Commutative Nothing
  , Dependency JoinSemiLattice Idempotent Nothing
  , Dependency MeetSemiLattice Associative Nothing
  , Dependency MeetSemiLattice Commutative Nothing
  , Dependency MeetSemiLattice Idempotent Nothing
  , Dependency Lattice JoinSemiLattice Nothing
  , Dependency Lattice MeetSemiLattice Nothing
  , Dependency BoundedJoinSemiLattice JoinSemiLattice Nothing
  , Dependency BoundedJoinSemiLattice Unital Nothing
  , Dependency BoundedMeetSemiLattice MeetSemiLattice Nothing
  , Dependency BoundedMeetSemiLattice Unital Nothing
  , Dependency BoundedLattice BoundedJoinSemiLattice Nothing
  , Dependency BoundedLattice BoundedMeetSemiLattice Nothing
  , Dependency Signed Multiplicative Nothing
  , Dependency Normed Additive Nothing
  , Dependency Epsilon Additive Nothing
  , Dependency Epsilon Subtractive Nothing
  , Dependency Epsilon MeetSemiLattice Nothing
  , Dependency Integral Distributive Nothing
  ]

fileSvg f s = renderSVG f (mkSizeSpec (Just <$> r2 s))

data Config = Config
  { _rectX :: Double
  , _rectY :: Double
  , _textScale :: Double
  , _arrowScale :: Double
  }

-- | Render an annotated graph as a diagram, given functions
--   controlling the drawing of vertices and of edges.  The first
--   function is given the label and location of each vertex. The
--   second function, for each edge, is given the label and location
--   of the first vertex, the label and location of the second vertex,
--   and the label and path corresponding to the edge.
boxes ps =
  zipWith
    (\p c ->
       place
         ((unitSquare # scaleX 50.0 # scaleY 25.0 # lc (sRGB 0.33 0.33 0.33) .
           opacity 0.3 <>
           (Diagrams.Prelude.text (Text.unpack $ show c) # scale 5.0)) #
          named c)
         p)
    (Map.elems ps :: [P2 Double])
    (Map.keys ps)

edge ::
     (Renderable (Path V2 Double) b)
  => Dependency
  -> QDiagram b V2 Double Any
  -> QDiagram b V2 Double Any
edge (Dependency to from Nothing) =
  connectOutside'
    (headStyle %~ fc (sRGB 0.33 0.33 0.33) . opacity 0.3 $ shaftStyle %~
     lc (sRGB 0.33 0.33 0.33) .
     opacity 0.3 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to
edge (Dependency to from (Just Addition)) =
  connectOutside'
    (headStyle %~ fc red . opacity 0.5 $ shaftStyle %~ lc red . opacity 0.5 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to
edge (Dependency to from (Just Multiplication)) =
  connectOutside'
    (headStyle %~ fc blue . opacity 0.5 $ shaftStyle %~ lc blue . opacity 0.5 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to
edge (Dependency to from (Just Actor)) =
  connectOutside'
    (headStyle %~ fc green . opacity 0.5 $ shaftStyle %~ lc green . opacity 0.5 $
     arrowHead .~
     dart $
     headLength .~
     8 $
     def)
    from
    to

ps cs ds =
  fst $ DGV.getGraph $ unsafeInlineIO $
  DGV.layoutGraph GV.Dot (DGV.mkGraph cs (toEdge <$> ds))
  where
    toEdge (Dependency to from wrapper) = (from, to, wrapper)

instance IsName Class

makeGraph ::
     ( (Renderable (Path V2 Double) b)
     , Renderable (Diagrams.TwoD.Text.Text Double) b
     )
  => Config
  -> [Class]
  -> [Dependency]
  -> QDiagram b V2 Double Any
makeGraph Config {} cs ds =
  L.fold (L.Fold (flip edge) (mconcat $ boxes (ps cs ds)) identity) ds

fieldClasses =
  [ Magma
  , Unital
  , Associative
  , Commutative
  , Invertible
  , Absorbing
  , Additive
  , Subtractive
  , Multiplicative
  , Divisive
  , Distributive
  , Ring
  , Field
  ]

allClasses =
  [ Magma
  , Unital
  , Associative
  , Commutative
  , Invertible
  , Absorbing
  , Group
  , AbelianGroup
  , Additive
  , Subtractive
  , Multiplicative
  , Divisive
  , Distributive
  , Ring
  , Field
  , ExpField
  , QuotientField
  , UpperBoundedField
  , LowerBoundedField
  , TrigField
  -- Higher-kinded numbers
  , MultiplicativeAction
  , Module
  -- Lattice
  , JoinSemiLattice
  , MeetSemiLattice
  , Lattice
  , BoundedJoinSemiLattice
  , BoundedMeetSemiLattice
  , BoundedLattice
  -- Measure
  , Normed
  , Signed
  , Epsilon
  -- Integral
  ]

main :: IO ()
main = do
  let gField = makeGraph (Config 3 30 10 1) fieldClasses dependencies
  fileSvg "other/field.svg" (600, 600) (gField :: QDiagram SVG V2 Double Any)
  let gNumHask = makeGraph (Config 3 30 10 1) allClasses dependencies
  fileSvg "other/numhask.svg" (600, 600) (gNumHask :: QDiagram SVG V2 Double Any)
