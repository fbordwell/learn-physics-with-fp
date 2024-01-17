{-# OPTIONS -Wall #-}

module Charge where

import CoordinateSystems
import Electricity
import Geometry
import Integrals
import SimpleVec

type Charge = R

data ChargeDistribution
  = PointCharge Charge Position
  | LineCharge ScalarField Curve
  | SurfaceCharge ScalarField Surface
  | VolumeCharge ScalarField Volume
  | MultipleCharges [ChargeDistribution]

protonOrigin :: ChargeDistribution
protonOrigin = PointCharge elementaryCharge origin

chargedLine :: Charge -> R -> ChargeDistribution
chargedLine q len =
  LineCharge (const $ q / len) $
    Curve (cart 0 0) (-len / 2) (len / 2)

chargedBall :: Charge -> R -> ChargeDistribution
chargedBall q radius =
  VolumeCharge (const $ q / (4 / 3 * pi * radius ** 3)) $
    Volume
      (\(r, theta, phi) -> sph r theta phi)
      0
      radius
      (const 0)
      (const pi)
      (\_ _ -> 0)
      (\_ _ -> 2 * pi)
      0
      radius
      (const 0)
      (const pi)
      (\_ _ -> 0)
      (\_ _ -> 2 * pi)

diskCap :: R -> R -> R -> ChargeDistribution
diskCap radius plateSep sigma =
  MultipleCharges
    [ SurfaceCharge (const sigma) $
        shiftSurface (vec 0 0 (plateSep / 2)) (disk radius),
      SurfaceCharge (const $ -sigma) $
        shiftSurface (vec 0 0 (-plateSep / 2)) (disk radius)
    ]

totalCharge :: ChargeDistribution -> Charge
totalCharge (PointCharge q _) = q
totalCharge (LineCharge lambda c) = scalarLineIntegral (curveSample 1000) lambda c
totalCharge (SurfaceCharge sigma s) = scalarSurfaceIntegral (surfaceSample 200) sigma s
totalCharge (VolumeCharge rho v) = scalarVolumeIntegral (volumeSample 50) rho v
totalCharge (MultipleCharges ds) = sum [totalCharge d | d <- ds]

simpleDipole ::
  Vec -> -- electric dipole moment
  R -> -- charge separation
  ChargeDistribution
simpleDipole p sep =
  let q = magnitude p / sep
      disp = (sep / 2) *^ (p ^/ magnitude p)
   in MultipleCharges
        [ PointCharge q (shiftPosition disp origin),
          PointCharge (-q) (shiftPosition (negateV disp) origin)
        ]