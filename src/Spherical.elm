module Spherical exposing (..)

-- Need some coordinate mangling
-- https://www.movable-type.co.uk/scripts/latlong.html

meanRadius = 6371000

metresPerDegree = 78846.81


-- Equirectangular approximation
range (φ1, λ1) (φ2, λ2) = 
  let 
      x = (λ2 - λ1) * cos ((φ1 + φ2)/2)
      y = (φ2 - φ1)
  in 
      meanRadius * sqrt (x*x + y*y)

findBearingToTarget (φ1, λ1) (φ2, λ2) =
  let
    y = sin (λ2 - λ1) * cos φ2
    x = cos φ1 * sin φ2 - sin φ1 * cos φ2 * cos (λ2 - λ1)
  in
      atan2 y x

-- Find new lat long after travelling d metres on given bearing.



newPosition : (Float, Float) -> Float -> Float -> (Float, Float)
newPosition (φ1, λ1) d θ =
  let
       δ = d / meanRadius
       φ2 = asin ( sin φ1 * cos δ + cos φ1 * sin δ * cos θ )
       λ2 = λ1 + atan2 (sin θ * sin δ * cos φ1) (cos δ - sin φ1 * sin φ2)
  in
    (φ2, λ2)

approximateElevation : Float -> Float -> Float
approximateElevation r h =
    let a = (meanRadius + h) / r
        b = r / (meanRadius + h)
    in
        a - b |> sqrt |> acos |> ((-) (pi/2))


cartesianTargetPosition : (Float, Float) -> Float -> Float -> (Float, Float)
cartesianTargetPosition (λ1, φ1) r θ =
    --Find lat and long given range and bearing from a known point.
    --Uses range in meters!
    --where φ is latitude, λ is longitude,
    --θ is the bearing (clockwise from north),
    --δ is the angular distance d/R;
    --d being the distance travelled, R the earth’s radius
    let δ =
            r / meanRadius
        φ2 =
            asin ( sin φ1 * cos δ + cos φ1 * sin δ * cos θ )
        λ2 =
            λ1 + atan2
                    ((sin θ) * (sin δ) * (cos φ1))
                    ((cos δ) - (sin φ1) * (sin φ2))
    in
        (λ2, φ2)
