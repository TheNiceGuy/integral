module Main where

-- | Ce type de données défini les systèmes de coordonnées possibles.
-- |
-- | Un point dans un système de coordonnées cartésiennes contient :
-- |     * une composante en $x$
-- |     * une composante en $y$
-- |     * une composante en $z$
-- |
-- | Un point dans un système de coordonnées cylindriques contient :
-- |     * une composante radiale $r$
-- |     * un angle $\theta$ tel que $0\leq\theta\leq 2\pi$
-- |     * une composante en $z$
-- |
-- | Un point dans un système de coordonnées sphériques contient :
-- |     * une composante radiale $r$
-- |     * un angle $\theta$ tel que $0\leq\theta<2\pi$
-- |     * un angle $\phi$ tel que $0\leq\phi\leq\pi$
data Coordinate = Cartesian | Cylindrical | Spherical

-- | Ce type de données défini un point ayant trois composantes. Ce point peut
-- | être un point dans un système de coordonnées cartésiennes, cylindriques ou
-- | sphériques.
data Point = Point Double Double Double

-- | Ce type de données défini les bornes de chaque axes dans un système de
-- | coordonnées.
data Space = Space Double Double Double Double Double Double

-- | Ce type de données défini une résolution pour chaque axe dans une
-- | résolution.
data Resolution = Resolution Int Int Int

-- | Ce type de données défini en ensemble de points dans un système de
-- | coordonnées.
data Domain = Domain Coordinate Double Double Double [Point]

-- | Cette fonction calcule une subdivision sur un intervalle séparé $n$ fois.
-- |
-- | Double: position initiale
-- | Double: position finale
-- |    Int: nombre de subdivisions $n$
delta :: Double -> Double -> Int -> Double
delta a b n = (b-a)/(fromIntegral n)

-- | Cette fonction calcule chaque positions d'un intervalle séparé $n$ fois.
-- |
-- | Double: position initiale
-- | Double: taille d'une subdivision
-- |    Int: nombre de subdivision $n$
interval :: Double -> Double -> Int -> [Double]
interval a d n = [a+d*(fromIntegral i) | i <- [1..n]]

-- | Cette fonction normalize le rayon $r$ en coordonnées cylindriques ou
-- | sphériques de sorte à ce que $r\geq 0$.
-- |
-- | Double: le rayon $r$ à normalizer
normr :: Double -> Double
normr r = if r < 0
              then 0
              else r

-- | Cette fonction normalize l'angle $\theta$ en coordonnées cylindriques ou
-- | sphériques de sorte à ce que $0\leq\theta<2\pi$.
-- |
-- | Double: l'angle $\theta$ à normalizer
normt :: Double -> Double
normt t = if t < 0
              then (normt t+2*pi)
              else if t > 2*pi
                   then (normt t-2*pi)
                   else t

-- | Cette fonction normalize l'angle $\phi$ en coordonnées sphériques de sorte
-- | à ce que $0\leq\phi<\pi$.
-- |
-- | Double: l'angle $\phi$ à normalizer
normp :: Double -> Double
normp p = if p < 0
              then 0
              else if p > pi
                   then pi
                   else p

-- | Cette fonction génère les points d'un domaine cartésien, cylindrique ou
-- | sphérique. Elle ne génère pas nécessairement le domaine d'intégration,
-- | mais les points qui vont éventuellement faire partie du ce domaine. Pour
-- | une bonne intégration, ce domaine doit contenir au minimum le domaine
-- | d'intégration.
-- |
-- | Coordinate: le système de coordonnées
-- |      Space: le domaine de chaque axe
-- | Resolution: le nombre de subdivisions à appliquer sur chaque axe
createDomain :: Coordinate -> Space -> Resolution -> Domain
createDomain (Cartesian) s r = Domain Cartesian dx dy dz points
    where
        dx = (\(Space x1 x2 _ _ _ _) (Resolution n _ _) -> delta x1 x2 n) s r
        dy = (\(Space _ _ y1 y2 _ _) (Resolution _ n _) -> delta y1 y2 n) s r
        dz = (\(Space _ _ _ _ z1 z2) (Resolution _ _ n) -> delta z1 z2 n) s r
        xx = (\(Space x1 _ _ _ _ _) (Resolution n _ _) -> interval x1 dx n) s r
        yy = (\(Space _ _ y1 _ _ _) (Resolution _ n _) -> interval y1 dy n) s r
        zz = (\(Space _ _ _ _ z1 _) (Resolution _ _ n) -> interval z1 dz n) s r
        points = (Point) <$> xx <*> yy <*> zz
createDomain (Cylindrical) s r = Domain Cylindrical dr dt dz points
    where
        dr = (\(Space r1 r2 _ _ _ _) (Resolution n _ _) -> delta (normr r1) (normr r2) n) s r
        dt = (\(Space _ _ t1 t2 _ _) (Resolution _ n _) -> delta (normt t1) (normt t2) n) s r
        dz = (\(Space _ _ _ _ z1 z2) (Resolution _ _ n) -> delta z1 z2 n) s r
        rr = (\(Space r1 _ _ _ _ _) (Resolution n _ _) -> interval r1 dr n) s r
        tt = (\(Space _ _ t1 _ _ _) (Resolution _ n _) -> interval t1 dt n) s r
        zz = (\(Space _ _ _ _ z1 _) (Resolution _ _ n) -> interval z1 dz n) s r
        points = (Point) <$> rr <*> tt <*> zz
createDomain (Spherical) s r = Domain Spherical dr dt dp points
    where
        dr = (\(Space r1 r2 _ _ _ _) (Resolution n _ _) -> delta (normr r1) (normr r2) n) s r
        dt = (\(Space _ _ t1 t2 _ _) (Resolution _ n _) -> delta (normt t1) (normt t2) n) s r
        dp = (\(Space _ _ _ _ p1 p2) (Resolution _ _ n) -> delta (normp p1) (normp p2) n) s r
        rr = (\(Space r1 _ _ _ _ _) (Resolution n _ _) -> interval r1 dr n) s r
        tt = (\(Space _ _ t1 _ _ _) (Resolution _ n _) -> interval t1 dt n) s r
        pp = (\(Space _ _ _ _ p1 _) (Resolution _ _ n) -> interval p1 dp n) s r
        points = (Point) <$> rr <*> tt <*> pp

-- | Cette fonction calcul le volume infinitisémal d'un système de coordonnées.
-- |
-- | Domain: le domaine auquel on veut le volume
-- |  Point: le point du volume infinitisémal
getVolume :: Domain -> Point -> Double
getVolume (Domain Cartesian dx dy dz _) _ = dx*dy*dz
getVolume (Domain Cylindrical dr dt dz _) (Point r t z) = r*dr*dt*dz
getVolume (Domain Spherical dr dt dp _) (Point r t p) = r*r*sin(p)*dr*dt*dp

-- | Cette fonction extrait les points d'un domaine.
-- |
-- | Domain: le domaine auquel on veut les points
getPoints :: Domain -> [Point]
getPoints (Domain _ _ _ _ p) = p

-- | Cette fonction calcule une intégrale triple. Un prédicat est utilisé pour
-- | filtrer le domain afin d'obtenir le domain d'intégration.
-- |
-- |   Domain: le domaine d'intégration initiale
-- | Fonction: prédicat pour filtrer le domaine
-- | Fonction: fonction à intégrer
integrate :: Domain -> (Point -> Bool) -> (Point -> Double) -> Double
integrate domain predicate function = sum $ vol
    where
        set = [p | p <- (getPoints domain), (predicate p)]
        vol = map (\p -> (function p)*(getVolume domain p)) set

main :: IO ()
main = do
    -- compute the volume of a sphere of radius 3
    putStrLn $ show $ (36.0*pi)
    putStrLn $ show $ integrate (createDomain coor1 space1 res1) predicate1 function1
    putStrLn $ show $ integrate (createDomain coor2 space2 res2) predicate2 function2
    putStrLn $ show $ integrate (createDomain coor3 space3 res3) predicate3 function3
        where
            coor1   = Cartesian
            res1    = Resolution 100 100 100
            space1  = Space (-4) 4 (-4) 4 (-4) 4
            predicate1 = (\(Point x y z) -> x*x+y*y+z*z <= 9)
            function1  = (\(Point x y z) -> 1)

            coor2   = Cylindrical
            res2    = Resolution 100 200 100
            space2  = Space 0 3 0 (2*pi) (-4) 4
            predicate2 = (\(Point r t z) -> z <= (sqrt(9-r*r)) && z >= (-sqrt(9-r*r)))
            function2  = (\(Point r t z) -> 1)

            coor3   = Spherical
            res3    = Resolution 100 200 200
            space3  = Space 0 3 0 (2*pi) 0 pi
            predicate3 = (\(Point r t p) -> True)
            function3  = (\(Point r t p) -> 1)
