{-# LANGUAGE DeriveDataTypeable, RecordWildCards  #-}
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Debug.Trace

leftV (x, y) = (-y, x)
addV (x1, y1) (x2, y2) = (x1+x2, y1+y2)

data Object = Object {
    position :: Point,
    velocity :: Vector,
    charge :: Float,
    mass :: Float
}

type MagneticField = Point -> Float -- egyelőre mindig szembe néz
type FieldDefinition = Model -> MagneticField

data Model = Model {
    field :: FieldDefinition,
    object :: Object,
    path :: [Point],
    currentTime :: Float
}

size = 500

lorentzForce :: MagneticField -> Object -> Vector
lorentzForce aktField aktObj = mulSV (fieldHere * charge aktObj) (leftV $ velocity aktObj)
    where
        fieldHere = aktField (position aktObj)


progress _ time original@Model{..} = if False then original
    else 
        original{
            object = object{
                position = newPosition,
                velocity = mulSV (magV (velocity object)) (normalizeV (addV (velocity object) deltaVelocity))
            },
            path = take 8000 $ newPosition : path,
            currentTime = currentTime + time
        }
    where
        aktField = field original
        force = lorentzForce aktField object
        deltaMomentum = mulSV time force
        deltaVelocity = mulSV (1 / mass object) deltaMomentum
        deltaPosition = mulSV time (velocity object)
        newPosition = addV (position object) deltaPosition

adapt = Scale sizeFloat sizeFloat
        where sizeFloat = fromIntegral size

circleAt radius pos = uncurry Translate pos $ Pictures [(Color yellow $ Circle radius)]
toRad :: Float -> Float
toRad angle = angle * (pi/180)
origo :: Point
origo = (0,0)

drawVector :: Vector -> Picture
drawVector vec = Scale 0.4 0.4 $ Line [origo, vec, rotScale sc ang vec, vec, rotScale sc (-ang) vec]
    where
        rotScale newScale angle = rotateV angle . mulSV newScale
        ang = toRad 5
        sc = 0.9

drawVectorFrom :: Point -> Vector -> Picture
drawVectorFrom pos = uncurry Translate pos . drawVector



view :: Model -> Picture
view original@Model{..} =
    adapt $ Pictures [prevPath, aktObject, aktVel, aktForce]
    where
        prevPath = Pictures (map (circleAt 0.001) path)
        aktObject = circleAt 0.003 (position object)
        aktVel = Color red $ drawVectorFrom (position object) (velocity object)
        aktForce = Color green $ drawVectorFrom (position object) force
        force = lorentzForce (field original) object
        


interesting :: [FieldDefinition]
interesting = [allando, belulTartos, idoFuggo]


belulTartos :: FieldDefinition
belulTartos Model{..} pos@(x, y) = if x > 0 || magV pos > 0.9 then 1 else -1

idoFuggo :: FieldDefinition
idoFuggo Model{..} pos@(x, y) = sin (currentTime)

allando :: FieldDefinition
allando _ _ = 1

initial = Model{
    field = idoFuggo,
    object = Object{
        position = (0,0),
        velocity = (0.5, 0),
        charge = 1,
        mass = 0.2
    },
    path = [],
    currentTime = 0
}
main = simulate
    (InWindow "Szimuláció" (size*2,size*2) (100, 100))
    black
    600
    initial
    view
    progress