-- { -w 320 -h 240 -z 1 -r 1 -s 1 Simple/2Lights.lua #-}

local C = require("Colors")

camera = Camera({2.0,2,-6.0}, {1.0,1.0,0.0}, 60)

SetBackground(YGradient(C.purple, C.red, 0.5))
AddLight(PointLight({-2.0, 2.0, 6.0}, C.yellow))
AddLight(PointLight({2.0, 2.0, 0.0}, C.blue))

floor = Plane({0.0, 1.0, 0.0}, 0.0,
              { texture = Checkerboard(C.white, C.black, 1.0),
                material = { ambient = 0.1, diffuse = 0.8 } })
AddObject(floor)

