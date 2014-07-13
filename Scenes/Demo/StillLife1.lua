-- { -w 180 -h 120 -z 1 -r 1 -s 1 Demo/StillLife1 #-}

local C = require("Colors")

camera = Camera({4.0,0.5,-3.0}, {0.0,0.5,0.0}, 60)

SetBackground(YGradient(C.purple, C.red, 0.5))
AddLight(PointLight({1.0, 3.0, -4.0}, C.white))

obj1 = Sphere(1.0, 
             {texture = Checkerboard(C.red, C.green, 0.08),
              material = { 
                ambient = 0.1,
                diffuse = 0.7,
                specular = 0.9,
                reflect = 0.2,
                shine = 30 } } )
--obj1 = RZ(RX(T(obj1, 1, 0, 4),45),30)
obj1 = Translate(obj1, 1, 0, 4)
AddObject(obj1)

obj2 = Sphere(1.0, 
              {texture = Pigment(C.black),
               material = { 
                 ambient = 0.7,
                 diffuse = 0.7,
                 specular = 0.9,
                 reflect = 0.7,
                 refract = 4,
                 shine = 20 } } )
obj2 = Translate(obj2, 1.1, 0.5, 1)
AddObject(obj2)


floor = Plane({0.0, 1.0, 0.0}, 1.0,
              {texture = Checkerboard(C.black, C.white, 1),
               material = {ambient = 0.4, diffuse = 0.6 } } )
AddObject(floor)
