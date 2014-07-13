-- { -w 320 -h 240 -z 1 -r 1 -s 1 Simple/Simple1 #-}

local C = require("Colors")

camera = Camera({2.0,2,-6.0}, {1.0,1.0,0.0}, 60)

SetBackground(YGradient(C.purple, C.red, 0.5))
AddLight(PointLight({0.0, 1.0, 6.0}, C.white))
AddLight(PointLight({4.0, 1.0, 3.0}, C.white))

sphere1 = Sphere(1.0, { texture = Checkerboard(C.red, C.green, 0.1),
                        mapping = Spherical(),
                        material = {
                          ambient  = 0.1, 
                          diffuse = 0.7, 
                          specular = 0.9, 
                          reflect = 0.3, 
                          shine = 20 }})

sphere1 = RX(T(sphere1, 2, 0.9, 6.0), 30)
--sphere1 = RX(RZ(T(sphere1, 2, 0.9, 6.0), 30), 45)

AddObject(sphere1)

box1 = Box({0.0, 0.0, 0.0}, {1,1,1},
           {texture  = Pigment(C.blue),
            material = { ambient = 0.2, diffuse = 0.9, specular = 0.9, opacity = 0.5 }})
box1 = RX(RZ(T(box1, -2.5, 1.0, 6.0),15),45)
AddObject(box1)


floor = Plane({0.0, 1.0, 0.0}, 0.0,
              { texture = Checkerboard(C.white, C.black, 1.0),
                material = { ambient = 0.1, diffuse = 0.8 } })
AddObject(floor)

