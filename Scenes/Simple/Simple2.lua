-- { -w 320 -h 240 -z 1 -r 1 -s 1 Simple/Simple2 #-}

local C = require("Colors")

camera = Camera({-4,1,-5}, {-2,1,2}, 60)

SetBackground(YGradient(C.purple, C.red, 0.5))

AddLight(PointLight({-0.5, 2.0, 6}, C.white))

ls = Sphere(0.1, { texture = Pigment(C.white), material = {ambient  = 1.0, shadow = false}})
ls = T(ls, -0.5, 2.0, 4)
AddObject(ls)

---
s = Sphere(0.75, { texture = Checkerboard(C.red, C.green, 0.08),
              mapping = Cubic(),
                        material = {
                          ambient  = 0.2, 
                          diffuse = 0.6, 
                          specular = 1.0, 
                          reflect = 0.1, 
                          shine = 20 }})

s = RZ(RX(T(s, 0.0, 0.4, 7.0), 30),30)
AddObject(s)

wall = Box({0.0, 0.0, 0.0}, {1,1,1},
           {texture  = Pigment(C.blue),
            material = { ambient = 0.2
                       , diffuse = 0.9
                       , reflect = 0.95
                       , specular = 0.9
                       , opacity = 0.6
                       , refract = 1.33
                       , shine = 100 }})
wall = RX(RY(T(wall, -2, -1.0, 8.0),15),-25)
wall = Scale(wall,5,5,0.01)
AddObject(wall)

floor = Plane({0.0, 1.0, 0.0}, 1.0,
              { texture = Checkerboard(C.white, C.black, 1.0),
                material = { ambient = 0.4, diffuse = 0.6 } })
AddObject(floor)
