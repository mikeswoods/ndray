-- { -w 180 -h 120 -z 1 -r 1 -s 1 Demo/Teapot1 #-}

local C = require("Colors")

--camera = Camera({4.0,4.0,-10.0}, {0.0,1.5,0.0}, 60)
camera = Camera({0.0,3.0,-10.0}, {0.0,1,5.0}, 80)

SetBackground(YGradient(C.purple, C.red, 0.5))

AddLight(PointLight({1.0, 3.5, -4.0}, C.white))
ls = Sphere(0.1, { texture = Pigment(C.white), material = {ambient  = 1.0, shadow = false}})
ls = T(ls, 1.0, 3.5, -4.0)
AddObject(ls)


teapot = Mesh("Models/teapot.obj", 
              false, 
              { texture = ImageMap("Test/pattern01"),
               --texture = Checkerboard(C.red, C.green, 0.025),
               -- texture = Pigment(C.green),
               mapping = Cylindrical(),
               material = { 
                 ambient = 0.3,
                 diffuse = 0.5,
                 specular = 0.8,
                 shine = 50 } })

--teapot = Scale(teapot,1,1,1)
teapot = RZ(RY(teapot,-30), 15)

AddObject(teapot)

floor = Plane({0.0, 1.0, 0.0}, 0.0,
              {texture = Checkerboard(C.black, C.white, 1),
               material = {ambient = 0.4, diffuse = 0.6 } } )
AddObject(floor)
