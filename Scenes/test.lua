local Colors = require("Colors")

SetBackground(scene, YGradient(Colors.purple, Colors.red, 0.5))
AddLight(scene, PointLight(0.5, 3.0, -3.0, Colors.white))

a = Attributes()
a.texture = Checkerboard(Colors.red, Colors.green, 0.08)
a.material.ambient  = 0.2
a.material.diffuse  = 0.7
a.material.specular = 0.9
a.material.reflect  = 0.2
a.material.shine    = 90
sphere1 = Sphere(1.0, a)
sphere1 = Translate(RotateX(RotateZ(sphere1, 30), 45), 0.0, 1.0, 4.0)
AddObject(scene, sphere1)
