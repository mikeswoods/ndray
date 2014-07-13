### What is this is

This project is a continuation of a final project originally submitted for 
[CIS 552: Advanced Programming](http://www.seas.upenn.edu/~cis552/) at the University of Pennsylvania.

### What is this is not

It is a simple raytracer and definitely **NOT** physically based in
any way, shape or form (although this *may* change in the future). It was (and is)
a learning experience and an excuse to play around with Haskell and computer
graphics (as if one needs an excuse!). 

#### TODO

* Cubic texture mapping support

* Sky sphere support -- right now, only static backgrounds are supported

* Textures should be made more extensible so that defining new textures
does not mean adding another constructor to the BasicTexture data type. I
envision having a BasicTexture type, ImageTexture type, NoiseTexture type, etc.
and that there is a uniform way to retrieve a texel from each given a (u,v)
coordinate. I can't figure out how to do this yet with just typeclasses -- need
to look into type/data families, etc.

* Create some kind of mesh rendering options data type to control how meshes
are rendered: enable/disable Phong shading, autoscaling, etc.

* Create or find a decent Perlin/Simplex noise implementation

* More procedural texture types: sinusoidal, noise-based, as well as more exotic 
ones like marble, wood, etc.

* Allow the Lua script to modify objects in-place. So instead of, say rotating
a sphere like "sphere = RotateX(sphere, 30)", you could just do 
"RotateX(sphere, 30)". Maybe this is a good/bad idea? Need to think about it more.

* Split out KD-Tree implementation into a standalone package and submit to Hackage.
Maybe do the same for the Tensor module as well?

#### Various notes

##### Affine Transformations

###### How to transform an object in an object's intersection test function

Given a 4x4 transformation matrix M:

1. Transform the intersection point with M

2. Transform the ray's origin and direction with inverse(M)

3. Transform the normal at the point of intersection with transpose(inverse(M))

###### Refractive indices for various materials

Taken from http://en.wikipedia.org/wiki/List_of_refractive_indices

* Vacuum n = 1.00
* Water: n = 1.31
* Glass: n = 1.49
* Diamond: n = 2.419
 
#### Resources I found useful

Below is a short list of resources I found online that have helped me along the
way.  

##### Bounding boxes

###### AABBs (axis-aligned bounding boxes)
* http://zeuxcg.org/2010/10/17/aabb-from-obb-with-component-wise-abs/
* http://www.gamedev.net/topic/495016-aabb-rotation/
* http://www.cs.unc.edu/~zhangh/technotes/bbox.pdf
* http://dev.theomader.com/transform-bounding-boxes/

###### Algorithm overview
* http://www.cs.utexas.edu/~fussell/courses/cs384g/lectures/lecture09-Ray_tracing.pdf
* http://www.mikejutan.com/raytracer/mikejutanA5Manual.pdf

###### Affine transformations
* http://stackoverflow.com/questions/18418855/raytracing-problems-with-transformations
* http://scratchapixel.com/lessons/3d-basic-lessons/lesson-4-geometry/transforming-normals/

###### Camera orientation
* http://stackoverflow.com/questions/13078243/ray-tracing-camera
* http://www.cse.ohio-state.edu/~parent/classes/681/Lectures/08.RTgeometryHO.pdf
* http://stackoverflow.com/a/5944402
* http://stackoverflow.com/a/13078758
* http://steveharveynz.wordpress.com/2012/12/20/ray-tracer-part-two-creating-the-camera/

##### Haskell-related

###### Unboxing vectors, etc.
* http://stackoverflow.com/a/10866718

###### Using the ST monad (needed for JuicyPixels)
* http://stackoverflow.com/questions/22594806/how-do-i-write-pixels-with-juicypixels-in-st-monad
* http://www.haskell.org/haskellwiki/Monad/ST

###### Lenses
* http://www.haskell.org/haskellwiki/LensBeginnersCheatsheet

###### Matrix / REPA stuff
* http://www.cse.chalmers.se/edu/course/DAT280_Parallel_Functional_Programming/Papers/tutorialWinner.pdf
* http://chimera.labs.oreilly.com/books/1230000000929/ch05.html##sec_par-repa-arrays
* http://stackoverflow.com/questions/10747079/what-are-the-key-differences-between-the-repa-2-and-3-apis
* http://negativeprobability.blogspot.com/2011/11/affine-transformations-and-their.html

###### Lua intergration with HsLua
* http://osa1.net/posts/2014-04-27-calling-haskell-lua.html

##### Math

###### Linear algebra
* http://stackoverflow.com/questions/2624422/efficient-4x4-matrix-inverse-affine-transform

###### Interpolation
* http://paulbourke.net/miscellaneous/interpolation/

#### Data structures 

###### KD-Trees
* http://blog.frogslayer.com/kd-trees-for-faster-ray-tracing-with-triangles/

##### OBJ models

###### Format
* http://people.sc.fsu.edu/~jburkardt/data/obj/obj.html
* http://en.wikipedia.org/wiki/Wavefront_.obj_file

###### Example models
* http://people.sc.fsu.edu/~jburkardt/data/obj/obj.html

##### Intersection formulas

###### Plane
* http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_1_Introduction.shtml

###### Sphere
* http://wiki.cgsociety.org/index.php/Ray_Sphere_Intersection

###### Triangle
* http://en.wikipedia.org/wiki/M%C3%B6ller%E2%80%93Trumbore_intersection_algorithm

##### Lighting

###### Shading
* http://www.ccs.neu.edu/home/fell/CSU540/programs/RayTracingFormulas.htm
* http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_1_Introduction.shtml
* http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_2_Phong_Mirrors_and_Shadows.shtml

###### Attenuation
http://www.cs.cmu.edu/~jkh/462_s07/assts/assignment3/

###### Reflection
* http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_2_Phong_Mirrors_and_Shadows.shtml

###### Refraction
* http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_2_Phong_Mirrors_and_Shadows.shtml
* http://en.wikipedia.org/wiki/List_of_refractive_indices

##### UV Mapping / texture mapping
* http://bentonian.com/Lectures/AdvGraph1314/3.%20Ray%20tracing%20-%20color%20and%20texture.pdf (Cube, sphere, & torus mapping)
* http://en.wikipedia.org/wiki/UV_mapping
* http://www.flipcode.com/archives/Raytracing_Topics_Techniques-Part_6_Textures_Cameras_and_Speed.shtml
* http://paulyg.f2s.com/uv.htm
* http://www.siggraph.org/education/materials/HyperGraph/mapping/surface0.htm (Good resource on mapping)

##### Other resources

###### General

* http://paulbourke.net -- This guy's site is great. It's full of 3D related info. 

###### Textures
* http://paulbourke.net/texture_colour/
