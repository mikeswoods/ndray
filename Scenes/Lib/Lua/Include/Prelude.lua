--------------------------------------------------------------------------------
-- Module: Prelude
--------------------------------------------------------------------------------

function table.has_key(t, k)
  return t[k] ~= nil
end

function class(obj)
  return getmetatable(obj).class
end

--------------------------------------------------------------------------------

T  = Translate

function TranslateX(obj,x)
  return Translate(obj,x,0.0,0.0)
end

TX = TranslateX

function TranslateY(obj,y)
  return Translate(obj,0.0,y,0.0)
end

TY = TranslateY

function TranslateZ(obj,z)
  return Translate(obj,0.0,0.0,z)
end

TZ = TranslateZ

R  = Rotate

function RotateX(obj,x)
  return Rotate(obj,x,0.0,0.0)
end

RX = RotateX

function RotateY(obj,y)
  return Rotate(obj,0.0,y,0.0)
end

RY = RotateY

function RotateZ(obj,z)
  return Rotate(obj,0.0,0.0,z)
end

RZ = RotateZ

S  = Scale

function ScaleX(obj,x)
  return Scale(obj,x,1.0,1.0)
end

SX = ScaleX

function ScaleY(obj,y)
  return Scale(obj,1.0,y,1.0)
end

SY = ScaleY

function ScaleZ(obj,z)
  return Scale(obj,1.0,1.0,z)
end

SZ = ScaleZ

--------------------------------------------------------------------------------

function Add(attribute, obj)
  if attribute == "background" then
    scene[attribute] = obj
  else
    if table.has_key(scene, attribute) then
      table.insert(scene[attribute], obj)
    else
      scene[attribute] = {obj}
    end
  end
end

function AddObject(obj)
  Add("objects", obj)
end

function AddLight(light)
  Add("lights", light)
end

function SetBackground(bg)
  Add("background", bg)
end

