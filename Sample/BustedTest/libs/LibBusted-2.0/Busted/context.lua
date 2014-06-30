local MAJOR, MINOR = "Olivine:Busted:Context-2.0", 1
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
  return
end
-------------------------------------------------------------------------------
--- Olivine-Labs Busted-Context
-------------------------------------------------------------------------------

local context = APkg and APkg.tPackage or {}

local data = {}
local parents = {}
local children = {}

function context.ref()
  local ref = {}
  local ctx = data

  function ref.get(key)
    if not key then return ctx end
    return ctx[key]
  end

  function ref.set(key, value)
    ctx[key] = value
  end

  function ref.attach(child)
    if not children[ctx] then children[ctx] = {} end
    parents[child] = ctx
    children[ctx][#children[ctx]+1] = child
  end

  function ref.children(parent)
    return children[parent] or {}
  end

  function ref.parent(child)
    return parents[child]
  end

  function ref.push(child)
    if not parents[child] then error('Detached child. Cannot push.') end
    ctx = child
  end

  function ref.pop()
    if parents[ctx] then -- TODO: Determine if this is the correct fix
      ctx = parents[ctx]
    end
  end

  function ref.reset()
    for k,v in pairs(ctx) do
      if k ~= "env" then
        ctx[k] = nil
      end
    end
    parents = {}
    children = {}
  end

  return ref
end

Apollo.RegisterPackage(context, MAJOR, MINOR, {})
