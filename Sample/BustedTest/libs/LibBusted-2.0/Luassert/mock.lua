local MAJOR, MINOR = "Olivine:Luassert:Mock-1.0", 1
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
  return
end

-------------------------------------------------------------------------------
--- Olivine-Labs mock
-------------------------------------------------------------------------------
local stub = Apollo.GetPackage("Olivine:Luassert:Stub-1.0").tPackage
local spy = Apollo.GetPackage("Olivine:Luassert:Spy-1.0").tPackage

local mocklib = APkg and APkg.tPackage or {}

local function mock(object, dostub, func, self, key)
  local data_type = type(object)
  if data_type == "table" then
    if spy.is_spy(object) then
      -- this table is a function already wrapped as a spy, so nothing to do here
    else
      for k,v in pairs(object) do
        object[k] = mock(v, dostub, func, object, k)
      end
    end
  elseif data_type == "userdata" then
    mock(getmetatable(object), dostub, func, self, key)
  elseif data_type == "function" then
    if dostub then
      return stub(self, key, func)
    elseif self==nil then
      return spy.new(object)
    else
      return spy.on(self, key)
    end
  end
  return object
end

local function unmock(object)
  local mockedObject = type(object) == "userdata" and getmetatable(object) or object
  if type(mockedObject) == "table" then
    if spy.is_spy(mockedObject) then
      mockedObject:revert()
    else
      for k,v in pairs(mockedObject) do
        unmock(v)
      end
    end
  end
end

mocklib.mock = mock
mocklib.unmock = unmock

Apollo.RegisterPackage(mocklib, MAJOR, MINOR, {("Olivine:Luassert:Stub-1.0", "Olivine:Luassert:Spy-1.0"})
