local MAJOR, MINOR = "Olivine:Luassert:Stub-1.0", 1
-- Get a reference to the package information if any
local APkg = Apollo.GetPackage(MAJOR)
-- If there was an older version loaded we need to see if this is newer
if APkg and (APkg.nVersion or 0) >= MINOR then
    return
end

-------------------------------------------------------------------------------
--- Olivine-Labs stub
-------------------------------------------------------------------------------
local util = Apollo.GetPackage("Olivine:Luassert:Util-1.0").tPackage

-- Set a reference to the actual package or create an empty table
local stub = APkg and APkg.tPackage or {}

local stubfunc = function() end

function stub.new(object, key, func)
    if object == nil and key == nil then
        -- called without arguments, create a 'blank' stub
        object = {}
        key = ""
    end
    assert(type(object) == "table" and key ~= nil, "stub.new(): Can only create stub on a table key, call with 2 params; table, key")
    assert(object[key] == nil or util.callable(object[key]), "stub.new(): The element for which to create a stub must either be callable, or be nil")
    local old_elem = object[key]    -- keep existing element (might be nil!)
    object[key] = type(func) == "function" and func or stubfunc  -- set the stubfunction
    local s = spy.on(object, key)   -- create a spy on top of the stub function
    local spy_revert = s.revert     -- keep created revert function

    s.revert = function(self)       -- wrap revert function to restore original element
        if not self.reverted then
            spy_revert(self)
            object[key] = old_elem
            self.reverted = true
        end
        return old_elem
    end

    return s
end

function stub.is_stub(object)
    return spy.is_spy(object) and object.callback == stubfunc
end

local function set_stub(state)
end

stub = setmetatable( stub, {
        __call = function(self, ...)
            -- stub originally was a function only. Now that it is a module table
            -- the __call method is required for backward compatibility
            -- NOTE: this deviates from spy, which has no __call method
            return stub.new(...)
        end })

function stub:OnLoad()
    assert:register("modifier", "stub", set_stub)
end

Apollo.RegisterPackage(stub, MAJOR, MINOR, {"Lib:Assert-1.0"})
